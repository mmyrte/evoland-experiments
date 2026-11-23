"""The reference Bayes-eKDE (clumpy's ekde and Bayes estimators) on files, for ekde-tests.r.

Usage, from the repository root, with a Python that has clumpy, ekde and hyperclip (the
repository's uv environment, or the uv project of an mmyrte/clumpy checkout):

    python mlr3-custom-learners/ekde-reference/reference.py <work_dir>

<work_dir>/config.json holds {"mode": "density" | "bayes" | "volumes", "bounds": [...], "seed": int, and
optionally "h", "q", "n_corrections_max", "n_fit_max", "canonical_signs": bool}.

- mode "density": fits ekde.KDE (box kernel, Terrell bandwidth unless "h" is given) on
  <work_dir>/train.csv (feature columns only) and writes its density at the rows of
  <work_dir>/pred.csv to density.csv (column f), and the bandwidth to info.json.
- mode "bayes": <work_dir>/train.csv has the feature columns and a last column "y" (integer
  class codes). Fits clumpy's Bayes estimator (density_estimator="ekde") with one-hot W of the
  sorted classes and P(v) their frequencies, as 2026-09-model-comparison/015-clumpy-kde does,
  and writes P(z | v) (dens_v.csv), P(z) (dens.csv) and the adjusted P(v | z) (prob.csv) at the
  rows of pred.csv.
- mode "volumes": hyperclip's volumes(A, R, C, h, zero) on <work_dir>/A.csv (d x m), R.csv
  (one column), C.csv (n x d) and config "h", "zero"; writes volumes.csv.

"canonical_signs": true replaces the sign of each right singular vector of the whitening by the
one that makes its largest component positive (the convention of ekde.r). The reference takes
the signs LAPACK returns, which depend on the order of the rows; this option is the only change
to the reference, and is used to compare the rest of the algorithm independently of it.
Prints the wall time of fit and predict.
"""

import json
import os
import sys
import time

import numpy as np
import pandas as pd

import ekde.base
from ekde.whitening_transformer import WhiteningTransformer


class CanonicalWhiteningTransformer(WhiteningTransformer):
    def fit(self, X):
        super().fit(X)
        idx = np.argmax(np.abs(self._V), axis=0)
        signs = np.sign(self._V[idx, np.arange(self._V.shape[1])])
        signs[signs == 0] = 1
        self._V = self._V * signs
        n = self._num_obs
        self._transform_matrix = self._V @ np.diag(1 / self._s) * np.sqrt(n - 1)
        self._inverse_transform_matrix = np.diag(self._s) @ self._V.T / np.sqrt(n - 1)
        self._transform_det = np.abs(np.linalg.det(self._transform_matrix))
        self._inverse_transform_det = np.abs(np.linalg.det(self._inverse_transform_matrix))
        self.scale_ = self._inverse_transform_det
        return self


def main():
    work_dir = sys.argv[1]
    with open(os.path.join(work_dir, "config.json")) as f:
        cfg = json.load(f)
    if cfg.get("canonical_signs", False):
        ekde.base.WhiteningTransformer = CanonicalWhiteningTransformer
    np.random.seed(cfg.get("seed", 0))

    def read(name):
        return pd.read_csv(os.path.join(work_dir, name), float_precision="round_trip")

    bounds = cfg.get("bounds", [])
    kde_args = {"q": cfg.get("q", 51)}
    if "h" in cfg:
        kde_args["h"] = float(cfg["h"])

    if cfg["mode"] == "volumes":
        import hyperclip.hyperfunc

        A, R, C = (np.ascontiguousarray(read(f).to_numpy(float)) for f in ("A.csv", "R.csv", "C.csv"))
        R = R[:, 0].copy()
        t0 = time.time()
        vols = np.array(hyperclip.hyperfunc.volumes(A, R, C, float(cfg["h"]), zero=cfg["zero"]))
        t1 = time.time()
        pd.DataFrame({"v": vols}).to_csv(
            os.path.join(work_dir, "volumes.csv"), index=False, float_format="%.17g"
        )
        info = {"fit": 0.0, "predict": t1 - t0}
    elif cfg["mode"] == "density":
        train, pred = read("train.csv"), read("pred.csv")
        X = train.to_numpy(float)
        Y = pred[train.columns].to_numpy(float)
        t0 = time.time()
        kde = ekde.base.KDE(bounds=bounds, **kde_args).fit(X)
        t1 = time.time()
        f = kde.predict(Y)
        t2 = time.time()
        pd.DataFrame({"f": f}).to_csv(
            os.path.join(work_dir, "density.csv"), index=False, float_format="%.17g"
        )
        info = {"h": kde._h, "fit": t1 - t0, "predict": t2 - t1}
    else:
        train, pred = read("train.csv"), read("pred.csv")
        from clumpy.density_estimation import _methods
        from clumpy.transition_probability_estimation._bayes import Bayes

        cols = [c for c in train.columns if c != "y"]
        states = sorted(set(train["y"]))
        W = np.stack([train["y"].to_numpy() == s for s in states], axis=1)
        de = _methods["ekde"](**kde_args)
        t0 = time.time()
        tpe = Bayes(
            density_estimator=de,
            n_corrections_max=cfg.get("n_corrections_max", 1000),
            n_fit_max=cfg.get("n_fit_max", 10**5),
        )
        tpe.fit(Z=train[cols].to_numpy(float), W=W, bounds=bounds)
        t1 = time.time()
        P, P_Y, P_Y__v = tpe.compute(
            Y=pred[cols].to_numpy(float),
            P_v=W.mean(axis=0),
            return_P_Y=True,
            return_P_Y__v=True,
        )
        t2 = time.time()
        names = [str(s) for s in states]
        pd.DataFrame(P_Y__v, columns=names).to_csv(
            os.path.join(work_dir, "dens_v.csv"), index=False, float_format="%.17g"
        )
        pd.DataFrame({"f": P_Y[:, 0]}).to_csv(
            os.path.join(work_dir, "dens.csv"), index=False, float_format="%.17g"
        )
        pd.DataFrame(P, columns=names).to_csv(
            os.path.join(work_dir, "prob.csv"), index=False, float_format="%.17g"
        )
        info = {"h": [c._h for c in tpe.cde if hasattr(c, "_h")], "h_marginal": tpe.de._h,
                "fit": t1 - t0, "predict": t2 - t1}
    with open(os.path.join(work_dir, "info.json"), "w") as f:
        json.dump(info, f)
    print(f"TIMING,{info['fit']:.3f},{info['predict']:.3f}", flush=True)


if __name__ == "__main__":
    main()
