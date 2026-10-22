"""clumpy's Bayes-eKDE transition probability estimator on the PIE benchmark.

Usage (from the repository root, in the repository's uv environment):

    python 2026-09-model-comparison/015-clumpy-kde/fit_predict.py <work_dir> <seed>

For each anterior class u, reads <work_dir>/train_<u>.csv (id_coord, id_pred_*, final: the
1985 predictors of the cells of class u in 1985 and their class in 1991) and
<work_dir>/pred_<u>.csv (id_coord, id_pred_*: the 1991 predictors of the cells of class u in
1991). Fits P(Z | u, v) for every final class v by kernel density estimation (ekde), and
writes P(v | u, Z) for the transitions (v != u) to <work_dir>/potentials_<u>.csv, by Bayes'
rule with the calibration frequencies P(v | u) (clumpy's Bayes TPE). Prints the wall time of
each class to stdout.
"""

import glob
import os
import re
import sys
import time

os.environ.setdefault("MPLBACKEND", "Agg")

import numpy as np
import pandas as pd
from clumpy.transition_probability_estimation._bayes import Bayes


def main():
    work_dir, seed = sys.argv[1], int(sys.argv[2])
    for path in sorted(glob.glob(os.path.join(work_dir, "train_*.csv"))):
        u = int(re.search(r"train_(\d+)\.csv$", path).group(1))
        train = pd.read_csv(path)
        pred = pd.read_csv(os.path.join(work_dir, f"pred_{u}.csv"))
        cols = sorted((c for c in train.columns if c.startswith("id_pred_")),
                      key=lambda c: int(c.split("_")[-1]))
        # final classes: staying first, then the transitions observed in calibration
        states = [u] + sorted(set(train["final"]) - {u})
        W = np.stack([train["final"].to_numpy() == s for s in states], axis=1)
        # every predictor here is non-negative (elevation, slope, distance, counts):
        # bounded on the left, as clumpy bounds its distance variables
        bounds = ["left"] * len(cols)

        t0 = time.time()
        # Bayes subsamples classes above n_fit_max cells with np.random
        np.random.seed(seed + u)
        tpe = Bayes(density_estimator="ekde", n_corrections_max=1000, n_fit_max=10**5)
        tpe.fit(Z=train[cols].to_numpy(float), W=W, bounds=bounds)
        P = tpe.compute(Y=pred[cols].to_numpy(float), P_v=W.mean(axis=0))
        seconds = time.time() - t0

        out = pd.concat(
            [pd.DataFrame({"id_coord": pred["id_coord"], "id_lulc_posterior": v,
                           "value": P[:, k]})
             for k, v in enumerate(states) if v != u]
        )
        out.to_csv(os.path.join(work_dir, f"potentials_{u}.csv"), index=False)
        # clumpy prints its own warnings to stdout: the timing lines carry a prefix
        print(f"TIMING,{u},{seconds:.2f},{len(train)},{len(pred)}", flush=True)


if __name__ == "__main__":
    main()
