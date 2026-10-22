"""CLUinPy's CLUMondo allocation of given class suitabilities, for Fig. 2.

Usage (from the repository root, with CLUinPy's src/ and src/suitability/ on PYTHONPATH):

    python 2026-09-paper-figures/010-fig2-ensembles/cluinpy_alloc.py <cluinpy_repo> <work_dir> \
        <n_realisations> <seed_base> <out_dir>

<work_dir> holds, written by 010-fig2-ensembles.r:

- land.tif: the initial map, classes 0..K-1 in CLUinPy's convention, -9999 for no data;
- suit.tif: K bands, band k the suitability of class k;
- demand.csv: one row, K columns, the class totals to reach;
- allow.csv: K x K, 1 where a conversion from row to column is allowed;
- params.csv: one row per class: name, conv_res (conversion resistance), neigh_weight.

Allocates in one step and writes <out_dir>/rNN.tif with classes 1..K. Realisation i seeds Python's
random module with seed_base + i: CLUinPy's only randomness is its demand-adjustment speed.
"""

import glob
import os
import random
import runpy
import sys

import numpy as np
import pandas as pd
import rasterio

NO_DATA = -9999
YEAR = 1


def write_like(template, path, array, dtype, nodata):
    with rasterio.open(template) as src:
        profile = src.profile
    profile.update(dtype=dtype, nodata=nodata, count=1, compress="deflate")
    with rasterio.open(path, "w", **profile) as dst:
        dst.write(array.astype(dtype), 1)


def main():
    cluinpy_repo, work_dir, n, seed_base, out_dir = sys.argv[1:6]
    n, seed_base = int(n), int(seed_base)
    sys.path[:0] = [os.path.join(cluinpy_repo, "src"), os.path.join(cluinpy_repo, "src", "suitability")]
    os.makedirs(out_dir, exist_ok=True)

    params = pd.read_csv(os.path.join(work_dir, "params.csv"))
    classes = params["name"].tolist()
    land_path = os.path.join(work_dir, "land.tif")
    land = rasterio.open(land_path).read(1)
    # CLUinPy's region map: 0 where the model runs
    region_path = os.path.join(work_dir, "region.tif")
    write_like(land_path, region_path, np.where(land == NO_DATA, 1, 0), "int16", NO_DATA)

    demand = pd.read_csv(os.path.join(work_dir, "demand.csv"))
    demand.columns = [f"{c}_area" for c in classes]
    demand.to_excel(os.path.join(work_dir, "demand.xlsx"), index=False)
    # one service per class: its area
    identity = pd.DataFrame(np.eye(len(classes)), columns=[f"{c}_area" for c in classes])
    identity.insert(0, "class", classes)
    identity.to_excel(os.path.join(work_dir, "lus_matrix.xlsx"), index=False)
    identity.to_excel(os.path.join(work_dir, "lus_conv.xlsx"), index=False)
    allow = pd.read_csv(os.path.join(work_dir, "allow.csv"), header=None)
    allow.columns = classes
    allow.insert(0, "class", classes)
    allow.to_excel(os.path.join(work_dir, "allow.xlsx"), index=False)

    for i in range(1, n + 1):
        run_dir = os.path.join(work_dir, f"run_{i:03d}")
        os.makedirs(run_dir, exist_ok=True)
        config = {
            "land_array": land_path,
            "suit_array": os.path.join(work_dir, "suit.tif"),
            "region_array": region_path,
            "neigh_weights": ",".join(str(w) for w in params["neigh_weight"]),
            "start_year": YEAR,
            "end_year": YEAR,
            "demand": os.path.join(work_dir, "demand.xlsx"),
            "dem_weights": ",".join("1" for _ in classes),
            "lus_conv": os.path.join(work_dir, "lus_conv.xlsx"),
            "lus_matrix_path": os.path.join(work_dir, "lus_matrix.xlsx"),
            "allow": os.path.join(work_dir, "allow.xlsx"),
            # % of demand, as in 2026-09-model-comparison's crossing
            "max_diff_allow": 0.5,
            "totdiff_allow": 0.5,
            "max_iter": 30000,
            "out_dir": run_dir,
            "crs": "EPSG:2056",
            "dtype": "int16",
            "no_data_out": NO_DATA,
            "conv_res": ",".join(str(r) for r in params["conv_res"]),
            "out_year": YEAR,
            "no_data_value": NO_DATA,
        }
        config_path = os.path.join(run_dir, "config.txt")
        with open(config_path, "w") as f:
            f.write("\n".join(f"--{k}={v}" for k, v in config.items()))
        random.seed(seed_base + i)
        sys.argv = ["run_CLUinPy", "--config", config_path]
        runpy.run_module("scripts.run_CLUinPy", run_name="__main__")
        result = glob.glob(os.path.join(run_dir, "**", f"cov{YEAR}*.tif"), recursive=True)
        if not result or "error" in os.path.basename(result[0]):
            raise RuntimeError(f"CLUinPy did not converge in realisation {i}: {result}")
        cov = rasterio.open(result[0]).read(1)
        write_like(land_path, os.path.join(out_dir, f"r{i:02d}.tif"),
                   np.where(cov == NO_DATA, 255, cov + 1), "uint8", 255)


if __name__ == "__main__":
    main()
