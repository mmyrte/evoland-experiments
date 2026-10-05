"""CLUinPy on the PIE benchmark: logistic suitability on the 1991 map, CLUMondo-style
allocation of 1992..1999 with the observed 1999 class totals as demand.

Usage (from the repository root, with CLUinPy's src/ and src/suitability/ on PYTHONPATH):

    python 2026-09-model-comparison/030-cluinpy/run_pie.py <cluinpy_repo> <n_realisations>

Writes outputs/maps/cluinpy/rNN.tif and outputs/timings-cluinpy.csv.
"""

import csv
import glob
import os
import random
import runpy
import shutil
import sys
import time

import numpy as np
import pandas as pd
import rasterio

PIE_DIR = "2026-09-model-comparison"
# PIE_SCALE = k > 1: the tiled grids of 080-scaling, see common.r
_SCALE = os.environ.get("PIE_SCALE", "")
_SUFFIX = f"-k{_SCALE}" if _SCALE else ""
DATA_DIR = os.path.join(PIE_DIR, "data" + _SUFFIX)
OUT_DIR = os.path.join(PIE_DIR, "outputs" + _SUFFIX)
WORK_DIR = os.path.abspath(os.path.join(OUT_DIR, "cluinpy-work"))
MAPS_DIR = os.path.join(OUT_DIR, "maps", "cluinpy")
TIMINGS = os.path.join(OUT_DIR, "timings-cluinpy.csv")
NO_DATA = -9999
CLASSES = ["Forest", "Built", "Other"]  # PIE codes 1, 2, 3 -> CLUinPy 0, 1, 2
START_YEAR, END_YEAR = 1992, 1999


def record_timing(stage, seconds, note=""):
    new = not os.path.exists(TIMINGS)
    with open(TIMINGS, "a", newline="") as f:
        w = csv.writer(f, quoting=csv.QUOTE_NONNUMERIC)
        if new:
            w.writerow(["tool", "stage", "seconds", "note", "host", "timestamp"])
        w.writerow(["cluinpy", stage, round(seconds, 2), note, os.uname().nodename,
                    time.strftime("%Y-%m-%dT%H:%M:%S")])


def write_like(template, path, array, dtype, nodata, count=1):
    with rasterio.open(template) as src:
        profile = src.profile
    profile.update(dtype=dtype, nodata=nodata, count=count, compress="deflate")
    with rasterio.open(path, "w", **profile) as dst:
        if count == 1:
            dst.write(array.astype(dtype), 1)
        else:
            dst.write(array.astype(dtype))


def prepare_inputs():
    """Reclassify land use, align predictors' no-data value, write the tables."""
    lu_1991_path = os.path.join(DATA_DIR, "lu_1991.tif")
    with rasterio.open(lu_1991_path) as src:
        lu = src.read(1, masked=True)
    land = np.where(lu.mask, NO_DATA, lu.filled(1).astype("int16") - 1).astype("int16")
    land_path = os.path.join(WORK_DIR, "land_1991.tif")
    write_like(lu_1991_path, land_path, land, "int16", NO_DATA)

    env_dir = os.path.join(WORK_DIR, "env")
    os.makedirs(env_dir, exist_ok=True)
    for i in (1, 2, 3):
        with rasterio.open(os.path.join(DATA_DIR, f"ef_00{i}.tif")) as src:
            ef = src.read(1, masked=True)
        write_like(lu_1991_path, os.path.join(env_dir, f"ef_00{i}.tif"),
                   ef.filled(NO_DATA), "float32", NO_DATA)

    region = np.where(land == NO_DATA, 1, 0).astype("int16")
    region_path = os.path.join(WORK_DIR, "region.tif")
    write_like(lu_1991_path, region_path, region, "int16", NO_DATA)

    # one service per class: the class area. Demand interpolates the observed totals
    totals = pd.read_csv(os.path.join(OUT_DIR, "demand_class_totals.csv"))
    t91 = totals[totals.year == 1991].sort_values("id_lulc").N.to_numpy(float)
    t99 = totals[totals.year == 1999].sort_values("id_lulc").N.to_numpy(float)
    years = np.arange(START_YEAR, END_YEAR + 1)
    frac = (years - 1991) / (1999 - 1991)
    demand = pd.DataFrame(np.outer(1 - frac, t91) + np.outer(frac, t99),
                          columns=[f"{c}_area" for c in CLASSES])
    demand.to_excel(os.path.join(WORK_DIR, "demand.xlsx"), index=False)

    identity = pd.DataFrame(np.eye(3), columns=[f"{c}_area" for c in CLASSES])
    identity.insert(0, "class", CLASSES)
    identity.to_excel(os.path.join(WORK_DIR, "lus_matrix.xlsx"), index=False)
    identity.to_excel(os.path.join(WORK_DIR, "lus_conv.xlsx"), index=False)

    allow = pd.DataFrame(np.ones((3, 3), dtype=int), columns=CLASSES)
    allow.insert(0, "class", CLASSES)
    allow.to_excel(os.path.join(WORK_DIR, "allow.xlsx"), index=False)
    return land_path, region_path, env_dir


def fit_suitability(land_path, env_dir):
    """CLUinPy's own suitability module, logistic regression, as in its tutorial."""
    from suitability.main import suitability
    from suitability.sampling import sample_per_class
    from suitability.io_utils import find_files

    land = rasterio.open(land_path).read(1)
    sample_list = sample_per_class(land, NO_DATA, "fraction", 0.1, 100, 500)
    out = os.path.join(WORK_DIR, "suitability_out")
    suitability(classification=land_path,
                env_vars=find_files(env_dir, ".tif", ".tif"),
                mode="logistic",
                out_path=out,
                n_samples_corr=1000,
                vif_threshold=5,
                min_distance=3,
                test_fraction=0.3,
                random_state=12,
                sample_size_list=sample_list,
                no_data_value=NO_DATA,
                predict_outputs=True)
    stacks = sorted(glob.glob(os.path.join(out, "*", "suitability_stack.tif")))
    return stacks[-1]


def write_config(path, land_path, suit_path, region_path, out_dir):
    lines = {
        "land_array": land_path,
        "suit_array": suit_path,
        "region_array": region_path,
        # as in the CLUinPy tutorial for comparable classes: no neighbourhood for forest and
        # other, 0.3 for built-up land; inertia rises from other to built-up land
        "neigh_weights": "0,0.3,0",
        "start_year": START_YEAR,
        "end_year": END_YEAR,
        "demand": os.path.join(WORK_DIR, "demand.xlsx"),
        "dem_weights": "1,1,1",
        "lus_conv": os.path.join(WORK_DIR, "lus_conv.xlsx"),
        "lus_matrix_path": os.path.join(WORK_DIR, "lus_matrix.xlsx"),
        "allow": os.path.join(WORK_DIR, "allow.xlsx"),
        "max_diff_allow": 0.1,  # the tutorial uses 2 (%), which leaves ~700 built cells unallocated here
        "totdiff_allow": 0.1,
        "max_iter": 3000,
        "out_dir": out_dir,
        "crs": "EPSG:26986",
        "dtype": "int16",
        "no_data_out": NO_DATA,
        "conv_res": "0.8,1,0.6",
        "out_year": END_YEAR,
        "no_data_value": NO_DATA,
    }
    with open(path, "w") as f:
        f.write("\n".join(f"--{k}={v}" for k, v in lines.items()))


def main():
    cluinpy_repo, n_realisations = sys.argv[1], int(sys.argv[2])
    shutil.rmtree(WORK_DIR, ignore_errors=True)
    os.makedirs(WORK_DIR)
    os.makedirs(MAPS_DIR, exist_ok=True)
    if os.path.exists(TIMINGS):
        os.remove(TIMINGS)

    land_path, region_path, env_dir = prepare_inputs()
    t0 = time.time()
    suit_path = fit_suitability(land_path, env_dir)
    record_timing("fit_suitability", time.time() - t0)
    shutil.copy(suit_path, os.path.join(OUT_DIR, "maps", "cluinpy-suitability.tif"))

    for i in range(1, n_realisations + 1):
        out_dir = os.path.join(WORK_DIR, f"run_{i:02d}")
        os.makedirs(out_dir)
        config = os.path.join(out_dir, "config.txt")
        write_config(config, land_path, suit_path, region_path, out_dir)
        # the allocation draws its demand-adjustment "speed" from Python's random module
        random.seed(40000 + i)
        sys.argv = ["run_CLUinPy", "--config", config]
        t0 = time.time()
        runpy.run_module("scripts.run_CLUinPy", run_name="__main__")
        record_timing("allocate", time.time() - t0, f"realisation={i}")
        result = glob.glob(os.path.join(out_dir, "**", f"cov{END_YEAR}*.tif"), recursive=True)
        if not result or "error" in os.path.basename(result[0]):
            raise RuntimeError(f"CLUinPy did not converge for realisation {i}: {result}")
        with rasterio.open(result[0]) as src:
            cov = src.read(1)
        lu = np.where(cov == NO_DATA, 255, cov + 1).astype("uint8")
        write_like(os.path.join(DATA_DIR, "lu_1991.tif"), os.path.join(MAPS_DIR, f"r{i:02d}.tif"),
                   lu, "uint8", 255)


if __name__ == "__main__":
    main()
