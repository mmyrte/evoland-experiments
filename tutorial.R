#' ---
#' title: "Evoland Tutorial"
#' author: "Jan Hartman"
#' ---

library(evoland)
library(data.table)
library(terra)

unlink("firstmodel.evolanddb", recursive = TRUE)
db <- evoland_db$new(path = "firstmodel.evolanddb")

# %%
#| label: set-coords
# Coordinates: 30×30 grid in Swiss LV95
# We keep a template to later generate synthetic data for tutorial purposes
template_rast <- terra::rast(
  crs = "EPSG:2056",
  extent = terra::ext(c(
    xmin = 2697000,
    xmax = 2700000,
    ymin = 1252000,
    ymax = 1255000
  )),
  resolution = 100
)
db$coords_t <- create_coords_t_square(
  # evoland uses EPSG codes to store the CRS
  epsg = terra::crs(template_rast, describe = TRUE)$code |>
    as.integer(),
  extent = terra::ext(template_rast),
  # square pixels; single resolution value
  resolution = terra::res(template_rast)[1]
)

# retrieve the table
db$coords_t
# subset coords_t using data.table semantics
db$coords_t[lon == 2699650]
# minimal representation (id, lat, lon) using an active binding
# this is akin to a view in databases
db$coords_minimal

# %%
#| label: set-periods

# note that an additional "0th" period is added at the end of the observed
# range; this is used for labelling predictor data as static
db$periods_t <- create_periods_t(
  period_length_str = "P10Y", # 10 year period
  start_observed = "1995-01-01",
  end_observed = "2020-01-01",
  end_extrapolated = "2030-01-01"
)

# %%
#| label: recode-arealstatistik
# create metadata lookup table
db$lulc_meta_t <- create_lulc_meta_t(
  list(
    forest = list(
      pretty_name = "Forest",
      description = "Areas with lots of trees",
      src_classes = 1:3
    ),
    arable = list(
      pretty_name = "Arable Land",
      src_classes = c(4, 8)
    ),
    urban = list(
      pretty_name = "Urban Areas",
      description = "Where nature goes to die",
      src_classes = 5:7
    ),
    static = list(
      pretty_name = "Immutable",
      description = "Areas where we cannot conceptualize change",
      src_classes = 9:10
    )
  )
)

# %%
#| label: synthesize-and-agg-lulc-data

# autoregressive noise with skellam distribution
n_cells <- dim(template_rast)[1] * dim(template_rast)[2]
noise1 <- runif(n_cells, min = 0, max = 10)
noise2 <- noise1 + stats::rpois(n_cells, 1) - stats::rpois(n_cells, 1)
noise3 <- noise2 + stats::rpois(n_cells, 1) - stats::rpois(n_cells, 1)

# we generate a synthetic LULC raster with 3 layers (one per period) by adding
# noise to the previous layer, then smoothing and classifying it
synthetic_lulc <-
  rast(template_rast, nlyrs = 3, vals = c(noise1, noise2, noise3)) |>
  focal(w = 3, fun = mean, na.rm = TRUE) |>
  clamp(lower = 0, upper = 10) |>
  classify(rcl = data.frame(from = 0:9, to = 1:10, becomes = sample(1:10, 10)))

# we extract the synthetic LULC values at the coordinates in coords_t; the
# underlying logic is from terra::extract
synthetic_at_coords <-
  extract_using_coords_t(synthetic_lulc, db$coords_t)

# we join in the metadata to replace the source class (1-10) with the conceptual
# classes (forest, arable, urban, static)
synthetic_joint_meta <-
  synthetic_at_coords[, .(
    id_coord,
    id_period = substr(layer, 4, 4) |> as.integer(), # derive from synthetic_lulc layer names
    src_class = value
  )][
    db$lulc_meta_long_v,
    on = .(src_class),
    nomatch = NULL
  ]

# we add a run ID (0 for the base run) and coerce to a lulc_data_t object,
# ensuring that the data is in the precise long format expected by evoland
synthetic_for_upsert <- as_lulc_data_t(synthetic_joint_meta[, .(
  id_run = 0L, # this is the base run ID
  id_period,
  id_lulc,
  id_coord
)])

# we can now upsert the synthetic data into the database
db$lulc_data_t <- synthetic_for_upsert

# as soon as the data is upserted, we can derive the transitions that occurred
# within the synthetic data.
db$trans_v[id_lulc_anterior != id_lulc_posterior]

# %%
#| label: add-predictors
# you have already seen how to extract data at coordinates using coords_t; you
# could do the same with any SpatRaster or SpatVector, as long as the CRS and
# extent are compatible with coords_t. for demo purposes, we'll just use test
# data that comes with evoland.
# note that the metadata hold a fill_value that is used to infer what value the
# variable should assume when data is missing for a coordinate; it defaults to
# NA, but this may poison some models. hence, it makes sense to be explicit
# about the assumption that "if there is no soil type specified, we assume"
db$pred_meta_t <- evoland:::test_pred_meta_t
db$pred_data_t <- evoland:::test_pred_data_t

# %%
#| label: neighbors
# because statistical models like GLM that we use for transition probability
# estimation do not have inherent concepts of spatial relations, we explicitly
# calculate the neighbors for each coordinate and store them in the database.
# this allows us to then compute predictors like "number of neighboring cells
# within distance [x, y) that are forest" for each coordinate.
db$set_neighbors(
  max_distance = 1000, # in CRS units (meters for LV95)
  distance_breaks = c(0, 100, 500, 1000) # distance classes for neighbor-based predictors
)
# check out the result. even for 900 coordinates, we already get 208k neighbor
# pairs! this is the least efficient part of the process, but we only have to do
# it once and then we can compute as many predictors as we want from the
# neighbor information without having to recalculate the neighbors.
db$neighbors_t

# now we can generate predictors based on the neighbors; for example, we might
# want to know how many neighboring cells within 100m are forest, arable, urban,
# or static for each coordinate and period. this is a common predictor in land
# use change modeling, as the land use of neighboring cells can influence the
# probability of change.
db$generate_neighbor_predictors()

# check out pred_data_t - we're now already at 24k rows!
db$row_count("pred_data_t")

# the metadata show us which predictors hav been generated for which distance classes
db$pred_meta_t

# %%
#| label: eligible-transitions-and-predictors
# now we can determine which transitions are eligible for modeling; here, we
# filter by a minimum absolute number of observed transitions
db$trans_meta_t <- create_trans_meta_t(db$trans_v, min_cardinality_abs = 50)

# and which predictors are available for those transitions based on the data we
# have. we start out by associating all predictors with all transitions, then we
# will start pruning that set of relations (stored in db$trans_preds_t)
db$set_full_trans_preds()

# filter based on variable importance using a two-stage covariance filter, see
# help page for covariance_filter for details. we get loads of errors because of
# zero-weights and non-convergence because of the synthetic data / random noise
# we use; let's just disregard that for now.
# the pruning function accepts any function that takes a table of
# transition-predictor data (see trans_pred_data_v) according to the _current_
# trans_preds_t relations: if it is full, all predictors are associated with all
# transitions and then tested.
# the pruning function returns a character vector of column names to keep, so
# you could implement your own custom filter function if you wanted to. the
# assignment to the table is _not_ an upsert, but an overwrite: we want to drop
# rows, after all! you will be prompted to confirm that you want to overwrite
# the table, if you are running interactively.
db$trans_preds_t <- db$get_pruned_trans_preds_t(
  filter_fun = covariance_filter,
  # this is a very low threshold; ideally, we could achieve parsimony with a
  # much higher correlation cutoff
  corcut = 0.1
)

# %%
#| label: trans-models
#' we are now ready to actually model transitions! first, we fit partial models,
#' i.e. models that make use of training/validation splits.

db$trans_models_t <- db$fit_partial_models(
  fit_fun = fit_glm,
  gof_fun = gof_glm,
  sample_frac = 0.7,
  seed = 42 # for reproducibility
)

# now that we have partial models whose goodness-of-fit we can evaluate, we can
# fit full models where the gof_criterion "auc" is maximized - in this case
# we've only fitted one model per transition, so each partial model gets
# retrained on the full data.
db$trans_models_t <- db$fit_full_models(gof_criterion = "auc", gof_maximize = TRUE)

# %%
#| label: transition-rates-alloc-params
# as a constrained pattern-based model, evoland uses DinamicaEGO to allocate
# transitions. in a simple approximation, we can simply extrapolate the rate of
# each transition from the observed data:
db$trans_rates_t <-
  db$get_obs_trans_rates() |>
  extrapolate_trans_rates(
    periods = db$periods_t,
    coord_count = n_cells
  )

# we also need to estimate the allocation parameters for DinamicaEGO, which
# determine the shape and size of new patches, respectively which fraction of
# a transition occurs as expansion of existing patches vs new patch formation.
# the n_perturbations relates to how many times we want to perturb our best
# initial guess: there is no statistically correct method for estimating these
# paramaters, so in a real study, you would perturb these paramaters many times
# over and then evaluate each of them in a backcasting test to see which
# parametrization delivers the best similarity.
alloc_for_eval <- db$create_alloc_params_t(n_perturbations = 0)

# but wait, the allocation parameters now start with an id_run = 1, which is
# intended for evaluating each set of allocation parameters, but if we just want
# to use the best guess with the base scenario, we set these values for id_run = 0
alloc_for_eval[, id_run := 0L]
db$alloc_params_t <- alloc_for_eval

# %%
#| label: allocation
# now we have all the components we need to do an allocation in DinamicaEGO, which is
# the final step in the process. if you have not installed Dinamica on your system, you
# will get a warning that the anterior distribution is simply being returned as
# posterior.
db$alloc_dinamica(
  id_period = db$periods_t[is_extrapolated == TRUE, id_period],
  gof_criterion = "auc",
  gof_maximize = TRUE
)

# %%
#| label: visualization
# we can extract the simulated LULC maps from the database, for instance as
# SpatRaster objects, and visualize them using terra::plot or ggplot2 or tmap or
# whatever you like.
labels <- db$periods_t[id_period != 0, paste0(year(start_date), " to ", year(end_date))]
db$lulc_data_as_rast() |>
  setNames(labels) |>
  plot()

# you should now see 4 maps: the first three with changes, plus the extrapolated
# last map that does not show any change if Dinamica is not installed.
