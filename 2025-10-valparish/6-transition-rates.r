# Transition Rates Calculation and Extrapolation
# This script calculates historical transition rates from observed data
# and extrapolates them to future periods using linear regression

devtools::load_all("~/github-repos/evoland/evoland-plus/")
db <- evoland_db$new(path = "fullch.evolanddb")

# Step 1: Calculate observed (historical) transition rates
# For each period and transition type, calculates: rate = (# cells A→B) / (# cells that were A)
# Uses db$trans_v (view of all transitions) and db$trans_meta_t (viable transitions)
db$trans_rates_t <- obs_rates <- create_obs_trans_rates_t(db$trans_v, db$trans_meta_t)

# Step 2: Extrapolate future rates using linear regression
# Fits rate ~ id_period independently for each id_trans
# Negative predicted rates are clamped to 0, rates > 1 capped at 1
# The upsert operation (assignment to db$trans_rates_t) adds the extrapolated rates
db$trans_rates_t <- create_extr_trans_rates_t(obs_rates, db$periods_t)

# Step 3: Export transition rates for Dinamica EGO
# Generates CSV files with columns: From* (anterior class), To* (posterior class), Rate
# Export for all periods that have rates
# all_period_ids <- unique(db$trans_rates_t$id_period)
# for (period_id in all_period_ids) {
#   rates <- db$trans_rates_dinamica_v(id_period = period_id)
#   data.table::fwrite(rates, sprintf("rates_period_%d.csv", period_id))
# }
