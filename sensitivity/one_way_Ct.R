#-------------------------------------------------------------------------------
# one-way deterministic sensitivity analysis for cost per test
#
# purpose: evaluate uncertainty in key parameters influence on unit cost per test
#   - n: equipment lifespan (years)
#   - r: discount rate
#   - t: testing throughput / annual volume (relative change)
#   - C_c: consumable price (relative change)
#
# process:
#   1. modify relevant input
#   2. recompute annualized fixed costs (when n or r changes)
#   3. recalculate unit cost per test
#   4. compare against base case to get Δ cost per test
#
# inputs:
#   - ranges: list of lower and upper limits for key parameters
#   - cost_basis_fa_*: base case cost per test
#   - recompute_fixed_costs() + modify_unit_costs() from utils/helper_functions.R
#
# outputs:
#   - results aggregated by country (median across clusters), cluster level 
#   
#-------------------------------------------------------------------------------

#----- param ranges -----
ranges <- list(
    n   = c(4, 15),      # equipment lifespan (4-15 years)
    r   = c(0, 0.06),    # discount rate (0-6%)
    t   = c(0.5, 2.0),   # throughput relative change (-50%, +200%)
    C_c = c(0.5, 1.5)    # consumables price relative change (±50%)
)

#----- base costs -----
base_costs <- cost_basis_fa$totals

#----- tornado loop -----
tornado_data <- map_dfr(names(ranges), function(param) {
    vals <- ranges[[param]]
    
    map_dfr(vals, function(val) {
        costs <- modify_unit_costs(param, val)
        
        costs %>%
            left_join(base_costs %>% dplyr::select(country, cluster_num, base = total_cost_per_test)) %>%
            mutate(
                parameter = param,
                value = val,
                which = ifelse(val == vals[1], "Low", "High"),
                delta = total_cost_per_test - base
            )
    })
})

# aggregate by country
tornado_data_agg <- tornado_data %>%
    group_by(country, parameter, which) %>%
    summarise(delta = median(delta, na.rm = TRUE))

write_csv(tornado_data, file = paste0(path_sub_results, "/sensitivity/cost_per_test_sensitivity.csv"))

rm(delta_draws)
