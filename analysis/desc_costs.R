# ------------------------------------------------------------------------------
# descriptive cost summarised as weighted mean + 95% CI
#   - cost per test
#   - cost per infant
#
# NOTE: ce_exp_fa not shared do to confidentiality
# ------------------------------------------------------------------------------


#----- cost per test -----
set.seed(777)

# by country and arm
r3.1 <- summarise_cost(
    cost_basis_fa$totals, 
    group_vars = "country_group", 
    outcome = total_cost_per_test)

r3.1

write.csv(r3.1, file = paste0(path_sub_results, "/cost_per_test_agg_financial_risk.csv"))

# component proportions
cost_component_prop <- cost_basis_fa$components %>%
    group_by(country, category) %>%
    summarise(mean(cost_per_test/total_cost_per_test))

cost_component_prop

#----- cost per infant -----
set.seed(778)

# by country and arm
## eid testing and treatment costs
## NOTE: for documentation only ->
## ce_exp_fa links individual-level testing and treatment data and is not available due to confidentiality / small numbers of infants
#r3.2 <- summarise_cost(
#    ce_exp_fa,
#    group_vars = "country_group",
#    outcome = cost_infant
#)

#r3.2

#write.csv(r3.2, file = paste0(path_sub_results, "/cost_per_infant_agg_financial_risk.csv"))
