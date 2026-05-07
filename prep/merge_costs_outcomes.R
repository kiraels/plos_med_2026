# ------------------------------------------------------------------------------
# combine cost and effectiveness outcomes to get cost per infant
#
# purpose
#   - merge indiv level resources + outcomes with costs
#   - add cost per infant (testing and art)
#
# inputs 
#   - outcomes: effectiveness outcomes including 8wk EID and 1wk ART per country/cluster and arm
#   - cost_basis_fa: cost per test per country/cluster and arm, fixed costs allocated annually
#   - cost_basis_fw: cost per test per country/cluster and arm, fixed costs allocated weekly
# 
# NOTE: for documentation only; cost_basis_*, outcomes not publicly available due to confidentiality 
# (ce_exp_fw links individual-level testing and treatment data)
# ------------------------------------------------------------------------------

merge_key <- c("country", "cluster_num", "studyarm", "country_group")

# add cost per infant to cost_basis_fa
## NOTE: for documentation only -> to show how ce_exp_fa was constructed
ce_exp_fa <- cost_basis_fa$totals %>%
    merge(., outcomes, by = merge_key) %>%
    mutate(total_cost_testing = eid_total*total_cost_per_test,
           lam_ind = ifelse(add_art > 0, 1, 0),
           lam_ind = ifelse(is.na(lam_ind), 0, lam_ind),
           cost_infant = ifelse(country == "Mozambique", total_cost_testing + (lamivudine_moz*4*lam_ind),
                                total_cost_testing + (abc3tc_tanz*6*lam_ind))
            ) %>%
    dplyr::select(ID, country, cluster, cluster_num, studyarm, country_group, eid_total, life_poc_total,
                  poc_eid_8wks, pos_birth, lam_ind, cost_infant, ve_art)

# add cost per infant to cost_basis_fw
## NOTE: for documentation only -> to show how ce_exp_fw was constructed
ce_exp_fw <- cost_basis_fw$totals %>%
    merge(., outcomes, by = c(merge_key, "isoweek")) %>%
    group_by(ID) %>%
    mutate(total_cost_testing = sum(eid_total*total_cost_per_test),
           eid_total = sum(eid_total)) %>%
    slice(which.max(age_weeks)) %>%
    mutate(lam_ind = ifelse(add_art < 0, 1, 0),
           lam_ind = ifelse(is.na(lam_ind), 0, lam_ind),
           cost_infant = ifelse(country == "Mozambique", total_cost_testing + (lamivudine_moz*4*lam_ind),
                                total_cost_testing + (abc3tc_tanz*6*lam_ind))
           ) %>%
    dplyr::select(ID, country, cluster, cluster_num, studyarm, country_group, eid_total, life_poc_total,
                  poc_eid_8wks, pos_birth, lam_ind, cost_infant, ve_art)
