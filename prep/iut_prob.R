# ------------------------------------------------------------------------------
# intrauterine transmission probability bootstrap
#
# purpose
#   - add intrauterine transmission probability to ce_exp_fw 
#   - used in joint model with 1wk art outcome 
#
# inputs:
#   - ce_exp_fw: per infant cost and effectiveness outcome 
#
# NOTE: for documentation only; ce_exp_fw not publicly available due to confidentiality 
# (ce_exp_fw links individual-level testing and treatment data)
# ------------------------------------------------------------------------------

# apply bootstrap for each country and study arm
## 1 bootstrap draw sampled per infant to propagate uncertainty in intrauterine transmission probability into probabilistic ce analysis 

iut_prob_list <- list()

set.seed(7) 

for (group in unique(ce_exp_fw$country_group)) {
    group_data <- as.numeric(as.character(ce_exp_fw[ce_exp_fw$country_group == group, ]$pos_birth))
    iut_prob_list[[group]] <- boot_iut_prob(group_data, 10000)
}

# add transmission prob from bayes bootstrap to ce_exp_fw
ce_exp_fw <- ce_exp_fw %>%
    group_by(country_group) %>%
    mutate(
        ## map over each group and row
        iut_prob = map2(country_group, row_number(), function(group, row) {
            ## extract relevant bootstrap probs for current group
            iut_prob <- iut_prob_list[[group]]
            ## randomly sample one probability from bootstrap distribution for this row
            sample(iut_prob, 1)
        })
    ) %>%
    ungroup()

ce_exp_fw$iut_prob <- as.numeric(ce_exp_fw$iut_prob)
