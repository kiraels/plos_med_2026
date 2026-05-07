#-------------------------------------------------------------------------------
# sensitivity analysis for influence of intrauterine transmission on icer
# 
# purpose:
#   - evaluate icer variability with 0-40% HIV transmission at birth
#
# inputs:
#   - ce_exp_fw: individual-level cost and effectiveness outcome input data (not available)
#   - p1.0: posterior draws from m1.0 (not available)
#   - pi_grid: transmission probabilities vector
# 
# outputs:
#   - iut_sens: country-level icer results for each pi
#
# NOTES: 
#   - icer is based on ratio of posterior means and uncertainty is based on delta method for variance of a ratio (consistent with hesim)
#   - ce_exp_fw and p1.0 link individual-level cost and effectiveness outcome data and are not available due to confidentiality
#-------------------------------------------------------------------------------

#----- check column names -----
check_names <- names(p1.0)

# required predictors 
required <- c("b_veart_Intercept", "b_veart_studyarmVEID", "b_veart_pos_birth1")
stopifnot(all(required %in% check_names))

#----- parameters -----
pi_grid <- seq(0.005, 0.40, by = 0.001)

#----- icer sensitivity -----
calculate_icer_sensitivity <- function(m, d, pi_grid = seq(0.005, 0.40, by = 0.001)) {
    
    p <- as_draws_df(m)
    
    # random effects for cost
    random_effects_cost <- as.data.frame(ranef(m)$`country:cluster_num`) %>%
        rownames_to_column("group") %>%
        separate(group, into = c("country", "cluster_num"), sep = "_") %>%
        mutate(studyarm = case_when(
            cluster_num %in% veid_sites ~ "VEID",
            TRUE ~ "SoC"
        )) %>%
        mutate(across(c(country, cluster_num, studyarm), as.character))
    
    sens <- lapply(unique(d$country), function(y) {
        arm_results <- list()
        
        for (t in unique(d$studyarm)) {
            subset_data <- d[d$country == y & d$studyarm == t, ]
            mean_eid_total <- mean(subset_data$eid_total, na.rm = TRUE)
            
            cluster_random_intercepts <- random_effects_cost %>%
                filter(country == y & studyarm == t) %>%
                pull(Estimate.costinfant_Intercept)
            mean_cluster_random_intercept <- mean(cluster_random_intercepts)
            
            # cost per infant
            cost_log <- p$b_costinfant_Intercept +
                ifelse(t == "VEID", p$b_costinfant_studyarmVEID, 0) +
                (p$b_costinfant_eid_total * mean_eid_total) +
                ifelse(t == "VEID", p$`b_costinfant_studyarmVEID:eid_total` * mean_eid_total, 0) +
                mean_cluster_random_intercept
            cost_infant <- exp(cost_log) * (1 - p$hu_costinfant)
            
            # ve art
            ve_art_logit <- p$b_veart_Intercept +
                ifelse(t == "VEID", p$b_veart_studyarmVEID, 0) +
                ifelse(t == "VEID", p$b_veart_pos_birth1, 0) +
                ifelse(y == "Tanzania", p$b_veart_countryTanzania/s, 0)
            ve_art <- plogis(ve_art_logit)
            
            arm_results[[t]] <- list(
                cost_infant = cost_infant,
                ve_art = ve_art
            )
        }
        
        # loop over transmission probability grid
        pi_results <- lapply(pi_grid, function(pi_val) {
            # scale cost per pos infant
            dC <- (arm_results$VEID$cost_infant / pi_val) - (arm_results$SoC$cost_infant / pi_val)
            dE <- arm_results$VEID$ve_art - arm_results$SoC$ve_art
            
            u_dC <- mean(dC)
            u_dE <- mean(dE)
            
            # ICER as ratio of means
            u_icer <- u_dC / u_dE
            
            # Delta method SE
            var_dC <- var(dC)
            var_dE <- var(dE)
            cov_ce <- cov(dC, dE)
            
            tibble(
                country = y,
                pi = pi_val,
                #dC = dC,
                #ll_dC = dC - 1.96 * sqrt(var_dC),
                #ul_dC = dC + 1.96 * sqrt(var_dC),
                #u_dE = u_dE,
                #ll_dE = dE - 1.96 * sqrt(var_dE),
                #ul_dE = dE + 1.96 * sqrt(var_dE),
                u_icer = u_icer,
                ll_icer = u_icer - 1.96 * sqrt((var_dC / u_dE^2) + (u_dC^2 * var_dE / u_dE^4) - 2 * u_dC * cov_ce / u_dE^3),
                ul_icer = u_icer + 1.96 * sqrt((var_dC / u_dE^2) + (u_dC^2 * var_dE / u_dE^4) - 2 * u_dC * cov_ce / u_dE^3)
            )
        })
        
        bind_rows(pi_results)
    })
    
    bind_rows(sens)
}

#----- run + save -----
iut_sens <- calculate_icer_sensitivity(m1.0, ce_exp_fw, pi_grid)

saveRDS(sens_out, file = paste0(path_sub_results, "/sensitivity/iut_icer_sensitivity.rds"))
write_csv(sens_out, file = paste0(path_sub_results, "/sensitivity/iut_icer_sensitivity.csv"))



