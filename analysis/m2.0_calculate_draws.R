# ------------------------------------------------------------------------------
# cost-effectiveness analysis: generate ce draws for m2.0
#
# purpose:
#   - generate draws of mean costs and effects, incremental costs and incremental effects
#   - provide descriptive inputs for ce planes and downstream decision analysis
#
# description:
#   - cost outcome: per-infant cost (hurdle gamma, log link)
#   - effect outcome: probability of 8-week EID (Bernoulli, logit link)
#   - random effects as estimated in the fitted model used for country and arm specific predictions
#
# inputs:
#   - m : fitted brms model object for m2.0
#   - d : analysis dataset used (ce_exp_fw; not available)
#
# outputs:
#   - ce_draws: posterior draws of costs and effects by country x arm
#   - delta_draws: posterior draws of incremental cost (ΔC) and incremental effect (ΔE)
#
# notes:
#   - model-specific to m2.0; m1.0 uses a separate implementation
#   - individual-level linked cost and effectiveness outcome data (ce_exp_fw) not available due to confidentiality
# ------------------------------------------------------------------------------
#----- directories -----
dir.create(paste0(path_sub_results, "/cea/"))


#----- calculate c, e, icer -----
calculate_ce_draws_m2.0 <- function(m, d) {
    
    p <- as_draws_df(m)
    n_draws <- nrow(p)
    
    # random effects
    re <- as.data.frame(ranef(m)$`country:cluster_num`) %>%
        rownames_to_column("group") %>%
        separate(group, into = c("country", "cluster_num"), sep = "_") %>%
        mutate(studyarm = case_when(
            cluster_num %in% veid_sites ~ "VEID",
            TRUE ~ "SoC"
        )) %>%
        mutate(across(c(country, cluster_num, studyarm), ~as.character(.)))
    
    # storage
    ce_draws <- list()
    
    # loop through countries
    for (y in unique(d$country)) {
        
        # loop through study arms
        for (t in unique(d$studyarm)) {
            
            subset_data <- d[d$country == y & d$studyarm == t, ]
            
            # means for predictors (used for population-level effects)
            mean_eid_total <- mean(subset_data$eid_total, na.rm = TRUE)
            
            # mean cluster random intercept cost
            mean_cluster_re_cost <- re %>%
                filter(country == y & studyarm == t) %>%
                pull(Estimate.costinfant_Intercept) %>%
                mean()
            
            # cost (log-scale to original scale)
            cost_log <- p$b_costinfant_Intercept +
                ifelse(t == "VEID", p$b_costinfant_studyarmVEID, 0) +
                (p$b_costinfant_eid_total * mean_eid_total) +
                ifelse(t == "VEID", p$`b_costinfant_studyarmVEID:eid_total` * mean_eid_total, 0) +
                mean_cluster_re_cost
            
            cost_infant <- exp(cost_log) * (1 - p$hu_costinfant)  
            
            # mean cluster random intercept effect
            mean_cluster_re_poc <- re %>%
                filter(country == y & studyarm == t) %>%
                pull(Estimate.poceid8wks_Intercept) %>%
                mean()
            
            # effect (logit-scale to probability)
            poc_eid_8wks_logit <- p$b_poceid8wks_Intercept +
                ifelse(t == "VEID", p$b_poceid8wks_studyarmVEID, 0) +
                mean_cluster_re_poc
            
            poc_eid_8wks <- plogis(poc_eid_8wks_logit)  
            
            ce_draws[[length(ce_draws) + 1]] <-
                tibble(
                    draw = seq_len(n_draws),
                    country = y,
                    studyarm = t,
                    C = cost_infant,
                    E = poc_eid_8wks
                )
        }
    }
    
    bind_rows(ce_draws)
}



# ----- run + save -----
# ce
ce_draws2.0 <- calculate_ce_draws_m2.0(m2.0, ce_exp_fw)
saveRDS(ce_draws2.0, paste0(path_save, "m2.0_ce_draws.rds"))
write_csv(ce_draws2.0, paste0(path_save, "m2.0_ce_draws.csv"))

# delta
delta_draws2.0 <- calculate_delta_draws(ce_draws2.0)

