# ------------------------------------------------------------------------------
# cost-effectiveness analysis: generate ce draws for m1.0
#
# purpose:
#   - generate draws of mean costs and effects, incremental costs and incremental effects
#   - provide descriptive inputs for ce planes and downstream decision analysis
#
# description:
#   - cost outcome: per-infant cost (hurdle gamma, log link)
#   - effect outcome: probability of 1-week ART initiation (Bernoulli, logit link)
#   - random effects as estimated in the fitted model used for country and arm specific predictions
#
# inputs:
#   - m : fitted brms model object for m1.0
#   - d : analysis dataset used (ce_exp_fw; not shared)
#
# outputs:
#   - ce_draws: posterior draws of costs and effects by country x arm
#   - delta_draws: posterior draws of incremental cost (ΔC) and incremental effect (ΔE)
#
# notes:
#   - model-specific to m1.0; m2.0 uses a separate implementation
#   - individual-level cost and outcome data (ce_exp_fw) not shared due to disclosure risk associated with rare outcomes
# ------------------------------------------------------------------------------


#----- calculate c, e, icer -----
calculate_ce_draws_m1.0 <- function(m, d) {
    
    p <- as_draws_df(m)
    n_draws <- nrow(p)
    
    # random effects
    random_effects_cost <- as.data.frame(ranef(m)$`country:cluster_num`) %>%
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
            mean_iut_prob <- mean(subset_data$iut_prob, na.rm = TRUE)
            #ll_iut_prob <- quantile(subset_data$iut_prob, probs = 0.025)
            #ul_iut_prob <- quantile(subset_data$iut_prob, probs = 0.975)
            
            # mean cluster random intercept
            mean_cluster_re <- random_effects_cost %>%
                filter(country == y & studyarm == t) %>%
                pull(Estimate.costinfant_Intercept) %>%
                mean()
            
            # cost (log-scale to original scale)
            cost_log <- p$b_costinfant_Intercept +
                ifelse(t == "VEID", p$b_costinfant_studyarmVEID, 0) +
                (p$b_costinfant_eid_total * mean_eid_total) +
                ifelse(t == "VEID", p$`b_costinfant_studyarmVEID:eid_total` * mean_eid_total, 0) +
                mean_cluster_re
            
            cost_infant <- exp(cost_log) * (1 - p$hu_costinfant)  
            cost_infant_pos <- cost_infant / mean_iut_prob    
            #cost_infant_pos_ll_iut <- cost_infant / ll_iut_prob
            #cost_infant_pos_ul_iut <- cost_infant / ul_iut_prob
            
            # effect (logit-scale to probability)
            ve_art_logit <- p$b_veart_Intercept +
                ifelse(t == "VEID", p$b_veart_studyarmVEID, 0) +
                ifelse(t == "VEID", p$b_veart_pos_birth1, 0) +
                ifelse(y == "Tanzania", p$b_veart_countryTanzania/s, 0)
            
            ve_art <- plogis(ve_art_logit)  
            
            ce_draws[[length(ce_draws) + 1]] <-
                tibble(
                    draw = seq_len(n_draws),
                    country = y,
                    studyarm = t,
                    C = cost_infant_pos,
                    E = ve_art
                )
        }
    }
    
    bind_rows(ce_draws)
}


#----- run + save -----
# ce 
ce_draws1.0 <- calculate_ce_draws_m1.0(m1.0, ce_exp_fw)
saveRDS(ce_draws1.0, paste0(path_sub_results, "m1.0_ce_draws.rds"))
write_csv(ce_draws1.0, paste0(path_sub_results, "m1.0_ce_draws.csv"))

# delta
delta_draws1.0 <- calculate_delta_draws(ce_draws1.0)

