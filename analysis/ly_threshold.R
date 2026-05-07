#-------------------------------------------------------------------------------
# life-years break even calculation for 1wk ART
#
# purpose:
#   - estimate number of life-years per infant and early ART initiation needed to meet ce thresholds
#
# inputs:
#   - delta_draws1.0: posterior draws of incremental cost (dC) and incremental effect (dE) from m1.0 generated from analysis/m1.0_calculate_draws.R
#   - lambda_grid: willingness to pay thresholds (per ly)
#
# output:
#   - table with required life-years + uncertainty by country at empirical ce threshold & 1x GDP
#
# NOTE: analysis uses MC simulation to propagate uncertainty in dC and dE estimates
#------------------------------------------------------------------------------

delta_draws <- calculate_delta_draws(ce_draws1.0)

#----- life years break even -----
make_threshold_df <- function(y, delta_draws, lambda_grid, eps = 1e-9) {
    d <- delta_draws %>% filter(country == y)
    dC <- d$dC 
    dE <- d$dE  
    
    results <- lapply(lambda_grid, function(lam) {
        req_infant <- dC / lam
        req_per_early <- req_infant / dE
        list(
            lambda = lam,
            median_infant = median(req_infant, na.rm = TRUE),
            ci_infant = quantile(req_infant, c(0.025, 0.975), na.rm = TRUE),
            median_per_early = median(req_per_early[is.finite(req_per_early)], na.rm = TRUE),
            ci_per_early = quantile(req_per_early[is.finite(req_per_early)], c(0.025, 0.975), na.rm = TRUE),
            prop_dE_near_zero = mean(abs(dE) < eps),
            raw_req_per_early = req_per_early
        )
    })
    
    # summary
    df <- map_dfr(results, function(x) {
        tibble(
            lambda = x$lambda,
            median_infant = x$median_infant,
            ci_infant_low = x$ci_infant[1],
            ci_infant_high = x$ci_infant[2],
            median_per_early = x$median_per_early,
            ci_per_early_low = x$ci_per_early[1],
            ci_per_early_high = x$ci_per_early[2],
            prop_dE_near_zero = x$prop_dE_near_zero,
            country = y
        )
    })
    
    # raw draws for density plots
    raw_df <- map2_dfr(results, lambda_grid, function(res, lam) {
        tibble(lambda = lam, req_per_early = res$raw_req_per_early, country = y)
    })
    
    list(summary = df, raw = raw_df)
}

#----- run per country, combine -----
lambda_grid <- as.vector(do.call(rbind, wtp))
ly_moz <- make_threshold_df("Mozambique", delta_draws, lambda_grid)
ly_tan <- make_threshold_df("Tanzania", delta_draws, lambda_grid)

ly_threshold <- bind_rows(ly_moz$summary, ly_tan$summary)
ly_threshold_raw <- bind_rows(ly_moz$raw, ly_tan$raw)

ly_table <- ly_threshold %>%
    mutate(
        `Required life-years per infant` = sprintf("%.2f (%.2f–%.2f)", median_infant, ci_infant_low, ci_infant_high),
        `Required life-years per early ART` = sprintf("%.2f (%.2f–%.2f)", median_per_early, ci_per_early_low, ci_per_early_high)
    ) %>%
    dplyr::select(country, lambda, `Required life-years per infant`, `Required life-years per early ART`)

#----- output -----
ly_table[c(1,3,6,8),]

rm(ly_moz, ly_tan)


