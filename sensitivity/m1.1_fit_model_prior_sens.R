# ------------------------------------------------------------------------------
# model config: m1.1 – 1-week ART primary model prior sensitivity
#
# purpose: fit joint model m1.1 to assess prior sensitivity for m1.0
#       * per infant cost (hurdle gamma)
#       * prob infant ART within 1 week (Bernoulli)
#
# inputs:
#   - analysis dataset `ce_exp_fw` 
#   - model config read from rds (or yaml)
#
# outputs:
#   - brms model object (.rds) saved in model_objects/ 
#   - posterior draws (.csv) for downstream analysis/sharing
#
# NOTE: part of model-fitting pipeline, no config authorization workflow
#   - config rds is version-controlled; YAML files exist for reference
# ------------------------------------------------------------------------------

prior_summary(m1.0)

m1.1 <- update(
    m1.0,
    prior = c(
        prior(exponential(2), class = "sd", coef = "Intercept", group = "country:cluster_num", resp = "costinfant"),
        prior(exponential(2), class = "sd", coef = "eid_total", group = "country:cluster_num", resp = "costinfant"),
        prior(normal(0, 2.5), class = "b", resp = "veart")
    ),
    iter = 2000,
    warmup = 1000,
    recompile = FALSE
    )


# ----- save model object -----
saveRDS(m1.1_prior_sens, file = paste0(path_save, "model_objects/m1.1_prior_sens.rds"))

# ----- save posterior only ----
# lightweight object for downstream analysis/sharing
p1.1 <- as_draws_df(m1.1)
saveRDS(p1.1, file = paste0(path_save, "/posterior/m1.1_posterior_draws.rds"))
write_csv(p1.1, file = paste0(path_save, "posterior/m1.1_1wk_art_posterior_draws.csv"))
