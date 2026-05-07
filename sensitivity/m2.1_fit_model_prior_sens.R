# ------------------------------------------------------------------------------
# model config: m2.1 – 8-week EID primary model prior sensitivity
#
# purpose: fit joint model m2.1 to assess prior sensitivity for m2.0
#       * per infant cost (hurdle gamma)
#       * prob infant EID within 8 weeks (Bernoulli)
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

prior_summary(m2.0)

m2.1 <- update(
    m2.0,
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
saveRDS(m2.1_prior_sens, file = paste0(path_save, "model_objects/m1.1_prior_sens.rds"))

# ----- save posterior only ----
# lightweight object for downstream analysis/sharing
p2.1 <- as_draws_df(m2.1)
saveRDS(p2.1, file = paste0(path_save, "/posterior/m2.1_posterior_draws.rds"))
write_csv(p2.1, file = paste0(path_save, "posterior/m2.1_1wk_art_posterior_draws.csv"))
