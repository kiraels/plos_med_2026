# ------------------------------------------------------------------------------
# analysis/m2.0_fit_model.R
#
# purpose: fit joint model m2.0  
#       * per infant cost (hurdle gamma)
#       * prob infant EID within 8 weeks (Bernoulli)
#
# inputs:
#   - analysis dataset `ce_exp_fw` (not shared)
#   - model config read from yaml
#
# outputs:
#   - brms model object (.rds) saved in model_objects/ 
#   - posterior draws (.csv) for downstream analysis
#
# NOTE: part of model-fitting pipeline, no config authorization workflow
# ------------------------------------------------------------------------------

#----- setup -----
# directories
dir.create(paste0(path_sub_models, "/model_objects"))
dir.create(paste0(path_sub_models, "/posterior"))

#----- load config -----
m2.0_config <- yaml::read_yaml("config/models/m2.0_config.yaml")

# ----- run -----
m2.0 <- brm(
    bf(as.formula(m2.0_config$model$formula$cost_infant),
       family = hurdle_gamma()) + 
        bf(as.formula(m2.0_config$model$formula$poc_eid_8wks),
           family = bernoulli()) +
        set_rescor(FALSE),
    data = get(m2.0_config$model$data),
    prior = c(m2.0_config$priors$prior_1, m2.0_config$priors$prior_2, 
              m2.0_config$priors$prior_3, m2.0_config$priors$prior_4),
    iter = m2.0_config$mcmc$iter,
    warmup = m2.0_config$mcmc$warmup,
    control = list(
        adapt_delta = m2.0_config$mcmc$adapt_delta,
        max_treedepth = m2.0_config$mcmc$max_treedepth
    ),
    chains = m2.0_config$mcmc$chains,
    cores = m2.0_config$mcmc$cores,
    seed = m2.0_config$mcmc$seed
)

# ----- save model object -----
saveRDS(m2.0, file = paste0(path_sub_models, "/model_objects/m2.0.rds"))

# ----- save posterior only ----
# lightweight object for downstream analysis/sharing
p2.0 <- as_draws_df(m2.0)
saveRDS(p2.0, file = paste0(path_sub_models, "/posterior/m2.0_posterior_draws.rds"))
write_csv(p2.0, file = paste0(path_sub_models, "/posterior/m2.0_8wk_eid_posterior_draws.csv"))
