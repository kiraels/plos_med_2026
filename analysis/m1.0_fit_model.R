# ------------------------------------------------------------------------------
# analysis/m1.0_fit_model.R
#
# purpose: fit joint model m1.0 
#       * per infant cost (hurdle gamma)
#       * prob infant ART within 1 week (Bernoulli)
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
m1.0_config <- yaml::read_yaml("config/models/m1.0_config.yaml")

# ----- run -----
m1.0 <- brm(
    bf(as.formula(m1.0_config$model$formula$cost_infant),
       family = hurdle_gamma()) + 
        bf(as.formula(m1.0_config$model$formula$ve_art),
           family = bernoulli()) +
        set_rescor(FALSE),
    data = get(m1.0_config$model$data),
    prior = c(m1.0_config$priors$prior_1, m1.0_config$priors$prior_2, 
              m1.0_config$priors$prior_3, m1.0_config$priors$prior_4),
    iter = m1.0_config$mcmc$iter,
    warmup = m1.0_config$mcmc$warmup,
    control = list(
        adapt_delta = m1.0_config$mcmc$adapt_delta,
        max_treedepth = m1.0_config$mcmc$max_treedepth
    ),
    chains = m1.0_config$mcmc$chains,
    cores = m1.0_config$mcmc$cores,
    seed = m1.0_config$mcmc$seed
)

# ----- save model object -----
saveRDS(m1.0, file = paste0(path_sub_models, "/model_objects/m1.0.rds"))

# ----- save posterior only ----
# lightweight object for downstream analysis
p1.0 <- as_draws_df(m1.0)
saveRDS(p1.0, file = paste0(path_sub_models, "/posterior/m1.0_posterior_draws.rds"))
write_csv(p1.0, file = paste0(path_sub_models, "/posterior/m1.0_1wk_art_posterior_draws.csv"))
