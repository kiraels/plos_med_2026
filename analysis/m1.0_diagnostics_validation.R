# ------------------------------------------------------------------------------
# model diagnostics and validation - 1-week ART outcome
#
# purpose: post-processing of m1.0 
#
# includes:
#   - convergence diagnostics: trace, autocorrelation, pairs
#   - posterior predictive checks
#   - model estimates and credible intervals
#   - icc calculation
#
# inputs:
#   - brms model object (m1.0.rds) 
#   - posterior draws (p1.0.rds)
#   - constants: veid_sites
#
# outputs: diagnostics/, validation/, model_estimates/
#
# NOTE: for documentation only; model object m1.0 and posterior draws p1.0 not available due to confidentiality
# ------------------------------------------------------------------------------

# ----- set up -----
dir.create(paste0(path_sub_models, "/diagnostics"))
dir.create(paste0(path_sub_models, "/validation"))
dir.create(paste0(path_sub_models, "/model_estimates"))

# set plot theme
color_scheme_set("brightblue")

# posterior
## NOTE: m1.0 and p1.0, which are tied to individual-level testing and treatment data, are not available due to 
## confidentiality / small numbers of infants
p1.0 <- as_draws_df(m1.0)


#----- convergence -----
# trace
mcmc_trace_highlight(m1.0)

p1.0_params <- p1.0[,1:13]

colnames(p1.0_params) <- c("gamma[0]", "delta[0]",  "gamma[1]", "gamma[2]", "gamma[3]", "delta[1]", "delta[2]",
                    "delta[3]", "tau[1]^2", "tau[2]^2", "rho", "k", "hu")

p1.1 <- mcmc_trace(p1.0_params, facet_args = list(labeller = ggplot2::label_parsed)) +
    scale_x_continuous(breaks = seq(0, 10000, by = 2000),
                       labels = c("0", "2K", "4K", "6K", "8K", "10K")) +
    labs(x = "") +
    theme_minimal() +
    theme(legend.position = "none") +
    NULL

p1.1

ggsave("diagnostics/m1.0_trace.png",
       path = path_sub_models,
       width = 2200,
       height = 1800,
       units = "px",
       dpi = 300)

# autocorrelation
mcmc_acf(m1.0, regex_pars = c("b_", "sd_"))

ggsave("diagnostics/m1.0_afc.png",
       path = path_sub_models,
       width = 7200,
       height = 3600,
       units = "px",
       dpi = 300)


# pairs
theme_set(theme_default() + plot_bg(fill = "white", color = "transparent"))
pairs_plot <- pairs(m1.0, diag_fun = "dens")
plots <- pairs_plot$bayesplots

## rename parameters
plots[[1]] <- plots[[1]] + labs(subtitle = expression(gamma[0]))
plots[[13]] <- plots[[13]] + labs(subtitle = expression(delta[0]))
plots[[25]] <- plots[[25]] + labs(subtitle = expression(gamma[1]))
plots[[37]] <- plots[[37]] + labs(subtitle = expression(gamma[2]))
plots[[49]] <- plots[[49]] + labs(subtitle = expression(gamma[3]))
plots[[61]] <- plots[[61]] + labs(subtitle = expression(delta[1]))
plots[[73]] <- plots[[73]] + labs(subtitle = expression(delta[2]))
plots[[85]] <- plots[[85]] + labs(subtitle = expression(delta[3]))
plots[[97]] <- plots[[97]] + labs(subtitle = expression(tau[1]^2))
plots[[109]] <- plots[[109]] + labs(subtitle = expression(tau[2]^2))
plots[[121]] <- plots[[121]] + labs(subtitle = expression(rho))

## reconstruct grid
bayesplot_grid(plots = plots)
## save 1650x1150 px


# ----- fit -----
# cost
p1.2 <- pp_check(m1.0, resp = "costinfant", ndraws = 200) +
    scale_x_continuous(limits = c(0, 225)) +
    labs(x = "Cost per infant", y = "Density", tag = "A") +
    theme_classic() 

p1.2

ggsave("validation/m1.0_ppcheck_cost.png",
       path = path_sub_models,
       width = 2200,
       height = 1800,
       units = "px",
       dpi = 300)

# early ART
p1.3 <- pp_check(m1.0, resp = "veart", ndraws = 200) +
    #scale_x_continuous(limits = c(0.4, 1.0)) +
    scale_y_continuous(limits = c(0, 50)) +
    labs(x = "Probability of 1-week ART uptake", tag = "B") +
    theme_classic()

p1.3

p1.3_inset <- pp_check(m1.0, resp = "veart", ndraws = 1000) +
    scale_x_continuous(limits = c(0.4, 1.0), breaks = seq(0.4, 1.0, by = 0.2)) +
    labs(x = "Probability of 1-week ART uptake", tag = "B") +
    theme_classic()

p1.3 + p1.3_inset

ggsave("validation/m1.0_ppcheck_veart.png",
       path = path_sub_models,
       width = 3600,
       height = 1800,
       units = "px",
       dpi = 300)


# cost tail behavior
p1.2.1 <- pp_check(m1.0, resp = "costinfant", type = "stat", stat = "max")
p1.2.2 <- pp_check(m1.0, resp = "costinfant", type = "stat", stat = "sd")

p1.2.1 + p1.2.2

ggsave("validation/m1.0_ppcheck_cost_tail.png",
       path = path_sub_models,
       width = 5400,
       height = 1800,
       units = "px",
       dpi = 300)



# ----- model estimates EDA -----

# model estimates
m1.0_sum <- summary(m1.0)

fe <- m1.0_sum$fixed %>%
    as.data.frame() %>%
    rownames_to_column("parameter") %>%
    mutate(type = "fixed") 

re <- m1.0_sum$random %>%
    map_df(~ as.data.frame(.x) %>% rownames_to_column("parameter"), .id = "group") %>%
    mutate(type = "random")

m1.0_sum <- bind_rows(fe, re)

write_csv(m1.0_sum, file = paste0(path_save, "model_estimates/m1.0_summary.csv"))

# model estimates vis
p1.4 <- mcmc_plot(m1.0,
                type = "intervals",
                variable = c("_costinfant_"),
                regex = TRUE,
                prob = 0.9) +
    scale_x_continuous(limits = c(-1, 4)) +
    scale_y_discrete(limits = rev,
                     labels = c(expression(rho), 
                                expression(tau[1]^2),
                                expression(tau[0]^2),
                                expression(gamma[3]),
                                expression(gamma[2]),
                                expression(gamma[1]), 
                                expression(gamma[0]))
    ) +
    labs(x = "Posterior mean (95% CrI)", y = "Parameter", tag = "A") +
    theme(axis.text.y = element_text(size = 14)) +
    NULL

p1.5 <- mcmc_plot(m1.0,
                type = "intervals",
                variable = c("_veart_"),
                regex = TRUE,
                prob = 0.9) +
    scale_x_continuous(limits = c(-15, 10)) +
    scale_y_discrete(limits = rev,
                     labels = c(expression(delta[3]),
                                expression(delta[2]),
                                expression(delta[1]),
                                expression(delta[0]))
    ) +
    labs(x = "", y = "Parameter", tag = "B") +
    theme(axis.text.y = element_text(size = 14)) +
    NULL

p1.4 + p1.5

ggsave("model_estimates/m1.0_est_intervals.png",
       path = path_save,
       width = 3600,
       height = 1800,
       units = "px",
       dpi = 300)


conditional_effects(m1.0, effects = "studyarm:eid_total")
conditional_effects(m1.0, effects = "studyarm")
conditional_effects(m1.0, effects = "eid_total")


# ----- icc calc -----
# cost
m1.0_icc <- p1.0 %>%
    mutate(
        var_u = `sd_country:cluster_num__costinfant_Intercept`^2,
        mu = exp(b_costinfant_Intercept),
        var_res = mu^2 / shape_costinfant,
        icc = var_u / (var_u + var_res)
    ) %>%

    summarise(
        mean = mean(icc),
        median = median(icc),
        ll = quantile(icc, 0.025),
        ul = quantile(icc, 0.975)
    ) %>%
    mutate(resp = "cost")

m1.0_icc

write_csv(m1.0_icc, paste0(path_save, "model_estimates/m1.0_icc.csv"))

