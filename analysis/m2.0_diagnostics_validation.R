# ------------------------------------------------------------------------------
# model diagnostics and validation - 8-week EID outcome
#
# purpose: post-processing of m2.0 
#
# includes:
#   - convergence diagnostics: trace, autocorrelation, pairs
#   - posterior predictive checks
#   - model estimates and credible intervals
#   - icc calculation
#
# inputs:
#   - brms model object (m2.0.rds)
#   - posterior draws (p2.0.rds)
#   - constants: veid_sites
#
# outputs: diagnostics/, validation/, model_estimates/
#
# NOTE: for documentation only; model object m2.0 and posterior draws p2.0 not available due to confidentiality
# ------------------------------------------------------------------------------

# ----- set up -----
dir.create(paste0(path_sub_models, "/diagnostics"))
dir.create(paste0(path_sub_models, "/validation"))
dir.create(paste0(path_sub_models, "/model_estimates"))

# set plot theme
color_scheme_set("brightblue")

# posterior
## NOTE: m2.0 and p2.0, which are tied to individual-level testing and treatment data, are not available due to 
## confidentiality / small numbers of infants
p2.0 <- as_draws_df(m2.0)


#----- convergence -----
# trace
mcmc_trace_highlight(m2.0)

p2.0_params <- p2.0[,1:10]

colnames(p2.0_params) <- c("gamma[0]", "epsilon[0]",  "gamma[1]", "gamma[2]", "gamma[3]", "epsilon[1]", 
                    "sigma^2", "rho", "k", "hu")

p2.1 <- mcmc_trace(p2.0_params, facet_args = list(labeller = ggplot2::label_parsed)) +
    scale_x_continuous(breaks = seq(0, 10000, by = 2000),
                       labels = c("0", "2K", "4K", "6K", "8K", "10K")) +
    labs(x = "Iteration", tag = "B") +
    theme_minimal() +
    theme(legend.position = "none") +
    NULL

p2.1

ggsave("diagnostics/m2.0_trace.png",
       path = path_save,
       width = 2200,
       height = 1800,
       units = "px",
       dpi = 300)


# autocorrelation
mcmc_acf(m2.0, regex_pars = c("b_", "sd_"))

ggsave("diagnostics/m2.0_afc.png",
       path = path_save,
       width = 7200,
       height = 3600,
       units = "px",
       dpi = 300)


# pairs
theme_set(theme_default() + plot_bg(fill = "white", color = "transparent"))
pairs_plot <- pairs(m2.0, diag_fun = "dens")
plots <- pairs_plot$bayesplots

## rename parameters
plots[[1]] <- plots[[1]] + labs(subtitle = expression(gamma[0]))
plots[[12]] <- plots[[12]] + labs(subtitle = expression(epsilon[0]))
plots[[23]] <- plots[[23]] + labs(subtitle = expression(gamma[1]))
plots[[34]] <- plots[[34]] + labs(subtitle = expression(gamma[2]))
plots[[45]] <- plots[[45]] + labs(subtitle = expression(gamma[3]))
plots[[56]] <- plots[[56]] + labs(subtitle = expression(epsilon[1]))
plots[[67]] <- plots[[67]] + labs(subtitle = expression(tau[1]^2))
plots[[78]] <- plots[[78]] + labs(subtitle = expression(tau[2]^2))
plots[[89]] <- plots[[89]] + labs(subtitle = expression(sigma^2))
plots[[100]] <- plots[[100]] + labs(subtitle = expression(rho))

## reconstruct grid
bayesplot_grid(plots = plots)
## save 1650x1150 px

#----- fit -----
# cost
p2.2 <- pp_check(m2.0, resp = "costinfant", ndraws = 200) +
    scale_x_continuous(limits = c(0, 225)) +
    labs(x = "Cost per infant", y = "Density", tag = "A") +
    theme_classic() 

p2.2

ggsave("validation/m2.0_ppcheck_cost.png",
       path = path_save,
       width = 2200,
       height = 1800,
       units = "px",
       dpi = 300)

# eid uptake
p2.3 <- pp_check(m2.0, resp = "poceid8wks", ndraws = 200) +
    #scale_x_continuous(limits = c(0, 2)) +
    scale_y_continuous(limits = c(0, 10)) +
    labs(x = "Probability of 8-week PoC EID test", tag = "C") +
    theme_classic()

p2.3

ggsave("validation/m2.0_ppcheck_eid.png",
       path = path_save,
       width = 2200,
       height = 1800,
       units = "px",
       dpi = 300)


# cost tail behavior
p2.2.1 <- pp_check(m2.0, resp = "costinfant", type = "stat", stat = "max")
p2.2.2 <- pp_check(m2.0, resp = "costinfant", type = "stat", stat = "sd")

p2.2.1 + p2.2.2

ggsave("validation/m2.0_ppcheck_cost_tail.png",
       path = path_save,
       width = 5400,
       height = 1800,
       units = "px",
       dpi = 300)


# ----- model estimates EDA -----
# model estimates
m2.0_sum <- summary(m2.0)

fe <- m2.0_sum$fixed %>%
    as.data.frame() %>%
    rownames_to_column("parameter") %>%
    mutate(type = "fixed") 

re <- m2.0_sum$random %>%
    map_df(~ as.data.frame(.x) %>% rownames_to_column("parameter"), .id = "group") %>%
    mutate(type = "random")

m2.0_sum <- bind_rows(fe, re)

write_csv(m2.0_sum, file = paste0(path_save, "model_estimates/m2.0_summary.csv"))

# model estimates vis
p2.4 <- mcmc_plot(m2.0,
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

p2.5 <- mcmc_plot(m2.0,
                type = "intervals",
                variable = c("_poceid8wks_"),
                regex = TRUE,
                prob = 0.9) +
    scale_x_continuous(limits = c(-15, 10)) +
    scale_y_discrete(limits = rev,
                     labels = c(expression(sigma^2),
                                expression(epsilon[1]),
                                expression(epsilon[0]))
    ) +
    labs(x = "Posterior mean (95% CrI)", y = "Parameter", tag = "C") +
    theme(axis.text.y = element_text(size = 14)) +
    NULL

p2.4 + p2.5

ggsave("model_estimates/m2.0_est_intervals.png",
       path = path_save,
       width = 3600,
       height = 1800,
       units = "px",
       dpi = 300)


# ----- icc calc -----
# cost
icc_cost <- p2.0 %>%
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

icc_cost

# eid uptake
icc_eid <- p2.0 %>%
    mutate(
        var_u = `sd_country:cluster_num__poceid8wks_Intercept`^2,
        var_res = pi^2 / 3,
        icc = var_u / (var_u + var_res)
    ) %>%

    summarise(
        mean = mean(icc),
        median = median(icc),
        ll = quantile(icc, 0.025),
        ul = quantile(icc, 0.975)
    ) %>%
    mutate(resp = "eid")

icc_eid

m2.0_icc <- bind_rows(icc_cost, icc_eid)

write_csv(m2.0_icc, paste0(path_save, "model_estimates/m2.0_icc.csv"))

