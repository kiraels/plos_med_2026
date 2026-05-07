#-------------------------------------------------------------------------------
# overlay life-year gains from CHER on life-years threshold analysis
#
# inputs:
#   - CHER trial data (Cotton et al., 2013):
#       - deaths_early, py_early: mortality and person-years in early ART group
#       - deaths_def, py_def: mortality and person-years in the delayed ART group
#       - followup_years: duration of follow-up in years
#   - ly_threshold: summary required life-years from threshold analysis generated in analysis/ly_threshold.R
#   
# outputs:
#   - cher_est: median (95% CrI) life-years gained per infant (based on MC simulation of CHER trial rates)
#
# NOTE: this analysis uses MC simulation to propogate uncertainty in mortality rates to life-years
#-------------------------------------------------------------------------------


#----- CHER moratlity data -----

deaths_early <- 10
py_early <- 205
deaths_def <- 20
py_def <- 94
followup_years <- 4.8

# rates
rate_early <- deaths_early / py_early
rate_def   <- deaths_def / py_def

# standard errors (Poisson approx.)
se_early <- sqrt(deaths_early) / py_early
se_def   <- sqrt(deaths_def) / py_def

#----- MC sim -----
set.seed(12)
n_sims <- 20000
rate_early_samp <- rnorm(n_sims, rate_early, se_early)
rate_def_samp   <- rnorm(n_sims, rate_def, se_def)

# avoid negatives
rate_early_samp <- pmax(rate_early_samp, 0)
rate_def_samp   <- pmax(rate_def_samp, 0)

# ----- ly per infant in CHER -----
ly_gain_per_infant <- (rate_def_samp - rate_early_samp) * followup_years
quantile(ly_gain_per_infant, c(0.025, 0.5, 0.975))

cher_est <- tibble(
    est = median(ly_gain_per_infant),
    low = quantile(ly_gain_per_infant, 0.025),
    high = quantile(ly_gain_per_infant, 0.975)
)