# constants used across project

# utils/constants.R
# ------------------------------------------------------------------------------

#----- site info -----
veid_sites <- paste("Site", c(1:7, 15:21))
soc_sites <- paste("Site", c(8:14, 22:28))

#----- lam costs -----
# costs per week
lamivudine_moz <- 2.098454 # NOTE: additional cost of 3TC per week
abc3tc_tanz <- 1.843  # NOTE: additional cost of ABC/3TC tabs per week including cost savings from proportion of infants already on ePNP

#----- wtp thresholds ----
wtp <- list(
    Mozambique = c(189, 462),  # empirical (PR), pcGDP
    Tanzania   = c(316, 1117)  # empirical (PR), pcGDP
)
