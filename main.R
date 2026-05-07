# ------------------------------------------------------------------------------
# NOTE ON REPRODUCIBILITY
#
# repository intended to provide transparency of the data processing, modeling, and analysis code used in the study
#
# due to confidentiality, individual-level merged cost-effectiveness outcome dataset (ce_exp_fw) cannot be shared ->
# scripts that require these data are included for documentation purposes but are not executable
#
# ------------------------------------------------------------------------------

#---- setup -----
library(here)


#---- config ----
# **first copy /config/paths_template.R and add user-specific paths, then run:
source("config/paths.R")


#----- /utils -----
source("utils/load_packages.R")
source("utils/helper_functions.R")
source("utils/constants.R")
source("utils/plot_style.R")


#----- /prep -----
# see /prep/prep_data.R for raw data anonymization (documentation only)

# NOTE: these scripts require confidential individual-level linked cost and effectiveness outcome data and are provided for documentation only

#source("prep/merge_costs_outcomes.R") 
#source("prep/iut_prob.R") 


# load public data
source("prep/load_data.R")


#----- /analysis/ *descriptive -----
# descriptive results
source("analysis/desc_outcomes.R")
source("analysis/desc_costs.R") # note: r3.2 documentation only; ce_exp_fa not available due to confidentiality


#----- /analysis/ *models -----
# NOTE: these scripts require confidential individual-level linked cost and effectiveness outcome data and are provided for documentation only

## 1wk ART
#source("analysis/m1.0_fit_model.R") 
#source("analysis/m1.0_diagnostics_validation.R") 

## 8wk EID
#source("analysis/m2.0_fit_model.R") 
#source("analysis/m2.0_diagnostics_validation.R") 


#----- /analysis/ *cost-effectiveness -----

## 1wk ART
#source("analysis/m1.0_calculate_draws.R") # note documentation only; ce_exp_fw not available due to confidentiality
source("analysis/m1.0_cea.R")
source("analysis/ly_threshold.R")
source("analysis/ly_projection_CHER.R")

## 8wk EID
#source("analysis/m2.0_calculate_draws.R") # note documentation only; ce_exp_fw not available due to confidentiality
source("analysis/m2.0_cea.R")

#----- /sensitivity -----
source("sensitivity/one_way_Ct.R")

# NOTE: the following scripts require confidential individual-level linked cost and effectiveness outcome data and are provided for documentation only 
#source("sensitivity/m1.1_fit_model_prior_sens.R")
#source("sensitivity/m2.1_fit_model_prior_sens.R")
#source("sensitivity/iut_icer.R") # note: for documentation only; results provided in iut_icer_sensitivity.csv

#----- /figures -----
# main figures
## fig 1 is a process diagram (no code needed)
source("figures/fig2.R") 
source("figures/fig3_r2.R")
source("figures/fig4.R")
source("figures/fig5.R")
source("figures/fig6.R")
source("figures/fig7.R") 

## supplement figures
source("figures/figS1.R") 
source("figures/figS2_r2.R")
source("figures/figS3.R")


