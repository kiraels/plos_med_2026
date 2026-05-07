# ------------------------------------------------------------------------------
# load data available in release
# ------------------------------------------------------------------------------

#----- costs -----

# BASE CASE: 5-year lifespan, 3% discounting
fixed_costs <- readxl::read_excel(here("data/cost_inputs.xlsx"), sheet = "fixed") %>%
    rowwise() %>%
    mutate(C_f_total = present_value(init, ifelse(is.na(maint), 0, maint), n, r, annualization_method = "eac")$C_i_n_total,
           C_f_annual = present_value(init, ifelse(is.na(maint), 0, maint), n, r, annualization_method = "eac")$C_i_n_annual,
           C_f_weekly = C_f_annual / 52
    )

# variable costs
variable_costs <- readxl::read_excel(here("data/cost_inputs.xlsx"), sheet = "variable") %>%
    filter(!(category %in% c("equipment", "overhead")))



#----- testing volume -----
vol_annual <- readRDS(here("data/vol_annual_redacted.rds"))

#-----  cost per test -----
cost_basis_fa <- readRDS(here("data/cost_basis_fa_redacted.rds"))
cost_basis_fw <- readRDS(here("data/cost_basis_fw_redacted.rds"))


#----- effectiveness outcomes -----
outcomes_eid_uptake_8wk <- readRDS(here("data/outcomes_eid_uptake_8wk_redacted.rds"))
outcomes_art_init_1wk <- readRDS(here("data/outcomes_art_init_1wk_redacted.rds"))


#----- model draws -----
ce_draws1.0 <- read_csv(paste0(path_sub_results, "/m1.0_ce_draws.csv"))
ce_draws2.0 <- read_csv(paste0(path_sub_results, "/m2.0_ce_draws.csv"))


#----- sensitivity -----
iut_sens <- read_csv(paste0(path_sub_results, "/sensitivity/iut_icer_sensitivity.csv"))
                                                