# ------------------------------------------------------------------------------
# create datasets for external sharing 
#   - make id anonymous
#   - remove cluster
#   - label study arms
#
# NOTE: for documentation only; outcomes, cost_basis_*, vol_life_annual not publicly available due to confidentiality 
# ------------------------------------------------------------------------------

#----- testing volume -----
vol_annual_redact <- vol_life_annual %>%
    mutate(cluster = NA_character_) %>%
    dplyr::select(country, cluster_num, studyarm, life_poc_total)


#----- outcomes -----
set.seed(12)

outcomes_eid_uptake_8wk_redact <- outcomes %>%
    ungroup() %>%
    slice_sample(prop = 1) %>%   # randomize row order
    mutate(ID = paste0("E", row_number()),
           cluster = NA_character_) %>%
    dplyr::select(ID:country_group, eid_total, poc_eid_8wks)

outcomes_art_init_1wk_redact <- outcomes %>%
    ungroup() %>%
    filter(hiv_pos == 1) %>%
    slice_sample(prop = 1) %>%   # randomize row order
    mutate(ID = paste0("P", row_number()),
           cluster = NA_character_,
           cluster_num = NA_character_) %>%
    dplyr::select(ID:country_group, pos_birth, ve_art)


#----- cost basis -----
## fixed costs allocated annually
cost_basis_fa_redact <- list()

cost_basis_fa_redact$components <- cost_basis_annual$components %>%
    mutate(cluster = NA_character_)

cost_basis_fa_redact$totals <- cost_basis_annual$totals %>%
    mutate(cluster = NA_character_)

## fixed costs allocated weekly
cost_basis_fw_redact <- list() 

cost_basis_fw_redact$components <- cost_basis_weekly$components %>%
    mutate(cluster = NA_character_)

cost_basis_fw_redact$totals <- cost_basis_weekly$totals %>%
    mutate(cluster = NA_character_)
