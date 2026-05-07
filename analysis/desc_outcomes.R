# ------------------------------------------------------------------------------
# descriptive effectiveness outcome measures summarised as proportions, median [IQR or range]
#   - poc eid within 8 weeks
#   - art initiation within 1 week
#
# analysis/desc_outcomes.R
# ------------------------------------------------------------------------------

#----- first eid -----

# by arm
r1.1 <- outcomes_eid_uptake_8wk %>%
    group_by(studyarm) %>%
    summarise(
        n = n(),
        x = sum(poc_eid_8wks == 1),
        prop = x / n,
        med = median(age_eid, na.rm = TRUE),
        q1 = quantile(age_eid, 0.25, na.rm = TRUE),
        q3 = quantile(age_eid, 0.75, na.rm = TRUE),
        .groups = "drop"
    )

r1.1

# by country + arm
r1.2 <- outcomes_eid_uptake_8wk %>%
    group_by(country, studyarm) %>%
    summarise(
        n = n(),
        x = sum(poc_eid_8wks == 1),
        prop = x / n,
        med = median(age_eid, na.rm = TRUE),
        q1 = quantile(age_eid, 0.25, na.rm = TRUE),
        q3 = quantile(age_eid, 0.75, na.rm = TRUE),
        .groups = "drop"
    )

r1.2


#----- art initiation -----
# by arm
# NOTE: exact age at ART initiation (age_art) not available due to confidentiality
r2.1 <- outcomes_art_init_1wk %>%
    filter(hiv_pos == 1) %>%
    group_by(studyarm) %>%
    summarise(
        n = n(),
        x = sum(art == 1),
        prop = x / n
        #med = median(age_art, na.rm = TRUE),
        #min = min(age_art, na.rm = TRUE),
        #max = max(age_art, na.rm = TRUE),
    )

r2.1

# subset pos at birth with result available to site (VEID only)
r2.2 <- outcomes_art_init_1wk %>%
    filter(hiv_pos == 1 & pos_birth == 1) %>%
    group_by(studyarm) %>%
    summarise(
        n = n(),
        x = sum(ve_art == 1),
        prop = x / n,
        .groups = "drop"
    )

r2.2
