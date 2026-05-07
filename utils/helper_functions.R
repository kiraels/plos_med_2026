# helper functions

# utils/helper_functions.R
# ------------------------------------------------------------------------------

#----- discounting for capital costs -----

## present value
## compute discounted cashflows with initial + maintenance
present_value <- function(init, maint, n, r, discount = TRUE, annualization_method = c("straight", "eac")) {
    # init: initial payment in year 1
    # maint: maintenance payment each year from 2...n
    # n: number of years (>= 1)
    # r: discount rate (0.00, 0.03,...,r)
    
    method <- match.arg(annualization_method)
    
    # build cashflow vector: init at year 1, maint repeated years 2...n
    C_i <- c(init, rep(maint, n - 1))
    t <- 1:n
    
    # discounted sum
    if(discount) {
        C_i_n_total <- sum(C_i / (1 + r)^t)
    } else {
        C_i_n_total <- sum(C_i)
    }

    # annual cost
    if(method == "straight") {
        C_i_n_annual <- C_i_n_total / n
    }
    if(method == "eac") {
        C_i_n_annual <- if(r == 0) C_i_n_total / n else C_i_n_total * r / (1 - (1 + r)^(-n))
    }
    
    return(list(
        C_i_n_total = C_i_n_total,
        C_i_n_annual = C_i_n_annual
    ))
}

#----- calculate unit cost per test -----

## cost basis
# fixed (equipment, overhead) per year + variable (consumables, labor) per test
# for each site based on analyzer capacity
calc_unit_cost_per_test <- function(vol_life, fixed_costs, variable_costs, scale = c("annual", "isoweek")) {
    
    # ensure tibble and remove any rowwise/grouping
    vol_life <- as_tibble(vol_life) %>% ungroup()
    fixed_costs <- as_tibble(fixed_costs)
    variable_costs <- as_tibble(variable_costs)
    
    scale <- match.arg(scale)  # force correct input
    cost_col <- if (scale == "annual") "C_f_annual" else "C_f_weekly"
    
    group_vars <- c("country", "cluster", "cluster_num", "studyarm", if (scale == "isoweek") "isoweek")
    
    # 1. analyzer-level fixed costs (join by country + label)
    analyzer_fixed <- vol_life %>%
        left_join(
            fixed_costs %>% 
                filter(label %in% c("mpima","xpert2","xpert4")),
            by = c("country","label"),
            relationship = "many-to-many"  # multiple rows per label
        ) %>%
        mutate(
            type = "fixed",
            life_poc_total = pmax(life_poc_total, 1),
            cost_per_test = ifelse(
                label == "mpima" & cap_week == 72,
                (!!sym(cost_col)) * 2 / life_poc_total,
                (!!sym(cost_col)) / life_poc_total)        
        ) %>%
        dplyr::select(all_of(group_vars), category, type, cost_per_test)
    
    # 2. site-level fixed costs (training, upgrades, electricity, comms)
    site_fixed <- vol_life %>%
        left_join(
            fixed_costs %>% 
                filter(!(label %in% c("mpima","xpert2","xpert4"))),
            by = "country",
            relationship = "many-to-many"
        ) %>%
        group_by(across(all_of(group_vars)), category) %>%        
        summarise(
            cost_per_test = sum((!!sym(cost_col)) / pmax(life_poc_total, 1), na.rm = TRUE),
            .groups = "drop"
        ) %>%        
        mutate(type = "fixed") %>%
        dplyr::select(all_of(group_vars), category, type, cost_per_test)
    
    # 3. variable costs (per-test)
    variable <- vol_life %>%
        left_join(variable_costs, by = "country", relationship = "many-to-many") %>%
        filter(category != "drugs") %>%
        group_by(across(all_of(group_vars)), category) %>%        
        summarise(
            cost_per_test = sum(C_v, na.rm = TRUE), 
            .groups = "drop"
        ) %>%
        mutate(
            type = "variable",
        ) %>%
        dplyr::select(all_of(group_vars), category, type, cost_per_test)
    
    # 4. combine
    components <- bind_rows(analyzer_fixed, site_fixed, variable) %>%
        group_by(across(all_of(group_vars)), category) %>%        
        summarise(cost_per_test = sum(cost_per_test, na.rm = TRUE), .groups = "drop_last") %>%
        mutate(total_cost_per_test = sum(cost_per_test, na.rm = TRUE)) %>%
        ungroup()
    
    totals <- components %>%
        dplyr::select(-category, -cost_per_test) %>%
        unique() %>%
        merge(., vol_life) 
    
    return(list(
        components = components,
        totals = totals
    ))
}

#----- cost per test uncertainty -----
# bootstrapped CI for weighted mean
boot_wmean <- function(x, w, B = 10000, conf = 0.95) {
    n <- length(x)
    
    # bootstrap weighted means
    wm <- replicate(B, {
        idx <- sample(seq_len(n), replace = TRUE)
        weighted.mean(x[idx], w = w[idx])
    })
    
    # compute mean and CI
    c(
        mean = weighted.mean(x, w = w),
        lci = quantile(wm, probs = (1 - conf)/2),
        uci = quantile(wm, probs = 1 - (1 - conf)/2)
    )
}

#----- summarise cost per test -----
summarise_cost <- function(data, group_vars, outcome) {
    
    outcome <- rlang::enquo(outcome)
    
    data %>%
        group_by(across(all_of(group_vars))) %>%
        summarise(
            mean = mean(!!outcome),
            w_mean = weighted.mean(!!outcome, life_poc_total),
            ci = list(boot_wmean(!!outcome, life_poc_total)),
            median = median(!!outcome),
            q1 = quantile(!!outcome, 0.25),
            q3 = quantile(!!outcome, 0.75),
            .groups = "drop"
        ) %>%
        mutate(ll_wmean = map_dbl(ci, 2),
               ul_wmean = map_dbl(ci, 3)
               ) %>%
        dplyr::select(any_of(group_vars), mean, w_mean, ll_wmean, ul_wmean, median, q1, q3)
                      
}

#---- iu transmission rate uncertainty -----
# Bayes bootstrap for iu transmission probability 
boot_iut_prob <- function(data, m) {
    
    n <- length(data)
    avg_prob <- numeric(m)
    
    for (i in 1:m) {
        weights <- rdirichlet(1, rep(1, n))
        avg_prob[i] <- sum(weights * data)
    }
    
    return(avg_prob)
}

#----- delta ce results -----
calculate_delta_draws <- function(draws) {
    draws %>%
        pivot_wider(
            id_cols = c(draw, country),
            names_from = studyarm,
            values_from = c(C, E) 
        ) %>%
        
        mutate(
            dC = C_VEID - C_SoC,
            dE = E_VEID - E_SoC
        ) %>%
        dplyr::select(draw, country, dC, dE)
}


#----- sensitivity: one-way deterministic; cost per test -----
# fixed costs
recompute_fixed_costs <- function(fixed_mod) {
    fixed_mod %>%
        rowwise() %>%
        mutate(
            C_f_total  = present_value(init, ifelse(is.na(maint), 0, maint), n, r, annualization_method = "eac")$C_i_n_total,
            C_f_annual = present_value(init, ifelse(is.na(maint), 0, maint), n, r, annualization_method = "eac")$C_i_n_annual,
            C_f_weekly = C_f_annual / 52
        ) %>%
        ungroup()
}

# modify costs for a given param/value
modify_unit_costs <- function(param, val) {
    fixed_mod <- fixed_costs
    variable_mod <- variable_costs
    vol_life_mod <- vol_annual
    
    if(param == "n") fixed_mod <- fixed_mod %>% mutate(n = ifelse(category == "equipment", val, n))
    if(param == "r") fixed_mod <- fixed_mod %>% mutate(r = ifelse(category == "equipment", val, r))
    if(param == "t") vol_life_mod <- vol_life_mod %>% mutate(life_poc_total = pmax(1, life_poc_total * val))
    if(param == "C_c") variable_mod <- variable_mod %>% mutate(C_v = C_v * val)
    
    # **recalculate C_f_annual after changing n or r**
    fixed_mod <- recompute_fixed_costs(fixed_mod)
    
    calc_unit_cost_per_test(vol_life_mod, fixed_mod, variable_mod, scale = "annual")$totals %>%
        dplyr::select(country, country_group, cluster_num, total_cost_per_test)
}









