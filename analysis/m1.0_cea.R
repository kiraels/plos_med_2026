# ------------------------------------------------------------------------------
# decision analysis for m1.0
#
# purpose: 
#   - compute nmb at multiple wtp thresholds -> mean, 95% CrI, prob cost-effective
#
# inputs:
#   - ce_draws1.0: posterior draws of cost and effect
#   - wtp thresholds (lambda)
#
# outputs:
#   - summary tables with mean nmb, 95% CrI, Pr(cost-effective)
# ------------------------------------------------------------------------------

# ----- ce draws -----
d <- ce_draws1.0

# ----- cost and effect summary -----
# Note:
# hesim::cea() computes expected costs and effects and derives icers from expectations, not from ratio of posterior draw-level differences
# draw-level icers retained in ce_draws1.0 -> cep for uncertainty vis

ce_results1.0 <- hesim::cea(
    d, 
    sample = "draw", 
    strategy = "studyarm",
    grp = "country", 
    e = "E", 
    c = "C"
)

r4.1 <- ce_results1.0[[1]]
r4.1

write.csv(r4.1, file = paste0(path_sub_results, "/m1.0_ce_results.csv"))


# ----- icer summary -----

cea_results_moz1.0 <- hesim::cea_pw(
    d[d$country == "Mozambique",], 
    k = seq(0, 50000, 100), 
    sample = "draw", 
    strategy = "studyarm",
    e = "E",
    c = "C",
    comparator = "SoC"
)

r4.2_moz <- cea_results_moz1.0[[1]]
r4.2_moz

cea_results_tan1.0 <- hesim::cea_pw(
    d[d$country == "Tanzania",], 
    k = seq(0, 50000, 100), 
    sample = "draw", 
    strategy = "studyarm",
    e = "E",
    c = "C",
    comparator = "SoC"
)

r4.2_tan <- cea_results_tan1.0[[1]]
r4.2_tan

write.csv(r4.2_moz, file = paste0(path_sub_results, "/m1.0_icer_moz.csv"))
write.csv(r4.2_tan, file = paste0(path_sub_results, "/m1.0_icer_tan.csv"))
