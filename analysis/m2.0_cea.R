# ------------------------------------------------------------------------------
# decision analysis for m2.0 
#
# purpose: 
#   - compute nmb at multiple wtp thresholds -> mean, 95% CrI, prob cost-effective
#
# inputs:
#   - ce_draws2.0: posterior draws of cost and effect
#   - wtp thresholds (lambda)
#
# outputs:
#   - summary tables with mean nmb, 95% CrI, Pr(cost-effective)
# ------------------------------------------------------------------------------

# ----- ce draws -----
d <- ce_draws2.0 

# ----- cost and effect summary -----
# Note:
# hesim::cea() computes expected costs and effects and derives icers from expectations, not from ratio of posterior draw-level differences
# draw-level icers retained in ce_draws2.0 -> cep for uncertainty vis

ce_results2.0 <- hesim::cea(
    d, 
    sample = "draw", 
    strategy = "studyarm",
    grp = "country", 
    e = "E", 
    c = "C"
)

r4.3 <- ce_results2.0[[1]]
r4.3

write.csv(r4.3, file = paste0(path_sub_results, "/m2.0_ce_results.csv"))


# ----- icer summary -----
cea_results_moz2.0 <- hesim::cea_pw(
    d[d$country == "Mozambique",], 
    k = seq(0, 1000, 10), 
    sample = "draw", 
    strategy = "studyarm",
    e = "E", 
    c = "C",
    comparator = "SoC"
)

r4.4_moz <- cea_results_moz2.0[[1]]
r4.4_moz

cea_results_tan2.0 <- hesim::cea_pw(
    d[d$country == "Tanzania",], 
    k = seq(0, 1000, 10), 
    sample = "draw", 
    strategy = "studyarm",
    e = "E", 
    c = "C",
    comparator = "SoC"
)

r4.4_tan <- cea_results_tan2.0[[1]]
r4.4_tan

write.csv(r4.4_moz, file = paste0(path_sub_results, "/m2.0_icer_moz.csv"))
write.csv(r4.4_tan, file = paste0(path_sub_results, "/m2.0_icer_tan.csv"))

