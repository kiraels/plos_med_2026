# load packages

# utils/load_packages.R
# ------------------------------------------------------------------------------

# core workflow
library(here)        # path handling
library(tidyverse)   # data manipulation + ggplot2
library(zoo)         # time series / rolling functions
library(yaml)        # read/write config files

# modeling
library(brms)        # Bayesian regression models
library(lme4)        # mixed-effects models
library(MCMCpack)    # Bayesian tools (MCMC)
library(segmented)   # segmented (piecewise) regression
library(emmeans)     # estimated marginal means
library(DescTools)   # descriptive stats + tests
library(psych)       # psychometrics, scale reliability
library(hesim)       # psa

# bayesian visualization / post-processing
library(tidybayes)   # tidy interface for Bayesian models
library(bayesplot)   # posterior plotting

# vsualization - extensions
library(ggh4x)       # facets, themes, scales
library(ggridges)    # ridge plots
library(ggpointdensity) # density scatterplots
library(patchwork)   # plot layouts

# parallelization
library(future)      # plan multisession/cluster
library(furrr)       # purrr + future for parallel map

