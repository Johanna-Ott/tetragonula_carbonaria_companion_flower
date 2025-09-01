#Model selection
#Based on 03_model_development.R, I created multiple models trying to get 
#the best one. 
#Here I will try to find out which one is actually the best fit


#Since for all three response variables, the differences in model validity
#(as determined by cross validation (LOO)) for autocor_as_rf with and without 
#interaction are negligible, I decided to go for the models including the interaction. 

#packages----
library(brms)

#loading in the data
lapply(list.files("output/model_development/model_selection_hex/", 
                  pattern = "\\.RData$", full.names = TRUE), 
       load, envir = .GlobalEnv)


# LOO comparison ----------------------------------------------------------



#The LOO  (leave-one-out cross) validation
#assesses the predictive ability of posterior distributions
#elpd estimate for each model, the higher value the better the fit.
loo(abs_hex_autocor,
    abs_hex_autocor_as_rf,
    abs_hex_autocor_as_rf_interaction,
    abs_hex_autocor_interaction,
    abs_hex_no_autocor_interaction,
    abs_hex_interaction_day_as_rf,
    abs_hex_no_autocor,
    compare = TRUE)

plot(absolute_hexdistance)[2]
ggsave(file="output/plots/model_fit/abs_hex_trace.png",
       height=5, width=5, bg="white")

#Results
# Output of model 'abs_hex_autocor':
#   
#   Computed from 16000 by 1188 log-likelihood matrix.
# 
# Estimate    SE
# elpd_loo  -3230.1  62.4
# p_loo       654.7  38.1
# looic      6460.2 124.8

#   MCSE of elpd_loo is NA.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.4, 1.9]).
# 
# Pareto k diagnostic values:
#   Count Pct.    Min. ESS
# (-Inf, 0.7]   (good)     1071  90.2%   188     
# (0.7, 1]   (bad)        90   7.6%   <NA>    
#   (1, Inf)   (very bad)   27   2.3%   <NA>    
#   See help('pareto-k-diagnostic') for details.
# 
# Output of model 'abs_hex_autocor_as_rf':
#   
#   Computed from 16000 by 1188 log-likelihood matrix.
# 
# Estimate   SE
# elpd_loo  -2680.1 31.0
# p_loo        77.1  3.5
# looic      5360.2 62.0

#   MCSE of elpd_loo is 0.1.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.4, 2.0]).
# 
# All Pareto k estimates are good (k < 0.7).
# See help('pareto-k-diagnostic') for details.
# 
# Output of model 'abs_hex_autocor_as_rf_interaction':
#   
#   Computed from 16000 by 1188 log-likelihood matrix.
# 
# Estimate   SE
# elpd_loo  -2680.6 31.0
# p_loo        77.4  3.5
# looic      5361.2 62.0

#   MCSE of elpd_loo is 0.1.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.4, 1.9]).
# 
# All Pareto k estimates are good (k < 0.7).
# See help('pareto-k-diagnostic') for details.
# 
# Output of model 'abs_hex_autocor_interaction':
#   
#   Computed from 16000 by 1188 log-likelihood matrix.
# 
# Estimate    SE
# elpd_loo  -3232.1  62.4
# p_loo       656.7  38.0
# looic      6464.3 124.8

#   MCSE of elpd_loo is NA.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.4, 2.5]).
# 
# Pareto k diagnostic values:
#   Count Pct.    Min. ESS
# (-Inf, 0.7]   (good)     1061  89.3%   185     
# (0.7, 1]   (bad)        99   8.3%   <NA>    
#   (1, Inf)   (very bad)   28   2.4%   <NA>    
#   See help('pareto-k-diagnostic') for details.
# 
# Output of model 'abs_hex_interaction_day_as_rf':
#   
#   Computed from 16000 by 1188 log-likelihood matrix.
# 
# Estimate   SE
# elpd_loo  -2826.0 32.2
# p_loo        28.6  1.6
# looic      5652.0 64.5

#   MCSE of elpd_loo is 0.0.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.7, 1.3]).
# 
# All Pareto k estimates are good (k < 0.7).
# See help('pareto-k-diagnostic') for details.
# 
# Output of model 'abs_hex_no_autocor':
#   
#   Computed from 40000 by 1188 log-likelihood matrix.
# 
# Estimate   SE
# elpd_loo  -2953.9 31.3
# p_loo        14.1  0.8
# looic      5907.9 62.6

#   MCSE of elpd_loo is 0.0.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.6, 1.3]).
# 
# All Pareto k estimates are good (k < 0.7).
# See help('pareto-k-diagnostic') for details.
# 
# Model comparisons:
#   elpd_diff se_diff
# abs_hex_autocor_as_rf                0.0       0.0 
# abs_hex_autocor_as_rf_interaction   -0.5       0.4 
# abs_hex_interaction_day_as_rf     -145.9      16.6 
# abs_hex_no_autocor                -273.8      19.3 
# abs_hex_autocor                   -550.0      46.2 
# abs_hex_autocor_interaction       -552.0      46.1 
# Warning messages:
#   1: Found 117 observations with a pareto_k > 0.7 in model 'abs_hex_autocor'. We recommend to set 'moment_match = TRUE' in order to perform moment matching for problematic observations.  
# 2: Found 127 observations with a pareto_k > 0.7 in model 'abs_hex_autocor_interaction'. We recommend to set 'moment_match = TRUE' in order to perform moment matching for problematic observations.  
# > 


# global visitation -------------------------------------------------------

loo(global_hex_autocor,
    global_hex_autocor_as_rf,
    global_hex_autocor_as_rf_interaction,
    global_hex_autocor_interaction,
    global_hex_no_autocor,
    global_visitation_interaction_day_as_rf,
    global_visitation_no_autocor_interaction,
    compare = TRUE)
sink()
# Results----
# Output of model 'global_hex_autocor':
# 
# Computed from 16000 by 1188 log-likelihood matrix.
# 
# Estimate    SE
# elpd_loo  -6636.1 184.4
# p_loo      2923.4 150.4
# looic     13272.2 368.9

#   MCSE of elpd_loo is NA.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.5, 1.7]).
# 
# Pareto k diagnostic values:
#   Count Pct.    Min. ESS
# (-Inf, 0.7]   (good)     471   39.6%   192     
# (0.7, 1]   (bad)      394   33.2%   <NA>    
#   (1, Inf)   (very bad) 323   27.2%   <NA>    
#   See help('pareto-k-diagnostic') for details.
# 
# Output of model 'global_hex_autocor_as_rf':
#   
#   Computed from 16000 by 1188 log-likelihood matrix.
# 
# Estimate   SE
# elpd_loo  -3920.0 38.5
# p_loo        78.7  4.3
# looic      7839.9 77.0

#   MCSE of elpd_loo is 0.1.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.4, 1.7]).
# 
# All Pareto k estimates are good (k < 0.7).
# See help('pareto-k-diagnostic') for details.
# 
# Output of model 'global_hex_autocor_as_rf_interaction':
#   
#   Computed from 16000 by 1188 log-likelihood matrix.
# 
# Estimate   SE
# elpd_loo  -3919.9 38.6
# p_loo        78.7  4.4
# looic      7839.9 77.1

#   MCSE of elpd_loo is 0.1.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.4, 1.6]).
# 
# All Pareto k estimates are good (k < 0.7).
# See help('pareto-k-diagnostic') for details.
# 
# Output of model 'global_hex_autocor_interaction':
#   
#   Computed from 16000 by 1188 log-likelihood matrix.
# 
# Estimate    SE
# elpd_loo  -6689.7 192.8
# p_loo      2976.2 159.1
# looic     13379.3 385.5

#   MCSE of elpd_loo is NA.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.5, 2.1]).
# 
# Pareto k diagnostic values:
#   Count Pct.    Min. ESS
# (-Inf, 0.7]   (good)     474   39.9%   244     
# (0.7, 1]   (bad)      390   32.8%   <NA>    
#   (1, Inf)   (very bad) 324   27.3%   <NA>    
#   See help('pareto-k-diagnostic') for details.
# 
# Output of model 'global_hex_no_autocor':
#   
#   Computed from 16000 by 1188 log-likelihood matrix.
# 
# Estimate   SE
# elpd_loo  -4288.7 34.9
# p_loo        14.1  0.8
# looic      8577.3 69.8

#   MCSE of elpd_loo is 0.0.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.6, 1.2]).
# 
# All Pareto k estimates are good (k < 0.7).
# See help('pareto-k-diagnostic') for details.
# 
# Output of model 'global_visitation_interaction_day_as_rf':
#   
#   Computed from 16000 by 1188 log-likelihood matrix.
# 
# Estimate   SE
# elpd_loo  -4113.0 35.6
# p_loo        27.6  1.3
# looic      8226.0 71.3

#   MCSE of elpd_loo is 0.0.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.7, 1.3]).
# 
# All Pareto k estimates are good (k < 0.7).
# See help('pareto-k-diagnostic') for details.
# 
# Output of model 'global_visitation_no_autocor_interaction':
#   
#   Computed from 16000 by 1188 log-likelihood matrix.
# 
# Estimate   SE
# elpd_loo  -4283.0 35.2
# p_loo        14.9  0.9
# looic      8566.0 70.5

#   MCSE of elpd_loo is 0.0.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.7, 1.2]).
# 
# All Pareto k estimates are good (k < 0.7).
# See help('pareto-k-diagnostic') for details.
# 
# Model comparisons:
#   elpd_diff se_diff
# global_hex_autocor_as_rf_interaction         0.0       0.0
# global_hex_autocor_as_rf                     0.0       0.4
# global_visitation_interaction_day_as_rf   -193.0      20.9
# global_visitation_no_autocor_interaction  -363.1      26.0
# global_hex_no_autocor                     -368.7      26.1
# global_hex_autocor                       -2716.1     167.3
# global_hex_autocor_interaction           -2769.7     175.8
# Warning messages:
#   1: Found 717 observations with a pareto_k > 0.7 in model 'global_hex_autocor'. We recommend to set 'moment_match = TRUE' in order to perform moment matching for problematic observations.  
# 2: Found 714 observations with a pareto_k > 0.7 in model 'global_hex_autocor_interaction'. We recommend to set 'moment_match = TRUE' in order to perform moment matching for problematic observations.  


sink(file = "output/model_development/global_visitation_model_summary.md")
summary(global_visitation_categorical_autocor_as_rf_interaction)
sink()
sink(file=NULL)

plot(global_visitation_categorical_autocor_as_rf_interaction)




#relative visitation ----

sink("output/model_development/loo_relative_visitation.txt")
loo(relative_hex_autocor,
    relative_hex_autocor_as_rf,
    relative_hex_autocor_as_rf_interaction,
    relative_hex_autocor_interaction,
    relative_hex_interaction_day_as_rf,
    relative_hex_interaction_day_as_rf,
    relative_hex_no_autocor,
    relative_hex_no_autocor_interaction,
    compare = TRUE)
sink()
sink(file=NULL)

# Output of model 'relative_hex_autocor':
#   
#   Computed from 16000 by 1056 log-likelihood matrix.
# 
# Estimate    SE
# elpd_loo  -2970.3  91.4
# p_loo      1016.9  68.9
# looic      5940.7 182.7

#   MCSE of elpd_loo is NA.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.3, 2.6]).
# 
# Pareto k diagnostic values:
#   Count Pct.    Min. ESS
# (-Inf, 0.7]   (good)     804   76.1%   296     
# (0.7, 1]   (bad)      123   11.6%   <NA>    
#   (1, Inf)   (very bad) 129   12.2%   <NA>    
#   See help('pareto-k-diagnostic') for details.
# 
# Output of model 'relative_hex_autocor_as_rf':
#   
#   Computed from 16000 by 1056 log-likelihood matrix.
# 
# Estimate   SE
# elpd_loo  -2016.6 37.5
# p_loo        82.6  5.3
# looic      4033.2 75.1

#   MCSE of elpd_loo is NA.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.4, 1.8]).
# 
# Pareto k diagnostic values:
#   Count Pct.    Min. ESS
# (-Inf, 0.7]   (good)     1038  98.3%   1753    
# (0.7, 1]   (bad)         1   0.1%   <NA>    
#   (1, Inf)   (very bad)   17   1.6%   <NA>    
#   See help('pareto-k-diagnostic') for details.
# 
# Output of model 'relative_hex_autocor_as_rf_interaction':
#   
#   Computed from 16000 by 1056 log-likelihood matrix.
# 
# Estimate   SE
# elpd_loo  -2016.8 37.5
# p_loo        82.8  5.3
# looic      4033.7 75.1

#   MCSE of elpd_loo is NA.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.4, 2.0]).
# 
# Pareto k diagnostic values:
#   Count Pct.    Min. ESS
# (-Inf, 0.7]   (good)     1038  98.3%   2052    
# (0.7, 1]   (bad)         1   0.1%   <NA>    
#   (1, Inf)   (very bad)   17   1.6%   <NA>    
#   See help('pareto-k-diagnostic') for details.
# 
# Output of model 'relative_hex_autocor_interaction':
#   
#   Computed from 16000 by 1056 log-likelihood matrix.
# 
# Estimate    SE
# elpd_loo  -2983.9  92.8
# p_loo      1030.4  70.4
# looic      5967.8 185.5

#   MCSE of elpd_loo is NA.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.4, 2.5]).
# 
# Pareto k diagnostic values:
#   Count Pct.    Min. ESS
# (-Inf, 0.7]   (good)     797   75.5%   277     
# (0.7, 1]   (bad)      125   11.8%   <NA>    
#   (1, Inf)   (very bad) 134   12.7%   <NA>    
#   See help('pareto-k-diagnostic') for details.
# 
# Output of model 'relative_hex_interaction_day_as_rf':
#   
#   Computed from 16000 by 1056 log-likelihood matrix.
# 
# Estimate   SE
# elpd_loo  -2218.7 48.1
# p_loo        51.8  3.8
# looic      4437.3 96.2

#   MCSE of elpd_loo is NA.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.5, 1.5]).
# 
# Pareto k diagnostic values:
#   Count Pct.    Min. ESS
# (-Inf, 0.7]   (good)     1039  98.4%   1074    
# (0.7, 1]   (bad)         0   0.0%   <NA>    
#   (1, Inf)   (very bad)   17   1.6%   <NA>    
#   See help('pareto-k-diagnostic') for details.
# 
# Output of model 'relative_hex_interaction_day_as_rf':
#   
#   Computed from 16000 by 1056 log-likelihood matrix.
# 
# Estimate   SE
# elpd_loo  -2218.7 48.1
# p_loo        51.8  3.8
# looic      4437.3 96.2

#   MCSE of elpd_loo is NA.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.5, 1.5]).
# 
# Pareto k diagnostic values:
#   Count Pct.    Min. ESS
# (-Inf, 0.7]   (good)     1039  98.4%   1074    
# (0.7, 1]   (bad)         0   0.0%   <NA>    
#   (1, Inf)   (very bad)   17   1.6%   <NA>    
#   See help('pareto-k-diagnostic') for details.
# 
# Output of model 'relative_hex_no_autocor':
#   
#   Computed from 16000 by 1056 log-likelihood matrix.
# 
# Estimate    SE
# elpd_loo  -2287.3  52.4
# p_loo        25.0   1.8
# looic      4574.6 104.8

#   MCSE of elpd_loo is NA.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.5, 1.5]).
# 
# Pareto k diagnostic values:
#   Count Pct.    Min. ESS
# (-Inf, 0.7]   (good)     1039  98.4%   5171    
# (0.7, 1]   (bad)         0   0.0%   <NA>    
#   (1, Inf)   (very bad)   17   1.6%   <NA>    
#   See help('pareto-k-diagnostic') for details.
# 
# Output of model 'relative_hex_no_autocor_interaction':
#   
#   Computed from 16000 by 1056 log-likelihood matrix.
# 
# Estimate    SE
# elpd_loo  -2287.7  52.0
# p_loo        27.0   2.0
# looic      4575.4 104.0

#   MCSE of elpd_loo is NA.
# MCSE and ESS estimates assume MCMC draws (r_eff in [0.5, 1.4]).
# 
# Pareto k diagnostic values:
#   Count Pct.    Min. ESS
# (-Inf, 0.7]   (good)     1039  98.4%   5302    
# (0.7, 1]   (bad)         0   0.0%   <NA>    
#   (1, Inf)   (very bad)   17   1.6%   <NA>    
#   See help('pareto-k-diagnostic') for details.
# 
# Model comparisons:
#   elpd_diff se_diff
# relative_hex_autocor_as_rf                0.0       0.0 
# relative_hex_autocor_as_rf_interaction   -0.2       0.1 
# relative_hex_interaction_day_as_rf     -202.1      27.6 
# relative_hex_interaction_day_as_rf     -202.1      27.6 
# relative_hex_no_autocor                -270.7      33.6 
# relative_hex_no_autocor_interaction    -271.1      33.1 
# relative_hex_autocor                   -953.7      75.1 
# relative_hex_autocor_interaction       -967.3      76.5 
# Warning messages:
#   1: Found 252 observations with a pareto_k > 0.7 in model 'relative_hex_autocor'. We recommend to set 'moment_match = TRUE' in order to perform moment matching for problematic observations.  
# 2: Found 18 observations with a pareto_k > 0.7 in model 'relative_hex_autocor_as_rf'. We recommend to set 'moment_match = TRUE' in order to perform moment matching for problematic observations.  
# 3: Found 18 observations with a pareto_k > 0.7 in model 'relative_hex_autocor_as_rf_interaction'. We recommend to set 'moment_match = TRUE' in order to perform moment matching for problematic observations.  
# 4: Found 259 observations with a pareto_k > 0.7 in model 'relative_hex_autocor_interaction'. We recommend to set 'moment_match = TRUE' in order to perform moment matching for problematic observations.  
# 5: Found 17 observations with a pareto_k > 0.7 in model 'relative_hex_interaction_day_as_rf'. We recommend to set 'moment_match = TRUE' in order to perform moment matching for problematic observations.  
# 6: Found 17 observations with a pareto_k > 0.7 in model 'relative_hex_interaction_day_as_rf'. We recommend to set 'moment_match = TRUE' in order to perform moment matching for problematic observations.  
# 7: Found 17 observations with a pareto_k > 0.7 in model 'relative_hex_no_autocor'. We recommend to set 'moment_match = TRUE' in order to perform moment matching for problematic observations.  
# 8: Found 17 observations with a pareto_k > 0.7 in model 'relative_hex_no_autocor_interaction'. We recommend to set 'moment_match = TRUE' in order to perform moment matching for problematic observations.  
# > 


#Looking at "winning model"----
#barely any differnce based on itneraction, so I will go for the one with interaction
pp_check(relative_visitation_categorical_autocor_as_rf_interaction)
#But the pp_check looks quite good (I guess?)
ggsave("output/plots/pp_check_relative_target_vis.png", bg="white")

#
plot(relative_visitation_categorical_autocor_as_rf_interaction)

#saving summary
sink(file="output/model_development/model_summary_relative_visitation.txt")
summary(relative_visitation_categorical_autocor_as_rf_interaction)
sink()
sink(file=NULL)
