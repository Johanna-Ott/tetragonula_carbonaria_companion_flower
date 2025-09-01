
#04 Prior validation and sensitivity analysis
#14-05-2025
#Based on points 7-9 of the WAMBS checklist: https://www.rensvandeschoot.com/tutorials/wambs-checklist-in-r-using-brms/



# libraries ---------------------------------------------------------------
library(tidyverse)
library(brms)

# data --------------------------------------------------------------------
stingless_target<-read.csv("data/stingless_bees_tidy1.csv")
stingless_target_no_control<-filter(stingless_target, treatment!="5")


# function to extract estimates -------------------------------------------

##function----
extract_model_estimates <- function(model_path) {
  load(model_path)
  
  fit_object <- get(ls()[sapply(ls(), function(x) inherits(get(x), "brmsfit"))][1])
  model_name <- tools::file_path_sans_ext(basename(model_path))
  
  # --- Fixed Effects ---
  fixed_df <- as.data.frame(summary(fit_object)$fixed) %>%
    rownames_to_column("Parameter") %>%
    mutate(
      Estimate_with_SD = sprintf("%.2f (%.2f)", Estimate, Est.Error),
      model = model_name
    ) %>%
    select(model, Parameter, Estimate_with_SD)
  
  # --- Distributional Parameters (e.g. shape) ---
  dist_df <- NULL
  if (!is.null(summary(fit_object)$spec_pars)) {
    dist_df <- as.data.frame(summary(fit_object)$spec_pars) %>%
      rownames_to_column("Parameter") %>%
      mutate(
        Estimate_with_SD = sprintf("%.2f (%.2f)", Estimate, Est.Error),
        model = model_name
      ) %>%
      select(model, Parameter, Estimate_with_SD)
  }
  
  # --- Multilevel Hyperparameters: SDs of the Random Effects ---
  hyper_df <- NULL
  if (!is.null(summary(fit_object)$random)) {
    hyper_df <- do.call(rbind, lapply(names(summary(fit_object)$random), function(gr) {
      df <- as.data.frame(summary(fit_object)$random[[gr]])
      rownames(df) <- paste0("sd_", gr, "_", rownames(df))  # e.g. sd_id_Intercept
      df
    })) %>%
      rownames_to_column("Parameter") %>%
      mutate(
        Estimate_with_SD = sprintf("%.2f (%.2f)", Estimate, Est.Error),
        model = model_name
      ) %>%
      select(model, Parameter, Estimate_with_SD)
  }
  
  # --- creating summary ---
  summary_df <- bind_rows(fixed_df, dist_df, hyper_df)
  return(summary_df)
}



# relative target visitation ----------------------------------------------------------------

#Baseline priors
relative_priors11 <- c(
  prior("student_t(5, 0.3, 3)", class = "Intercept"), 
  #intercept distribution, students t because it is less strict
  #5 df, 0.3 as mean, 3 gives fatness of tails
  prior(normal(0, 5), class = "b"),
  #Centered on 0, large sd
  prior(exponential(2), class = "sd")
  #I need a prior for the colony
)


relative_hexdistance<- brm(
  formula = bf(
    number_individuals|trials(total_visitation) ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors11,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(relative_hexdistance, file="output/sensitivity_analysis2/relative_hexdistance_original.RData")


## Intercept ----------------------------------------------------------------


relative_hexdistance_Intercept_up<- brm(
  formula = bf(
    number_individuals|trials(total_visitation) ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors1,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(relative_hexdistance_Intercept_up, file="output/sensitivity_analysis2/relative_hexdistance_Intercept_up.RData")


#shifting mean down by 1
relative_hexdistance_Intercept_down<- brm(
  formula = bf(
    number_individuals|trials(total_visitation) ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors2,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(relative_hexdistance_Intercept_down, file="output/sensitivity_analysis2/relative_hexdistance_Intercept_down.RData")


#narrowing
relative_hexdistance_Intercept_narrow<- brm(
  formula = bf(
    number_individuals|trials(total_visitation) ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors3,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(relative_hexdistance_Intercept_narrow, file="output/sensitivity_analysis2/relative_hexdistance_Intercept_narrow.RData")

#widen
relative_hexdistance_Intercept_wide<- brm(
  formula = bf(
    number_individuals|trials(total_visitation) ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors4,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(relative_hexdistance_Intercept_wide, file="output/sensitivity_analysis2/relative_hexdistance_Intercept_wide.RData")

## Fixed effect -----
#shifting mean up by 1
relative_hexdistance_fixed_up<- brm(
  formula = bf(
    number_individuals|trials(total_visitation) ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors5,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(relative_hexdistance_fixed_up, file="output/sensitivity_analysis2/relative_hexdistance_fixed_up.RData")


#shifting mean down by 1
relative_hexdistance_fixed_down<- brm(
  formula = bf(
    number_individuals|trials(total_visitation) ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors6,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(relative_hexdistance_fixed_down, file="output/sensitivity_analysis2/relative_hexdistance_fixed_down.RData")

#narrowing
relative_hexdistance_fixed_narrow<- brm(
  formula = bf(
    number_individuals|trials(total_visitation) ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors7,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(relative_hexdistance_fixed_narrow, 
     file="output/sensitivity_analysis2/relative_hexdistance_fixed_narrow.RData")


#widening
relative_hexdistance_fixed_wide<- brm(
  formula = bf(
    number_individuals|trials(total_visitation) ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors8,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(relative_hexdistance_fixed_wide, file="output/sensitivity_analysis2/relative_hexdistance_fixed_wide.RData")

## SD ----


relative_hexdistance_sd1<- brm(
  formula = bf(
    number_individuals|trials(total_visitation) ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors9,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(relative_hexdistance_sd1, file="output/sensitivity_analysis2/relative_hexdistance_sd2.RData")

relative_hexdistance_sd2<- brm(
  formula = bf(
    number_individuals|trials(total_visitation) ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors10,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(relative_hexdistance_sd2, file="output/sensitivity_analysis2/relative_hexdistance_sd2.RData")


## extraction ----

#Setting model path
model_path <- "output/sensitivity_analysis2/relative"

#extracting all models in that folder
model_files <- list.files(model_path, pattern = "\\.RData$", full.names = TRUE)


#do that for all models at once
all_estimates <- map_dfr(model_files, extract_model_estimates)

#reformatting table
result_table_rel <- all_estimates %>%
  pivot_wider(names_from = model, values_from = Estimate_with_SD)

#saving result as csv
write.csv(result_table_rel, file ="output/sensitivity_analysis2/relative/estimate_comparison.csv")


# absolute hex  ----------------------------------------------------------------


absolute_priors <- c(
  prior("student_t(3, 8, 5)", class = "Intercept"), 
  prior(normal(0, 5), class = "b"),
  prior(gamma(1,1), class = "shape"), 
  prior(exponential(1), class = "sd")
)

absolute_hex_baseline <- brm(
  formula = bf(number_individuals ~ attractiveness * hex_distance + (1 | colony) + (1|id)),
  family = negbinomial() ,
  data = stingless_target,
  prior = absolute_priors,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)
save(absolute_hex_baseline, file="output/sensitivity_analysis2/absolute/absolute_hex_baseline.RData")

## Intercept ---------------------------------------------------------------
#Shifting intercept up
absolute_priors_1 <- c(
  prior("student_t(3, 10, 5)", class = "Intercept"), 
  prior(normal(0, 5), class = "b"),
  prior(gamma(1,1), class = "shape"), 
  prior(exponential(1), class = "sd")
)

absolute_hex_Intercept_up <- brm(
  formula = bf(number_individuals ~ attractiveness * hex_distance + (1 | colony) + (1|id)),
  family = negbinomial() ,
  data = stingless_target,
  prior = absolute_priors_1,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)

save(absolute_hex_Intercept_up, file="output/sensitivity_analysis2/absolute/absolute_hex_Intercept_up.RData")

#shifting Intercept down
absolute_priors_2 <- c(
prior("student_t(3, 6, 5)", class = "Intercept"), 
prior(normal(0, 5), class = "b"),
prior(gamma(1,1), class = "shape"), 
prior(exponential(1), class = "sd")
)

absolute_hex_Intercept_down <- brm(
  formula = bf(number_individuals ~ attractiveness * hex_distance + (1 | colony) + (1|id)),
  family = negbinomial() ,
  data = stingless_target,
  prior = absolute_priors_2,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)
save(absolute_hex_Intercept_down, file="output/sensitivity_analysis2/absolute/absolute_hex_Intercept_down.RData")

#narrowing Intercept
absolute_priors_3 <- c(
  prior("student_t(3, 8, 1)", class = "Intercept"), 
  prior(normal(0, 5), class = "b"),
  prior(gamma(1,1), class = "shape"), 
  prior(exponential(1), class = "sd")
)

absolute_hex_Intercept_narrow <- brm(
  formula = bf(number_individuals ~ attractiveness * hex_distance + (1 | colony) + (1|id)),
  family = negbinomial() ,
  data = stingless_target,
  prior = absolute_priors_3,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)
save(absolute_hex_Intercept_narrow, file="output/sensitivity_analysis2/absolute/absolute_hex_Intercept_narrow.RData")

#widening intercept
absolute_priors_4 <- c(
  prior("student_t(3, 8, 10)", class = "Intercept"), 
  prior(normal(0, 5), class = "b"),
  prior(gamma(1,1), class = "shape"), 
  prior(exponential(1), class = "sd")
)

absolute_hex_Intercept_wide <- brm(
  formula = bf(number_individuals ~ attractiveness * hex_distance + (1 | colony) + (1|id)),
  family = negbinomial() ,
  data = stingless_target,
  prior = absolute_priors_4,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)
save(absolute_hex_Intercept_wide, file="output/sensitivity_analysis2/absolute/absolute_hex_Intercept_wide.RData")


## fixed -------------------------------------------------------------------

absolute_priors_5 <- c(
  prior("student_t(3, 8, 5)", class = "Intercept"), 
  prior(normal(1, 5), class = "b"),
  prior(gamma(1,1), class = "shape"), 
  prior(exponential(1), class = "sd")
)

#shifting distribution up
absolute_hex_fixed_up <- brm(
  formula = bf(number_individuals ~ attractiveness * hex_distance + (1 | colony) + (1|id)),
  family = negbinomial() ,
  data = stingless_target,
  prior = absolute_priors_5,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)

save(absolute_hex_fixed_up, file="output/sensitivity_analysis2/absolute/absolute_hex_fixed_up.RData")


#shifting fixed down
absolute_priors_6 <- c(
  prior("student_t(3, 8, 5)", class = "Intercept"), 
  prior(normal(-1, 5), class = "b"),
  prior(gamma(1,1), class = "shape"), 
  prior(exponential(1), class = "sd")
)

absolute_hex_fixed_down <- brm(
  formula = bf(number_individuals ~ attractiveness * hex_distance + (1 | colony) + (1|id)),
  family = negbinomial() ,
  data = stingless_target,
  prior = absolute_priors_6,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)

save(absolute_hex_fixed_down, file="output/sensitivity_analysis2/absolute/absolute_hex_fixed_down.RData")

#narrowing fixed
absolute_priors_7 <- c(
  prior("student_t(3, 8, 5)", class = "Intercept"), 
  prior(normal(0, 2), class = "b"),
  prior(gamma(1,1), class = "shape"), 
  prior(exponential(1), class = "sd")
)

absolute_hex_fixed_narrow <- brm(
  formula = bf(number_individuals ~ attractiveness * hex_distance + (1 | colony) + (1|id)),
  family = negbinomial() ,
  data = stingless_target,
  prior = absolute_priors_7,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)

save(absolute_hex_fixed_narrow, file="output/sensitivity_analysis2/absolute/absolute_hex_fixed_narrow.RData")

#widening fixed
absolute_priors_8 <- c(
  prior("student_t(3, 8, 5)", class = "Intercept"), 
  prior(normal(0, 10), class = "b"),
  prior(gamma(1,1), class = "shape"), 
  prior(exponential(1), class = "sd")
)

absolute_hex_fixed_wide <- brm(
  formula = bf(number_individuals ~ attractiveness * hex_distance + (1 | colony) + (1|id)),
  family = negbinomial() ,
  data = stingless_target,
  prior = absolute_priors_8,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)
save(absolute_hex_fixed_wide, file="output/sensitivity_analysis2/absolute/absolute_hex_fixed_wide.RData")


#changing shape

absolute_priors_9 <- c(
  prior("student_t(3, 8, 5)", class = "Intercept"), 
  prior(normal(0, 5), class = "b"),
  prior(gamma(2,1), class = "shape"), 
  prior(exponential(1), class = "sd")
)

absolute_hex_shape1 <- brm(
  formula = bf(number_individuals ~ attractiveness * hex_distance + (1 | colony) + (1|id)),
  family = negbinomial() ,
  data = stingless_target,
  prior = absolute_priors_9,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)

save(absolute_hex_shape1, file="output/sensitivity_analysis2/absolute/absolute_hex_shape1.RData")


absolute_priors_10 <- c(
  prior("student_t(3, 8, 5)", class = "Intercept"), 
  prior(normal(0, 5), class = "b"),
  prior(gamma(1,2), class = "shape"), 
  prior(exponential(1), class = "sd")
)

absolute_hex_shape2 <- brm(
  formula = bf(number_individuals ~ attractiveness * hex_distance + (1 | colony) + (1|id)),
  family = negbinomial() ,
  data = stingless_target,
  prior = absolute_priors_10,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)
save(absolute_hex_shape2, file="output/sensitivity_analysis2/absolute/absolute_hex_shape2.RData")

## sd ----

absolute_priors_11 <- c(
  prior("student_t(3, 8, 5)", class = "Intercept"), 
  prior(normal(0, 5), class = "b"),
  prior(gamma(1,1), class = "shape"), 
  prior(exponential(2), class = "sd")
)

absolute_hex_sd1 <- brm(
  formula = bf(number_individuals ~ attractiveness * hex_distance + (1 | colony) + (1|id)),
  family = negbinomial() ,
  data = stingless_target,
  prior = absolute_priors_11,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)
save(absolute_hex_sd1, file="output/sensitivity_analysis2/absolute/absolute_hex_sd1.RData")


absolute_priors_12 <- c(
  prior("student_t(3, 8, 5)", class = "Intercept"), 
  prior(normal(0, 5), class = "b"),
  prior(gamma(1,1), class = "shape"), 
  prior(exponential(0.5), class = "sd")
)

absolute_hex_sd2 <- brm(
  formula = bf(number_individuals ~ attractiveness * hex_distance + (1 | colony) + (1|id)),
  family = negbinomial() ,
  data = stingless_target,
  prior = absolute_priors_12,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)

save(absolute_hex_sd2, file="output/sensitivity_analysis2/absolute/absolute_hex_sd2.RData")





## extraction --------------------------------------------------------------


#Setting model path
model_path <- "output/sensitivity_analysis2/absolute"

#extracting all models in that folder
model_files <- list.files(model_path, pattern = "\\.RData$", full.names = TRUE)


#do that for all models at once
all_estimates <- map_dfr(model_files, extract_model_estimates)

#reformatting table
result_table_rel <- all_estimates %>%
  pivot_wider(names_from = model, values_from = Estimate_with_SD)

#saving result as csv
write.csv(result_table_rel, file ="output/sensitivity_analysis2/absolute/estimate_comparison.csv")


# global ------------------------------------------------------------------
global_priors <- c(
  prior("student_t(10, 20, 3)", class = "Intercept"),
  prior(normal(0, 5), class = "b"),
   prior(gamma(1,1), class = "shape"),   prior(exponential(1), class = "sd")
)


global_hex_baseline<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    total_visitation ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = global_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(global_hex_baseline, file="output/sensitivity_analysis2/global/global_hex_baseline.RData")



## Intercept ---------------------------------------------------------------
#Shifting intercept  up
global_priors_1 <- c(
  prior("student_t(10, 30, 3)", class = "Intercept"),
  prior(normal(0, 5), class = "b"),
  prior(gamma(1,1), class = "shape"),   prior(exponential(1), class = "sd")
)


global_hex_Intercept_up<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    total_visitation ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = global_priors_1,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(global_hex_Intercept_up, file="output/sensitivity_analysis2/global/global_hex_Intercept_up.RData")

#Shifting intercept ten down
global_priors_2 <- c(
  prior("student_t(10, 20, 3)", class = "Intercept"),
  prior(normal(0, 5), class = "b"),
  prior(gamma(1,1), class = "shape"),   prior(exponential(1), class = "sd")
)


global_hex_Intercept_down<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    total_visitation ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = global_priors_2,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(global_hex_Intercept_down, file="output/sensitivity_analysis2/global/global_hex_Intercept_down.RData")

#narrowing intercept
global_priors_3 <- c(
  prior("student_t(10, 20, 1)", class = "Intercept"),
  prior(normal(0, 5), class = "b"),
  prior(gamma(1,1), class = "shape"),   prior(exponential(1), class = "sd")
)


global_hex_Intercept_narrow<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    total_visitation ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = global_priors_3,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(global_hex_Intercept_narrow, file="output/sensitivity_analysis2/global/global_hex_Intercept_narrow.RData")

#widening Intercept
global_priors_4 <- c(
  prior("student_t(10, 20, 5)", class = "Intercept"),
  prior(normal(0, 5), class = "b"),
  prior(gamma(1,1), class = "shape"),   prior(exponential(1), class = "sd")
)


global_hex_Intercept_wide<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    total_visitation ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = global_priors_4,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(global_hex_Intercept_wide, file="output/sensitivity_analysis2/global/global_hex_Intercept_wide.RData")


## fixed effects -----------------------------------------------------------

global_priors_5 <- c(
  prior("student_t(10, 20, 3)", class = "Intercept"),
  prior(normal(1, 5), class = "b"),
  prior(gamma(1,1), class = "shape"),   prior(exponential(1), class = "sd")
)


global_hex_fixed_up<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    total_visitation ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = global_priors_5,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(global_hex_fixed_up, file="output/sensitivity_analysis2/global/global_hex_fixed_up.RData")

#fixed effects down
global_priors_6 <- c(
  prior("student_t(10, 20, 3)", class = "Intercept"),
  prior(normal(-1, 5), class = "b"),
  prior(gamma(1,1), class = "shape"),   prior(exponential(1), class = "sd")
)


global_hex_fixed_down<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    total_visitation ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = global_priors_6,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(global_hex_fixed_down, file="output/sensitivity_analysis2/global/global_hex_fixed_down.RData")

#fixed narrow
global_priors_7 <- c(
  prior("student_t(10, 20, 3)", class = "Intercept"),
  prior(normal(0, 2), class = "b"),
  prior(gamma(1,1), class = "shape"),   prior(exponential(1), class = "sd")
)


global_hex_fixed_narrow<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    total_visitation ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = global_priors_7,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(global_hex_fixed_narrow, file="output/sensitivity_analysis2/global/global_hex_fixed_narrow.RData")

#fixed wide
global_priors_8 <- c(
  prior("student_t(10, 20, 3)", class = "Intercept"),
  prior(normal(0, 10), class = "b"),
  prior(gamma(1,1), class = "shape"),   
  prior(exponential(1), class = "sd")
)


global_hex_fixed_wide<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    total_visitation ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = global_priors_8,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(global_hex_fixed_wide, file="output/sensitivity_analysis2/global/global_hex_fixed_wide.RData")


#shape 1
global_priors_9 <- c(
  prior("student_t(10, 20, 3)", class = "Intercept"),
  prior(normal(0, 5), class = "b"),
  prior(gamma(2,1), class = "shape"),   prior(exponential(1), class = "sd")
)


global_hex_shape1<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    total_visitation ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = global_priors_9,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(global_hex_shape1, file="output/sensitivity_analysis2/global/global_hex_shape1.RData")


#shape2
global_priors_10 <- c(
  prior("student_t(10, 20, 3)", class = "Intercept"),
  prior(normal(0, 5), class = "b"),
  prior(gamma(1,2), class = "shape"),   prior(exponential(1), class = "sd")
)


global_hex_shape2<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    total_visitation ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = global_priors_10,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(global_hex_shape2, file="output/sensitivity_analysis2/global/global_hex_shape2.RData")


#sd1
global_priors_11 <- c(
  prior("student_t(10, 20, 3)", class = "Intercept"),
  prior(normal(0, 5), class = "b"),
  prior(gamma(1,1), class = "shape"),  
  prior(exponential(2), class = "sd")
)


global_hex_sd1<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    total_visitation ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = global_priors_11,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(global_hex_sd1, file="output/sensitivity_analysis2/global/global_hex_sd1.RData")


#sd2
global_priors_12 <- c(
  prior("student_t(10, 20, 3)", class = "Intercept"),
  prior(normal(0, 5), class = "b"),
  prior(gamma(1,1), class = "shape"),   prior(exponential(0.5), class = "sd")
)


global_hex_sd2<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    total_visitation ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = global_priors_12,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(global_hex_sd2, file="output/sensitivity_analysis2/global/global_hex_sd2.RData")



## extraction --------------------------------------------------------------


#Setting model path
model_path <- "output/sensitivity_analysis2/global"

#extracting all models in that folder
model_files <- list.files(model_path, pattern = "\\.RData$", full.names = TRUE)


#do that for all models at once
all_estimates <- map_dfr(model_files, extract_model_estimates)

#reformatting table
result_table_rel <- all_estimates %>%
  pivot_wider(names_from = model, values_from = Estimate_with_SD)

#saving result as csv
write.csv(result_table_rel, file ="output/sensitivity_analysis2/global/global_estimate_comparison.csv")
