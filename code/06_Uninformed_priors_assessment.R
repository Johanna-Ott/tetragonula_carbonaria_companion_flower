#Late model wrangling
#08.05.2025
#As I am leaning towards using the hexdistance models now, I want to rerun them with a bunch of differnt priors

#Packages----
library(tidyverse)
library(brms)
library(tidybayes)
lapply(list.files("output/model_development/hexdistance/", 
                  pattern = "\\.RData$", full.names = TRUE),load, envir = .GlobalEnv)
#Loading in data----
long_data<-read.csv("data/stingless_bees_tidy1.csv")
stingless_target <- filter(long_data, species=="stingless", flowertype =="target")
stingless_target_no_control<-filter(stingless_target,attractiveness!="control" )
#Default priors?  ----
absolute_hexdistance_default<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    number_individuals ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(absolute_hexdistance_default, file="output/model_development/hexdistance/absolute_hexdistance_default.RData")

#Relative default
relative_hexdistance_default<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    number_individuals|trials(total_visitation) ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  #Strictness?
  control = list(adapt_delta = 0.95)
)

 save(relative_hexdistance_default, file="output/model_development/hexdistance/relative_hexdistance_default.RData")

#global default
global_hexdistance_default<- brm(
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
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(global_hexdistance_default, file="output/model_development/hexdistance/global_hexdistance_default.RData")


#Very weak ----
#Priors
absolute_priors_very_weak <- c(
  prior(normal(0,100), class = "Intercept"), 
  #Intercept distribution, student_t is less strict than normal
  #3 df, 8 as mean, 5 determines "fatness of tails"
  prior(normal(0, 100), class = "b"),
  #effect of fixed effects, centered on 0 with big sd
  prior(gamma(1,1), class = "shape"),  # shape parameter of the distribution
  #based on this image: https://www.researchgate.net/figure/The-gamma-distribution-function-for-various-values-of-the-shape-parameter-a_fig1_236583233
  prior(exponential(1), class = "sd"))
# prior for the colony


absolute_hexdistance_very_weak<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    number_individuals ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = absolute_priors_very_weak,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

#Saving model 
save(absolute_hexdistance_very_weak,  
     file="output/model_development/hexdistance/absolute_hexdistance_very_weak.RData")


#Global visitation

#Priors
global_priors_very_weak <- c(
  prior(normal(0,100), class = "Intercept"), 
  #the intercept distribution, students t because is less strict
  #3 df, 15 as mean, 5 gives fatness of tails
  prior(normal(0, 100), class = "b"),
  #Effect of fixed effects, centered on 0 with big sd
  prior(gamma(1,1), class = "shape"),  # shape parameter of the distribution
  #based on this image: https://www.researchgate.net/figure/The-gamma-distribution-function-for-various-values-of-the-shape-parameter-a_fig1_236583233
  prior(exponential(1), class = "sd")
  #Prior for the colony
)

#Model

global_hexdistance_very_weak<- brm(
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
  prior = global_priors_very_weak,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(global_hexdistance_very_weak, file="output/model_development/hexdistance/global_hexdistance_very_weak.RData")

#Relative visitation

#since in the control treatment, the relative visitation is always 100%, it will
#not be considered in this analysis
#Therefore, creating new df without control
stingless_target_no_control<-filter(stingless_target, treatment!="5")


#priors
relative_priors_very_weak <- c(
  prior(normal(0,100), class = "Intercept"), 
  #intercept distribution, students t because it is less strict
  #5 df, 0.3 as mean, 3 gives fatness of tails
  prior(normal(0, 100), class = "b"),
  #Centered on 0, large sd
  prior(exponential(1), class = "sd")
  #I need a prior for the colony
)

#Model

relative_hexdistance_very_weak<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    number_individuals|trials(total_visitation) ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior =relative_priors_very_weak,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(relative_hexdistance_very_weak, file="output/model_development/hexdistance/relative_hexdistance_very_weak.RData")


#Weak ----
#Priors
absolute_priors_weak <- c(
  prior(normal(0,10), class = "Intercept"), 
  #Intercept distribution, student_t is less strict than normal
  #3 df, 8 as mean, 5 determines "fatness of tails"
  prior(normal(0, 10), class = "b"),
  #effect of fixed effects, centered on 0 with big sd
  prior(gamma(1,1), class = "shape"),  # shape parameter of the distribution
  #based on this image: https://www.researchgate.net/figure/The-gamma-distribution-function-for-various-values-of-the-shape-parameter-a_fig1_236583233
  prior(exponential(1), class = "sd"))
# prior for the colony


absolute_hexdistance_weak<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    number_individuals ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = absolute_priors_weak,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

#Saving model 
save(absolute_hexdistance_weak,  
     file="output/model_development/hexdistance/absolute_hexdistance_weak.RData")


#Global visitation

#Priors
global_priors_weak <- c(
  prior(normal(0,10), class = "Intercept"), 
  #the intercept distribution, students t because is less strict
  #3 df, 15 as mean, 5 gives fatness of tails
  prior(normal(0, 10), class = "b"),
  #Effect of fixed effects, centered on 0 with big sd
  prior(gamma(1,1), class = "shape"),  # shape parameter of the distribution
  #based on this image: https://www.researchgate.net/figure/The-gamma-distribution-function-for-various-values-of-the-shape-parameter-a_fig1_236583233
  prior(exponential(1), class = "sd")
  #Prior for the colony
)

#Model

global_hexdistance_weak<- brm(
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
  prior = global_priors_weak,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(global_hexdistance_weak, file="output/model_development/hexdistance/global_hexdistance_weak.RData")

#Relative visitation

#since in the control treatment, the relative visitation is always 100%, it will
#not be considered in this analysis
#Therefore, creating new df without control
stingless_target_no_control<-filter(stingless_target, treatment!="5")


#priors
relative_priors_weak <- c(
  prior(normal(0,10), class = "Intercept"), 
  #intercept distribution, students t because it is less strict
  #5 df, 0.3 as mean, 3 gives fatness of tails
  prior(normal(0, 10), class = "b"),
  #Centered on 0, large sd
  prior(exponential(1), class = "sd")
  #I need a prior for the colony
)

#Model

relative_hexdistance_weak<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    number_individuals|trials(total_visitation) ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors_weak,
    #Strictness?
    control = list(adapt_delta = 0.95)
)

save(relative_hexdistance_weak, file="output/model_development/hexdistance/relative_hexdistance_weak.RData")

summary(absolute_hexdistance_weak)


# plotting ----------------------------------------------------------------

## relative
model_colors <- c("Baseline" = "red",
                  "Default" = "black","Very weak" = "black","Weak" = "black")
load("output/model_development/relative_hexdistance.RData")
all_draws <-bind_rows(
  spread_draws(relative_hexdistance, b_Intercept,b_hex_distance, b_attractivenesslow, `b_attractivenesslow:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Baseline"),
  spread_draws(relative_hexdistance_default,  b_Intercept,b_hex_distance, b_attractivenesslow, `b_attractivenesslow:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Default")#,
  #spread_draws(relative_hexdistance_very_weak,  b_Intercept,b_hex_distance, b_attractivenesslow, `b_attractivenesslow:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Very weak"),
  #spread_draws(relative_hexdistance_weak,  b_Intercept,b_hex_distance, b_attractivenesslow, `b_attractivenesslow:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Weak")
  )

(hexdistance_priors<-all_draws %>%
    pivot_longer(cols=c("b_Intercept","b_hex_distance", "b_attractivenesslow", 
                        "b_attractivenesslow:hex_distance", "sd_colony__Intercept", 
                        "sd_id__Intercept"), names_to= "parameter", values_to="values") %>% 
    mutate(parameter=fct_relevel(parameter,"b_Intercept","b_hex_distance", "b_attractivenesslow", 
                                 "b_attractivenesslow:hex_distance", "sd_colony__Intercept", 
                                 "sd_id__Intercept")) %>% 
    ggplot(aes(x = values, y = model, color = model)) +
    #stat_halfeye()+
    stat_pointinterval(.width = c(0.5, 0.95)) +
    scale_color_manual(values=model_colors)+
    facet_grid(~parameter, scales= "free_x"
    )+
    theme_bw()+
    theme(legend.position="none",
          panel.grid.major.y = element_blank()
    )+
    labs(title="Estimates compared to default / very weak priors")+
    geom_vline(xintercept=0, linetype="dashed"))

ggsave(hexdistance_priors, file="output/plots/sensitivity_relative/relative_hexdistance_default_priors.png",
       height=3,
       width=12.22,
       bg="white")


## absolute

model_colors <- c("Baseline" = "red",
                  "Default" = "black","Very weak" = "black","Weak" = "black")
load("output/model_development/absolute_hexdistance.RData")
all_draws_abs <-bind_rows(
  spread_draws(absolute_hexdistance, b_Intercept,b_hex_distance, b_attractivenesslow, b_attractivenesshigh, `b_attractivenesslow:hex_distance`, `b_attractivenesshigh:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Baseline"),
  spread_draws(absolute_hexdistance_default,b_Intercept, b_hex_distance, b_attractivenesslow, b_attractivenesshigh,`b_attractivenesslow:hex_distance`, `b_attractivenesshigh:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Default"),
  spread_draws(absolute_hexdistance_very_weak, b_Intercept,b_hex_distance, b_attractivenesslow, b_attractivenesshigh,`b_attractivenesslow:hex_distance`,  `b_attractivenesshigh:hex_distance`,sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Very weak"))
 # spread_draws(absolute_hexdistance_weak, b_Intercept,b_hex_distance, b_attractivenesslow,b_attractivenesshigh, `b_attractivenesslow:hex_distance`,  `b_attractivenesshigh:hex_distance`,sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Weak"))

(abs_hexdistance_priors<-all_draws_abs %>%
    pivot_longer(cols=c("b_Intercept","b_hex_distance", "b_attractivenesslow", "b_attractivenesshigh",
                        "b_attractivenesslow:hex_distance", 
                        "b_attractivenesshigh:hex_distance", "sd_colony__Intercept", 
                        "sd_id__Intercept"), names_to= "parameter", values_to="values") %>% 
    mutate(parameter=fct_relevel(parameter,"b_Intercept","b_hex_distance", "b_attractivenesslow", "b_attractivenesshigh",
                                 "b_attractivenesslow:hex_distance", 
                                 "b_attractivenesshigh:hex_distance", "sd_colony__Intercept", 
                                 "sd_id__Intercept")) %>% 
    ggplot(aes(x = values, y = model, color = model)) +
    #stat_halfeye()+
    stat_pointinterval(.width = c(0.5, 0.95)) +
    scale_color_manual(values=model_colors)+
    facet_grid(~parameter, scales= "free_x"
    )+
    theme_bw()+
    theme(legend.position="none",
          panel.grid.major.y = element_blank()
    )+
    labs(title="Estimates compared to default / very weak priors")+
    geom_vline(xintercept=0, linetype="dashed"))

ggsave(abs_hexdistance_priors, file="output/plots/sensitivity_relative/absolute_hexdistance_default_priors.png",
       height=4.5,
       width=12.22,
       bg="white")
## global

model_colors <- c("Baseline" = "red",
                  "Default" = "black","Very weak" = "black","Weak" = "black")
load("output/model_development/global_hexdistance.RData")
all_draws_global <-bind_rows(
  spread_draws(global_hexdistance, b_Intercept,b_hex_distance, b_attractivenesslow, b_attractivenesshigh, `b_attractivenesslow:hex_distance`, `b_attractivenesshigh:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Baseline"),
  spread_draws(global_hexdistance_default,b_Intercept, b_hex_distance, b_attractivenesslow, b_attractivenesshigh,`b_attractivenesslow:hex_distance`, `b_attractivenesshigh:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Default"),
  spread_draws(global_hexdistance_very_weak, b_Intercept,b_hex_distance, b_attractivenesslow, b_attractivenesshigh,`b_attractivenesslow:hex_distance`,  `b_attractivenesshigh:hex_distance`,sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Very weak"))
  #spread_draws(global_hexdistance_weak, b_Intercept,b_hex_distance, b_attractivenesslow,b_attractivenesshigh, `b_attractivenesslow:hex_distance`,  `b_attractivenesshigh:hex_distance`,sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Weak"))

(global_hexdistance_priors<-all_draws_global %>%
    pivot_longer(cols=c("b_Intercept","b_hex_distance", "b_attractivenesslow", "b_attractivenesshigh",
                        "b_attractivenesslow:hex_distance", 
                        "b_attractivenesshigh:hex_distance", "sd_colony__Intercept", 
                        "sd_id__Intercept"), names_to= "parameter", values_to="values") %>% 
    mutate(parameter=fct_relevel(parameter,"b_Intercept","b_hex_distance", "b_attractivenesslow", "b_attractivenesshigh",
                                 "b_attractivenesslow:hex_distance", 
                                 "b_attractivenesshigh:hex_distance", "sd_colony__Intercept", 
                                 "sd_id__Intercept")) %>% 
    ggplot(aes(x = values, y = model, color = model)) +
    #stat_halfeye()+
    stat_pointinterval(.width = c(0.5, 0.95)) +
    scale_color_manual(values=model_colors)+
    facet_grid(~parameter, scales= "free_x"
    )+
    theme_bw()+
    theme(legend.position="none",
          panel.grid.major.y = element_blank()
    )+
    labs(title="Estimates compared to default / very weak priors")+
    geom_vline(xintercept=0, linetype="dashed"))

ggsave(global_hexdistance_priors, file="output/plots/sensitivity_relative/global_hexdistance_default_priors.png",
       height=4,
       width=12,
       bg="white")
# Tables and such ---------------------------------------------------------

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
  
  # --- Multilevel Hyperparameters: SDs der Random Effects ---
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
  
  # --- Gesamt-Zusammenfassung ---
  summary_df <- bind_rows(fixed_df, dist_df, hyper_df)
  return(summary_df)
}

## extraction ----

#Setting model path
model_path <- "output/model_development/hexdistance/"

#extracting all models in that folder
model_files <- list.files(model_path, pattern = "\\.RData$", full.names = TRUE)


#do that for all models at once
all_estimates <- map_dfr(model_files, extract_model_estimates)

#reformatting table
result_table_rel <- all_estimates %>%
  pivot_wider(names_from = model, values_from = Estimate_with_SD)

#saving result as csv
write.csv(result_table_rel, file ="output/model_development/hexdistance/estimate_comparison.csv")



