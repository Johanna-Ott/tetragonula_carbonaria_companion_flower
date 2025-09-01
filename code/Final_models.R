#Libraries----
library(tidyverse)
library(brms)
library(ggplot2)

#Data ----
stingless_target<-read.csv("data/stingless_bees_tidy1.csv")
stingless_target_no_control<-filter(stingless_target, treatment!="5")

#color as fixed effect----
##absolute----
absolute_priors <- c(
  prior("student_t(3, 8, 5)", class = "Intercept"), 
  #Intercept distribution, student_t is less strict than normal
  #3 df, 8 as mean, 5 determines "fatness of tails"
  prior(normal(0, 5), class = "b"),
  #effect of fixed effects, centered on 0 with big sd
  prior(gamma(1,1), class = "shape"),  # shape parameter of the distribution
  #based on this image: https://www.researchgate.net/figure/The-gamma-distribution-function-for-various-values-of-the-shape-parameter-a_fig1_236583233
  prior(exponential(1), class = "sd", group = "colony"))
# prior for the colony


abs_target_visitation_categorical_interaction_color <- brm(
  formula = bf(
    number_individuals ~ attractiveness*companion_color + (1 | colony)+(1|id)),
  family = negbinomial() ,
  data = stingless_target,
  prior = absolute_priors,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)

save(abs_target_visitation_categorical_interaction_color, file="output/model_development/abs_target_visitation_categorical_interaction_color.RData")


##global----
global_priors <- c(
  prior("student_t(10, 20, 3)", class = "Intercept"), 
  #the intercept distribution, students t because is less strict
  #3 df, 15 as mean, 5 gives fatness of tails
  prior(normal(0, 5), class = "b"),
  #Effect of fixed effects, centered on 0 with big sd
  prior(gamma(1,1), class = "shape"),  # shape parameter of the distribution
  #based on this image: https://www.researchgate.net/figure/The-gamma-distribution-function-for-various-values-of-the-shape-parameter-a_fig1_236583233
  prior(exponential(1), class = "sd")
  #Prior for the colony
)


#model

global_visitation_interaction_color<- brm(
  formula = bf(total_visitation ~ attractiveness * companion_color + (1 | colony)+(1|id)),
  family = negbinomial() ,
  data = stingless_target,
  prior = global_priors,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)
save(global_visitation_interaction_color, file="output/model_development/global_visitation_categorical_interaction_color.RData")

##relative----


#defining priors
relative_priors <- c(
  prior("student_t(5, 0.3, 3)", class = "Intercept"), 
  #intercept distribution, students t because it is less strict
  #5 df, 0.3 as mean, 3 gives fatness of tails
  prior(normal(0, 5), class = "b"),
  #Centered on 0, large sd
  prior(exponential(1), class = "sd", group = "colony")
  #I need a prior for the colony
)


relative_visitation_categorical_interaction_color<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    number_individuals|trials(total_visitation) ~ attractiveness*companion_color + (1 | colony) +(1|id)
    #Change of family because of described changes in data structure
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(relative_visitation_categorical_interaction_color, file="output/model_development/relative_visitation_categorical_interaction_color.RData")


#Hexdistance models----
col_dist<-read.csv("data/col_dist.csv")

distance_from_orange<- col_dist[1:4,] %>% 
  mutate(patch1 = "orange",
         patch2 = case_when(patch2=="SPECTRUM0047"~"red",
                            patch2=="SPECTRUM0048"~"yellow",
                            patch2=="SPECTRUM0049" ~"dark_blue",
                            patch2==" SPECTRUM0050 "~"light_blue")) %>% 
  rename(companion_color=patch2,
         hex_distance = dS)

stingless_target<-stingless_target %>% 
  left_join(distance_from_orange) %>% 
  mutate(hex_distance = case_when(companion_color =="orange" ~ 0,
                                  companion_color !="orange" ~hex_distance))



##absolute target visitation ----
#with nectar concentration as categorical
#and hexdistance as continuous fixed effect
#
absolute_hexdistance<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    number_individuals ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = test_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(absolute_hexdistance, file="output/model_development/absolute_hexdistance.RData")


##relative----
#
relative_hexdistance<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    number_individuals|trials(total_visitation) ~ attractiveness*hex_distance +
      (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(relative_hexdistance, file="output/model_development/relative_hexdistance.RData")

summary(relative_hexdistance)

##global----
#
global_hexdistance<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    total_visitation ~ attractiveness*hex_distance + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = test_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(global_hexdistance, file="output/model_development/global_hexdistance.RData")



# color characteristics ---------------------------------------------------

hex_vinyl<-read.csv("data/hex.vinyl.csv")
hex_vinyl$companion_color<-hex_vinyl$X


stingless_target<-stingless_target %>% 
  left_join(hex_vinyl, join_by(companion_color))

stingless_target_no_control<-filter(stingless_target, treatment!="5")


##priors----
hue_abs_priors<- c(
  prior("student_t(3, 8, 5)", class = "Intercept"), 
  #Intercept distribution, student_t is less strict than normal
  #3 df, 8 as mean, 5 determines "fatness of tails"
  prior(normal(0, 5), class = "b"),
  #effect of fixed effects, centered on 0 with big sd
  prior(gamma(1,1), class = "shape"),  # shape parameter of the distribution
  #based on this image: https://www.researchgate.net/figure/The-gamma-distribution-function-for-various-values-of-the-shape-parameter-a_fig1_236583233
  prior(exponential(1), class = "sd", group = "colony")
  # prior for the colony
)


hue_global_priors <- c(
  prior("student_t(10, 20, 3)", class = "Intercept"), 
  #the intercept distribution, students t because is less strict
  #3 df, 15 as mean, 5 gives fatness of tails
  prior(normal(0, 5), class = "b"),
  #Effect of fixed effects, centered on 0 with big sd
  prior(gamma(1,1), class = "shape"),  # shape parameter of the distribution
  #based on this image: https://www.researchgate.net/figure/The-gamma-distribution-function-for-various-values-of-the-shape-parameter-a_fig1_236583233
  prior(exponential(1), class = "sd", group = "colony")
  #Prior for the colony
)

hue_relative_priors <- c(
  prior("student_t(5, 0.3, 3)", class = "Intercept"), 
  #intercept distribution, students t because it is less strict
  #5 df, 0.3 as mean, 3 gives fatness of tails
  prior(normal(0, 5), class = "b"),
  #Centered on 0, large sd
  prior(exponential(1), class = "sd", group = "colony")
  #I need a prior for the colony
)


##absolute ----
hue_abs_model<-brm(bf(number_individuals ~ attractiveness*h.theta + (1 | colony) +(1|id),
                      family = negbinomial()),
                   data = stingless_target,
                   chains = 4,
                   iter = 15000,
                   warmup = 5000,
                   #cores to run stuff in parallel for faster runtime
                   cores = parallel::detectCores(),
                   prior = hue_abs_priors,
                   #Strictness?
                   control = list(adapt_delta = 0.95))

save(hue_abs_model, file="output/model_development/hue_abs_model.RData")

saturation_abs_model<-brm(bf(number_individuals ~ attractiveness*r.vec + (1 | colony) +(1|id),
                             family = negbinomial()),
                          data = stingless_target,
                          chains = 4,
                          iter = 15000,
                          warmup = 5000,
                          #cores to run stuff in parallel for faster runtime
                          cores = parallel::detectCores(),
                          prior = hue_abs_priors,
                          #Strictness?
                          control = list(adapt_delta = 0.95))

save(saturation_abs_model, file="output/model_development/saturation_abs_model.RData")

brighntess_abs_model<-brm(bf(number_individuals ~ attractiveness*lum + (1 | colony) +(1|id),
                             family = negbinomial()),
                          data = stingless_target,
                          chains = 4,
                          iter = 15000,
                          warmup = 5000,
                          #cores to run stuff in parallel for faster runtime
                          cores = parallel::detectCores(),
                          prior = hue_abs_priors,
                          #Strictness?
                          control = list(adapt_delta = 0.95))

save(brighntess_abs_model, file="output/model_development/brightness_abs_model.RData")

##Relative ----

stingless_target_no_control<-filter(stingless_target, treatment!="5")


hue_relative_model<-brm(
  formula = bf(
    number_individuals|trials(total_visitation) ~ attractiveness*h.theta + (1 | colony) +(1|id),
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = hue_relative_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(hue_relative_model, file="output/model_development/hue_relative_model.RData")


saturation_relative_model<-brm(
  formula = bf(
    number_individuals|trials(total_visitation) ~ attractiveness*r.vec + (1 | colony) +(1|id),
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = hue_relative_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(saturation_relative_model, file="output/model_development/saturation_relative_model.RData")

brightness_relative_model<-brm(
  formula = bf(
    number_individuals|trials(total_visitation) ~ attractiveness*lum + (1 | colony) +(1|id),
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = hue_relative_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(brightness_relative_model, file="output/model_development/brightness_relative_model.RData")

## global----
hue_global_model<-brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    total_visitation ~ attractiveness*h.theta + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = hue_global_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(hue_global_model, file="output/model_development/hue_global_model.RData")

saturation_global_model<-brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    total_visitation ~ attractiveness*r.vec + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = hue_global_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(saturation_global_model, file="output/model_development/saturation_global_model.RData")

brightness_global_model<-brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    total_visitation ~ attractiveness*lum + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 15000,
  warmup = 5000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = hue_global_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(brightness_global_model, file="output/model_development/brightness_global_model.RData")



sink("output/summaries_colors.txt")
summary(hue_abs_model)
summary(saturation_abs_model)
summary(brighntess_abs_model)
summary(hue_relative_model)
summary(saturation_relative_model)
summary(brightness_relative_model)
summary(hue_global_model)
summary(saturation_global_model)
sink()

