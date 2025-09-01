#Working on models
#Johanna Ott, starting in March 2025

#Incentive----
#I want to look at three response variables because they answer slightly different questions
#Absolute target visitation (do I get more pollination?)
#Global flower visitation (do different treatments attract variable numbers of bees?)
#Relative target visitation (Do the treatments differ in spillover?)
#For each of this, I run models with and without autocorrelation, with id as random factor
#And each of these three with and without interaction of my fixed effects
#Since all of these make biological sense but model convergence might be tough for some
#In the "next" script (03_model_selection), I will determine, which one is best suited


#Libraries----
library(tidyverse)
library(brms)
library(ggplot2)
library(tidybayes)
library(bayesplot)
#Loading my data----
long_data<-read.csv("data/stingless_bees_tidy1.csv")
stingless_target <- filter(long_data, species=="stingless", flowertype =="target")
#long_data<-read.csv("data/stingless_bees_tidy1csv")

summaries_abs<-stingless_target %>% 
  group_by(companion_concentration) %>% 
  summarise(mean_target=mean(number_individuals)) %>% 
  ungroup()

summaries_tot<-stingless_target %>% 
  group_by(companion_concentration) %>% 
  summarise(mean_total=mean(total_visitation)) %>% 
  ungroup()

summaries_rel<-stingless_target %>% 
  mutate(relative_target_visitation=case_when(is.na(relative_target_visitation)~ 0,
                                              !is.na(relative_target_visitation)~relative_target_visitation)) %>% 
  group_by(companion_concentration) %>% 
  summarise(mean_rel=mean(relative_target_visitation)) %>% 
  ungroup()



stingless_target_collapsed<-stingless_target %>% 
  group_by(id, companion_color,attractiveness, colony) %>% 
  summarise(mean_target=mean(number_individuals),
            mean_total=mean(total_visitation),
            mean_relative =mean(!is.na(relative_target_visitation))) 



#Ruling out temperature ----
stingless_target$companion_concentration<-as.numeric(stingless_target$companion_concentration)


temperature_model<- brm(
  formula = bf(
    total_visitation ~ temp_at_start_of_experiment + (1 | colony),
    family = zero_inflated_negbinomial(),
    autocor = cor_ar(~ timeslot | colony:treatment)) ,
  data = stingless_target,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95) 
)
summary(temperature_model)
#No effect of temperature 
save(temperature_model, file="output/model_development/model_selection_hex/temperature_model.RData")
plot(temperature_model)

autocorrelation <- brm(
  formula = bf(
    total_visitation ~ 1 + (1 | colony)
  ),
  family = negbinomial(),
  data = stingless_target,
  autocor = cor_ar(~ timeslot | id, p = 1),
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95)
)

save(autocorrelation, file="output/model_development/model_selection_hex/autocorrelation_test.RData")

#Absolute target vis with categorical + priors ----
test_priors <- c(
  prior("student_t(3, 8, 5)", class = "Intercept"), 
  #Tells me what I would expect for 
  #the intercept distribution
  #I went with students t because it is less strict
  prior(normal(0, 5), class = "b"),
  #Tells me how big of an effect I would expect for my fixed effects.
  #Which is not crazy big,
  prior(gamma(1,1), class = "shape"),  # shape parameter of the distribution
  #based on this image: https://www.researchgate.net/figure/The-gamma-distribution-function-for-various-values-of-the-shape-parameter-a_fig1_236583233
  #I will just guess 1
  prior(exponential(1), class = "sd")

)


#prior predictive checking
prior_check_abs <- brm(
  formula = bf(
    number_individuals ~ attractiveness*hex_distance + (1 | colony)+(1|id)),
  family = negbinomial() ,
  data = stingless_target,
  prior = test_priors,
  chains = 1,
  iter = 2000,
  warmup = 1000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
  sample_prior = "only"
)

pp_check(prior_check_abs,prefix="ppd", ndraws = 100) + xlim(0,100)
pp_check(prior_check_abs, prefix = "ppd", type = "stat", stat = "mean") + xlim(0,100)
pp_check(prior_check_abs, prefix = "ppd", type = "stat", stat = "sd")+xlim(0,100)
 
#running the model
abs_hex_no_autocor <- brm(
  formula = bf(
    number_individuals ~ attractiveness+hex_distance + (1 | colony)),
    family = negbinomial() ,
  data = stingless_target,
  prior = test_priors,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)

summary(abs_hex_no_autocor)
save(abs_hex_no_autocor, 
     file="output/model_development/model_selection_hex/abs_hex_no_autocor.RData")
pp_check(abs_hex_no_autocor)
plot(abs_hex_no_autocor)


#with autocor
#defining priors (same as above + autocor)
test_priors_1 <- c(
  prior("student_t(3, 8, 5)", class = "Intercept"), 
  #Tells me what I would expect for 
  #the intercept distribution
  #I went with students t because it is less strict
  prior(normal(0, 5), class = "b"),
  #Tells me how big of an effect I would expect for my fixed effects.
  #Which is not crazy big,
  prior(gamma(1,1), class = "shape"),  # shape parameter of the distribution
  #based on this image: https://www.researchgate.net/figure/The-gamma-distribution-function-for-various-values-of-the-shape-parameter-a_fig1_236583233
  #I will just guess 1
  prior(normal(0, 2), class = "ar"),  # Autokorrelationseffekt
   prior(exponential(1), class = "sd")
  #I need a prior for the colony
)

#running model
abs_hex_autocor <- brm(
 formula = bf(number_individuals ~ attractiveness + hex_distance + (1 | colony),
  autocor = cor_ar(~ timeslot | colony:treatment)),
  family = negbinomial() ,
  data = stingless_target,
  prior = test_priors,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)

summary(abs_hex_autocor)
save(abs_hex_autocor, 
     file="output/model_development/model_selection_hex/abs_hex_autocor.RData")
pp_check(abs_hex_autocor)
plot(abs_hex_autocor)

#autocor as random effect
test_priors <- c(
  prior("student_t(3, 8, 5)", class = "Intercept"), 
  #Tells me what I would expect for 
  #the intercept distribution
  #I went with students t because it is less strict
  prior(normal(0, 5), class = "b"),
  #Tells me how big of an effect I would expect for my fixed effects.
  #Which is not crazy big,
  prior(gamma(1,1), class = "shape"),  # shape parameter of the distribution
  #based on this image: https://www.researchgate.net/figure/The-gamma-distribution-function-for-various-values-of-the-shape-parameter-a_fig1_236583233
  #I will just guess 1
  prior(exponential(1), class = "sd")
  #I need a prior for the colony
)


#running the model
abs_hex_autocor_as_rf <- brm(
  formula = bf(
    number_individuals ~ attractiveness+hex_distance + (1 | colony)+(1|id)),
  family = negbinomial() ,
  data = stingless_target,
  prior = test_priors,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)

summary(abs_hex_autocor_as_rf)
save(abs_hex_autocor_as_rf, 
     file="output/model_development/model_selection_hex/abs_hex_autocor_as_rf.RData")
pp_check(abs_hex_autocor_as_rf)
plot(abs_hex_autocor_as_rf)



#interaction
#setting priors
test_priors <- c(
  prior("student_t(3, 8, 5)", class = "Intercept"), 
  #Tells me what I would expect for 
  #the intercept distribution
  #I went with students t because it is less strict
  prior(normal(0, 5), class = "b"),
  #Tells me how big of an effect I would expect for my fixed effects.
  #Which is not crazy big,
  prior(gamma(1,1), class = "shape"),  # shape parameter of the distribution
  #based on this image: https://www.researchgate.net/figure/The-gamma-distribution-function-for-various-values-of-the-shape-parameter-a_fig1_236583233
  #I will just guess 1
  prior(exponential(1), class = "sd")
  #I need a prior for the colony
)



#running the model
abs_hex_no_autocor_interaction <- brm(
  formula = bf(
    number_individuals ~ attractiveness*hex_distance + (1 | colony)),
  family = negbinomial() ,
  data = stingless_target,
  prior = test_priors,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)

summary(abs_hex_no_autocor_interaction)
save(abs_hex_no_autocor_interaction, 
     file="output/model_development/abs_hex_no_autocor_interaction.RData")
pp_check(abs_hex_no_autocor_interaction)
plot(abs_hex_no_autocor_interaction)



#with autocor
#defining priors (same as above + autocor)
test_priors_1 <- c(
  prior("student_t(3, 8, 5)", class = "Intercept"), 
  #Tells me what I would expect for 
  #the intercept distribution
  #I went with students t because it is less strict
  prior(normal(0, 5), class = "b"),
  #Tells me how big of an effect I would expect for my fixed effects.
  #Which is not crazy big,
  prior(gamma(1,1), class = "shape"),  # shape parameter of the distribution
  #based on this image: https://www.researchgate.net/figure/The-gamma-distribution-function-for-various-values-of-the-shape-parameter-a_fig1_236583233
  #I will just guess 1
  prior(normal(0, 2), class = "ar"),  # Autokorrelationseffekt
  prior(exponential(1), class = "sd")
  #I need a prior for the colony
)

#running model
abs_hex_autocor_interaction <- brm(
  formula = bf(number_individuals ~ attractiveness * hex_distance + (1 | colony),
               autocor = cor_ar(~ timeslot | colony:treatment)),
  family = negbinomial() ,
  data = stingless_target,
  prior = test_priors,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)

summary(abs_hex_autocor_interaction)
save(abs_hex_autocor_interaction, 
     file="output/model_development/model_selection_hex/abs_hex_autocor_interaction.RData")
pp_check(abs_hex_autocor_interaction)
plot(abs_hex_autocor_interaction)

#autocor as random effect
test_priors <- c(
  prior("student_t(3, 8, 5)", class = "Intercept"), 
  #Intercept distribution, student_t is less strict than normal
  #3 df, 8 as mean, 5 determines "fatness of tails"
  prior(normal(0, 5), class = "b"),
  #effect of fixed effects, centered on 0 with big sd
  prior(gamma(1,1), class = "shape"),  # shape parameter of the distribution
  #based on this image: https://www.researchgate.net/figure/The-gamma-distribution-function-for-various-values-of-the-shape-parameter-a_fig1_236583233
  prior(exponential(1), class = "sd")
  # prior for the colony
)

#running the model~
abs_hex_autocor_as_rf_interaction <- brm(
  formula = bf(
    number_individuals ~ attractiveness*hex_distance + (1 | colony)+(1|id)),
  family = negbinomial() ,
  data = stingless_target,
  prior = test_priors,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)

summary(abs_hex_autocor_as_rf_interaction)
save(abs_hex_autocor_as_rf_interaction, 
     file="output/model_development/model_selection_hex/abs_hex_autocor_as_rf_interaction.RData")
#pp_check(abs_hex_autocor_as_rf_interaction)
#plot(abs_hex_autocor_as_rf_interaction)



#Global visitation with categorical + priors----
#Obv. I need new priors for here

#Prior predictive checking
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


# global_prior_testing <- brm(
#    formula = bf(total_visitation ~ attractiveness * hex_distance + (1 | colony)),
#     family = negbinomial() ,
#     data = stingless_target,
#     prior = global_priors,
#     chains = 4,
#     iter = 2000,
#     warmup = 2000,
#     cores = parallel::detectCores(),
#     control = list(adapt_delta = 0.95),
#    sample_prior = "only"
#   )

#pp_check(global_prior_testing,prefix="ppd", ndraws = 100) + xlim(0,100)
#pp_check(global_prior_testing, prefix = "ppd", type = "stat", stat = "mean", bins=100) + xlim(0,100)

#pp_check(global_prior_testing, prefix = "ppd", type = "stat", stat = "sd")+xlim(0,100)

#running model
global_visitation_no_autocor_interaction<- brm(
  formula = bf(total_visitation ~ attractiveness * hex_distance + (1 | colony)),
  family = negbinomial() ,
  data = stingless_target,
  prior = global_priors,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)
save(global_visitation_no_autocor_interaction, file="output/model_development/model_selection_hex/global_hex_no_autocor_interaction.RData")
summary(global_hex_no_autocor_interaction)
#plot(global_hex_no_autocor_interaction)
#pp_check(global_hex_no_autocor_interaction)


#without interaction
global_hex_no_autocor<- brm(
  formula = bf(total_visitation ~ attractiveness + hex_distance + (1 | colony)),
  family = negbinomial() ,
  data = stingless_target,
  prior = global_priors,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)
save(global_hex_no_autocor, file="output/model_development/model_selection_hex/global_hex_no_autocor.RData")
summary(global_hex_no_autocor)
#plot(global_hex_no_autocor)
#pp_check(global_hex_no_autocor)


#autocor as rf and interaction
global_hex_autocor_as_rf_interaction<- brm(
  formula = bf(total_visitation ~ attractiveness * hex_distance + (1 | colony)+(1|id)),
  family = negbinomial() ,
  data = stingless_target,
  prior = global_priors,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)
save(global_hex_autocor_as_rf_interaction, file="output/model_development/model_selection_hex/global_hex_autocor_as_rf_interaction.RData")
summary(global_hex_autocor_as_rf_interaction)
#plot(global_hex_autocor_as_rf_interaction)
#pp_check(global_hex_autocor_as_rf_interaction)

#autocor as rf without interaction
global_hex_autocor_as_rf<- brm(
  formula = bf(total_visitation ~ attractiveness + hex_distance + (1 | colony)+(1|id)),
  family = negbinomial() ,
  data = stingless_target,
  prior = global_priors,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)
save(global_hex_autocor_as_rf, file="output/model_development/model_selection_hex/global_hex_autocor_as_rf.RData")
summary(global_hex_autocor_as_rf)
#plot(global_hex_autocor_as_rf)
#pp_check(global_hex_autocor_as_rf)



#with autocor
global_priors_1 <- c(
  prior("student_t(10, 20, 3)", class = "Intercept"), 
  #Tells me what I would expect for 
  #the intercept distribution
  #I went with students t because it is less strict
  #3 df, 15 as mean, 5 gives fatness of tails
  prior(normal(0, 5), class = "b"),
  #Tells me how big of an effect I would expect for my fixed effects.
  #Which is not crazy big, but I give a big sd to allow for a bunch of values
  prior(gamma(1,1), class = "shape"),  # shape parameter of the distribution
  #based on this image: https://www.researchgate.net/figure/The-gamma-distribution-function-for-various-values-of-the-shape-parameter-a_fig1_236583233
  #I will just guess 1
  prior(normal(0, 0.5), class = "ar"),  # Autokorrelationseffekt
  prior(exponential(1), class = "sd")
  
  #I need a prior for the colony
)
#running model
global_hex_autocor_interaction <- brm(
  formula = bf(total_visitation ~ attractiveness * hex_distance + (1 | colony),
               autocor = cor_ar(~ timeslot | colony:treatment)),
  family = negbinomial() ,
  data = stingless_target,
  prior = global_priors_1,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95)
)

save(global_hex_autocor_interaction, file="output/model_development/model_selection_hex/global_hex_autocor_interaction.RData")
summary(global_hex_autocor_interaction)

#plot(global_hex_autocor_interaction)
#pp_check(global_hex_autocor_interaction)

#without interaction
global_hex_autocor<- brm(
  formula = bf(total_visitation ~ attractiveness + hex_distance + (1 | colony),
               autocor = cor_ar(~ timeslot | colony:treatment)),
  family = negbinomial() ,
  data = stingless_target,
  prior = global_priors_1,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95)
)

save(global_hex_autocor, file="output/model_development/model_selection_hex/global_hex_autocor.RData")
summary(global_hex_autocor)
#plot(global_hex_autocor)
#pp_check(global_hex_autocor)


#Relative visitation with categorical and priors----

#since in the control treatment, the relative visitation is always 100%, it will
#not be considered in this analysis
#Therefore, creating new df without control
stingless_target_no_control<-filter(stingless_target, treatment!="5")

#having a look at the distribution

#hist(stingless_target_no_control$relative_target_visitation)
#As now I have values between 0 and 1
#Since my data is not in integers anymore, I cannot use neg binomial
#I switch to a binomial distribution
#Here the relative distribution is factored in as "Successes"/"trials" 
#This allows me to have results from 0 to 1 whereas a beta distribution does not allow to have either 0 or 1

#prior predictive check

#defining priors
relative_priors <- c(
  prior("student_t(5, 0.3, 3)", class = "Intercept"), 
  #intercept distribution, students t because it is less strict
  #5 df, 0.3 as mean, 3 gives fatness of tails
  prior(normal(0, 5), class = "b"),
  #Centered on 0, large sd
  prior(exponential(1), class = "sd")
  #I need a prior for the colony
)

#
# relative_prior_checking <- brm(
#   formula = bf(
#     #Both fixed effects but without interaction, colony as random effect
#     number_individuals|trials(total_visitation) ~ attractiveness+hex_distance + (1 | colony),
#     #Change of family because of described changes in data structure
#     family = binomial()),
#   data = stingless_target_no_control,
#   chains = 4,
#   iter = 2000,
#   warmup = 2000,
#   #cores to run stuff in parallel for faster runtime
#   cores = parallel::detectCores(),
#   prior = relative_priors,
#   #Strictness?
#   control = list(adapt_delta = 0.95),
#   sample_prior="only"
# )
# 
# pp_check(relative_prior_checking, prefix="ppd")


#running the model
relative_hex_no_autocor <- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    number_individuals|trials(total_visitation) ~ attractiveness+hex_distance + (1 | colony),
    #Change of family because of described changes in data structure
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(relative_hex_no_autocor,   file = "output/model_development/model_selection_hex/relative_hex_no_autocor.RData")
#With interaction

relative_hex_no_autocor_interaction <- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    number_individuals|trials(total_visitation) ~ attractiveness*hex_distance + (1 | colony),
    #Change of family because of described changes in data structure
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors,
  #Strictness?
  control = list(adapt_delta = 0.95))
 
save(relative_hex_no_autocor_interaction, file = "output/model_development/model_selection_hex/relative_hex_no_autocor_interaction.RData"
)

#autocor as rf
relative_hex_autocor_as_rf <- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    number_individuals|trials(total_visitation) ~ attractiveness+hex_distance + (1 | colony)+(1|id),
    #Change of family because of described changes in data structure
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(relative_hex_autocor_as_rf,   file = "output/model_development/model_selection_hex/relative_hex_autocor_as_rf.RData")

#autocor as rf + interaction
relative_hex_autocor_as_rf_interaction <- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    number_individuals|trials(total_visitation) ~ attractiveness*hex_distance + (1 | colony)+(1|id),
    #Change of family because of described changes in data structure
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(relative_hex_autocor_as_rf_interaction,   file = "output/model_development/model_selection_hex/relative_hex_autocor_as_rf_interaction.RData")
#autocor
relative_priors_autocor <- c(
  prior("student_t(5, 0.3, 3)", class = "Intercept"), 
  #Tells me what I would expect for 
  #the intercept distribution
  #I went with students t because it is less strict
  #5 df, 0.3 as mean, 3 gives fatness of tails
  prior(normal(0, 5), class = "b"),
  #Tells me how big of an effect I would expect for my fixed effects.
  #Which is not crazy big, but I give a big sd to allow for a bunch of values
  prior(exponential(1), class = "sd"),
  #I need a prior for the colony
  prior(normal(0, 0.5), class = "ar")
)

#model
relative_hex_autocor <- brm(
  formula = bf(number_individuals|trials(total_visitation)~ attractiveness + hex_distance + (1 | colony),
                         autocor = cor_ar(~ timeslot | colony:treatment)),
    #Change of family because of described changes in data structure
    family = binomial(),
  data = stingless_target_no_control,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors_autocor,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(relative_hex_autocor,  file = "output/model_development/model_selection_hex/relative_hex_autocor.RData")

#autocor + interaction
relative_hex_autocor_interaction <- brm(
  formula = bf(number_individuals|trials(total_visitation) ~ attractiveness *hex_distance + (1 | colony),
               autocor = cor_ar(~ timeslot | colony:treatment)),
  #Change of family because of described changes in data structure
  family = binomial(),
  data = stingless_target_no_control,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors_autocor,
  #Strictness?
  control = list(adapt_delta = 0.95)
  )

save(relative_hex_autocor_interaction,     file = "output/model_development/model_selection_hex/relative_hex_autocor_interaction.RData")


#testing day as random effect----
##absolute

absolute_priors <- c(
  prior("student_t(3, 8, 5)", class = "Intercept"), 
  #Intercept distribution, student_t is less strict than normal
  #3 df, 8 as mean, 5 determines "fatness of tails"
  prior(normal(0, 5), class = "b"),
  #effect of fixed effects, centered on 0 with big sd
  prior(gamma(1,1), class = "shape"),  # shape parameter of the distribution
  #based on this image: https://www.researchgate.net/figure/The-gamma-distribution-function-for-various-values-of-the-shape-parameter-a_fig1_236583233
  prior(exponential(1), class = "sd"))
# prior for the colony


abs_hex_interaction_day_as_rf <- brm(
  formula = bf(
    number_individuals ~ attractiveness*hex_distance + (1 | colony)+(1|date)),
  family = negbinomial() ,
  data = stingless_target,
  prior = absolute_priors,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)

save(abs_hex_interaction_day_as_rf, file="output/model_development/model_selection_hex/abs_hex_interaction_day_as_rf.RData")


summary(abs_hex_interaction_day_as_rf)


#global
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

global_visitation_interaction_day_as_rf<- brm(
  formula = bf(total_visitation ~ attractiveness * hex_distance + (1 | colony)+(1|date)),
  family = negbinomial() ,
  data = stingless_target,
  prior = global_priors,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)
save(global_visitation_interaction_day_as_rf, file="output/model_development/model_selection_hex/global_hex_interaction_day_as_rf.RData")

#relative

stingless_target_no_control<-filter(stingless_target, treatment!="5")


#defining priors
relative_priors <- c(
  prior("student_t(5, 0.3, 3)", class = "Intercept"), 
  #intercept distribution, students t because it is less strict
  #5 df, 0.3 as mean, 3 gives fatness of tails
  prior(normal(0, 5), class = "b"),
  #Centered on 0, large sd
  prior(exponential(1), class = "sd")
  #I need a prior for the colony
)


relative_hex_interaction_day_as_rf <- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    number_individuals|trials(total_visitation) ~ attractiveness*hex_distance + (1 | colony) +(1|date),
    #Change of family because of described changes in data structure
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(relative_hex_interaction_day_as_rf, file="output/model_development/model_selection_hex/relative_hex_interaction_day_as_rf.RData")

#color as fixed effect----
absolute_priors <- c(
  prior("student_t(3, 8, 5)", class = "Intercept"),
  #Intercept distribution, student_t is less strict than normal
  #3 df, 8 as mean, 5 determines "fatness of tails"
  prior(normal(0, 5), class = "b"),
  #effect of fixed effects, centered on 0 with big sd
  prior(gamma(1,1), class = "shape"),  # shape parameter of the distribution
  #based on this image: https://www.researchgate.net/figure/The-gamma-distribution-function-for-various-values-of-the-shape-parameter-a_fig1_236583233
  prior(exponential(1), class = "sd"))
# prior for the colony


abs_hex_interaction_color <- brm(
  formula = bf(
    number_individuals ~ attractiveness*companion_color + (1 | colony)+(1|id)),
  family = negbinomial() ,
  data = stingless_target,
  prior = absolute_priors,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)

save(abs_hex_interaction_color, file="output/model_development/model_selection_hex/abs_hex_interaction_color.RData")


#global
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
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)
save(global_visitation_interaction_color, file="output/model_development/model_selection_hex/global_hex_interaction_color.RData")

#relative

stingless_target_no_control<-filter(stingless_target, treatment!="5")


#defining priors
relative_priors <- c(
  prior("student_t(5, 0.3, 3)", class = "Intercept"),
  #intercept distribution, students t because it is less strict
  #5 df, 0.3 as mean, 3 gives fatness of tails
  prior(normal(0, 5), class = "b"),
  #Centered on 0, large sd
  prior(exponential(1), class = "sd")
  #I need a prior for the colony
)


relative_hex_interaction_color<- brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    number_individuals|trials(total_visitation) ~ attractiveness*companion_color + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = relative_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(relative_hex_interaction_color, file="output/model_development/model_selection_hex/relative_hex_interaction_color.RData")


### Hexdistance models----
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



#absolute target visitation
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
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = test_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(absolute_hexdistance, file="output/model_development/model_selection_hex/absolute_hexdistance.RData")


#relative
#
relative_hexdistance<- brm(
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
  prior = relative_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(relative_hexdistance, file="output/model_development/model_selection_hex/relative_hexdistance.RData")

summary(relative_hexdistance)

#global
#
global_hexdistance<- brm(
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
  prior = test_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(global_hexdistance, file="output/model_development/model_selection_hex/global_hexdistance.RData")



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
    prior(exponential(1), class = "sd")
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
  prior(exponential(1), class = "sd")
  #Prior for the colony
)

hue_relative_priors <- c(
  prior("student_t(5, 0.3, 3)", class = "Intercept"),
  #intercept distribution, students t because it is less strict
  #5 df, 0.3 as mean, 3 gives fatness of tails
  prior(normal(0, 5), class = "b"),
  #Centered on 0, large sd
  prior(exponential(1), class = "sd")
  #I need a prior for the colony
)


##absolute ----
hue_abs_model<-brm(bf(number_individuals ~ attractiveness*h.theta + (1 | colony) +(1|id),
                      family = negbinomial()),
                   data = stingless_target,
                   chains = 4,
                   iter = 6000,
                   warmup = 2000,
                   #cores to run stuff in parallel for faster runtime
                   cores = parallel::detectCores(),
                   prior = hue_abs_priors,
                   #Strictness?
                   control = list(adapt_delta = 0.95))

save(hue_abs_model, file="output/model_development/model_selection_hex/hue_abs_model.RData")

saturation_abs_model<-brm(bf(number_individuals ~ attractiveness*r.vec + (1 | colony) +(1|id),
                             family = negbinomial()),
                          data = stingless_target,
                          chains = 4,
                          iter = 6000,
                          warmup = 2000,
                          #cores to run stuff in parallel for faster runtime
                          cores = parallel::detectCores(),
                          prior = hue_abs_priors,
                          #Strictness?
                          control = list(adapt_delta = 0.95))

save(saturation_abs_model, file="output/model_development/model_selection_hex/saturation_abs_model.RData")

brighntess_abs_model<-brm(bf(number_individuals ~ attractiveness*lum + (1 | colony) +(1|id),
                             family = negbinomial()),
                          data = stingless_target,
                          chains = 4,
                          iter = 6000,
                          warmup = 2000,
                          #cores to run stuff in parallel for faster runtime
                          cores = parallel::detectCores(),
                          prior = hue_abs_priors,
                          #Strictness?
                          control = list(adapt_delta = 0.95))

save(brighntess_abs_model, file="output/model_development/model_selection_hex/brightness_abs_model.RData")

##Relative ----

stingless_target_no_control<-filter(stingless_target, treatment!="5")


hue_relative_model<-brm(
  formula = bf(
    number_individuals|trials(total_visitation) ~ attractiveness*h.theta + (1 | colony) +(1|id),
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = hue_relative_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(hue_relative_model, file="output/model_development/model_selection_hex/hue_relative_model.RData")


saturation_relative_model<-brm(
  formula = bf(
    number_individuals|trials(total_visitation) ~ attractiveness*r.vec + (1 | colony) +(1|id),
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = hue_relative_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(saturation_relative_model, file="output/model_development/model_selection_hex/saturation_relative_model.RData")

brightness_relative_model<-brm(
  formula = bf(
    number_individuals|trials(total_visitation) ~ attractiveness*lum + (1 | colony) +(1|id),
    family = binomial()),
  data = stingless_target_no_control,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = hue_relative_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)

save(brightness_relative_model, file="output/model_development/model_selection_hex/brightness_relative_model.RData")

## global----
hue_global_model<-brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    total_visitation ~ attractiveness*h.theta + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = hue_global_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(hue_global_model, file="output/model_development/model_selection_hex/hue_global_model.RData")

saturation_global_model<-brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    total_visitation ~ attractiveness*r.vec + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = hue_global_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(saturation_global_model, file="output/model_development/model_selection_hex/saturation_global_model.RData")

brightness_global_model<-brm(
  formula = bf(
    #Both fixed effects but without interaction, colony as random effect
    total_visitation ~ attractiveness*lum + (1 | colony) +(1|id),
    #Change of family because of described changes in data structure
    family = negbinomial()),
  data = stingless_target,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  #cores to run stuff in parallel for faster runtime
  cores = parallel::detectCores(),
  prior = hue_global_priors,
  #Strictness?
  control = list(adapt_delta = 0.95)
)
save(brightness_global_model, file="output/model_development/model_selection_hex/brightness_global_model.RData")

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




#Collapsed data----

#data transformation

stingless_target_collapsed<-stingless_target %>%
  group_by(id, companion_color,attractiveness, colony,hex_distance) %>%
  summarise(mean_target=round(mean(number_individuals)),
            mean_total=round(mean(total_visitation)),
            mean_relative =mean(!is.na(relative_target_visitation)))


#
abs_collapsed <- brm(
  formula = bf(
    mean_target~ attractiveness*hex_distance + (1 | colony)),
  family = negbinomial() ,
  data = stingless_target_collapsed,
  prior = test_priors,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)
save(abs_collapsed, file="output/model_development/model_selection_hex/abs_collapsed.RData")


#relative
#
rel_collapsed <- brm(
  formula = bf(
    mean_target|trials(mean_total)~ attractiveness*hex_distance + (1 | colony)),
  family = binomial() ,
  data = filter(stingless_target_collapsed, hex_distance !="control"),
  prior = relative_priors,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)
save(rel_collapsed, file="output/model_development/model_selection_hex/rel_collapsed.RData")

summary(rel_collapsed)
#global

global_collapsed <- brm(
  formula = bf(
    mean_total~ attractiveness*hex_distance + (1 | colony)),
  family = negbinomial() ,
  data = stingless_target_collapsed,
  prior = test_priors,
  chains = 4,
  iter = 6000,
  warmup = 2000,
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.95),
)
save(global_collapsed, file="output/model_development/model_selection_hex/global_collapsed.RData")

#Checking for overdispersion of my absolute number of individuals at target----
# mean <- mean(stingless_target$number_individuals)
# variance<- var(stingless_target$number_individuals)
#
# var_mean_ratio <- variance / mean
# print(var_mean_ratio)
# #My var_mean_ratio is greater than 5, therefore I have serious overdispersion
# #This means I cannot use a poisson distribution, but instead a negative binomial
#
# #What about my relative data?
# stingless_target$relative_target_visitation<-as.numeric(stingless_target$relative_target_visitation)
# rel_mean <- mean(stingless_target$relative_target_visitation, na.rm =T)
# rel_variance<- var(stingless_target$relative_target_visitation, na.rm =T)
#
# rel_var_mean_ratio <- rel_variance / rel_mean
# print(rel_var_mean_ratio)
# #I guess that would be underdispersed? Since I will be using a different
# #Kind of family (see below in the model), I think that´s alright
#
