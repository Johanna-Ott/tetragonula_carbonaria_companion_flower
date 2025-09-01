# Still needs some tidying up
#loading pacakges --------------------------------------------------------

library(insight)
library(sjstats)
library(tidyverse)




# loading models ----------------------------------------------------------
#Empty environment so that I only have my models in there
rm(list=ls())

#loading all models based on location
# load("output/model_development/hexdistance/absolute_hexdistance.RData")
# load("output/model_development/hexdistance/relative_hexdistance.RData")
# load("output/model_development/hexdistance/global_hexdistance.RData")

load("output/model_development/hue_abs_model.RData")
load("output/model_development/hue_global_model.RData")
load("output/model_development/hue_relative_model.RData")
load("output/model_development/saturation_abs_model.RData")
load("output/model_development/saturation_global_model.RData")
load("output/model_development/saturation_relative_model.RData")
load("output/model_development/brightness_abs_model.RData")
load("output/model_development/brightness_global_model.RData")
load("output/model_development/brightness_relative_model.RData")
#saving names of all model names in list
mylist<-ls()
#lapply(list.files("output/model_development/hexdistance/", 
#                  pattern = "\\.RData$", full.names = TRUE),load, envir = .GlobalEnv)

#saving all objects into list based on name
brms_list <- mget(mylist)


results_list <- lapply(brms_list, parameters::model_parameters, effects = "all")


formula_list<-lapply(brms_list, function(model) summary(model)$formula)

fixed_list<-lapply(brms_list, function(model) summary(model)$fixed)



fixed_df <- bind_rows(
  lapply(names(fixed_list), function(name) {
    cbind(model = name, fixed_list[[name]])
  }),
  .id = "id"
)




random_effects_df <- map_df(names(brms_list), function(model_name) {
  random_parts <- summary(brms_list[[model_name]])$random
  
  # If there are multiple grouping factors (each a data.frame), loop over them
  map_df(names(random_parts), function(grouping) {
    df <- random_parts[[grouping]]
    df$group <- grouping  # Add grouping factor
    df$model <- model_name  # Add model name
    df
  })
})


all_estimates<-bind_rows(fixed_df, random_effects_df) %>% 
  select(-id)

all_estimates<-all_estimates[order(all_estimates$model), ]

write.csv(all_estimates, file="output/colorcharacteristics_model_summary_table.csv")

install.packages("stargazer")
library(stargazer)
y<-stargazer(all_estimates, type = "html", summary = FALSE, digits = 3)


# loading models ----------------------------------------------------------
#Empty environment so that I only have my models in there
rm(list=ls())

#loading all models based on location
load("output/model_development/global_visitation_categorical_interaction_color.RData")
load("output/model_development/abs_target_visitation_categorical_interaction_color.RData")
load("output/model_development/relative_visitation_categorical_interaction_color.RData")
#saving names of all model names in list
mylist<-ls()

#saving all objects into list based on name
brms_list <- mget(mylist)


results_list <- lapply(brms_list, parameters::model_parameters, effects = "all")


formula_list<-lapply(brms_list, function(model) summary(model)$formula)

fixed_list<-lapply(brms_list, function(model) summary(model)$fixed)



fixed_df <- bind_rows(
  lapply(names(fixed_list), function(name) {
    cbind(model = name, fixed_list[[name]])
  }),
  .id = "id"
)




random_effects_df <- map_df(names(brms_list), function(model_name) {
  random_parts <- summary(brms_list[[model_name]])$random
  
  # If there are multiple grouping factors (each a data.frame), loop over them
  map_df(names(random_parts), function(grouping) {
    df <- random_parts[[grouping]]
    df$group <- grouping  # Add grouping factor
    df$model <- model_name  # Add model name
    df
  })
})


all_estimates<-bind_rows(fixed_df, random_effects_df) %>% 
  select(-id)

all_estimates<-all_estimates[order(all_estimates$model), ]

write.csv(all_estimates, file="output/color_model_summary_table.csv")

