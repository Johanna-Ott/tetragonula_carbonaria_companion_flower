#Visualizing Sensitivity analysis2 


# libraries ---------------------------------------------------------------
library(tidyverse)
library(brms)
library(ggplot2)
library(ggpubr)
library(MCMCvis) # for mcmcplot
library(tidybayes) # for get_variables

# data --------------------------------------------------------------------

long_data<-read.csv("data/stingless_bees_tidy1.csv")
stingless_target <- filter(long_data, species=="stingless", flowertype =="target")

#loading all the models 
lapply(list.files("output/sensitivity_analysis2/relative", 
                  pattern = "\\.RData$", full.names = TRUE),load, envir = .GlobalEnv)
lapply(list.files("output/sensitivity_analysis2/absolute", 
                  pattern = "\\.RData$", full.names = TRUE),load, envir = .GlobalEnv)
lapply(list.files("output/sensitivity_analysis2/global", 
                  pattern = "\\.RData$", full.names = TRUE),load, envir = .GlobalEnv)

# original models
load("output/model_development/relative_hexdistance.RData")
load("output/model_development/absolute_hexdistance.RData")
load("output/model_development/global_hexdistance.RData")
# trying out plotting -----------------------------------------------------
# I want to plot multiple models into one plot as they do in
# https://cran.r-project.org/web/packages/MCMCvis/vignettes/MCMCvis.html#mcmctrace



# creating color scheme
model_colors_rel <- c("Baseline" = "red",
                  "Model 1" = "black","Model 2" = "black","Model 3" = "black",
                  "Model 4" = "black","Model 5" = "black","Model 6" = "black",
                  "Model 7" = "black","Model 8" = "black","Model 9" = "black")

#creating large df with draws from all relative models of the sensitivity analyses
all_draws_reative <-bind_rows(
  spread_draws(relative_hexdistance, b_Intercept,b_hex_distance, b_attractivenesslow, `b_attractivenesslow:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Baseline"),
  spread_draws(relative_hexdistance_fixed_down, b_Intercept, b_hex_distance, b_attractivenesslow, `b_attractivenesslow:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 5"),
  spread_draws(relative_hexdistance_fixed_up, b_Intercept, b_hex_distance, b_attractivenesslow, `b_attractivenesslow:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 6"),
  spread_draws(relative_hexdistance_fixed_narrow, b_Intercept, b_hex_distance, b_attractivenesslow, `b_attractivenesslow:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 7"),
  spread_draws(relative_hexdistance_fixed_wide, b_Intercept, b_hex_distance, b_attractivenesslow, `b_attractivenesslow:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 8"),
  spread_draws(relative_hexdistance_Intercept_down, b_Intercept, b_hex_distance, b_attractivenesslow, `b_attractivenesslow:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 1"),
  spread_draws(relative_hexdistance_Intercept_up, b_Intercept, b_hex_distance, b_attractivenesslow, `b_attractivenesslow:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 2"),
  spread_draws(relative_hexdistance_Intercept_narrow, b_Intercept, b_hex_distance, b_attractivenesslow, `b_attractivenesslow:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 3"),
  spread_draws(relative_hexdistance_Intercept_wide, b_Intercept, b_hex_distance, b_attractivenesslow, `b_attractivenesslow:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 4"),
  spread_draws(relative_hexdistance_sd2, b_Intercept, b_hex_distance, b_attractivenesslow, `b_attractivenesslow:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 9")
 )

#creating plot 
(hexdistance_sensitivity<-all_draws_relative %>%
    # relevelling column so that the models are in the right order in the plot
    mutate(model = fct_relevel(model, "Baseline", "Model 1", "Model 2", "Model 3", 
                               "Model 4", "Model 5","Model 6", "Model 7", "Model 8")) %>% 
    # bringing dataframe in long form for plotting
  pivot_longer(cols=c("b_Intercept","b_hex_distance", "b_attractivenesslow", 
                      "b_attractivenesslow:hex_distance", "sd_colony__Intercept",
                      "sd_id__Intercept"), 
               names_to= "parameter", 
               values_to="values") %>% 
  # bringing parameters into right order  
  mutate(parameter = fct_relevel(parameter, "b_Intercept","b_hex_distance", 
                                 "b_attractivenesslow", "b_attractivenesslow:hex_distance", 
                                 "sd_colony__Intercept", "sd_id__Intercept")) %>% 
  # creating plots
  ggplot(aes(x = values, y = model, color = model)) +
  # point intervals plot
  stat_pointinterval(.width = c(0.5, 0.95)) +
  # applying colors to make only original model red
  scale_color_manual(values=model_colors_rel)+
  # splitting plots by parameters
  facet_grid(~parameter, scales= "free_x" )+
  # theme to make it look pretty
  theme_bw()+
  # removing legend and panel lines
  theme(legend.position="none",
        panel.grid.major.y = element_blank())+
  # inserting vertical line to show 0-effect
 geom_vline(xintercept=0, linetype="dashed")+
 # adding title
  labs(title="Estimates for sensitivity analysis models"))


# saving picture
ggsave(hexdistance_sensitivity, 
       file="output/plots/sensitivity_relative/hexdistance_estimate_comparisons.png",
       height=5.5,
       width=14,
       bg="white")


# Absolute ----------------------------------------------------------------

# defining colors
model_colors <- c("Baseline" = "red",
                  "Model 1" = "black","Model 2" = "black","Model 3" = "black",
                  "Model 4" = "black","Model 5" = "black","Model 6" = "black",
                  "Model 7" = "black","Model 8" = "black","Model 9" = "black",
                  "Model 10" = "black", "Model 11" = "black", "Model 12" = "black")

# creating dataframe with all the draws from absolute models
all_draws_abs <-bind_rows(
  spread_draws(absolute_hex_baseline,b_Intercept, b_hex_distance, b_attractivenesslow,b_attractivenesshigh, `b_attractivenesslow:hex_distance`,`b_attractivenesshigh:hex_distance`,sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Baseline"),
  spread_draws(absolute_hex_fixed_down, b_Intercept,b_hex_distance, b_attractivenesslow, b_attractivenesshigh,`b_attractivenesslow:hex_distance`, `b_attractivenesshigh:hex_distance`,sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 5"),
  spread_draws(absolute_hex_fixed_up,b_Intercept, b_hex_distance, b_attractivenesslow, b_attractivenesshigh,`b_attractivenesslow:hex_distance`, `b_attractivenesshigh:hex_distance`,sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 6"),
  spread_draws(absolute_hex_fixed_narrow, b_Intercept,b_hex_distance, b_attractivenesslow,b_attractivenesshigh, `b_attractivenesslow:hex_distance`,`b_attractivenesshigh:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 7"),
  spread_draws(absolute_hex_fixed_wide, b_Intercept,b_hex_distance, b_attractivenesslow,b_attractivenesshigh, `b_attractivenesslow:hex_distance`, `b_attractivenesshigh:hex_distance`,sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 8"),
  spread_draws(absolute_hex_Intercept_down, b_Intercept,b_hex_distance, b_attractivenesslow, b_attractivenesshigh,`b_attractivenesslow:hex_distance`,`b_attractivenesshigh:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 1"),
  spread_draws(absolute_hex_Intercept_up, b_Intercept,b_hex_distance, b_attractivenesslow,b_attractivenesshigh, `b_attractivenesslow:hex_distance`,`b_attractivenesshigh:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 2"),
  spread_draws(absolute_hex_Intercept_narrow,b_Intercept, b_hex_distance, b_attractivenesslow, b_attractivenesshigh,`b_attractivenesslow:hex_distance`,`b_attractivenesshigh:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 3"),
  spread_draws(absolute_hex_Intercept_wide,b_Intercept, b_hex_distance, b_attractivenesslow,b_attractivenesshigh, `b_attractivenesslow:hex_distance`,`b_attractivenesshigh:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 4"),
  spread_draws(absolute_hex_shape1, b_Intercept,b_hex_distance, b_attractivenesslow, b_attractivenesshigh,`b_attractivenesslow:hex_distance`, `b_attractivenesshigh:hex_distance`,sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 9"),
  spread_draws(absolute_hex_shape2, b_Intercept,b_hex_distance, b_attractivenesslow,b_attractivenesshigh, `b_attractivenesslow:hex_distance`,`b_attractivenesshigh:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 10"),
  spread_draws(absolute_hex_sd1, b_Intercept,b_hex_distance, b_attractivenesslow,b_attractivenesshigh, `b_attractivenesslow:hex_distance`, `b_attractivenesshigh:hex_distance`,sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 11"),
  spread_draws(absolute_hex_sd2,b_Intercept, b_hex_distance, b_attractivenesslow, b_attractivenesshigh,`b_attractivenesslow:hex_distance`, `b_attractivenesshigh:hex_distance`,sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 12"))


#creating plot
(abs_hexdistance_sensitivity<-all_draws_abs %>%
    # reordering levels to make them appear in the right order in the plot
    mutate(model = fct_relevel(model, "Baseline", "Model 1", "Model 2", "Model 3", 
                               "Model 4", "Model 5",
                               "Model 6", "Model 7", "Model 8", "Model 9", 
                               "Model 10", "Model 11", "Model 12")) %>% 
    # bringing df into long form for plotting 
    pivot_longer(cols=c("b_Intercept","b_hex_distance", "b_attractivenesslow",
                                 "b_attractivenesshigh", "b_attractivenesslow:hex_distance",
                                 "b_attractivenesshigh:hex_distance", "sd_colony__Intercept", 
                                 "sd_id__Intercept"), 
                 names_to= "parameter", values_to="values") %>%
    # relevelling parameters for plotting
    mutate(parameter=fct_relevel(parameter,"b_Intercept","b_hex_distance", 
                                 "b_attractivenesslow", "b_attractivenesshigh",
                                 "b_attractivenesslow:hex_distance",
                                 "b_attractivenesshigh:hex_distance", 
                                 "sd_colony__Intercept", "sd_id__Intercept")) %>%
    # plotting
    ggplot(aes(x = values, y = model, color = model)) +
    # adding point interval layer
    stat_pointinterval(.width = c(0.5, 0.95)) +
    # applying colors
    scale_color_manual(values=model_colors)+
    # splitting up into panels per parameter
    facet_grid(~parameter, scales= "free_x")+
    # adding vertical line at zero to show where the 0-effect is
    geom_vline(xintercept= 0, linetype="dashed")+
    # applying theme to make it pretty
    theme_bw()+
    # removing legend and panel lines
    theme(legend.position="none",
          panel.grid.major.y = element_blank()
    )+
    # adding title
    labs(title="Estimates for sensitivity analysis models"))

# saving plot
ggsave(abs_hexdistance_sensitivity, 
       file="output/plots/sensitivity_relative/ABSOLUTE_hexdistance_estimate_comparisons.png",
       height=5.5,
       width=14.22,
       bg="white")


# Global ----------------------------------------------------------------

# color scheme
model_colors <- c("Baseline" = "red",
                  "Model 1" = "black","Model 2" = "black","Model 3" = "black",
                  "Model 4" = "black","Model 5" = "black","Model 6" = "black",
                  "Model 7" = "black","Model 8" = "black","Model 9" = "black",
                  "Model 10" = "black", "Model 11" = "black", "Model 12" = "black")

# creating df with all draws from global models
all_draws_global <-bind_rows(
  spread_draws(global_hex_baseline,b_Intercept, b_hex_distance, b_attractivenesslow,b_attractivenesshigh, `b_attractivenesslow:hex_distance`,`b_attractivenesshigh:hex_distance`,sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Baseline"),
  spread_draws(global_hex_fixed_down, b_Intercept,b_hex_distance, b_attractivenesslow, b_attractivenesshigh,`b_attractivenesslow:hex_distance`, `b_attractivenesshigh:hex_distance`,sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 5"),
  spread_draws(global_hex_fixed_up,b_Intercept, b_hex_distance, b_attractivenesslow, b_attractivenesshigh,`b_attractivenesslow:hex_distance`, `b_attractivenesshigh:hex_distance`,sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 6"),
  spread_draws(global_hex_fixed_narrow, b_Intercept,b_hex_distance, b_attractivenesslow,b_attractivenesshigh, `b_attractivenesslow:hex_distance`,`b_attractivenesshigh:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 7"),
  spread_draws(global_hex_fixed_wide, b_Intercept,b_hex_distance, b_attractivenesslow,b_attractivenesshigh, `b_attractivenesslow:hex_distance`, `b_attractivenesshigh:hex_distance`,sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 8"),
  spread_draws(global_hex_Intercept_down, b_Intercept,b_hex_distance, b_attractivenesslow, b_attractivenesshigh,`b_attractivenesslow:hex_distance`,`b_attractivenesshigh:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 1"),
  spread_draws(global_hex_Intercept_up, b_Intercept,b_hex_distance, b_attractivenesslow,b_attractivenesshigh, `b_attractivenesslow:hex_distance`,`b_attractivenesshigh:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 2"),
  spread_draws(global_hex_Intercept_narrow,b_Intercept, b_hex_distance, b_attractivenesslow, b_attractivenesshigh,`b_attractivenesslow:hex_distance`,`b_attractivenesshigh:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 3"),
  spread_draws(global_hex_Intercept_wide,b_Intercept, b_hex_distance, b_attractivenesslow,b_attractivenesshigh, `b_attractivenesslow:hex_distance`,`b_attractivenesshigh:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 4"),
  spread_draws(global_hex_shape1, b_Intercept,b_hex_distance, b_attractivenesslow, b_attractivenesshigh,`b_attractivenesslow:hex_distance`, `b_attractivenesshigh:hex_distance`,sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 9"),
  spread_draws(global_hex_shape2, b_Intercept,b_hex_distance, b_attractivenesslow,b_attractivenesshigh, `b_attractivenesslow:hex_distance`,`b_attractivenesshigh:hex_distance`, sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 10"),
  spread_draws(global_hex_sd1, b_Intercept,b_hex_distance, b_attractivenesslow,b_attractivenesshigh, `b_attractivenesslow:hex_distance`, `b_attractivenesshigh:hex_distance`,sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 11"),
  spread_draws(global_hex_sd2,b_Intercept, b_hex_distance, b_attractivenesslow, b_attractivenesshigh,`b_attractivenesslow:hex_distance`, `b_attractivenesshigh:hex_distance`,sd_colony__Intercept, sd_id__Intercept) %>% mutate(model = "Model 12"))


# creating plot
(global_hexdistance_sensitivity<-all_draws_global %>%
    # reordering levels to allow plotting in the right order
    mutate(model = fct_relevel(model, "Baseline", "Model 1", "Model 2", 
                               "Model 3", "Model 4", "Model 5",
                               "Model 6", "Model 7", "Model 8", 
                               "Model 9", "Model 10", "Model 11", "Model 12")) %>% 
    # bringing df into long form for plotting
    pivot_longer(cols=c("b_Intercept","b_hex_distance", "b_attractivenesslow", 
                        "b_attractivenesshigh", "b_attractivenesslow:hex_distance",
                        "b_attractivenesshigh:hex_distance", "sd_colony__Intercept", 
                        "sd_id__Intercept"), 
                 names_to= "parameter", values_to="values") %>% 
    # relevelling parameter make them plot in the right order
    mutate(parameter=fct_relevel(parameter,"b_Intercept","b_hex_distance", "b_attractivenesslow", 
                                 "b_attractivenesshigh", "b_attractivenesslow:hex_distance",
                                 "b_attractivenesshigh:hex_distance", "sd_colony__Intercept", 
                                 "sd_id__Intercept")) %>% 
    # plotting
    ggplot(aes(x = values, y = model, color = model)) +
    #adding layer with point interval
    stat_pointinterval(.width = c(0.5, 0.95)) +
    # adding color scheme
    scale_color_manual(values=model_colors)+
    # facetting per parameter
    facet_grid(~parameter, scales= "free_x")+
    # adding vertical line to show 0 effect
    geom_vline(xintercept= 0, linetype="dashed")+
    # adjusting theme to make it pretty
    theme_bw()+
    # removing legend and panel lines
    theme(legend.position="none",
          panel.grid.major.y = element_blank())+
    # title
    labs(title="Estimates for sensitivity analysis models"))

#saving picture
ggsave(global_hexdistance_sensitivity, file="output/plots/sensitivity_relative/global_hexdistance_estimate_comparisons.png",
       height=5.5,
       width=14.22,
       bg="white")

