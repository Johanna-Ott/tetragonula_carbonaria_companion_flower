# Visualization of color models


# libraries ---------------------------------------------------------------
library(tidyverse)
library(brms)
library(ggplot2)
library(ggpubr)
library(MCMCvis) # for mcmcplot
library(tidybayes) # for get_variables
library(ggnewscale)
library(patchwork) # for panel arrangement

#data----

#loading in data
stingless_target<-read.csv("data/stingless_bees_tidy1.csv") %>% 
  mutate(condition=paste(companion_color, attractiveness, sep="_"))
load("output/model_development/relative_visitation_categorical_interaction_color.RData")
load("output/model_development/abs_target_visitation_categorical_interaction_color.RData")
load("output/model_development/global_visitation_categorical_interaction_color.RData")
#data frame for relative stuff
stingless_target_no_control<-filter(stingless_target, treatment!="5")
stingless_target_no_control<-stingless_target %>% 
  filter(similarity!="control") %>% 
  mutate(companion_color=as.factor(companion_color))
#colors and functions -----------------------------------------------------
color_colors<-c("dark_blue"="blue3", 
                "light_blue"="lightblue",
                "red" = "orangered",
                "yellow"="gold",#"yellow",
                "orange"="orange")

text_colors<-c("white","black","black","black","white","black","black","black")

text_colors_1<-c("white","black","black","black","white","black","black","black","black")


#colors for line indiciating mean and cis

point_col <-rep(c("black","white","black","black"),length.out=4024)
line_col<-rep(c("white","white","white","black","black","black","black","black"
                ,"black","black","black","black","black","black","black"),
              length.out=4024)

# function to make plots look uniform
make_it_pretty<-function(){
  theme_classic()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.x = element_text(size=12),
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y=element_text(size=12),
          axis.text.y=element_text(size=12),
          #axis.text.x.bottom = element_blank(),
          axis.ticks.x=element_blank()
    )
}

#Relative----

#get predicted draws from model: create grid

mean_trials <-  round(mean(stingless_target_no_control$total_visitation))


new_data_rel <- expand.grid(x = levels(as.factor(stingless_target_no_control$companion_color)), z = c("high","low")) 
# adding dummy colony and id for plotting
new_data_rel<-new_data_rel%>%
  mutate(colony = "dummy_colony",
         id = "dummy_id",
         total_visitation= mean_trials)%>% 
  rename(companion_color =x,
         attractiveness=z)

#adding predicted draws, calculating proportional visitation
posterior_relative <- relative_visitation_categorical_interaction_color %>%
  epred_draws(newdata = new_data_rel, allow_new_levels=T, re_formula=NA) %>%
  # specifying condition as the interaction between companion color and concentration 
  mutate(condition = interaction(companion_color,attractiveness,  sep = "_")) %>% 
  # getting relative visitation by dividing prediction by total visitation 
  mutate(prob= .epred / total_visitation)


#calculating mean observed relative visitation per condition
observed_relative_summary<-stingless_target_no_control %>% 
  dplyr::filter(!is.na(relative_target_visitation))%>% 
  group_by(condition, companion_color) %>%
  summarise(mean_obs=mean(relative_target_visitation)) %>% 
  ungroup() 

#calculating mean predicted rleative visitation per condition and adding observed to df
posterior_relative_summary<-posterior_relative %>% group_by(condition) %>% 
  summarise(mean_pred=mean(prob)) %>% ungroup()%>% 
  left_join(observed_relative_summary) %>% 
  # rounding to be able to put it nicely onto plot
  mutate(mean_pred=format(round(mean_pred,digits=3)),
         mean_obs = format(round(mean_obs,digits=3)),
         mean_label=paste(mean_obs, " (", mean_pred, ")", sep=""))

#adding values to big df for plotting
posterior_relative<-posterior_relative %>% 
  left_join(posterior_relative_summary)


#Relevelling the condition to make it consistent and have the right order int he plot
posterior_relative<-posterior_relative %>%
  mutate(condition=fct_relevel(condition,
                               "dark_blue_high",
                               "light_blue_high",
                               "red_high",
                               "yellow_high",
                               "NULL",
                               "dark_blue_low",
                               "light_blue_low",
                               "red_low",
                               "yellow_low"
                               ),
         x_value = case_when(condition == "dark_blue_high" ~ 1.5,
                             condition == "light_blue_high" ~ 3,
                             condition ==  "red_high" ~ 4.5,
                             condition == "yellow_high"~ 6,
                             condition == "dark_blue_low"~8.5,
                             condition == "light_blue_low"~10,
                             condition == "red_low"~11.5,
                             condition == "yellow_low"~13))

# also relevelling in the summary df
posterior_relative_summary<-posterior_relative_summary %>% 
  mutate(condition=fct_relevel(condition,
                               "dark_blue_high",
                               "light_blue_high",
                               "red_high",
                               "yellow_high",
                               "dark_blue_low",
                               "light_blue_low",
                               "red_low",
                               "yellow_low"
  ),
  x_value = case_when(condition == "dark_blue_high" ~ 1.5,
                      condition == "light_blue_high" ~ 3,
                      condition ==  "red_high" ~ 4.5,
                      condition == "yellow_high"~ 6,
                      condition == "dark_blue_low"~8.5,
                      condition == "light_blue_low"~10,
                      condition == "red_low"~11.5,
                      condition == "yellow_low"~13))


stingless_target_no_control<-stingless_target_no_control %>% 
  mutate(x_value = case_when(condition == "dark_blue_high" ~ 1.5,
                    condition == "light_blue_high" ~ 3,
                    condition ==  "red_high" ~ 4.5,
                    condition == "yellow_high"~ 6,
                    condition == "dark_blue_low"~8.5,
                    condition == "light_blue_low"~10,
                    condition == "red_low"~11.5,
                    condition == "yellow_low"~13))

#plot
(relative_distributions<- 
    ggplot() +
    # first layer: halfeye plot for observed relative visitation
    stat_halfeye( data=stingless_target_no_control, 
                  aes(x=x_value, y= relative_target_visitation, fill=companion_color), 
                  side="left",
                  alpha=0.25,
                  adjust=0.5,
                  .width = 0,
                  scale=0.6,
                  point_colour = NA)+
    # second layer: dots for observed relative visitation
    geom_dots( data=stingless_target_no_control, 
               aes(x=x_value, y= relative_target_visitation, color=companion_color, fill=companion_color), 
                              dotsize=5,
                              side="left",
                              alpha=0.5,
               binwidth=NA)+
    # third layer: halfeye plot for predicted relative visitation
    stat_halfeye(data=posterior_relative, aes(x = x_value, y = prob, fill = companion_color), 
                 adjust=0.5,
                .width=c(0.5,0.95),
                slab_color="black",
                slab_linewidth=0.1,
                scale=0.5           ) +
    scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1))+
    scale_color_manual(values=color_colors)+  # colors of plots and half eyes
    scale_fill_manual(values=color_colors)+ 
    new_scale_color()+ ##allows to have another color scheme 
    # adding labels into plot
    annotate("text", x=4, y=1.1, label="High Nectar Concentration", size=5)+ # adding annotation indicating low and high nectar concentration treatments
    annotate("text", x=11, y=1.1, label="Low Nectar Concentration", size=5)+
    annotate("text", x=7.25,y=-0.1, label="observed", size= 3)+
  annotate("text", x=7.25,y=-0.05, label="predicted", size= 3)+
  # adding vertical line to separate high and low nectar concentration
  geom_segment(aes(x=7.25, xend =7.25, y= 0, yend=1), linetype="dashed")+
  # applying theme elements
  make_it_pretty()+
  # adding mean values for observation
  geom_text(data=posterior_relative_summary, aes(x=x_value, y=-0.1, label=mean_obs))+
  # adding mean values for predicted values
   geom_text(
     data = posterior_relative_summary,
     aes(x = x_value, y = -0.05, label =  mean_pred))+
  # adjusting x axis with breaks and labels
    scale_x_continuous(breaks= c(1.5,3,4.5,6,8.5,10,11.5,13),
                     labels=c("dark\nblue", "light\nblue", "dark\norange", "yellow",
                              "dark\nblue", "light\nblue", "dark\norange", "yellow") # x axis ticks
                    )+
    # adding title and y axis 
    labs(y="Relative target visitation\n",title="A")+
    # final theme adjustments
    theme( plot.title = element_text(face = "bold",
                                              hjust = -0.05)))

 # saving plot
ggsave(relative_distributions, file="output/plots/relative_plot_test.png",
       height=7,
       width=14,
       bg="white")

# absolute ----------------------------------------------------------------

##absolute----

# data grid with dummy vairables
new_data_abs <- expand.grid(x = levels(as.factor(stingless_target$companion_color)), z = c("high","low","control")) 
new_data_abs<-new_data_abs%>%
  mutate(colony = "dummy_colony",
         id = "dummy_id")%>% 
  rename(companion_color =x,
         attractiveness=z)


# preparing dataframe: getting draws
posterior_absolute<- abs_target_visitation_categorical_interaction_color %>%
  epred_draws(newdata = new_data_abs, allow_new_levels=T, re_formula=NA) %>% 
  # specifying condition to be interaction of attractiveness and companion color
  mutate(condition = interaction(attractiveness, companion_color, sep = "_"),
         # relevelling
         condition=fct_relevel(condition,    
                               "high_dark_blue","high_light_blue",
                               "high_red","high_yellow","low_dark_blue","low_light_blue",
                               "low_red", "low_yellow","control_orange"),
         x_value = case_when(condition == "high_dark_blue" ~ 1.5,
                             condition == "high_light_blue" ~ 3,
                             condition ==  "high_red" ~ 4.5,
                             condition == "high_yellow"~ 6,
                             condition == "low_dark_blue"~9,
                             condition == "low_light_blue"~10.5,
                             condition == "low_red"~12,
                             condition == "low_yellow"~13.5,
                             condition == "control_orange" ~ 16.5))%>% ungroup() %>%
  # removing conditions that don´t exist in the experiment
  filter(!condition %in% c("control_dark_blue","control_light_blue","control_red",
                           "control_yellow",
                           "high_orange","low_orange")) %>% 
  droplevels() 


# same procedure to dataframe with raw data
stingless_target<-stingless_target %>% 
  mutate(condition=paste(attractiveness,companion_color,sep="_"),
         x_value = case_when(condition == "high_dark_blue" ~ 1.5,
                             condition == "high_light_blue" ~ 3,
                             condition ==  "high_red" ~ 4.5,
                             condition == "high_yellow"~ 6,
                             condition == "low_dark_blue"~9,
                             condition == "low_light_blue"~10.5,
                             condition == "low_red"~12,
                             condition == "low_yellow"~13.5,
                             condition == "control_orange" ~ 16.5))


#calculating mean observed relative visitation per condition
observed_absolute_summary<-stingless_target %>% dplyr::filter(!is.na(number_individuals))%>% 
  group_by(condition, companion_color) %>% 
  summarise(mean_obs=mean(number_individuals)) %>% 
  ungroup() 

#calculating mean predicted rleative visitation per condition and adding observed to df
posterior_absolute_summary<-posterior_absolute %>% group_by(condition) %>% 
  summarise(mean_pred=mean(.epred)) %>% 
  ungroup()%>% 
  left_join(observed_absolute_summary) %>% 
  # rounding to plot it nicely
  mutate(mean_pred=format(round(mean_pred,digits=2)),
         mean_obs = format(round(mean_obs,digits=2)),
         mean_label=paste(mean_obs, " (", mean_pred, ")", sep=""),
         x_value = case_when(condition == "high_dark_blue" ~ 1.5,
                             condition == "high_light_blue" ~ 3,
                             condition ==  "high_red" ~ 4.5,
                             condition == "high_yellow"~ 6,
                             condition == "low_dark_blue"~9,
                             condition == "low_light_blue"~10.5,
                             condition == "low_red"~12,
                             condition == "low_yellow"~13.5,
                             condition == "control_orange" ~ 16.5))


#adding values to big df for plotting
posterior_absolute<-posterior_absolute %>% left_join(posterior_absolute_summary)



#plot----

(absolute_distributions<- 
    ggplot() +
   # first layer: halfeye of observations
    stat_halfeye( data=stingless_target, 
                  aes(x=x_value, y= number_individuals, fill=companion_color),
                  side="left",
                  alpha=0.25,
                  adjust=0.5,
                  .width = 0,
                  scale=0.6,
                  point_colour = NA)+
   # second layer: dots observation
    geom_dots( data=stingless_target, aes(x=x_value, y= number_individuals, color=companion_color, fill=companion_color),
               dotsize=18,
               side="left",
               alpha=0.5,
               binwidth=.01   )+
   # third layer: halfeye of predictions
    stat_halfeye(data=posterior_absolute, aes(x = x_value, y = .epred , fill = companion_color),
                 adjust=0.5,
                 .width=c(0.5,0.95),
                 slab_color="black",
                 slab_linewidth=0.1,
                 scale=0.5) +
   # adjusting x axis breaks and labels
    scale_x_continuous(breaks = c(1.5,3,4.5,6,9,10.5,12,13.5,16.5),
      labels=c("dark\nblue", "light\nblue", "dark\norange", "yellow",
               "dark\nblue", "light\nblue", "dark\norange", "yellow","orange"))+
   # adjusting colors 
    scale_color_manual(values=color_colors)+ 
    scale_fill_manual(values=color_colors)+
    new_scale_color()+
   # ajdusting colors for text
   scale_color_manual(values=text_colors_1)+  
   scale_fill_manual(values=color_colors)+
   # adding text labels
   annotate("text", x=4, y=40, label="High Nectar Concentration", size=4)+
   annotate("text", x=11.25, y=40, label="Low Nectar Concentration",size=4)+
   annotate("text", x=16.75, y=40, label="Control",size=4)+
   annotate("text", x = 7.5,y=-6, label = "observed", size= 3)+
   annotate("text", x = 7.5, y=-2, label = "predicted", size= 3)+
   annotate("text", x = 15,y=-6, label = "observed", size= 3)+
   annotate("text", x = 15, y=-2, label = "predicted", size= 3)+
   # adding vertical lines to separate high and low and control from each other
   geom_segment(aes(x=7.5,xend=7.5, y = 0, yend = 40), linetype="dashed")+
   geom_segment(aes(x=15, xend = 15, y=0, yend=40), linetype="dashed")+
   # adjusting theme
   make_it_pretty()+
   # adding mean values for observation
    geom_text(data=posterior_absolute_summary, 
              aes(x=x_value, y=-6, label=mean_obs))+ 
   #adding mean values for prediction 
   geom_text(data = posterior_absolute_summary, 
             aes(x = x_value, y = -2, label =  mean_pred))+
   # adding y and title 
    labs(y="Absolute target visitation\n",  title="B")+
   # final theme adjustments of title
  theme( plot.title = element_text(face = "bold",
                                              hjust = -0.05)))

# saving plot
ggsave(absolute_distributions, file="output/plots/absolute_plot_test.png",
       height=7,
       width=14,
       bg="white")

# global ------------------------------------------------------------------

#create grid
new_data <- expand.grid(x = levels(as.factor(stingless_target$companion_color)),
                        z = c("high","low","control")) 
new_data<-new_data%>%
  mutate(colony = "dummy_colony",
         id = "dummy_id")%>% 
  rename(companion_color =x,
         attractiveness=z)


#get draws
posterior_global<- global_visitation_interaction_color %>%
  epred_draws(newdata = new_data, allow_new_levels=T, re_formula=NA) %>% 
  mutate(condition = interaction(attractiveness, companion_color, sep = "_")) %>% ungroup() %>% 
  filter(!condition %in% c("control_dark_blue","control_light_blue","control_red","control_yellow",
                           "high_orange","low_orange")) %>% 
  droplevels()


# relevelling factors for plotting
posterior_global<-posterior_global %>% 
  mutate(condition=fct_relevel(condition,
                               "high_dark_blue",
                               "high_light_blue",
                               "high_red",
                               "high_yellow",
                               "low_dark_blue",
                               "low_light_blue",
                               "low_red",
                               "low_yellow",
                               "control_orange"),
         x_value = case_when(condition == "high_dark_blue" ~ 1.5,
                             condition == "high_light_blue" ~ 3,
                             condition ==  "high_red" ~ 4.5,
                             condition == "high_yellow"~ 6,
                             condition == "low_dark_blue"~9,
                             condition == "low_light_blue"~10.5,
                             condition == "low_red"~12,
                             condition == "low_yellow"~13.5,
                             condition == "control_orange" ~ 16.5))

# getting df with mean values per condition (observations)
observed_global_summary<-stingless_target %>% 
  dplyr::filter(!is.na(total_visitation))%>% 
  group_by(condition, companion_color) %>% 
  summarise(mean_obs=mean(total_visitation)) %>% 
  ungroup() 

#calculating mean predicted rleative visitation per condition and adding observed to df
posterior_global_summary<-posterior_global %>% group_by(condition) %>% summarise(mean_pred=mean(.epred)) %>% ungroup()%>% 
  left_join(observed_global_summary) %>% 
  mutate(mean_pred=format(round(mean_pred,digits=2)),
         mean_obs = format(round(mean_obs,digits=2)),
         mean_label=paste(mean_obs, " (", mean_pred, ")", sep=""),
         x_value = case_when(condition == "high_dark_blue" ~ 1.5,
                             condition == "high_light_blue" ~ 3,
                             condition ==  "high_red" ~ 4.5,
                             condition == "high_yellow"~ 6,
                             condition == "low_dark_blue"~9,
                             condition == "low_light_blue"~10.5,
                             condition == "low_red"~12,
                             condition == "low_yellow"~13.5,
                             condition == "control_orange" ~ 16.5))

#plot
(global_distributions<- 
    ggplot() +
    # first layer: halfeye plot of observations
    stat_halfeye( data=stingless_target, 
                  aes(x=x_value, y= total_visitation, fill=companion_color),
                  side="left",
                  alpha=0.25,
                  adjust=0.5,
                  .width = 0,
                  scale=0.55,
                  point_colour = NA)+
    # second layer: points of observations
    geom_dots( data=stingless_target, aes(x=x_value, y= total_visitation, color=companion_color, fill=companion_color),
               dotsize=10,
               side="left",
               alpha=0.5,
               binwidth=.15)+
    # third layer: halfeye plot of predictions
    stat_halfeye(data=posterior_global, aes(x = x_value, y = .epred , fill = companion_color),
                 adjust=0.5,
                 .width=c(0.5,0.95),
                 slab_color="black",
                 slab_linewidth=0.1,
                 scale=0.5) +
    # adjusting x axis breaks and labels
    scale_x_continuous(breaks = c(1.5,3,4.5,6,9,10.5,12,13.5,16.5),
                       labels=c("dark\nblue", "light\nblue", "dark\norange", "yellow","dark\nblue", "light\nblue", "dark\norange", "yellow","orange"))+
    # adjusting colors
    scale_color_manual(values=color_colors)+ 
    scale_fill_manual(values=color_colors)+
    new_scale_color()+
    # adjusting colors of labels
    scale_color_manual(values=text_colors_1)+  
    scale_fill_manual(values=color_colors)+
    # adding labels 
    annotate("text", x=4, y=160, label="High Nectar Concentration", size=4)+
    annotate("text", x=11.25, y=160, label="Low Nectar Concentration",size=4)+
    annotate("text", x=16.75, y=160, label="Control",size=4)+
    annotate("text", x = 7.5,y=-22, label = "observed", size= 3)+
    annotate("text", x = 7.5, y=-8, label = "predicted", size= 3)+
    annotate("text", x = 15,y=-22, label = "observed", size= 3)+
    annotate("text", x = 15, y=-8, label = "predicted", size= 3)+
    # adding dashed vertical lines to separate high, lwo and control
    geom_segment(aes(x=7.5,xend=7.5, y = 0, yend = 180), linetype="dashed")+
    geom_segment(aes(x=15, xend = 15, y=0, yend=180), linetype="dashed")+
    # changing plot appearance
    make_it_pretty()+
    # adding mean values for observations and predictions
    geom_text(data=posterior_global_summary, aes(x=x_value, y=-22, label=mean_obs))+ 
    geom_text(  data = posterior_global_summary,
                aes(x = x_value, y = -8, label =  mean_pred))+
    # adding labels
    labs(y="Total patch visitation\n",
         title="C")+
    # adjusting title appearance
    theme( plot.title = element_text(face = "bold",
                                                             hjust = -0.05)))



# putting everything into panel
panel<-relative_distributions|(absolute_distributions/global_distributions)

# saving panel
ggsave(panel,file="output/plots/panel_messy_1.png", height=6.3, width=16.3, bg="white")

