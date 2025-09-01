# Color characteristics overview plot

# loading packages --------------------------------------------------------

library(MCMCvis)
library(ggplot2)
library(ggpubr)
library(ggeffects)
library(tidybayes)
library(tidyverse)
library(ggridges)
library(brms)
library(bayesplot)
library(bayestestR)
library(modelr)
library(ggpubr)
library(bayesplot) #for get_variables
library(gridExtra) #for grid.arrange
library(survminer) #for aligning plots in panel
library(patchwork)
library(grid)
# data --------------------------------------------------------------------
lapply(list.files("output/model_development", pattern = "\\.RData$", full.names = TRUE), load, envir = .GlobalEnv)

stingless_target<-read.csv("data/stingless_bees_tidy1.csv") %>% 
  mutate(condition=paste(companion_color, attractiveness, sep="_"))

stingless_target_no_control<-filter(stingless_target, treatment!="5")


# individual plots----
##brightness----
brightness_abs_dist <- brighntess_abs_model%>%
  spread_draws(  `b_attractivenesslow`,`b_attractivenesshigh`,`b_lum`,
                 `b_attractivenesshigh:lum`, `b_attractivenesslow:lum`
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Parameter",
    values_to = "Estimate"
  ) %>%
  mutate(Parameter = fct_relevel(Parameter,
                                 "b_lum",
                                 "b_attractivenesslow:lum",
                                 "b_attractivenesshigh:lum",
                                 "b_attractivenesslow",
                                 "b_attractivenesshigh")) %>% 
  filter(Parameter!=".draw" , Parameter!=".iteration", Parameter!=".chain") %>%
  ggplot(aes(x = Estimate, y = Parameter)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_pointinterval(alpha = 0.6, ...width = c(0.5, 0.95)) +
  theme_bw()+theme(panel.border=element_rect(),panel.grid=element_blank()) +
  labs(
    
    x = "Estimate",
    y = "Parameter"
  )+
  scale_y_discrete(labels=  c(
    "b_lum"      = "control: brightness",
    "b_attractivenesslow:lum"   = "low concentration:\nbrightness",
    "b_attractivenesslow"       = "low concentration",
    "b_attractivenesshigh:lum"    = "high concentration:\nbrightness",
    "b_attractivenesshigh" = "high concentration"
  )  )+

  theme(axis.text =  element_text(size=12),
        axis.title = element_text(size=15),
        title=element_text(size=15))+
  xlim(-6,6)



#relative
brightness_relative_dist <- brightness_relative_model%>%
  spread_draws(  `b_attractivenesslow`,`b_lum`,`b_attractivenesslow:lum`  ) %>%
  pivot_longer(    cols = everything(),
                   names_to = "Parameter",
                   values_to = "Estimate"  ) %>%
  mutate(Parameter = fct_relevel(Parameter,
                                 "b_lum",
                                 "b_attractivenesslow:lum",
                                 "b_attractivenesslow")) %>% 
  filter(Parameter!=".draw" , Parameter!=".iteration", Parameter!=".chain") %>%
  ggplot(aes(x = Estimate, y = Parameter)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_pointinterval(alpha = 0.6,.width = c(0.5, 0.95)) +
  theme_bw()+theme(panel.border=element_rect(),panel.grid=element_blank()) +
  labs(  x = "Estimate",
         y = "Parameter" )+
  scale_y_discrete(labels=  c(
    "b_lum"      = "high concentration:\nbrightness",
    "b_attractivenesslow:lum"   = "low concentration:\nbrightness",
    "b_attractivenesslow"       = "low concentration"  )  )+

  theme(axis.text =  element_text(size=12),
        axis.title = element_text(size=15),
        title=element_text(size=15))+
  xlim(-2,2)


##global 
brightness_global_dist <- brightness_global_model %>% 
  spread_draws(  `b_attractivenesslow`,`b_attractivenesshigh`,`b_lum`,
                 `b_attractivenesshigh:lum`, `b_attractivenesslow:lum`
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Parameter",
    values_to = "Estimate"
  ) %>%
  mutate(Parameter = fct_relevel(Parameter,
                                 "b_lum",
                                 "b_attractivenesslow:lum",
                                 "b_attractivenesshigh:lum",
                                 "b_attractivenesslow",
                                 "b_attractivenesshigh")) %>% 
  filter(Parameter!=".draw" , Parameter!=".iteration", Parameter!=".chain") %>%
  ggplot(aes(x = Estimate, y = Parameter)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_pointinterval(alpha = 0.6,.width = c(0.5, 0.95)) +
  theme_bw()+theme(panel.border=element_rect(),panel.grid=element_blank()) +
  labs(
    
    x = "Estimate",
    y = "Parameter"
  )+
  scale_y_discrete(labels=  c(
    "b_lum"      = "control: brightness",
    "b_attractivenesslow:lum"   = "low concentration:\nbrightness",
    "b_attractivenesslow"       = "low concentration",
    "b_attractivenesshigh:lum"    = "high concentration:\nbrightness",
    "b_attractivenesshigh" = "high concentration"
  )  )+

  theme(axis.text =  element_text(size=12),
        axis.title = element_text(size=15),
        title=element_text(size=15))+
  xlim(-6,6)

##hue----

#absolute
hue_abs_dist <- hue_abs_model%>%
  spread_draws(  `b_attractivenesslow`,`b_attractivenesshigh`,`b_h.theta`,
                 `b_attractivenesshigh:h.theta`, `b_attractivenesslow:h.theta`
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Parameter",
    values_to = "Estimate"
  ) %>%
  mutate(Parameter = fct_relevel(Parameter,
                                 "b_h.theta",
                                 "b_attractivenesslow:h.theta",
                                 "b_attractivenesshigh:h.theta",
                                 "b_attractivenesslow",
                                 "b_attractivenesshigh")) %>% 
  filter(Parameter!=".draw" , Parameter!=".iteration", Parameter!=".chain") %>%
  ggplot(aes(x = Estimate, y = Parameter)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_pointinterval(alpha = 0.6,.width = c(0.5, 0.95)) +
  theme_bw()+theme(panel.border=element_rect(),panel.grid=element_blank()) +
  labs(
    
    x = "Estimate",
    y = "Parameter"
  )+
  scale_y_discrete(labels=  c(
    "b_h.theta"      = "control: hue",
    "b_attractivenesslow:h.theta"   = "low concentration:\nhue",
    "b_attractivenesslow"       = "low concentration",
    "b_attractivenesshigh:h.theta"    = "high concentration:\nhue",
    "b_attractivenesshigh" = "high concentration"
  )  )+

  theme(axis.text =  element_text(size=12),
        axis.title = element_text(size=15),
        title=element_text(size=15))+
  xlim(-6,6)


#relative

hue_relative_dist <- hue_relative_model%>%
  spread_draws(  `b_attractivenesslow`,`b_h.theta`,
                 `b_attractivenesslow:h.theta`
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Parameter",
    values_to = "Estimate"
  ) %>%
  mutate(Parameter = fct_relevel(Parameter,
                                 "b_h.theta",
                                 "b_attractivenesslow:h.theta",
                                 "b_attractivenesslow")) %>% 
  filter(Parameter!=".draw" , Parameter!=".iteration", Parameter!=".chain") %>%
  ggplot(aes(x = Estimate, y = Parameter)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_pointinterval(alpha = 0.6,.width = c(0.5, 0.95)) +
  theme_bw()+theme(panel.border=element_rect(),panel.grid=element_blank()) +
  labs(
    
    x = "Estimate",
    y = "Parameter"
  )+
  scale_y_discrete(labels=  c(
    "b_h.theta"      = "high concentration:\nhue",
    "b_attractivenesslow:h.theta"   = "low concentration:\nhue",
    "b_attractivenesslow"       = "low concentration"
  )  )+
  

  theme(axis.text =  element_text(size=12),
        axis.title = element_text(size=15),
        title=element_text(size=15))+
  xlim(-2,2)

#global

hue_global_dist <- hue_global_model%>%
  spread_draws(  `b_attractivenesslow`,`b_attractivenesshigh`,`b_h.theta`,
                 `b_attractivenesshigh:h.theta`, `b_attractivenesslow:h.theta`
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Parameter",
    values_to = "Estimate"
  ) %>%
  mutate(Parameter = fct_relevel(Parameter,
                                 "b_h.theta",
                                 "b_attractivenesslow:h.theta",
                                 "b_attractivenesshigh:h.theta",
                                 "b_attractivenesslow",
                                 "b_attractivenesshigh")) %>% 
  filter(Parameter!=".draw" , Parameter!=".iteration", Parameter!=".chain") %>%
  ggplot(aes(x = Estimate, y = Parameter)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_pointinterval(alpha = 0.6,.width = c(0.5, 0.95)) +
  theme_bw()+theme(panel.border=element_rect(),panel.grid=element_blank()) +
  labs(
    
    x = "Estimate",
    y = "Parameter"
  )+
  scale_y_discrete(labels=  c(
    "b_h.theta"      = "control: hue",
    "b_attractivenesslow:h.theta"   = "low concentration:\nhue",
    "b_attractivenesslow"       = "low concentration",
    "b_attractivenesshigh:h.theta"    = "high concentration:\nhue",
    "b_attractivenesshigh" = "high concentration"
  )  )+

  theme(axis.text =  element_text(size=12),
        axis.title = element_text(size=15),
        title=element_text(size=15))+
  xlim(-6,6)

##saturation----

saturation_abs_dist <- saturation_abs_model%>%
  spread_draws(  `b_attractivenesslow`,`b_attractivenesshigh`,`b_r.vec`,
                 `b_attractivenesshigh:r.vec`, `b_attractivenesslow:r.vec`
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Parameter",
    values_to = "Estimate"
  ) %>%
  mutate(Parameter = fct_relevel(Parameter,
                                 "b_r.vec",
                                 "b_attractivenesslow:r.vec",
                                 "b_attractivenesshigh:r.vec",
                                 "b_attractivenesslow",
                                 "b_attractivenesshigh")) %>% 
  filter(Parameter!=".draw" , Parameter!=".iteration", Parameter!=".chain") %>%
  ggplot(aes(x = Estimate, y = Parameter)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_pointinterval(alpha = 0.6) +
  theme_bw()+theme(panel.border=element_rect(),panel.grid=element_blank()) +
  labs(
    
    x = "Estimate",
    y = "Parameter"
  )+
  scale_y_discrete(labels=  c(
    "b_r.vec"      = "control: saturation",
    "b_attractivenesslow:r.vec"   = "low concentration:\nsaturation",
    "b_attractivenesslow"       = "low concentration",
    "b_attractivenesshigh:r.vec"    = "high concentration:\nsaturation",
    "b_attractivenesshigh" = "high concentration"
  )  )+
  

  theme(axis.text =  element_text(size=12),
        axis.title = element_text(size=15),
        title=element_text(size=15))+
  xlim(-6,6)

#relative
saturation_relative_dist <- saturation_relative_model%>%
  spread_draws(  `b_attractivenesslow`,`b_r.vec`
                 , `b_attractivenesslow:r.vec`
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Parameter",
    values_to = "Estimate"
  ) %>%
  mutate(Parameter = fct_relevel(Parameter,
                                 "b_r.vec",
                                 "b_attractivenesslow:r.vec",
                                 "b_attractivenesslow")) %>% 
  filter(Parameter!=".draw" , Parameter!=".iteration", Parameter!=".chain") %>%
  ggplot(aes(x = Estimate, y = Parameter)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_pointinterval(alpha = 0.6,.width = c(0.5, 0.95)) +
  theme_bw()+theme(panel.border=element_rect(),panel.grid=element_blank()) +
  labs(
    
    x = "Estimate",
    y = "Parameter"
  )+
  scale_y_discrete(labels=  c(
    "b_r.vec"      = "high concentration:\nsaturation",
    "b_attractivenesslow:r.vec"   = "low concentration:\nsaturation",
    "b_attractivenesslow"       = "low concentration"
  )  )+
  

  theme(axis.text =  element_text(size=12),
        axis.title = element_text(size=15),
        title=element_text(size=15))+
  xlim(-2,2)

#global
saturation_global_dist <- saturation_global_model%>%
  spread_draws(  `b_attractivenesslow`,`b_attractivenesshigh`,`b_r.vec`,
                 `b_attractivenesshigh:r.vec`, `b_attractivenesslow:r.vec`
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Parameter",
    values_to = "Estimate"
  ) %>%
  mutate(Parameter = fct_relevel(Parameter,
                                 "b_r.vec",
                                 "b_attractivenesslow:r.vec",
                                 "b_attractivenesshigh:r.vec",
                                 "b_attractivenesslow",
                                 "b_attractivenesshigh")) %>% 
  filter(Parameter!=".draw" , Parameter!=".iteration", Parameter!=".chain") %>%
  ggplot(aes(x = Estimate, y = Parameter,label="a")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_pointinterval(alpha = 0.6, ..width = c(0.5, 0.95)) +
  theme_bw()+theme(panel.border=element_rect(),panel.grid=element_blank()) +
  theme(panel.grid.major = element_blank(),
        panel.border = element_rect(fill=NULL))+
  labs(
    x = "Estimate",
    y = "Parameter"
  )+
  scale_y_discrete(labels=  c(
    "b_r.vec"      = "control: saturation",
    "b_attractivenesslow:r.vec"   = "low concentration:\nsaturation",
    
    "b_attractivenesshigh:r.vec"    = "high concentration:\nsaturation",
    "b_attractivenesslow"       = "low concentration",
    "b_attractivenesshigh" = "high concentration"
  )  )+
  

  theme(axis.text =  element_text(size=12),
        axis.title = element_text(size=15),
        title=element_text(size=15))+
  xlim(-6,6)



# panel ----
#removing axis titles for panel
brightness_abs_dist<-brightness_abs_dist + labs(x="",y="")
brightness_abs_dist<-annotate_figure(brightness_abs_dist, fig.lab = "   a", fig.lab.pos="top.left", fig.lab.size=15)
brightness_global_dist<-brightness_global_dist + labs(x="",y="")
brightness_global_dist<-annotate_figure(brightness_global_dist, fig.lab = "   b", fig.lab.pos="top.left", fig.lab.size=15)
brightness_relative_dist<-brightness_relative_dist + labs(x="",y="")
brightness_relative_dist<-annotate_figure(brightness_relative_dist, fig.lab = "   c", fig.lab.pos="top.left", fig.lab.size=15)
saturation_abs_dist<-saturation_abs_dist + labs(x="",y="")
saturation_abs_dist<-annotate_figure(saturation_abs_dist, fig.lab = "   d", fig.lab.pos="top.left", fig.lab.size=15)
saturation_global_dist<-saturation_global_dist + labs(x="",y="")
saturation_global_dist<-annotate_figure(saturation_global_dist, fig.lab = "   e", fig.lab.pos="top.left", fig.lab.size=15)
saturation_relative_dist<-saturation_relative_dist + labs(x="",y="")
saturation_relative_dist<-annotate_figure(saturation_relative_dist, fig.lab = "   f", fig.lab.pos="top.left", fig.lab.size=15)
hue_abs_dist<-hue_abs_dist + labs(x="",y="")
hue_abs_dist<-annotate_figure(hue_abs_dist, fig.lab = "   g", fig.lab.pos = "top.left",fig.lab.size = 15)
hue_global_dist<-hue_global_dist + labs(x="",y="")
hue_global_dist<-annotate_figure(hue_global_dist, fig.lab = "   h", fig.lab.pos="top.left", fig.lab.size=15)
hue_relative_dist<-hue_relative_dist + labs(x="",y="")
hue_relative_dist<-annotate_figure(hue_relative_dist, fig.lab = "   i", fig.lab.pos="top.left", fig.lab.size=15)


#trying around

col_A <- brightness_abs_dist / brightness_global_dist / brightness_relative_dist+ plot_annotation(title = "Brightness")
col_B <-saturation_abs_dist / saturation_global_dist / saturation_relative_dist+ plot_annotation(title = "saturation")
col_C<-hue_abs_dist / hue_global_dist / hue_relative_dist+ plot_annotation(title = "hue")

# Column titles (top row)

col_titles <- c("Absolute target visitation", "Total patch visitation", "Relative target visitation")
col_title_row <- wrap_elements(grid::textGrob(" ", gp = gpar(fontsize = 12)))  # Leer für linke obere Ecke
for (title in col_titles) {
  col_title_row <- col_title_row | wrap_elements(grid::textGrob(title, gp = gpar(fontface = "bold", fontsize = 12),x=0.5, just="center"))
}

# Kombiniere alle Teile
row_title_brightness <- wrap_elements(grid::textGrob("Brightness", rot = 90, gp = gpar(fontface = "bold", fontsize = 12),x = unit(0.9, "npc"),  # weiter rechts in der Box
                                                     just = "center"         # Text rechtsbündig
))
row_brightness <- row_title_brightness | brightness_abs_dist | brightness_global_dist | brightness_relative_dist

row_title_saturation <- wrap_elements(grid::textGrob("Saturation", rot = 90, gp = gpar(fontface = "bold", fontsize = 12),x = unit(0.9, "npc"),  # weiter rechts in der Box
                                                     just = "center"         # Text rechtsbündig
))
row_saturation <- row_title_saturation | saturation_abs_dist | saturation_global_dist | saturation_relative_dist

row_title_hue <- wrap_elements(grid::textGrob("Hue", rot = 90, gp = gpar(fontface = "bold", fontsize = 12),
                                              x = unit(0.9, "npc"),  # weiter rechts in der Box
                                              just = "center"         # Text rechtsbündig
))
row_hue <- row_title_hue | hue_abs_dist | hue_global_dist | hue_relative_dist


final_plot <- col_title_row / row_brightness / row_saturation / row_hue +
  plot_layout(heights = c(0.08, 1, 1, 1), widths = c(0.03, 1, 1, 1))

#saving plot
ggsave(final_plot, file="output/plots/color_characteristics_estimates_annotated_1.png",
       height=8,
       width=15,
       bg="white")
