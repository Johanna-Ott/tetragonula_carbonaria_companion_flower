# Plotting prior distributions
#JO


library(tidyverse)

# absolute ----------------------------------------------------------------

# Define scales
x_student <- seq(-10, 30, length.out = 1000)
x_normal <- seq(-20, 20, length.out = 1000)
x_gamma <- seq(0, 10, length.out = 1000)
x_exp <- seq(0, 10, length.out = 1000)

# binding together into dataframe
absolute_priors_df <- bind_rows(
  data.frame(    x =x_student,
                 density = dt((x_student - 8)/5, df = 3) / 5,
                 prior = "Intercept"),
  data.frame( x = x_normal,
              density = dnorm(x_normal, mean = 0, sd = 5),
              prior = "Fixed effects"),
  data.frame(   x = x_gamma,
                density = dgamma(x_gamma, shape = 1, rate = 1),
                prior = "shape"),
  data.frame( x = x_exp,
              density = dexp(x_exp, rate = 1),
              prior = "sd "))

#plotting both adapted and baseline distributions into one plot
absolute_prior_plot<-  ggplot(absolute_priors_df, aes(x = x, y = density)) +
  geom_line(size = 1) +
  #one graph per prior
  facet_wrap(~ prior, scales = "free", ncol = 2) +
  #title and axis titles
  labs(title = "Absolute target visitation prior distribution",
       x = "Value",
       y = "Density")+
  theme_minimal(base_size = 13)

# saving plot
ggsave(absolute_prior_plot,
       file="output/plots/model_fit/absolute_priors.png",
       height=8,
       width=8,
       bg="white")


# global ------------------------------------------------------------------

# binding together into dataframe for global priors
global_priors_df <- bind_rows(
  data.frame(    x =x_student,
                 density = dt((x_student - 20)/3, df = 10) / 3,
                 prior = "Intercept"),
  data.frame( x = x_normal,
              density = dnorm(x_normal, mean = 0, sd = 5),
              prior = "Fixed effects"),
  data.frame(   x = x_gamma,
                density = dgamma(x_gamma, shape = 1, rate = 1),
                prior = "shape"),
  data.frame( x = x_exp,
              density = dexp(x_exp, rate = 1),
              prior = "sd "))


#plotting
global_prior_plot<-  ggplot(global_priors_df, aes(x = x, y = density)) +
    geom_line(size = 1) +
    #one graph per prior
    facet_wrap(~ prior, scales = "free", ncol = 2) +
    #title and axis titles
    labs(title = "Total patch visitation prior distribution",
         x = "Value",
         y = "Density")+
    theme_minimal(base_size = 13)

#saving plot
ggsave(global_prior_plot,
       file="output/plots/model_fit/global_priors.png",
       height=8,
       width=8,
       bg="white")


# Relative priors ---------------------------------------------------------

relative_priors_df <- bind_rows(
  data.frame(    x =x_student,
                 density = dt((x_student - 0.3)/3, df = 5) / 3,
                 prior = "Intercept"),
  data.frame( x = x_normal,
              density = dnorm(x_normal, mean = 0, sd = 5),
              prior = "Fixed effects"),
  data.frame( x = x_exp,
              density = dexp(x_exp, rate = 1),
              prior = "sd "))

#plotting both adapted and baseline distributions into one plot
relative_prior_plot<-  ggplot(relative_priors_df, aes(x = x, y = density)) +
  geom_line(size = 1) +
  #one graph per prior
  facet_wrap(~ prior, scales = "free", ncol = 2) +
  #title and axis titles
  labs(title = "Relative prior distribution",
       x = "Value",
       y = "Density")+
  theme_minimal(base_size = 13)

#saving plot
ggsave(relative_prior_plot,
       file="output/plots/model_fit/relative_priors.png",
       height=8,
       width=8,
       bg="white")
