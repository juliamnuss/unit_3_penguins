# 02.17.2026
# JMN
# Class Script
# Unit 3.2

# ggplot2
# library(ggplot2) # technically dont need to call ggplot if your calling in tidyverse bc its included
library(palmerpenguins)
library(tidyverse)

ggplot(data = penguins) +
  geom_point(aes(x = flipper_length_mm, y = body_mass_g, color = species, shape = sex)) + 
  geom_smooth(aes(x = flipper_length_mm, y = body_mass_g), method = "lm") + # used loess formula to smooth data
  theme_bw() + # *_bw() *_classic() 
  xlab("Flipper Length (mm)") +
  ylab("Body Mass (g)") + 
  ggtitle("Penguins are Cool!")
# aes = aesthetics and is a function used to map data to something visual on the plot
# outside colors do not go into the aes function unless you are using it to make a key
# ggplot has a lot of built in aesthetics like the background and theme, but it can be changed
# 1m = linear model and will fit data to that model

penguins_ts = penguins %>%
  group_by(species, year) %>%
  summarize(n_penguins = n())

ggplot(data = penguins_ts) +
  geom_line(aes(x = year, y = n_penguins, color = species)) +
  theme_classic() + 
  labs( x = "Year", y = "Number of Penguins", title = "Pengwings" )

ggplot(data = penguins) + 
  geom_histogram(aes(x = flipper_length_mm, fill = species), 
                      position = "identity", # identity vs stack = how the bars are arranged
                      alpha = 0.65, # alpha = transparancy
                      binwidth = 3) +
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4")) +
  theme_classic() +
  labs( y = "Flipper Length (mm)", x = "", title = "Pangwang")

ggplot(penguins) +
  geom_boxplot(aes(y = flipper_length_mm, x = species)) + 
  geom_jitter(aes(y = flipper_length_mm, x = species, color = species, shape = sex), width = 0.2) +
  labs( y = "Flipper Length (mm)", x = "", title = "Panwan") + 
  theme_minimal()

ggplot(penguins %>% filter(!is.na(sex))) +
  geom_bar(aes( x = sex, fill = species)) + 
  facet_wrap(~species, nrow = 1) +
  # coord_flip() +
  theme_bw() +
  labs( x = "", y = "Count", title = "Pinwin")
ggsave(filename = "my_sex_plot.png", 
      device = "png", 
      width = 7, 
      height = 5, 
      units = "in", 
      dpi = 600)

colors() # gives you a list of all the colors you can use

# exercise 2.2
ggplot(penguins) +
  geom_point(aes(x = bill_depth_mm, y = bill_length_mm, color = sex)) +
  facet_wrap(~species, ncol = 1) + 
  theme_bw() + 
  labs(x = "Bill Depth (mm)", y = "Bill Length (mm)", title = "Pingagne") + 
  scale_color_manual(values = c("violetred3", "yellow3"))
