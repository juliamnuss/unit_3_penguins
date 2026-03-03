# 03.03.2024
# JMN
# Class Script
# Unit 3.5

# Linear Models
# basis of modeling
# predicting y as a function of x
library(tidyverse)
library(palmerpenguins)
library(GGally)

head(penguins)
summary(penguins)
ggpairs(data = penguins %>% select(where(is.numeric)))

# build a bad model (y as a function of x)
lm_1 = lm(bill_depth_mm ~ bill_length_mm, data = penguins)
summary(lm_1)

str(lm_1) # ugly output but it shows you more info
class(lm_1)

ggplot(data = penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  geom_smooth(methods="lm")

plot(lm_1)

# better one species model
gentoo = penguins %>% filter(species == "Gentoo")
gentoo %>%
  select(bill_depth_mm, bill_length_mm) %>%
  GGally::ggpairs()
lm_2 = lm(bill_depth_mm ~ bill_length_mm, data = gentoo)
summary(lm_2)

ggplot(data = gentoo, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() + 
  geom_smooth(method = "lm")
plot(lm_2)


ggplot(data = penguins) +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_smooth(aes(x = bill_length_mm, y = bill_depth_mm, color = species),method = "lm") +
  geom_smooth(aes(x = bill_length_mm, y = bill_depth_mm), method = "lm", color= "black") + 
  labs(x = "Bill Length (mm)", y = "Bill Depth (mm)", title = "pangwangs bill depth prediction") +
  theme_bw()
ggsave(filename="figures/bill_length_v_bill_depth_species.png", height=5, width=7, units="in", dpi=300)

# Exercise 5.1
lm_3 = lm(data = penguins, flipper_length_mm ~ bill_depth_mm)
summary(lm_3)
plot(lm_3)
lm_4 = lm(data = gentoo, flipper_length_mm ~ bill_depth_mm)
summary(lm_4)
plot(lm_4)

ggplot(data = penguins) +
  geom_point(aes(x = flipper_length_mm, y = bill_depth_mm, color = species)) +
  geom_smooth(aes(x = flipper_length_mm, y = bill_depth_mm, color = species),method = "lm") +
  geom_smooth(aes(x = flipper_length_mm, y = bill_depth_mm), method = "lm", color= "black") + 
  labs(x = "Flipper Length (mm)", y = "Bill Depth (mm)", title = "Penguin Bill Depth Prediction") +
  theme_bw()
