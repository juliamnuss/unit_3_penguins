# 02.19.2026
# JMN
# Class Script
# Units 3.3-3.4

# Unit 3.3

library(palmerpenguins)
library(tidyverse)
# install.packages("rstatix")
library(rstatix) # for levene_tests() and t_test()
library(knitr) # prints pretty tables with RMarkdown

# run a t-test on gentoo body mass
# are the gentoo body mases measured in the dataset similar or different than the body mass in the literature
# lit average = 5500 g

gentoo = penguins %>%
  filter(species=="Gentoo") # %>% droplevels() = will drop all the old empty labels
summary(gentoo)
mean(gentoo$body_mass_g, na.rm=TRUE)
sd(gentoo$body_mass_g, na.rm=TRUE)

ggplot(data = gentoo) + 
  geom_histogram(aes(x=body_mass_g, fill = sex), alpha = 0.5, position = "stack") +
  theme_bw() +
  labs( y = "Count", X = "Body Mass (g)", title = "Gentoo")

#QQ plot
ggplot(gentoo) +
  stat_qq(aes(sample = body_mass_g))

# one sample T-test
gentoo_body_mass_g_symonds = 5500 # from symonds and tattersall, accesed via EOL
my_t_test = t.test(gentoo$body_mass_g, mu = gentoo_body_mass_g_symonds) # Base R
my_t_test$p.value # will print your p value
# rstatix version of ^this - pipe friendly
t_test_results = gentoo %>% t_test(body_mass_g ~ 1, mu = gentoo_body_mass_g_symonds)
kable(t_test_results) # prints results with RMarkdown

# Gentoo versus Adelie body mass
# Independent sample T-test
g_vs_a_data = penguins %>%
  dplyr::filter(species %in% c("Gentoo", "Adelie"), 
          !is.na(body_mass_g)) %>%
  select(species, body_mass_g) %>%
  droplevels()
summary(g_vs_a_data)

g_vs_a_data %>% 
  group_by(species) %>%
  summarize(mean=mean(body_mass_g), sd = sd(body_mass_g))

ggplot(data = g_vs_a_data, aes(x = body_mass_g)) + 
  geom_histogram() +
  facet_wrap(~species)

ggplot(g_vs_a_data) + 
  stat_qq(aes(sample=body_mass_g)) +
  facet_wrap(~species, scales = "free")

# check for equality of variance
# if true, use student's t-test 
# if false, use welch's
g_vs_a_data %>% levene_test(body_mass_g ~ species) # if p<0.05, variance are NOT equal use welch's

# Base R version:
t.test(g_vs_a_data$body_mass_g ~ g_vs_a_data$species, var.equal = T)

# dplyr-friendly version
g_vs_a_data %>%
  t_test(body_mass_g ~ species, var.equal = T)

########################################################################

# Unit 3.4

ggplot(gentoo) +
  geom_point(aes(x = bill_length_mm, y= bill_depth_mm))

#check normality assumption with qqplot
ggplot(gentoo) + 
  stat_qq(aes(sample = bill_length_mm))
ggplot(gentoo) +
  stat_qq(aes(sample = bill_depth_mm))

#cor() returns just the correlation coefficient r
cor(x = gentoo$bill_length_mm, y = gentoo$bill_depth_mm, use = "complete.obs")

# cor.test() returns the t-statistic, df, p-value, etc.
cor.test(x=gentoo$bill_length_mm, y = gentoo$bill_depth_mm, use = "comlete.obs")

# cor_test is the pipe friendly version from rstatix
gentoo %>%
  cor_test(bill_length_mm, bill_depth_mm) # use param handles NAs
# correlation tests : "peason", "kendall", "spearman"

# Exercise 4.1
ggplot(penguins) +
  geom_point(aes(x = bill_length_mm, y= bill_depth_mm))
ggplot(penguins) + 
  stat_qq(aes(sample = bill_length_mm))
ggplot(penguins) +
  stat_qq(aes(sample = bill_depth_mm))
penguins %>%
  cor_test(bill_length_mm, bill_depth_mm)

# correlation matrix
head(gentoo)
cor(gentoo[,3:6], use = "complete.obs")
gentoo = gentoo %>%
  filter(!is.na(body_mass_g))
cor(gentoo[, seq(3,6)] )

# install.packages("GGally")
library(GGally) #ggpairs()

gentoo %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  ggpairs()

penguins %>%
  select(species, body_mass_g, ends_with("_mm")) %>%
  GGally::ggpairs(aes(color = species))
      # could have also done (species, where(is.numeric))
