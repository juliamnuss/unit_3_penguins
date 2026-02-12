# 02.12.2026
# JMN
# Class Script
# Unit 3.1

# install.packages("tidyverse")
# tidyverse_packages() # tells you all of the packages in tidyverse
# install.packages("palmerpenguins")

library("tidyverse")
library("palmerpenguins")

head(penguins)
summary(penguins)
dim(penguins)
glimpse(penguins) # another version of the head function

# filter
gentoo = filter(penguins, species == "Gentoo") # subset the penguin data to only show the Gentoos
gentoo_ladies = filter(gentoo, sex =="female")
summary(gentoo_ladies)
# we could have also done this in a single line:
gentoo_ladies = filter(penguins, species=="Gentoo", sex=="female")

# introduce the pipe (%>%) feeds line output into next line
gentoo_ladies = penguins %>%
  filter(species == "Gentoo") %>%
  filter(sex == "female")

female_mean_mass = penguins %>% 
  filter(sex == "female") %>% 
  summarize(mean_mass_g = mean(body_mass_g))
female_mean_mass

gentoo_ladies_body_mass_g = penguins %>%
  filter(species == "Gentoo") %>%
  filter(sex == "female") %>%
  summarize(gentoo_ladies_body_mass_g = mean(body_mass_g)) %>%
  print()

gentoo_ladies_body_mass_g = mean(penguins$body_mass_g[penguins$sex == "female" & penguins$species == "Gentoo"], na.rm=T)

# Exercise 1.1
chinstrap = filter(penguins, species == "Chinstrap")
summary(chinstrap)
big_chinstraps = filter(chinstrap, flipper_length_mm > 200)
summary(big_chinstraps)

# more dplyr functions
# calculate mean mass of each species
species_mean_mass = penguins %>% 
  group_by(species) %>%
  summarize(mean_mass_g = mean(body_mass_g, na.rm=T))
species_mean_mass

#calculate mass of each species by sex
species_sex_mean_mass = penguins %>% 
  filter(!is.na(sex)) %>% # removes NAs from the data set (! = not)
  group_by(species, sex) %>%
  summarize(mean_mass_g = mean(body_mass_g), 
            sd_body_mass_g = sd(body_mass_g, na.rm=T), 
            n_penguins = n()) %>%
  print()

# save the table
write_csv(x=species_sex_mean_mass, 
  file="data/processed/penguin_mean_body_mass_g.csv")

# read that table back in to check it
temp = read_csv("data/processed/penguin_mean_body_mass_g.csv")
head(temp)

# convert body mass units
penguins_for_america = penguins %>%
  mutate(body_mass_lb = body_mass_g * 0.0022)
head(penguins_for_america)

# quickly display all islands sampled quickly
penguins %>%
  distinct(island)

# remove data
penguins_no_bill = penguins %>%
  select(-bill_length_mm, -bill_depth_mm)
head(penguins_no_bill)
