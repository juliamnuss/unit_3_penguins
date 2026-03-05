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


######################################################################################
# 03.05.2026
# Multiple Linear Regression
# Predicting y as a function of more than one x
# bill depth as a function of bill length and species

unique(penguins$year)
penguins %>% distinct(year)

# continuos and categorial
# drop the nas from the data you're using before making a multiple regression model
penguins_lm5 = penguins %>%
  filter(!is.na(bill_depth_mm),
        !is.na(bill_length_mm), 
        !is.na(species))
lm_5 = lm(bill_depth_mm ~ bill_length_mm + species, data = penguins_lm5)
summary(lm_5)
coef(lm_5) # vector of coefficients
coef(lm_5)[1] # subset the y-intercept

anova(lm_5)

# broom packages are not automatically loaded with tidyverse but can be called with tidy() function
# can also load broom with library(broom)
broom::tidy(lm_5) # ?tidy.lm
broom::tidy(lm_5, conf.int = TRUE, conf.level = 0.95) # Added confidence intervals to output
broom::tidy(lm_5, conf.int = TRUE, conf.level = 0.95) %>%
  mutate_if(is.numeric, round, 2) # round to 2 decimals
# save table
lm_5_table = broom::tidy(lm_5, conf.int = TRUE, conf.level = 0.95) %>%
  mutate_if(is.numeric, round, 2) 
write_csv(lm_5_table, file = "figures/lm_5_table.csv")

# install.packages("ggiraph")
# install.packages("ggiraphExtra")
library(ggiraph)
library(ggiraphExtra)
ggPredict(lm_5, se = T) + theme_bw() # will plot the linear model w/ standard error
ggPredict(lm_5, se = T, interactive = T) # make the graph interactive to see plot information

# predict()
lm_5_predictions = predict(lm_5, interval = "confidence", level = 0.95) # calculates lm predictions for the original dataset ?predict.lm
head(lm_5_predictions)
head(penguins_lm5)
penguins_lm5_predict = cbind(penguins_lm5, lm_5_predictions)
head(penguins_lm5_predict)

ggplot(data = penguins_lm5_predict, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = species, color = NULL), alpha = 0.3) +
  geom_line(aes(y = fit), linewidth = 1) +
  theme_bw() + 
  labs(x = "Bill Length (mm)", y = "Bill Depth (mm)", title = "Predictions")

# this is going to be done using base R
# notice how the predictions only span the range of the recorded data, but what if you wanted a full line . . .
# build a new bill_length_mm dataset that spans the full range of teh original data at even intervals
newdata_bill_length_mm = seq(min(penguins_lm5$bill_length_mm), max(penguins_lm5$bill_length_mm), by = 0.1)
# now complete bill length data for each species
newdata = expand.grid(bill_length_mm = newdata_bill_length_mm, species = unique(penguins_lm5$species)) # data frame names must match exactly
head(newdata)

# feed the prediction new data
newdata_predict_lm5 = cbind(newdata, predict(lm_5, interval = "confidence", level = 0.95, newdata = newdata))
dim(newdata_predict_lm5)
ggplot() +
  geom_point(data = penguins_lm5, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, x = bill_length_mm, fill = species, color = NULL), alpha = 0.3, data = newdata_predict_lm5) + 
  geom_line(aes(y = fit, x = bill_length_mm, color = species), linewidth = 1, data = newdata_predict_lm5) + 
  theme_bw() +
  labs(x = "Bill Length (mm)", y = "Bill Depth (mm)", title = "Predictions #2")

# Now were gonna do this again but with tidyverse tools and not base R
# Get model predictions
lm_5_predict = lm_5 %>%
  broom::augment(data = penguins_lm5, se_fit = TRUE, interval = "confidence") # augment() instead of predict() # see ?augment.lm
#mutate(lwr = .fitted - 1.96 * .se.fit, upr = .fitted + 1.96 * .se.fit) # Calculate 95% CI using SE

# Plot the data and the model predictions
ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species), data = lm_5_predict) + 
  geom_point() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = species, color = NULL), alpha = 0.2) +
  geom_line(aes(y = .fitted), linewidth = 1) + 
  labs(x = "Bill Length (mm)", y = "Bill Depth (mm)", title = "Predictions Again") + 
  theme_bw()

# get model predictions with new data
newdata = penguins_lm5 %>%
  tidyr::expand(bill_length_mm, species) # instead of expand.grid, type ?tidyr::expand in R console
lm_5_predict = lm_5 %>%
  broom::augment(newdata = newdata, se_fit = TRUE, interval = "confidence") #instead of predict()

# plot the data and model predictions
ggplot() + 
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species), data = penguins_lm5) + 
  geom_ribbon(aes(ymin = .lower, ymax = .upper, x = bill_length_mm, fill = species, color = NULL), alpha = 0.2, data = lm_5_predict) + 
  geom_line(data = lm_5_predict, aes (y = .fitted, x = bill_length_mm, color = species), linewidth = 1) +
  theme_bw() + 
  labs( x = "Bill Length (mm)", y = "Bill Depth (mm)", title = "Predictions #3")
