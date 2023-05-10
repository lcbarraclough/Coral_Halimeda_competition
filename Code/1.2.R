## Laura Barraclough
## s1729795@ed.ac.uk
## Started: 08/05/2023

## Modelling competition between hard coral and Halimeda in the Maldives
## RQ 1: How do hard coral and Halimeda spp. interact in the Magoodhoo island
## reefs?
---------------------------------------------------------------------------------
  ### 3. Model approach 1 ----
# Aim: create a model for an individual site using the simplest model
#   firstly create a data frame for the individual sites
harbour_reef <- filter(coral, Site_Name == "Harbour_reef")

harbour_m <- lm(Total_hard_CC. ~ Halimeda_cover.,
                data = harbour_reef)
summary(harbour_m)
plot(harbour_m)
#   what these plots tell us is that this model isn't doing a good job
#   of representing the data


### 4. Model approach 2 ----
# Hierarchical model with random effects
#   start with depth being the only random effect
coral_m1 <- lmer(Total_hard_CC. ~ Halimeda_cover. +
                   (1|Depth_m),
                 data = coral)
summary(coral_m1)#don't know why this won't work
plot(coral_m1)
#     add in site as another random effect
coral_m2 <- lmer(Total_hard_CC. ~ Halimeda_cover. +
                   (1|Depth_m/ Site_Name),
                 data = coral)
summary(coral_m2)

plot(coral_m2) #don't know why this won't work

