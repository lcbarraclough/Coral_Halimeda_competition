## Laura Barraclough
## s1729795@ed.ac.uk
## Started: 08/05/2023

## Modelling competition between hard coral and Halimeda in the Maldives
## RQ 1: How do hard coral and Halimeda spp. interact in the Magoodhoo island
## reefs?
---------------------------------------------------------------------------------

## Workflow:
### 1. Download programmes and load csv into dataframe ----
# Load libraries ----
library(tidyverse)  # for data manipulation (tidyr, dplyr), visualization, (ggplot2), ...
##install.packages("tidyverse")
library(lme4)  # for hierarchical models
##install.packages("lme4")
library(sjPlot)  # to visualise model outputs
##install.packages("sjPlot")
library(ggeffects)  # to visualise model predictions
##install.packages("ggeffects")
library(MCMCglmm)  # for Bayesian models
##install.packages("MCMCglmm")
library(MCMCvis)  # to visualise Bayesian model outputs
##install.packages("MCMCvis")
library(stargazer)  # for tables of model outputs
library(ggplot2)


# Load dataframe and check contents ----
coral <- read.csv("C:\\Users\\lcbar\\OneDrive\\Documents\\MSc Marine Systems and Policy\\Maldives documents\\Report\\Coral_Halimeda_competition\\Input\\Input_data1.csv")
#   check that the data frame loaded properly
View(coral)
#   check the classes of data in our frame
str(coral)
#   depth is an integer but should be a character

coral$Depth_m <- as.character(coral$Depth_m)
#   check this worked
str(coral)
#   yay it did :)

# Look at the structure of the data
unique(coral$Site_Name)
length(unique(coral$Site_Name))
#   three site names: "Harbour_reef"  "Coral_gardens" "Blue_cove"  



### 2. Initial data plotting
# Plot simple histograms ----
#   plot distribution of total hard coral cover across all sites and depths
(hist1 <- ggplot(coral, aes(x = Total_hard_CC.)) +
   geom_histogram() +
   theme_classic())
#   looks like normal (ish) distribution

#   plot distribution of Halimeda cover across all sites and depths
(hist2 <- ggplot(coral, aes(x = Halimeda_cover.)) +
    geom_histogram() +
    theme_classic())
#   not so normal 

## Scatter plot for data ----
dev.off() #fixes plotting error

(prelim_plot <- ggplot(coral, aes(x = Halimeda_cover.,
                                  y = Total_hard_CC.)) +
   geom_point() +
   geom_smooth(method = "lm"))

basic.lm <- lm(Total_hard_CC. ~ Halimeda_cover.,
               data = coral)
summary(basic.lm)
plot(basic.lm, which = 1)
plot(basic.lm, which = 2)
plot(basic.lm, which = 3)
plot(basic.lm, which = 4)

## Split points by colour = location
(prelim_plot2 <- ggplot(coral, aes(x = Halimeda_cover.,
                                   y = Total_hard_CC.,
                                   colour = Site_Name,
                                   shape = Depth_m)) +
    geom_point(size = 2) +
    #geom_smooth(method = "lm") +
    theme_classic())


(split_plot <- ggplot(aes(x = Halimeda_cover.,
                          y = Total_hard_CC.,
                          colour = Depth_m), data = coral) + 
    geom_point() + 
    facet_wrap(~ Site_Name) + # create a facet for Site 
    xlab("Halimeda cover (%)") + 
    ylab("Hard coral cover (%)"))

