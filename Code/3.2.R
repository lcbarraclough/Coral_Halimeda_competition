## Laura Barraclough
## s1729795@ed.ac.uk
## Started: 11/05/2023

## Modelling competition between hard coral and Halimeda in the Maldives
## RQ 1: How do hard coral and Halimeda spp. interact in the Magoodhoo island
## reefs?

## Aim: look at individual depths for trends as suggested by Seb.
#------------------------------------------------------------------------------
library(tidyverse)

## Anova of depth and Halimeda cover ----
coral$Depth_m <- as.numeric(coral$Depth_m)
(halimeda.depth <- ggplot(coral, aes(x = Depth_m, 
                                     y = Halimeda_cover.,
                                     colour = Site_Name)) +
   geom_point(alpha = 1) +
    geom_smooth(method = "lm") +
    xlab("Depth (m)") +
    ylab("Halimeda cover (%)") +
    theme_bw())
#ggsave(halimeda.depth, file = "Outputs/halimeda_depth.png", width = 5, height = 5)

# So Halimeda only increases significantly at depth at Coral Gardens.
# We can have a closer look at this relationship by constructing an anova.

anova.halimeda_depth <- aov(Halimeda_cover. ~ Depth_m,
                        data = coral)
summary(anova.halimeda_depth)
plot(anova.halimeda_depth)
# Df Sum Sq Mean Sq F value  Pr(>F)   
# Depth_m      1   4382    4382   11.88 0.00165 **
#  Residuals   31  11437     369 


## Look at relationships between had coral cover and Halimeda cover ----
(coralvshalimeda1 <- ggplot(coral_a, aes(x = Halimeda_cover.,
                              y = Total_hard_CC.,
                              colour = Site_Name)) +
   geom_point(alpha = 1) +
   geom_smooth(method = "lm") +
   theme_bw() +
   xlab("Halimeda cover (%)") +
   ylab("Total hard coral cover (%)") +
   xlim(0,81) +
   ylim(0,60))
 
#ggsave(snorkel_1, file = "Outputs/snorkel_1.png", width = 5, height = 5)

(coralvshalimeda2 <- ggplot(coral_a, aes(x = Halimeda_cover.,
                                y = Total_hard_CC.)) +
    geom_point(alpha = 1) +
    geom_smooth(method = "lm") +
    theme_bw() +
    xlab("Halimeda cover (%)") +
    ylab("Total hard coral cover (%)") +
    xlim(0,81) +
    ylim(0,47) +
    ggtitle("Halimeda sp. cover and hard coral cover at 3m depth"))
ggsave(snorkel_2, file = "Outputs/snorkel_2.png", width = 5, height = 5)

#   create a snorkel dataframe 
library(vegan)

#snorkel_data <- coral_a %>% 
  #filter( ., Depth_m == "3") %>% 
  #group_by(Site_Name) %>% 
  #summarise(across(everything(), sum, na.rm = TRUE)) %>% 
  #ungroup %>% 
  #select(4:10) %>% 
  #tibble(diversity = .) %>% 
  #bind_cols(snorkel_data, .)

#rlang::last_error()

## What effect does depth have on this relationship? ----

## Try potting Halimeda cover and depth with basic linear regression at
## each of the three depths

# 3m depth
#(snorkel <- coral_a %>% 
             # dplyr:: filter(Depth_m == "3") %>%
#  ggplot(data =coral_a) +
#  (mapping = aes(x = Halimeda_cover.,
#  y = Total_hard_CC.,
#  colour = Site_Name)) +
#  geom_point(alpha = 1) +
#  geom_smooth(method = "lm") +
# theme_bw() +
#  xlab("Halimeda cover (%)") +
#  ylab("Total hard coral cover (%)"))

(snorkel <- ggplot(data = coral_a %>% 
                     dplyr::filter(Depth_m == "3"),
                   aes(x = Halimeda_cover.,
                    y = Total_hard_CC.)) +
                    geom_point(alpha = 1) +
                    geom_smooth(method = "lm") +
                    theme_bw() +
                    xlab("Halimeda cover (%)") +
                    ylab("Total hard coral cover (%)"))

# 7m depth

(dive_7m <- ggplot(data = coral_a %>% 
                     dplyr::filter(Depth_m == "7"),
                   aes(x = Halimeda_cover.,
                       y = Total_hard_CC.)) +
    geom_point(alpha = 1) +
    geom_smooth(method = "lm") +
    theme_bw() +
    xlab("Halimeda cover (%)") +
    ylab("Total hard coral cover (%)"))

# 12m depth

(dive_12m <- ggplot(data = coral_a %>% 
                      dplyr::filter(Depth_m == "12"),
                    aes(x = Halimeda_cover.,
                        y = Total_hard_CC.)) +
    geom_point(alpha = 1) +
    geom_smooth(method = "lm") +
    theme_bw() +
    xlab("Halimeda cover (%)") +
    ylab("Total hard coral cover (%)"))

## Create a panel
library(gridExtra)
(depth_panel<- grid.arrange(
  snorkel + ggtitle("(a)"),
  dive_7m + ggtitle("(b)"),
  dive_12m + ggtitle("(c)"),
  ncol(3)))

  