## Laura Barraclough
## s1729795@ed.ac.uk
## Script started on: 23/05/2023

## Aims: use proportion data calculated in excel to express differences. 
## 
#--------------------------------------------------------------------------------
# Packages and dataframes ----
# 1.  Load dataframe and libraries ----
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
library(stats)

coral_prop <- read.csv("C:\\Users\\lcbar\\OneDrive\\Documents\\MSc Marine Systems and Policy\\Maldives documents\\Report\\diversity_index1.csv")
coral <- read.csv("C:\\Users\\lcbar\\OneDrive\\Documents\\MSc Marine Systems and Policy\\Maldives documents\\Report\\Coral_Halimeda_competition\\Input\\Input_data1.csv")

##  Hypothesis (i) 
# Bar plot----
coral_prop$Depth_m <- factor(coral_prop$Depth_m,
                        levels = c("3", "7", "12"),
                        labels = c("3", "7", "12"))
coral_prop$Site_name <- factor(coral_prop$Site_name,
                               levels = c("Harbour_reef", "Coral_gardens", "Blue_cove"))

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

(bar.prop.rus <- ggplot(coral_prop, aes(x = Site_name,
                              y = Prop.Porites_rus,
                              fill = Depth_m)) +
    geom_histogram(stat = "identity", position = "dodge") +
    theme_bw() +
    ylab("Proportion Porites rus. cover (%)\n") +                             
    xlab("Site name")  +
    theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),# Angled labels, so text doesn't overlap
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 14, face = "plain"),                      
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm"),
          legend.title = element_text(size = 14, face = "italic"),
          legend.text = element_text(size = 12, face = "italic"),
          legend.box.background = element_rect(colour = "grey",
                                               size = 0.3)) +
    scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73"),
                      labels = c("3",
                                 "7",
                                 "12"),
                      name = "Depth (m)") +
    scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
    scale_x_discrete(labels = c("Blue_cove" = "Blue cove",
                                "Coral_gardens" = "Coral gardens",
                                "Harbour_reef" = "Harbour reef"))
)
#ggsave(bar.prop.rus, file = "Outputs/barplot_final.png", width = 5, height = 5)

## Hypothesis (ii) ----
(test1 <- ggplot(coral_prop, aes(x = Prop.Porites_rus,
                                 y = mean_rem,
                                 colour = Depth_m,
                                 shape = Site_name)) +
   geom_point(alpha = 1,
              size = 3) +
   xlab("Proportion Porites rus cover") +
   ylab("Mean proportional cover of remaining species") +
   theme_bw() +
   scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73"),
                     labels = c("3",
                                "7",
                                "12"),
                     name = "Depth (m)") +
   scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73"))
   )
## I don't think this graph shows anything.
# Use standard deviation as a way to express the range of coral species coverage between sites.
# if the stdv is low, species have fairly even coverage. 


(stdv_plot <- ggplot(coral_prop, aes(x = Prop.Porites_rus,
                                 y = stdv_rem,
                                 colour = Depth_m,
                                 shape = Site_name)) +
    geom_point(alpha = 1,
               size = 3) +
    xlab("Proportion Porites rus cover") +
    ylab("Standard deviation remaining species") +
    theme_bw() +
    scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73"),
                      labels = c("3",
                                 "7",
                                 "12"),
                      name = "Depth (m)") +
    scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73"))
)

stdv.lm <-  lm(stdv_rem ~ Prop.Porites_rus,
                data = coral_prop)
summary(stdv.lm)
plot(stdv.lm)


stdv.lm_pred <- ggpredict(stdv.lm, terms = c("Prop.Porites_rus"))

(stdv.lm_plot <- ggplot(stdv.lm_pred) +
  geom_line(aes(x = x, y = predicted)) +
  geom_ribbon(aes(x = x, ymin = predicted - std.error,
                  ymax = predicted + std.error),
              fill = "lightgrey", alpha = 0.5) +
  geom_point(data = coral_prop,
             aes(x = Prop.Porites_rus,
                 y = stdv_rem,
                 colour = Depth_m,
                 shape = Site_name),
             size = 3,
             alpha = 1) +
         theme_bw() +
    scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
    scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
    xlab("Proportional Porites rus cover") +
    ylab("Standard deviation cover of other coral species") +
    theme(legend.title = element_blank())
    )
#ggsave(stdv.lm_plot, file = "Outputs/stdvlmplot.png", height = 5, width = 5)

## Hypothesis (iii)----
## Look at wider dataset
# Using the gather() function 
coral_long <- gather(coral, Species, Cover,
                     c(Platygyra_sp, Acropora_sp, Porites_sp., Pocillopora_damicornis, Favia))
# this makes a longer dataframe of all the coral data

## Plot this as a bar graph
(hypothesis3 <- ggplot(coral_long, aes(x = Porites_rus,
                                       y = Cover,
                                       colour = Species)) +
    geom_point(size = 2, alpha = 1) +
    facet_wrap(~ Species, scales = "free_y") +
    theme(legend.position = "right") +
    ylab("Coral cover (%)") +
    xlab("Porites rus cover"))
#ggsave(hypothesis3, file = "Outputs/hypothesis_3.png", width = 7, height = 5)

#Check models
lm_Platygyra <- lmer(Platygyra_sp ~ Porites_rus +
                       (1|Depth_m),
                   data = coral)
summary(lm_Platygyra)

lm_Acropora <- lm(Acropora_sp ~ Porites_rus,
                  data = coral)
summary(lm_Acropora)

lm_Porites <- lm(Porites_sp. ~ Porites_rus,
                 data = coral)
summary(lm_Porites)

lm_Pocillopora <- lm(Pocillopora_damicornis ~ Porites_rus,
                     data = coral)
summary(lm_Pocillopora)

lm_Favia <- lm(Favia ~ Porites_rus,
               data = coral)
summary(lm_Favia)
