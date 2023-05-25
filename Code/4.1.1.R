## Laura Barraclough
## s1729795@ed.ac.uk
## Script started on: 12/05/2023

## Aims: to look at coral distribution across the three sites sampled in Magoodhoo.
#--------------------------------------------------------------------------------
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
library(ggplot2)


# Load dataframe
reefs <- read.csv("C:\\Users\\lcbar\\OneDrive\\Documents\\MSc Marine Systems and Policy\\Maldives documents\\Report\\Input_data2.csv")
View(reefs)
# 2. Visualize coral distributions ----
##  we can do this as a bar chart
str(reefs)
reefs$Depth_m <- as.character(reefs$Depth_m)

(bar1 <- ggplot(reefs, aes(x = Site,
                           y = CCT,
                           fill = Depth_m)) +
   geom_histogram(stat = "identity", position = "dodge") +
   theme_bw() +
   ylab("Total hard coral cover (%)\n") +                             
   xlab("Site")  +
   theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
         axis.text.y = element_text(size = 12),
         axis.title = element_text(size = 14, face = "plain"),                      
         panel.grid = element_blank(),                                          
         plot.margin = unit(c(1,1,1,1), units = , "cm")))

## ANOVA models

anova.CCT <- aov(CCT ~ Site,
                 data = reefs)
summary(anova.CCT)
plot(anova.CCT)

anova.depth <- reefs %>% 
  group_by(Site) %>% 
  aov(CCT ~ Depth_m,
      data = .)
summary(anova.depth)
plot(anova.depth)

anova.porites_rus <- aov(Porites_rus ~ Site_Name,
                         data = coral_a)
summary(anova.porites_rus)

anova.porites_rusgrpd <- aov(Porites_rus ~ Depth_m,
                               data = coral_a %>% 
                               group_by(Site_Name))
summary(anova.porites_rusgrpd)


## Focus on Porites rus. ----
#   Firstly plot this as a bar graph
# reorder the data so the depths are more logical
reefs$Depth_m <- factor(reefs$Depth_m,
                        levels = c("3", "7", "12"),
                        labels = c("3", "7", "12"))
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#7B68EE")
                       
(bar.rus <- ggplot(reefs, aes(x = Site,
                           y = Porites_rus,
                           fill = Depth_m)) +
   geom_histogram(stat = "identity", position = "dodge") +
   theme_bw() +
   ylab("Porites rus. cover (%)\n") +                             
   xlab("Site name")  +
   theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),# Angled labels, so text doesn't overlap
         axis.text.y = element_text(size = 12),
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
    scale_x_discrete(labels = c("Blue_cove" = "Blue Cove",
                                "Coral_gardens" = "Coral Gardens",
                                "Harbour_reef" = "Harbour Reef"))
)
#ggsave(bar.rus, file = "Outputs/bar_rus.png", width = 10, height = 10)

# How does this look as a boxplot for each site? ----
coral_a$Depth_m <- factor(coral_a$Depth_m,
                        levels = c("3", "7", "12"),
                        labels = c("3", "7", "12"))
coral_a$Depth_m <- as.character(coral_a$Depth_m)

(box.rus_Blue <- ggplot(data = coral_a %>% 
                          dplyr:: filter(Site_Name == "Blue_cove"),
                        aes(y = Porites_rus,
                            x = Depth_m,
                            group = Depth_m)) +
   geom_boxplot(aes(fill = Depth_m)) +
   theme_bw() +
   ylab("Porites rus. cover (%)\n") +                             
   xlab("Depth (m)")  +
   theme(axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         axis.title = element_text(size = 14, face = "plain"),                      
         panel.grid = element_blank(),                                          
         plot.margin = unit(c(1,1,1,1), units = , "cm"),
         legend.title = element_text(size = 14, face = "italic"),
         legend.text = element_text(size = 12, face = "italic"),
         legend.box.background = element_rect(colour = "grey",
                                              size = 0.3)) +
    ylim(0,18) +
   scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73"),
                     breaks = c("3", "7", "12"),
                     name = "Depth (m)",
  labels = c("3", "7", "12")) +
   scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73"))
)
#ggsave(box.rus_Blue, file = "Outputs/boxplot_Blue.png", width = 5, height = 5)


(box.rus_Coral <- ggplot(data = coral_a %>% 
                          dplyr:: filter(Site_Name == "Coral_gardens"),
                        aes(y = Porites_rus,
                            x = Depth_m,
                            group = Depth_m)) +
    geom_boxplot(aes(fill = Depth_m)) +
    theme_bw() +
    ylab("Porites rus. cover (%)\n") +                             
    xlab("Depth (m)")  +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                      
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm"),
          legend.title = element_text(size = 14, face = "italic"),
          legend.text = element_text(size = 12, face = "italic"),
          legend.box.background = element_rect(colour = "grey",
                                               size = 0.3)) +
    ylim(0,18) +
    scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73"),
                      breaks = c("3", "7", "12"),
                      name = "Depth (m)",
                      labels = c("3", "7", "12")) +
    scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73"))
)
#ggsave(box.rus_Coral, file = "Outputs/boxplot_Coral.png", width = 5, height = 5)

(box.rus_Harbour <- ggplot(data = coral_a %>% 
                          dplyr:: filter(Site_Name == "Harbour_reef"),
                        aes(y = Porites_rus,
                            x = Depth_m,
                            group = Depth_m)) +
    geom_boxplot(aes(fill = Depth_m)) +
    theme_bw() +
    ylab("Porites rus. cover (%)\n") +                             
    xlab("Depth (m)")  +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                      
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm"),
          legend.title = element_text(size = 14, face = "italic"),
          legend.text = element_text(size = 12, face = "italic"),
          legend.box.background = element_rect(colour = "grey",
                                               size = 0.3)) +
    ylim(0, 18) +
    scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73"),
                      breaks = c("3", "7", "12"),
                      name = "Depth (m)",
                      labels = c("3", "7", "12")) +
    scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73"))
)
#ggsave(box.rus_Harbour, file = "Outputs/boxplot_Harbour.png", width = 5, height = 5)

# Make these into a panel ----
# THis isn't working :((()))
par(mfrow = c(1, 3))
(box.rus_Blue)
(box.rus_Coral)
(box.rus_Harbour)
