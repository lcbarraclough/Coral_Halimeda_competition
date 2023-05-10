## Laura Barraclough
## s1729795@ed.ac.uk
## Started: 09/05/2023

## Modelling competition between hard coral and Halimeda in the Maldives
## RQ 1: How do hard coral and Halimeda spp. interact in the Magoodhoo island
## reefs?

## Aim: plot model coral_m2
#---------------------------------------------------------------------------------
summary(coral_m2)

# Plot 1 ----
(m2_plot <- ggplot(coral, aes(x = Halimeda_cover.,
                              y = Total_hard_CC.,
                              colour = Depth_m)) +
   facet_wrap(~Site_Name, nrow = 1) +
   theme_classic() +
   xlab("Halimeda cover (%)") +
   ylab("Total hard coral cover (%)") +
   geom_point(alpha = 1) +
   geom_line(data = cbind(coral, pred = predict(coral_m2)),
                                                aes(y = pred),
                                                size = 1))

#ggsave(m2_plot, file = "Outputs/m2_plot.png", width = 10, height = 5)

# Plot 2 ----
coral_a <- coral[-c(30),]

#   try running model again with out large outlier
coral_m2.2 <-lmer(Total_hard_CC. ~ Halimeda_cover. +
                    (1|Depth_m/ Site_Name),
                  data = coral_a)
plot(coral_m2.2)
print(summary(coral_m2.2))

#Plot this
(m2.2_plot <- ggplot(coral_a, aes(x = Halimeda_cover.,
                              y = Total_hard_CC.,
                              colour = Depth_m)) +
    facet_wrap(~Site_Name, nrow = 1) +
    theme_classic() +
    xlab("Halimeda cover (%)") +
    ylab("Total hard coral cover (%)") +
  geom_point(alpha = 1) +
    geom_line(data = cbind(coral_a, pred = predict(coral_m2.2)),
              aes(y = pred),
              size = 1))

#ggsave(m2.2_plot, file = "Outputs/m2.2_plot.png", width = 10, height = 5)

