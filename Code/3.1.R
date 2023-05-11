## Laura Barraclough
## s1729795@ed.ac.uk
## Started: 11/05/2023

## Modelling competition between hard coral and Halimeda in the Maldives
## RQ 1: How do hard coral and Halimeda spp. interact in the Magoodhoo island
## reefs?

## Aim: add trend lines to each of the plots
#--------------------------------------------------------------------------------
library(ggeffects)

## Platygyra ----
platygyra_m <- lmer(Platygyra_sp ~ Halimeda_cover. +
  (1|Site_Name) + (1|Depth_m),
data = coral)
summary(platygyra_m)
plot(platygyra_m)


#     Prediction dataframe
plat_pred <- ggpredict(platygyra_m, terms = c("Halimeda_cover."))

# Plot model predictions
(platy_m.plot <- ggplot(plat_pred)) +
  geom_line(aes(x = x, y = predicted)) +
  geom_ribbon(aes(x = x, ymin = predicted - std.error,
                  ymax = predicted + std.error),
              fill = "lightgrey", alpha = 0.5) +
  geom_point(data = coral,
             aes(x = Halimeda_cover.,
             y = Platygyra_sp,
             colour = Depth_m,
             shape = Site_Name)) +
  labs(x = "Halimeda cover (%)", y = "Platygyra cover (%)" +
  theme_coral())
ggsave(platy_m.plot, file = "Outputs/platy_mplot.png", height = 10, width = 10)
  





(coral_panel <- grid.arrange(
  platygyra_plot + ggtitle("(a)") + 
    theme_coral(),
  acropora_plot + ggtitle("(b)") + 
    theme_coral(),
  porites_plot + ggtitle("(c)") + 
    theme_coral(),
  poritesR_plot + ggtitle ("(d)") +
    theme_coral(),
  pocillopora_plot + ggtitle("(e)") +
    theme_coral(),
  favia_plot + ggtitle("(f)") +
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2),
                             units = ,"cm"),
          legend.position = "right",
          panel.grid = element_blank(),
          axis.text = element_text(size = 9),
          axis.title = element_text(size = 9),
          plot.title = element_text(size = 11, hjust = 0.5, face = "bold")),
  ncol = 3))
ggsave(coral_panel, file = "Outputs/coral_panel.png", width = 15, height = 15)
