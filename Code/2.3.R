## Laura Barraclough
## s1729795@ed.ac.uk
## Started: 09/05/2023

## Modelling competition between hard coral and Halimeda in the Maldives
## RQ 1: How do hard coral and Halimeda spp. interact in the Magoodhoo island
## reefs?

## Aim: look at how different hard coral species interact with Halimeda.
#--------------------------------------------------------------------------------

## Plotting each species against Halimeda cover
#   Platygyra sp.----

coral_a$Depth_m <- as.character(coral_a$Depth_m)

(platygyra_plot <- ggplot(coral, aes (x = Halimeda_cover.,
                                      y = Platygyra_sp,
                                      colour = Depth_m,
                                      shape = Site_Name)) +
    theme_classic() +
    geom_point(alpha = 1) +
    xlab("Halimeda cover (%)") +
    ylab("Platygyra cover (%)") +
    geom_smooth())
    #geom_smooth(method = "lm"))
#ggsave(platygyra_plot, file = "Outputs/platygyra_plot.png")

# Acropora ----

(acropora_plot <- ggplot(coral, aes (x = Halimeda_cover.,
                                      y = Acropora_sp,
                                      colour = Depth_m,
                                      shape = Site_Name)) +
    theme_classic() +
    geom_point(alpha = 1.5) +
    xlab("Halimeda cover (%)") +
    ylab("Acropora cover (%)"))
#geom_smooth(method = "lm"))
#ggsave(acropora_plot, file = "Outputs/acropora_plot.png")

# Porites sp. ----
(porites_plot <- ggplot(coral, aes (x = Halimeda_cover.,
                                      y = Porites_sp.,
                                      colour = Depth_m,
                                      shape = Site_Name)) +
   theme_classic() +
   geom_point(alpha = 1) +
   xlab("Halimeda cover (%)") +
   ylab("Porites_sp. cover (%)"))
#geom_smooth(method = "lm"))
#ggsave(porites_plot, file = "Outputs/porites_plot.png")

# Porites rus ----
(poritesR_plot <- ggplot(coral, aes (x = Halimeda_cover.,
                                      y = Porites_rus,
                                      colour = Depth_m,
                                      shape = Site_Name)) +
   theme_classic() +
   geom_point(alpha = 1) +
   xlab("Halimeda cover (%)") +
   ylab("Porites rus. (%)"))
#geom_smooth(method = "lm"))
#ggsave(poritesR_plot, file = "Outputs/poritesR_plot.png")

# Pocillopora damicornis ----
(pocillopora_plot <- ggplot(coral, aes (x = Halimeda_cover.,
                                      y = Pocillopora_damicornis,
                                      colour = Depth_m,
                                      shape = Site_Name)) +
   theme_classic() +
   geom_point(alpha = 1) +
   xlab("Halimeda cover (%)") +
   ylab("Pocillopora damicornus cover (%)"))
#geom_smooth(method = "lm"))
#ggsave(pocillopora_plot, file = "Outputs/pocillopora_plot.png")

# Favia ----
(favia_plot <- ggplot(coral, aes (x = Halimeda_cover.,
                                      y = Favia,
                                      colour = Depth_m,
                                      shape = Site_Name)) +
   theme_classic() +
   geom_point(alpha = 1) +
   xlab("Halimeda cover (%)") +
   ylab("Favia cover (%)"))
#geom_smooth(method = "lm"))
#ggsave(favia_plot, file = "Outputs/favia_plot.png")



    


## Panel construction ----
library(gridExtra)

theme_coral <- function(){
  theme_bw() +
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2),
                             units = ,"cm"),
          legend.position = "none",
          panel.grid = element_blank(),
          axis.text = element_text(size = 9),
          axis.title = element_text(size = 9),
          plot.title = element_text(size = 11, hjust = 0.5, face = "bold"))
}


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

