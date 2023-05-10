## Laura Barraclough
## s1729795@ed.ac.uk
## Started: 09/05/2023

## Modelling competition between hard coral and Halimeda in the Maldives
## RQ 1: How do hard coral and Halimeda spp. interact in the Magoodhoo island
## reefs?

## Aim: look at diversity of hard corals and model changes with Halimeda.
#---------------------------------------------------------------------------------
#  adding a column to the dataset for shannon diversity index
library(vegan) #package that does shannon diversity
coral_2 <- coral #make a copy of the original dataset

coral_2 %>% 
  mutate(shannon_index = diversity(Platygyra_sp, Pocillopora_damicornis,
                                   Porites_rus, Porites_sp., Favia))
