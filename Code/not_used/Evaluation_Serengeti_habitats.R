require(tidyverse)

path <- "/Users/annak52/Documents/Serengeti/Results/habitats/A3B3/"
Extension <- ".RData"


habitats <- c("control","S","D","T","R","G","W","K")

for (i in 1:length(habitats)){
  # Result approximation method
  hab <- habitats[i]
  load(paste0(path,"HigherHab_",hab, Extension))
  persistence <- mutate(persistence, habitat_higher_pi=hab)
  if (hab=="control"){
    tab <- persistence
  } else{ 
    tab <- bind_rows(tab,persistence)
  }
}

tab %>%
  filter(group %in% c(1:6)) %>% 
  ggplot() +
  aes(x=factor(group), y=per, color=factor(group)) +
  geom_boxplot(outlier.size=0.75) +
  facet_wrap(~habitat_higher_pi) +
  theme_bw()

tes <- tab %>%
  filter(habitat_higher_pi == "G") 