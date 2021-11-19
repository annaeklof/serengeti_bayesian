require(tidyverse)
library(rcartocolor)
#funcresp = "Linear"
funcresp = "A3B3"


labs_groups <- c(
    "7" = "group 7, n=3",
    "8" = "group 8, n=9",
    "9" = "group 9, n=25",
    "10" = "group 10, n=1",
    "11" = "group 11, n=24",
    "12" = "group 12, n=1",
    "13" = "group 13, n=64",
    "14" = "group 14, n=2"
)

labs_traits <- c(
    "tree" = "trees, n=5",
    "shrub" = "shrubs, n=38",
    "herb" = "herbs, n=25",
    "graminoid" = "grasses, n=55",
    "annual" = "annuals, n=6")

labs_habs <- c(
    "S" = "shrubland, n=5",
    "D" = "disturbed, n=2",
    "T" = "thicket, n=2",
    "R" = "riverland, n=7",
    "missing" = "missing, n=18",
    "G" = "grassland, n=46",
    "W" = "wetland, n=28",
    "K" = "kopje, n=17")

cbp2 <- c("#E69F00", "#56B4E9", "#009E73", "#000000", 
          "#000099",  "#996633", "#CC79A7", "#660066" )


# CALCULATE FRACTION OF LINKS

## HABITATS
# to find fraction of links going from consumers to plant groups

links <- tibble(read.table(paste("Data/serengeti_links_habs.csv",sep=''), sep=";", header=TRUE)) %>%
    mutate_if(is.factor, as.character)

# Here, serengeti_info is tweaked since I have some instances
# of a species belonging to more than one habitat (only occurs with Grassland, Wetland, Kopje)
# a predator preying on a species in both hab G and K will feed on this species
# like it's two species instead of one (for ex SID --> SID_K and SID_G)

habs <- tibble(read.table(paste("Data/serengeti_info_habs.csv",sep=''), sep=",", header=TRUE)) %>%
    select(code, habitat)


# Connect predators to their respective habitat (non-existing)
pred_links <- links %>% 
    select(code=pred_code) %>%
    left_join(., habs, by="code") %>%
    select(pred_code=code, pred_trait=habitat)
# Connect preys to their resp. habitat, and combine the resulting 
# data frame with the one for predators
links_groups <- links %>% 
    select(code=prey_code) %>%
    left_join(., habs, by="code") %>%
    select(prey_code=code, prey_trait=habitat) %>%
    bind_cols(pred_links,.)
# below summarizes the fraction of links going into each plant group
# (here plant groups based on traits) from herbivores
# Problematic habitats: G, W, K (plants can belong to two habitats)
frac_links <- links_groups %>%
    filter(pred_trait %in% c("small_herbi", "large_grazer", "mega_grazer", "hyrax")) %>%
    mutate(pred_code = 1, prey_code=1) %>%
    group_by(prey_trait, pred_trait) %>%
    mutate(counts = sum(prey_code)) %>%
    ungroup() %>%
    distinct(.) %>%
    select(prey_trait, counts, pred_trait) %>%
    distinct(.) %>%
    group_by(pred_trait) %>%
    mutate(all_links = sum(counts)) %>%
    mutate(frac=round((counts/all_links),3)) %>%
    ungroup()

frac_links <- links_groups %>%
    filter(pred_trait %in% c("small_herbi", "large_grazer", "mega_grazer", "hyrax")) %>%
    mutate(prey_code=1) %>%
    group_by(prey_trait, pred_code) %>%
    mutate(counts = sum(prey_code)) %>%
    ungroup() %>%
    distinct(.) %>%
    select(prey_trait, counts, pred_code, pred_trait) %>%
    group_by(pred_code) %>%
    mutate(all_links = sum(counts)) %>%
    mutate(frac=round((counts/all_links),3)) %>%
    ungroup() 

p1 <- ggplot(frac_links) +
    aes(x=pred_code, y=frac, color=factor(prey_trait)) +
    geom_point(alpha=0.7, size=1.8) +
    #facet_wrap(~factor(pred_trait), scales="free_x", labeller=as_labeller(dist_labels_habs)) +
    #ggtitle("Various disturbances, nonlinear") +
    ylab("Fraction of links") +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    scale_color_manual(name = "Plant group", labels = labs_habs, 
                       values=cbp2) +
    ylim(0,1)


# TRAITS
# to find fraction of links going from consumers to plant groups
links <- tibble(read.table(paste("Data/serengeti_links.csv",sep=''), sep=",", header=TRUE)) %>%
    mutate_if(is.factor, as.character)
traits <- as_tibble(read.table("Data/All_traits_r.txt", header=TRUE, sep="\t")) %>%
    select(code, growth_form)

# Connect predators to their respective group
pred_links <- links %>% 
    select(code=pred_code) %>%
    left_join(., traits, by="code") %>%
    select(pred_code=code, pred_trait=growth_form)
# Connect preys to their resp. group, and combine the resulting 
# data frame with the one for predators
links_groups <- links %>% 
    select(code=prey_code) %>%
    left_join(., traits, by="code") %>%
    select(prey_code=code, prey_trait=growth_form) %>%
    bind_cols(pred_links,.)
# below summarizes the fraction of links going into each plant group
# (here plant groups based on traits) from herbivores
frac_links <- links_groups %>%
    filter(pred_trait %in% c("group_3", "group_4", "group_5", "group_6")) %>%
    mutate(pred_code = 1, prey_code=1) %>%
    group_by(prey_trait, pred_trait) %>%
    mutate(counts = sum(prey_code)) %>%
    ungroup() %>%
    distinct(.) %>%
    select(prey_trait, counts, pred_trait) %>%
    group_by(pred_trait) %>%
    mutate(all_links = sum(counts)) %>%
    ungroup() %>%
    mutate(frac=round((counts/all_links),3)) 
### separated by species
frac_links <- links_groups %>%
    filter(pred_trait %in% c("group_3", "group_4", "group_5", "group_6")) %>%
    mutate(prey_code=1) %>%
    group_by(prey_trait, pred_code) %>%
    mutate(counts = sum(prey_code)) %>%
    ungroup() %>%
    distinct(.) %>%
    select(prey_trait, counts, pred_code, pred_trait) %>%
    group_by(pred_code) %>%
    mutate(all_links = sum(counts)) %>%
    mutate(frac=round((counts/all_links),3)) %>%
    ungroup() 

p2 <- frac_links %>%
    mutate(prey_trait = case_when(
        prey_trait== "tree" ~ "trees, n=5",
        prey_trait=="shrub" ~ "shrubs, n=38",
        prey_trait=="herb" ~ "herbs, n=25",
        prey_trait=="graminoid" ~ "grasses, n=55",
        prey_trait=="annual" ~ "annuals, n=6",
        TRUE ~ prey_trait
    )) %>%
    mutate(prey_trait = fct_reorder(prey_trait, as.integer(as_factor(prey_trait)))) %>%
    
    ggplot() +
    aes(x=pred_code, y=frac, color=factor(prey_trait)) +
    geom_point(alpha=0.7, size=1.8) +
    ylab("Fraction of links") +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    #theme(axis.text.x = element_text(angle = 90, size=7)) +
    scale_color_manual(name = "Plant group", 
                       values=cbp2) 
#ggsave(paste0("../Results/Figures/", funcresp, "/frac_traits.jpg"), width = 6, height = 5)

## GROUPS
# to find fraction of links going from consumers to plant groups
links <- tibble(read.table(paste("Data/serengeti_links.csv",sep=''), sep=",", header=TRUE)) %>%
    mutate_if(is.factor, as.character)
groups <- read_csv(paste("Data/serengeti_groups.csv",sep=''), col_types="ci") 

# Connect predators to their respective group
pred_links <- links %>% 
    select(code=pred_code) %>%
    left_join(., groups, by="code") %>%
    select(pred_code=code, pred_group=group)
# Connect preys to their resp. group, and combine the resulting 
# data frame with the one for predators
links_groups <- links %>% 
    select(code=prey_code) %>%
    left_join(., groups, by="code") %>%
    select(prey_code=code, prey_group=group) %>%
    bind_cols(pred_links,.) %>%
    slice(-57)

# below summarizes the fraction of links going into each plant group
# (here plant groups based on traits) from herbivores (cons.gr 3:6)
frac_links <- links_groups %>%
    filter(pred_group %in% c(3:6)) %>%
    mutate(pred_code = 1, prey_code=1) %>%
    group_by(prey_group, pred_group) %>%
    mutate(counts = sum(prey_code)) %>%
    ungroup() %>%
    distinct(.) %>%
    select(prey_group, counts, pred_group) %>%
    distinct(.) %>%
    group_by(pred_group) %>%
    mutate(all_links = sum(counts)) %>%
    mutate(frac=round((counts/all_links),3)) %>%
    ungroup() 

### separated by species
frac_links <- links_groups %>%
    filter(pred_group %in% c(3:6)) %>%
    mutate(prey_code=1) %>%
    group_by(prey_group, pred_code) %>%
    mutate(counts = sum(prey_code)) %>%
    ungroup() %>%
    distinct(.) %>%
    select(prey_group, counts, pred_code, pred_group) %>%
    group_by(pred_code) %>%
    mutate(all_links = sum(counts)) %>%
    mutate(frac=round((counts/all_links),3)) %>%
    ungroup() 
###

p3 <- ggplot(frac_links) +
    aes(x=pred_code, y=frac, color=factor(prey_group)) +
    geom_point(alpha=0.7, size=1.8) +
    #facet_wrap(~factor(pred_group), scales="free_x", labeller=as_labeller(dist_labels_groups)) +
    #ggtitle("Various disturbances, nonlinear") +
    xlab("Primary consumer") + ylab("Fraction of links") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size=7)) +
    scale_color_manual(name = "Plant group", labels = labs_groups, 
                       values=cbp2) 
#ggsave(paste0("../Results/Figures/", funcresp, "/frac_groups.jpg"), width = 6, height = 5)




#ggsave(paste0("../Results/Figures/", funcresp, "/frac_habs.jpg"), width = 6, height = 5)
plot_grid(p1, p2, p3,
          nrow = 3,
          labels = "AUTO",
          label_size = 12,
          align = "v",
          rel_heights = c(3,3,3.5))
ggsave(paste0("../Results/Figures/", funcresp, "/all_frac.jpg"), width = 8, height = 9.5)
