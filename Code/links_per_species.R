## Links per species
## TRAITS
require(tidyverse)
source("mean_links_formations.R")
#funcresp = "Linear"
funcresp = "A3B3"
path <- "/Users/annak52/Documents/Serengeti/Results/original/"
links <- tibble(read.table(paste("Data/serengeti_links.csv",sep=''), sep=",", header=TRUE)) %>%
    mutate_if(is.factor, as.character)
traits <- as_tibble(read.table("Data/All_traits_r.txt", header=TRUE, sep="\t")) %>%
    select(code, growth_form) 

# Connect predators to their respective group
# take all preds in the links df. give them their "trait" (being group - pred_trait)
pred_links <- links %>% 
    select(code=pred_code) %>%
    left_join(., traits, by="code") %>%
    select(pred_code=code, pred_trait=growth_form)
# Connect preys to their resp. group -
# take all preys in the links df. give them their trait (prey_trait)
# and combine the resulting data frame with the one for predators
links_groups <- links %>% 
    select(code=prey_code) %>%
    left_join(., traits, by="code") %>%
    select(prey_code=code, prey_trait=growth_form) %>%
    bind_cols(pred_links,.)

frac_traits <- rename(frac_traits, prey_trait=plant_form) %>%
    select(prey_trait, mean_link_group)

####  
# Here, we get the number of predatory links going into each plant species
frac_links <- links_groups %>%
    filter(pred_trait %in% c("group_3", "group_4", "group_5", "group_6")) %>% # we only want primary consumers, so not predator group 1-2
    mutate(count=1) %>%
    group_by(prey_code) %>%
    mutate(links_per_species =sum(count)) %>%
    ungroup() %>%
    #filter(prey_code=="ACASEN") %>%
    select(prey_code, prey_trait, links_per_species, count) %>%
    distinct(.) %>%
    #count gives here total no of plant sp (129)
    #tot_links gives total number of predatory links in the network (only from prim. cons)
    mutate(tot_links = sum(links_per_species), count=sum(count)) %>%
    mutate(mean_link_sp = tot_links/count) %>%
    left_join(., frac_traits, by="prey_trait")
    


p1 <- frac_links %>%
    ggplot() +
    geom_boxplot(aes(x=prey_trait, y=links_per_species)) +
    geom_hline(yintercept = 3.93, lty= "dashed", color="black", size=1) +
    geom_point(aes(x=prey_trait, y=mean_link_group), color="darkblue", shape=5, size=3) +
    #geom_point(aes(x=prey_trait, y=mean_link_sp), color="red", size=4, shape=4) +
    xlab("Plant trait") + ylab("Links per plant species") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13)) 
ggsave(paste0("../Results/Figures/", funcresp, "/links_traits.jpg"), width = 8, height = 6)


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
    bind_cols(pred_links,.)

# below summarizes the fraction of links going into each plant group
# (here plant groups based on traits) from herbivores (cons.gr 3:6)
frac_links <- links_groups %>%
    filter(pred_group %in% c(3:6)) %>%
    mutate(count=1) %>%
    group_by(prey_code) %>%
    mutate(links_per_species =sum(count)) %>%
    ungroup() %>%
    select(prey_code, prey_group, links_per_species, count) %>%
    distinct(.) %>%
    mutate(tot_links = sum(links_per_species), count=sum(count)) %>%
    mutate(mean_link_sp = tot_links/count) %>%
    left_join(., frac_groups, by="prey_group")

p2 <- frac_links %>%
    ggplot() +
    geom_boxplot(aes(x=as.factor(prey_group), y=links_per_species)) +
    geom_point(aes(x=as.factor(prey_group), y=mean_link_group), color="darkblue", shape=5, size=3) +
    geom_hline(yintercept = 3.93, lty= "dashed", color="black", size=1) +
    xlab("Group membership of plants") + ylab("Links per plant species") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13)) 
pggsave(paste0("../Results/Figures/", funcresp, "/links_groups.jpg"), width = 8, height = 6)

## HABITATS
links <- tibble(read.table(paste("Data/serengeti_links_habs.csv",sep=''), sep=";", header=TRUE)) %>%
    mutate_if(is.factor, as.character)

# Here, serengeti_info is tweaked since I have some instances
# of a species belonging to more than one habitat (only occurs with Grassland, Wetland, Kopje)
# a predator preying on a species in both hab G and K will feed on this species
# like it's two species instead of one (for ex SID --> SID_K and SID_G)

habs <- tibble(read.table(paste("Data/serengeti_info_habs.csv",sep=''), sep=",", header=TRUE)) %>%
    select(code, habitat)

# Connect predators to their respective group
# take all preds in the links df. give them their "trait" (being group - pred_trait)
pred_links <- links %>% 
    select(code=pred_code) %>%
    left_join(., habs, by="code") %>%
    select(pred_code=code, pred_trait=habitat)
# Connect preys to their resp. group -
# take all preys in the links df. give them their trait (prey_trait)
# and combine the resulting data frame with the one for predators
links_groups <- links %>% 
    select(code=prey_code) %>%
    left_join(., habs, by="code") %>%
    select(prey_code=code, prey_trait=habitat) %>%
    bind_cols(pred_links,.)

####  
# Here, we get the number of predatory links going into each plant species
frac_links <- links_groups %>%
    filter(pred_trait %in% c("small_herbi", "large_grazer", "mega_grazer", "hyrax")) %>% # we only want primary consumers, so not predator group 1-2
    mutate(count=1) %>%
    group_by(prey_code) %>%
    mutate(links_per_species =sum(count)) %>%
    ungroup() %>%
    #filter(prey_code=="ACASEN") %>%
    select(prey_code, prey_trait, links_per_species, count) %>%
    distinct(.) %>%
    #count gives here total no of plant sp (129)
    #tot_links gives total number of predatory links in the network (only from prim. cons)
    mutate(tot_links = sum(links_per_species), count=sum(count)) %>%
    mutate(mean_link_sp = tot_links/count) %>%
    left_join(., frac_habs, by="prey_trait")

p3 <- frac_links %>%
    ggplot() +
    geom_boxplot(aes(x=prey_trait, y=links_per_species)) +
    geom_hline(yintercept = 3.93, lty= "dashed", color="black", size=1) +
    geom_point(aes(x=prey_trait, y=mean_link_group), color="darkblue", shape=5, size=3) +
    xlab("Plant habitat") + ylab("Links per plant species") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13)) 
ggsave(paste0("../Results/Figures/", funcresp, "/links_habs.jpg"), width = 8, height = 6)

plot_grid(p3, p1, p2,
                 nrow = 3,
                 labels = "AUTO",
                 label_size = 12,
                 #hjust = -1, vjust = -1,
                 #label_x = 1, label_y = 1,
                 align = "v",
                 rel_heights = c(7,7,7))
ggsave(paste0("../Results/Figures/", funcresp, "/links_all_plants.jpg"), width = 8, height = 10)
