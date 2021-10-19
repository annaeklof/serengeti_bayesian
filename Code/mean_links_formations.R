## Links per species
## TRAITS
require(tidyverse)
#funcresp = "Linear"
funcresp = "A3B3"
path <- "/Users/annak52/Documents/Serengeti/Results/original/"
links <- tibble(read.table(paste("Data/serengeti_links.csv",sep=''), sep=",", header=TRUE)) %>%
    mutate_if(is.factor, as.character)
traits <- as_tibble(read.table("Data/All_traits_r.txt", header=TRUE, sep="\t")) %>%
    select(code, growth_form) 
groups <- read_csv(paste("Data/serengeti_groups.csv",sep=''), col_types="ci") 

## TRAITS
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

group_prey <- traits %>%
    filter(growth_form %in% c("herb", "shrub", "tree","graminoid", "annual")) %>%
    group_by(growth_form) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    select(prey_trait=growth_form, preys_per_group=n)

frac_traits <- links_groups %>%
    filter(pred_trait %in% c("group_3", "group_4", "group_5", "group_6")) %>% # we only want primary consumers, so not predator group 1-2
    mutate(pred_code = 1, prey_code=1) %>% # not interested in indentity of sp, just their trait belonging
    group_by(prey_trait) %>%
    mutate(links = sum(prey_code)) %>%
    ungroup() %>%
    distinct(.) %>%
    select(prey_trait, links) %>%
    distinct(.) %>%
    mutate(tot_links = sum(links)) %>%
    mutate(frac=round((links/tot_links),3)) %>%
    left_join(., group_prey, by="prey_trait") %>%
    mutate(tot_preys=sum(preys_per_group)) %>%
    mutate(mean_link_sp = tot_links/tot_preys, mean_link_group = links/preys_per_group) %>%
    select(plant_form = prey_trait, mean_link_group, mean_link_sp)

## GROUPS
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

group_prey <- groups %>%
    filter(group %in% c(7:14)) %>%
    group_by(group) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    select(prey_group=group, preys_per_group=n)

frac_groups <- links_groups %>%
    filter(pred_group %in% c(3:6)) %>% # we only want primary consumers, so not predator group 1-2
    mutate(pred_code = 1, prey_code=1) %>% # not interested in indentity of sp, just their trait belonging
    group_by(prey_group) %>%
    mutate(links = sum(prey_code)) %>%
    ungroup() %>%
    distinct(.) %>%
    select(prey_group, links) %>%
    distinct(.) %>%
    mutate(tot_links = sum(links)) %>%
    mutate(frac=round((links/tot_links),3)) %>%
    left_join(., group_prey, by="prey_group") %>%
    mutate(tot_preys=sum(preys_per_group)) %>%
    mutate(mean_link_sp = tot_links/tot_preys, mean_link_group = links/preys_per_group) %>%
    select(prey_group, mean_link_group)

## HABITAT
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

group_prey <- habs %>%
    filter(habitat %in% c("D", "G", "K","missing", "R", "S", "T", "W")) %>%
    group_by(habitat) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    select(prey_trait=habitat, preys_per_group=n)

frac_habs <- links_groups %>%
    filter(pred_trait %in% c("small_herbi", "large_grazer", "mega_grazer", "hyrax")) %>% # we only want primary consumers, so not predator group 1-2
    mutate(pred_code = 1, prey_code=1) %>% # not interested in indentity of sp, just their trait belonging
    group_by(prey_trait) %>%
    mutate(links = sum(prey_code)) %>%
    ungroup() %>%
    distinct(.) %>%
    select(prey_trait, links) %>%
    distinct(.) %>%
    mutate(tot_links = sum(links)) %>%
    mutate(frac=round((links/tot_links),3)) %>%
    left_join(., group_prey, by="prey_trait") %>%
    mutate(tot_preys=sum(preys_per_group)) %>%
    mutate(mean_link_sp = tot_links/tot_preys, mean_link_group = links/preys_per_group) %>%
    select(prey_trait,mean_link_group)

# ggplot(frac_groups) +
#     aes(x=plant_form, y=mean_link_group, color=factor(plant_form)) +
#     geom_point() +
#     geom_hline(yintercept = 3.93, lty= "dashed", color="red", size=1) +
#     theme(axis.text.x = element_text(angle = 90, size=10)) 
# 
#     scale_color_manual(guide=FALSE,values=carto_pal(name="Safe")) +
#     facet_wrap(~factor(dist_scen), labeller=as_labeller(labs_groups)) +
#     ggtitle("Plants randomly disturbed following functional groups") +
#     xlab("Consumer group") + ylab("Persistence") +
#     theme_bw() +
#     theme(legend.position = "none") +
#     theme(axis.text.x = element_text(angle = 90, size=5)) 
#     