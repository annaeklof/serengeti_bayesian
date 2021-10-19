require(tidyverse)
library(rcartocolor)
#funcresp = "Linear"
funcresp = "A3B3"

# CALCULATE FRACTION OF LINKS

## HABITATS
# to find fraction of links going from consumers to plant groups

links_h <- tibble(read.table(paste("Data/serengeti_links_habs.csv",sep=''), sep=";", header=TRUE)) %>%
    mutate_if(is.factor, as.character)

# Here, serengeti_info is tweaked since I have some instances
# of a species belonging to more than one habitat (only occurs with Grassland, Wetland, Kopje)
# a predator preying on a species in both hab G and K will feed on this species
# like it's two species instead of one (for ex SID --> SID_K and SID_G)

habs <- tibble(read.table(paste("Data/serengeti_info_habs.csv",sep=''), sep=",", header=TRUE)) %>%
    select(code, habitat)

# Connect predators to their respective habitat (non-existing)
pred_links_h <- links_h %>% 
    select(code=pred_code) %>%
    left_join(., habs, by="code") %>%
    select(pred_code=code, pred_trait=habitat)
# Connect preys to their resp. habitat, and combine the resulting 
# data frame with the one for predators
links_groups_h <- links_h %>% 
    select(code=prey_code) %>%
    left_join(., habs, by="code") %>%
    select(prey_code=code, prey_trait=habitat) %>%
    bind_cols(pred_links_h,.)

# TRAITS
# to find fraction of links going from consumers to plant groups
links_t <- tibble(read.table(paste("Data/serengeti_links.csv",sep=''), sep=",", header=TRUE)) %>%
    mutate_if(is.factor, as.character)
traits <- as_tibble(read.table("Data/All_traits_r.txt", header=TRUE, sep="\t")) %>%
    select(code, growth_form)

# Connect predators to their respective group
pred_links_t <- links_t %>% 
    select(code=pred_code) %>%
    left_join(., traits, by="code") %>%
    select(pred_code=code, pred_trait=growth_form)
# Connect preys to their resp. group, and combine the resulting 
# data frame with the one for predators
links_groups_t <- links_t %>% 
    select(code=prey_code) %>%
    left_join(., traits, by="code") %>%
    select(prey_code=code, prey_trait=growth_form) %>%
    bind_cols(pred_links_t,.)

## GROUPS
# to find fraction of links going from consumers to plant groups
links_g <- tibble(read.table(paste("Data/serengeti_links.csv",sep=''), sep=",", header=TRUE)) %>%
    mutate_if(is.factor, as.character)
groups <- read_csv(paste("Data/serengeti_groups.csv",sep=''), col_types="ci") 

# Connect predators to their respective group
pred_links_g <- links_g %>% 
    select(code=pred_code) %>%
    left_join(., groups, by="code") %>%
    select(pred_code=code, pred_trait=group) %>%
    mutate(pred_trait=as.character(pred_trait))
# Connect preys to their resp. group, and combine the resulting 
# data frame with the one for predators
links_groups_g <- links_g %>% 
    select(code=prey_code) %>%
    left_join(., groups, by="code") %>%
    select(prey_code=code, prey_trait=group) %>%
    mutate(prey_trait=as.character(prey_trait)) %>%
    bind_cols(pred_links_g,.) %>% 
    slice(-57) 

all <- links_groups_h %>%
    bind_rows(., links_groups_t, links_groups_g)


all %>%
    mutate(prey = 1) %>%
    group_by(pred_code, prey_trait) %>%
    mutate(preys_group = sum(prey)) %>%
    ungroup() %>%
    select(pred_code, prey_trait, preys_group) %>%
    distinct() %>%
    arrange(pred_code) %>%
    group_by(pred_code) %>%
    mutate(sum_preys = sum(preys_group)) %>%
    ungroup() %>%
    mutate(groups = 1) %>%
    group_by(pred_code) %>%
    mutate(no_groups = sum(groups)) %>%
    ungroup() %>%
    select(-groups) #%>%
    #filter(pred_code=="CANMES")

    
    # ggplot() +
    # aes(x=pred_code, y=no_groups) +
    # geom_point() +
    # theme(axis.text.x = element_text(angle = 90, size=7)) 





