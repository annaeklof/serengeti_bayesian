# CALCULATE FRACTION OF LINKS

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
frac_links_pg <- links_groups %>%
    filter(pred_trait %in% c("group_3", "group_4", "group_5", "group_6")) %>%
    mutate(pred_code = 1, prey_code=1) %>%
    group_by(prey_trait) %>%
    mutate(counts = sum(prey_code)) %>%
    ungroup() %>%
    distinct(.) %>%
    select(prey_trait, counts) %>%
    distinct(.) %>%
    mutate(all_links = sum(counts)) %>%
    mutate(frac=round((counts/all_links),3))

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
frac_links_gr <- links_groups %>%
    filter(pred_group %in% c(3:6)) %>%
    mutate(pred_code = 1, prey_code=1) %>%
    group_by(prey_group) %>%
    mutate(counts = sum(prey_code)) %>%
    ungroup() %>%
    distinct(.) %>%
    select(prey_group, counts) %>%
    distinct(.) %>%
    mutate(all_links = sum(counts)) %>%
    mutate(frac=round((counts/all_links),3))

## HABITATS
# to find fraction of links going from consumers to plant groups
links <- tibble(read.table(paste("Data/serengeti_links_habs.csv",sep=''), sep=";", header=TRUE)) %>%
    mutate_if(is.factor, as.character)

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
frac_links_hab <- links_groups %>%
    filter(pred_trait %in% c("small_herbi", "large_grazer", "mega_grazer", "hyrax")) %>%
    mutate(pred_code = 1, prey_code=1) %>%
    group_by(prey_trait) %>%
    mutate(counts = sum(prey_code)) %>%
    ungroup() %>%
    distinct(.) %>%
    select(prey_trait, counts) %>%
    distinct(.) %>%
    mutate(all_links = sum(counts)) %>%
    mutate(frac=round((counts/all_links),3))
