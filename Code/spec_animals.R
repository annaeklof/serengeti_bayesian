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

ha <- c("S","D","T","R","missing","G","W","K", "large_carni", "small_carni", 
        "small_herbi", "large_grazer", "hyrax", "mega_grazer")

gr <- c(1:14)

tr <- c("tree", "shrub", "herb", "graminoid", "annual", "group_1", "group_2",
        "group_3", "group_4", "group_5", "group_6")

test <- all %>%
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
    #mutate(hab_type=case_when(prey_trait%in%ha~"habs", prey_trait%in%gr~"groups",
    #                      prey_trait%in%tr~"traits")) %>%
    mutate(groups = 1) %>%
    group_by(pred_code) %>%
    mutate(no_groups = sum(groups)) %>%
    ungroup() %>%
    select(-groups) %>%
    left_join(ranking_sub, ., by="pred_code") %>%
    filter(pred_code %in% c("PEDCAP", "DAMKOR", "PANPAR", "TRASCR", "ACIJUB")) %>%
    mutate(prey_trait = case_when(
        prey_trait=="shrub" ~ "shrubs",
        prey_trait== "tree" ~ "trees",
        prey_trait=="herb" ~ "herbs",
        prey_trait=="graminoid" ~ "grasses",
        prey_trait=="annual" ~ "annuals",
        prey_trait=="7" ~ "group 7",
        prey_trait=="8" ~ "group 8",
        prey_trait=="9" ~ "group 9",
        prey_trait=="10" ~ "group 10",
        prey_trait=="11" ~ "group 11",
        prey_trait=="12" ~ "group 12",
        prey_trait=="13" ~ "group 13",
        prey_trait=="14" ~ "group 14",
        prey_trait=="D" ~ "disturbed",
        prey_trait=="T" ~ "thicket",
        prey_trait=="S" ~ "shrubland",
        prey_trait=="R" ~ "riverland",
        prey_trait=="missing" ~ "missing",
        prey_trait=="W" ~ "wetland",
        prey_trait=="K" ~ "kopje",
        prey_trait=="G" ~ "grassland",
        prey_trait=="large_carni"~ "large carnivores",
        prey_trait=="small_carni"~ "small carnivores",
        prey_trait=="small_herbi"~ "small herbivores",
        prey_trait=="large_grazer"~ "large grazers",
        prey_trait=="mega_grazer"~ "mixed grazers",
        prey_trait=="hyrax"~ "hyrax",
        prey_trait=="group_1"~ "large carnivores",
        prey_trait=="group_2"~ "small carnivores",
        prey_trait=="group_3"~ "small herbivores",
        prey_trait=="group_4"~ "large herbivores",
        prey_trait=="group_5"~ "hyrax",
        prey_trait=="group_6"~ "mixed grazers",
        prey_trait=="1"~ "large carnivores",
        prey_trait=="2"~ "small carnivores",
        prey_trait=="3"~ "small herbivores",
        prey_trait=="4"~ "large herbivores",
        prey_trait=="5"~ "hyrax",
        prey_trait=="6"~ "mixed grazers",
        TRUE ~ prey_trait
    )) 
    mutate(prey_trait = fct_relevel(prey_trait, "disturbed",
                                    "grassland", "kopje", "missing",
                                    "riverland", "shrubland", "thicket", "wetland",
                                    "shrubs",
                                    "group 7", "group 8", "group 9","group 10",
                                    "group 11","group 12","group 13","group 14",
                                    "trees", "grasses","herbs","annuals",
                                    "large carnivores", "small carnivores",
                                    "small herbivores", "large herbivores",
                                    "hyrax", "mixed grazers"
    )) 




test %>% ggplot() +
    aes(x=no_groups, y=sum_preys, color= rank_sum/max(rank_sum), shape=factor(type)) +
    geom_point(size=4) +
    #geom_line(aes(group=pred_code), linetype="dotted", size=1) +
    #facet_wrap(~hab_type) +
    geom_text(aes(label=ifelse(rank_sum>221,as.character(pred_code),'')),hjust=0,vjust=0, size=3) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 0, size=12)) +
    theme(axis.text.y = element_text(angle = 0, size=12)) +
    sc +
    #geom_smooth(method='lm', formula= y~x, size = 0.5, colour = "black") +
    #scale_shape_manual(name="Consumer type") +
    xlab("Number of groups") + 
    ylab("Number of links") +
    guides(shape = guide_legend(""))
ggsave(paste0("../Results/Figures/", funcresp, "/rank_all.jpg"), width = 8, height = 5)

sc <- scale_colour_gradientn(colours = myPalette(10), limits=c(0.65,1), name="Normalized rank \n of persistence \n")
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

min(ranking_sub$rank_sum)
max(ranking_sub$rank_sum)

