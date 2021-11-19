# code for determining no of identical species in two groups
grami<- traits %>% filter(growth_form=="graminoid") %>% select(code)
grassland <- habs %>% filter(habitat=="G") %>% select(code)
intersect(grami, grassland)

# which species is found in both grassland and grasses?
# 24 species in total. Grassland total of 46 species, 
# grasses total of 55 species. So they share app 50% species, not very much. 