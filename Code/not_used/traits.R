require(tidyverse)
require(dplyr)

traitdata <- read.table("sp_traits.txt", header = TRUE, sep="\t")
tr_data <- traitdata %>%
  discard(~all(is.na(.x))) 
  

numeric_cols <- 3:173
tr_data[numeric_cols] <- as.integer(tr_data[numeric_cols] != 0)
rowSums(tr_data[numeric_cols])



# TRAIT 42 - plant form
MyID <- read.table("Data/AccSpID_alt.txt", header=TRUE, sep="\t") 
data <- read.table("Data/6727/6727.txt", header = TRUE, sep="\t")
tab <- data %>%
    select(AccSpeciesID, AccSpeciesName, TraitID, TraitName, OrigValueStr) %>%
    filter(TraitID==42) 
    
compl42 <- left_join(MyID,tab, by="AccSpeciesID")
View(compl42)
write.csv(compl42, "trait42.csv")

# TRAIT 343 - Raunkiaer plant form
MyID <- read.table("Data/AccSpID_alt.txt", header=TRUE, sep="\t") 
data <- read.table("Data/6727/6727.txt", header = TRUE, sep="\t")
tab <- data %>%
    select(AccSpeciesID, AccSpeciesName, TraitID, TraitName, OrigValueStr) %>%
    filter(TraitID==343) 

compl343<- left_join(MyID,tab, by="AccSpeciesID")
View(compl343)
write.csv(compl343, "test.csv")
# Which rows to remove? In original data set, we have Abutilon sp. I only need
# one Abutilon, data consists of multiple. First, I remove those with NA in trait value,
# and excessive so I only have one sp. in Abultilon.