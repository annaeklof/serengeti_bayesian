require(XRJulia) ## package allowing for Julia interfacing
require(tidyverse) ## for efficient data manipulation and plotting
require(igraph)
library(dplyr)
save_path <- "/Users/annak52/Documents/Serengeti/Results/original/"
Extension <- ".RData"
source("BN-DFS.R") ## to get prey average TL

juliaUsing("Random, Distributions")

AdjMatrix <- function(links) {
    ## Create adjacency matrix A; links point from resource to consumer, so
    ## the two columns of the edge list data frame are flipped
    A <- graph_from_data_frame(d=links[,2:1]) %>% ## create igraph graph
        as_adjacency_matrix %>% ## convert to igraph adjacency matrix
        as.matrix ## convert to a regular matrix
    ## get producers in the first rows
    A <- A[order(colSums(A)),order(colSums(A))]
    return(A)
}

## Monte Carlo evaluation of Bayesian network, written in Julia
bayes_ntw_MC_jul <- juliaEval("
                              function bayesianNetworkMonteCarlo(A, pBaseline, a, b, reps)
                              sA = sum(A, dims = 2)
                              nRows = length(sA)
                              lastBasal = maximum(findall(sA .== minimum(sA)))[1][1]
                              marginals = zeros(nRows)
                              for rep in 1:1:reps
                              extant = ones(nRows)
                              for i in 1:1:lastBasal
                              extant[i] = rand() < pBaseline[i] ? 0 : 1
                              end
                              for i in (lastBasal + 1):1:size(A, 1)
                              frac = 1 - (extant' * A[i,:])[1] / sum(A[i,:])
                              pExt = pBaseline[i] + (1 - pBaseline[i]) * cdf(Beta(a, b), frac)
                              extant[i] = rand() < pExt ? 0 : 1
                              end
                              marginals = marginals + extant
                              end
                              return(vec(marginals) / reps)
                              end
                              ")

## turn the above Julia code to an R function
bayes_ntw_MC <- JuliaFunction(bayes_ntw_MC_jul)

# read in the network, create adjacency matrix from links.
# make the adjacency matrix lower triangular
links <- read.table(paste("Data/serengeti_links.csv",sep=''), sep=",", header=TRUE)
A <- AdjMatrix(links) 
S <- dim(A)[1]
#image(t(apply(A,2,rev)))
A <- DFS(A)
o <- graph_from_adjacency_matrix(A) %>% topo_sort(mode="out")
A <- A[o,o]
A <- t(A) # for Julia, lower triangular

# create data frame with species names (code) and give all species a number,
# note, the same number as the row names in the adjacency matrix
df <- tibble(
    code=rownames(A),
    sp_no=1:dim(A)[1]
)
# read in group belonging and traits for each species
# combine with df 
groups <- left_join(df,read_csv(paste("Data/serengeti_groups.csv",sep=''), col_types="ci"), by="code") %>%
    left_join(.,as_tibble(read.table("Data/All_traits_r.txt", header=TRUE, sep="\t")), by="code")%>%
    select(-Species) 

appMarg <- function(alpha, beta) {
    if (alpha==1) funcresp="/Linear"
    else funcresp="/A3B3"
    ## the vector charac specifies all possible disturbances, including:
    ## habitat groups (grassland (G), woodland(W), shrubland(S), kopje(K), 
    ## disturbed(D), Thicket(T), riverland(R)). Plant functional groups (Baskerville)
    ## 7-14, woodiness, cycle, growth form.
    charac <- c("control","S","D","T","R", "missing", "G","W","K", 7:14, "Woody", 
                "NWoody", "P", "A", "tree", "shrub", "herb", "graminoid", "annual")
    for (i in 1:length(charac)){
        pl_charac <- charac[i]
        print(pl_charac)
        # need an if-statement when plant charac is equal to habitats (note, not 
        # necessary for the other plant characs), since some species occur in more 
        # than one habitat. If a species belongs to two hab, it will be assigned 
        # each hab in order. this holds for hab Grassland, Woodland, Kopje (i>5&i>9).
        if (i < 7){
            all <- tibble(read.table(paste("Data/serengeti_info.csv",sep=''), sep=",", header=TRUE)) %>%
                select(-group) %>%
            left_join(., groups, by="code")
        } 
        if (i>6 & i<10){
            all <- tibble(read.table(paste("Data/serengeti_info_", pl_charac, ".csv",sep=''), 
                                            sep=",", header=TRUE)) %>%
                select(-group) %>%
                left_join(., groups, by="code")
        }   
        
        Pb <- rep(0.1, S) ## baseline extinction probabilities: same for each species
        
        # if not control, create vector of species going to be disturbed (HAB) 
        if (pl_charac!="control" & i<10){ 
            per_vec <- all %>%
                filter(habitat==pl_charac) %>%
                pull(sp_no)
        } 
        # if not control, create vector of species going to be disturbed (TRAITS) 
        if (pl_charac!="control" & i>9){
            per_vec <- all %>%
                gather(trait, trait_val, group, woodiness, cycle, growth_form) %>%
                filter(trait_val==pl_charac) %>% 
                pull(sp_no)
        }
        if (pl_charac!="control"){
            Pb[per_vec] <- 0.75
        }
        
        persistence <- tibble(
            code=rownames(A), ## species numbers
            per = bayes_ntw_MC(A, Pb, alpha, beta, 100000) %>% ## 100k evals of BN
                juliaGet ## convert vector from Julia to R format
        )  
        persistence <- left_join(groups,persistence, by="code") %>%
            mutate(habitat_higher_pi=pl_charac)
        save(persistence, file=paste0(save_path,funcresp,"/higherPi75/Higher_",pl_charac,Extension))
    }
}


appMarg(alpha=3, beta=3)
