# Michael Rowley
# 16 October 2019
# Comparing CACH and BCCH network metrics from 2012-2013 (irruption year) and 2017-2018 (non-irruption year)
# from both Great Marsh (GM; both years) and Nolde Forest (NF; just irruption year) field sites

data("group_by_individual")
data("times")# subset GBI (to reduce run time of the example)
gbi <- gbi[,1:80]
times


## load packages
install.packages(c("asnipe", "igraph", "sna", "stats", "broom", "ggplot2", "ggsignif", "plotrix", "lme4", "MCMCglmm", "ramify"), dependencies = TRUE)
library(asnipe)
library(igraph)
library(sna)
library(stats)
library(broom)
library(ggplot2)
library(ggsignif)
library(plotrix)
library(lme4)
library(MCMCglmm)
library(ramify)
# setwd("~/Dropbox/Currylab_shared_not_Filemaker/Rowley_SNA/R_code/Fortnight_sna")
# setwd("C:/Users/Michael Rowley/Dropbox/Currylab_shared_not_Filemaker/Rowley_SNA/R_code/Fortnight_sna") # MR's old laptop
# setwd("C:/Users/mrowl/OneDrive - Villanova University/Research/FM_exports") # Using Onedrive
setwd("C:/Users/mrowl/Documents/Research/Villanova/FM_exports") # MR's new laptop

## Import the files
GM2012 <- read.csv(file = "GM_2012-2013_ALL.csv", header = TRUE, sep = ",")
NF2012 <- read.csv(file = "NF_2012-2013_ALL.csv", header = TRUE, sep = ",")

#### Set up GM2012 data.frame ####

G12_first_study_day <- 274

GM2012$Study_day <- ifelse((GM2012$Date < G12_first_study_day), (GM2012$Date - (G12_first_study_day - 1) + 366), (GM2012$Date - (G12_first_study_day - 1))) # Creates a col of numbers representing the day of the study
num_Study_days <- max(GM2012$Study_day) # total number of days in the study
time_break <- 14 # time break is 14 here because we want fortnight-sized networks

# GM2012$Fortnight <- ceiling(GM2012$Study_day / time_break) # Do this if your study period divides perfectly into time_break lengths (i.e., no remainders)
GM2012$Fortnight <- ifelse ((GM2012$Study_day > 182), (floor(GM2012$Study_day / time_break)), (ceiling(GM2012$Study_day / time_break)))

## create a new field on file for location + date
GM2012$Loc_date <- paste(GM2012$Location, GM2012$Study_day, sep = "_")



#### Set up NF2012 data.frame ####

NF12_first_study_day <- 326

NF2012$Study_day <- ifelse((NF2012$Date < NF12_first_study_day), (NF2012$Date - (NF12_first_study_day - 1) + 366), (NF2012$Date - (NF12_first_study_day - 1))) # Creates a col of numbers representing the day of the study
num_Study_days <- max(NF2012$Study_day) # total number of days in the study
time_break <- 12 # time break is 14 here because we want fortnight-sized networks

# NF2012$Fortnight <- ceiling(NF2012$Study_day / time_break) # Do this if your study period divides perfectly into time_break lengths (i.e., no remainders)
NF2012$Fortnight <- ifelse ((NF2012$Study_day > 168), (floor(NF2012$Study_day / time_break)), (ceiling(NF2012$Study_day / time_break)))

## create a new field on file for location + date
NF2012$Loc_date <- paste(NF2012$Location, NF2012$Study_day, sep = "_")

#### create new dataframes (both BCCH and CACH in GM 2012-2013) broken into time periods ####

# GM2012_fortnights <- data.frame("Fortnight" = 1:(num_Study_days %/% time_break), "GMM" = NA, "GBI" = NA, "Events" = NA, "Networks" = NA, "Graph" = NA)

GM2012_fortnight_1 <- GM2012[GM2012$Fortnight == 1,]
GM2012_fortnight_2 <- GM2012[GM2012$Fortnight == 2,]
GM2012_fortnight_3 <- GM2012[GM2012$Fortnight == 3,]
GM2012_fortnight_4 <- GM2012[GM2012$Fortnight == 4,]
GM2012_fortnight_5 <- GM2012[GM2012$Fortnight == 5,]
GM2012_fortnight_6 <- GM2012[GM2012$Fortnight == 6,]
GM2012_fortnight_7 <- GM2012[GM2012$Fortnight == 7,]
GM2012_fortnight_8 <- GM2012[GM2012$Fortnight == 8,]
GM2012_fortnight_9 <- GM2012[GM2012$Fortnight == 9,]
GM2012_fortnight_10 <- GM2012[GM2012$Fortnight == 10,]
GM2012_fortnight_11 <- GM2012[GM2012$Fortnight == 11,]
GM2012_fortnight_12 <- GM2012[GM2012$Fortnight == 12,]
GM2012_fortnight_13 <- GM2012[GM2012$Fortnight == 13,]

# apply a Gaussian Mixture Model (GMM) to each data frame and write it to it's own file
# setwd("~/Dropbox/Currylab_shared_not_Filemaker/Rowley_SNA/R_code/Fortnight_sna/Fortnight_gmms/GM2012_all")
# setwd("C:/Users/Michael Rowley/Dropbox/Currylab_shared_not_Filemaker/Rowley_SNA/R_code/Fortnight_sna/Fortnight_gmms/GM2012_all") # MR's laptop



setwd("~/Research/Villanova/Social network analysis/Thesis_SNA/GM_2012-2018/GM_GBI_matrices")

# GMM functions take a long time to run, so if their resulting GBI tables are already stored, just read them in without re-running the GMM. 

# GM2012_gmm1 <- gmmevents(time = GM2012_fortnight_1$Time, identity = GM2012_fortnight_1$Identity, location = GM2012_fortnight_1$Loc_date)
# # save all of the GMM products (because it would suck to lose them and have to run the long function again)
# write.csv(GM2012_gmm1$gbi, file = "GM2012_gbi1.csv", row.names = FALSE)
# 
# GM2012_gmm2 <- gmmevents(time = GM2012_fortnight_2$Time, identity = GM2012_fortnight_2$Identity, location = GM2012_fortnight_2$Loc_date)
# write.csv(GM2012_gmm2$gbi, file = "GM2012_gbi2.csv", row.names = FALSE)
# 
# GM2012_gmm3 <- gmmevents(time = GM2012_fortnight_3$Time, identity = GM2012_fortnight_3$Identity, location = GM2012_fortnight_3$Loc_date)
# write.csv(GM2012_gmm3$gbi, file = "GM2012_gbi3.csv", row.names = FALSE)
# 
# GM2012_gmm4 <- gmmevents(time = GM2012_fortnight_4$Time, identity = GM2012_fortnight_4$Identity, location = GM2012_fortnight_4$Loc_date)
# write.csv(GM2012_gmm4$gbi, file = "GM2012_gbi4.csv", row.names = FALSE)
# 
# GM2012_gmm5 <- gmmevents(time = GM2012_fortnight_5$Time, identity = GM2012_fortnight_5$Identity, location = GM2012_fortnight_5$Loc_date)
# write.csv(GM2012_gmm5$gbi, file = "GM2012_gbi5.csv", row.names = FALSE)
# 
# GM2012_gmm6 <- gmmevents(time = GM2012_fortnight_6$Time, identity = GM2012_fortnight_6$Identity, location = GM2012_fortnight_6$Loc_date)
# write.csv(GM2012_gmm6$gbi, file = "GM2012_gbi6.csv", row.names = FALSE)
# 
# GM2012_gmm7 <- gmmevents(time = GM2012_fortnight_7$Time, identity = GM2012_fortnight_7$Identity, location = GM2012_fortnight_7$Loc_date)
# write.csv(GM2012_gmm7$gbi, file = "GM2012_gbi7.csv", row.names = FALSE)
# 
# GM2012_gmm8 <- gmmevents(time = GM2012_fortnight_8$Time, identity = GM2012_fortnight_8$Identity, location = GM2012_fortnight_8$Loc_date)
# write.csv(GM2012_gmm8$gbi, file = "GM2012_gbi8.csv", row.names = FALSE)
# 
# GM2012_gmm9 <- gmmevents(time = GM2012_fortnight_9$Time, identity = GM2012_fortnight_9$Identity, location = GM2012_fortnight_9$Loc_date)
# write.csv(GM2012_gmm9$gbi, file = "GM2012_gbi9.csv", row.names = FALSE)
# 
# GM2012_gmm10 <- gmmevents(time = GM2012_fortnight_10$Time, identity = GM2012_fortnight_10$Identity, location = GM2012_fortnight_10$Loc_date)
# write.csv(GM2012_gmm10$gbi, file = "GM2012_gbi10.csv", row.names = FALSE)
# 
# GM2012_gmm11 <- gmmevents(time = GM2012_fortnight_11$Time, identity = GM2012_fortnight_11$Identity, location = GM2012_fortnight_11$Loc_date)
# write.csv(GM2012_gmm11$gbi, file = "GM2012_gbi11.csv", row.names = FALSE)
# 
# GM2012_gmm12 <- gmmevents(time = GM2012_fortnight_12$Time, identity = GM2012_fortnight_12$Identity, location = GM2012_fortnight_12$Loc_date)
# write.csv(GM2012_gmm12$gbi, file = "GM2012_gbi12.csv", row.names = FALSE)
# 
# GM2012_gmm13 <- gmmevents(time = GM2012_fortnight_13$Time, identity = GM2012_fortnight_13$Identity, location = GM2012_fortnight_13$Loc_date)
# write.csv(GM2012_gmm13$gbi, file = "GM2012_gbi13.csv", row.names = FALSE)


# After running gbis for the first time and saving them to csvs, you can read them into their variables again
GM2012_gbi1 <- read.csv("GM2012_gbi1.csv", header = TRUE, check.names = FALSE)
GM2012_gbi2 <- read.csv("GM2012_gbi2.csv", header = TRUE, check.names = FALSE)
GM2012_gbi3 <- read.csv("GM2012_gbi3.csv", header = TRUE, check.names = FALSE)
GM2012_gbi4 <- read.csv("GM2012_gbi4.csv", header = TRUE, check.names = FALSE)
GM2012_gbi5 <- read.csv("GM2012_gbi5.csv", header = TRUE, check.names = FALSE)
GM2012_gbi6 <- read.csv("GM2012_gbi6.csv", header = TRUE, check.names = FALSE)
GM2012_gbi7 <- read.csv("GM2012_gbi7.csv", header = TRUE, check.names = FALSE)
GM2012_gbi8 <- read.csv("GM2012_gbi8.csv", header = TRUE, check.names = FALSE)
GM2012_gbi9 <- read.csv("GM2012_gbi9.csv", header = TRUE, check.names = FALSE)
GM2012_gbi10 <- read.csv("GM2012_gbi10.csv", header = TRUE, check.names = FALSE)
GM2012_gbi11 <- read.csv("GM2012_gbi11.csv", header = TRUE, check.names = FALSE)
GM2012_gbi12 <- read.csv("GM2012_gbi12.csv", header = TRUE, check.names = FALSE)
GM2012_gbi13 <- read.csv("GM2012_gbi13.csv", header = TRUE, check.names = FALSE)

# Create a list that contains all of these variables (so that we can use for loops to iterate through them)
GM2012_gbis <- list(GM2012_gbi1, GM2012_gbi2, GM2012_gbi3, GM2012_gbi4, GM2012_gbi5, GM2012_gbi6, GM2012_gbi7, 
                    GM2012_gbi8, GM2012_gbi9, GM2012_gbi10, GM2012_gbi11, GM2012_gbi12, GM2012_gbi13)



#### create new dataframes (both BCCH and CACH from NF 2012-13) broken into time periods ####

# NF2012_fortnights <- data.frame("Fortnight" = 1:(num_Study_days %/% time_break), "GMM" = NA, "GBI" = NA, "Events" = NA, "Networks" = NA, "Graph" = NA)

NF2012_fortnight_1 <- NF2012[NF2012$Fortnight == 1,]
NF2012_fortnight_2 <- NF2012[NF2012$Fortnight == 2,]
NF2012_fortnight_3 <- NF2012[NF2012$Fortnight == 3,]
NF2012_fortnight_4 <- NF2012[NF2012$Fortnight == 4,]
NF2012_fortnight_5 <- NF2012[NF2012$Fortnight == 5,]
NF2012_fortnight_6 <- NF2012[NF2012$Fortnight == 6,]
NF2012_fortnight_7 <- NF2012[NF2012$Fortnight == 7,]
NF2012_fortnight_8 <- NF2012[NF2012$Fortnight == 8,]
NF2012_fortnight_9 <- NF2012[NF2012$Fortnight == 9,]
NF2012_fortnight_10 <- NF2012[NF2012$Fortnight == 10,]
NF2012_fortnight_11 <- NF2012[((NF2012$Fortnight == 11) | (NF2012$Fortnight == 12)),]
# NF2012_fortnight_12 <- NF2012[NF2012$Fortnight == 12,] # only two individuals in the 12th time period, so I merged it

# apply a Gaussian Mixture Model (GMM) to each data frame and write it to it's own file
# setwd("~/Dropbox/Currylab_shared_not_Filemaker/Rowley_SNA/R_code/Fortnight_sna/Fortnight_gmms/NF2012_all")
# setwd("C:/Users/Michael Rowley/Dropbox/Currylab_shared_not_Filemaker/Rowley_SNA/R_code/Fortnight_sna/Fortnight_gmms/NF2012_all") # MR's laptop



setwd("~/Research/Villanova/Social network analysis/Thesis_SNA/NF_2012-2013")

# GMM functions take a long time to run, so if their resulting GBI tables are already stored, just read them in without re-running the GMM.

# NF2012_gmm1 <- gmmevents(time = NF2012_fortnight_1$Time, identity = NF2012_fortnight_1$Identity, location = NF2012_fortnight_1$Loc_date)
# # save all of the GMM products (because it would suck to lose them and have to run the long function again)
# write.csv(NF2012_gmm1$gbi, file = "NF2012_gbi1.csv", row.names = FALSE)
# 
# NF2012_gmm2 <- gmmevents(time = NF2012_fortnight_2$Time, identity = NF2012_fortnight_2$Identity, location = NF2012_fortnight_2$Loc_date)
# write.csv(NF2012_gmm2$gbi, file = "NF2012_gbi2.csv", row.names = FALSE)
# 
# NF2012_gmm3 <- gmmevents(time = NF2012_fortnight_3$Time, identity = NF2012_fortnight_3$Identity, location = NF2012_fortnight_3$Loc_date)
# write.csv(NF2012_gmm3$gbi, file = "NF2012_gbi3.csv", row.names = FALSE)
# 
# NF2012_gmm4 <- gmmevents(time = NF2012_fortnight_4$Time, identity = NF2012_fortnight_4$Identity, location = NF2012_fortnight_4$Loc_date)
# write.csv(NF2012_gmm4$gbi, file = "NF2012_gbi4.csv", row.names = FALSE)
# 
# NF2012_gmm5 <- gmmevents(time = NF2012_fortnight_5$Time, identity = NF2012_fortnight_5$Identity, location = NF2012_fortnight_5$Loc_date)
# write.csv(NF2012_gmm5$gbi, file = "NF2012_gbi5.csv", row.names = FALSE)
# 
# NF2012_gmm6 <- gmmevents(time = NF2012_fortnight_6$Time, identity = NF2012_fortnight_6$Identity, location = NF2012_fortnight_6$Loc_date)
# write.csv(NF2012_gmm6$gbi, file = "NF2012_gbi6.csv", row.names = FALSE)
# 
# NF2012_gmm7 <- gmmevents(time = NF2012_fortnight_7$Time, identity = NF2012_fortnight_7$Identity, location = NF2012_fortnight_7$Loc_date)
# write.csv(NF2012_gmm7$gbi, file = "NF2012_gbi7.csv", row.names = FALSE)
# 
# NF2012_gmm8 <- gmmevents(time = NF2012_fortnight_8$Time, identity = NF2012_fortnight_8$Identity, location = NF2012_fortnight_8$Loc_date)
# write.csv(NF2012_gmm8$gbi, file = "NF2012_gbi8.csv", row.names = FALSE)
# 
# NF2012_gmm9 <- gmmevents(time = NF2012_fortnight_9$Time, identity = NF2012_fortnight_9$Identity, location = NF2012_fortnight_9$Loc_date)
# write.csv(NF2012_gmm9$gbi, file = "NF2012_gbi9.csv", row.names = FALSE)
# 
# NF2012_gmm10 <- gmmevents(time = NF2012_fortnight_10$Time, identity = NF2012_fortnight_10$Identity, location = NF2012_fortnight_10$Loc_date)
# write.csv(NF2012_gmm10$gbi, file = "NF2012_gbi10.csv", row.names = FALSE)
# 
# NF2012_gmm11 <- gmmevents(time = NF2012_fortnight_11$Time, identity = NF2012_fortnight_11$Identity, location = NF2012_fortnight_11$Loc_date)
# write.csv(NF2012_gmm11$gbi, file = "NF2012_gbi11.csv", row.names = FALSE)


# After running gbis for the first time and saving them to csvs, you can read them into their variables again
NF2012_gbi1 <- read.csv("NF2012_gbi1.csv", header = TRUE, check.names = FALSE)
NF2012_gbi2 <- read.csv("NF2012_gbi2.csv", header = TRUE, check.names = FALSE)
NF2012_gbi3 <- read.csv("NF2012_gbi3.csv", header = TRUE, check.names = FALSE)
NF2012_gbi4 <- read.csv("NF2012_gbi4.csv", header = TRUE, check.names = FALSE)
NF2012_gbi5 <- read.csv("NF2012_gbi5.csv", header = TRUE, check.names = FALSE)
NF2012_gbi6 <- read.csv("NF2012_gbi6.csv", header = TRUE, check.names = FALSE)
NF2012_gbi7 <- read.csv("NF2012_gbi7.csv", header = TRUE, check.names = FALSE)
NF2012_gbi8 <- read.csv("NF2012_gbi8.csv", header = TRUE, check.names = FALSE)
NF2012_gbi9 <- read.csv("NF2012_gbi9.csv", header = TRUE, check.names = FALSE)
NF2012_gbi10 <- read.csv("NF2012_gbi10.csv", header = TRUE, check.names = FALSE)
NF2012_gbi11 <- read.csv("NF2012_gbi11.csv", header = TRUE, check.names = FALSE)


# Create a list that contains all of these variables (so that we can use for loops to iterate through them)
NF2012_gbis <- list(NF2012_gbi1, NF2012_gbi2, NF2012_gbi3, NF2012_gbi4, NF2012_gbi5, NF2012_gbi6, NF2012_gbi7, 
                    NF2012_gbi8, NF2012_gbi9, NF2012_gbi10, NF2012_gbi11)

#### GM 2012-2013: create GMM, extract GBI and events, build Network, and construct Graph for each fortnight ####
minVisits <- 5

setwd("~/Research/Villanova/Social network analysis/Thesis_SNA/GM_2012-2018")
GM2012_attr <- read.csv("GM2013_Attr.csv", header = TRUE, check.names = FALSE)

# Create graphs etc for GM2012 (all birds)


# GM2012_gbis <- vector("list", length = length(GM2012_gmms)) # list to hold gbi variables for each fortnight
# GM2012_events <- list(length = length(GM2012_gmms))
# GM2012_obs <- list(length = length(GM2012_gmms)
GM2012_networks <- vector("list", length = length(GM2012_gbis)) # list to hold all networks
GM2012_graphs <- vector("list", length = length(GM2012_gbis)) # list to hold all graphs

for (i in 1:length(GM2012_gbis)) {
  
  # GM2012_gbis[[i]] <- (GM2012_gbis[[i]])[,which(colSums(GM2012_gbis[[i]]) > minVisits)] # removes events for which there are fewer than in 'minVisits' (5 here)
  GM2012_networks[[i]] <- get_network(GM2012_gbis[[i]]) # gets network from GBI.. almost there
  GM2012_graphs[[i]] <- graph.adjacency(GM2012_networks[[i]], mode="undirected", weighted=TRUE, diag=FALSE) # gets graph from network (that igraph can handle)
  V(GM2012_graphs[[i]])$Species <- as.character(GM2012_attr$Species[match(V(GM2012_graphs[[i]])$name, GM2012_attr$ID)]) # adds a species attribute for each node with its corresponding species code
}

#### NF 2012-2013: create GMM, extract GBI and events, build Network, and construct Graph for each fortnight ####
minVisits <- 5

setwd("~/Research/Villanova/Social network analysis/Thesis_SNA/NF_2012-2013")
NF2012_attr <- read.csv("NF_2012-2013_Attr.csv", header = TRUE, check.names = FALSE)

# Create graphs etc for GM2012 (all birds)


# GM2012_gbis <- vector("list", length = length(GM2012_gmms)) # list to hold gbi variables for each fortnight
# GM2012_events <- list(length = length(GM2012_gmms))
# GM2012_obs <- list(length = length(GM2012_gmms)
NF2012_networks <- vector("list", length = length(NF2012_gbis)) # list to hold all networks
NF2012_graphs <- vector("list", length = length(NF2012_gbis)) # list to hold all graphs

for (i in 1:length(NF2012_gbis)) {
  
  # NF2012_gbis[[i]] <- (NF2012_gbis[[i]])[,which(colSums(NF2012_gbis[[i]]) > minVisits)] # removes events for which there are fewer than in 'minVisits' (5 here)
  NF2012_networks[[i]] <- get_network(NF2012_gbis[[i]]) # gets network from GBI.. almost there
  NF2012_graphs[[i]] <- graph.adjacency(NF2012_networks[[i]], mode="undirected", weighted=TRUE, diag=FALSE) # gets graph from network (that igraph can handle)
  V(NF2012_graphs[[i]])$Species <- as.character(NF2012_attr$Species[match(V(NF2012_graphs[[i]])$name, NF2012_attr$ID)]) # adds a species attribute for each node with its corresponding species code
  V(NF2012_graphs[[i]])$Sex <- as.character(NF2012_attr$Sex[match(V(NF2012_graphs[[i]])$name, NF2012_attr$ID)]) # adds a species attribute for each node with its corresponding species code
  
}

#### GM 2012-2013 EDGE ANALYSIS - creating a dataframe that we'll use to store each edge's weight values for each time period ####

setwd("~/Research/Villanova/Social network analysis/Thesis_SNA/GM_2012-2018")

GM2012_edges_df <- data.frame("head" = character(), "tail" = character(), "edges" = character(), "head.spp" = character(), "tail.spp" = character(), "t1.weight" = numeric(), "t2.weight" = numeric(), "t3.weight" = numeric(), "t4.weight" = numeric(), "t5.weight" = numeric(), "t6.weight" = numeric(), "t7.weight" = numeric(), "t8.weight" = numeric(), "t9.weight" = numeric(), "t10.weight" = numeric(), "t11.weight" = numeric(), "t12.weight" = numeric(), "t13.weight" = numeric())

# Creates a new row in that dataframe for each unique edge (non-directed)
for (j in 1:length(GM2012_graphs)) {
  
  for (i in 1:length(E(GM2012_graphs[[j]]))) {
    
    new_edge <- paste(as_edgelist(GM2012_graphs[[j]])[i,1], as_edgelist(GM2012_graphs[[j]])[i,2], sep = "--")
    
    
    if (new_edge %in% GM2012_edges_df$edges == FALSE) {
      
      new_row <- data.frame("head" = as_edgelist(GM2012_graphs[[j]])[i,1], "tail" = as_edgelist(GM2012_graphs[[j]])[i,2], "edges" = new_edge)
      new_row$head.spp = GM2012_attr$Species[match(new_row$head, GM2012_attr$ID)]
      new_row$tail.spp = GM2012_attr$Species[match(new_row$tail, GM2012_attr$ID)]  
      weights <- data.frame("t1.weight" = NA, "t2.weight" = NA, "t3.weight" = NA, "t4.weight" = NA, "t5.weight" = NA, "t6.weight" = NA, "t7.weight" = NA, "t8.weight" = NA, "t9.weight" = NA, "t10.weight" = NA, "t11.weight" = NA, "t12.weight" = NA, "t13.weight" = NA)
      new_row <- cbind(new_row, weights)
      GM2012_edges_df <- rbind(GM2012_edges_df, new_row)
    }
  }
}


# Populates weight fields for each time period with weight values
for (j in 1:length(GM2012_graphs)) {
  
  for (i in 1:length(E(GM2012_graphs[[j]]))) {
    
    new_edge <- paste(as_edgelist(GM2012_graphs[[j]])[i,1], as_edgelist(GM2012_graphs[[j]])[i,2], sep = "--")
    new_weight <- E(GM2012_graphs[[j]])[i]$weight
    
    GM2012_edges_df[which(GM2012_edges_df$edges == new_edge), (j + 5)] <- new_weight
  }
}

write.csv(GM2012_edges_df, file = "GM2012_edges.csv", row.names = FALSE) 
GM2012_edges_df <- read.csv(file = "GM2012_edges.csv", check.names = FALSE)



# plotting mean edge weight over time for each edge type

GM2012_edge_plot_df <- data.frame("time.period" = character(), "species" = character(), "weight" = numeric(), "se" = numeric())


for (j in 1:length(GM2012_graphs)) {
  
  CC_row <- data.frame("time.period" = j, 
                       "species" = "CC", 
                       "weight" = mean(as.numeric(GM2012_edges_df[which((GM2012_edges_df$head.spp == "CACH") & (GM2012_edges_df$tail.spp == "CACH")), j+5]), na.rm = TRUE), 
                       "se" = std.error(as.numeric(GM2012_edges_df[which((GM2012_edges_df$head.spp == "CACH") & (GM2012_edges_df$tail.spp == "CACH")), j+5]), na.rm = TRUE))
  
  BB_row <- data.frame("time.period" = j, 
                       "species" = "BB", 
                       "weight" = mean(as.numeric(GM2012_edges_df[which((GM2012_edges_df$head.spp == "BCCH") & (GM2012_edges_df$tail.spp == "BCCH")), j+5]), na.rm = TRUE), 
                       "se" = std.error(as.numeric(GM2012_edges_df[which((GM2012_edges_df$head.spp == "BCCH") & (GM2012_edges_df$tail.spp == "BCCH")), j+5]), na.rm = TRUE))
  
  CB_row <- data.frame("time.period" = j, 
                       "species" = "CB", 
                       "weight" = mean(as.numeric(GM2012_edges_df[which(GM2012_edges_df$head.spp != GM2012_edges_df$tail.spp), j+5]), na.rm = TRUE), 
                       "se" = std.error(as.numeric(GM2012_edges_df[which(GM2012_edges_df$head.spp != GM2012_edges_df$tail.spp), j+5]), na.rm = TRUE))
  
  GM2012_edge_plot_df <- rbind(GM2012_edge_plot_df, CC_row, BB_row, CB_row)
  
}

# *Figure* of edge weights +/- SE across all time periods

GM2012_edge_plot <- ggplot(data = GM2012_edge_plot_df, aes(x = time.period, y = weight, width = 0.2)) +
  scale_x_discrete(limits = factor(unique(GM2012_edge_plot_df$time.period))) +
  geom_line(aes(colour = species, linetype = species), 
            position = position_dodge(width = .5), size = 1.0) +
  geom_errorbar(aes(x = time.period, ymin = weight-se, ymax = weight+se, colour = species, linetype = species), 
                position = position_dodge(width = .5), size = 1.0,) +
  ylab("Weight \u00B1 SE") +
  xlab("Time Period") + 
  labs(color = "Edge type", linetype = "Edge type") +
  guides(color = guide_legend(reverse = TRUE), linetype = guide_legend(reverse = TRUE)) +
  theme_classic(base_size = 12) + 
  theme(legend.margin = margin(0,0,0,-2,"mm"))


time_periods <- c(1:length(GM2012_graphs)) # added this in case you don't need to run all the graph stuff again

GM2012_edge_glmm_df <- data.frame(edge = character(), spp = character(), time.period = numeric(), weight = numeric())

for (j in 1:nrow(GM2012_edges_df)) {

  for (i in 1:length(time_periods)) {

    new_row <- data.frame(edge = GM2012_edges_df$edges[j], spp = paste(GM2012_edges_df$head.spp[j], GM2012_edges_df$tail.spp[j], sep = "-"), time.period = i,  weight = GM2012_edges_df[j, i + 5])

    GM2012_edge_glmm_df <- rbind(GM2012_edge_glmm_df, new_row)

  }
}

GM2012_edge_glmm_df$spp[which(GM2012_edge_glmm_df$spp == "BCCH-CACH")] <- "CACH-BCCH" # so as not to split the heterospecific edges into two categories

write.csv(GM2012_edge_glmm_df, file = "GM2012_edge_glmm_df.csv", row.names = FALSE)

GM2012_edge_glmm_df <- read.csv(file = "GM2012_edge_glmm_df.csv", check.names = FALSE)


# force R to recognize CACH-CACH edges as reference level

GM2012_edge_glmm_df$spp <- factor(GM2012_edge_glmm_df$spp)
GM2012_edge_glmm_df <- within(GM2012_edge_glmm_df, spp <- relevel(spp, ref = "CACH-CACH"))


# trying some Bayesian stuff using package "MCMCglmm" and the Bayesian glmm function of the same name

prior_G12e = list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V = 1, nu = 0.002))) #uninformative prior


GM12e_MCMC7.1 <- MCMCglmm(fixed = weight ~ time.period*spp, random = ~ edge,  data = GM2012_edge_glmm_df, family = "exponential", prior = prior_G12e, nitt = 1005000, burnin = 5000, verbose = FALSE, thin = 800) # "full" model - very good output

summary(GM12e_MCMC7.1)
plot(GM12e_MCMC7.1$Sol)

(sd(GM12e_MCMC7.1$Sol) / mean(GM12e_MCMC7.1$Sol))

autocorr.diag(GM12e_MCMC7.1$Sol)
autocorr.diag(GM12e_MCMC7.1$VCV)

GM12e_CC_int <- mean(GM12e_MCMC7.1$Sol[,1])
GM12e_CB_int <- mean(GM12e_MCMC7.1$Sol[,3])
GM12e_BB_int <- mean(GM12e_MCMC7.1$Sol[,4])

GM12e_CC_change <- mean(GM12e_MCMC7.1$Sol[,2])
GM12e_CB_change <- mean(GM12e_MCMC7.1$Sol[,5])
GM12e_BB_change <- mean(GM12e_MCMC7.1$Sol[,6])

anova(GM12e_MCMC7.1)


#### NF 2012-2013 EDGE ANALYSIS - creating a dataframe that we'll use to store each edge's weight values for each time period ####
NF2012_edges_df <- data.frame("head" = character(), "tail" = character(), "head.sex" = character(), "tail.sex" = character(), "edges" = character(), "head.spp" = character(), "tail.spp" = character(), "t1.weight" = numeric(), "t2.weight" = numeric(), "t3.weight" = numeric(), "t4.weight" = numeric(), "t5.weight" = numeric(), "t6.weight" = numeric(), "t7.weight" = numeric(), "t8.weight" = numeric(), "t9.weight" = numeric(), "t10.weight" = numeric(), "t11.weight" = numeric())


# Creates a new row in that dataframe for each unique edge (non-directed)
for (j in 1:length(NF2012_graphs)) {
  
  for (i in 1:length(E(NF2012_graphs[[j]]))) {
    
    new_edge <- paste(as_edgelist(NF2012_graphs[[j]])[i,1], as_edgelist(NF2012_graphs[[j]])[i,2], sep = "--")
    
    
    if (new_edge %in% NF2012_edges_df$edges == FALSE) {
      
      new_row <- data.frame("head" = as_edgelist(NF2012_graphs[[j]])[i,1], "tail" = as_edgelist(NF2012_graphs[[j]])[i,2], "edges" = new_edge)
      new_row$head.sex = NF2012_attr$Sex[match(new_row$head, NF2012_attr$ID)]
      new_row$tail.sex = NF2012_attr$Sex[match(new_row$tail, NF2012_attr$ID)] 
      new_row$head.spp = NF2012_attr$Species[match(new_row$head, NF2012_attr$ID)]
      new_row$tail.spp = NF2012_attr$Species[match(new_row$tail, NF2012_attr$ID)]  
      weights <- data.frame("t1.weight" = NA, "t2.weight" = NA, "t3.weight" = NA, "t4.weight" = NA, "t5.weight" = NA, "t6.weight" = NA, "t7.weight" = NA, "t8.weight" = NA, "t9.weight" = NA, "t10.weight" = NA, "t11.weight" = NA)
      new_row <- cbind(new_row, weights)
      NF2012_edges_df <- rbind(NF2012_edges_df, new_row)
    }
  }
}


# Populates weight fields for each time period with weight values
for (j in 1:length(NF2012_graphs)) {
  
  for (i in 1:length(E(NF2012_graphs[[j]]))) {
    
    new_edge <- paste(as_edgelist(NF2012_graphs[[j]])[i,1], as_edgelist(NF2012_graphs[[j]])[i,2], sep = "--")
    new_weight <- E(NF2012_graphs[[j]])[i]$weight
    
    NF2012_edges_df[which(NF2012_edges_df$edges == new_edge), (j + 7)] <- new_weight
  }
}


# plotting mean edge weight over time for each edge type

NF2012_edge_plot_df <- data.frame("time.period" = character(), "species" = character(), "weight" = numeric(), "se" = numeric())


for (j in 1:length(NF2012_graphs)) {
  
  CC_row <- data.frame("time.period" = j, 
                       "species" = "CC", 
                       "weight" = mean(as.numeric(NF2012_edges_df[which((NF2012_edges_df$head.spp == "CACH") & (NF2012_edges_df$tail.spp == "CACH")), j+5]), na.rm = TRUE), 
                       "se" = std.error(as.numeric(NF2012_edges_df[which((NF2012_edges_df$head.spp == "CACH") & (NF2012_edges_df$tail.spp == "CACH")), j+5]), na.rm = TRUE))
  
  BB_row <- data.frame("time.period" = j, 
                       "species" = "BB", 
                       "weight" = mean(as.numeric(NF2012_edges_df[which((NF2012_edges_df$head.spp == "BCCH") & (NF2012_edges_df$tail.spp == "BCCH")), j+5]), na.rm = TRUE), 
                       "se" = std.error(as.numeric(NF2012_edges_df[which((NF2012_edges_df$head.spp == "BCCH") & (NF2012_edges_df$tail.spp == "BCCH")), j+5]), na.rm = TRUE))
  
  CB_row <- data.frame("time.period" = j, 
                       "species" = "CB", 
                       "weight" = mean(as.numeric(NF2012_edges_df[which(NF2012_edges_df$head.spp != NF2012_edges_df$tail.spp), j+5]), na.rm = TRUE), 
                       "se" = std.error(as.numeric(NF2012_edges_df[which(NF2012_edges_df$head.spp != NF2012_edges_df$tail.spp), j+5]), na.rm = TRUE))
  
  NF2012_edge_plot_df <- rbind(NF2012_edge_plot_df, CC_row, BB_row, CB_row)
  
}

# *Figure* of edge weights +/- SE across all time periods

NF2012_edge_plot <- ggplot(data = NF2012_edge_plot_df, aes(x = time.period, y = weight, width = 0.2)) +
  scale_x_discrete(limits = factor(unique(NF2012_edge_plot_df$time.period))) +
  geom_line(aes(colour = species, linetype = species),
            position = position_dodge(width = .5), size = 1.0) +
  geom_errorbar(aes(x = time.period, ymin = weight-se, ymax = weight+se, colour = species, linetype = species), 
                position = position_dodge(width = .5), size = 1.0) +
  ylab("Weight \u00B1 SE") +
  xlab("Time Period") +
  labs(color = "Edge type", linetype = "Edge type") +
  guides(color = guide_legend(reverse = TRUE), linetype = guide_legend(reverse = TRUE)) +
  theme_classic(base_size = 12) + 
  theme(legend.margin = margin(0,0,0,-2,"mm"))



## GLMM for edge weights (might work hopefully)

# organize data frame

time_periods <- c(1:length(NF2012_graphs)) # added this in case you don't need to run all the graph stuff again

NF2012_edge_glmm_df <- data.frame(edge = character(), spp = character(), sex = character(), time.period = numeric(), weight = numeric())

for (j in 1:nrow(NF2012_edges_df)) {
  
  for (i in 1:length(time_periods)) {
    
    new_row <- data.frame(edge = NF2012_edges_df$edges[j], spp = paste(NF2012_edges_df$head.spp[j], NF2012_edges_df$tail.spp[j], sep = "-"), sex = paste(NF2012_edges_df$head.sex[j], NF2012_edges_df$tail.sex[j], sep = "-"), time.period = i,  weight = NF2012_edges_df[j, i + 7])
    
    NF2012_edge_glmm_df <- rbind(NF2012_edge_glmm_df, new_row)
    
  }
}

NF2012_edge_glmm_df$spp[which(NF2012_edge_glmm_df$spp == "BCCH-CACH")] <- "CACH-BCCH" # so as not to split the heterospecific edges into two categories

NF2012_edge_glmm_df$spp[which(NF2012_edge_glmm_df$spp == "F-M")] <- "M-F" # so as not to split the heterospecific edges into two categories

write.csv(NF2012_edge_glmm_df, file = "NF2012_edge_glmm_df.csv", row.names = FALSE)

NF2012_edge_glmm_df <- read.csv(file = "NF2012_edge_glmm_df.csv", check.names = FALSE)


# force R to recognize CACH-CACH edges as reference level

NF2012_edge_glmm_df$spp <- factor(NF2012_edge_glmm_df$spp)

NF2012_edge_glmm_df <- within(NF2012_edge_glmm_df, spp <- relevel(spp, ref = "CACH-CACH"))


# using "MCMCglmm" and the Bayesian glmm function of the same name

prior_NF2012e = list(R = list(V = 1, nu = 0.002), 
                     G = list(G1 = list(V = 1, nu = 0.002))) #uninformative prior


NF2012e_MCMC7 <- MCMCglmm(fixed = weight ~ time.period*spp, random = ~ edge,  data = NF2012_edge_glmm_df, family = "exponential", prior = prior_NF2012e, nitt = 1005000, burnin = 5000, verbose = FALSE, thin = 800) # "full" model - very good output



summary(NF2012e_MCMC7)
plot(NF2012e_MCMC7$Sol)

autocorr.diag(NF2012e_MCMC7$Sol)
autocorr.diag(NF2012e_MCMC7$VCV)

NF2012e_CC_int <- mean(NF2012e_MCMC7$Sol[,1])
NF2012e_CB_int <- mean(NF2012e_MCMC7$Sol[,3])
NF2012e_BB_int <- mean(NF2012e_MCMC7$Sol[,4])

NF2012e_CC_change <- mean(NF2012e_MCMC7$Sol[,2])
NF2012e_CB_change <- mean(NF2012e_MCMC7$Sol[,5])
NF2012e_BB_change <- mean(NF2012e_MCMC7$Sol[,6])

hist(NF2012_edge_glmm_df$weight)

#### GM 2012-2013 VERTEX ANALYSIS - creating a dataframe that we'll use to store each edge's weight values for each time period ####

# Creating a new dataframe! in this case, d= degree, s = strength, nd = normalized degree, and ns = normalized strength
# 
GM2012_vertices_df <- data.frame( "ID" = character(), "Spp" = character(), "t1.d" = numeric(), "t2.d" = numeric(), "t3.d" = numeric(), "t4.d" = numeric(), "t5.d" = numeric(), "t6.d" = numeric(), "t7.d" = numeric(), "t8.d" = numeric(), "t9.d" = numeric(), "t10.d" = numeric(), "t11.d" = numeric(), "t12.d" = numeric(), "t13.d" = numeric(), "t1.s" = numeric(), "t2.s" = numeric(), "t3.s" = numeric(), "t4.s" = numeric(), "t5.s" = numeric(), "t6.s" = numeric(), "t7.s" = numeric(), "t8.s" = numeric(), "t9.s" = numeric(), "t10.s" = numeric(), "t11.s" = numeric(), "t12.s" = numeric(), "t13.s" = numeric(), "t1.nd" = numeric(), "t2.nd" = numeric(), "t3.nd" = numeric(), "t4.nd" = numeric(), "t5.nd" = numeric(), "t6.nd" = numeric(), "t7.nd" = numeric(), "t8.nd" = numeric(), "t9.nd" = numeric(), "t10.nd" = numeric(), "t11.nd" = numeric(), "t12.nd" = numeric(), "t13.nd" = numeric(), "t1.ns" = numeric(), "t2.ns" = numeric(), "t3.ns" = numeric(), "t4.ns" = numeric(), "t5.ns" = numeric(), "t6.ns" = numeric(), "t7.ns" = numeric(), "t8.ns" = numeric(), "t9.ns" = numeric(), "t10.ns" = numeric(), "t11.ns" = numeric(), "t12.ns" = numeric(), "t13.ns" = numeric())



# Creates a new row in that dataframe for each unique vertex
for (j in 1:length(GM2012_graphs)) {
  
  for (i in 1:length(V(GM2012_graphs[[j]]))) {
    
    if (V(GM2012_graphs[[j]])[i]$name %in% GM2012_vertices_df$ID == FALSE) {
      
      new_row <- data.frame("ID" = V(GM2012_graphs[[j]])[i]$name)
      new_row$head.spp = GM2012_attr$Species[match(new_row$ID, GM2012_attr$ID)]
      
      metrics <- data.frame("t1.d" = NA, "t2.d" = NA, "t3.d" = NA, "t4.d" = NA, "t5.d" = NA, "t6.d" = NA, "t7.d" = NA, "t8.d" = NA, "t9.d" = NA, "t10.d" = NA, "t11.d" = NA, "t12.d" = NA, "t13.d" = NA, "t1.s" = NA, "t2.s" = NA, "t3.s" = NA, "t4.s" = NA, "t5.s" = NA, "t6.s" = NA, "t7.s" = NA, "t8.s" = NA, "t9.s" = NA, "t10.s" = NA, "t11.s" = NA, "t12.s" = NA, "t13.s" = NA, "t1.nd" = NA, "t2.nd" = NA, "t3.nd" = NA, "t4.nd" = NA, "t5.nd" = NA, "t6.nd" = NA, "t7.nd" = NA, "t8.nd" = NA, "t9.nd" = NA, "t10.nd" = NA, "t11.nd" = NA, "t12.nd" = NA, "t13.nd" = NA, "t1.ns" = NA, "t2.ns" = NA, "t3.ns" = NA, "t4.ns" = NA, "t5.ns" = NA, "t6.ns" = NA, "t7.ns" = NA, "t8.ns" = NA, "t9.ns" = NA, "t10.ns" = NA, "t11.ns" = NA, "t12.ns" = NA, "t13.ns" = NA)
      new_row <- cbind(new_row, metrics)
      GM2012_vertices_df <- rbind(GM2012_vertices_df, new_row)
    }
  }
}


# Populates degree and strength fields for each time period

for (j in 1:length(GM2012_graphs)) {
  
  for (i in 1:length(V(GM2012_graphs[[j]]))) {
    
    potential_associates <- length(V(GM2012_graphs[[j]])) - 1
    
    new_degree <-  as.numeric(igraph::degree(graph = GM2012_graphs[[j]], V(GM2012_graphs[[j]])[i]))
    new_strength <- as.numeric(strength(graph = GM2012_graphs[[j]], V(GM2012_graphs[[j]])[i]))
    new_n.degree <- as.numeric(igraph::degree(graph = GM2012_graphs[[j]], V(GM2012_graphs[[j]])[i]))/potential_associates
    new_n.strength <- as.numeric(strength(graph = GM2012_graphs[[j]], V(GM2012_graphs[[j]])[i]))/potential_associates
    
    GM2012_vertices_df[which(GM2012_vertices_df$ID == V(GM2012_graphs[[j]])[i]$name), (j + 2)] <- new_degree
    GM2012_vertices_df[which(GM2012_vertices_df$ID == V(GM2012_graphs[[j]])[i]$name), (j + 15)] <- new_strength
    GM2012_vertices_df[which(GM2012_vertices_df$ID == V(GM2012_graphs[[j]])[i]$name), (j + 28)] <- new_n.degree
    GM2012_vertices_df[which(GM2012_vertices_df$ID == V(GM2012_graphs[[j]])[i]$name), (j + 41)] <- new_n.strength
  }
}

#save vertex metrics

setwd("~/Research/Villanova/Social network analysis/Thesis_SNA/GM_2012-2018")

write.csv(GM2012_vertices_df, file = "GM2012_vertices.csv", row.names = FALSE)


GM2012_vertices_df <- read.csv(file = "GM2012_vertices.csv", check.names = FALSE)

# plotting mean vertex normalized degree and normalized strength over time for each edge type

GM2012_vert_plot_df <- data.frame("time.period" = character(), "species" = character(), "nd" = numeric(), "nd.se" = numeric(), "ns" = numeric(), "ns.se" = numeric())

# populates the GM2012_vert_plot_df dataframe with network metrics

for (j in 1:length(GM2012_graphs)) {
  
  C_row <- data.frame("time.period" = j, 
                      "species" = "CACH", 
                      "nd" = mean(as.numeric(GM2012_vertices_df[which(GM2012_vertices_df$head.spp == "CACH"), j+28]), na.rm = TRUE), 
                      "nd.se" = std.error(as.numeric(GM2012_vertices_df[which(GM2012_vertices_df$head.spp == "CACH"), j+28]), na.rm = TRUE),
                      "ns" = mean(as.numeric(GM2012_vertices_df[which(GM2012_vertices_df$head.spp == "CACH"), j+41]), na.rm = TRUE), 
                      "ns.se" = std.error(as.numeric(GM2012_vertices_df[which(GM2012_vertices_df$head.spp == "CACH"), j+41]), na.rm = TRUE))
  
  
  B_row <- data.frame("time.period" = j, 
                      "species" = "BCCH",
                      "nd" = mean(as.numeric(GM2012_vertices_df[which(GM2012_vertices_df$head.spp == "BCCH"), j+28]), na.rm = TRUE), 
                      "nd.se" = std.error(as.numeric(GM2012_vertices_df[which(GM2012_vertices_df$head.spp == "BCCH"), j+28]), na.rm = TRUE),
                      "ns" = mean(as.numeric(GM2012_vertices_df[which(GM2012_vertices_df$head.spp == "BCCH"), j+41]), na.rm = TRUE), 
                      "ns.se" = std.error(as.numeric(GM2012_vertices_df[which(GM2012_vertices_df$head.spp == "BCCH"), j+41]), na.rm = TRUE))
  
  
  GM2012_vert_plot_df <- rbind(GM2012_vert_plot_df, C_row, B_row)
  
}

# replaces NaN values with NA

for (i in 1:ncol(GM2012_vert_plot_df)) {
  
  GM2012_vert_plot_df[,i][is.nan(GM2012_vert_plot_df[,i])] <- NA
}

GM2012_vert_plot <- ggplot(data = GM2012_vert_plot_df, aes(x = time.period, y = ns, width = 0.2)) +
  geom_line(aes(colour = species, linetype = species), size = 1.0, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("#F8766D", "cornflowerblue")) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  scale_x_discrete(limits = factor(unique(GM2012_vert_plot_df$time.period))) +
  geom_errorbar(aes(x = time.period, ymin = ns-ns.se, ymax = ns+ns.se, colour = species, linetype = species), 
                position = position_dodge(width = .5), size = 1.0) +
  ylab("Normalized Strength \u00B1 SE") +
  xlab("GM Time Period") +
  labs(color = "Node type", linetype = "Node type") +
  guides(color = guide_legend(reverse = TRUE), linetype = guide_legend(reverse = TRUE)) +
  theme_classic(base_size = 12) + 
  theme(legend.margin = margin(0,0,0,-2,"mm"))



# organize data frame

time_periods <- c(1:13) # added this in case you don't need to run all the graph stuff again

GM2012_vertex_glmm_df <- data.frame(ID = character(), spp = character(), time.period = numeric(), d = numeric(), s = numeric(), nd = numeric(), ns = numeric())

for (j in 1:nrow(GM2012_vertices_df)) {
  
  for (i in 1:length(time_periods)) {
    
    new_row <- data.frame(ID = GM2012_vertices_df$ID[j], spp = GM2012_vertices_df$head.spp[j], time.period = i,  d = GM2012_vertices_df[j, i + 2], s = GM2012_vertices_df[j, i + 15],  nd = GM2012_vertices_df[j, i + 28], ns = GM2012_vertices_df[j, i + 41])
    
    GM2012_vertex_glmm_df <- rbind(GM2012_vertex_glmm_df, new_row)
    
  }
}



GM2012_vertex_glmm_df$spp <- factor(GM2012_vertex_glmm_df$spp)
GM2012_vertex_glmm_df <- within(GM2012_vertex_glmm_df, spp <- relevel(spp, ref = "CACH"))


prior_G12v = list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V = 1, nu = 0.002))) #uninformative prior

GM12v_MCMC <- MCMCglmm(fixed = ns ~ time.period*spp, random = ~ ID,  data = GM2012_vertex_glmm_df, family = "gaussian", prior = prior_G12v, nitt = 1005000, burnin = 5000, verbose = FALSE, thin = 800) # "full" model - very good output


par(mar=c(1,1,1,1))
plot(GM12v_MCMC$Sol)

#

#### NF 2012-2013 VERTEX ANALYSIS - creating a dataframe that we'll use to store each edge's weight values for each time period ####

# Creating a new dataframe! in this case, d= degree, s = strength, nd = normalized degree, and ns = normalized strength
# 
NF2012_vertices_df <- data.frame( "ID" = character(), "Sex" = character(), "Spp" = character(), "t1.ns" = numeric(), "t2.ns" = numeric(), "t3.ns" = numeric(), "t4.ns" = numeric(), "t5.ns" = numeric(), "t6.ns" = numeric(), "t7.ns" = numeric(), "t8.ns" = numeric(), "t9.ns" = numeric(), "t10.ns" = numeric(), "t11.ns" = numeric())



# Creates a new row in that dataframe for each unique vertex
for (j in 1:length(NF2012_graphs)) {
  
  for (i in 1:length(V(NF2012_graphs[[j]]))) {
    
    if (V(NF2012_graphs[[j]])[i]$name %in% NF2012_vertices_df$ID == FALSE) {
      
      new_row <- data.frame("ID" = V(NF2012_graphs[[j]])[i]$name)
      new_row$Sex = NF2012_attr$Sex[match(new_row$ID, NF2012_attr$ID)]
      new_row$head.spp = NF2012_attr$Species[match(new_row$ID, NF2012_attr$ID)]
      
      metrics <- data.frame("t1.ns" = NA, "t2.ns" = NA, "t3.ns" = NA, "t4.ns" = NA, "t5.ns" = NA, "t6.ns" = NA, "t7.ns" = NA, "t8.ns" = NA, "t9.ns" = NA, "t10.ns" = NA, "t11.ns" = NA)
      new_row <- cbind(new_row, metrics)
      NF2012_vertices_df <- rbind(NF2012_vertices_df, new_row)
    }
  }
}


# Populates degree and strength fields for each time period

for (j in 1:length(NF2012_graphs)) {
  
  for (i in 1:length(V(NF2012_graphs[[j]]))) {
    
    potential_associates <- length(V(NF2012_graphs[[j]])) - 1
    
    new_n.strength <- as.numeric(strength(graph = NF2012_graphs[[j]], V(NF2012_graphs[[j]])[i]))/potential_associates
    
    NF2012_vertices_df[which(NF2012_vertices_df$ID == V(NF2012_graphs[[j]])[i]$name), (j + 3)] <- new_n.strength
  }
}

#save vertex metrics

write.csv(NF2012_vertices_df, file = "NF2012_vertices.csv", row.names = FALSE)
NF2012_vertices_df <- read.csv(file = "NF2012_vertices.csv", check.names = FALSE)

# plotting mean vertex normalized degree and normalized strength over time for each edge type

NF2012_vert_plot_df <- data.frame("time.period" = character(), "species" = character(), "ns" = numeric(), "ns.se" = numeric())

# populates the NF2012_vert_plot_df dataframe with network metrics

for (j in 1:length(NF2012_graphs)) {
  
  C_row <- data.frame("time.period" = j, 
                      "species" = "CACH", 
                      "ns" = mean(as.numeric(NF2012_vertices_df[which(NF2012_vertices_df$head.spp == "CACH"), j+2]), na.rm = TRUE), 
                      "ns.se" = std.error(as.numeric(NF2012_vertices_df[which(NF2012_vertices_df$head.spp == "CACH"), j+2]), na.rm = TRUE))
  
  
  B_row <- data.frame("time.period" = j, 
                      "species" = "BCCH",
                      "ns" = mean(as.numeric(NF2012_vertices_df[which(NF2012_vertices_df$head.spp == "BCCH"), j+2]), na.rm = TRUE), 
                      "ns.se" = std.error(as.numeric(NF2012_vertices_df[which(NF2012_vertices_df$head.spp == "BCCH"), j+2]), na.rm = TRUE))
  
  
  NF2012_vert_plot_df <- rbind(NF2012_vert_plot_df, C_row, B_row)
  
}

# replaces NaN values with NA

for (i in 1:ncol(NF2012_vert_plot_df)) {
  
  NF2012_vert_plot_df[,i][is.nan(NF2012_vert_plot_df[,i])] <- NA
}

# *Figure* that graphs normalized strength for different species over time for nf2012-2013

NF2012_vert_plot <- ggplot(data = NF2012_vert_plot_df, aes(x = time.period, y = ns, width = 0.2)) +
  geom_line(aes(colour = species, linetype = species), position = position_dodge(width = 0.5), size = 1.0) +
  scale_color_manual(values = c("#F8766D", "cornflowerblue")) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  scale_x_discrete(limits = factor(unique(NF2012_vert_plot_df$time.period))) +
  geom_errorbar(aes(x = time.period, ymin = ns-ns.se, ymax = ns+ns.se, colour = species, linetype = species), 
                position = position_dodge(width = .5), size = 1.0) +
  ylab("Normalized Strength \u00B1 SE") +
  xlab("NF Time Period") +
  labs(color = "Node type", linetype = "Node type") +
  guides(color = guide_legend(reverse = TRUE), linetype = guide_legend(reverse = TRUE)) +
  theme_classic(base_size = 12) + 
  theme(legend.margin = margin(0,0,0,-2,"mm"))


## GLMM for vertex degrees, strengths, and normalized for both (might work hopefully)

# organize data frame

time_periods <- c(1:11) # added this in case you don't need to run all the graph stuff again

NF2012_vertex_glmm_df <- data.frame(ID = character(), sex = character(), spp = character(), time.period = numeric(), ns = numeric())

for (j in 1:nrow(NF2012_vertices_df)) {
  
  for (i in 1:length(time_periods)) {
    
    new_row <- data.frame(ID = NF2012_vertices_df$ID[j], sex = NF2012_vertices_df$Sex[j], spp = NF2012_vertices_df$head.spp[j], time.period = i,  ns = NF2012_vertices_df[j, i + 3])
    
    NF2012_vertex_glmm_df <- rbind(NF2012_vertex_glmm_df, new_row)
    
  }
}



hist(NF2012_vertex_glmm_df$ns) # looks pretty gaussian to me

NF2012_vertex_glmm_df$spp <- factor(NF2012_vertex_glmm_df$spp)
NF2012_vertex_glmm_df$sex <- factor(NF2012_vertex_glmm_df$sex)
NF2012_vertex_glmm_df <- within(NF2012_vertex_glmm_df, spp <- relevel(spp, ref = "CACH"))


prior_G12v = list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V = 1, nu = 0.002))) #uninformative prior

NF2012v_MCMC <- MCMCglmm(fixed = ns ~ time.period*spp, random = ~ ID,  data = NF2012_vertex_glmm_df, family = "gaussian", prior = prior_G12v, nitt = 1005000, burnin = 5000, verbose = FALSE, thin = 800) # "full" model - very good output

summary(NF2012v_MCMC)

par(mar=c(1,1,1,1))
plot(NF2012v_MCMC$Sol)

##### Figures #####


# Assemble example network from GM 2012 time period 1 (Fig 1)

NF1_graph <- NF2012_graphs[[1]]

NF1_fg <- cluster_fast_greedy(NF1_graph)

V(NF1_graph)$Species=as.character(NF2012_attr$Species[match(V(NF1_graph)$name,NF2012_attr$ID)])

V(NF1_graph)$color <- V(NF1_graph)$Species
V(NF1_graph)$color <- gsub("CACH", "cornflowerblue", V(NF1_graph)$color) # CACH will be red
V(NF1_graph)$color <- gsub("BCCH", "#F8766D", V(NF1_graph)$color) # BCCH will be blue

V(NF1_graph)$shape <- V(NF1_graph)$Species
V(NF1_graph)$shape <- gsub("CACH", "circle", V(NF1_graph)$shape) # CACH will be circle
V(NF1_graph)$shape <- gsub("BCCH", "square", V(NF1_graph)$shape) # BCCH will be square

V(NF1_graph)$label <- NA

plot(NF1_graph, layout = layout_with_graphopt, vertex.size = 12, edge.width = .7)


# Assemble grid plot from edge/node plots (figure 2)

edge_legend <- get_legend(
  # create some space to the left of the legend
  GM2012_edge_plot + theme(legend.box.margin = margin(0, 0, 0, 12))
)

vert_legend <- get_legend(
  # create some space to the left of the legend
  GM2012_vert_plot + theme(legend.box.margin = margin(0, 0, 0, 12))
)

top_row <- plot_grid(
  NF2012_edge_plot + theme(axis.title.x = element_blank(), 
                           legend.position = "none"),
  NULL,
  GM2012_edge_plot + theme(axis.title.x = element_blank(),
                           axis.title.y = element_blank(), 
                           legend.position = "none"),
  edge_legend,
  labels = c("a", "", "b"),
  label_x = c(0, 0, -0.05),
  rel_widths = c(1, .05, 1, .3),
  nrow = 1
)

bottom_row <- plot_grid(
  NF2012_vert_plot + theme(legend.position = "none"),
  NULL,
  GM2012_vert_plot + theme(axis.title.y = element_blank(), 
                           legend.position = "none"),
  vert_legend,
  labels = c("c", "", "d"),
  label_x = c(0, 0, -0.05),
  rel_widths = c(1, .05, 1, .3),
  nrow = 1
)

plot_grid(top_row, bottom_row, ncol = 1)



