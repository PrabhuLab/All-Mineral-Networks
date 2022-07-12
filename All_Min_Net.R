#All Mineralogy Network

library(readxl)
library(igraph)

X2021_AllMineralNetwork_v_1_09FEB <- read_excel("Downloads/2021-AllMineralNetwork-v.1-09FEB.xlsx", 
                                                sheet = "2020-All_RRUFF_IMA_8.29.2020")
View(X2021_AllMineralNetwork_v_1_09FEB)

rows<-X2021_AllMineralNetwork_v_1_09FEB$`Mineral Name`

X2021_AllMineralNetwork_v_1_09FEB[is.na(X2021_AllMineralNetwork_v_1_09FEB)] <- 0
X2021_AllMineralNetwork_v_1_09FEB[,1:66] <- lapply(X2021_AllMineralNetwork_v_1_09FEB[,1:66], factor)

X2021_AllMineralNetwork_v_1_09FEB$`2C`<-as.factor(as.numeric(X2021_AllMineralNetwork_v_1_09FEB$`2C`))
X2021_AllMineralNetwork_v_1_09FEB$`Mineral Name`<-NULL

rownames(X2021_AllMineralNetwork_v_1_09FEB)<-rows

#_______________________________________

Bip_Mat<-as.matrix(X2021_AllMineralNetwork_v_1_09FEB)
Bip_G<-graph_from_incidence_matrix(Bip_Mat)
V(Bip_G)$name
plot(Bip_G, vertex.size = degree(Bip_G))

V(Bip_G)$color <- "white"

V(Bip_G)$color[1:nrow(StellarMinerals)] <- StellarMinerals$color

library(Ggally)




V(Bip_G)$color

#__________________________________________

#Evolutionary System of Mineralogy Bipartite Network
#Author : Anirudh Prabhu
#Email : prabha2@rpi.edu
#Date : Feb 9th, 2021

library(readxl)
EvolSysMin_Feb2021 <- read_excel("~/Downloads//2021-AllMineralNetwork-v.1-09FEB.xlsx", 
                                 sheet = "2020-All_RRUFF_IMA_8.29.2020")

#Loading packages
library(ggplot2)
library(ggnetwork)
library(igraph)
library(intergraph)

library(GGally)

Min_Bip_EvolSys<-EvolSysMin_Feb2021[,c(68,3:67)]


row.names(Min_Bip_EvolSys)<-Min_Bip_EvolSys$`Mineral Name (plain)`
Min_Bip_EvolSys$`Mineral Name (plain)` <- NULL
Min_Bip_Net<-graph_from_incidence_matrix(incidence = as.matrix(Min_Bip_EvolSys),directed = F)
plot(Min_Bip_Net)

Min_Bip_Net<-simplify(graph = Min_Bip_Net,remove.multiple = T,remove.loops = T)
color <- c("blue", "green")

V(Min_Bip_Net)$nodetype<-as.factor(V(Min_Bip_Net)$type)
V(Min_Bip_Net)$color <- color[V(Min_Bip_Net)$nodetype]
V(Min_Bip_Net)$color

library(network)
Min_Bip_NetN<-asNetwork(Min_Bip_Net)
network.vertex.names(Min_Bip_NetN)

Min_Bip_NetN %v% "color"

ggnet2(Min_Bip_NetN)
ggnet2(Min_Bip_NetN, edge.size = 0.1, label = T, label.color = "black", label.size = 1,  edge.color = "grey75",size = "degree", color = "color")

