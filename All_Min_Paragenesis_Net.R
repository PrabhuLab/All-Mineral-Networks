#All Mineral Paragenesis Net With Attributes

# All Min Net
library(igraph)
library(readxl)
X2023_ParageneticModes_PlateTectonics_14JUN <- read_excel("~/Downloads/2022-ParageneticModes-UPDATE-20DEC.xlsx", 
                                                          sheet = "Sheet1")

X2023_ParageneticModes_PlateTectonics_14JUN <- read_excel("~/Downloads/2023-ParageneticModes-PlateTectonics-14JUN.xlsx", 
                                                          sheet = "Sheet1")

X2023_ParageneticModes_PlateTectonics_14JUN <- read_excel("~/Downloads/2022-ParageneticModes-IMA-UpdatedRed-01Jan_ThruHadean.xlsx", 
                                                          sheet = "Sheet1")

X2023_ParageneticModes_PlateTectonics_14JUN <- read_excel("~/Downloads/2022-ParageneticModes-IMA-UpdatedRed-01Jan_MeteoritesOnly.xlsx", 
                                                          sheet = "Sheet1")


aa<-X2023_ParageneticModes_PlateTectonics_14JUN$`AA-Mineral Name`
X2023_ParageneticModes_PlateTectonics_14JUN$`AA-Mineral Name`<-NULL

rownames(X2023_ParageneticModes_PlateTectonics_14JUN)<-aa

X2023_ParageneticModes_PlateTectonics_14JUN[is.na(X2023_ParageneticModes_PlateTectonics_14JUN)] <- 0

summary(X2023_ParageneticModes_PlateTectonics_14JUN)

X2023_ParageneticModes_PlateTectonics_14JUN$p2<-as.numeric(X2023_ParageneticModes_PlateTectonics_14JUN$p2)
X2023_ParageneticModes_PlateTectonics_14JUN$p18<-as.numeric(X2023_ParageneticModes_PlateTectonics_14JUN$p18)


rownames(X2023_ParageneticModes_PlateTectonics_14JUN)<-aa

g<-graph_from_incidence_matrix(X2023_ParageneticModes_PlateTectonics_14JUN)
V(g)$type[5854:5858]
V(g)$type

unloadNamespace("sna")


isolated = which(degree(g)==0)
g = delete_vertices(g,isolated)

#hist((degree(g)[1:5646]),main = "All Min Net")




gg<-bipartite_projection(g)
g<-gg$proj1

library(visNetwork)

data <- toVisNetworkData(g)

X2022_ParageneticModes_UPDATE_20DEC <- read_excel("~/Downloads/2022-ParageneticModes-UPDATE-20DEC.xlsx", 
                                                  sheet = "Sheet2")


X2022_ParageneticModes_UPDATE_20DEC$biotic[which(X2022_ParageneticModes_UPDATE_20DEC$`biotic and abiotic`==1)]<-0
X2022_ParageneticModes_UPDATE_20DEC$abiotic[which(X2022_ParageneticModes_UPDATE_20DEC$`biotic and abiotic`==1)]<-0

X2022_ParageneticModes_UPDATE_20DEC$id[which(X2022_ParageneticModes_UPDATE_20DEC$`biotic and abiotic`==0 & X2022_ParageneticModes_UPDATE_20DEC$biotic==0 & X2022_ParageneticModes_UPDATE_20DEC$abiotic==0)]



library(tidyr)
library(dplyr)

abiotic_biotic<-X2022_ParageneticModes_UPDATE_20DEC |>
  mutate(ID = id) |>
  pivot_longer(cols = c(biotic,abiotic,`biotic and abiotic`), names_to = "bio_abio") |>
  filter(value == "1")





data$nodes$label[which(data$nodes$id=="Christofoshaferite-(Ce)")]<-"Christofschaferite-(Ce)"
data$nodes$id[which(data$nodes$id=="Christofoshaferite-(Ce)")]<-"Christofschaferite-(Ce)"


nodes<-merge(data$nodes,abiotic_biotic,by = "id",all = F)

blah<-data$nodes$id[!(data$nodes$id %in% nodes$id)]

#X2022_ParageneticModes_UPDATE_20DEC$id[]


# Part 5
#g<-g-c("Anosovite" ,   "Bruennerite" , "Fassaite"  ,   "Fe-Mo Alloys" ,"Maskelynite"  ,"Mayenite"  ,   "Mellilite","Scapolite")

# Part 6
#g<-g-c("Anosovite"  ,  "Bruennerite" , "Fassaite"  ,   "Fe-Mo Alloys", "Maskelynite" , "Mayenite" ,   "Mazzite"    , "Mellilite" ,   "Obertite"   ,  "Phengite"   ,  "Scapolite"  )

#All Min
#g<-g-c("Changesite-(Y)" ,  "Hapkeite")

#Plate Tectonics
g<-g-blah

gsize(g)
gorder(g)
ecount(g)
#which((!(X2022_ParageneticModes_UPDATE_20DEC$id %in% data$nodes$id)) & )

set.seed(123)
x = layout_with_fr(graph = g)
nodes$x = x[, 1]
nodes$y = x[, 2]

xx = data.frame(x=x[, 1],y=x[, 2])


library(sna)

x = gplot.layout.fruchtermanreingold(asNetwork(g),NULL)
write.csv(x,"Part5_XY_fruchtermanreingold_SNA.csv")
write.csv(x,"Part6_XY_fruchtermanreingold_SNA.csv")
write.csv(x,"PlateTect_XY_fruchtermanreingold_SNA.csv")
write.csv(x,"AllMin_XY_fruchtermanreingold_SNA.csv")

x<-read.csv("AllMin_XY_fruchtermanreingold_SNA.csv")
x$X<-NULL
x<-as.matrix(x)

nodes$x = x[, 1]*10
nodes$y = x[, 2]*10

edges <- data$edges

df <- get.data.frame(g)

df_sub<-df[!duplicated(t(apply(df[1:2], 1, sort))), ]


edges$weight <- NULL


library(ggnetwork)
library(intergraph)
library(GGally)



  
V(g)$name[V(g)$type==F]

V(g)$name
V(g)$bio_abio <- nodes$bio_abio

#V(g)$bio_abio[V(g)$type==F] <- 

V(g)$bio_abio


png(filename="PlateTect_Uni_Eigen_3kres_May22_2024.png",width = 3000,height = 3000)
#ggnet2(asNetwork(g),layout = "fruchtermanreingold", cell.jitter = 0.75)
set.seed(1)
plot(ggnet2(asNetwork(g), edge.size = 0.1, label = T, label.color = "black", label.size = 1,size = eigen_centrality(g)$vector*100, color = "bio_abio",palette = c("abiotic" = "steelblue", "biotic" = "green", "biotic and abiotic" = "tomato"),mode=x)+
       guides(size = FALSE))
dev.off()

png(filename="PlateTect_Uni_Degree_3kres_May22_2024.png",width = 3000,height = 3000)

set.seed(1)
plot(ggnet2(asNetwork(g), edge.size = 0.1, label = F, label.color = "black", label.size = 1,size = "degree", color = "bio_abio",palette = c("abiotic" = "steelblue", "biotic" = "green", "biotic and abiotic" = "tomato"),mode=x)+
       guides(size = FALSE))
dev.off()

png(filename="Meteorite(Part5)_Uni_Eigen_3kres_May22_2024.png",width = 3000,height = 3000)

set.seed(1)
plot(ggnet2(asNetwork(g), edge.size = 0.1, label = T, label.color = "black", label.size = 1, size = eigen_centrality(g)$vector*100, color = "bio_abio",palette = c("abiotic" = "steelblue", "biotic" = "green", "biotic and abiotic" = "tomato"),mode=x)+
       guides(size = FALSE))
dev.off()


ggsave(file="AllMin_Uni_Eigen_May23_2024.pdf",device = "pdf", width=7000, height=7000,units = "px")



eigen_centrality(g)$vector

Compactness <- function(g) {
  gra.geo <- distances(g) ## get geodesics
  gra.rdist <- 1/gra.geo  ## get reciprocal of geodesics
  diag(gra.rdist) <- NA   ## assign NA to diagonal
  gra.rdist[gra.rdist == Inf] <- 0 ## replace infinity with 0
  # Compactness = mean of reciprocal distances
  comp.igph <- mean(gra.rdist, na.rm=TRUE) 
  return(comp.igph)
}

AllMinNet <-c(diameter(g),
              average.path.length(g),
              edge_density(g),
              mean(degree(g)),
              Compactness(g),
              transitivity(g),
              
              centr_degree(g)$centralization,
              #centr_betw(g)$centralization,
              centr_clo(g)$centralization,
              centr_eigen(g)$centralization,
              
              sd(degree(g)),
              #sd(betweenness(g)),
              sd(closeness(g)),
              sd(evcent(g)$vector),
              modularity(g,cluster_louvain(g)$membership),
              assortativity_degree(g),
              #assortativity(g,betweenness(g)),
              assortativity(g,closeness(g)),
              assortativity(g,eigen_centrality(g)$vector),
              max(cluster_leading_eigen(g)$membership),
              max(cluster_label_prop(g)$membership),
              max(cluster_louvain(g)$membership)
              #max(cluster_edge_betweenness(g)$membership)
)


#visIgraph(g)

nodes$ID<-NULL

nodes$value<-NULL


nodes$group <- nodes$bio_abio


 visNetwork(nodes = nodes, edges = edges, width = "100%",height = "1000px") %>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled=TRUE), selectedBy = "group")%>%
  visNodes(shapeProperties = list(useBorderWithImage = F),scaling = list(min = 10,max = 100)) %>%
  visGroups(groupname = "biotic", color = "green", 
            shadow = list(enabled = TRUE)) %>% 
  # red triangle for group "B"
  visGroups(groupname = "abiotic", color = "blue") %>%
  visGroups(groupname = "biotic and abiotic", color = "red") %>%
  visPhysics(stabilization = T)%>%
  visNodes(fixed = T) %>%
  visEdges(smooth = F) %>%
  visInteraction(tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;white-space: nowrap;
 font-family: cursive;font-size:18px;font-color:purple;background-color: red;') %>%
  visSave(file = "All_Min_Interactive_Bipartite_Network_Paragenesis_Paper.html")
 
 
 
 #________________________________________
 #Louvain
 

#plot(g,cluster_louvain(g))

clusterlouvain <- cluster_louvain(g)
#plot(g, vertex.color=rainbow(3, alpha=0.6)[clusterlouvain$membership])

V(g)$louvain <- clusterlouvain$membership

Part5_Louvain_List <- data.frame(NodeName = V(g)$name, Community = V(g)$louvain)
write.csv(Part5_Louvain_List,"Part5_LouvainList.csv")
Part6_Louvain_List <- data.frame(NodeName = V(g)$name, Community = V(g)$louvain)
write.csv(Part6_Louvain_List,"Part6_LouvainList.csv")
Plate_Tect_Louvain_List <- data.frame(NodeName = V(g)$name, Community = V(g)$louvain)
write.csv(Plate_Tect_Louvain_List,"Plate_Tect_LouvainList.csv")
AllMin_Louvain_List <- data.frame(NodeName = V(g)$name, Community = V(g)$louvain)
write.csv(AllMin_Louvain_List,"AllMin_LouvainList.csv")

png(filename="PlateTect_Uni_Eigen_Louvain_3kres_May22_2024.png",width = 3000,height = 3000)
#ggnet2(asNetwork(g),layout = "fruchtermanreingold", cell.jitter = 0.75)
set.seed(1)
plot(ggnet2(asNetwork(g), edge.size = 0.1, label = T, label.color = "white", label.size = 1,size = eigen_centrality(g)$vector*100, color = "louvain",mode=x)+
       guides(size = FALSE))
dev.off()

png(filename="Hadean(Part6)_Uni_Eigen_3kres_Louvain_May22_2024.png",width = 3000,height = 3000)

set.seed(1)
plot(ggnet2(asNetwork(g), edge.size = 0.1, label = T, label.color = "white", label.size = 1,size = eigen_centrality(g)$vector*100, color = "louvain",mode=x)+
       guides(size = FALSE))
dev.off()

png(filename="All_Min_Unipartite_Eigen_3kres_Louvain_May22_2024.png",width = 3000,height = 3000)

set.seed(1)
plot(ggnet2(asNetwork(g), edge.size = 0.1, label = T, label.color = "white", label.size = 1,size = eigen_centrality(g)$vector*100, color = "louvain",mode=x)+
       guides(size = FALSE))
dev.off()


png(filename="Meteorite(Part5)_Uni_Eigen_3kres_Louvain_May22_2024.png",width = 3000,height = 3000)

set.seed(1)
plot(ggnet2(asNetwork(g), edge.size = 0.1, label = T, label.color = "yellow", label.size = 1, size = eigen_centrality(g)$vector*100, color = "louvain",mode=x)+
       guides(size = FALSE))
dev.off()


ggsave(file="AllMin_Uni_Eigen_Louvain_May23_2024.pdf",device = "pdf", width=7000, height=7000,units = "px")



#____________________________________________________________

#Equivalent Random Networks

gsize(g)

Part5_dimension <- c(285,23897)
Part6_dimension <- c(822,62764)
Plate_Tect_dimension <- c(3721,1184058)
AllMin_dimension <- c(5851,2997712)
   
random_g_p5 <- erdos.renyi.game(n = Part5_dimension[1], p.or.m = Part5_dimension[2], type = "gnm")
random_g_p6 <- erdos.renyi.game(n = Part6_dimension[1], p.or.m = Part6_dimension[2], type = "gnm")
random_g_plateTect <- erdos.renyi.game(n = Plate_Tect_dimension[1], p.or.m = Plate_Tect_dimension[2], type = "gnm")
random_g_All <- erdos.renyi.game(n = AllMin_dimension[1], p.or.m = AllMin_dimension[2], type = "gnm")

#plot(random_g)


Random_AllMinNet <-c(diameter(random_g_All),
              average.path.length(random_g_All),
              edge_density(random_g_All),
              mean(degree(random_g_All)),
              Compactness(random_g_All),
              transitivity(random_g_All),
              
              centr_degree(random_g_All)$centralization,
              #centr_betw(random_g_All)$centralization,
              centr_clo(random_g_All)$centralization,
              centr_eigen(random_g_All)$centralization,
              
              sd(degree(random_g_All)),
              #sd(betweenness(random_g_All)),
              sd(closeness(random_g_All)),
              sd(evcent(random_g_All)$vector),
              modularity(random_g_All,cluster_louvain(random_g_All)$membership),
              assortativity_degree(random_g_All),
              #assortativity(random_g_All,betweenness(random_g_All)),
              assortativity(random_g_All,closeness(random_g_All)),
              assortativity(random_g_All,eigen_centrality(random_g_All)$vector),
              max(cluster_leading_eigen(random_g_All)$membership),
              max(cluster_label_prop(random_g_All)$membership),
              max(cluster_louvain(random_g_All)$membership)
              #max(cluster_edge_betweenness(random_g_All)$membership)
)

Random_Part5MinNet <-c(diameter(random_g_p5),
                     average.path.length(random_g_p5),
                     edge_density(random_g_p5),
                     mean(degree(random_g_p5)),
                     Compactness(random_g_p5),
                     transitivity(random_g_p5),
                     
                     centr_degree(random_g_p5)$centralization,
                     #centr_betw(random_g_p5)$centralization,
                     centr_clo(random_g_p5)$centralization,
                     centr_eigen(random_g_p5)$centralization,
                     
                     sd(degree(random_g_p5)),
                     #sd(betweenness(random_g_p5)),
                     sd(closeness(random_g_p5)),
                     sd(evcent(random_g_p5)$vector),
                     modularity(random_g_p5,cluster_louvain(random_g_p5)$membership),
                     assortativity_degree(random_g_p5),
                     #assortativity(g,betweenness(random_g_p5)),
                     assortativity(random_g_p5,closeness(random_g_p5)),
                     assortativity(random_g_p5,eigen_centrality(random_g_p5)$vector),
                     max(cluster_leading_eigen(random_g_p5)$membership),
                     max(cluster_label_prop(random_g_p5)$membership),
                     max(cluster_louvain(random_g_p5)$membership)
                     #max(cluster_edge_betweenness(random_g_p5)$membership)
)

Random_Part6MinNet <-c(diameter(random_g_p6),
                     average.path.length(random_g_p6),
                     edge_density(random_g_p6),
                     mean(degree(random_g_p6)),
                     Compactness(random_g_p6),
                     transitivity(random_g_p6),
                     
                     centr_degree(random_g_p6)$centralization,
                     #centr_betw(random_g_p6)$centralization,
                     centr_clo(random_g_p6)$centralization,
                     centr_eigen(random_g_p6)$centralization,
                     
                     sd(degree(random_g_p6)),
                     #sd(betweenness(random_g_p6)),
                     sd(closeness(random_g_p6)),
                     sd(evcent(random_g_p6)$vector),
                     modularity(random_g_p6,cluster_louvain(random_g_p6)$membership),
                     assortativity_degree(random_g_p6),
                     #assortativity(g,betweenness(random_g_p6)),
                     assortativity(random_g_p6,closeness(random_g_p6)),
                     assortativity(random_g_p6,eigen_centrality(random_g_p6)$vector),
                     max(cluster_leading_eigen(random_g_p6)$membership),
                     max(cluster_label_prop(random_g_p6)$membership),
                     max(cluster_louvain(random_g_p6)$membership)
                     #max(cluster_edge_betweenness(random_g_p6)$membership)
)

Random_PlateTectMinNet <-c(diameter(random_g_plateTect),
                     average.path.length(random_g_plateTect),
                     edge_density(random_g_plateTect),
                     mean(degree(random_g_plateTect)),
                     Compactness(random_g_plateTect),
                     transitivity(random_g_plateTect),
                     
                     centr_degree(random_g_plateTect)$centralization,
                     #centr_betw(random_g_plateTect)$centralization,
                     centr_clo(random_g_plateTect)$centralization,
                     centr_eigen(random_g_plateTect)$centralization,
                     
                     sd(degree(random_g_plateTect)),
                     #sd(betweenness(random_g_plateTect)),
                     sd(closeness(random_g_plateTect)),
                     sd(evcent(random_g_plateTect)$vector),
                     modularity(random_g_plateTect,cluster_louvain(random_g_plateTect)$membership),
                     assortativity_degree(random_g_plateTect),
                     #assortativity(g,betweenness(random_g_plateTect)),
                     assortativity(random_g_plateTect,closeness(random_g_plateTect)),
                     assortativity(random_g_plateTect,eigen_centrality(random_g_plateTect)$vector),
                     max(cluster_leading_eigen(random_g_plateTect)$membership),
                     max(cluster_label_prop(random_g_plateTect)$membership),
                     max(cluster_louvain(random_g_plateTect)$membership)
                     #max(cluster_edge_betweenness(random_g_plateTect)$membership)
)


library(sna)
x = gplot.layout.fruchtermanreingold(asNetwork(random_g_p5),NULL)
nodes$x = x[, 1]*10
nodes$y = x[, 2]*10

x = gplot.layout.fruchtermanreingold(asNetwork(random_g_p6),NULL)
x = gplot.layout.fruchtermanreingold(asNetwork(random_g_plateTect),NULL)
x = gplot.layout.fruchtermanreingold(asNetwork(random_g_All),NULL)

png(filename="PlateTect_RandomUnipartite_Eigen_3kres_May22_2024.png",width = 3000,height = 3000)
#ggnet2(asNetwork(g),layout = "fruchtermanreingold", cell.jitter = 0.75)
set.seed(1)
plot(ggnet2(asNetwork(random_g_plateTect), edge.size = 0.1, label = F, label.color = "white", label.size = 1,size = eigen_centrality(random_g_plateTect)$vector*100,mode=x)+
       guides(size = FALSE))
dev.off()

png(filename="Part6_Min_RandomUnipartite_Eigen_3kres_May22_2024.png",width = 2000,height = 2000)

set.seed(1)
plot(ggnet2(asNetwork(random_g_p6), edge.size = 0.1, label = F, label.color = "white", label.size = 1,size = eigen_centrality(random_g_p6)$vector*100,mode=x)+
       guides(size = FALSE))
dev.off()

png(filename="All_Min_RandomUnipartite_Eigen_3kres_May22_2024.png",width = 3000,height = 3000)

set.seed(1)
plot(ggnet2(asNetwork(random_g_All), edge.size = 0.1, label = F, label.color = "white", label.size = 1,size = eigen_centrality(random_g_All)$vector*100,mode=x)+
       guides(size = FALSE))
dev.off()


png(filename="Part5_RandomUnipartite_Eigen_3kres_May22_2024.png",width = 2000,height = 2000)

set.seed(1)
plot(ggnet2(asNetwork(random_g_p5), edge.size = 0.1, label = F, label.color = "white", label.size = 1, size = eigen_centrality(random_g_p5)$vector*100,mode=x)+
       guides(size = FALSE))
dev.off()



write.csv(Random_Part5MinNet,file = "~/Part5_EqRandomNet_Metrics.csv")
write.csv(Random_Part6MinNet,file = "~/Part6_EqRandomNet_Metrics.csv")
write.csv(Random_PlateTectMinNet,file = "~/PlateTect_EqRandomNet_Metrics.csv")
write.csv(Random_AllMinNet,file = "~/AllMin_EqRandomNet_Metrics.csv")

#_________________________________________
#Random Forest

library(randomForest)
library(randomForestExplainer)

library(readr)
All_minerals_cobined_attributes_v0_1 <- read_csv("Downloads/All_minerals_cobined_attributes_v0_1.csv")
louvain_all_min <- read_csv("AllMin_LouvainList.csv")

# Merge based on ID and Year using inner join
merged_inner <- merge(All_minerals_cobined_attributes_v0_1, louvain_all_min, by = c("Mineral"))
merged_inner$...1<-NULL

m<-randomForest()

#_________________________________________________________________________________

#Check which minerals are newly added for each paragenetic mode. 

rownames(X2023_ParageneticModes_PlateTectonics_14JUN)

list_to_check<-c()
PM_Unique_Count<-data.frame(PM=colnames(X2023_ParageneticModes_PlateTectonics_14JUN),Unique_Counts=NA,New_Min_Count=NA)

for(i in 1:67)
{
  list_to_check<-append(list_to_check,which(X2023_ParageneticModes_PlateTectonics_14JUN[,i]==1))
  list_to_check<-unique(list_to_check)
  PM_Unique_Count[i,"Unique_Counts"]<-length(list_to_check)
  
}


for(i in 2:67)
{
  PM_Unique_Count[i,"New_Min_Count"]<-PM_Unique_Count[i,"Unique_Counts"]-PM_Unique_Count[i-1,"Unique_Counts"]
}

which(rowSums(X2023_ParageneticModes_PlateTectonics_14JUN)==1)

write.csv(PM_Unique_Count,"All_Min_PM_Unique_Counts.csv")




