#Evolutionary System of Mineralogy Bipartite Network
#Author : Anirudh Prabhu
#Email : aprabhu@carnegiescience.edu
#Date : Dec 19th, 2022

library(readxl)
#Make sure All columns are numeric...R bugs out sometimes. 
EvolSysMin_Feb2021 <- read_excel("Downloads/2022-ParageneticModes-IMA-UpdatedRed-20DEC-Check.xlsx", 
                                 sheet = "Sheet1")

#EvolSysMin_Feb2021<-X2022_Hazen_ParageneticModes_AmMin_Supplement1_12JULY

#X2021_ParageneticAttributes_07OCT <- read_excel("Downloads/2021-ParageneticAttributes-07OCT.xlsx")

#X2021_ParageneticAttributes_07OCT$PM<-paste('p',X2021_ParageneticAttributes_07OCT$`Mode #`,sep="")

#X2021_ParageneticAttributes_07OCT[is.na(X2021_ParageneticAttributes_07OCT)]<-0

EvolSysMin_Feb2021_MineralList <- read_excel("Downloads/2022-ParageneticModes-IMA-UpdatedRed-20DEC-Check.xlsx", 
                                             sheet = "Sheet1")

summary(EvolSysMin_Feb2021)

EvolSysMin_Feb2021$p1<-as.numeric(EvolSysMin_Feb2021$p1)
EvolSysMin_Feb2021$p2<-as.numeric(EvolSysMin_Feb2021$p2)

EvolSysMin_Feb2021$p18<-as.numeric(EvolSysMin_Feb2021$p18)

#Loading packages
library(ggplot2)
library(ggnetwork)
library(igraph)
library(intergraph)
library(GGally)

#Min_Bip_EvolSys<-EvolSysMin_Feb2021[,c(68,3:67)]
#Min_Bip_EvolSys<-X2021_Hazen_ParageneticModes_AmMin_Supplement1_15APR[-c(5657:5663),]
Min_Bip_EvolSys  <- EvolSysMin_Feb2021

Min_Bip_EvolSys$Lunar<-NULL
#Min_Bip_EvolSys$`Mineral Name`[which(duplicated(Min_Bip_EvolSys$`Mineral Name`))]
rows<-Min_Bip_EvolSys$`Mineral Name`

Min_Bip_EvolSys$`Mineral Name` <- NULL

Min_Bip_EvolSys[is.na(Min_Bip_EvolSys)]<- 0
row.names(Min_Bip_EvolSys)<-rows
#%>%mutate_all(~replace(., is.na(.), 0))

Min_Bip_Net<-graph_from_incidence_matrix(incidence = as.matrix(Min_Bip_EvolSys),directed = F)
#plot(Min_Bip_Net)

Min_Bip_Net<-simplify(graph = Min_Bip_Net,remove.multiple = T,remove.loops = T)
node_color <- c("#9b4e23", "green")

V(Min_Bip_Net)$nodetype<-as.factor(V(Min_Bip_Net)$type)
V(Min_Bip_Net)$node_color <- node_color[V(Min_Bip_Net)$nodetype]
V(Min_Bip_Net)$node_color
V(Min_Bip_Net)$type[5883:5888]
V(Min_Bip_Net)$node_color[5883:5888]

V(Min_Bip_Net)$water<-"2"
V(Min_Bip_Net)$water[5657:5722]<-X2021_ParageneticAttributes_07OCT$Water

V(Min_Bip_Net)$biotic<-"2"
V(Min_Bip_Net)$biotic[5657:5722]<-X2021_ParageneticAttributes_07OCT$Biotic

clus_g<-cluster_louvain(Min_Bip_Net)

V(Min_Bip_Net)$Louvain<-clus_g$membership

V(Min_Bip_Net)$Elements <- "00"
V(Min_Bip_Net)$Elements[1:5884] <- EvolSysMin_Feb2021_MineralList$elements

V(Min_Bip_Net)$Elements[5883:5888]

`Periodic%20Table%20of%20Elements`$Symbol[i]

library(network)
Min_Bip_NetN<-asNetwork(Min_Bip_Net)
network.vertex.names(Min_Bip_NetN)


Min_Bip_NetN %v% "node_color"
Min_Bip_NetN %v% "Louvain" <- clus_g$membership
Min_Bip_NetN %v% "Louvain"
Min_Bip_NetN



nrow(Min_Bip_NetN %v% "Elements_List")

Min_Bip_NetN %v% "Elements"

ggnet2(Min_Bip_NetN)
ggnet2(Min_Bip_NetN, edge.size = 0.1, label = F, label.color = "black", label.size = 1,size = "degree", color = "node_color", edge.color = c("color", "grey50"))

#___________________________

#Color by paragenetic mode
df <- get.data.frame(Min_Bip_Net)

#df$edge_color <- "grey"
#df$edge_color[which(df$to=="p1"|df$to=="p2"|df$to=="p3"|df$to=="p4"|df$to=="p5"|df$to=="p6")]<-"black"
#df$edge_color[which(df$to=="p7"|df$to=="p8"|df$to=="p9"|df$to=="p10"|df$to=="p11")]<-"red"
#df$edge_color[which(df$to=="p12"|df$to=="p13"|df$to=="p14"|df$to=="p15"|df$to=="p16"|df$to=="p17"|df$to=="p18")]<-"lightblue"

#df$edge_color[which(df$to=="p21"|df$to=="p22"|df$to=="p23"|df$to=="p24"|df$to=="p25"|df$to=="p26")]<-"black"

require('gtools')

PM_List<-mixedsort(unique(df$to))

PM_List_Color<-rep(NA,66)
names(PM_List_Color)<-PM_List

PM_List_Color[1:6]<-"black"
PM_List_Color[7:11]<-"red"
PM_List_Color[12:18]<-"#33f3ff"
PM_List_Color[19:20]<-"grey"
PM_List_Color[21:30]<-"#f0ff00"
PM_List_Color[31:33]<-"#f15530"
PM_List_Color[34:37]<-"#1e06aa"
PM_List_Color[38:43]<-"#c37fe6"
PM_List_Color[44:56]<-"#afdb2f"
PM_List_Color[57:62]<-"#395e50"
PM_List_Color[63:66]<-"#fb1cd3"

#Paragenetic modes p1 to p6 -- meteorite minerals -- Black
#Paragenetic modes p7 to p11 -- Hadean primary minerals -- Red
#Paragenetic modes p12 to p18 -- Hadean hydrosphere minerals -- Lighter Blue
#Paragenetic modes p19 to p20 -- Primary crust minerals -- Grey
#Paragenetic modes p21 to p30 -- near-surface minerals -- Bright Yellow
#Paragenetic modes p31 to p33 -- High-T alteration minerals -- Orange
#Paragenetic modes p34 to p37 -- Highly evolved igneous minerals -- Darker Blue
#Paragenetic modes p38 to p43 -- Plate tectonics minerals -- Bright purple
#Paragenetic modes p44 to p47 -- Early Biosphere minerals -- Lighter yellow green
#Paragenetic modes p48 to p53 -- Phanerozoic biominerals -- Darker green
#Paragenetic modes p54 to p57 -- Anthropogenic minerals -- Reddish violet

length(V(Min_Bip_Net)$node_color)
V(Min_Bip_Net)$node_color[5885:length(V(Min_Bip_Net)$node_color)]<-PM_List_Color

V(Min_Bip_Net)$node_color[5885:length(V(Min_Bip_Net)$node_color)]

library(network)
Min_Bip_NetN<-asNetwork(Min_Bip_Net)

network.vertex.names(Min_Bip_NetN)
Min_Bip_NetN %v% "node_color"
Min_Bip_NetN %v% "Louvain" <- clus_g$membership
Min_Bip_NetN %v% "Elements"

df$edge_color<-""

#df$edge_color[df$to %in% names(PM_List_Color)]
PM_List_Color[match(df$to,names(PM_List_Color))]
df$edge_color<-PM_List_Color[match(df$to,names(PM_List_Color))]
df


Min_Bip_NetN %v% "node_color"
Min_Bip_NetN

#PM_List_Color


plot(ggnet2(Min_Bip_NetN, edge.size = 0.1, label = F, label.color = "black", label.size = 1,size = "degree", color = "node_color", edge.color = df$edge)+
       guides(size = FALSE))


#_______________________________________

#Grey out others evolving network

PM_List_Color[1:6]<-"black"
PM_List_Color[7:11]<-"red"
PM_List_Color[12:18]<-"#33f3ff"
PM_List_Color[19:20]<-"grey"
PM_List_Color[21:30]<-"#f0ff00"
PM_List_Color[31:33]<-"#f15530"
PM_List_Color[34:37]<-"#1e06aa"
PM_List_Color[38:43]<-"#c37fe6"
PM_List_Color[44:56]<-"#afdb2f"
PM_List_Color[57:62]<-"#395e50"
PM_List_Color[63:66]<-"#fb1cd3"

#Paragenetic modes p1 to p6 -- meteorite minerals -- Black
#Paragenetic modes p7 to p11 -- Hadean primary minerals -- Red
#Paragenetic modes p12 to p18 -- Hadean hydrosphere minerals -- Lighter Blue
#Paragenetic modes p19 to p20 -- Primary crust minerals -- Grey
#Paragenetic modes p21 to p30 -- near-surface minerals -- Bright Yellow
#Paragenetic modes p31 to p33 -- High-T alteration minerals -- Orange
#Paragenetic modes p34 to p37 -- Highly evolved igneous minerals -- Darker Blue
#Paragenetic modes p38 to p43 -- Plate tectonics minerals -- Bright purple
#Paragenetic modes p44 to p47 -- Early Biosphere minerals -- Lighter yellow green
#Paragenetic modes p48 to p53 -- Phanerozoic biominerals -- Darker green
#Paragenetic modes p54 to p57 -- Anthropogenic minerals -- Reddish violet

#df$edge_color<-""

PMType_Color_List<-rep(NA,66)

#Paragenetic modes p1 to p6 -- meteorite minerals -- Black
#Paragenetic modes p7 to p11 -- Hadean primary minerals -- Red
#Paragenetic modes p12 to p18 -- Hadean hydrosphere minerals -- Lighter Blue
#Paragenetic modes p19 to p20 -- Primary crust minerals -- Grey
#Paragenetic modes p21 to p30 -- near-surface minerals -- Bright Yellow
#Paragenetic modes p31 to p33 -- High-T alteration minerals -- Orange
#Paragenetic modes p34 to p37 -- Highly evolved igneous minerals -- Darker Blue
#Paragenetic modes p38 to p43 -- Plate tectonics minerals -- Bright purple
#Paragenetic modes p44 to p47 -- Early Biosphere minerals -- Lighter yellow green
#Paragenetic modes p48 to p53 -- Phanerozoic biominerals -- Darker green
#Paragenetic modes p54 to p57 -- Anthropogenic minerals -- Reddish violet

PMType_Color_List[1:6]<-"black"
PMType_Color_List[7:11]<-"red"
PMType_Color_List[12:18]<-"#33f3ff"
PMType_Color_List[19:20]<-"grey"
PMType_Color_List[21:30]<-"#f0ff00"
PMType_Color_List[31:33]<-"#f15530"
PMType_Color_List[34:37]<-"#1e06aa"
PMType_Color_List[38:43]<-"#c37fe6"
PMType_Color_List[44:56]<-"#afdb2f"
PMType_Color_List[57:62]<-"#395e50"
PMType_Color_List[63:66]<-"#fb1cd3"

names(PMType_Color_List)<-rep("1",66)

names(PMType_Color_List)[1:6]<-"meteorite minerals"
names(PMType_Color_List)[7:11]<-"Hadean primary minerals"
names(PMType_Color_List)[12:18]<-"Hadean hydrosphere minerals"
names(PMType_Color_List)[19:20]<-"Primary crust minerals"
names(PMType_Color_List)[21:30]<-"near-surface minerals"
names(PMType_Color_List)[31:33]<-"High-T alteration minerals"
names(PMType_Color_List)[34:37]<-"Highly evolved igneous minerals"
names(PMType_Color_List)[38:43]<-"Plate tectonics minerals"
names(PMType_Color_List)[44:56]<-"Early Biosphere minerals"
names(PMType_Color_List)[57:62]<-"Phanerozoic biominerals"
names(PMType_Color_List)[63:66]<-"Anthropogenic minerals"


names(PMType_Color_List)
names(PMType_Color_List[match(df$edge_color,PMType_Color_List)])
df$PMType<-names(PMType_Color_List[match(df$edge_color,PMType_Color_List)])

library(fastDummies)
PMType_dummy<-dummy_cols(df$PMType)
PMType_dummy$color <- df$edge_color

PMType_dummy[PMType_dummy==0]<-"#d8d8d8"
#i<-2
for(i in 2:12)
{
  PMType_dummy[ PMType_dummy[,i] == "1" , i ] <- PMType_dummy[ PMType_dummy[,i] == "1", "color" ]
}

set.seed(1273)
png("All_Min_Network/Plots/GreyOutEvol/example%02d.png",width = 1600, height = 900)
for(i in 2:12)
{
  plot(ggnet2(Min_Bip_NetN, edge.size = 0.1, label = F, label.color = "black", label.size = 1,size = "degree", color = "node_color", edge.color = PMType_dummy[,i])+
         guides(size = FALSE))
}
dev.off()

# VisNetwork Interactive All mineral network

AllMin_VisNet<-asIgraph(Min_Bip_NetN)

library(visNetwork)

data <- toVisNetworkData(AllMin_VisNet)

visNetwork(nodes = data$nodes, edges = data$edges, height = "500px")%>% 
  visOptions(collapse = TRUE)%>%
  visSave(file = "All_Min_Interactive_Network.html")

data$nodes$label<-data$nodes$vertex.names
data$nodes$color<-data$nodes$node_color

data$edges$color<-df$edge_color

visIgraph(AllMin_VisNet)%>%
  visSave(file = "AllMin_IgraphVis.html")

#___________________

PMType_Color_List <- c(PMType_Color_List, Mineral="#9b4e23")

data$nodes$group<-names(PMType_Color_List[match(as.list(data$nodes$node_color),PMType_Color_List)])
nodes <- data.frame(id = data$nodes$label, label = data$nodes$label, group = data$nodes$group,
                    Elements=data$nodes$Elements, color=data$nodes$node_color, replace = TRUE)


edges <- data.frame(from=df$to,to=df$from)



# keeping all parent node attributes  
visNetwork(nodes = nodes, edges = edges,height = "1500px", width = "100%" ) %>%
  visOptions(selectedBy = "group",highlightNearest = TRUE, 
             nodesIdSelection = TRUE)%>%
  visPhysics(stabilization = F)%>%
  visNodes(fixed = TRUE) %>%
  #visPhysics(solver = "barnesHut", barnesHut = list(gravitationalConstant = -80000,springConstant= 0.001,springLength=200),stabilization = FALSE)%>%
  visEdges(smooth = FALSE)%>%
  visLegend()%>%
  #visOptions(collapse = TRUE)#%>%
  visSave(file = "All_Min_Interactive_Network.html")

visNetwork(nodes = nodes, edges = edges,height = "1500px", width = "100%" ) %>%
  visOptions(selectedBy = "group",highlightNearest = TRUE, 
             nodesIdSelection = TRUE)%>%
  visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -80000,springConstant= 0.001,springLength=200),stabilization = FALSE)%>%
  visEdges(smooth = FALSE)%>%
  visLegend(useGroups = F,addNodes = )%>%
  #visOptions(collapse = TRUE)#%>%
  visSave(file = "All_Min_Interactive_Network_Force2Atlas.html")

# Set x and y co-ordinates from another layout for faster placement. 
library(sna)
x = gplot.layout.fruchtermanreingold(Min_Bip_NetN, NULL)

nodes$x = x[, 1]*10
nodes$y = x[, 2]*10

#Custom Legend
legendNodes <- data.frame(
  label = names(PMType_Color_List),
  color.background = PMType_Color_List,
  color.border = "black",
  shape="dot"
)


#values can be stored for sort list of dropdown



# keeping all parent node attributes  
visNetwork(nodes = nodes, edges = edges,height = "1500px", width = "100%",main = "Network of Mineralogy",submain = "Prabhu, Morrison, Williams, Hazen",footer = "Updated on 29th Sept,2021.") %>%
  visOptions(selectedBy = "biotic",highlightNearest = TRUE, nodesIdSelection = list(enabled=TRUE))%>%
  visPhysics(stabilization = F)%>%
  visNodes(fixed = TRUE) %>%
  #visPhysics(solver = "barnesHut", barnesHut = list(gravitationalConstant = -80000,springConstant= 0.001,springLength=200),stabilization = FALSE)%>%
  visEdges(smooth = FALSE)%>%
  visLegend(useGroups = F,addNodes = unique(legendNodes))%>%
  #visOptions(collapse = TRUE)#%>%
  visSave(file = "All_Min_Interactive_Network_Biotic.html")


#_______________________________________________________________

# Group by Rare element presence, OnePM and TwoPM



nodes$RareElement <- "2"
nodes$OnePM <- "2"
nodes$TwoPM <- "2"
nodes$OneOrTwoPM<-"2"

nodes$RareElement[1:5656]<-EvolSysMin_Feb2021_MineralList$`Has rare element`
nodes$OnePM[1:5656]<-EvolSysMin_Feb2021_MineralList$`Has 1 paragenetic mode`
nodes$TwoPM[1:5656]<-EvolSysMin_Feb2021_MineralList$`Has 2 paragenetic modes`

nodes$OneOrTwoPM[1:5656]<-EvolSysMin_Feb2021_MineralList$`Has 2 paragenetic modes`+EvolSysMin_Feb2021_MineralList$`Has 1 paragenetic mode`


visNetwork(nodes = nodes, edges = edges,height = "1500px", width = "100%",main = "Network of Mineralogy",submain = "Prabhu, Morrison, Williams, Hazen",footer = "Updated on 29th Sept,2021.") %>%
  visOptions(selectedBy = "RareElement",highlightNearest = TRUE, nodesIdSelection = list(enabled=TRUE))%>%
  visPhysics(stabilization = F)%>%
  visNodes(fixed = TRUE) %>%
  #visPhysics(solver = "barnesHut", barnesHut = list(gravitationalConstant = -80000,springConstant= 0.001,springLength=200),stabilization = FALSE)%>%
  visEdges(smooth = FALSE)%>%
  visLegend(useGroups = F,addNodes = unique(legendNodes))%>%
  #visOptions(collapse = TRUE)#%>%
  visSave(file = "All_Min_Interactive_Network_RareElement.html")


visNetwork(nodes = nodes, edges = edges,height = "1500px", width = "100%",main = "Network of Mineralogy",submain = "Prabhu, Morrison, Williams, Hazen",footer = "Updated on 29th Sept,2021.") %>%
  visOptions(selectedBy = "OnePM",highlightNearest = TRUE, nodesIdSelection = list(enabled=TRUE))%>%
  visPhysics(stabilization = F)%>%
  visNodes(fixed = TRUE) %>%
  #visPhysics(solver = "barnesHut", barnesHut = list(gravitationalConstant = -80000,springConstant= 0.001,springLength=200),stabilization = FALSE)%>%
  visEdges(smooth = FALSE)%>%
  visLegend(useGroups = F,addNodes = unique(legendNodes))%>%
  #visOptions(collapse = TRUE)#%>%
  visSave(file = "All_Min_Interactive_Network_OnePM.html")

visNetwork(nodes = nodes, edges = edges,height = "1500px", width = "100%",main = "Network of Mineralogy",submain = "Prabhu, Morrison, Williams, Hazen",footer = "Updated on 29th Sept,2021.") %>%
  visOptions(selectedBy = "TwoPM",highlightNearest = TRUE, nodesIdSelection = list(enabled=TRUE))%>%
  visPhysics(stabilization = F)%>%
  visNodes(fixed = TRUE) %>%
  #visPhysics(solver = "barnesHut", barnesHut = list(gravitationalConstant = -80000,springConstant= 0.001,springLength=200),stabilization = FALSE)%>%
  visEdges(smooth = FALSE)%>%
  visLegend(useGroups = F,addNodes = unique(legendNodes))%>%
  #visOptions(collapse = TRUE)#%>%
  visSave(file = "All_Min_Interactive_Network_TwoPM.html")

visNetwork(nodes = nodes, edges = edges,height = "1500px", width = "100%",main = "Network of Mineralogy",submain = "Prabhu, Morrison, Williams, Hazen",footer = "Updated on 29th Sept,2021.") %>%
  visOptions(selectedBy = "OneOrTwoPM",highlightNearest = TRUE, nodesIdSelection = list(enabled=TRUE))%>%
  visPhysics(stabilization = F)%>%
  visNodes(fixed = TRUE) %>%
  #visPhysics(solver = "barnesHut", barnesHut = list(gravitationalConstant = -80000,springConstant= 0.001,springLength=200),stabilization = FALSE)%>%
  visEdges(smooth = FALSE)%>%
  visLegend(useGroups = F,addNodes = unique(legendNodes))%>%
  #visOptions(collapse = TRUE)#%>%
  visSave(file = "All_Min_Interactive_Network_OneOrTwoPM.html")

#____________________________________________

#PM associated with Biology, Non anthropogenic

X2021_ParageneticAttributes_07OCT$Bio_NonAnthro<-X2021_ParageneticAttributes_07OCT$Biotic

X2021_ParageneticAttributes_07OCT$Bio_NonAnthro[63:66]<-0

nodes$Bio_NonAnthro<-"2"

nodes$Bio_NonAnthro[5657:5722]<-X2021_ParageneticAttributes_07OCT$Bio_NonAnthro


length(X2021_ParageneticAttributes_07OCT$Bio_NonAnthro)
length(nodes$Bio_NonAnthro[5657:5722])

visNetwork(nodes = nodes, edges = edges,height = "1500px", width = "100%",main = "Network of Mineralogy",submain = "Prabhu, Morrison, Williams, Hazen",footer = "Updated on 29th Sept,2021.") %>%
  visOptions(selectedBy = "Bio_NonAnthro",highlightNearest = TRUE, nodesIdSelection = list(enabled=TRUE))%>%
  visPhysics(stabilization = F)%>%
  visNodes(fixed = TRUE) %>%
  #visPhysics(solver = "barnesHut", barnesHut = list(gravitationalConstant = -80000,springConstant= 0.001,springLength=200),stabilization = FALSE)%>%
  visEdges(smooth = FALSE)%>%
  visLegend(useGroups = F,addNodes = unique(legendNodes))%>%
  #visOptions(collapse = TRUE)#%>%
  visSave(file = "All_Min_Interactive_Network_Bio_NonAnthro.html")

#_________________________________________________________________________________

#Mars and Moon 

nodes$Moon<-"2"
nodes$Mars<-"2"
nodes$biotic<-"2"

nodes$Moon[5657:5722]<-X2021_ParageneticAttributes_07OCT$Moon

nodes$Mars[5657:5722]<-X2021_ParageneticAttributes_07OCT$Mars
nodes$biotic[5657:5722]<-X2021_ParageneticAttributes_07OCT$Biotic

nodes$Elements_List<-"00"
nodes$Elements_List[1:5884] <- Elements_in_Minerals

nodes$Elements_List[5883:5888]


visNetwork(nodes = nodes, edges = edges,height = "1500px", width = "100%",main = "Network of Mineralogy",submain = "Prabhu, Morrison, Williams, Hazen",footer = "Updated on 29th Sept,2021.") %>%
  visOptions(selectedBy = "Mars",highlightNearest = TRUE, nodesIdSelection = list(enabled=TRUE))%>%
  visPhysics(stabilization = F)%>%
  visNodes(fixed = TRUE) %>%
  #visPhysics(solver = "barnesHut", barnesHut = list(gravitationalConstant = -80000,springConstant= 0.001,springLength=200),stabilization = FALSE)%>%
  visEdges(smooth = FALSE)%>%
  visLegend(useGroups = F,addNodes = unique(legendNodes))%>%
  #visOptions(collapse = TRUE)#%>%
  visSave(file = "All_Min_Interactive_Network_Mars.html")

visNetwork(nodes = nodes, edges = edges,height = "1500px", width = "100%",main = "Network of Mineralogy",submain = "Prabhu, Morrison, Williams, Hazen",footer = "Updated on 29th Sept,2021.") %>%
  visOptions(selectedBy = "Moon",highlightNearest = TRUE, nodesIdSelection = list(enabled=TRUE))%>%
  visPhysics(stabilization = F)%>%
  visNodes(fixed = TRUE) %>%
  #visPhysics(solver = "barnesHut", barnesHut = list(gravitationalConstant = -80000,springConstant= 0.001,springLength=200),stabilization = FALSE)%>%
  visEdges(smooth = FALSE)%>%
  visLegend(useGroups = F,addNodes = unique(legendNodes))%>%
  #visOptions(collapse = TRUE)#%>%
  visSave(file = "All_Min_Interactive_Network_Moon.html")

visNetwork(nodes = nodes, edges = edges,height = "1500px", width = "100%",main = "Network of Mineralogy",submain = "Prabhu, Morrison, Williams, Hazen",footer = "Updated on 29th Sept,2021.") %>%
  visOptions(selectedBy = "biotic",highlightNearest = TRUE, nodesIdSelection = list(enabled=TRUE))%>%
  visPhysics(stabilization = F)%>%
  visNodes(fixed = TRUE) %>%
  #visPhysics(solver = "barnesHut", barnesHut = list(gravitationalConstant = -80000,springConstant= 0.001,springLength=200),stabilization = FALSE)%>%
  visEdges(smooth = FALSE)%>%
  visLegend(useGroups = F,addNodes = unique(legendNodes))%>%
  #visOptions(collapse = TRUE)#%>%
  visSave(file = "All_Min_Interactive_Network_Biotic.html")

#____________________________________________________________________
# Elements

visNetwork(nodes = nodes, edges = edges,height = "1500px", width = "100%",main = "Network of Mineralogy",submain = "Prabhu, Morrison, Williams, Hazen",footer = "Updated on 19th Dec,2022.") %>%
  visOptions(selectedBy = `Periodic%20Table%20of%20Elements`$Symbol,highlightNearest = TRUE, nodesIdSelection = list(enabled=TRUE))%>%
  visPhysics(stabilization = F)%>%
  visNodes(fixed = TRUE) %>%
  #visPhysics(solver = "barnesHut", barnesHut = list(gravitationalConstant = -80000,springConstant= 0.001,springLength=200),stabilization = FALSE)%>%
  visEdges(smooth = FALSE)%>%
  visLegend(useGroups = F,addNodes = unique(legendNodes))%>%
  #visOptions(collapse = TRUE)#%>%
  visSave(file = "All_Min_Interactive_Network_Elements.html")
