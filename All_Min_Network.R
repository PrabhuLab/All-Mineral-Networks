#Evolutionary System of Mineralogy Bipartite Network
#Author : Anirudh Prabhu
#Email : prabha2@rpi.edu
#Date : Sept 21st, 2021

library(readxl)
EvolSysMin_Feb2021 <- read_excel("DTDI_Data/2021-Hazen-ParageneticModes-AmMin-Supplement1-15APR.xlsx", 
                                 sheet = "2020-All_RRUFF_IMA_8.29.2020")

#Loading packages
library(ggplot2)
library(ggnetwork)
library(igraph)
library(intergraph)
library(GGally)

#Min_Bip_EvolSys<-EvolSysMin_Feb2021[,c(68,3:67)]
Min_Bip_EvolSys<-EvolSysMin_Feb2021[-c(5657:5663),]
Min_Bip_EvolSys$Lunar<-NULL
#Min_Bip_EvolSys$`Mineral Name`[which(duplicated(Min_Bip_EvolSys$`Mineral Name`))]

row.names(Min_Bip_EvolSys)<-Min_Bip_EvolSys$`Mineral Name`
Min_Bip_EvolSys$`Mineral Name` <- NULL
Min_Bip_EvolSys[is.na(Min_Bip_EvolSys)]<- 0
Min_Bip_Net<-graph_from_incidence_matrix(incidence = as.matrix(Min_Bip_EvolSys),directed = F)
#plot(Min_Bip_Net)

Min_Bip_Net<-simplify(graph = Min_Bip_Net,remove.multiple = T,remove.loops = T)
node_color <- c("#9b4e23", "green")

V(Min_Bip_Net)$nodetype<-as.factor(V(Min_Bip_Net)$type)
V(Min_Bip_Net)$node_color <- node_color[V(Min_Bip_Net)$nodetype]
V(Min_Bip_Net)$node_color
V(Min_Bip_Net)$type[5656:5659]



library(network)
Min_Bip_NetN<-asNetwork(Min_Bip_Net)
network.vertex.names(Min_Bip_NetN)


Min_Bip_NetN %v% "node_color"
Min_Bip_NetN

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
#Paragenetic modes p34 to p37 -- Highly evolved igneous minerals -- Darker Blue
#Paragenetic modes p38 to p43 -- Plate tectonics minerals -- Bright purple
#Paragenetic modes p44 to p47 -- Early Biosphere minerals -- Lighter yellow green
#Paragenetic modes p48 to p53 -- Phanerozoic biominerals -- Darker green
#Paragenetic modes p54 to p57 -- Anthropogenic minerals -- Reddish violet

length(V(Min_Bip_Net)$node_color)
V(Min_Bip_Net)$node_color[5657:length(V(Min_Bip_Net)$node_color)]<-PM_List_Color

V(Min_Bip_Net)$node_color[5657:length(V(Min_Bip_Net)$node_color)]

library(network)
Min_Bip_NetN<-asNetwork(Min_Bip_Net)
network.vertex.names(Min_Bip_NetN)

df$edge_color<-""

#df$edge_color[df$to %in% names(PM_List_Color)]
PM_List_Color[match(df$to,names(PM_List_Color))]
df$edge_color<-PM_List_Color[match(df$to,names(PM_List_Color))]
df


Min_Bip_NetN %v% "node_color"
Min_Bip_NetN

#PM_List_Color


plot(ggnet2(Min_Bip_NetN, edge.size = 0.1, label = F, label.color = "black", label.size = 1,size = "degree", color = "node_color", edge.color = df$edge_color, color.legend = PM_List_Color)+
  guides(size = FALSE))

##Evolving Network
library(sna)
library(GGally)
library(network)
library(readr)

x = gplot.layout.fruchtermanreingold(Min_Bip_NetN, NULL)
Min_Bip_NetN %v% "x" = x[, 1]
Min_Bip_NetN %v% "y" = x[, 2]


#devtools::install_github("briatte/ggnet")
#library(ggnet)
b = theme(panel.background = element_rect(color = "white"))
z = guides(size = FALSE)

# PM Bursts only
png(file="DTDI_Plots/AllMin_anim/All_Min_PMbursts%02d.png",width = 1600, height = 900) 
i<-66
f<-rep(0,66)
for(i in 1:66)
{
  aa<-names(PM_List_Color)[i]
  ff<-Min_Bip_EvolSys[,aa]
  
  
  f[i]<-1
  fff<-c(ff[[1]],f)
  
  Min_Bip_NetN %v% aa <- fff
  
  Min_Bip_NetN %v% aa = ifelse(Min_Bip_NetN %v% aa, 1, NA)
  
  temp=ggnet2(Min_Bip_NetN, edge.size = 0.1, label = F, mode = c("x","y"), label.color = "black", label.size = 1,size = "degree", color = "node_color", na.rm = aa)+z+ggtitle(paste("PM = ",aa,sep = " "))
  plot(temp)
  
}
dev.off()
aa<-names(PM_List_Color)[i]
Min_Bip_EvolSys[,aa]

nrow(Min_Bip_EvolSys[,aa])
length(Min_Bip_NetN %v% "p1")

Min_Bip_NetN %v% "color"

#___________________________________________________________________
library(networkDynamic)
library(ndtv)
library(dplyr)

SimulationA <- getTableHosts(test.nosoiA, "A")
SimulationA$numID <- as.numeric(as.factor(SimulationA$hosts.ID)) #gets a unique numeric ID for each host
SimulationA[is.na(out.time),out.time:=(test.nosoiA$total.time)] #active hosts get a "end time" (i.e. the end of the simulation)

#Create "edges" table (line between nodes)
edges = SimulationA[,c("hosts.ID","inf.by","inf.time","out.time")]
colnames(edges) = c("to","from","infected","recovered")
edges$end = rep(test.nosoiA$total.time+1,nrow(edges))

#Create "vertex" table (nodes)
vertex2 = edges %>% group_by(to) %>% mutate(vertex=to)
vertex2 = vertex2[,c("vertex","infected","recovered","end")]
vertex2 = as.data.frame(vertex2)
vertex2$numericID = c(1:nrow(vertex2))
vertex2$infected = as.numeric(vertex2$infected)
vertex2$recovered = as.numeric(vertex2$recovered)
vertex2$end = as.numeric(vertex2$end)

vertex3=vertex2[,c(1,5)]
colnames(vertex3) = c("to","numeric.ID")

test=left_join(edges, vertex3, by=c("to"))
colnames(vertex3) = c("from","numeric.ID")

edges2=left_join(test, vertex3, by=c("from"))
colnames(edges2) = c("to","from","infected","recovered","end","to.num","from.num")

edges2[1,7] = 1

#Create main network using networkDynamic
test.network = networkDynamic(vertex.spells=vertex2[,c(2,4,5)],edge.spells =edges2[,c(3,5,7,6)])

#Animated plot where "active" hosts are red, inactive gray
activate.vertex.attribute(test.network,'color','gray',onset=0,terminus=47)
activate.vertex.attribute(test.network,'color','red',onset=vertex2$infected,terminus=vertex2$recovered)

#Render animation
library(tsna)
saveVideo(render.animation(test.network,vertex.col = "color",
                           render.par=list(show.time=TRUE),
                           edge.col = "gray60",
                           displaylabels=FALSE, render.cache='none',mode="kamadakawai"),video.name="transmission-nosoi.mp4")




