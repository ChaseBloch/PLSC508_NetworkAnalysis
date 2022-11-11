---
title: "PLSC508_MethodsTutorial_Reciprocity"
author: "Chase Bloch"
date: "February 3, 2020"
---

#These are all of the packages we will need for this tutorial. If the installations are not working you may have to update R.

#install.packages("statnet",dependencies=TRUE)
#install.packages('ggplot2')
#install.packages('ggnetwork',dependencies = TRUE)
#install.packages('igraph',dependencies = TRUE)
#install.packages('intergraph',dependencies = TRUE)


require(statnet)
require(ggplot2)
require(ggnetwork)

set.seed(8675309) # set a random number seed so that visualizations appear the same each time the script runs


#The data used for this project is from a paper by Brandon Kinne titled: *Dependent Diplomacy: Signaling, Strategy, and Prestige in the Diplomatic Network*. The data that we will be using for this tutorial tracks whether a country had a diplomat stationed in another country. Note that reciprocity is only relevant when a network has directionality, which makes using diplomatic presence useful as it is not always reciprocated.

load("./ISQ-Replication/Data/data.world")

#There are a lot of adjacency matrices in the dataset for multiple years and variables. The first step is to extract the one we will be using for this tutorial, which is diplomatic presence in the year 2000.

diplomats<-as.data.frame(data$diplomat$diplomat2000)

#This line renames the columns of the dataframe so that when/if it gets re-ordered we can still tell what the countries are.

names(diplomats)<-data$cownames


#Some countries that previously existed in the Correlates of War data are not around in the year 2000 and are filled in with NA values. Since we are working with an adjancency matrix, this means there are both entire rows and columns with all NA values. Therefore, using the base R na.comit command will cause us to lose all of the dataframe. The following lines delete NA values by rows and then columns only if more than 80% of the column or row is filled with NA values. Using this method we only lose six observations.

#Remove NAs by row
diplomats <- diplomats[!rowSums(is.na(diplomats)) > ncol(diplomats)*.8,]
#Remove NAs by column
diplomats <- diplomats[,!colSums(is.na(diplomats)) > nrow(diplomats)*.8]
diplomats[1:5,1:5]

#With our data imported and cleaned, it is time to establish the network object. Note that the 'directed' option is set to TRUE to indicate that there is directionality in this network.

net1 <- network(as.matrix(diplomats),matrix.type = 'adjacency',directed = TRUE,na.omit=TRUE)


#In the next three blocks of code I establish the network attributes that we wil be using thorughout the rest of the tutorial. The first block of code gives the number of bidirectional edges in the network. I could not find a command to do this, so instead I conducted element-wise multiplication of the adjacency matrix and its inverse. Due to the symmetry of the adjacency matrix this means that any non-bidirectional relationship between two countries will have a zero in the place of both countries. I then sum across rows to obtain the total number of bidirectional relationships for each country. Finally, I add this attribute to the network object's vertex.attribute.

#Convert diplomats dataframe to matrix object
mat_dip<-as.matrix(diplomats)
#Initialize a dataframe to store vertex attributes
nodelist<-as.data.frame(matrix(nrow=length(diplomats)))
#Element-wise multiplication of adjacency matrix and it's inverse
nodelist$nbidirectional<-rowSums(mat_dip*t(mat_dip))
#Add to vertex.attribute
network::set.vertex.attribute(net1, 'nbidirectional',nodelist$nbidirectional )


#We will have too many country names to fit on the network figure, so here I am limiting the number of labels to those countries that have more than 100 bidirectional diplomatic relationships.

#Add all of the names to the attribute dataframe
nodelist$names = names(diplomats)
#Include only those names of countries that have more than 100 bidirectional relationships.
nodelist$label <- ifelse(nodelist$nbidirectional >= 100, nodelist$names, NA)
#Add those names as a new vetex attribute names 'labels'.
network::set.vertex.attribute(net1, 'labels', nodelist$label)


#Now we are going to begin graphing this network. I create one figure using ggplot and statnet and another using igraph and base R's plotting functions to give you an idea of the trade-offs of each when working with directional networks. The first step with ggplot is to construct an object that contains the X and Y formats for a specified node placement algorithm. Here I use kamadakawai becuase I tested it and thought it looked the best. 

#Note that in the graphs below, the nodes will represent countries and the edges represnt the presence of diplomats.

net1_fr <- ggnetwork(net1, layout = 'kamadakawai')



#Base plot using statnet and ggplot
ggplot(data=net1_fr,aes(x, y, xend=xend, yend=yend))+
geom_edges(color="grey")+
geom_nodes(color="blue")+
geom_nodetext_repel(aes(label=labels))+
theme_blank()+
labs(title = 'Network of Presence of Diplomats')



#The network is extremely dense and it is difficult to tell what is happening. To help remedy this we can use our vertex attribute of the number of bidirectional relationships to create a color gradient for the nodes. After doing this, it becomes more obvious that the most connected nodes are in the center of the matrix, visually this gradient is showing something similar to centrality.

#Additionally, this is a directional network, but the above figure does not indicate this in anyway. We can fix this by using the 'arrow' option for geom_edges. In the graph below, we can now sort of see if two countries have a unidriectional or bidrectional diplomatic relationships. Those edges with two arrows represent didirectional relationships and those edges with one arrow represent unidirectional relationships.

ggplot(data=net1_fr,aes(x, y, xend=xend, yend=yend))+
#The line below adds arrows
geom_edges(color="grey", arrow = arrow(length = unit(0.3, "lines"), type = "open"))+
#The line below adds the gradient
geom_nodes(aes(color = nbidirectional))+
geom_nodetext_repel(aes(label=labels))+
theme_blank()+
labs(title = 'Network of Presence of Diplomats', color = 'Number of Bidirectional Edges')


#This plot looks nice and is close to aesthetically pleasing it can get with how dense the network is. It could, however, display more information based on the characteristic of the edges. Specifically, my idea is to color code the edges based on their directionality (bidirectional or unidirectional). I could not figure out a way to do this with statnet and ggplot, so we are going to move over to igraph and base R. The first step is convert our statnet network object to an igraph graph object.

require(igraph)
require(intergraph)

net2 <- asIgraph(net1)


#The next block of code creates our new figure. 

#Establish the color of the edges
E(net2)$color <- "grey50"
#Establish the color of the edges if the edge is bidirectional
E(net2)$color[is.mutual(net2)] = "grey"
#Resolution of the gradient
fine = 500
#Color scale of the gradient
pal = colorRampPalette(c('blue','red'))
plot(net2, 
edge.width = E(net2)$weight,
layout=layout_with_kk,
vertex.size=3,
vertex.label=nodelist$label,
vertex.label.dist=1,
vertex.label.degree=pi,
vertex.label.color="black",
vertex.label.cex=.7,
vertex.color = pal(fine)[as.numeric(cut(vertex_attr(net2,"nbidirectional"),breaks = fine))],
edge.arrow.size=0.3,
main="Network of Presence of Diplomats")


#Lighter grey represents bidirectional edges. We can loosely see that the center of the graph is mostly light grey and it becomes darker as you move out from the center. In general, however, this figure does not look as good. The labels are easier to maneuver with ggplot and I think that ggplot in general looks better. Finding a way to visualize edge characteristics in ggplot would be ideal.

#Finally we can move on to reciprocity. In a very basic sense, reciprocity occurs when there is a bidirectional node. In our example, this occurs if country A sends diplomats to country B and country B reciprocates by sending their own diplomats to country A. There are not many commands or measures for reciprocity built into R, basically there is one command in the 'igraph' package. 

#Default measure of reciprocity. Here is the definition from the documentation: It is most commonly defined as the probability that the opposite counterpart of a directed edge is also included in the graph. Or in adjacency matrix notation: sum(i, j, (A. X A')ij) / sum(i, j, Aij), where A. X A' is the element-wise product of matrix A and its transpose.

reciprocity(net2)

#The other allowed method is a simple ratio of the number of bidirectional edges over the total number of edges. A loop occurs when there is more than one path leading to and from the same node. They are ignored by default, but I change it to false to show that there are no loops in this network.

reciprocity(net2,mode='ratio')
reciprocity(net2,mode='ratio',ignore.loops = FALSE)


#Finally, there is the measure of reciprocity discussed by Garlaschelli and Loffredo (2004). The main benefit of this measure is that it allows us to compare to the amount of reciprocity expected by random. The sna package has a command called 'grecip', which has an option, 'correlation', that calculates the GL measure of reciprocity.

grecip(diplomats,measure='correlation')

L_bi = sum(vertex_attr(net2,"nbidirectional")) #Number of bidirectional edges
L = gsize(net2) #Number of edges
r = L_bi/L #Reciprocity ratio
N = length(diplomats) #Length of adjacency matrix
a_bar = L/(N*(N-1)) #Ratio of observed to possible links
p = (r-a_bar)/(1-a_bar) #New ratio measure
print(c(r,p))

#This measure is a bit lower than the default method in 'igraph'. This suggests that the network still has a lot more reciprocity than expected at random. 
