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

hist(rowSums(diplomats), main = 'Histogram of Ties',xlab = 'Countries')
hist(nodelist$nbidirectional, main='Histogram of Reciprocated Ties',xlab = 'Countries')


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
