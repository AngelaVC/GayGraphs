########################################################################
# This outputs data from octave trials into several graphs
# To run this type
# source('/Users/angelavierling-claassen/Documents/DataScience/GayGraphs/gaygraph_viz.R')
# Set name of trial below before using
########################################################################
library(igraph) 
library('cluster')
library('animation')

# Set names for trial -- change this each time
trialname='for-talk-data-2012-1-3-14-34'

# Set directory which contains files -- change as needed
setwd('/Users/angelavierling-claassen/Documents/DataScience/GayGraphs/DataFromTrials')

#####################################
# Create all for the ending data
#####################################
startend='end'

# read in the graph edges data and the nodes data
# change filenames as needed 
gaygraph_data_frame<-read.table(paste(trialname,'-edges-',startend,'.txt',sep=""))
nodes<-read.table(paste(trialname,'-nodes-',startend,'.txt',sep=""))

# Make the names of columns for edges and notes
colnames(gaygraph_data_frame) <- c('ego', 'alter', 'family', 'friend')
colnames(nodes)<-c('ego','opin','gay')

# Eliminate zero edges
gaygraph_nonzero_edges <- subset(gaygraph_data_frame,(family > 0 | friend > 0 ))

# Make the data as a graph object, include nodes (vertices), method of 'each' will 
# simply replace each directed edge with an undirected edge (and keeps edge attributes)
gaygraph <- as.undirected(graph.data.frame(d=gaygraph_nonzero_edges,vertices=nodes), mode='collapse')

# do walktrap algorithm to determine communities and add this to nodes
community_wt <- walktrap.community(gaygraph, steps=4,modularity=TRUE,labels=TRUE)

# Simple graph with a layout, vertex colors
fullGraph_layout<- layout.kamada.kawai(gaygraph)

# Color vertices red if gay
gay_vertex_colors = get.vertex.attribute(gaygraph,"opin")
colors = c('Black', 'gray48','gray78','gray88','gray98','Red')
gay_vertex_colors[gay_vertex_colors < .2] = colors[1]
gay_vertex_colors[gay_vertex_colors < .4] = colors[2]
gay_vertex_colors[gay_vertex_colors < .6] = colors[3]
gay_vertex_colors[gay_vertex_colors < .8] = colors[4]
gay_vertex_colors[gay_vertex_colors < 1] = colors[5]
gay_vertex_colors[gay_vertex_colors == 1] = colors[6]

# Make the tie types colored by friend or family
tie_type_colors = c('Blue', 'Green')
E(gaygraph)$color[ E(gaygraph)$family==1 ] = tie_type_colors[1]
E(gaygraph)$color[ E(gaygraph)$friend==1 ] = tie_type_colors[2]
E(gaygraph)$arrow.size=.5
V(gaygraph)$color = gay_vertex_colors
V(gaygraph)$frame = gay_vertex_colors

# Plot the graph
jpeg(paste(trialname,'-',startend,'.jpg',sep=""))
plot(gaygraph, 
     layout=fullGraph_layout, 
     vertex.color=gay_vertex_colors, 
     vertex.label=NA,
     edge.arrow.size=.5)
dev.off()

###############################
## Next try looking at just gay nodes
gay_only_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "gay")==0])
gay_only_graph
gay_layout <- layout.fruchterman.reingold(gay_only_graph)
jpeg(paste(trialname,'-',startend,'-gayonly','.jpg',sep=""))
plot(gay_only_graph, layout=gay_layout)
dev.off()

################################
## Look at subgraph of same number of straight nodes
straight_only_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "gay")==1])
straight_only_graph<- delete.vertices(straight_only_graph, V(straight_only_graph)[1:length(V(straight_only_graph))-length(V(gay_only_graph))-1])
jpeg(paste(trialname,'-',startend,'-straightonly','.jpg',sep=""))
straight_layout <- layout.fruchterman.reingold(straight_only_graph)
plot(straight_only_graph, layout=straight_layout)
dev.off()


################################3
## Next try for graphs of communities
comm0_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "membership")!=0])
community_layout <- layout.fruchterman.reingold(comm0_graph)
jpeg(paste(trialname,'-',startend,'-comm0','.jpg',sep=""))
	plot(comm0_graph, layout=community_layout)
	dev.off()

comm1_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "membership")!=1])
community_layout <- layout.fruchterman.reingold(comm1_graph)
jpeg(paste(trialname,'-',startend,'-comm1','.jpg',sep=""))
	plot(comm1_graph, layout=community_layout)
	dev.off()

comm2_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "membership")!=2])
community_layout <- layout.fruchterman.reingold(comm2_graph)
jpeg(paste(trialname,'-',startend,'-comm2','.jpg',sep=""))
	plot(comm2_graph, layout=community_layout)
	dev.off()

comm3_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "membership")!=3])
community_layout <- layout.fruchterman.reingold(comm3_graph)
jpeg(paste(trialname,'-',startend,'-comm3','.jpg',sep=""))
	plot(comm3_graph, layout=community_layout)
	dev.off()

comm4_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "membership")!=3])
community_layout <- layout.fruchterman.reingold(comm4_graph)
jpeg(paste(trialname,'-',startend,'-comm4','.jpg',sep=""))
	plot(comm4_graph, layout=community_layout)
	dev.off()



#####################################
# Repeat all for the starting data
#####################################
startend='start'

# read in the graph edges data and the nodes data
# change filenames as needed 
gaygraph_data_frame<-read.table(paste(trialname,'-edges-',startend,'.txt',sep=""))
nodes<-read.table(paste(trialname,'-nodes-',startend,'.txt',sep=""))

# Make the names of columns for edges and notes
colnames(gaygraph_data_frame) <- c('ego', 'alter', 'family', 'friend')
colnames(nodes)<-c('ego','opin','gay')

# Eliminate nonzero edges
gaygraph_nonzero_edges <- subset(gaygraph_data_frame,(family > 0 | friend > 0 ))

# Make the data as a graph object, include nodes (vertices), method of 'each' will 
# simply replace each directed edge with an undirected edge (and keeps edge attributes)
gaygraph <- as.undirected(graph.data.frame(d=gaygraph_nonzero_edges,vertices=nodes), mode='collapse')

# do walktrap algorithm to determine communities and add this to nodes
community_wt <- walktrap.community(gaygraph, steps=4,modularity=TRUE,labels=TRUE)


# Simple graph with a layout, vertex colors
fullGraph_layout<- layout.kamada.kawai(gaygraph)

# Color vertices red if gay
gay_vertex_colors = get.vertex.attribute(gaygraph,"opin")
colors = c('Black', 'gray48','gray78','gray88','gray98','Red')
gay_vertex_colors[gay_vertex_colors < .2] = colors[1]
gay_vertex_colors[gay_vertex_colors < .4] = colors[2]
gay_vertex_colors[gay_vertex_colors < .6] = colors[3]
gay_vertex_colors[gay_vertex_colors < .8] = colors[4]
gay_vertex_colors[gay_vertex_colors < 1] = colors[5]
gay_vertex_colors[gay_vertex_colors == 1] = colors[6]

# Make the tie types colored by friend or family
tie_type_colors = c('Blue', 'Green')
E(gaygraph)$color[ E(gaygraph)$family==1 ] = tie_type_colors[1]
E(gaygraph)$color[ E(gaygraph)$friend==1 ] = tie_type_colors[2]
E(gaygraph)$arrow.size=.5
V(gaygraph)$color = gay_vertex_colors
V(gaygraph)$frame = gay_vertex_colors

# Plot the graph
jpeg(paste(trialname,'-',startend,'.jpg',sep=""))
plot(gaygraph, 
     layout=fullGraph_layout, 
     vertex.color=gay_vertex_colors, 
     vertex.label=NA,
     edge.arrow.size=.5)
dev.off()

###############################
## Next try looking at just gay nodes
gay_only_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "gay")==0])
gay_only_graph
gay_layout <- layout.fruchterman.reingold(gay_only_graph)
jpeg(paste(trialname,'-',startend,'-gayonly','.jpg',sep=""))
plot(gay_only_graph, layout=gay_layout)
dev.off()

################################
## Look at subgraph of same number of straight nodes
straight_only_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "gay")==1])
straight_only_graph<- delete.vertices(straight_only_graph, V(straight_only_graph)[1:length(V(straight_only_graph))-length(V(gay_only_graph))-1])
jpeg(paste(trialname,'-',startend,'-straightonly','.jpg',sep=""))
straight_layout <- layout.fruchterman.reingold(straight_only_graph)
plot(straight_only_graph, layout=straight_layout)
dev.off()


################################3
## Next try for graphs of communities
comm0_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "membership")!=0])
community_layout <- layout.fruchterman.reingold(comm0_graph)
jpeg(paste(trialname,'-',startend,'-comm0','.jpg',sep=""))
	plot(comm0_graph, layout=community_layout)
	dev.off()

comm1_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "membership")!=1])
community_layout <- layout.fruchterman.reingold(comm1_graph)
jpeg(paste(trialname,'-',startend,'-comm1','.jpg',sep=""))
	plot(comm1_graph, layout=community_layout)
	dev.off()

comm2_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "membership")!=2])
community_layout <- layout.fruchterman.reingold(comm2_graph)
jpeg(paste(trialname,'-',startend,'-comm2','.jpg',sep=""))
	plot(comm2_graph, layout=community_layout)
	dev.off()

comm3_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "membership")!=3])
community_layout <- layout.fruchterman.reingold(comm3_graph)
jpeg(paste(trialname,'-',startend,'-comm3','.jpg',sep=""))
	plot(comm3_graph, layout=community_layout)
	dev.off()

comm4_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "membership")!=3])
community_layout <- layout.fruchterman.reingold(comm4_graph)
jpeg(paste(trialname,'-',startend,'-comm4','.jpg',sep=""))
	plot(comm4_graph, layout=community_layout)
	dev.off()







