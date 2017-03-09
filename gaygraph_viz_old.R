# Create graphs for visualizations

# set seed for random processes
set.seed(617)

library(igraph) 
library('cluster')
library('animation')

# Set to directory which contains files
setwd('/Users/angelavierling-claassen/Documents/DataScience/GayGraphs/DataFromTrials')

# read in the graph edges data and the nodes data
gaygraph_data_frame<-read.table('edgesTrial14end.txt')
nodes<-read.table('nodesTrial14end.txt')

# Make the names of columns for edges and notes
colnames(gaygraph_data_frame) <- c('ego', 'alter', 'family', 'friend')
colnames(nodes)<-c('ego','opin','gay')

# Eliminate zero edges (that is, not family or friend)
gaygraph_nonzero_edges <- subset(gaygraph_data_frame,(family > 0 | friend > 0 ))

# Make the data a graph object, include nodes (vertices), method of 'each' will 
# simply replace each directed edge with an undirected edge (and keeps edge attributes)
gaygraph <- as.undirected(graph.data.frame(d=gaygraph_nonzero_edges,vertices=nodes), mode='collapse')

# do walktrap algorithm to determine communities and add this to nodes
wt <- cluster_walktrap(gaygraph, steps=4,modularity=TRUE,membership=TRUE)

# since last time I did this these functions changed, so having trouble with next line
#nodes<-data.frame(nodes[1:3], community_wt["membership"])
gaygraph <- as.undirected(graph.data.frame(d=gaygraph_nonzero_edges,vertices=nodes), mode='collapse')

# Simple graph with a layout, vertex colors
fullGraph_layout<- layout.kamada.kawai(gaygraph)

# Color vertices red if gay
# If not gay, darker colors are more accepting
gay_vertex_colors = get.vertex.attribute(gaygraph,"opin")
colors = c('Black', 'gray48','gray78','gray88','gray98','Red')
gay_vertex_colors[gay_vertex_colors < .2] = colors[1]
gay_vertex_colors[gay_vertex_colors < .4] = colors[2]
gay_vertex_colors[gay_vertex_colors < .6] = colors[3]
gay_vertex_colors[gay_vertex_colors < .8] = colors[4]
gay_vertex_colors[gay_vertex_colors < 1] = colors[5]
gay_vertex_colors[gay_vertex_colors == 1] = colors[6]

# Make the tie types colored by friend or family
# Blue = Family
tie_type_colors = c('Blue', 'Green')
E(gaygraph)$color[ E(gaygraph)$family==1 ] = tie_type_colors[1]
E(gaygraph)$color[ E(gaygraph)$friend==1 ] = tie_type_colors[2]
E(gaygraph)$arrow.size=.5
V(gaygraph)$color = gay_vertex_colors
V(gaygraph)$frame = gay_vertex_colors

# Plot the graph
jpeg("trial14end.jpg")
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
jpeg("trial14_gayonly_end.jpg")
plot(gay_only_graph, layout=gay_layout)
dev.off()

################################
## Look at subgraph of same number of straight nodes
straight_only_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "gay")==1])
straight_only_graph<- delete.vertices(straight_only_graph, V(straight_only_graph)[1:length(V(straight_only_graph))-length(V(gay_only_graph))])
jpeg("trial14_straightonly_end.jpg")
straight_layout <- layout.fruchterman.reingold(straight_only_graph)
plot(straight_only_graph, layout=straight_layout)
dev.off()


################################3
## Next try for graphs of communities
comm0_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "membership")!=0])
community_layout <- layout.fruchterman.reingold(comm0_graph)
jpeg("trial14_comm0_end.jpg")
	plot(comm0_graph, layout=community_layout)
	dev.off()

comm1_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "membership")!=1])
community_layout <- layout.fruchterman.reingold(comm1_graph)
jpeg("trial14_comm1_end.jpg")
	plot(comm1_graph, layout=community_layout)
	dev.off()

comm2_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "membership")!=2])
community_layout <- layout.fruchterman.reingold(comm2_graph)
jpeg("trial14_comm2_end.jpg")
	plot(comm2_graph, layout=community_layout)
	dev.off()

comm3_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "membership")!=3])
community_layout <- layout.fruchterman.reingold(comm3_graph)
jpeg("trial14_comm3_end.jpg")
	plot(comm3_graph, layout=community_layout)
	dev.off()

comm4_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "membership")!=3])
community_layout <- layout.fruchterman.reingold(comm4_graph)
jpeg("trial14_comm4_end.jpg")
	plot(comm4_graph, layout=community_layout)
	dev.off()





## Repeat it all for the data from start of run

# read in the graph edges data and the nodes data
gaygraph_data_frame<-read.table('edgesTrial14start.txt')
nodes<-read.table('nodesTrial14start.txt')

# Make the names of columns for edges and notes
colnames(gaygraph_data_frame) <- c('ego', 'alter', 'family', 'friend')
colnames(nodes)<-c('ego','opin','gay')

# Eliminate nonzero edges
gaygraph_nonzero_edges <- subset(gaygraph_data_frame,(family > 0 | friend > 0 ))

# Make the data as a graph object, include nodes (vertices), method of 'each' will 
# simply replace each directed edge with an undirected edge (and keeps edge attributes)
gaygraph <- as.undirected(graph.data.frame(d=gaygraph_nonzero_edges,vertices=nodes), mode='each')

# Simple graph with a layout, vertex colors
fullGraph_layout<- layout.kamada.kawai(gaygraph)

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
jpeg("trial14start.jpg")
plot(gaygraph, 
     layout=fullGraph_layout, 
     vertex.color=gay_vertex_colors, 
     vertex.label=NA,
     edge.arrow.size=.5)
dev.off()

###############################
## Next try looking at just gay nodes
gay_only_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "gay")==0])
gay_layout <- layout.fruchterman.reingold(gay_only_graph)
jpeg("trial14_gayonly_start.jpg")
plot(gay_only_graph, layout=gay_layout)
dev.off()

################################
## Look at subgraph of same number of straight nodes
straight_only_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "gay")==1])
straight_only_graph<- delete.vertices(straight_only_graph, V(straight_only_graph)[1:length(V(straight_only_graph))-length(V(gay_only_graph))])
jpeg("trial14_straightonly_start.jpg")
straight_layout <- layout.fruchterman.reingold(straight_only_graph)
plot(straight_only_graph, layout=straight_layout)
dev.off()




################################3
## Next try for graphs of communities
comm0_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "membership")!=0])
community_layout <- layout.fruchterman.reingold(comm0_graph)
jpeg("trial14_comm0_end.jpg")
	plot(comm0_graph, layout=community_layout)
	dev.off()

comm1_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "membership")!=1])
community_layout <- layout.fruchterman.reingold(comm1_graph)
jpeg("trial14_comm1_end.jpg")
	plot(comm1_graph, layout=community_layout)
	dev.off()

comm2_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "membership")!=2])
community_layout <- layout.fruchterman.reingold(comm2_graph)
jpeg("trial14_comm2_end.jpg")
	plot(comm2_graph, layout=community_layout)
	dev.off()

comm3_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "membership")!=3])
community_layout <- layout.fruchterman.reingold(comm3_graph)
jpeg("trial14_comm3_end.jpg")
	plot(comm3_graph, layout=community_layout)
	dev.off()

comm4_graph <- delete.vertices(gaygraph, V(gaygraph)[get.vertex.attribute(gaygraph,name = "membership")!=3])
community_layout <- layout.fruchterman.reingold(comm4_graph)
jpeg("trial14_comm4_end.jpg")
	plot(comm4_graph, layout=community_layout)
	dev.off()





