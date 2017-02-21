unionColors <- c('#231F20',
                 '#AAE0FA',
                 '#F25822',
                 '#D41E4E')

import_pajek <- function(file) {
  require(network)
  require(igraph)
  require(intergraph)
  
  graphNetwork <- read.paj(file)
  graphIgraph <-asIgraph(graphNetwork)
  
  # Assign labels
  graphLabels <- sapply(graphNetwork[[3]], function(x) x$vertex.names)
  V(graphIgraph)$label <- graphLabels
  
  return(graphIgraph)
}

clique_plot <- function(graph, Clique) {
  subGraph <- subgraph(graph, Clique)
  plot(subGraph,
       vertex.label.family = "Futura Bold",
       vertex.label.color = 'white',
       vertex.frame.color = 'white',
       vertex.color = V(subGraph)$classColor,
       vertex.label.cex = 0.7,
       vertex.size = 40)
}

ego_analysis <- function(graph, Ego) {
  subGraph <- subgraph(graph, Ego)
  subUnion <- V(subGraph)$unionClass
  sapply(0:3, function(x) sum(subUnion == x, na.rm = T))
}

plot_ego_net <- function(graph, Ego, Node) {
  nodeEgo <- Ego[[Node]]
  subNet <- subgraph(hiTech, nodeEgo)
  
  plot(subNet,
       edge.arrow.size = 0.2,
       vertex.label.family = "Futura Bold",
       vertex.label.color = 'white',
       vertex.frame.color = 'white',
       vertex.color = V(subNet)$classColor,
       vertex.label.cex = 0.7,
       layout = layout_with_fr)
}