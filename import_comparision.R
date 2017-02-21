library(igraph)

# Load and plot the graph using Meng-Hao's method
hiTech <- read_graph('Hi-tech.net', format = 'pajek')
hiTech <- as.directed(hiTech, 'arbitrary')
jpeg(filename="Hi-tech_plot_1.jpg")
plot(hiTech, edge.arrow.size = 0.3)
dev.off()

importPajek <- function(file) {
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

# Load and plot the graph using the modifications to Matt's
# method
hiTechCorrect <- importPajek('Hi-tech.net')
jpeg(filename="Hi-tech_plot_2.jpg")
plot(hiTechCorrect, edge.arrow.size = 0.3)
dev.off()
