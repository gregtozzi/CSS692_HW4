source('helper_functions.R')

# Load and plot the graph
hiTech <- import_pajek('Hi-tech.net')
hiTech <- as.undirected(hiTech, mode = 'mutual')
plot(hiTech,
     edge.arrow.size = 0.2,
     vertex.label.family = "Futura Bold",
     vertex.label.color = 'white',
     vertex.frame.color = 'white',
     vertex.color = '#231F20',
     vertex.label.cex = 0.7)

# Add union classes
unionClass <- read.csv('Hi-techUnionClass.csv')$union
V(hiTech)$unionClass <- unionClass

# Assign vertex colors by class
classColors <- unionColors[V(hiTech)$unionClass + 1]

# Recolor Chris and Ovid
reColor <- which(V(hiTech)$label %in% c('Chris', 'Ovid'))
classColors[reColor] <- '#01ABCE'
V(hiTech)$classColor <- classColors

plot(hiTech,
     edge.arrow.size = 0.2,
     vertex.label.family = "Futura Bold",
     vertex.label.color = 'white',
     vertex.frame.color = 'white',
     vertex.color = V(hiTech)$classColor,
     vertex.label.cex = 0.7)

# Identify cliques
hiTechCliques <- max_cliques(hiTech, min = 3)
cliqueAnalysis <- sapply(hiTechCliques, function(x) ego_analysis(hiTech, x)) %>% t
cliqueAnalysisCollapse <- data.frame(cliqueAnalysis[,2], (cliqueAnalysis[,3] + cliqueAnalysis[,4]))
names(cliqueAnalysisCollapse) <- c('pro', 'anti')
mixedCliques <- which(cliqueAnalysisCollapse[,1] != 0 & cliqueAnalysisCollapse[,2] != 0)

# Plot mixed cliques
sapply(mixedCliques, function(x) clique_plot(hiTech, hiTechCliques[[x]]))

# Compute the co-clique matrix
CCM <- matrix(0, nrow = length(V(hiTech)),
              ncol = length(hiTechCliques))
for(i in 1:length(hiTechCliques)) {
  CCM[hiTechCliques[[i]], i] <- 1
}
CCMOutput <- data.frame(V(hiTech)$label, rowSums(CCM))
# Plot Chris' ego network
chrisEgo <- ego(hiTech, 1, nodes = 29)[[1]]
chrisNet <- subgraph(hiTech, chrisEgo)

plot(chrisNet,
     edge.arrow.size = 0.2,
     vertex.label.family = "Futura Bold",
     vertex.label.color = 'white',
     vertex.frame.color = 'white',
     vertex.color = V(chrisNet)$classColor,
     vertex.label.cex = 0.7,
     layout = layout_with_fr)

# Analyze ego nets
Ego <- ego(hiTech, 1)
egoAnalysis <- sapply(Ego, function(x) ego_analysis(hiTech, x)) %>% t
egoAnalysisCollapse <- data.frame(egoAnalysis[,2], (egoAnalysis[,3] + egoAnalysis[,4]))
names(egoAnalysisCollapse) <- c('pro', 'anti')
mixedNets <- which(egoAnalysisCollapse[,1] != 0 & egoAnalysisCollapse[,2] != 0)

# Plot the heterogeneous nets
sapply(mixedNets, function(x) plot_ego_net(hiTech, Ego, x))



