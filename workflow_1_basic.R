#
# cyREST example workflow
# 
#   Simple Community Detection
# * Perform statistical analysis
# * Detect communities
#
# 
# Non-overlapping community detection ###

library(RColorBrewer)
library(igraph)
library(RJSONIO)
library(httr)

# Utilities to use Cytoscape and R
source("cytoscape_util.R")
source("utils.R")


# Step 1: Network Data Preparation

# Load yeast network SIF file as Data Frame
yeast.table <- read.table("data/yeastHighQuality.sif")

# Convert it to simple edge list
yeast.table.edgelist <- yeast.table[c(1,3)]

# Convert DF to undirected igraph object
# This is a PPI network, so import as undirected.
g.original <- graph.data.frame(yeast.table.edgelist, directed=F)

# Extract componentes (individual connected subgraphs)
subgraphs <- decompose.graph(g.original)

# Pick largest subgraph
largest.subgraph <- subgraphs[[which.max(sapply(subgraphs, vcount))]]

# Remove duplicate edges
g <- simplify(largest.subgraph, remove.multiple=T, remove.loops=T)
g$name <- "Yeast network"


# Step 2: Basic statistical analysis

# Global Network Statistics
g$density <- graph.density(g) # Density
g$transitivity <- transitivity(g) # Transitivity

# Node statistics
V(g)$closeness <- closeness(g) # Closeness Centrarity
V(g)$degree <- degree(g) # Degree
V(g)$pagerank <- page.rank(g, directed = FALSE) # PageRank
V(g)$betweenness <- betweenness(g) # Betweenness Centrarity

# Edge statistics
E(g)$betweenness.edge <- edge.betweenness(g) # Edge Betweenness

# Step 3: Community Detection: Try multiple algorithms
communities.greedy <- fastgreedy.community(g)
communities.leading <- leading.eigenvector.community(g)
communities.label.propagation <- label.propagation.community(g)

V(g)$community.greedy <- communities.greedy$membership
V(g)$community.leading <- communities.leading$membership
V(g)$community.label.propagation<- communities.label.propagation$membership

# Generate color palet using number of communitie
communityToColors <- function(members, num.communities) {
  base.color <- "#AAAAAA"
  num.members <- length(members)
  colors <- array(base.color, dim=c(num.members))
  print("Color len:")
  print(length(colors))
  print("Num members:")
  print(length(members))
  # Split color space into number of communities
  color.pallet <- rainbow(num.communities)
  
  for(i in 1:num.members) {
    newcolor <- color.pallet[members[i]]
    print("Color VAL:")
    if(length(newcolor) == 0) {
      newcolor <- base.color
    }
    colors[i] <- newcolor
  }
  result <- array(sapply(colors,function(x){return(substring(x, 1, 7))}))
  return(result)
}

V(g)$colors.community.greedy <- communityToColors(communities.greedy$membership, length(communities.greedy))
V(g)$colors.community.leading <- communityToColors(communities.leading$membership, length(communities.leading))
V(g)$colors.community.label.propagation <- communityToColors(communities.label.propagation$membership, length(communities.label.propagation))

E(g)$community.greedy <- getCommunityEdge(g, V(g)$community.greedy)
E(g)$community.leading <- getCommunityEdge(g, V(g)$community.leading)
E(g)$community.label.propagation <- getCommunityEdge(g, V(g)$community.label.propagation)

E(g)$colors.community.greedy <- communityToColors(array(E(g)$community.greedy), length(communities.greedy))
E(g)$colors.community.leading <- communityToColors(array(E(g)$community.leading), length(communities.leading))
E(g)$colors.community.label.propagation <- communityToColors(array(E(g)$community.label.propagation), length(communities.label.propagation))

#for(i in 1:ecount(g)) {
#  newcolor <- color.pallet[edgecoms[i]]
#  print(newcolor)
#  if(length(newcolor) == 0) {
#    newcolor <- "#AAAAAA"
#  }
#  color.array.edge[i] <- newcolor
#}
#E(g)$color <- color.array.edge

cyjs <- toCytoscape(g)
network.url = paste(base.url, "networks", sep="/")
res <- POST(url=network.url, body=cyjs, encode="json")
network.suid = unname(fromJSON(rawToChar(res$content)))

# Build a custom Visual Style programatically
style.name = "CommunityGreedy3"

# Defaults
def.node.border.width <- list(
  visualProperty = "NODE_BORDER_WIDTH",
  value = 0
)

def.node.transparency <- list(
  visualProperty="NODE_TRANSPARENCY",
  value=200
)

def.edge.transparency <- list(
  visualProperty="EDGE_TRANSPARENCY",
  value=80
)

def.edge.width <- list(
  visualProperty="EDGE_WIDTH",
  value=1
)

def.network.background <- list(
  visualProperty = "NETWORK_BACKGROUND_PAINT",
  value = "black"
)

defaults <- list(
  def.node.border.width,
  def.edge.width,
  def.node.transparency,
  def.edge.transparency,
  def.network.background
)

# Mappings 
mappings = list()

# Color mappings
node.fill.color = list(
  mappingType="passthrough",
  mappingColumn="colors.community.greedy",
  mappingColumnType="String",
  visualProperty="NODE_FILL_COLOR"
)

edge.color = list(
  mappingType="passthrough",
  mappingColumn="colors.community.greedy",
  mappingColumnType="String",
  visualProperty="EDGE_STROKE_UNSELECTED_PAINT"
)

# Node Size Mapping
min.betweenness = min(V(g)$betweenness)
max.betweenness = max(V(g)$betweenness)

point1 = list(
  value=min.betweenness,
  lesser= "10.0",
  equal="10.0",
  greater="10.0"
)

point2 = list(
  value=max.betweenness,
  lesser="200.0",
  equal="200.0",
  greater="200.0"
)

node.size.continuous.points = list(point1, point2)

node.size = list(
  mappingType="continuous",
  mappingColumn="betweenness",
  mappingColumnType="Double",
  visualProperty="NODE_SIZE",
  points = node.size.continuous.points
)

mappings = list(node.fill.color, edge.color, node.size)

style <- list(title=style.name, defaults = defaults, mappings = mappings)
style.JSON <- toJSON(style)
style.url = paste(base.url, "styles", sep="/")
POST(url=style.url, body=style.JSON, encode = "json")

#apply.layout.url = paste(base.url, "apply/layouts/force-directed", toString(network.suid), sep="/")

apply.style.url = paste(base.url, "apply/styles", style.name, toString(network.suid), sep="/")
res <- GET(apply.style.url)

#apply.layout.url = paste(base.url, "apply/layouts/kamada-kawai", toString(network.suid), sep="/")
#res <- GET(apply.layout.url)

#apply.bundling.url = paste(base.url, "apply/edgebundling", toString(network.suid), sep="/")
#res <- GET(apply.bundling.url)

degree.dist <- degree.distribution(g)
plot(degree.dist)
