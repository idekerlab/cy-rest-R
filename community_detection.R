#
# cyRest Demo 1:
#   Community detection and visualization with igraph and Cytoscape
#
# Based on a tutorial by Dr. Kazuhiro Takemoto
# Extended by Keiichiro Ono
#

library(igraph)
library(RJSONIO)
library(httr)

source("toCytoscape.R")

# Basic settings for cyREST
port.number = 1234
base.url = paste("http://localhost:", toString(port.number), "/v1", sep="")

communityEdge <- function(g) {
  edges <- get.edgelist(g)
  edgeCount <- ecount(g)
  
  edgeComs <- list()
  for(i in 1:edgeCount) {
    source <- edges[i,1]
    target <- edges[i,2]
    
    sourceCom <- V(g)[source]$community
    targetCom <- V(g)[target]$community
    
    if(sourceCom == targetCom) {
      edgeComs[[i]] <- sourceCom
    }
  }
  return(edgeComs)
}

## Non-overlapping

# Load SIF file
brcaTable <- read.table("yeastHighQuality.sif")

# Convert it to simple edge list
brcaGraph <- brcaTable[c(1,3)]

# To igraph object
g0 <- graph.data.frame(brcaGraph,directed=F)

# Extract componentes (individual connected regions)
components <- decompose.graph(g0)

# Pick largest subgraph
largestSubgraph <- components[[which.max(sapply(components, vcount))]]

# Remove duplicate edges
g <- simplify(largestSubgraph, remove.multiple=T,remove.loops=T)

# Find community
coms <- fastgreedy.community(g)

# Save it as node attribute
V(g)$community <- coms$membership
V(g)$color <- coms$membership
E(g)$community <- communityEdge(g)

cyjs <- toCytoscape(g)
network.url = paste(base.url, "networks", sep="/")
res <- POST(url=network.url, body=cyjs, encode="json")
network.suid = unname(fromJSON(rawToChar(res$content)))

# Visualize network
plot(g,vertex.size=5,vertex.label=V(g)$name,layout=layout.kamada.kawai)

dend<-as.dendrogram(coms,use.modularity=F)
plot(dend)
