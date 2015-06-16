#source("http://Bioconductor.org/biocLite.R")
# cyRest Demo 2:
#   Community detection and visualization with igraph and Cytoscape
#
# by Keiichiro Ono (kono at uscd edu)

library(RColorBrewer)
library(linkcomm)
library(igraph)
library(RJSONIO)
library(httr)
library(biomaRt)

# Utilities to use Cytoscape and R
source("utility/cytoscape_util.R")

########## Network Data Preparation ###########
# Download HumanNet: Get description of data

# 1. Prepare column names
url.description <- "http://www.functionalnet.org/humannet/HumanNet.v1.evidence_code.txt"
file.description <- basename(url.description)
download.file(url.description, file.description)
humannet.columns <- read.table(file.description, sep = "=", fill=TRUE)
column.names <- sapply(humannet.columns[[1]], function(x) {sub("^\\s+", "", x)})
column.names <- c("gene1", "gene2", column.names)

# 2. Load network
url.humannet <- "http://www.functionalnet.org/humannet/HumanNet.v1.join.txt"
file.humannet <- basename(url.humannet)
download.file(url.humannet, file.humannet)
humannet.table <- read.table(file.humannet, comment.char = "!",sep = "\t", fill=TRUE )
humannet.graph <- graph.data.frame(humannet.table, directed=F)
humannet.graph.simple <- simplify(humannet.graph, remove.multiple=T,remove.loops=T)

colnames(humannet.table) <- column.names

# Extract unique genes
genes <- c(humannet.table[[1]], humannet.table[[2]])

# Annotate the network with Ensemble
ensembl_human = useMart("ensembl", dataset="hsapiens_gene_ensembl")
key="entrezgene"
columns <- c("entrezgene", "go_id", "name_1006", "chromosome_name", "band", "strand", "ensembl_gene_id", "hgnc_symbol", "description")
human.annotation <- getBM(attributes=columns, filters=key, values=genes, mart=ensembl_human)

## Non-overlapping community detection

# Load SIF file
#brcaTable <- read.table("data/yeastHighQuality.sif")
# Convert it to simple edge list
#brcaGraph <- brcaTable[c(1,3)]
# Convert data frame to igraph object
#originalGraph <- graph.data.frame(brcaGraph,directed=F)

# Extract componentes (individual connected regions)
components <- decompose.graph(humannet.graph.simple)
# Pick largest subgraph
g <- components[[which.max(sapply(components, vcount))]]

# Remove duplicate edges
#g <- simplify(largestSubgraph, remove.multiple=T,remove.loops=T)
# Find communities
coms <- fastgreedy.community(g)

# Set membership information to node attributes
V(g)$community <- coms$membership
V(g)$color <- coms$membership
E(g)$community <- getCommunityEdge(g)

cyjs <- toCytoscape(g)
network.url = paste(base.url, "networks", sep="/")
res <- POST(url=network.url, body=cyjs, encode="json")
network.suid = unname(fromJSON(rawToChar(res$content)))


# Build a custom Visual Style programatically
style.name = "Community"

# Defaults
def.node.border.width <- list(
  visualProperty = "NODE_BORDER_WIDTH",
  value = 0
)

def.edge.width <- list(
  visualProperty="EDGE_WIDTH",
  value=2
)

defaults <- list(def.node.border.width,def.edge.width)

node.label = list(
  mappingType="passthrough",
  mappingColumn="name",
  mappingColumnType="String",
  visualProperty="NODE_LABEL"
)

mappings = list(node.label)

style <- list(title=style.name, defaults = defaults, mappings = mappings)
style.JSON <- toJSON(style)

style.url = paste(base.url, "styles", sep="/")
POST(url=style.url, body=style.JSON, encode = "json")

apply.layout.url = paste(base.url, "apply/layouts/force-directed", toString(network.suid), sep="/")
apply.style.url = paste(base.url, "apply/styles", style.name, toString(network.suid), sep="/")

res <- GET(apply.layout.url)
res <- GET(apply.style.url)
# Visualize network
plot(g,vertex.size=5,vertex.label=V(g)$name,layout=layout.kamada.kawai)