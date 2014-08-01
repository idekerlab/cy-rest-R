###############################################################################
#
# Sample script to use Cytoscape via RESTful API
# 
#   by Keiichiro Ono
# 
###############################################################################
library(igraph)
library(httr)

# Load utility functions
source('toCytoscape.R')

# Make sure REST API module is running
cytoscape_version = GET("http://localhost:8080/v1/version")
cy_version = fromJSON(rawToChar(cytoscape_version$content))
print(cy_version)

# 1. Create simple network with Barabasi-Albert model
ba200 <- barabasi.game(200)

# 2. Calculate some statistics and assign then to the graph
ba200$name = "Scale-Free Network (BA Model)"
ba200$density = graph.density(ba200)


V(ba200)$degree <- degree(ba200)
V(ba200)$betweenness <- betweenness(ba200)
E(ba200)$betweenness <- edge.betweenness(ba200)

# 3. Send the graph to Cytoscape
cy_graph <- toCytoscape(ba200)


data1 <- '{"data" : {"name" : "dddd11111"},"elements" : {"nodes" : [],"edges" : []}}'
#res <- POST(url="http://localhost:8080/v1/networks", body = data1, encode = "json")

net_data = list(name="foo3", id="foo3")
dt = list(name="aaa", id="aaa")
n1 = list(data=dt)
nd = list(n1)
ed = list()
elm = list(nodes=nd, edges=ed)



dummy = toJSON(list(data=net_data, elements=elm))
print(cy_graph)

res2 <- POST(url="http://localhost:8080/v1/networks", body = cy_graph, encode = "json")
