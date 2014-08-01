toCytoscape <- function (igraphobj) {
  
  # Extract graph attributes
  graph_attr = graph.attributes(igraphobj)
  
  # Extract nodes
  node_count = length(V(igraphobj))
  V(igraphobj)$id <-c(1:node_count)
  
  nodes <- V(igraphobj)
  nds = list()
  
  v_attr = vertex.attributes(igraphobj)
  v_names = list.vertex.attributes(igraphobj)
  
  for(i in 1:node_count) {
    node_attr = list()
    
    for(j in 1:length(v_attr)) {
      if(v_names[j] == "id") {
        node_attr[j] = toString(v_attr[[j]][i])
      } else {
        node_attr[j] = v_attr[[j]][i]  
      }
    }
    names(node_attr) = v_names
    nds[[i]] = list(data = node_attr)
    
  }
  
  edges <- get.edgelist(igraphobj)
  edge_count = ecount(igraphobj)
  
  eds = list()
  for(i in 1:edge_count) {
    edge_attr = list(source=toString(edges[i,1]), target=toString(edges[i,2]))
    eds[[i]] = list(data=edge_attr)
  }
  
  
  el = list(nodes=nds, edges=eds)
  
  x <- list(data = graph_attr, elements = el)
  return (toJSON(x))
}