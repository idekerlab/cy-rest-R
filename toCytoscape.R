toCytoscape <- function (igraphobj) {
  
  # Extract graph attributes
  graph_attr = graph.attributes(igraphobj)
  
  # Extract nodes
  node_count = length(V(igraphobj))
  if('name' %in% list.vertex.attributes(igraphobj)) {
    V(igraphobj)$id <- V(igraphobj)$name
  } else {
    V(igraphobj)$id <-as.character(c(1:node_count))
  }
  
  
  nodes <- V(igraphobj)
  nds = list()
  
  v_attr = vertex.attributes(igraphobj)
  v_names = list.vertex.attributes(igraphobj)
  
  for(i in 1:node_count) {
    node_attr = list()
    
    for(j in 1:length(v_attr)) {
      node_attr[j] = v_attr[[j]][i]  
    }
    names(node_attr) = v_names
    nds[[i]] = list(data = node_attr)
    
  }
  
  edges <- get.edgelist(igraphobj)
  edge_count = ecount(igraphobj)
  e_attr <- edge.attributes(igraphobj)
  e_names = list.edge.attributes(igraphobj)
  
  e_names_len <- length(e_names)
  
  eds = list()
  for(i in 1:edge_count) {
    st = list(source=toString(edges[i,1]), target=toString(edges[i,2]))
              #path=e_attr[[1]][i])
    eds[[i]] = list(data=st)
  }
  
  
  el = list(nodes=nds, edges=eds)
  
  x <- list(data = graph_attr, elements = el)
  return (toJSON(x))
}