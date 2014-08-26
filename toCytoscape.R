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
#     node_attr = list()
#     
#     for(j in 1:length(v_attr)) {
#       node_attr[j] = v_attr[[j]][i]  
#     }
#     names(node_attr) = v_names
    nds[[i]] = list(data = mapAttributes(v_names, v_attr, i))
  }
  
  edges <- get.edgelist(igraphobj)
  edge_count = ecount(igraphobj)
  e_attr <- edge.attributes(igraphobj)
  e_names = list.edge.attributes(igraphobj)
  
  attr_exists = FALSE
  e_names_len = 0
  if(identical(e_names, character(0)) == FALSE) {
    attr_exists = TRUE
    e_names_len = length(e_names)
  }
  e_names_len <- length(e_names)
  
  eds = list()
  for(i in 1:edge_count) {
    st = list(source=toString(edges[i,1]), target=toString(edges[i,2]))
    
    # Extract attributes
    if(attr_exists) {
      eds[[i]] = list(data=c(st, mapAttributes(e_names, e_attr, i)))
    } else {
      eds[[i]] = list(data=st)
    }
  }

  el = list(nodes=nds, edges=eds)
  
  x <- list(data = graph_attr, elements = el)
  return (toJSON(x))
}

mapAttributes <- function(attr.names, all.attr, i) {
  attr = list()
  cur.attr.names = attr.names
  attr.names.length = length(attr.names)
  
  for(j in 1:attr.names.length) {
    if(is.na(all.attr[[j]][i]) == FALSE) {
#       attr[j] = all.attr[[j]][i]
      attr <- c(attr, all.attr[[j]][i])
    } else {
      cur.attr.names <- cur.attr.names[cur.attr.names != attr.names[j]]
    }
  }
  names(attr) = cur.attr.names
  return (attr)
}


send2cy <- function(cygraph, style.name, layout.name) {
  network.url = paste(base.url, "networks", sep="/")
  res <- POST(url=network.url, body=cygraph, encode="json")
  network.suid = unname(fromJSON(rawToChar(res$content)))
  print(network.suid)
  
  # Apply Visual Style
  apply.layout.url = paste(base.url, "apply/layouts", layout.name, toString(network.suid), sep="/")
  apply.style.url = paste(base.url, "apply/styles", style.name, toString(network.suid), sep="/")
  
  res <- GET(apply.layout.url)
  res <- GET(apply.style.url)
}
