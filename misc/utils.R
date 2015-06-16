# Utilities
# Generate color palet using number of communitie
communityToColors <- function(members, num.communities) {
  base.color <- "#AAAAAA"
  num.members <- length(members)
  colors <- array(base.color, dim=c(num.members))

  # Split color space into number of communities
  color.pallet <- rainbow(num.communities)

  for(i in 1:num.members) {
    newcolor <- color.pallet[members[i]]
    if(length(newcolor) == 0) {
      newcolor <- base.color
    }
    colors[i] <- newcolor
  }
  result <- array(sapply(colors,function(x){return(substring(x, 1, 7))}))
  return(result)
}

buildStyle <- function(style.name, g, colors, community) {
  # Preepare Defaults
  def.node.border.width <- list(
    visualProperty = "NODE_BORDER_WIDTH",
    value = 0
  )

  def.node.transparency <- list(
    visualProperty="NODE_TRANSPARENCY",
    value=230
  )

  def.edge.transparency <- list(
    visualProperty="EDGE_TRANSPARENCY",
    value=120
  )

  def.edge.width <- list(
    visualProperty="EDGE_WIDTH",
    value=2
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
    mappingColumn=colors,
    mappingColumnType="String",
    visualProperty="NODE_FILL_COLOR"
  )

  edge.color = list(
    mappingType="passthrough",
    mappingColumn=colors,
    mappingColumnType="String",
    visualProperty="EDGE_STROKE_UNSELECTED_PAINT"
  )

  # Node Size Mapping
  min.betweenness = min(V(g)$betweenness)
  max.betweenness = max(V(g)$betweenness)

  point1 = list(
    value=min.betweenness,
    lesser= "5.0",
    equal="5.0",
    greater="5.0"
  )

  point2 = list(
    value=max.betweenness,
    lesser="100.0",
    equal="100.0",
    greater="100.0"
  )

  node.size.continuous.points = list(point1, point2)

  node.size = list(
    mappingType="continuous",
    mappingColumn="betweenness",
    mappingColumnType="Double",
    visualProperty="NODE_SIZE",
    points = node.size.continuous.points
  )

  edge.trans.point = list(
    value=1.0,
    lesser= "40",
    equal="200",
    greater="200"
  )

  edge.trans.continuous.points = list(edge.trans.point)

  edge.trans = list(
    mappingType="continuous",
    mappingColumn=community,
    mappingColumnType="Double",
    visualProperty="EDGE_TRANSPARENCY",
    points = edge.trans.continuous.points
  )

  mappings = list(node.fill.color, edge.color, node.size, edge.trans)

  style <- list(title=style.name, defaults = defaults, mappings = mappings)
  return(toJSON(style))
}