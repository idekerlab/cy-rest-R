getColorPallet <- function(n) {
  colors <- rainbow(n)
  color.pallet <- array(
    sapply(colors, 
           function(x){
             return(substring(x, 1, 7))
            }
           )
    )
  return(color.pallet)
}