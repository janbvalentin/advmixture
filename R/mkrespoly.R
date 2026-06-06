



#' Make restricted polynomials
#'
#' @param x numeric vector
#' @param t1 numeric scalar indicating first knot point
#' @param t2 numeric scalar indicating second knot point
#' @param order integer > 3 defining the polynomial order. Degrees of freedom = order - 2. Default order is 4
#'
#' @returns matrix with number of rows equal to the length of x and number of columns equal to degrees of freedom.
#' @export
#'
#' @examples mkrespoly(c(0,1,2,3,4,5,6,7,8,9,10),0.5,9.5,6)
mkrespoly <- function(x,t1,t2,order=4) {
  if (order < 4) { stop("order must be 4 or larger") }
  if (t1 >= t2) { stop("t1 must be strictly smaller than t2") }
  respoly <- matrix(nrow=length(x),ncol=order-2)
  respoly[,1] <- x
  for(j in seq(4,order)){
    c <- j-2
    respoly[,c] <-
      ifelse(x>t1,((x-t1)/((t2-t1)^((j-2)/j)))^j - ((x-t1)/((t2-t1)^((j-2)/(j-1))))^(j-1)*(t2-t1)*j/(j-2),0)
    respoly[,c] <-
      respoly[,c] -
      ifelse(x>=t2,((x-t2)/((t2-t1)^((j-2)/j)))^j,0)
    for(k in seq(1,j-3)){
      respoly[,c] <-
        respoly[,c] -
        ifelse(x>=t2,((x-t2)/((t2-t1)^((j-2)/(j-k))))^(j-k)*(t2-t1)^k*choose(j,k)*(j-k-2)/(j-2),0)
    }
  }
  return(respoly)
}
