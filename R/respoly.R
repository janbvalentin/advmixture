



#' Restricted polynomials
#'
#' @param x
#' @param t1
#' @param t2
#' @param order
#'
#' @returns
#' @export
#'
#' @examples
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
