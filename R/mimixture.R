





#' Get default node types
#' @author Jan Brink Valentin
#' @description
#' Generates a vector of proposed node types based on the data types and data distribution.
#' For
#' * ordinal factor data, node type binomial is used
#' * nominal factor data, node type categorical is used
#' * numeric data larger than or equal to zero and containing only integers, node type poisson is used
#' * numeric data larger than or equal to zero and containing decimal numbers, node type gamma is used
#' * numeric data other than the above, node type normal is used.
#'
#' @param data data to be imputed
#'
#' @returns char vector of node types
#' @export
#' @note
#' The function is used directly in mimixture, but can also be used when changing a subset of nodes to a different type.
#' @references TBA
#'
#' @examples
#' dtable <-
#' default_node_types <- get_default_node_type(dtable)
#' default_node_types
#'
#' default_node_types[4] <- "normal"
#' default_node_types
#'
get_default_node_types <- function(data) {

}


#' Multiple imputation mixture model
#' @author Jan Brink Valentin
#' @description
#' Conduct multiple imputation with a multivariate mixture model and variable specific probability distributions.
#'
#' @param data data to be imputed
#' @param M Number of imputation sets
#' @param optimise Whether to search for the optimal number of hidden states
#' @param node_types overrides default node types (see get_default_node_types)
#' @param ... parameters inherited by MM.optimise or MM.train if optimize=FALSE
#'
#' @returns data.table with all M imputation sets stacked. Column mi.sid indicates the observation number and mi.mid indicates the imputation set number.
#' @export
#' @note
#' -
#' @references TBA
#'
#' @examples
mimixture <- function(data,M=20,optimise=TRUE,node_types=NULL,...) {

  # ordinal: binomial distribution
  # categorical: multinomial distribution
  # numeric: normal

  # Generate node type

  return(newdata)
}

#' Title
#' @author Jan Brink Valentin
#' @description
#' Conduct multiple imputation with a multivariate mixture model and variable specific probability distributions.
#'
#' @param x mimixture data object. Must contain the variables mi.sid and mi.mid
#' @param ...
#'
#' @returns
#' @export
#' @note
#' -
#' @references TBA
#'
#' @examples
glm_mimixture <- function(x,...) {

  # TODO: multiply vcov by 1/M
  # TODO: how to use doubly robust causal inferience (weights and standardisation)

}

#' Variance-covariance of a glm model on multiple imputed data.
#' @author Jan Brink Valentin
#' @description
#' Using clustered and unclustered data, and doubly robust causal inference
#'
#' @param x
#' @param ...
#'
#' @returns
#' @export
#' @note
#' -
#' @references TBA
#'
#' @examples
#' # matched data
#'
#'
#' # doubly robust estimation
vcov_mimixture <- function(x,...) {

}

#' Bootstrap function for estimating variance-covariance matrix
#' @author Jan Brink Valentin
#' @description
#' Missing data must be re-sampled for each bootstrap-sample to ensure Rubin's rule
#'
#' @param x
#' @param nsamples
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
bootstrap_mimixture <- function(x,nsamples,...) {

}




