
#library(data.table)
library(data.table, include.only = c("data.table"))

#' Mixture model structure class
#' @description
#' Initialises object
#' MM.Structure.Class$new
#'
#' @param data = data.table::data.table(),
#' @param sequence_data = data.table::data.table(),
#' @param subject_id = "id",
#' @param phidden = c(NaN,NaN,NaN),
#' @param node_types = c("temp_ologit","temp_mlogit","temp_regress","normal","multinomial","poisson"),
#' @param node_names = c("n1","n2","n3","n4","n5","n6"),
#' @param temporal_variables = c("t1","t2","t3","t4"),
#' @param debug_level = 0
#'
#' @field nhidden numeric.
#' @field phidden numeric.
#' @field nodes list.
#' @field weights matrix.
#' @field likelihood_weights matrix.
#' @field data data.table.
#' @field sequence_data data.table.
#' @field observations_pr_subj numeric.
#' @field nsubjects numeric.
#' @field nobservations numeric.
#' @field temporal_variables character.
#' @field node_types character.
#' @field node_names character.
#' @field sid character.
#' @field log_likelihood numeric.
#' @field nparameters numeric.
#' @field BIC numeric.
#' @field trained logical.
#' @field debug_level numeric.
#'
#'
#' @returns MM.Structure.Class$new returns a reference to a new MM.Structure.Class object
#' @export
#'
#' @examples
#'
#'
MM.Structure.Class <- setRefClass(
  "MM.Structure.Class",
  fields = list(
    nhidden = "numeric",
    phidden = "numeric",
    nodes = "list",
    weights = "matrix",
    likelihood_weights = "matrix",
    data = "data.table",
    sequence_data = "data.table",
    observations_pr_subj = "numeric",
    nsubjects = "numeric",
    nobservations = "numeric",
    temporal_variables = "character",
    node_types = "character",
    node_names = "character",
    sid = "character",
    log_likelihood = "numeric",
    nparameters = "numeric",
    BIC = "numeric",
    trained = "logical",
    debug_level = "numeric"
  ),
  methods = list(

    #############################
    # Initialize: called when object is initialized using MM.Structure.Class$new()
    # Calls setup_node_conf and set_random
    #############################
    initialize = function(data = data.table::data.table(),
                          sequence_data = data.table::data.table(),
                          subject_id = "id",
                          phidden = c(NaN,NaN,NaN),
                          node_types = c("temp_ologit","temp_mlogit","temp_regress","normal","multinomial","poisson"),
                          node_names = c("n1","n2","n3","n4","n5","n6"),
                          temporal_variables = c("t1","t2","t3","t4"),
                          debug_level = 0) {
      callSuper()

      time_series_nodes <- c("temp_ologit","temp_mlogit","temp_regress")
      standard_nodes <- c("normal","multinomial","poisson")

      .self$nhidden <- length(phidden)
      .self$phidden <- phidden
      .self$data <- data.table::data.table(data)
      .self$sequence_data <- data.table::data.table(sequence_data)
      .self$sid <- subject_id
      .self$nsubjects <- nrow(.self$data)
      .self$temporal_variables <- temporal_variables
      .self$node_types <- node_types
      .self$node_names <- node_names
      .self$debug_level <- debug_level


      time_series_nodes_selected <- any(.self$node_types %in% time_series_nodes)
      standard_nodes_selected <- any(.self$node_types %in% standard_nodes)

      .self$nobservations <- nrow(.self$data)*sum(.self$node_types %in% standard_nodes) + nrow(.self$sequence_data)*sum(.self$node_types %in% time_series_nodes)

      # Check node_types and node_names are lists of same length
      if (length(.self$node_types) != length(node_names)) {
        stop("node_types and node_names not of same length")
      }
      for (node_type in .self$node_types) {
        if (!(node_type %in% c(time_series_nodes,standard_nodes))) {
          stop(sprintf("%s is not an allowed node type",node_type))
        }
      }

      if (time_series_nodes_selected) {
        if (nrow(.self$sequence_data) != 0) {
          # Initialize observation counts per sequence
          .self$observations_pr_subj <- .self$sequence_data[, .(count = .N), by = eval(as.name(subject_id))]$count
        } else {
          stop("Time series nodes selected, but no time series data available")
        }
        # Loop over time series nodes
        for (j in length(.self$node_types)) {
          if (.self$node_types[j] %in% time_series_nodes) {
            if (!(.self$node_names[j] %in% names(.self$sequence_data))) {
              stop(sprintf("%s is not in sequence_data",.self$node_names[j]))
            }
          }
        }
        # Loop over time series variables
        for (tv in .self$temporal_variables) {
          if (!(tv %in% names(.self$sequence_data))) {
            stop(sprintf("%s is not in sequence_data",.self$node_names[j]))
          }
        }
      }

      if (standard_nodes_selected) {
        if (nrow(.self$data) == 0)  {
          stop("Standard nodes selected, but no data available")
        }
        # Loop over standard nodes
        for (j in length(.self$node_types)) {
          if (.self$node_types[j] %in% standard_nodes) {
            if (!(.self$node_names[j] %in% names(.self$data))) {
              stop(sprintf("%s is not in data",.self$node_names[j]))
            }
          }
        }
      }

      if (time_series_nodes_selected & standard_nodes_selected) {
        if (!identical(.self$data[,eval(as.name(.self$sid))],unique(.self$sequence_data[,eval(as.name(.self$sid))]))) {
          stop("The subject_id variables in data and sequence_data are not indentical or identically sorted. If some subjects are missing from either data set, add a row with NAs for those subjects.")
        }
      }

      .self$setup_node_conf()
      .self$set_random()
    },

    #############################
    # Setting up node configuration
    # Called by initialize and reset_hidden
    #############################
    setup_node_conf = function() {

      .self$nparameters <- length(.self$node_types)-1

      # Loop over nodes
      for (j in 1:length(.self$node_types)) {
        .self$nodes[[j]] <- list("type"=.self$node_types[j],"name"=.self$node_names[j])

        if (.self$node_types[j] == "temp_ologit" | .self$node_types[j] == "temp_mlogit" | .self$node_types[j] == "temp_regress") {

          # prepare for nhidden fitted models
          .self$nodes[[j]]$models <- rep(list(NULL),.self$nhidden)


          nparameters_tmp <- length(.self$temporal_variables)+1

          if (.self$node_types[j] == "temp_ologit") {

            # rearrange sequence data for ordered logistic regression analysis
            .self$nodes[[j]]$levels <- as.numeric(unique(.self$sequence_data[,eval(as.name(.self$nodes[[j]]$name))]))
            .self$nodes[[j]]$levels <- .self$nodes[[j]]$levels[order(.self$nodes[[j]]$levels)]

            .self$nodes[[j]]$data <- lapply(1:(length(.self$nodes[[j]]$levels)-1),function(j){
              data_tmp <- data.table::copy(.self$sequence_data);
              data_tmp[,cat_outcome := .self$nodes[[j]]$levels[j+1]]
            }) |> rbindlist()
            .self$nodes[[j]]$data[,bin_outcome := data.table::fifelse(.self$nodes[[j]]$data[,eval(as.name(.self$nodes[[j]]$name))]>=.self$nodes[[j]]$data[,"cat_outcome"],1,0)]

            .self$nodes[[j]]$formula <- reformulate(c("0","cat_outcome",.self$temporal_variables),"bin_outcome")

            nparameters_tmp <- nparameters_tmp + length(.self$nodes[[j]]$levels)-2

          } else if (.self$node_types[j] == "temp_mlogit") {

            # rearrange sequence data for multinomial logistic regression analysis
            .self$nodes[[j]]$levels <- as.numeric(unique(.self$sequence_data[,eval(as.name(.self$nodes[[j]]$name))]))
            .self$nodes[[j]]$levels <- .self$nodes[[j]]$levels[order(.self$nodes[[j]]$levels)]

            .self$nodes[[j]]$data <- lapply(1:(length(.self$nodes[[j]]$levels)-1),function(j){
              data_tmp <- data.table::copy(.self$sequence_data[.self$sequence_data[,eval(as.name(.self$nodes[[j]]$name))] == .self$nodes[[j]]$levels[1] | .self$sequence_data[,eval(as.name(.self$nodes[[j]]$name))] == .self$nodes[[j]]$levels[j+1],]);
              data_tmp[,cat_outcome := .self$nodes[[j]]$levels[j+1]]
            }) |> rbindlist()
            .self$nodes[[j]]$data[,bin_outcome := data.table::fifelse(.self$nodes[[j]]$data[,eval(as.name(.self$nodes[[j]]$name))]==.self$nodes[[j]]$data[,"cat_outcome"],1,0)]

            .self$nodes[[j]]$formula <- reformulate(c("0","cat_outcome",paste(.self$temporal_variables,"cat_outcome",sep=":")),"bin_outcome")

            nparameters_tmp <- nparameters_tmp*(length(.self$nodes[[j]]$levels)-1)

          }
          .self$nparameters <- .self$nparameters + nparameters_tmp

        } else if (.self$node_types[j] == "normal") {

          .self$nodes[[j]]$mu <- rep(NaN,.self$nhidden)
          .self$nodes[[j]]$sigma <- rep(NaN,.self$nhidden)
          .self$nparameters <- .self$nparameters + 2*.self$nhidden

        } else if (.self$node_types[j] == "multinomial") {

          .self$nodes[[j]]$levels <- as.numeric(unique(.self$data[,eval(as.name(.self$nodes[[j]]$name))]))
          .self$nodes[[j]]$levels <- .self$nodes[[j]]$levels[order(.self$nodes[[j]]$levels)]

          n_levels <- length(.self$nodes[[j]]$levels)
          .self$nodes[[j]]$probs <- matrix(rep(NaN,.self$nhidden*n_levels),ncol = n_levels)
          .self$nparameters <- .self$nparameters + (n_levels-1)*.self$nhidden

        } else if (.self$node_types[j] == "poisson") {

          .self$nodes[[j]]$lambda <- rep(NaN,.self$nhidden)
          .self$nparameters <- .self$nparameters + .self$nhidden

        } else {
          stop("Node type unknown")
        }
      }

      .self$trained = FALSE

      return(NULL)
    },

    #############################
    # Set weights to random values
    # Called by initialize and reset_hidden
    #############################
    set_random = function() {

      # Set hidden node to random
      # if (is.na(sum(.self$phidden))) {
      #   warning("Probabilities of hidden states are missing, resetting")
      #   .self$phidden <- as.numeric(rdirichlet(1,runif(.self$nhidden)))
      # } else if (sum(.self$phidden) != 1) {
      #   warning("Probabilities of hidden states does not sum to one, resetting")
      #   .self$phidden <- as.numeric(rdirichlet(1,runif(.self$nhidden)))
      # }

      # Set weights to random
      .self$likelihood_weights <- matrix(runif(.self$nhidden*.self$nsubjects),ncol = .self$nhidden)

      # Normalise weights across hidden states
      .self$likelihood_weights <- .self$likelihood_weights/apply(.self$likelihood_weights,1,sum)

      .self$trained = FALSE

      return(NULL)
    },

    #############################
    # Resets number of hidden states
    # Calls setup_node_conf and set_random
    #############################
    reset_hidden = function(phidden = c(NaN,NaN,NaN)) {
      .self$nhidden <- length(phidden)
      .self$phidden <- phidden
      .self$setup_node_conf()
      .self$set_random()
      return(NULL)
    },

    #############################
    # M-step:
    # Estimate new model parameters and hidden state probabilities
    #############################
    Mstep = function() {

      # TODO: multithread by hidden state inside regression nodes

      .self$weights <- .self$likelihood_weights

      for (j in 1:length(.self$nodes)) {

        if (.self$nodes[[j]]$type == "temp_ologit") {

          # Estimate models from the random weights
          for (k in 1:.self$nhidden) {
            .self$nodes[[j]]$models[[k]] <- speedglm::speedglm(.self$nodes[[j]]$formula,
                                                               family = binomial(link="logit"),
                                                               data = .self$nodes[[j]]$data,
                                                               weights = vctrs::vec_rep_each(.self$weights[,k],.self$observations_pr_subj))
          }

        } else if (.self$nodes[[j]]$type == "temp_mlogit") {

          # Estimate models from the random weights
          for (k in 1:.self$nhidden) {
            .self$nodes[[j]]$models[[k]] <- speedglm::speedglm(.self$nodes[[j]]$formula,
                                                               family = binomial(link="logit"),
                                                               data = .self$nodes[[j]]$data,
                                                               weights = vctrs::vec_rep_each(.self$weights[,k],.self$observations_pr_subj))
          }

        } else if (.self$nodes[[j]]$type == "temp_regress") {

          # Estimate models from the random weights
          for (k in 1:.self$nhidden) {
            .self$nodes[[j]]$models[[k]] <- speedglm::speedglm(reformulate(.self$temporal_variables,.self$nodes[[j]]$name),
                                                               family = gaussian(link="identity"),
                                                               data = .self$sequence_data,
                                                               weights = vctrs::vec_rep_each(.self$weights[,k],.self$observations_pr_subj),
                                                               fitted = TRUE) # Faster predict
          }

        } else if (.self$nodes[[j]]$type == "normal") {

          Nk_temp <- apply(.self$weights*(!is.na(.self$data[,eval(as.name(.self$nodes[[j]]$name))])),2,sum)
          #.self$nodes[[j]]$mu <- rnorm(.self$nhidden, mean = 0, sd = 1)
          #.self$nodes[[j]]$sigma <- rgamma(.self$nhidden, shape = 1, rate = 1)
          .self$nodes[[j]]$mu <- apply(.self$weights*.self$data[,eval(as.name(.self$nodes[[j]]$name))],2,sum,na.rm=TRUE)/Nk_temp
          .self$nodes[[j]]$sigma <- sqrt(mapply(function(k){sum(.self$weights[,k]*(.self$data[,eval(as.name(.self$nodes[[j]]$name))]-.self$nodes[[j]]$mu[k])^2,na.rm=TRUE)/Nk_temp[k]},1:.self$nhidden))

        } else if (.self$nodes[[j]]$type == "multinomial") {

          Nk_temp <- apply(.self$weights*(!is.na(.self$data[,eval(as.name(.self$nodes[[j]]$name))])),2,sum)
          #.self$nodes[[j]]$probs <- matrix(as.numeric(rdirichlet(.self$nhidden,runif(n_levels))),ncol = n_levels)
          .self$nodes[[j]]$probs <- mapply(function(i){apply(.self$weights*(.self$data[,eval(as.name(.self$nodes[[j]]$name))]==i),2,sum,na.rm=TRUE)/Nk_temp},.self$nodes[[j]]$levels)

        } else if (.self$nodes[[j]]$type == "poisson") {

          Nk_temp <- apply(.self$weights*(!is.na(.self$data[,eval(as.name(.self$nodes[[j]]$name))])),2,sum)
          #.self$nodes[[j]]$lambda <- rexp(.self$nhidden, rate = 1)
          .self$nodes[[j]]$lambda <- apply(.self$weights*.self$data[,eval(as.name(.self$nodes[[j]]$name))],2,sum,na.rm=TRUE)/Nk_temp

        }
      }

      # Estimate new state probabilities
      .self$phidden <- apply(.self$weights,2,sum)/.self$nsubjects

      return(NULL)
    },

    #############################
    # E-step:
    # Evaluate responsibilities (weights) using current model parameters
    #############################
    Estep = function() {

      # TODO: multithread by hidden state inside regression nodes

      .self$likelihood_weights[,] <- 0

      # Get subject-wise likelihood
      for (j in 1:length(.self$nodes)) {

        if (.self$nodes[[j]]$type == "temp_ologit") {

          highest_level <- .self$nodes[[j]]$levels[length(.self$nodes[[j]]$levels)]
          lowest_level <- .self$nodes[[j]]$levels[1]
          for (k in 1:.self$nhidden) {

            ll_prediction_1 <- predict(.self$nodes[[j]]$models[[k]],
                                       newdata=.self$sequence_data[,cat_outcome := eval(as.name(.self$nodes[[j]]$name))],
                                       type='response')
            ll_prediction_2 <- predict(.self$nodes[[j]]$models[[k]],
                                       newdata=.self$sequence_data[,cat_outcome := pmin(eval(as.name(.self$nodes[[j]]$name))+1,highest_level)],
                                       type='response')

            # Terms are switched because logistic regression uses 1/(exp(-xb)+1) and ologit uses 1/(exp(-k+xb)+1)
            ll_prediction <- cbind(data.table::data.table(log(
              data.table::fifelse(.self$sequence_data[,eval(as.name(.self$nodes[[j]]$name))] == lowest_level,1,ll_prediction_1) -
              data.table::fifelse(.self$sequence_data[,eval(as.name(.self$nodes[[j]]$name))] == highest_level,0,ll_prediction_2)))
              ,.self$sequence_data[,eval(as.name(.self$sid))])

            # Add to LogLik matrix
            if (.self$debug_level >= 5) {
              print(sprintf("Printing observation-wise and subjectwise log-likelihood contribution of class %s:",class(ll_prediction)[1]))
              print(head(ll_prediction))
              print(head(ll_prediction[, lapply(.SD,sum,na.rm = TRUE), by = eval(.self$sid)]))
            }
            .self$likelihood_weights[,k] <- .self$likelihood_weights[,k] + as.matrix(ll_prediction[, lapply(.SD,sum,na.rm = TRUE), by = eval(.self$sid)][,2])
          }

        } else if (.self$nodes[[j]]$type == "temp_mlogit") {

          n_levels <- length(.self$nodes[[j]]$levels)
          for (k in 1:.self$nhidden) {

            ll_prediction <- lapply(.self$nodes[[j]]$levels[2:n_levels],function(level){
              exp(predict(.self$nodes[[j]]$models[[k]],newdata=.self$sequence_data[,cat_outcome := level],type='link'))
            }) |> cbindlist()

            ll_prediction <- cbind(data.table::data.table(log(
              apply(cbind(1,ll_prediction,.self$sequence_data[,eval(as.name(.self$nodes[[j]]$name))]),
                    1,
                    function(x) x[which(x[n_levels+1]==.self$nodes[[j]]$levels)])/rowSums(cbind(1,ll_prediction)))),
              .self$sequence_data[,eval(as.name(.self$sid))])

            # Add to LogLik matrix
            if (.self$debug_level >= 5) {
              print(sprintf("Printing observation-wise and subjectwise log-likelihood contribution of class %s:",class(ll_prediction)[1]))
              print(head(ll_prediction))
              print(head(ll_prediction[, lapply(.SD,sum,na.rm = TRUE), by = eval(.self$sid)]))
            }
            .self$likelihood_weights[,k] <- .self$likelihood_weights[,k] + as.matrix(ll_prediction[, lapply(.SD,sum,na.rm = TRUE), by = eval(.self$sid)][,2])
          }

        } else if (.self$nodes[[j]]$type == "temp_regress") {

          # Estimate models from the random weights
          for (k in 1:.self$nhidden) {

            residuals <- .self$sequence_data[,eval(as.name(.self$nodes[[j]]$name))] - predict(.self$nodes[[j]]$models[[k]])
            ll_prediction <- cbind(data.table::data.table(
              dnorm(residuals,
                    mean = 0,
                    sd = sqrt(sum(residuals^2,na.rm = TRUE)/.self$nodes[[j]]$models[[k]]$df),
                    log = TRUE)),
              .self$sequence_data[,.self$sid,with=FALSE])

            # Add to LogLik matrix
            if (.self$debug_level >= 5) {
              print(sprintf("Printing observation-wise and subjectwise log-likelihood contribution of class %s:",class(ll_prediction)[1]))
              print(head(ll_prediction))
              print(head(ll_prediction[, lapply(.SD,sum,na.rm = TRUE), by = eval(.self$sid)]))
            }
            .self$likelihood_weights[,k] <- .self$likelihood_weights[,k] + as.matrix(ll_prediction[, lapply(.SD,sum,na.rm = TRUE), by = eval(.self$sid)][,2])
          }

        } else if (.self$nodes[[j]]$type == "normal") {

          for (k in 1:.self$nhidden) {

            .self$likelihood_weights[,k] <- .self$likelihood_weights[,k] +
              data.table::fifelse(is.na(.self$data[,eval(as.name(.self$nodes[[j]]$name))]),
                      0,
                      dnorm(.self$data[,eval(as.name(.self$nodes[[j]]$name))],.self$nodes[[j]]$mu[k],.self$nodes[[j]]$sigma[k],log=TRUE))
          }

        } else if (.self$node_types[j] == "multinomial") {

          for (k in 1:.self$nhidden) {

            .self$likelihood_weights[,k] <- .self$likelihood_weights[,k] +
              data.table::fifelse(is.na(.self$data[,eval(as.name(.self$nodes[[j]]$name))]),
                      0,
                      log(.self$nodes[[j]]$probs[which(.self$data[,eval(as.name(.self$nodes[[j]]$name))]==.self$nodes[[j]]$levels)]))
          }

        } else if (.self$nodes[[j]]$type == "poisson") {

          for (k in 1:.self$nhidden) {

            .self$likelihood_weights[,k] <- .self$likelihood_weights[,k] +
              data.table::fifelse(is.na(.self$data[,eval(as.name(.self$nodes[[j]]$name))]),
                      0,
                      dpois(.self$data[,eval(as.name(.self$nodes[[j]]$name))],.self$nodes[[j]]$lambda[k],log=TRUE))
          }
        }
      }


      # Add hidden state probabilities:
      .self$likelihood_weights <- .self$likelihood_weights + log(.self$phidden)
      if (.self$debug_level >= 3) {
        print(sprintf("Printing likelihood weights after adding hidden state probabilities:"))
        print(head(.self$likelihood_weights))
      }

      subject_extreme_factor <- apply(.self$likelihood_weights,1,mean)
      if (.self$debug_level >= 3) {
        print(sprintf("Printing factors for avoiding extreme likelihood weights:"))
        print(subject_extreme_factor)
      }
      .self$likelihood_weights <- .self$likelihood_weights-subject_extreme_factor
      subject_normalisation_factor <- log(apply(exp(.self$likelihood_weights),1,sum))
      if (.self$debug_level >= 3) {
        print(sprintf("Printing log-normalisation factors for likelihood weights:"))
        print(subject_normalisation_factor)
      }

      # Likelihood and BIC (log and sum) (check Bishop)
      .self$log_likelihood <- sum(subject_normalisation_factor+subject_extreme_factor)
      .self$BIC <- .self$log_likelihood + 0.5*log(.self$nobservations)*.self$nparameters

      # Normalise across hidden states
      .self$likelihood_weights <- exp(.self$likelihood_weights-subject_normalisation_factor)

      return(NULL)
    },

    #############################
    # Help functions
    #############################
    check_node_names <- function(nodes) {
      if (length(nodes)==0){
        stop("node_values must contain names")
      } else {
        if (!all(nodes %in% .self$node_names)) {
          stop("Not all node names in node_values exists")
        }
        if (length(nodes)!=length(unique(nodes))){
          stop("Node names in node_values may only appear ones")
        }
      }
      return(NULL)
    },
    is.trained <- function() {
      return(.self$trained)
    },
    get_log_likelihood <- function() {
      return(.self$log_likelihood)
    },
    get_BIC <- function() {
      return(.self$BIC)
    },
    get_model_parameters <- function(hidden_state,node) {

    },
    set_model_parameters <- function(hidden_state,node,params) {
      # TODO: check params are consistent with node
    }
  )
)



#' Title
#'
#' @param x
#' @param number_of_tries
#' @param hidden_states
#' @param reinitialise
#' @param tol
#'
#' @returns
#' @export
#'
#' @examples
MM.optimise <- function(x,number_of_tries=5,hidden_states=c(5,10,15,20),reinitialise=FALSE,tol=0.01) {

}

#' Title
#'
#' @param x MM.Structure.Class object
#' @param tol tolerance
#' @param reinitialise whether to reinitialise x with random weights
#'
#' @returns function returns NULL because x is a reference class
#' @export
#'
#' @examples
MM.train <- function(x,reinitialise=FALSE,tol=0.01) {

  if (class(x) != "MM.Structure.Class") {
    stop("x is not an MM.Structure.Class object")
  }

  if (reinitialise) {
    x$setup_node_conf()
    x$set_random()
  }

  x$Mstep()
  x$Estep()
  old_ll <- x$log_likelihood
  repeat {
    x$Mstep()
    x$Estep()
    if (x$log_likelihood-old_ll < tol) {break}
    old_ll <- x$log_likelihood
  }

  x$trained = TRUE
  return(NULL)
}
#' Title
#'
#' @param x MM.Structure.Class object
#' @param node_values conditional values of nodes  either as vector e.g c("M1"=1,"M2"=2.33) or data.frame. If multiple rows in data.frame then the function returns n samples per row
#' @param n number of samples
#'
#' @returns
#' @export
#'
#' @examples
MM.cond.h.sample <- function(x,node_values,n) {

  if (class(x) != "MM.Structure.Class") {
    stop("x is not an MM.Structure.Class object")
  }
  if (!x$is.trained) {
    stop("x has not been trained or has been reinitialised. Use MM.train to train x")
  }


  x.check_node_names(names(node_values))



  #         H
  #       / | \
  #      |  |  |
  #      V  V  V
  #     L1  M1 M2
  #
  # P(H|M1,M2)=P(M1|H)P(M2|H)P(H)/(sum_H P(M1|H)P(M2|H)P(H))

  # 1) Calculate conditional probability for each hidden state using P(M1|H)P(M2|H)P(H)
  # 2) Normalise conditional hidden state probabilities
  # 3) Sample H from conditional hidden state probabilities

  for (j in 1:length(x$nodes)) {
    for (k in 1:length(x$phidden)) {
      if (x$nodes[[j]]$type == "") {

      }
    }
  }

  # TODO: Condition on time series? E.g. use normal distribution and predicted curve as mean

}


#' Title
#'
#' @param x
#' @param phidden
#'
#' @returns
#' @export
#'
#' @examples
reset_hidden = function(x,phidden = c(NaN,NaN,NaN)) {
  if (class(x) != "MM.Structure.Class") {
    stop("x is not an MM.Structure.Class object")
  }
  x.reset_hidden(phidden)
  return(NULL)
}


#' Title
#'
#' @param x MM.Structure.Class object
#' @param hidden_state which hidden state to condition on. If multiple hidden states, then the function returns n samples per hidden state
#' @param nodes vector of characters indicating which nodes to sample
#' @param n number of samples
#'
#' @returns
#' @export
#'
#' @examples
MM.cond.sample <- function(x,hidden_state,nodes,n) {

  if (class(x) != "MM.Structure.Class") {
    stop("x is not an MM.Structure.Class object")
  }
  if (!x$is.trained) {
    stop("x has not been trained or has been reinitialised. Use MM.train to train x")
  }

  if (length(hidden_state)!=1) {
    stop(paste("hidden_state must be one of the following numbers",paste(sprintf("%i ",1:length(x$phidden)),collapse='')))
  }
  if (!(hidden_state %in% 1:length(x$phidden))) {
    stop(paste("hidden_state must be one of the following numbers",paste(sprintf("%i ",1:length(x$phidden)),collapse='')))
  }

  x.check_node_names(nodes)

  #         H
  #       / | \
  #      |  |  |
  #      V  V  V
  #     L1  M1 M2

  # 1) Choose a hidden state H
  # 2) Sample from P(L1|H), P(M1|H) and P(M2|H) (all or subset)



}

MM.impute.missing <- function(x,data,data_time_series,n) {

}

###################
## EQUIVALENCE 1 ##

#    M1
#    |         H
#    V       /   \
#    H      |     |
#    |      V     V
#    V     L1     M1
#    L1

###################
## EQUIVALENCE 2 ##

#  M1   M2
#   \   /        H
#    | |       /   \
#    V V      |     |
#     H       V     V
#     |      L1   M1,M2
#     V
#     L1

###################


Mstep <- function(x) {

}
Estep <- function(x) {

}
#' Title
#'
#' @param x
#'
#' @returns
#' @export
#' @note
#' \deqn{f(x)=}
#' @references TBA
#'
#' @examples
get_log_likelihood <- function(x) {

}
#' Title
#'
#' @param x
#'
#' @returns
#' @export
#' @note
#' \deqn{f(x)=}
#' @references TBA
#'
#' @examples
get_BIC <- function(x) {

}
#' Title
#'
#' @param x
#' @param hidden_state
#' @param node
#'
#' @returns
#' @export
#' @note
#' \deqn{f(x)=}
#' @references TBA
#'
#' @examples
get_model_parameters <- function(x,hidden_state,node) {

}
#' Set model parameters
#' @author Jan Brink Valentin
#' @description
#' Can be used to set initial conditions, but remember to run Estep before training, otherwise initial conditions are overridden
#'
#' @param x
#' @param hidden_state
#' @param node
#' @param params
#'
#' @returns
#' @export
#' @note
#' \deqn{f(x)=}
#' @references TBA
#'
#' @examples
set_model_parameters <- function(x,hidden_state,node,params) {
  # TODO: check params are consistent with node
}


# Note: marginals does not necessarily make sense???





if (FALSE) {
  test <- function(object) {
    object$phidden <- as.numeric(rdirichlet(1,runif(object$nhidden)))
  }

  # Make test data
  set.seed(12345)
  mm.test.data <- data.table::data.table(
    id = 1:5000,
    var1 = rnorm(5000,sample(c(-2,14,3,4),5000,replace=TRUE),3), # normal
    cat1 = rbinom(5000,5,sample(c(0.1,0.5,0.4,0.7),5000,replace=TRUE)), # multinomial6
    cat2 = rpois(5000,sample(c(5,17,4,8),5000,replace=TRUE)), # poisson
    cat3 = rpois(5000,sample(c(5,12,4,8),5000,replace=TRUE))  # poisson
    )
  mm.test.sequences <- data.table::copy(mm.test.data[,c("id")])
  mm.test.sequences[,param1:=runif(5000,sample(c(-5,-4,-1,-8),5000,replace=TRUE),sample(c(5,2,4,3),5000,replace=TRUE))]
  mm.test.sequences[,param2:=runif(5000,sample(c(-5,-4,-1,-8),5000,replace=TRUE),sample(c(5,2,4,3),5000,replace=TRUE))]
  mm.test.sequences[,param3:=runif(5000,sample(c(-5,-4,-1,-8),5000,replace=TRUE),sample(c(5,2,4,3),5000,replace=TRUE))]
  mm.test.sequences[,param4:=runif(5000,sample(c(-5,-4,-1,-8),5000,replace=TRUE),sample(c(5,2,4,3),5000,replace=TRUE))]
  mm.test.sequences <- mm.test.sequences[rep(id,50)]
  mm.test.sequences[,time:=1:.N,by=id]
  mm.test.sequences <- mm.test.sequences[order(id,time)]
  mm.test.sequences[,tvar1:=param1*time+rnorm(5000,0,40)] # continous outcome
  mm.test.sequences[,tvar2:=param2*time+rnorm(5000,0,40)] # continous outcome
  mm.test.sequences[,tvar3:=rbinom(50*5000,3,1/(1+exp(param3*time*0.01+rnorm(5000,0,5))))] # nominal outcome k=4
  mm.test.sequences[,tvar4:=rbinom(50*5000,3,1/(1+exp(param4*time*0.01+rnorm(5000,0,5))))] # ordered outcome k=4
  mm.test.sequences <- mm.test.sequences[,!c("param1","param2","param3","param4")]

  # TODO: add option for mean or sum of time series likelihood contributions (test how and if varying lengths of time series affects training)

  # TODO: add missingness

  # TODO; conduct weighted ordered, multinomial and linear regression for comparison.

  mm.object <- MM.Structure.Class$new(data = mm.test.data,
                                      sequence_data = mm.test.sequences,
                                      subject_id = "id",
                                      phidden = c(NaN,NaN,NaN),
                                      node_types = c("normal"), #,"temp_ologit","temp_mlogit","temp_regress","normal","multinomial","poisson"),
                                      node_names = c("var1"),
                                      temporal_variables = c("time"),
                                      debug_level = 0)
  hist(mm.test.data$var1)
  mm.object$Mstep()
  mm.object$nodes[[1]]$mu
  mm.object$nodes[[1]]$sigma
  mm.object$Estep()

  mm.object <- MM.Structure.Class$new(data = mm.test.data,
                                      sequence_data = mm.test.sequences,
                                      subject_id = "id",
                                      phidden = c(NaN,NaN,NaN),
                                      node_types = c("temp_regress","temp_regress","normal"), #,"temp_ologit","temp_mlogit","temp_regress","normal","multinomial","poisson"),
                                      node_names = c("tvar1","tvar2","var1"),
                                      temporal_variables = c("time"),
                                      debug_level = 5)

  mm.object$Mstep()
  mm.object$nodes[[1]]$models
  mm.object$phidden
  mm.object$Estep()

  mm.object$Mstep()
  mm.object$likelihood_weights
  mm.object$phidden
  mm.object$nodes
  mm.object$log_likelihood
  mm.object$BIC
  mm.object$Estep()
  check <- mm.object$likelihood_weights



  mm.object$nodes
  mm.object$set_random()
  mm.object$phidden
  mm.object$nodes

  mm.object$phidden
  test(mm.object)
  mm.object$phidden
}






















