#' Provide useful summary statistic of selected variables
#'
#' Returns simple descriptive statistics and number of missing entries in each year in the data set.
#' Character and factor variables are automatically binarized to obtain descriptive statistics.
#'
#' @param data data.frame to perform summary statistics.
#' @param id_time (Default = \code{NULL}) A column name indicating time (e.g., year) in \code{data}. If unspecified, missing values will counted for the entire data set.
#' @importFrom tidyr pivot_wider
#' @importFrom stats quantile na.omit
#' @return \code{check_missing} returns a list containing the following elements:
#'  \itemize{
#'    \item \code{desc}: Data frame containing descriptive statistics including mean, standard deviation, minimum, maximum, 10, 25, 50, 75, and 90th percentiles for all variables.
#'    \item \code{miss}: Data frame with total number of missing observations (i.e., countries) for each values in \code{id_time}.
#'  }
#' @references Egami and Lee. (2023+). Designing Multi-Context Studies for External Validity: Site Selection via Synthetic Purposive Sampling. Available at \url{https://naokiegami.com/paper/sps.pdf}.
#' @export

check_missing <- function(data, id_time = NULL){

  if (!is.data.frame(data)){
    stop('`data` must be a data.frame.')
  }

  if (!is.null(id_time)){
    if (!(id_time %in% names(data))){
    stop(print(paste0("Variable `", id_time, "' does not exist in `data`.")))
    }
  }

  df_desc <- NULL
  vars    <- names(data)
  remove  <- c()
  # dummify character/factor variables
  for (i in vars){
    if (is.character(data[[i]])|is.factor(data[[i]])){
      vars <- vars[vars != i]
      if (length(unique(na.omit(data[[i]]))) > 10){
        vars <- vars[vars != i]
        remove <- c(remove, i)
      }
      for (j in unique(na.omit(data[[i]]))){
        data[[paste0(i, ":", j)]] <- ifelse(is.na(data[[i]]), NA, as.numeric(as.character(data[[i]]) == j))
        vars <- c(vars, paste0(i, ":", j))
      }
    }
  }

  for (i in vars){
    avg <- sd <- min <- max <- qtile <- NA
    if (is.numeric(data[[i]])){
      qtile <- quantile(data[[i]], probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = T)
      avg   <- mean(data[[i]], na.rm = T)
      sd    <- sd(data[[i]],   na.rm = T)
      min   <- min(data[[i]],  na.rm = T)
      max   <- max(data[[i]],  na.rm = T)
    }
    # by-year missing values
    if (!is.null(id_time)){
      miss <- NULL
      for (y in unique(na.omit(data$year))){
        miss <- rbind(miss, data.frame(year = y, nmiss = sum(is.na(data[[i]][data$year == y]))))
      }
      miss <- tidyr::pivot_wider(miss, names_from = 'year', values_from = 'nmiss', names_prefix = ("nmiss."))
      df_desc <- rbind(df_desc,
                       cbind(data.frame(variable = i,
                                        mean = avg, stdev = sd,
                                        min = min, p10 = qtile[1], p25 = qtile[2], p50 = qtile[3], p75 = qtile[4], p90 = qtile[5], max = max,
                                        n = nrow(data), `nmiss.total` = sum(is.na(data[[i]]))), miss))
    }
    else{
      df_desc <- rbind(df_desc,
                       data.frame(variable = i,
                                  mean = avg, stdev = sd,
                                  min = min, p10 = qtile[1], p25 = qtile[2], p50 = qtile[3], p75 = qtile[4], p90 = qtile[5], max = max,
                                  n = nrow(data), `nmiss.total` = sum(is.na(data[[i]]))))
    }
  }
  row.names(df_desc) <- NULL

  cat("\n")
  cat("-------------------------\n")
  cat(" Descriptive Summary\n")
  cat("-------------------------\n\n")
  print(df_desc[, 1:(which(names(df_desc) == "n"))])
  cat("\n\n")
  cat("-------------------------\n")
  cat(" Missing Values\n")
  cat("-------------------------\n\n")
  print(df_desc[, c(1, which(names(df_desc) == "nmiss.total"):ncol(df_desc))])
  cat("\n\n")

  if (length(remove)>0){
    cat("Following character variable(s) has too many unique values and was not included in the descriptive summary:\n")
    print(remove)
    cat("\n")
  }

  invisible(list("desc" = df_desc[, 1:(which(names(df_desc) == "nmiss.total"))],
                 "miss" = df_desc[, c(1, which(names(df_desc) == "nmiss.total"):ncol(df_desc))]))
}
