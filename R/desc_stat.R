#' Provide useful summary statistic of selected variables
#' @param data Data frame for new variables to be merged onto. Default is sps_data.
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom Hmisc rcorr
#' @importFrom stats quantile na.omit
#' @return \code{desc_stat} returns a list containing the following elements:
#'  \itemize{
#'    \item \code{desc}: Data frame containing descriptive statistics including mean, standard deviation, minimum, maximum, 10, 25, 50, 75, and 90th percentiles for all variables.
#'    \item \code{miss}: Data frame with total number of missing observations (i.e., countries) for each year.
#'    \item \code{corr}: Correlation matrix of all variables.
#'  }
#' @references Egami and Lee. (2023+). Designing Multi-Context Studies for External Validity: Site Selection via Synthetic Purposive Sampling. Available at \url{https://naokiegami.com/paper/sps.pdf}.
#' @export

desc_stat <- function(data){
  # descriptive stat
  df_desc <- NULL
  vars    <- names(data)[which(!(names(data) %in% c("country", "iso3", "region", "year", "match", "lang", "cname_used")))]

  # dummify character/factor variables
  for (i in vars){
    if (is.character(data[[i]])|is.factor(data[[i]])){
      vars <- vars[vars != i]
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
    miss <- NULL
    for (y in unique(na.omit(data$year))){
      miss <- bind_rows(miss, data.frame(year = y, nmiss = sum(is.na(data[[i]][data$year == y]))))
    }
    miss <- tidyr::pivot_wider(miss, names_from = year, values_from = nmiss, names_prefix = ("nmiss:"))
    df_desc <- bind_rows(df_desc,
                         cbind(data.frame(variable = i,
                                          mean = avg, stdev = sd,
                                          min = min, p10 = qtile[1], p25 = qtile[2], p50 = qtile[3], p75 = qtile[4], p90 = qtile[5], max = max,
                                          n = nrow(data), nmiss_total = sum(is.na(data[[i]]))), miss))
  }
  row.names(df_desc) <- NULL

  # correlation check
  cor <- Hmisc::rcorr(as.matrix(data[,vars]))

  return(list(desc = df_desc[, 1:(which(names(df_desc) == "nmiss_total"))],
              miss = df_desc[, c(1, which(names(df_desc) == "nmiss_total"):ncol(df_desc))],
              corr = cor$r))
}
