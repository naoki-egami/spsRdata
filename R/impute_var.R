#' Impute Missing Values
#'
#' Imputes missing values in both numeric and character variables.
#' Character variables are converted into dummies for each unique values prior to imputation.
#' The dataset can be either cross-sectional or time-series cross-sectional.
#' Users may use this function to impute missing values on the site-level variables that they want to diversify prior to subsetting the data to the target population.
#'
#' @param data A data.frame containing variables to impute.
#' @param id_unit A unique identifier for sites. A column name in \code{data}.
#' @param id_time (Default = \code{NULL}) A unique identifier for time index. A column name in \code{data}. If unspecified, it assumes \code{data} is cross-sectional.
#' @param var_impute (Default = \code{NULL}) A vector with one or more variable names for which imputation is performed. Imputes all variables that contain missing values in \code{data} except \code{id_unit} and \code{id_time} if not specified.
#' @param var_predictor (Default = \code{NULL}) A vector with one or more variable names that we use as predictors to impute variables in \code{var_impute}. If \code{NULL}, the function uses all variables in \code{data} except for variables in \code{var_impute}.
#' @param method (Optional. Default = "amelia") Imputation method. Choose from "amelia", and "miceranger".
#' @param n_impute (Optional. Default = 5) The number of imputed datasets to create (equivalent of argument \code{m} in \code{amelia()} and \code{miceRanger()}).
#' @param ... Arguments passed onto \code{Amelia::amelia()} or \code{miceRanger::miceRanger()}.
#' @import miceRanger
#' @importFrom Amelia amelia
#' @importFrom stats na.omit
#' @return A dataframe with new variables merged onto the base dataset.
#' @references Egami and Lee. (2023+). Designing Multi-Context Studies for External Validity: Site Selection via Synthetic Purposive Sampling. Available at \url{https://naokiegami.com/paper/sps.pdf}.
#' @export

library(spsRdata)
data <- sps_country_data[sps_country_data$year == 2018, c('iso3', 'e_p_polity', 'AG.LND.TOTL.K2', 'v2x_libdem', 'subregion', 'SP.POP.TOTL', 'v2x_civlib')]
id_unit = 'iso3'
id_time = NULL
var_impute = NULL
var_predictor = NULL

impute_var <- function(data, id_unit = NULL, id_time = NULL, var_impute = NULL, var_predictor = NULL, method = 'amelia', n_impute = 5, ...){

  if (is.null(id_unit)){
    stop('id_unit must be specified.')
  }

  if (is.null(var_impute)){
    var_impute <- setdiff(colnames(data)[colSums(is.na(data)) > 0], c(id_unit, id_time))
    print(paste('No variables selected to impute. Following variables will be imputed: ',
                paste(var_impute, collapse = ", "), "\n"))
  }

  if (is.null(id_time)){
    print('id_time unspecified. Assumes dataset is cross-sectional.\n')
  }

  if (is.null(var_predictor)){
    print('var_predictor unspecified. All variables except for unique identifiers and variables stored in var_impute.\n')
  }

  if (!is.null(id_time) & nrow(data[duplicated(data[,c(id_unit, id_time)]),])>0){
    stop(paste0('Data is not unique by ', id_unit, ' and ', id_time, '.'))
  }

  if (is.null(id_time) & nrow(data[duplicated(data[,id_unit]),])>0){
    stop(paste0('Data is not unique by ', id_unit, '.'))
  }

  # Converting character and factor variables
  id_binary <- id_refcat <- c()
  for (i in intersect(names(data[sapply(data, class) %in% c('character', 'factor')]), var_impute)){
    var_impute <- var_impute[var_impute != i]
    id_refcat <- append(id_refcat, paste0(i, '_', unique(na.omit(data[[i]]))[1]))
    for (j in unique(na.omit(data[[i]]))){
      data[[paste0(i, '_', j)]] <- ifelse(is.na(data[[i]]), NA, as.numeric(data[[i]] == j))
      id_binary <- append(id_binary, paste0(i, '_', j))
    }
  }

  # Checking for binary variables
  id_binary <- append(id_binary, setdiff(names(data)[apply(data, 2, function(x) { all(is.numeric(x) & x %in% c(0,1,NA)) })], id_binary))
  var_impute <- var_impute[!var_impute %in% id_binary]

  # Checking predictors
  data_x <- data
  if (!is.null(var_predictor)){
    data_x <- data[,c(id_time, id_unit, var_impute, id_binary, var_predictor)]
  }

  # Amelia
  if (method == 'amelia'){
    ids <- setdiff(names(data_x[, sapply(data_x, class) == 'character']), c(id_unit, var_impute))
    ids <- c(ids, setdiff(names(data_x[vapply(data_x, function(x) length(unique(x)) == 1, logical(1L))]), c(id_unit, var_impute)))
    imputed <- amelia(x = data_x[, setdiff(names(data_x), id_refcat)],
                      m = n_impute,
                      idvars = ids,
                      ts = id_time,
                      cs = id_unit,
                      ...)

    merged <- data
    for (v in c(var_impute, id_binary[!(id_binary %in% id_refcat)])){
      combined_df <- do.call(cbind, lapply(imputed$imputation, function(x) x[[v]]))
      if (v %in% id_binary){ merged[[v]] <- ifelse(rowMeans(combined_df, na.rm = T) < 0.5, 0, 1) }
      else{ merged[[v]] <- rowMeans(combined_df, na.rm = T) }
      }
    }

  # MICERanger
  if (method == 'miceranger'){
    imputed <- miceRanger(data = data_x[, sapply(data_x, class) != 'character'],
                          m    = n_impute,
                          vars = c(var_impute, id_binary),
                          returnModels = FALSE,
                          ...)
    imputed <- completeData(imputed)

    merged  <- data
    for (v in c(var_impute, id_binary)){
      combined_df <- do.call(cbind, lapply(imputed, function(x) x[[v]]))
      if (v %in% id_binary){ merged[[v]] <- ifelse(rowMeans(combined_df, na.rm = T) < 0.5, 0, 1) }
      else{ merged[[v]] <- rowMeans(combined_df, na.rm = T) }
    }
  }

  return(merged)
}
