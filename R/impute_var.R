#' Impute Missing Values
#'
#' Imputes missing values in both numeric and character variables.
#' Character variables are converted into dummies for each unique values prior to imputation.
#' The dataset can be either cross-sectional or time-series cross-sectional.
#' Users may use this function to impute missing values on the site-level variables that they want to diversify prior to subsetting the data to the target population.
#'
#' @param data A \code{data.frame} containing variables to impute.
#' @param id_unit A unique identifier for sites. A column name in \code{data}.
#' @param id_time (Default = \code{NULL}) A unique identifier for time index. A column name in \code{data}. If unspecified, it assumes \code{data} is cross-sectional.
#' @param var_impute (Default = \code{NULL}) A vector with one or more variable names for which imputation is performed. Imputes all variables that contain missing values in \code{data} except \code{id_unit} and \code{id_time} if not specified.
#' @param var_predictor (Default = \code{NULL}) A vector with one or more variable names that we use as predictors to impute variables in \code{var_impute}. If \code{NULL}, the function uses all variables in \code{data} except for variables in \code{var_impute}.
#' @param var_ord (Default = \code{NULL}) A vector with one or more variable names to be imputed which are ordinal. Binary variable names can be in either \code{var_ord} or \code{var_nom}.
#' @param var_nom (Default = \code{NULL}) A vector with one or more variable names to be imputed which are nominal.
#' @param var_lgstc (Default = \code{NULL}) A vector with one or more variable names to be imputed which are proportional (ranging between 0 and 1).
#' @param method (Optional. Default = "amelia") Imputation method. Choose from "amelia", "mice", and "miceranger".
#' @param n_impute (Optional. Default = 5) The number of imputed datasets to create (equivalent of argument \code{m} in \code{amelia()}, \code{mice()}, and \code{miceRanger()}).
#' @param ... Arguments passed onto \code{Amelia::amelia()}, \code{mice::mice()}, or \code{miceRanger::miceRanger()}.
#' @import mice
#' @import miceRanger
#' @importFrom Amelia amelia
#' @importFrom stats na.omit
#' @return A dataframe with new variables merged onto the base dataset.
#' @references Egami and Lee. (2023+). Designing Multi-Context Studies for External Validity: Site Selection via Synthetic Purposive Sampling. Available at \url{https://naokiegami.com/paper/sps.pdf}.
#' @export

impute_var <- function(data, id_unit = NULL, id_time = NULL, var_impute = NULL, var_predictor = NULL, var_ord = NULL, var_nom = NULL, var_lgstc = NULL,
                       method = 'amelia', n_impute = 5, ...){

  ## ################
  ## Housekeeping
  ## ################

  ## data
  if (!is.data.frame(data)){
    stop('`data` must be a data frame.')
  }

  ## id_unit
  id_unit_internal <- FALSE
  if (is.null(id_unit) == TRUE & is.null(id_time) == FALSE){
    stop('`id_unit` must be specified.')
  }else if(is.null(id_unit) == TRUE & is.null(id_time) == TRUE){
    id_unit <- seq(1:nrow(data))
    data <- cbind(data, id_unit)
    colnames(data)[ncol(data)] <- "id_unit"
    id_unit <- "id_unit"
    id_unit_internal <- TRUE
  }
  if ((id_unit %in% colnames(data)) == FALSE){
    stop(" `id_unit` should be one of colnames(data) ")
  }

  ## id_time
  if (is.null(id_time)){
    cat('`id_time` unspecified. Assumes dataset is cross-sectional.\n')
  }else{
    if((id_time %in% colnames(data)) == FALSE){
      stop(" `id_time` should be one of colnames(data) ")
    }
  }
  ## duplication check
  if (!is.null(id_time) & nrow(data[duplicated(data[,c(id_unit, id_time)]),])>0){
    stop(paste0('Data is not unique by ', id_unit, ' and ', id_time, '.'))
  }
  if (is.null(id_time) & nrow(data[duplicated(data[,id_unit]),])>0){
    stop(paste0('Data is not unique by ', id_unit, '.'))
  }

  ## var_impute
  if (is.null(var_impute)){
    warning('No variables selected to impute. All variables with missing values will be imputed.\n')
  }else{
    if (all(var_impute %in% colnames(data)) == FALSE){
      stop("`var_impute` should be a subset of `colnames(data)`.")
    }
  }

  ## var_ords
  if (!is.null(var_ord)){
    if (all(var_ord %in% colnames(data)) == FALSE){
      stop("`var_ord` does not exist in `data`.")
    }
    if (!is.null(var_impute)){
      if (all(var_ord %in% var_impute) == FALSE){
        warning('One or more variables in `var_ord` is not included in `var_impute`. Assuming all variables are to be imputed.\n')
      }
    }
  }

  ## var_nom
  if (!is.null(var_nom)){
    if (all(var_nom %in% colnames(data)) == FALSE){
      stop("`var_nom` does not exist in `data`.")
    }
    if (!is.null(var_impute)){
      if (all(var_nom %in% var_impute) == FALSE){
        warning('One or more variables in `var_nom` is not included in `var_impute`. Assuming all variables are to be imputed.\n')
      }
    }
  }

  ## var_lgstc
  if (!is.null(var_lgstc)){
    if (all(var_lgstc %in% colnames(data)) == FALSE){
      stop("`var_lgstc` does not exist in `data`.")
    }
    for (i in var_lgstc){
      if (!is.numeric(data[[i]])){
        stop(paste0("`var_lgstc` variable ", i, "is not numeric."))
      }
      if (min(data[[i]], na.rm = T) < 0 | max(data[[i]], na.rm = T) > 1){
        stop(paste0("`var_lgstc` variable ", i, "must be between 0 and 1."))
      }
    }
    if (!is.null(var_impute)){
      if (all(var_lgstc %in% var_impute) == FALSE){
        warning('One or more variables in `var_lgstc` is not included in `var_impute`. Assuming all variables are to be imputed.\n')
      }
    }
  }

  ## var duplicates
  if (any(var_lgstc %in% var_nom)){
    stop('One or more variables in `var_lgstc` is also in `var_nom`. A variable can only be assigend to one transformation.')
  }
  if (any(var_lgstc %in% var_ord)){
    stop('One or more variables in `var_lgstc` is also in `var_ord`. A variable can only be assigend to one transformation.')
  }
  if (any(var_nom %in% var_ord)){
    stop('One or more variables in `var_ord` is also in `var_nom`. A variable can only be assigend to one transformation.')
  }

  ## var_predictor
  if (is.null(var_predictor)){
    warning('`var_predictor` unspecified. All variables except for unique identifiers and variables in `var_impute` are used as predictors.\n')
  }else{
    if(all(var_predictor %in% colnames(data)) == FALSE){
      stop(" `var_predictor` should be a subset of `colnames(data)`. ")
    }
  }

  ## method
  if((method %in% c("amelia", 'mice', 'miceranger')) == FALSE){
    stop(" `method` should be either`amelia` or `mice` or `miceranger`.  ")
  }

  ############

  # Get var_impute
  if (is.null(var_impute)){
    # Grab all variables
    var_impute <- setdiff(colnames(data)[colSums(is.na(data)) > 0], c(id_unit, id_time))
    # Check for nominal variables
    for (i in var_impute){
      if (class(data[[i]]) %in% c('factor', 'character')){
        data[[i]] <- as.character(data[[i]])
        var_nom   <- unique(c(var_nom, i))
      }
    }
  }else{
    var_impute <- unique(c(var_impute, var_ord, var_nom, var_lgstc))
  }
  var_num <- setdiff(var_impute, c(var_ord, var_nom, var_lgstc))

  cat("\n")
  cat("-----------------------------\n")
  cat(paste("Following", length(var_impute), "variables will be imputed using", method, ":\n"))
  cat(ifelse(length(var_num)>0, paste("Continuous:", paste(var_num, collapse = ", "), "\n"), ""))
  cat(ifelse(length(var_ord)>0, paste("Ordinal:", paste(var_ord, collapse = ", "), "\n"), ""))
  cat(ifelse(length(var_nom)>0, paste("Nominal:", paste(var_nom, collapse = ", "), "\n"), ""))
  cat(ifelse(length(var_lgstc)>0, paste("Proportional:", paste(var_lgstc, collapse = ", "), "\n"), ""))
  cat("-----------------------------\n")
  cat("\n")

  # Checking predictors
  data_x <- data
  if (!is.null(var_predictor)){
    data_x <- data[,c(id_time, id_unit, var_impute, var_predictor)]
  }


  ids <- setdiff(names(data_x)[sapply(data_x, is.character)], c(id_unit, var_impute))
  ids <- c(ids, setdiff(names(data_x[vapply(data_x, function(x) length(unique(x)) == 1, logical(1L))]), c(id_unit, var_impute)))
  if (length(ids)==0) ids <- NULL

  # Amelia
  if (method == 'amelia'){
    imputed <- amelia(x = data_x,
                      m = n_impute,
                      idvars = ids,
                      ts = id_time,
                      cs = id_unit,
                      ords = var_ord,
                      noms = var_nom,
                      lgstc = var_lgstc,
                      ...)

    merged <- data
    for (v in var_impute){
      combined_df <- do.call(cbind, lapply(imputed$imputation, function(x) x[[v]]))
      if (v %in% c(var_ord, var_nom)){
        merged[[v]] <- apply(combined_df, 1, function(x) sample(x, 1))
      }else{
          merged[[v]] <- rowMeans(combined_df, na.rm = T)
          }
      }
    }

  # miceranger
  if (method == 'miceranger'){
    imputed <- miceRanger(data = data_x,
                    m    = n_impute,
                    vars = var_impute,
                    returnModels = FALSE,
                    ...)
    imputed <- completeData(imputed)

    merged  <- data
    for (v in var_impute){
      combined_df <- do.call(cbind, lapply(imputed, function(x) x[[v]]))
      if (v %in% c(var_ord, var_nom)){
        merged[[v]] <- apply(combined_df, 1, function(x) sample(x, 1))
      }else{
        merged[[v]] <- rowMeans(combined_df, na.rm = T)
      }
    }
  }

  # mice
  if (method == 'mice'){
    for (i in c(var_ord, var_nom)){
      data_x[[i]] <- factor(data_x[[i]])
    }

    var_bin <- c()
    for (i in c(var_ord, var_nom)){
      if (length(unique(na.omit(data_x[[i]]))) == 2){
        var_bin <- c(var_bin, i)
        var_ord <- var_ord[var_ord != i]
        var_nom <- var_ord[var_nom != i]
      }
    }

    mice_method <- rep("", length(var_impute))
    mice_method[which(var_impute %in% var_num)]   <- "2lonly.mean"
    mice_method[which(var_impute %in% var_lgstc)] <- "2lonly.mean"
    mice_method[which(var_impute %in% var_bin)] <- "logreg"
    mice_method[which(var_impute %in% var_ord)] <- "polr"
    mice_method[which(var_impute %in% var_nom)] <- "polyreg"

    pred <- quickpred(data_x,
                      mincor = 0.1,
                      exclude = c(id_time, id_unit, ids))
    pred[,id_unit] <- -2
    if (!is.null(id_time)){
      pred[,id_time] <- 2
      data_x[[id_time]] <- as.integer(data_x[[id_time]])
    }

    data_x[[id_unit]] <- as.integer(as.factor(data_x[[id_unit]]))
    imputed <- mice(data = data_x, m = n_impute, predictorMatrix = pred[var_impute,], method = mice_method, blocks = var_impute)
    imputed <- lapply(1:n_impute, function(x) complete(imputed, x))

    merged  <- data
    for (v in var_impute){
      if (v %in% c(var_ord, var_nom)){
        combined_df <- do.call(cbind, lapply(imputed, function(x) as.character(x[[v]])))
        merged[[v]] <- apply(combined_df, 1, function(x) sample(x, 1))
        if (v %in% var_ord){
          merged[[v]] <- as.numeric(merged[[v]])
        }
      }else{
        combined_df <- do.call(cbind, lapply(imputed, function(x) x[[v]]))
        merged[[v]] <- rowMeans(combined_df, na.rm = T)
        merged[[v]] <- ifelse(is.nan(merged[[v]]), NA, merged[[v]])
      }
    }
  }

  if(id_unit_internal == TRUE){
    merged[, "id_unit"] <- NULL
  }

  return(merged)
}
