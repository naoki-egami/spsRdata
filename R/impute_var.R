#' Impute Missing Values
#' @param data data.frame
#' @param id_site A site-level variable name included in \code{data}.
#' @param id_year A year variable name included in \code{data}.
#' @param id_impute (Default = \code{NULL}) Vector with one or more variable names for which imputation is performed. Imputes all variables in \code{data} except \code{id_site} and \code{id_year} if not specified.
#' @param method (Optional. Default = "miceranger") Imputation method. Choose from "carryforward", "amelia", and "miceranger".
#' @param n_impute (Optional. Used only when "miceranger" or "amelia" is selected in \code{method}. Default = 5) The number of imputed datasets to create (equivalent of argument \code{m} in \code{amelia()} and \code{miceRanger()}).
#' @param carryforward_yrs (Optional. Used only when "carryforward" is selected in \code{method}. Default = \code{NULL}) Maximum number of years allowed to be carried forward. If not specified, it will impute values from the most recent years available.
#' @param ... Arguments passed onto \code{Amelia::amelia()} or \code{miceRanger::miceRanger()}.
#' @importFrom dplyr arrange group_by
#' @importFrom tidyr fill
#' @importFrom Amelia amelia
#' @importFrom stats na.omit
#' @import miceRanger
#' @return A dataframe with new variables merged onto the base dataset.
#' @references Egami and Lee. (2023+). Designing Multi-Context Studies for External Validity: Site Selection via Synthetic Purposive Sampling. Available at \url{https://naokiegami.com/paper/sps.pdf}.
#' @export

impute_var <- function(data, id_site = NULL, id_year = NULL, id_impute = NULL, method = 'miceranger', carryforward_yrs = NULL, ...){

  if (is.null(id_impute)){
    id_impute <- setdiff(colnames(data)[colSums(is.na(data)) > 0], c(id_site, id_year))
    warning(paste('No variables selected to impute. Following variables will be imputed: ',
                   paste(id_impute, collapse = ", "), "\n"))
  }

  if (is.null(id_site) | is.null(id_year)){
    stop('Either id_site or id_year is unspecified.')
  }

  if (nrow(data[duplicated(data[,c(id_site, id_year)]),])>0){
    stop(paste0('Data is not unique by ', id_site, ' and ', id_year, '.'))
  }

  # Converting character and factor variables
  id_binary <- id_refcat <- c()
  for (i in setdiff(names(data[sapply(data, class) %in% c('character', 'factor')]), c(id_site, id_year))){
    id_impute <- id_impute[id_impute != i]
    id_refcat <- append(id_refcat, paste0(i, '_', unique(na.omit(data[[i]]))[1]))
    for (j in unique(na.omit(data[[i]]))){
      data[[paste0(i, '_', j)]] <- ifelse(is.na(data[[i]]), NA, as.numeric(data[[i]] == j))
      id_binary <- append(id_binary, paste0(i, '_', j))
    }
  }

  # Checking for binary variables
  id_binary <- append(id_binary, setdiff(names(data)[apply(data, 2, function(x) { all(is.numeric(x) & x %in% c(0,1,NA)) })], id_binary))
  id_impute <- id_impute[!id_impute %in% id_binary]

  # Carry-Forward
  if (method == 'carryforward'){
    if (is.null(carryforward_yrs)) carryforward_yrs <- max(data[[id_year]]) - min(data[[id_year]])

    imputed <- dplyr::arrange(data[data[[id_year]] %in% c((max(data[[id_year]])-carryforward_yrs):max(data[[id_year]])),],
                       .data[[id_site]], .data[[id_year]])
    imputed <- dplyr::group_by(imputed, .data[[id_site]])
    imputed <- tidyr::fill(imputed, all_of(c(id_impute, id_binary)), .direction = 'down')
    imputed <- imputed[, c(id_site, id_year, id_impute, id_binary)]

    merged <- merge(data,
                    imputed,
                    by = c(id_site, id_year),
                    all.x = TRUE,
                    suffixes = c('', '.imputed'))
    for (v in c(id_impute, id_binary)){
      merged[[v]] <- ifelse(is.na(merged[[v]]), merged[[paste0(v, '.imputed')]], merged[[v]])
    }
    merged <- merged[,names(merged)[!grepl('\\.imputed', names(merged))]]
  }

  # Amelia
  if (method == 'amelia'){
    ids <- setdiff(names(data[, sapply(data, class) == 'character']), c(id_site, id_impute))
    imputed <- amelia(x = data[, setdiff(names(data), id_refcat)],
                      m = n_impute,
                      idvars = ids,
                      ts = id_year,
                      cs = id_site,
                      ...)

    merged <- data
    for (v in c(id_impute, id_binary[!(id_binary %in% id_refcat)])){
      combined_df <- do.call(cbind, lapply(imputed$imputation, function(x) x[[v]]))
      if (v %in% id_binary){ merged[[v]] <- ifelse(rowMeans(combined_df, na.rm = T) < 0.5, 0, 1) }
      else{ merged[[v]] <- rowMeans(combined_df, na.rm = T) }
      }
    }

  # MICERanger
  if (method == 'miceranger'){
    imputed <- miceRanger(data = data[, sapply(data, class) != 'character'],
                          m    = n_impute,
                          vars = c(id_impute, id_binary),
                          returnModels = FALSE,
                          ...)
    imputed <- completeData(imputed)
    merged  <- data
    for (v in c(id_impute, id_binary)){
      combined_df <- do.call(cbind, lapply(imputed, function(x) x[[v]]))
      if (v %in% id_binary){ merged[[v]] <- ifelse(rowMeans(combined_df, na.rm = T) < 0.5, 0, 1) }
      else{ merged[[v]] <- rowMeans(combined_df, na.rm = T) }
    }
  }

  return(merged)
}