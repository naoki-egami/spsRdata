#' Impute Missing Values
#' @param data Dataframe for new variables to be merged onto (we recommend you use \code{sps_data}).
#' @param countryvar Country column name in \code{data}. Default is "iso3".
#' @param yearvar Year column name in \code{data}. Default is "year".
#' @param imputevars A character vector with one or more variable names for which imputation is performed.
#' @param method Imputation method. Choose from "carryforward" (default), "amelia", and "miceranger".
#' @param carryforward_yrs Maximum number of years allowed to be carried forward. Used only when "carryforward" is selected for \code{method}. Default is NULL, which will impute values from the most recent years available.
#' @param ... Arguments passed onto \code{Amelia::amelia} or \code{miceRanger::miceRanger}.
#' @importFrom dplyr arrange group_by
#' @importFrom tidyr fill
#' @importFrom Amelia amelia
#' @importFrom stats na.omit
#' @import miceRanger
#' @return A dataframe with new variables merged on
#' @references Egami and Lee. (2023+). Designing Multi-Context Studies for External Validity: Site Selection via Synthetic Purposive Sampling. Available at \url{https://naokiegami.com/paper/sps.pdf}.
#' @export

impute_var <- function(data, countryvar = NULL, yearvar = NULL, imputevars = NULL, method = 'carryforward', carryforward_yrs = NULL, ...){

  if (is.null(imputevars)){
    stop('ERROR: No variables selected to impute. Make sure to include the imputevars argument.')
  }

  if (nrow(data[duplicated(data[,c(countryvar, yearvar)]),])>0){
    stop(paste0('ERROR: Data is not unique by ', countryvar, ' and ', yearvar, '.'))
  }

  # Carry-Forward
  if (method == 'carryforward'){
    if (is.null(carryforward_yrs)) carryforward_yrs <- max(data[[yearvar]]) - min(data[[yearvar]])

    imputed <- data[data[[yearvar]] %in% c((max(data[[yearvar]])-carryforward_yrs):max(data[[yearvar]])),] %>%
      arrange(.data[[countryvar]], .data[[yearvar]]) %>%
      group_by(.data[[countryvar]]) %>%
      fill(all_of(imputevars), .direction = 'down')
    imputed <- imputed[, c(countryvar, yearvar, imputevars)]

    merged <- merge(data,
                    imputed,
                    by = c(countryvar, yearvar),
                    all.x = TRUE,
                    suffixes = c('', '.imputed'))
    for (v in imputevars){
      merged[[v]] <- ifelse(is.na(merged[[v]]), merged[[paste0(v, '.imputed')]], merged[[v]])
    }
    merged <- merged[,names(merged)[!grepl('\\.imputed', names(merged))]]
  }

  # Amelia
  if (method == 'amelia'){
    ids <- names(data[, sapply(data, class) == 'character'])
    imputed <- amelia(x = data,
                      m = 5,
                      idvars = ids[ids != countryvar],
                      ts = yearvar,
                      cs = countryvar,
                      ...)

    merged <- data
    for (v in imputevars){
      combined_df <- do.call(cbind, lapply(imputed$imputation, function(x) x[[v]]))
      merged[[v]] <- rowMeans(combined_df, na.rm = T)
      }
    }

  # MICERanger
  if (method == 'miceranger'){
    imputed <- miceRanger(data = data,
                          m    = 5,
                          vars = imputevars,
                          returnModels = FALSE,
                          ...)
    imputed <- completeData(imputed)
    merged <- data
    for (v in imputevars){
      combined_df <- do.call(cbind, lapply(imputed, function(x) x[[v]]))
      merged[[v]] <- rowMeans(combined_df, na.rm = T)
    }
  }

  return(merged)
}
