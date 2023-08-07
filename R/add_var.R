#' Add Additional Covariates from V-Dem Dataset and/or World Bank API
#' @param data Dataframe for new variables to be merged onto (we recommend you use \code{sps_data}). A dataset must have a column 'iso3' and 'year'.
#' @param vars A character vector with one or more variable names from V-Dem dataset or World Bank Indicators.
#' @import vdemdata
#' @import wbstats
#' @import dplyr
#' @import devtools
#' @return A dataframe with new variables merged on
#' @references Egami and Lee. (2023+). Designing Multi-Context Studies for External Validity: Site Selection via Synthetic Purposive Sampling. Available at \url{https://naokiegami.com/paper/sps.pdf}.
#' @export

add_var <- function(data, vars = NULL){
  for (i in vars){
    if (i %in% names(data)){
      stop(paste('Variable', i, 'already exists.'))
    }
    if (i %in% names(vdemdata::vdem)){
      print(paste('Merging', i, 'from V-Dem'))
      data <- merge(data,
                    vdemdata::vdem[,c('country_text_id', 'year', i)],
                    by.x = c('iso3', 'year'),
                    by.y = c('country_text_id', 'year'),
                    all.x = TRUE, sort = FALSE)
    }
    else{
      print(paste('Merging', i, 'from World Bank'))
      wb <- wbstats::wb_data(indicator = i, start_date = 2010, end_date = 2022)[,c('iso3c', 'date', i)]
      data <- merge(data,
                    wb,
                    by.x = c('iso3', 'year'),
                    by.y = c('iso3c', 'date'),
                    all.x = TRUE, sort = FALSE)
    }
  }
  return(data)
}
