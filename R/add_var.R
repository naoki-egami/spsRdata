#' Synthetic Purposive Sampling: Dataset for Site Selection
#' Add Additional Covariates from V-Dem Dataset and/or World Bank API
#' @param data Dataframe for new variables to be merged onto. Default is sps_data. A dataset must have a column 'iso3' and 'year'.
#' @param vars A character vector with one or more variable names from V-Dem dataset or World Bank Indicators.
#' @import vdemdata
#' @import wbstats
#' @import dplyr
#' @importFrom labelled var_label
#' @return A dataframe with new variables merged on
#' @examples
#' data(sps_data)
#' new <- add_var(vars = "SM.POP.TOTL.ZS")
#' @references Egami and Lee. (2023+). Designing Multi-Context Studies for External Validity: Site Selection via Synthetic Purposive Sampling. Available at \url{https://naokiegami.com/paper/sps.pdf}.
#' @export

add_var <- function(data = sps_data, vars = NULL){
  for (i in vars){
    if (i %in% names(data)){
      stop(paste('Variable', i, 'already exists.'))
    }
    if (i %in% names(vdemdata::vdem)){
      print(paste('Merging', i, 'from V-Dem'))
      data <- data %>% left_join(vdemdata::vdem[,c('country_text_id', 'year', i)], by = c('iso3'='country_text_id', 'year'))
    }
    else{
      print(paste('Merging', i, 'from World Bank'))
      wb <- wbstats::wb_data(indicator = i, start_date = 2010, end_date = 2022)[,c('iso3c', 'date', i)]
      labelled::var_label(wb[[i]]) <- NULL
      data <- data %>% left_join(wb, by = c('iso3' = 'iso3c', 'year' = 'date'))
    }
  }
  return(data)
}
