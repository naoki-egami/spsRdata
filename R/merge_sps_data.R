#' Merge new variables from V-Dem/World Bank or external dataset
#'
#' Allows users to either add new variable(s) from V-Dem or World Bank or merge their own data set to the baseline data set.
#'
#' @param data data.frame for new variables to be merged onto.
#' @param vars (Default = \code{NULL}) A vector with one or more variable names that should be merged onto \code{data}. Use \code{search_var()} to search for new variables.
#' @import vdemdata
#' @import wbstats
#' @import devtools
#' @import dplyr
#' @return A dataframe with new variables merged.
#' @references
#' Egami and Lee. (2023+). Designing Multi-Context Studies for External Validity: Site Selection via Synthetic Purposive Sampling. Available at \url{https://naokiegami.com/paper/sps.pdf}. \cr
#' Michael Coppedge et al. (2023). “V-Dem Dataset V13.” Varieties of Democracy (V-Dem) Project. \url{https://doi.org/10.23696/VDEMDS23}. \cr
#' World Bank. (2023). World Development Indicators. \url{https://databank.worldbank.org/source/world-development-indicators}.
#' @export

merge_sps_data <- function(data, vars = NULL){

  if (!is.data.frame(data)){
    stop('`data` must be a data.frame.')
  }
  if (is.null(vars)){
    stop('Variable names must be specified in `vars` argument. Use `search_var()` to search for variables to be included.')
  }

  wbi <- unique(toupper(wb_indicators(lang = 'en')$indicator_id))
  dne <- c()
  for (i in vars){
    if (i %in% names(data)){
      print(paste('Variable', i, 'already exists and will not be merged.'))
    }
    else if (i %in% names(vdemdata::vdem)){
      data <- merge(data,
                    vdemdata::vdem[,c('country_text_id', 'year', i)],
                    by.x = c('iso3', 'year'),
                    by.y = c('country_text_id', 'year'),
                    all.x = TRUE, sort = FALSE)
    }
    else if (toupper(i) %in% wbi){
      wb <- try(wbstats::wb_data(indicator = i, start_date = 2010, end_date = 2022)[,c('iso3c', 'date', i)])
      data <- merge(data,
                       wb,
                       by.x = c('iso3', 'year'),
                       by.y = c('iso3c', 'date'),
                       all.x = TRUE, sort = FALSE)

    }
    else{
      dne <- c(dne, i)
    }
  }
  if (length(dne)>0){
    cat('\n')
    cat('Following variables were not found in either World Bank or V-Dem data set:\n')
    cat(dne)
    cat('\n')
    cat('Check for typos or use `search_var()` to search for variables.\n')
  }
  return(data)
}
