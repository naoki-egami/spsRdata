#' Search for Additional Covariates from V-Dem Dataset and World Bank API
#'
#' Allows users to search for other country-level variables in V-Dem and World Bank using keywords.
#'
#' @param keyword A character string with a keyword to search for. Also allows for regex notation.
#' @import vdemdata
#' @import wbstats
#' @import devtools
#' @return A dataframe with list of variable names, descriptions, variable source, and coverage.
#' @references
#' Egami and Lee. (2023+). Designing Multi-Context Studies for External Validity: Site Selection via Synthetic Purposive Sampling. Available at \url{https://naokiegami.com/paper/sps.pdf}. \cr
#' Michael Coppedge et al. (2023). “V-Dem Dataset V13.” Varieties of Democracy (V-Dem) Project. \url{https://doi.org/10.23696/VDEMDS23}.\cr
#' World Bank. (2023). World Development Indicators. \url{https://databank.worldbank.org/source/world-development-indicators}.
#' @export

search_sps_data <- function(keyword = NULL){

  if (class(keyword) != 'character'){
    stop('Incorrect class for `keyword`.')
  }

  vdem <- try(find_var(keyword)[,c('tag', 'name', 'clarification', 'years')])
  names(vdem) <- c('Variable', 'Name', 'Description', 'Coverage')

  wb   <- try(wb_search(keyword)[,c('indicator_id', 'indicator', 'indicator_desc')])
  names(wb) <- c('Variable', 'Name', 'Description')

  if (nrow(vdem) == 0 & nrow(wb) == 0){
    stop(paste0('No results based on a keyword ', keyword))
  }

  else{
    if (nrow(vdem)>0) vdem$Source <- 'V-DEM'
    if (nrow(wb)>0){
      wb$Coverage <- 'Annual'
      wb$Source   <- 'World Bank'
    }

    result <- rbind(vdem, wb)
    result <- result[(!is.na(result$Name) | !is.na(result$Description)),]
    return(result)
  }
}
