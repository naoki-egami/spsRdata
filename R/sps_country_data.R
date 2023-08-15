#' Call particular years from the full country-level dataset
#' @param year (Default = \code{NULL}) A year in which the country-level dataset should be subsetted to. Can be either a single numeric value (e.g., 2010) or a range (e.g., 2010:2015). Returns the entire data if \code{NULL}.
#' @return A data.frame with new variables merged onto \code{data}.
#' @references Egami and Lee. (2023+). Designing Multi-Context Studies for External Validity: Site Selection via Synthetic Purposive Sampling. Available at \url{https://naokiegami.com/paper/sps.pdf}.
#' @export

sps_country_data <- function(year = NULL){
  data('sps_country_data_full')
  return(sps_country_data_full[which(sps_country_data_full$year %in% year),])
}
