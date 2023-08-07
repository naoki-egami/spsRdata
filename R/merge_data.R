#' Merge external dataset
#' @param basedata Data frame for new variables to be merged onto. We recommend you use \code{sps_data}.
#' @param newdata Data frame that contains new variables. All variables included in the dataset except for the variables asserted in \code{countryvar} and \code{yearvar} will be merged on to \code{basedata} automatically.
#' @param yearvar Year column name in \code{newdata}.
#' @param countryvar Country column name in \code{newdata}.
#' @param iso Whether or not the \code{countryvar} is an ISO3 code. If TRUE, the \code{newdata} is merged only using the iso3 code. If FALSE, the \code{newdata} is merged using exact match, followed by a probabilistic match using \code{fastLink} Default is FALSE.
#' @import fuzzyjoin
#' @importFrom dplyr group_by_at slice_min
#' @return A dataframe with new variables merged.
#' @references Egami and Lee. (2023+). Designing Multi-Context Studies for External Validity: Site Selection via Synthetic Purposive Sampling. Available at \url{https://naokiegami.com/paper/sps.pdf}.
#' @export

merge_data <- function(basedata, newdata = NULL, yearvar = NULL, countryvar = NULL, iso = FALSE, fuzzy_match = "fuzzyjoin"){
  if (nrow(newdata[duplicated(newdata[,c(yearvar, countryvar)]),])>0){
    stop('ERROR: newdata is not unique by countryvar (and yearvar). Check the data before merging.')
  }
  if (!('iso3' %in% names(basedata))){
    stop('ERROR: make sure you have iso3 in basedata.')
  }
  if (iso == TRUE){
    xcol <- c('year', 'iso3')
    if (is.null(yearvar)) xcol <- c('iso3')

    newdata$match <- 'exact:iso3'
    data <- merge(basedata,
                  newdata,
                  by.x = xcol,
                  by.y = c(yearvar, countryvar),
                  all.x = TRUE)
  }
  else{
    # Try exact matching first
    newvars <- names(newdata)[which(!names(newdata) %in% c(yearvar, countryvar))]
    base_unique <- basedata[,c('iso3', 'country')]
    base_unique <- base_unique[!duplicated(base_unique),]
    names(base_unique) <- c('iso3', 'cmatch')
    newdata$cmatch <- newdata[[countryvar]]

    fuzzy <- stringdist_inner_join(base_unique, newdata,
                                   by = "cmatch",
                                   distance_col = "distance",
                                   max_dist = 20) %>%
      dplyr::group_by_at(c('cmatch.y', yearvar)) %>%
      dplyr::slice_min(order_by = .data[['distance']], n = 1)
    fuzzy <- fuzzy[, c('iso3', 'cmatch.x', countryvar, yearvar, newvars)]
    names(fuzzy) <- c('iso3', 'country', 'cname_used', yearvar, newvars)

    fuzzy$merge  <- 'fuzzyjoin'
    newdata$merge <- 'exact'
    xcol <- c('year', 'country')
    if (is.null(yearvar)) xcol <- c('country')
    data0 <- merge(basedata, newdata[,c(countryvar, yearvar, newvars, 'merge')], by.x = xcol, by.y = c(yearvar, countryvar), all.x = TRUE)
    data1 <- merge(data0, fuzzy, by.x = c('year', 'iso3', 'country'), by.y = c(yearvar, 'iso3', 'country'), all.x = TRUE, suffixes = c('', '.f'))
    data1$match <- ifelse(is.na(data1$merge), data1$merge.f, data1$merge)
    data1$cname_used <- ifelse(data1$match == 'exact', data1$country, data1$cname_used)
    for (i in newvars){
      data1[[i]] <- ifelse(is.na(data1$match), NA, ifelse(data1$match == 'exact', data1[[i]], data1[[paste0(i, '.f')]]))
    }
    data <- data1[,c(names(basedata), newvars, 'match', 'cname_used')]
  }
  return(data)
}
