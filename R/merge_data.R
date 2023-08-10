#' Merge external dataset
#' @param basedata data.frame for new variables to be merged onto. We recommend you use \code{sps_data}.
#' @param newdata data.frame that contains new variables. All variables included in the dataset except for the variables asserted in \code{id_country} and \code{id_year} will be merged on to \code{basedata} automatically.
#' @param id_country (Default = \code{NULL}) A country variable name included in \code{newdata}.
#' @param id_year (Optional, Default = \code{NULL}) A year variable name included in \code{newdata}.If \code{NULL}, values in new variables will be merged to every year in `basedata`.
#' @param iso3 (Default = \code{FALSE}) Logical \code{TRUE}/\code{FALSE} on whether or not the \code{id_country} is an ISO3 country code. If \code{TRUE}, \code{newdata} is merged only using the iso3 code. If \code{FALSE}, then \code{newdata} is merged using exact match, followed by a fuzzy match using \code{fuzzyjoin::stringdist_join()}.
#' @param ... Arguments passed onto \code{fuzzymatch::stringdist_join()}.
#' @import fuzzyjoin
#' @importFrom dplyr group_by_at slice_min
#' @return A dataframe with new variables merged.
#' @references Egami and Lee. (2023+). Designing Multi-Context Studies for External Validity: Site Selection via Synthetic Purposive Sampling. Available at \url{https://naokiegami.com/paper/sps.pdf}.
#' @export

merge_data <- function(basedata, newdata, id_year = NULL, id_country = NULL, iso3 = FALSE, ...){
  if (nrow(newdata[duplicated(newdata[,c(id_year, id_country)]),])>0){
    stop('ERROR: newdata is not unique by id_country (and id_year). Check the data before merging.')
  }
  if (!('iso3' %in% names(basedata))){
    stop('ERROR: make sure you have iso3 in basedata.')
  }
  if (iso3 == TRUE){
    xcol <- c('year', 'iso3')
    if (is.null(id_year)) xcol <- c('iso3')

    newdata$match <- 'exact:iso3'
    data <- merge(basedata,
                  newdata,
                  by.x = xcol,
                  by.y = c(id_year, id_country),
                  all.x = TRUE)
  }
  else{
    # Try exact matching first
    newvars <- names(newdata)[which(!names(newdata) %in% c(id_year, id_country))]
    base_unique <- basedata[,c('iso3', 'country')]
    base_unique <- base_unique[!duplicated(base_unique),]
    names(base_unique) <- c('iso3', 'cmatch')
    newdata$cmatch <- newdata[[id_country]]

    fuzzy <- stringdist_join(base_unique,
                             newdata,
                             by = "cmatch",
                             distance_col = "distance",
                             mode = "inner",
                             max_dist = 20,
                             ...)
    fuzzy <- dplyr::group_by_at(fuzzy, c('cmatch.y', id_year))
    fuzzy <- dplyr::slice_min(fuzzy, order_by = .data[['distance']], n = 1, with_ties = FALSE)
    if (max(fuzzy$distance)>0){
      warning(paste('\nFollowing countries were matched using fuzzy-match:\n',
                    paste(paste(fuzzy$cmatch.y[fuzzy$distance>0], '=>', fuzzy$cmatch.x[fuzzy$distance>0]), collapse = '\n '),
                    '\nPlease review and if necessary, recode the country names in your dataset or supply the ISO3 code instead.'))
    }
    # fuzzy <- fuzzy[, c('iso3', 'cmatch.x', id_country, id_year, newvars)]
    # names(fuzzy) <- c('iso3', 'country', 'cname_used', id_year, newvars)
    fuzzy <- fuzzy[, c('iso3', 'cmatch.x', id_year, newvars)]
    names(fuzzy) <- c('iso3', 'country', id_year, newvars)

    fuzzy$merge  <- 'fuzzy'
    newdata$merge <- 'exact'
    xcol <- c('year', 'country')
    if (is.null(id_year)) xcol <- c('country')
    data0 <- merge(basedata, newdata[,c(id_country, id_year, newvars, 'merge')], by.x = xcol, by.y = c(id_year, id_country), all.x = TRUE)
    data1 <- merge(data0, fuzzy, by.x = c(xcol, 'iso3'), by.y = c(id_year, 'country', 'iso3'), all.x = TRUE, suffixes = c('', '.f'))
    data1$match <- ifelse(is.na(data1$merge), data1$merge.f, data1$merge)
    # data1$cname_used <- ifelse(data1$match == 'exact', data1$country, data1$cname_used)
    for (i in newvars){
      data1[[i]] <- ifelse(is.na(data1$match), NA, ifelse(data1$match == 'exact', data1[[i]], data1[[paste0(i, '.f')]]))
    }
    # data <- data1[,c(names(basedata), newvars, 'match', 'cname_used')]
    data <- data1[,c(names(basedata), newvars)]
  }
  return(data)
}
