#' Merge external dataset
#' @param basedata Data frame for new variables to be merged onto. We recommend you use \code{sps_data}.
#' @param newdata Data frame that contains new variables. All variables included in the dataset except for the variables asserted in \code{countryvar} and \code{yearvar} will be merged on to \code{basedata} automatically.
#' @param yearvar Year column name in \code{newdata}.
#' @param countryvar Country column name in \code{newdata}.
#' @param iso Whether or not the \code{countryvar} is an ISO3 code. If TRUE, the \code{newdata} is merged only using the iso3 code. If FALSE, the \code{newdata} is merged using exact match, followed by a probabilistic match using \code{fastLink} Default is FALSE.
#' @import fastLink
#' @import dplyr
#' @import wbstats
#' @return A dataframe with new variables merged.
#' @references Egami and Lee. (2023+). Designing Multi-Context Studies for External Validity: Site Selection via Synthetic Purposive Sampling. Available at \url{https://naokiegami.com/paper/sps.pdf}.
#' @export

merge_data <- function(basedata, newdata = NULL, yearvar = NULL, countryvar = NULL, iso = FALSE){
  if (nrow(newdata[duplicated(newdata[,c(yearvar, countryvar)]),])>0){
    stop('ERROR: newdata is not unique by countryvar (and yearvar). Check the data before merging.')
  }
  if (!('iso3' %in% names(basedata))){
    stop('ERROR: make sure you have iso3 in basedata.')
  }
  if (iso == TRUE){
    if (is.null(yearvar)){
      data <- left_join(basedata,
                        newdata %>% mutate(match = 'exact:iso3'),
                        by = c('iso3' = countryvar))
    }
    else{
      data <- left_join(basedata,
                        newdata %>% mutate(match = 'exact:iso3'),
                        by = c('year' = yearvar, 'iso3' = countryvar))
    }
  }
  else{
    # Merging world bank country names to do a better fuzzy match
    basedata_u <- distinct(basedata[,c('iso3', 'country')]) %>% rename(cmatch = country) %>%
      left_join(wb_countries() %>% transmute(iso3 = iso3c, cmatch_wb = country))
    newvars    <- names(newdata)[!(names(newdata) %in% c(yearvar, countryvar))]
    newdata$cmatch    <- newdata[[countryvar]]
    newdata$cmatch_wb <- newdata[[countryvar]]
    crosswalk <- fastLink(dfA = newdata,
                          dfB = basedata_u,
                          varnames         = c('cmatch', 'cmatch_wb'),
                          stringdist.match = c('cmatch', 'cmatch_wb'),
                          partial.match    = c('cmatch', 'cmatch_wb'),
                          threshold.match = 0.0001)
    dfmatch <- getMatches(dfA = newdata,
                          dfB = basedata_u,
                          fl.out = crosswalk,
                          threshold.match = 0.0001) %>%
      rename(iso3 = `dfB.match[, names.dfB]`) %>%
      select(-matches('gamma|posterior|cntry|cmatch_wb'))

    data <- basedata %>%
      left_join(newdata %>% mutate(merge = 'exact'), by = c('year' = yearvar, 'country' = countryvar)) %>%
      left_join(dfmatch %>% mutate(merge = 'fastlink'), by = c('year' = yearvar, 'iso3'), suffix = c('', '.fl')) %>%
      mutate(match = coalesce(merge, merge.fl),
             cname_used = ifelse(is.na(match), NA, ifelse(match == 'exact', country, ifelse(match == 'fastlink', cmatch.fl, NA)))) %>%
      mutate(across(all_of(newvars), ~ ifelse(match == merge & !is.na(merge), .x,
                                              ifelse(match == merge.fl & !is.na(merge.fl), get(paste0(cur_column(), '.fl')), NA)))) %>%
      select(-matches('\\.fl$|merge|cmatch'))
  }
}
