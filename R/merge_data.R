#' Add new variables or merge external dataset
#' @param data data.frame for new variables to be merged onto.
#' @param newdata (Default = \code{NULL}) data.frame that contains new variables.
#' @param vars (Default = \code{NULL}) A vector with one or more variable names that should be merged onto \code{data}. Must be specified if \code{newdata} is \code{NULL}. Use \code{search_var()} to search for new variables.
#' @param id_site (Default = \code{NULL}) A site variable name included in \code{data} and \code{newdata}.
#' @param id_year (Optional, Default = \code{NULL}) A year variable name included in \code{data} and \code{newdata}. If \code{NULL}, assumes \code{newdata} is cross-sectional.
#' @param ... Arguments passed onto \code{fuzzymatch::stringdist_join()}.
#' @import fuzzyjoin
#' @import vdemdata
#' @import wbstats
#' @import devtools
#' @importFrom dplyr group_by_at slice_min
#' @return A dataframe with new variables merged.
#' @references Egami and Lee. (2023+). Designing Multi-Context Studies for External Validity: Site Selection via Synthetic Purposive Sampling. Available at \url{https://naokiegami.com/paper/sps.pdf}.
#' @export

merge_data <- function(data, vars = NULL, newdata = NULL, id_site = NULL, id_year = NULL, ...){

  if (is.null(newdata)){
    print('newdata is unspecified. Merging variables listed in vars onto data.')
    if (is.null(vars)){
      stop('Variable names must be specified in vars argument. Use search_var() to search for variables to be included.')
    }
    if (!is.null(vars)){
      for (i in vars){
        if (i %in% names(data)){
          print(paste('Variable', i, 'already exists and will not be merged.'))
        }
        else if (i %in% names(vdemdata::vdem)){
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
    }
  }
  if (!is.null(newdata)){
    if (is.null(id_site)){
      stop('id_site must be specified.')
    }
    if (!id_site %in% names(data)){
      stop('id_site does not exist in data.')
    }
    if (!id_site %in% names(newdata)){
      stop('id_site does not exist in newdata.')
    }
    if (is.null(id_year)){
      print('id_year unspecified. Assumes newdata is cross-sectional.')
    }
    if (!is.null(id_year)){
      if (!id_year %in% names(data)){
        stop('id_year variable does not exist in data.')
      }
      if (!id_year %in% names(newdata)){
        stop('id_year variable does not exist in newdata.')
      }
    }
    if (nrow(newdata[duplicated(newdata[,c(id_year, id_site)]),])>0){
      stop('newdata is not unique by id_site (and id_year). Check the data before merging.')
    }

    # Try exact matching first
    newvars <- names(newdata)[which(!names(newdata) %in% c(id_year, id_site))]
    base_unique <- data.frame(data[!duplicated(data[[id_site]]),id_site])
    names(base_unique) <- 'sitevar'
    newdata$sitevar <- newdata[[id_site]]

    fuzzy <- stringdist_join(base_unique,
                             newdata,
                             by = "sitevar",
                             distance_col = "distance",
                             mode = "inner",
                             max_dist = 20,
                             ...)

    fuzzy <- dplyr::group_by_at(fuzzy, c('sitevar.y', id_year))
    fuzzy <- dplyr::slice_min(fuzzy, order_by = .data[['distance']], n = 1, with_ties = FALSE)

    xcol <- c(id_year, id_site)
    if (is.null(id_year)) xcol <- id_site

    # iso3 code
    if (length(unique(nchar(fuzzy$sitevar.x)==3))==1 & TRUE %in% unique(nchar(fuzzy$sitevar.x)==3)){
      fuzzy <- fuzzy[fuzzy$distance == 0, ]
      }
    else if (max(fuzzy$distance)>0){
        warning(paste('\nFollowing sites are matched using fuzzyjoin:\n',
                      paste(paste(fuzzy$sitevar.y[fuzzy$distance>0], '=>', fuzzy$sitevar.x[fuzzy$distance>0]), collapse = '\n '),
                      '\nPlease review and if necessary, recode the site names in your dataset. For a country-level dataset, consider supplying ISO3 code instead.'))
      }

    fuzzy <- fuzzy[, c('sitevar.x', id_year, newvars)]
    names(fuzzy) <- c(id_site, id_year, newvars)
    data <- merge(data, fuzzy, by.x = xcol, by.y = xcol, all.x = TRUE, suffixes = c('', '.new'))
  }

  return(data)
}
