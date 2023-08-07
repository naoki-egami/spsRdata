#' Create a world map with target population and selected countries from SPS indicated.
#' @param data Data frame Default is sps_data.
#' @param targetvar Name of the binary indicator variable in \code{data} that indicates target population. Default is NULL, which treats all countries as target population.
#' @param countryvar Name of the country variable in \code{data}.
#' @param countrysps A character vector including country names selected from SPS. Values must come from the variable selected in \code{countryvar}.
#' @import maps
#' @import dplyr
#' @import ggplot2
#' @importFrom countrycode countrycode
#' @return A dataframe with new variables merged.
#' @references Egami and Lee. (2023+). Designing Multi-Context Studies for External Validity: Site Selection via Synthetic Purposive Sampling. Available at \url{https://naokiegami.com/paper/sps.pdf}.
#' @export

sps_map <- function(data, targetvar = NULL, countryvar = NULL, countrysps = NULL){
  # World map data
  world_map <- map_data("world") %>% mutate(region = ifelse(region %in% c('Trinidad', 'Tobago'), 'Trinidad and Tobago', region))
  world_map$iso3 <- countrycode::countrycode(world_map$region, "country.name", "iso3c",
                                custom_match = c('Micronesia'     = 'FSM',
                                                 'Virgin Islands' = 'VIR',
                                                 'Saint Martin'   = 'MAF',
                                                 'Hong Kong'  = 'HKG',
                                                 'Gaza Strip' = 'PSE',
                                                 'Somaliland' = 'SML',
                                                 'Zanzibar'   = 'ZZB',
                                                 'Kosovo'     = 'XKX'))

  # Merge region information
  data$target <- ifelse(data[[countryvar]] %in% countrysps, 'Selected Countries', 'Target Population')
  if (!is.null(targetvar)) data$target <- ifelse(data[[targetvar]] == 0, 'Target Population', data$target)

  world_map <- left_join(world_map, distinct(data %>% select(target, iso3, matches(paste0('^',countryvar,'$')))), by = 'iso3')
  world_map$target <- ifelse(is.na(world_map$target) & !is.null(targetvar), 'Rest of the World',
                             ifelse(is.na(world_map$target) & is.null(targetvar), 'Target Population', world_map$target))

  if (is.null(targetvar)){
    world_map$target <- factor(world_map$target, levels = c('Selected Countries', 'Target Population'))
    col <- c('red4', 'chartreuse4')
  }
  if (!is.null(targetvar)){
    world_map$target <- factor(world_map$target, levels = c('Selected Countries', 'Target Population', 'Rest of the World'))
    col <- c('red4', 'chartreuse4', 'gray')
  }

  gmap <- ggplot(data = world_map, aes(map_id = region)) +
    geom_map(data = world_map,
             map  = world_map,
             aes(fill = target)) +
    scale_fill_manual(na.value = 'white', values = col) +
    xlab('') + ylab('') +
    expand_limits(x = world_map$long, y = world_map$lat) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = 'bottom',
          axis.text = element_blank(),
          axis.ticks = element_blank())
  return(gmap)
}
