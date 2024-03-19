#' Obtain OSA Data (webscrape)
#'
#' @param eid EID Code (see `eidcodes`)
#' @param years vector of years (2012 to 2021 available)
#'
#' @return tibble of requested financial data
#' @export
#'
get_osadata <- function(eid, years = 2012:2021) {

  base_url = "https://www.osa.state.mn.us/reports-data-analysis/data/city-comparison-tool/?eid1=%s&yr1=%s&eid2=&yr2="
  osadata <- years %>%
    purrr::map(~sprintf(base_url, eid, .x)) %>%
    purrr::map(rvest::read_html) %>%
    purrr::map(rvest::html_elements, "#data") %>%
    purrr::map_dfr(rvest::html_table, na.strings = c("N/A", ""), .id = "id") %>%
    dplyr::mutate(calcyear = years[as.integer(id)]) %>%
    dplyr::select(Year = calcyear,
           Budget.Item = X1,
           Amount = X2,
           Per.Capita = X3,
           Rank = X4) %>%
    dplyr::group_by(Year) %>%
    dplyr::mutate(Budget.Item = if_else(row_number() %in% c(41:51), paste(Budget.Item, "Capital Outlay"), Budget.Item)) %>%
    dplyr::slice(-c(1, 2, 6, 14, 20, 25, 27, 40, 53, 55)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = Budget.Item, values_from = c(Amount, Per.Capita, Rank), names_repair = "universal") %>%
    dplyr::mutate(across(-Year & !contains("Rank_"), readr::parse_number))

  return(osadata)
}
