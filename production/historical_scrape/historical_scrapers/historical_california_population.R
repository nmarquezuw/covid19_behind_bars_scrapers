source("./R/generic_scraper.R")
source("./R/utilities.R")

get_all_historical_ca_pop_urls <- function(){
    urls_20 <- get_src_by_attr(
        "https://www.cdcr.ca.gov/research/weekly-total-population-report-archive-2020/", 
        "a", attr = "href", attr_regex = "(?i)Tpop1d.*.pdf")
    
    urls_21 <- get_src_by_attr(
        "https://www.cdcr.ca.gov/research/2021-weekly-total-population-reports/", 
        "a", attr = "href", attr_regex = "(?i)Tpop1d.*.pdf")

    c(urls_20, urls_21)
}

historical_ca_pop_pull <- function(x, date){
    date <- as.Date(date, format = "%Y-%m-%d")
    year4 <- format.Date(date, "%Y")
    year2 <- format.Date(date, "%y")
    month <- format.Date(date, "%m")
    day <- format.Date(date, "%d")

    stringr::str_c(
        "https://www.cdcr.ca.gov/research/wp-content/uploads/sites/174/", 
        format.Date(date, "%Y"), "/", month, "/Tpop1d", year2, month, day, ".pdf")
}

historical_ca_pop_restruct <- function(x, date = NULL){
    magick::image_read_pdf(x, pages = 2) %>% 
        ExtractTable()
}

historical_ca_pop_extract <- function(x, date = NULL){
    col_name_mat <- matrix(c(
        "Institutions", "X0", "Name", 
        "Felon/ Other", "X1", "Residents.Population", 
        "Design Capacity", "X2", "Capacity", 
        "Percent Occupied", "X3", "Percent.Occupied", 
        "Staffed Capacity", "X4", "Staffed.Capacity"
    ), ncol = 3, nrow = 5, byrow = TRUE)
    
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)
    
    df_ <- as.data.frame(x)
    
    check_names_extractable(df_, col_name_df)
    
    rename_extractable(df_, col_name_df) %>% 
        as_tibble() %>% 
        filter(!Name %in% c("Institutions", "Male Institutions", "Female Institutions")) %>% 
        filter(!str_detect(Name, "(?i)Total")) %>% 
        mutate_at(vars(-Name), string_to_clean_numeric) %>% 
        clean_scraped_df()
}

#' Scraper class for California population data 
#' 
#' @name historical_california_pop_scraper
#' @description 
#' \describe{
#'   \item{}{}
#' }

historical_ca_pop_scraper <- R6Class(
    "historical_california_pop_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.cdcr.ca.gov/research/weekly-total-population-report-archive-2020/",
            id = "historical_ca_pop",
            type = "pdf",
            state = "CA",
            jurisdiction = "state",
            pull_func = historical_ca_pop_pull,
            restruct_func = historical_ca_pop_restruct,
            extract_func = historical_ca_pop_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction  = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    historical_ca_pop_scraper <- historical_ca_pop_scraper$new(log=TRUE)
    historical_ca_pop_scraper$reset_date("SET_DATE_HERE")
    historical_ca_pop_scraper$raw_data
    historical_ca_pop_scraper$pull_raw(date = historical_ca_pop_scraper$date, .dated_pull = TRUE)
    historical_ca_pop_scraper$raw_data
    historical_ca_pop_scraper$save_raw()
    historical_ca_pop_scraper$restruct_raw(date = historical_ca_pop_scraper$date)
    historical_ca_pop_scraper$restruct_data
    historical_ca_pop_scraper$extract_from_raw(date = historical_ca_pop_scraper$date)
    historical_ca_pop_scraper$extract_data
    historical_ca_pop_scraper$validate_extract()
    historical_ca_pop_scraper$save_extract()
}
