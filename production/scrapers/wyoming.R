source("./R/generic_scraper.R")
source("./R/utilities.R")

wyoming_pull <- function(x){
    "1VhAAbzipvheVRG0UWKMLT6mCVQRMdV98lUUkk-PCYtQ" %>%
        googlesheets4::read_sheet(sheet = "WY", 
                                  col_types = "Dccc")
}

wyoming_restruct <- function(x){
    x %>%
        filter(!is.na(Date)) %>% 
        filter(Date == max(Date))
}

wyoming_extract <- function(x, exp_date = Sys.Date()){
    
    error_on_date(first(x$Date), exp_date)
    
    check_names(x, c(
        "Date", 
        "Name", 
        "Positive Inmate Cases", 
        "Inmate Deaths")
    )
    
    x %>%
        select(
            Name = `Name`,
            Residents.Active = `Positive Inmate Cases`,
            Residents.Deaths = `Inmate Deaths`) %>% 
        clean_scraped_df()
}

#' Scraper class for Wyoming data 
#' 
#' @name wyoming_scraper
#' @description Grabs manually entered Google Sheet data for Wyoming. Only active 
#' cases and deaths are reported. 
#' 
#' \describe{
#'   \item{Positive inmate cases}{Active cases among residents}
#'   \item{Inmate deaths}{Cumulative COVID-19 deaths}
#' }

wyoming_scraper <- R6Class(
    "wyoming_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "http://corrections.wyo.gov/",
            id = "wyoming",
            type = "manual",
            state = "WY",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = wyoming_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = wyoming_restruct,
            # Rename the columns to appropriate database names
            extract_func = wyoming_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    wyoming <- wyoming_scraper$new(log=TRUE)
    wyoming$raw_data
    wyoming$pull_raw()
    wyoming$raw_data
    wyoming$save_raw()
    wyoming$restruct_raw()
    wyoming$restruct_data
    wyoming$extract_from_raw()
    wyoming$extract_data
    wyoming$validate_extract()
    wyoming$save_extract()
}
