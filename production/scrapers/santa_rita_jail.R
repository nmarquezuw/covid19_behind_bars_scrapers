source("./R/generic_scraper.R")
source("./R/utilities.R")

santa_rita_jail_pull <- function(x){
    "196jMpPfuE4IMlplsd7K_3mP1l018cIbS-oTO2SuVklw" %>%
        googlesheets4::read_sheet()
}

santa_rita_jail_restruct <- function(x){
    x %>%
        filter(!is.na(Date)) %>%
        mutate(Date = lubridate::ymd(Date)) %>%
        filter(Date == max(Date))
}

santa_rita_jail_extract <- function(x, exp_date = Sys.Date()){
    
    error_on_date(x$Date, exp_date)
    
    check_names(x, c(
        "Date", 
        "SRJ Population (total)", 
        "SRJ Population (diff)", 
        "Tests (incarcerated population, total)", 
        "Tests (Incarcerated population, difference)", 
        "Pending tests", 
        "Percentage of population tested within the past: 7 days", 
        "Percentage of population tested within the past: 14 days", 
        "Percentage of population tested within the past: 30 days", 
        "Incarcerated population cases (total)", 
        'Incarcerated population cases ("active")',
        "1-day change in 'active' cases",
        "Incarcerated population hospitalizations (total)",
        "Staff cases (total)",
        "1-day change in staff cases",
        "Red patients (current)",
        "Dark Red patients (current)",
        "Orange patients (current)",
        "1-day change in Orange patients",
        "Percent of Orange patients in population",
        "Total Resolved Cases",
        "Released while Active",
        "Percentage of total cases released while active",
        "Released after Resolved",
        "Percentage of total cases released after resolved",
        "Resolved in Custody",
        "Percentage of total cases resolved in custody",
        "Deaths"))
    
    x %>%
        select(
            Residents.Confirmed = `Incarcerated population cases (total)`,
            Residents.Active = `Incarcerated population cases ("active")`,
            Residents.Recovered = `Total Resolved Cases`,
            Residents.Deaths = Deaths,
            Residents.Tadmin = `Tests (incarcerated population, total)`,
            Residents.Pending = `Pending tests`,
            Residents.Population = `SRJ Population (total)`,
            Staff.Confirmed = `Staff cases (total)`
            ) %>%
        mutate(Name = "SANTA RITA JAIL")
}

#' Scraper class for general santa_rita_jail COVID data
#' 
#' @name santa_rita_jail_scraper
#' @description This will be a description of santa_rita_jail data and what the scraper
#' does
#' \describe{
#'   \item{Facility_Name}{The faciilty name.}
#' }

santa_rita_jail_scraper <- R6Class(
    "santa_rita_jail_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://www.davisvanguard.org/tag/covid-19/",
            id = "santa_rita_jail",
            type = "csv",
            state = "CA",
            jurisdiction = "county",
            # pull the JSON data directly from the API
            pull_func = santa_rita_jail_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = santa_rita_jail_restruct,
            # Rename the columns to appropriate database names
            extract_func = santa_rita_jail_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    santa_rita_jail <- santa_rita_jail_scraper$new(log=TRUE)
    santa_rita_jail$raw_data
    santa_rita_jail$pull_raw()
    santa_rita_jail$raw_data
    santa_rita_jail$save_raw()
    santa_rita_jail$restruct_raw()
    santa_rita_jail$restruct_data
    santa_rita_jail$extract_from_raw()
    santa_rita_jail$extract_data
    santa_rita_jail$validate_extract()
    santa_rita_jail$save_extract()
}

