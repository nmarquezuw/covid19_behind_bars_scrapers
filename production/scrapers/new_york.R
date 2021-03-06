source("./R/generic_scraper.R")
source("./R/utilities.R")

new_york_pull <- function(x){
    get_src_by_attr(
        x, "a", attr = "href", attr_regex = "(?i)facility-")
}

new_york_restruct <- function(x){
    ny_pgs <- magick::image_read_pdf(x)
    ExtractTable(ny_pgs)
}

new_york_extract <- function(x){
    bad_names <- c("", "TOTAL", "NYS DOCCS", "NYS DOCCS INCARCERATED")
    
    ny <- as.data.frame(x) %>%
        filter(!(X0 %in% bad_names))
    
    col_name_mat <- matrix(c(
            "REPORTED FACILITY", "X0", "Name",
            "RECOVERED", "X1", "Residents.Recovered",
            "DECEASED", "X2", "Residents.Deaths",
            "TOTAL POSITIVE", "X3", "Residents.Confirmed",
            "PENDING", "X4", "Residents.Pending",
            "NEGATIVE", "X5", "Residents.Negative"
            ), ncol = 3, nrow = 6, byrow = TRUE)
        
    colnames(col_name_mat) <- c("check", "raw", "clean")
    col_name_df <- as_tibble(col_name_mat)

    check_names_extractable(ny, col_name_df)
  
    rename_extractable(ny, col_name_df) %>%
        filter(!str_detect(Name, "(?i)FACILITY")) %>%
        filter(Name != "") %>%
        filter(!str_detect(Name, "(?i)total")) %>%
        clean_scraped_df() %>%
        as_tibble() %>%
        mutate(Name = ifelse(
            grepl(Name, pattern = "MOHAWK"), "MOHAWK WALSH RMU", Name)) %>%
        mutate(Residents.Tadmin = Residents.Negative +
                 Residents.Pending + Residents.Confirmed)
}

#' Scraper class for general new_york COVID data
#' 
#' @name new_york_scraper
#' @description NY facility level data is only provided for residents and not
#' staff. Pdf has been consistent but data used to be directly embedded in the
#' html.
#' \describe{
#'   \item{Facility}{The facility name}
#'   \item{Recovered}{Residents recovered from infection}
#'   \item{Deceased}{Residents deaths related to covid}
#'   \item{Positive}{Residents Confirmed}
#'   \item{Pending}{Residents Pending}
#'   \item{Negative}{Residents Negative}
#' }

new_york_scraper <- R6Class(
    "new_york_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doccs.ny.gov/doccs-covid-19-report",
            id = "new_york",
            type = "pdf",
            state = "NY",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = new_york_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = new_york_restruct,
            # Rename the columns to appropriate database names
            extract_func = new_york_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    new_york <- new_york_scraper$new(log=TRUE)
    new_york$raw_data
    new_york$pull_raw()
    new_york$raw_data
    new_york$save_raw()
    new_york$restruct_raw()
    new_york$restruct_data
    new_york$extract_from_raw()
    new_york$extract_data
    new_york$validate_extract()
    new_york$save_extract()
}

