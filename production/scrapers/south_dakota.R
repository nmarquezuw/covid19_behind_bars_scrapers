source("./R/generic_scraper.R")
source("./R/utilities.R")

south_dakota_pull <- function(x){
    get_src_by_attr(x, "a", attr = "href", attr_regex = "(?i)positivecase")
}

south_dakota_restruct <- function(x){
    sd_pgs <- magick::image_read_pdf(x)
    ExtractTable(img = sd_pgs)
}

south_dakota_extract <- function(x){

    headers <- sapply(x, function(z) stringr::str_c(z[1,], collapse = " "))

    rez_idx <- which(stringr::str_detect(headers, "(?i)offender"))
    staff_idx <- which(stringr::str_detect(headers, "(?i)staff"))

    sd_staff <- as.data.frame(x[staff_idx]) %>%
        .[2:nrow(.),]
    sd_res <- as.data.frame(x[rez_idx]) %>%
        .[2:nrow(.),]

    if(ncol(sd_staff) == 6){
        col_name_st <- matrix(c(
            "Facility", "X0", "Name",
            "Cumulative Positive", "X1", "Staff.Confirmed",
            "Negative", "X2", "Staff.Negative",
            "Recovered", "X3", "Staff.Recovered",
            "Active Positive", "X4", "Drop.Staff.Active",
            "Death", "X5", "Staff.Deaths"
        ), ncol = 3, nrow = 6, byrow = TRUE)
    
        col_name_res <- matrix(c(
            "Facility", "X0", "Name",
            "Cumulative Positive", "X1", "Residents.Confirmed",
            "Negative", "X2", "Residents.Negative",
            "Recovered", "X3", "Residents.Recovered",
            "Active Positive", "X4", "Residents.Active",
            "Death", "X5", "Residents.Deaths"
        ), ncol = 3, nrow = 6, byrow = TRUE)
    }

    else{
        col_name_st <- matrix(c(
            "Facility", "X0", "Name",
            "Total Positive", "X1", "Staff.Confirmed",
            "Negative", "X2", "Staff.Negative",
            "Recovered", "X3", "Staff.Recovered",
            "Death", "X4", "Staff.Deaths"
        ), ncol = 3, nrow = 5, byrow = TRUE)
        
        col_name_res <- matrix(c(
            "Facility", "X0", "Name",
            "Total Positive", "X1", "Residents.Confirmed",
            "Negative", "X2", "Residents.Negative",
            "Recovered", "X3", "Residents.Recovered",
            "Death", "X4", "Residents.Deaths"
        ), ncol = 3, nrow = 5, byrow = TRUE)
    }

    colnames(col_name_st) <- c("check", "raw", "clean")
    col_name_st_df <- as_tibble(col_name_st)

    colnames(col_name_res) <- c("check", "raw", "clean")
    col_name_res_df <- as_tibble(col_name_res)
    
    check_names_extractable(sd_staff, col_name_st_df)
    check_names_extractable(sd_res, col_name_res_df)

    sd_df <- rename_extractable(sd_staff, col_name_st_df) %>%
        as_tibble() %>%
        filter(!str_detect(Name, "(?i)facility|total")) %>%
        full_join(
            rename_extractable(sd_res, col_name_res_df) %>%
                as_tibble() %>%
                filter(!str_detect(Name, "(?i)facility|total")),
            by = "Name") %>%
        select(-starts_with("Drop")) %>%
        clean_scraped_df()

    # legacy code
    sd_df$Name[sd_df$Name=="Jameson Annex"] <- 
        "Jameson Prison Annex Sioux Falls"
    sd_df$Name[sd_df$Name=="Mike Durfee State Prison"] <- 
        "Mike Durfee State Prison Springfield"
    sd_df$Name[sd_df$Name=="South Dakota Women's Prison"] <- 
        "Womens Prison Pierre"
    sd_df$Name[sd_df$Name=="South Dakota Women's Prison Unit E"] <- 
        "Unit E Pierre"

    sd_df
}

#' Scraper class for general South Dakota COVID data
#' 
#' @name south_dakota_scraper
#' @description Data is pulled from a pdf on the page whose link is
#' occasionally updated. Within the pdf there are two tables with data.
#' One for staff and another for residents.
#' \describe{
#'   \item{Facility}{The facility name.}
#'   \item{Staff Positive}{}
#'   \item{Staff Negative}{}
#'   \item{Staff Recovered}{}
#'   \item{Staff Death}{}
#'   \item{Resident Positive}{}
#'   \item{Resident Negative}{}
#'   \item{Resient Recovered}{}
#'   \item{Resident Death}{}
#' }

south_dakota_scraper <- R6Class(
    "south_dakota_scraper",
    inherit = generic_scraper,
    public = list(
        log = NULL,
        initialize = function(
            log,
            url = "https://doc.sd.gov/about/Coronavirus.aspx",
            id = "south_dakota",
            type = "pdf",
            state = "SD",
            jurisdiction = "state",
            # pull the JSON data directly from the API
            pull_func = south_dakota_pull,
            # restructuring the data means pulling out the data portion of the json
            restruct_func = south_dakota_restruct,
            # Rename the columns to appropriate database names
            extract_func = south_dakota_extract){
            super$initialize(
                url = url, id = id, pull_func = pull_func, type = type,
                restruct_func = restruct_func, extract_func = extract_func,
                log = log, state = state, jurisdiction = jurisdiction)
        }
    )
)

if(sys.nframe() == 0){
    south_dakota <- south_dakota_scraper$new(log=TRUE)
    south_dakota$raw_data
    south_dakota$pull_raw()
    south_dakota$raw_data
    south_dakota$save_raw()
    south_dakota$restruct_raw()
    south_dakota$restruct_data
    south_dakota$extract_from_raw()
    south_dakota$extract_data
    south_dakota$validate_extract()
    south_dakota$save_extract()
}

