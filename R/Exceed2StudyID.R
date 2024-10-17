#' Exceed2StudyID class
#'
#' List the data frame that maps EXCEED ID to Study ID (LHIS primary care data pseudo ID)
#' Withdrawn EXCEED participants are not included from the output data
#'
#' @docType class
#' @format An R6 class object.
#' @description Exceed2StudyID class
#' @importFrom R6 R6Class
#' @export
Exceed2StudyID <- R6::R6Class(
  "Exceed2StudyID",
  inherit = exceedapi::Step,
  
  public = list(
    
    transform = function(.data, .collect, ...) {
      
      profiles_data <- self$client$profiles() %>% 
        filter(basicconsent__withdrawals__scope != "3" | 
                 is.na(basicconsent__withdrawals__scope)) %>% 
        select("exceed_id") %>%
        collect() %>% 
        unique()
      
      lhis_ids <- self$client$identities(domain="lhis") %>%
        select(uuid, study_id = pid) %>%
        collect()
      
      exceed_ids <- self$client$identities(domain = "*") %>%
        filter(str_detect(domain, "^exceed")) %>%
        select(uuid, exceed_id = pid)  %>%
        mutate(exceed_id = as.character(exceed_id),
               exceed_id = str_pad(exceed_id, 6, "left", "0")) %>%
        filter(exceed_id %in% profiles_data$exceed_id)%>%
        collect()
      
      exceed_ids <- lhis_ids %>%
        left_join(exceed_ids, by='uuid') %>%
        select(-uuid)
      
      return(exceed_ids)
    }
  )
)

