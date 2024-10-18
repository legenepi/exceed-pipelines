#' LoadSpirometryFVL class
#'
#' Load EXCEED spirometry data
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadSpirometryFVL class
#' @importFrom R6 R6Class
#' @export
LoadSpirometryFVL <- R6::R6Class(
  "LoadSpirometryFVL",
  inherit = exceedapi::Step,
  
  public = list(
    
    transform = function(.data, .collect, ...) {
      
      primarycare_data <- self$client$ehr("primarycare",
                                          snapshot = "2019-09-03"
      )
      
      testResult <- primarycare_data %>%
        tbl("TblTestresult") %>%
        mutate(pid=substring(vcendid,unlist(gregexpr('-',vcendid))[1]+1)) %>%
        mutate(pid=substring(pid,1,unlist(gregexpr('-',pid))-1)) %>%
        mutate(testid=as.numeric(substring(vcstartid,unlist(gregexpr('-',vcstartid))[1]+1))) %>%
        select(pid,
               testid,
               fvl=vcfvl) %>%
        collect()
      
      testResult <- arrange(testResult,pid,testid)
      
      fvl <- as.list(testResult$fvl)
      fvl <- gsub(" ", "", fvl)
      fvl <- gsub(",", ".", fvl)
      fvl <- strsplit(fvl,"|", fixed=TRUE)
      fvl <- purrr::map(fvl, type.convert, as.is = TRUE)

      return(fvl)
    }
  )
)

