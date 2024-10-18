#' LoadSpirometryVTL class
#'
#' Load EXCEED spirometry data
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadSpirometryVTL class
#' @importFrom R6 R6Class
#' @export
LoadSpirometryVTL <- R6::R6Class(
  "LoadSpirometryVTL",
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
               vtl=vcvtl) %>%
        collect()
      
      testResult <- arrange(testResult,pid,testid)
      
      vtl <- as.list(testResult$vtl)
      vtl <- gsub(" ", "", vtl)
      vtl <- gsub(",", ".", vtl)
      vtl <- strsplit(vtl,"|", fixed=TRUE)
      vtl <- purrr::map(vtl, type.convert, as.is = TRUE)
      
      return(vtl)
    }
  )
)

