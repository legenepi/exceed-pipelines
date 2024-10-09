#' LoadSpirometry class
#'
#' Load EXCEED spirometry data
#'
#' @docType class
#' @format An R6 class object.
#' @description LoadSpirometry class
#' @importFrom R6 R6Class
#' @export
LoadSpirometry <- R6::R6Class(
  "LoadSpirometry",
  inherit = exceedapi::Step,

  public = list(
    testResult = NULL,
    only_good_blow = T,
    initialize = function(only_good_blow = T) {
      self$only_good_blow <- only_good_blow
      self$testResult <- NA
    },

    transform = function(.data, .collect, ...) {

      primarycare_data <- self$client$ehr("primarycare",
        snapshot = "2019-09-03"
      )

      tbl_testresult <- primarycare_data %>%
        tbl("TblTestresult") %>%
        collect()

      # Collect spirometry data
      self$testResult <- tbl_testresult %>%
        mutate(pid=substring(vcendid,unlist(gregexpr('-',vcendid))[1]+1)) %>%
        mutate(pid=substring(pid,1,unlist(gregexpr('-',pid))-1)) %>%
        mutate(timeid=substring(vcendid,unlist(gregexpr('-',vcendid))[1]+1)) %>%
        mutate(timeid=substring(timeid,unlist(gregexpr('-',timeid))+1)) %>%
        mutate(testid=as.numeric(substring(vcstartid,unlist(gregexpr('-',vcstartid))[1]+1))) %>%
        select(pid,
               testid,
               timeid,
               fev75=intfev75,
               fev1=intfev1,
               fev3=intfev3,
               fev6=intfev6,
               fvc=intfvc,
               pef=intpef,
               mef75=intmef75,
               mef50=intmef50,
               mef25=intmef25,
               mmef=intmmef,
               met=intmet,
               fet=intfet,
               erv=interv,
               irv=intirv,
               ic=intic,
               frc=intfrc,
               warning=intwarning,
               fvl_count=intfvlcount,
               vtl_count=intvtlcount,
               fvl_xscale=intfvlxscale,
               fvl_yscale=intfvlyscale,
               vtl_xscale=intvtlxscale,
               vtl_yscale=intvtlyscale,
               vtl=vcvtl,
               fvl=vcfvl)

      arrange(self$testResult,pid,testid)
    }
  )
)

