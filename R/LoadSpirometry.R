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

    #' transform the data by zero-padding the exceed_id
    transform = function(only_good_blow, fvl50, vtl50, ...) {

      only_good_blow <- ifelse(is.null(self$only_good_blow), T, self$snapshot)
      fvl50 <- ifelse(is.null(self$fvl50), F, self$snapshot)
      vtl50 <- ifelse(is.null(self$vtl50), F, self$snapshot)

      primarycare_data <- self$client$ehr(
        "primarycare",
        snapshot = "2019-09-03"
      )

      # tbl_exam <- primarycare_data %>%
      #   tbl("TblExamination") %>%
      #   .collect()
      #
      # tbl_logging <- primarycare_data %>%
      #   tbl("TblLogging") %>%
      #   .collect()
      #
      # tbl_patient <- primarycare_data %>%
      #   tbl("TblPatient") %>%
      #   .collect()

      tbl_testresult <- primarycare_data %>%
        tbl("TblTestresult") %>%
        .collect()

      # tbl_user <- primarycare_data %>%
      #   tbl("TblUser") %>%
      #   .collect()

      # # Collect patient data
      # pat <- tbl_patient %>%
      #   mutate(pid=substring(vcpatientid,unlist(gregexpr('-',vcpatientid))[1]+1)) %>%
      #   select(pid,
      #          firstname=vcfirstname,
      #          lastname=vclastname,
      #          dob=vcdateofbirth,
      #          sex=intsex,
      #          height=intheight,
      #          weight=intweight,
      #          race=intrace) %>%
      #   unique()

      # # Collect examination data
      # exam <- tbl_exam %>%
      #   mutate(pid=substring(vcendid,unlist(gregexpr('-',vcendid))[1]+1)) %>%
      #   select(pid,testid=inttest1,startid=vcstartid)
      #
      # exam <- arrange(exam,pid,testid)

      # Collect spirometry data
      tresult <- tbl_testresult %>%
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

      #apply filters
      if(only_good_blow==T) {
        idx=tresult$warning==7
        tresult=tresult[which(idx),]
      }

      if(fvl50==T) {
        idx=tresult$fvl_count>50
        tresult=tresult[which(idx),]
      }

      if(vtl50==T) {
        idx=tresult$vtl_count>50
        tresult=tresult[which(idx),]
      }
      arrange(tresult,pid,testid)

      # tresult <- arrange(tresult,pid,testid)
      # fvl=c()
      # vtl=c()
      # for (x in 1:nrow(tresult)) {
      #   fvl[[x]] <- unlist(strsplit(tresult$fvl[x],"|", fixed=TRUE))
      #   fvl[[x]] <- sub(" ", "", fvl[[x]])
      #   fvl[[x]] <- sub(", ", ".", fvl[[x]])
      #   fvl[[x]] <- sub(". ", ".", fvl[[x]])
      #   fvl[[x]] <- as.double(fvl[[x]])
      #   vtl[[x]] <- unlist(strsplit(tresult$vtl[x],"|", fixed=TRUE))
      #   vtl[[x]] <- sub(" ", "", vtl[[x]])
      #   vtl[[x]] <- sub(", ", ".", vtl[[x]])
      #   vtl[[x]] <- sub(". ", ".", vtl[[x]])
      #   vtl[[x]] <- as.double(vtl[[x]])
      # }
      # tresult %>% select(-c(fvl ,vtl))
    }

  )
)

