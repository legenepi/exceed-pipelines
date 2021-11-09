ZeroPadding <- R6::R6Class(
  "ZeroPadding", 
  inherit = Step,

  public = list(
    
    #' transform the data by zero-padding the exceed_id
    transform = function(.data, ...) {
      mutate(.data, exceed_id = str_pad(exceed_id, 6, pad = 0))
    }
    
  )
)

