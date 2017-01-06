library(R6)

#' The job response class. This class is returned by client methods, storing
#' job information.
#' 
#' @export
DOcplexcloudJobResponse <- R6Class("DOcplexcloudJobResponse",
    public = list (
        joburl = NULL,
        executionStatus = NULL,
        initialize = function() {
            self$joburl <- NULL
            self$executionStatus <- NULL
        }
    )
)

#' The job class. This class provides the API to manage operations on jobs.
#'
#' @field client The \code{DOcplexcloudClient}.
#' @field joburl The url of the job.
#' @field jobid The id of the job.
#' @field exectutionStatus The execution status if the job has been executed.
#' @field attachments The attachments
#' @note You normally don't use this to actually create a job, but
#'   instead you would create a new job from a \code{DOcplexcloudClient}
#'
#' @examples
#' \dontrun{
#' client <- DOcplexcloudClient(url='Your DOcplexcloud base URL',
#'                              key='Your DOcplexcloud api key')
#'
#' # create job
#' job <- submitJob(client, addAttachment(file="model.lp"))
#' status <- waitForCompletion(job)
#' }
#' @export
DOcplexcloudJob <- function(client = NULL, joburl = NULL, jobid = NULL,
                            executionStatus = NULL,
                            attachments = list()) {
    me <- list(client = client,
               joburl = joburl,
               jobid = jobid,
               executionStatus = executionStatus,
               attachments = attachments)
    class(me) <- append(class(me), "DOcplexcloudJob")
    return(me)
}

