library(R6)

#' The DOcplexcloud client class.
#'
#' @docType class
#' @format The DOcplexcloud client class.
#' @importFrom R6 R6Class
#' @field url The DOcplexcloud url. If not specified, the value of environment
#'      variable DOCPLEXCLOUD_URL is used.
#' @field key The DOcplexcloud api key. If not specified, the value of environment
#'      variable DOCPLEXCLOUD_KEY is used
#' @field verbose If TRUE, sets the verbose mode.
#' @export
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(url, key, verbose)}}{Initialize a new client.}
#'   \item{\code{submitJob(...)}}{Submits a job execution.}
#'   \item{\code{getAllJobs(...)}}{Returns all jobs for the current user.}
#'   \item{\code{deleteAllJobs(...)}}{Delete all jobs for the current user.}
#' }
#'
#' @examples
#' \dontrun{
#' # create a new client, using environment variables for url and api key
#' client <- DOcplexCloudClient$new()
#'
#' # create and execute a job to solve model.lp
#' job <- client$submitJob(addAttachment(file="model.lp"))
#'
#' # create and execute a job which model is stored
#' # in memory
#' model <- "Minimize
#'             obj: x + y
#'           Subject To
#'           Bounds
#'             3 <= x <= 17
#'             2 <= y
#'           End"
#' job <- client$submitJob(addAttachment(name="model.lp",
#'                                       data=charToRaw(model)))
#' }
DOcplexcloudClient <- R6Class("DOcplexcloudClient",
    public = list(
        url = NULL,
        key = NULL,
        verbose = NULL,
        initialize = function(url=NULL, key=NULL, verbose=FALSE) {
            "Create a new client."
            if (is.null(url))
                url = Sys.getenv("DOCPLEXCLOUD_URL")
            if (is.null(key))
                key = Sys.getenv("DOCPLEXCLOUD_KEY")
            self$url <- url
            self$key <- key
            self$verbose <- verbose
        },
        submitJob = function(...) {
            "Submits a job for execution."
            dots <- list(...)
            attachments <- Filter(function(d) { inherits(d, "DOcplexcloudAttachment")}, dots)

            job <- DOcplexcloudJob(client=self)
            job$attachments <- attachments
            job <- create(job)
            job <- execute(job)
            return (job)
        },
        makeRequest = function(verb, url, fail_message, ...) {
            response = RETRY(verb,
                             url = url,
                             add_headers("X-IBM-Client-Id" = self$key),
                             ...)
            stop_for_status(response, paste(fail_message, url, sep=" "))
            return(response)
        },
        getAllJobs = function(...) {
            "Returns all jobs for the current user."
            url <- paste(self$url, "/jobs", sep="")
            response = self$makeRequest("GET",
                                   url = url,
                                   content_type_json(),
                                   ...)
            return(content(response))
        },
        deleteAllJobs = function(...) {
            "Delete all jobs for the current user."
            url <- paste(self$url, "/jobs", sep="")
            response = self$makeRequest("DELETE",
                                   url = url,
                                   content_type_json(),
                                   ...)
            return(response)
        }
    )
)

