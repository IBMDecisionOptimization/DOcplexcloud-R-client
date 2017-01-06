library(R6)

#' Returns the joburl if a DOcplexcloudJob is passed. Otherwise return the arg.
#'
#' @keywords internal
getJobUrl <- function(job_or_url) {
    if (inherits(d, "DOcplexcloudJob")) {
        return(job_or_url$joburl)
    } else {
        return(job_or_url)
    }
}

#' The DOcplexcloud client class.
#' 
#' The client makes it easier to work with the
#' solve service by encapsulating connection parameters (connection URL, api key)
#' and provides support for features of DOcplexcloud solve service not tied to a
#' particular job, like listing
#' all jobs for the current user or deleting all jobs.
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
#' @import httr
#' @import rjson
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(url, key, verbose)}}{Initialize a new client.}
#'   \item{\code{submitJob(..., wait=TRUE)}}{Submits a job execution.
#' 
#'       This method creates the job, upoads the attachments, submit an execution
#'       request, then wait for completion of the job, unless \code{wait} is
#'       \code{FALSE}.
#'
#'       Returns a \code{DOcplexcloudJob}.
#'   }
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
        #
        # Internal methods
        #
        makeRequest = function(verb, url, fail_message, ...) {
            "common method to all request methods"         
            response = RETRY(verb,
                             url = url,
                             add_headers("X-IBM-Client-Id" = self$key),
                             ...)
            stop_for_status(response, paste(fail_message, url, sep=" "))
            return(response)
        },
        #
        # direct REST api mapping methods
        #
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
        },
        abortJob = function(joburl, ...) {
            "Abords the specified job"
            url <- paste(joburl, "/execute", sep="")
            response = self$makeRequest("DELETE",
                                        url = url,
                                        fail_message = "abort job",
                                        content_type_json(),
                                        ...)
            return(response)
        },
        deleteJob = function(joburl, ...) {
            "Deletes the specified job"
            response = self$makeRequest("DELETE",
                                        url = joburl,
                                        fail_message = "delete job",
                                        content_type_json(),
                                        ...)
            return(response)
        },
        executeJob = function(joburl, ...) {
            "Submits an execute request on a job"
            url = paste(joburl, "/execute", sep="")

            response = self$makeRequest("POST",
                                        url = url,
                                        fail_message = "execute job",
                                        content_type_json(),
                                        ...)
            return(response)
        },
        getJobStatus = function(joburl, ...) {
            url = paste(joburl, "/execute", sep="")
            response = self$makeRequest("GET",
                                        url = url,
                                        fail_message = "get status",
                                        content_type_json(),
                                        ...)
            return(content(response)$executionStatus)
        },
        getJobLogs = function(joburl, ...) {
            "Download the logs for a job"
            log_url <- paste(joburl, "/log/blob", sep="")
            response <- self$makeRequest("GET",
                                         url = log_url,
                                         fail_message = "get logs",
                                         content_type("application/text"),
                                         ...)
            return(rawToChar(content(response)))
        },
        waitForCompletion = function(joburl, waittime=Inf, ...) {
            if(self$verbose) {
                cat("Waiting for completion\n")
            }
            elapsed <- 0
            sleep_duration <- 2
            repeat {
                st <- self$getJobStatus(joburl)
                if (st == "PROCESSED" || st == "FAILED" || st == "INTERRUPTED") {
                    return (st)
                }
                Sys.sleep(sleep_duration)
                elapsed <- elapsed + sleep_duration
                if (elapsed > waittime) {
                    # returns the last know status
                    return (st)
                }
            }
        },
        createJob = function(attachments,...) {
            "Creates a job"
            if(self$verbose) {
                cat("Creating job\n")
            }
            url <- paste(self$url, "/jobs", sep="")
            att_names <- Map(function(a) a$getName(),
                             attachments)
            body <- list(attachments=att_names)
            response <- self$makeRequest("POST",
                                        url = url,
                                        fail_message = "create job on url",
                                        content_type_json(),
                                        ...,
                                        body = toJSON(body))
            stop_for_status(response, paste('could not create job', url))
            joburl <- headers(response)$location
            e <- strsplit(joburl, "/", fixed=TRUE)[[1]]
            jobid <- e[[length(e)]]
            
            # now upload attachments
            for (a in attachments)
            {
                self$uploadAttachment(joburl, attachment=a)
            }
            return(joburl)
        },
        uploadAttachment = function(joburl, attachment, ...) {
            name <- attachment$getName()
            att_url <- paste(joburl, "/attachments/", name, "/blob", sep="")
            if (!is.null(attachment$file) && !(attachment$file == "")) {
                att_data <- charToRaw(readChar(attachment$file, file.info(attachment$file)$size))
            } else {
                att_data <- attachment$data
            }
            if (!is.raw(att_data)) {
              att_data <- as.raw(att_data)
            }
            if (self$verbose) {
                cat("Uploading attachment", name, "\n")
            }  
            response <- self$makeRequest("PUT",
                                     url = att_url,
                                     fail_message = "upload attachment to url",
                                     content_type("application/octet-stream"),
                                     ...,
                                     body=att_data)
            return(response)
        },
        getAttachment = function(joburl, name, convert=TRUE, ...) {
            att_url <- paste(joburl, "/attachments/", name, "/blob", sep="")
            response <- self$makeRequest("GET",
                                  url = att_url,
                                  fail_message = "get attachment",
                                  content_type("application/octet-stream"),
                                  ...)
            if (convert && endsWith(tolower(name), ".json")) {
                return(fromJSON(rawToChar(content(response))))
            } else {
                return(content(response))
            }
        },
        getJobInfo = function(joburl, ...) {
            response <- self$makeRequest("GET",
                                         url = joburl,
                                         fail_message = "get info",
                                         content_type_json(),
                                         ...)
            return(content(response))
        },
        copyJob = function(joburl, shallow=FALSE, override_data="", ...) {
            shallow_value = ifelse(shallow, "true", "false")
            shallow_option = ifelse(shallow, paste("?shallow=", shallow_value, sep=""),
                                    "")
            
            url <- paste(joburl, "/copy", shallow_option, sep="")

            if (self$verbose) {
                print(paste("copying job, url =", url))
            }
            
            response = self$makeRequest("POST",
                                        url = url,
                                        fail_message = "copy job",
                                        content_type_json(),
                                        body = override_data,
                                        ...)

            copy_url =  headers(response)$location
            return(copy_url)
        },
        #
        # Utility methods
        #
        submitJob = function(..., wait = TRUE) {
            "Submits a job for execution and wait for completion"
            dots <- list(...)
            attachments <- Filter(function(d) { inherits(d, "DOcplexcloudAttachment")}, dots)

            job <- DOcplexcloudJob$new()
            job$joburl <- self$createJob(attachments=attachments)
            response <- self$executeJob(job$joburl)
            if (wait) {
                status <- self$waitForCompletion(job$joburl)
                job$executionStatus <- status
                if (self$verbose) {
                    cat("execution status =", job$executionStatus, "\n")
                }
            }
            return (job)
        }
    )
)

