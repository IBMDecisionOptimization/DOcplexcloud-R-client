library(R6)

#' Returns the joburl if a DOcplexcloudJob is passed. Otherwise return the arg.
#'
#' @keywords internal
getJobUrl <- function(job_or_url) {
    if (inherits(job_or_url, "DOcplexcloudJob")) {
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
#' @field url The DOcplexcloud URL.
#' @field key The DOcplexcloud api key.
#' @field verbose If TRUE, sets the verbose mode.
#' @export
#' @import httr
#' @import rjson
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(url, key, verbose=FALSE)}}{Initialize a new client with
#'       the specified \code{url} and \code{key} parameters.
#'
#'       If \code{url} is not specified, the value of environment variable
#'       \code{DOCPLEXCLOUD_URL} is used. If \code{key} is not specified, the
#'       value of environment variable \code{key} is used.
#'
#'       When \code{verbose} is TRUE, extra information is printed when the
#'       client API is called.
#'   }
#'   \item{\code{submitJob(..., wait=TRUE)}}{Submits a job execution.
#' 
#'       This method creates the job, upoads the attachments, submit an execution
#'       request, then wait for completion of the job, unless \code{wait} is
#'       \code{FALSE}.
#'
#'       \code{...} Extra parameters passed to \code{httr}
#'
#'       Returns a \code{DOcplexcloudJob}.
#'   }
#'   \item{\code{getAllJobs(...)}}{Returns all jobs for the current user.
#'
#'       \code{...} Extra parameters passed to \code{httr}
#'    }
#'   \item{\code{deleteAllJobs(...)}}{Delete all jobs for the current user.
#'
#'       \code{...} Extra parameters passed to \code{httr}
#'   }
#'   \item{\code{waitForCompletion(job, waittime=Inf, ...)}}{
#'       Wait for the specified \code{job} to complete.
#'
#'       \code{job} The \code{DOcplexcloudJob} or a job URL.
#'
#'       \code{waittime} The maximum time to wait, in seconds. This default to
#'       \code{Inf} if not specified.
#'
#'       \code{...} Extra parameters passed to \code{httr}
#'
#'       Upon time out, the last known execution status of the job is returned. 
#'
#'       Returns the job execution status, which can be: \code{CREATED},
#'       \code{NOT_STARTED}, \code{RUNNING}, \code{INTERRUPTING},
#'       \code{INTERRUPTED}, \code{FAILED}, \code{PROCESSED}
#'   }
#'   \item{\code{abortJob(job, ...)}}{Aborts the specified job.
#'
#'       \code{job} The \code{DOcplexcloudJob} or a job URL.
#'
#'       \code{...} Extra parameters passed to \code{httr}
#'   }
#'   \item{\code{deleteJob(job, ...)}}{Deletes the specified job.
#'
#'       \code{job} The \code{DOcplexcloudJob} or a job URL.
#'
#'       \code{...} Extra parameters passed to \code{httr}
#'   }
#'   \item{\code{executeJob(job, ...)}}{Submits an execution request for a job.
#'
#'       \code{job} The \code{DOcplexcloudJob} or a job URL.
#'
#'       \code{...} Extra parameters passed to \code{httr}
#'   }
#'   \item{\code{getJobStatus(job, ...)}}{Returns the execution status of a job.
#'
#'       \code{job} The \code{DOcplexcloudJob} or a job URL.
#'
#'       \code{...} Extra parameters passed to \code{httr}
#'   }
#'   \item{\code{getJobLogs(job, ...)}}{Download the logs for the job.
#'
#'       \code{job} The \code{DOcplexcloudJob} or a job URL.
#'
#'       \code{...} Extra parameters passed to \code{httr}
#'
#'       Returns a character string containing the job logs.
#'   }
#'   \item{\code{createJob(attachments=NULL, ...)}}{Creates a new job. The specified list
#'       of attachments is uploaded.
#'
#'       \code{attachments} A list of attachments.
#'
#'       \code{...} Any extra arguments of type \code{DOcplexcloudAttachment}
#'       (for instance, created with \code{link{addAttachment}}) are combined
#'       with \code{attachments}. Any other extra parameters are passed to
#'       \code{httr}
#'
#'       Returns The job URL.
#'   }
#'   \item{\code{uploadAttachment(job, attachment, ...)}}{
#'       Uploads the specified attachment for a job.
#'
#'       \code{job} The \code{DOcplexcloudJob} or a job URL.
#'
#'       \code{attachment} A \code{DOcplexcloudAttachment} attachment
#'       specification. See \code{\link{addAttachment}}.
#'
#'       \code{...} Extra parameters passed to \code{httr}
#'   }
#'   \item{\code{getAttachment(job, name, convert=TRUE, ...)}}{
#'       Downloads the specified attachment.
#'
#'       Attachments are always returned as raw data, unless
#'       the attachment ends with ".json". In that case, the
#'       json is downloaded, parsed and converted from JSON,
#'       unless \code{convert} is false
#'
#'       \code{job} The \code{DOcplexcloudJob} or a job URL.
#'
#'       \code{name} The name of the attachement.
#'
#'       \code{convert} If TRUE, the method try to convert the attachment to a
#'       data structure ready for use (example: parse JSON).
#'
#'       \code{...} Extra parameters passed to \code{httr}
#'   }
#'   \item{\code{getJobInfo(job, ...)}}{Returns job info for the specified job.
#'
#'       \code{job} The \code{DOcplexcloudJob} or a job URL.
#'
#'       \code{...} Extra parameters passed to \code{httr}
#'   }
#'   \item{\code{copyJob(job, shallow=FALSE, overide_data="", ...)}}{
#'       Creates a new job by copying an existing job.
#'
#'       The existing job must not be running or waiting for execution. All creation
#'       data are copied over to the new job. By default, the input attachments and
#'       their contents are copied in the new job. If a shallow copy is requested the
#'       new attachment will point to the existing job, and if it is deleted accessing
#'       the attachment wil raise an exception. Output attachments are not copied.
#'       Optionally, a job creation data can be passed to override the parameters and
#'       declare additional or replacement input attachments.
#' 
#'       \code{job} The \code{DOcplexcloudJob} or a job URL.
#'
#'       \code{shallow} Indicates if the copy is shallow.
#'
#'       \code{override_data} Data to override as a JSON formatted string.
#'
#'       \code{...} Extra parameters passed to \code{httr}
#'
#'       Returns The job URL.
#'   }
#' }
#'
#' @examples
#' \dontrun{
#' # create a new client, using environment variables for url and api key
#' client <- DOcplexcloudClient$new()
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
#' # download solution
#' solution = client$getAttachment(job, "solution.json")
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
        abortJob = function(job, ...) {
            "Abords the specified job"
            url <- paste(getJobUrl(job), "/execute", sep="")
            response = self$makeRequest("DELETE",
                                        url = url,
                                        fail_message = "abort job",
                                        content_type_json(),
                                        ...)
            return(response)
        },
        deleteJob = function(job, ...) {
            "Deletes the specified job"
            response = self$makeRequest("DELETE",
                                        url = getJobUrl(job),
                                        fail_message = "delete job",
                                        content_type_json(),
                                        ...)
            return(response)
        },
        executeJob = function(job, ...) {
            "Submits an execute request on a job"
            url = paste(getJobUrl(job), "/execute", sep="")

            response = self$makeRequest("POST",
                                        url = url,
                                        fail_message = "execute job",
                                        content_type_json(),
                                        ...)
            return(response)
        },
        getJobStatus = function(job, ...) {
            url = paste(getJobUrl(job), "/execute", sep="")
            response = self$makeRequest("GET",
                                        url = url,
                                        fail_message = "get status",
                                        content_type_json(),
                                        ...)
            return(content(response)$executionStatus)
        },
        getJobLogs = function(job, ...) {
            "Download the logs for a job"
            log_url <- paste(getJobUrl(job), "/log/blob", sep="")
            response <- self$makeRequest("GET",
                                         url = log_url,
                                         fail_message = "get logs",
                                         content_type("application/text"),
                                         ...)
            return(rawToChar(content(response)))
        },
        waitForCompletion = function(job, waittime=Inf, ...) {
            if(self$verbose) {
                cat("Waiting for completion\n")
            }
            elapsed <- 0
            sleep_duration <- 2
            url = getJobUrl(job)
            repeat {
                st <- self$getJobStatus(url)
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
        createJob = function(attachments=NULL,...) {
            "Creates a job"
            if(self$verbose) {
                cat("Creating job\n")
            }
            # get attachments from ...
            dots <- list(...)
            attachments_from_dots <- Filter(function(d) { inherits(d, "DOcplexcloudAttachment")}, dots)
            if(is.null(attachments)) {
                attachments <- list()
            }
            attachments <- c(attachments, attachments_from_dots)
            # do the requests
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
        uploadAttachment = function(job, attachment, ...) {
            name <- attachment$getName()
            att_url <- paste(getJobUrl(job), "/attachments/", name, "/blob", sep="")
            att_data <- attachment$getData()
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
        getAttachment = function(job, name, convert=TRUE, ...) {
            att_url <- paste(getJobUrl(job), "/attachments/", name, "/blob", sep="")
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
        getJobInfo = function(job, ...) {
            response <- self$makeRequest("GET",
                                         url = getJobUrl(job),
                                         fail_message = "get info",
                                         content_type_json(),
                                         ...)
            return(content(response))
        },
        copyJob = function(job, shallow=FALSE, override_data="", ...) {
            shallow_value = ifelse(shallow, "true", "false")
            shallow_option = ifelse(shallow, paste("?shallow=", shallow_value, sep=""),
                                    "")
            
            url <- paste(getJobUrl(job), "/copy", shallow_option, sep="")

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

