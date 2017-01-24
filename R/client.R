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
#' solve service by encapsulating the connection parameters (connection URL, API key).
#' The client also provides support for features of the DOcplexcloud solve service
#' that are not tied to a particular job, such as listing
#' all of the jobs for the current user or deleting all jobs.
#'
#' @docType class
#' @format The DOcplexcloud client class.
#' @importFrom R6 R6Class
#' @field url The DOcplexcloud URL.
#' @field key The DOcplexcloud API key.
#' @field verbose If TRUE, sets the verbose mode.
#' @export
#' @import httr
#' @import rjson
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(url, key, verbose=FALSE)}}{Initializes a new client with
#'       the specified \code{url} and \code{key} parameters.
#'
#'       If \code{url} is not specified, the value of environment variable
#'       \code{DOCPLEXCLOUD_URL} is used. If \code{key} is not specified, the
#'       value of the environment variable \code{key} is used.
#'
#'       When \code{verbose} is \code{TRUE}, extra information is printed when the
#'       client API is called.
#'   }
#'   \item{\code{submitJob(..., wait=TRUE)}}{Submits a job for execution.
#'
#'       This method creates the job, upoads the attachments, submits an execution
#'       request, then waits for completion of the job, unless \code{wait} is
#'       \code{FALSE}.
#'
#'       \code{compression} The compression algorithm used to compress data before
#'       it is send. The \code{compression} can be "none" or "gzip"
#'
#'       \code{...} Extra parameters passed to \code{httr}.
#'
#'       Returns a \code{DOcplexcloudJob}.
#'   }
#'   \item{\code{getAllJobs(...)}}{Returns all jobs for the current user.
#'
#'       \code{...} Extra parameters passed to \code{httr}.
#'    }
#'   \item{\code{deleteAllJobs(...)}}{Delete all jobs for the current user.
#'
#'       \code{...} Extra parameters passed to \code{httr}.
#'   }
#'   \item{\code{waitForCompletion(job, waittime=Inf, ...)}}{
#'       Waits for the specified \code{job} to complete.
#'
#'       \code{job} The \code{DOcplexcloudJob} or a job URL.
#'
#'       \code{waittime} The maximum time to wait, in seconds. This default is
#'       \code{Inf} if not specified.
#'
#'       \code{...} Extra parameters passed to \code{httr}.
#'
#'       Upon a time out, the last known execution status of the job is returned.
#'
#'       Returns the job execution status, which can be: \code{CREATED},
#'       \code{NOT_STARTED}, \code{RUNNING}, \code{INTERRUPTING},
#'       \code{INTERRUPTED}, \code{FAILED}, \code{PROCESSED}
#'   }
#'   \item{\code{abortJob(job, ...)}}{Aborts the specified job.
#'
#'       \code{job} The \code{DOcplexcloudJob} or a job URL.
#'
#'       \code{...} Extra parameters passed to \code{httr}.
#'   }
#'   \item{\code{deleteJob(job, ...)}}{Deletes the specified job.
#'
#'       \code{job} The \code{DOcplexcloudJob} or a job URL.
#'
#'       \code{...} Extra parameters passed to \code{httr}.
#'   }
#'   \item{\code{executeJob(job, ...)}}{Submits an execution request for a job.
#'
#'       \code{job} The \code{DOcplexcloudJob} or a job URL.
#'
#'       \code{...} Extra parameters passed to \code{httr}.
#'   }
#'   \item{\code{getJobStatus(job, ...)}}{Returns the execution status of a job.
#'
#'       \code{job} The \code{DOcplexcloudJob} or a job URL.
#'
#'       \code{...} Extra parameters passed to \code{httr}.
#'   }
#'   \item{\code{getJobLogs(job, ...)}}{Downloads the logs for the job.
#'
#'       \code{job} The \code{DOcplexcloudJob} or a job URL.
#'
#'       \code{...} Extra parameters passed to \code{httr}.
#'
#'       Returns a character string containing the job logs.
#'   }
#'   \item{\code{createJob(attachments=NULL, compression="gzip",...)}}{Creates a new job. The specified list
#'       of attachments is uploaded.
#'
#'       \code{attachments} A list of attachments.
#'
#'       \code{compression} The compression algorithm used to compress data before
#'       it is send. The \code{compression} can be "none" or "gzip"
#'
#'       \code{...} Any extra arguments of type \code{DOcplexcloudAttachment}
#'       (for instance, created with \code{link{addAttachment}}) are combined
#'       with \code{attachments}. Any other extra parameters are passed to
#'       \code{httr}.
#'
#'       Returns The job URL.
#'   }
#'   \item{\code{uploadAttachment(job, attachment, compression="gzip", ...)}}{
#'       Uploads the specified attachment for a job. Data are compressed using
#'       the specified \code{compression}.
#'
#'       \code{job} The \code{DOcplexcloudJob} or a job URL.
#'
#'       \code{attachment} A \code{DOcplexcloudAttachment} attachment
#'       specification. See \code{\link{addAttachment}}.
#'
#'       \code{compression} Specifies the compression algorithm to use. Currently,
#'       supported values are: "none" or "gzip"
#'
#'       \code{...} Extra parameters passed to \code{httr}.
#'   }
#'   \item{\code{getAttachment(job, name, convert=TRUE, ...)}}{
#'       Downloads the specified attachment.
#'
#'       Attachments are always returned as raw data, unless
#'       the attachment ends with ".json". In that case, the
#'       JSON file is downloaded, parsed, and converted from JSON,
#'       unless \code{convert} is false.
#'
#'       \code{job} The \code{DOcplexcloudJob} or a job URL.
#'
#'       \code{name} The name of the attachement.
#'
#'       \code{convert} If TRUE, the method tries to convert the attachment to a
#'       data structure that is ready for use (example: parse JSON).
#'
#'       \code{...} Extra parameters passed to \code{httr}.
#'   }
#'   \item{\code{getJobInfo(job, ...)}}{Returns job info for the specified job.
#'
#'       \code{job} The \code{DOcplexcloudJob} or a job URL.
#'
#'       \code{...} Extra parameters passed to \code{httr}.
#'   }
#'   \item{\code{copyJob(job, shallow=FALSE, overide_data="", ...)}}{
#'       Creates a new job by copying an existing job.
#'
#'       The existing job must not be running or waiting for execution. All creation
#'       data are copied over to the new job. By default, the input attachments and
#'       their contents are copied in the new job. If a shallow copy is requested, the
#'       new attachment will point to the existing job, and, if it is deleted, accessing
#'       the attachment wil raise an exception. Output attachments are not copied.
#'       Optionally, job creation data can be passed to override the parameters and
#'       declare additional or replacement input attachments.
#'
#'       \code{job} The \code{DOcplexcloudJob} or a job URL.
#'
#'       \code{shallow} Indicates if the copy is shallow.
#'
#'       \code{override_data} Data to override as a JSON formatted string.
#'
#'       \code{...} Extra parameters passed to \code{httr}.
#'
#'       Returns The job URL.
#'   }
#' }
#'
#' @examples
#' \dontrun{
#' # Create a new client, using environment variables for URL and API key
#' client <- DOcplexcloudClient$new()
#'
#' # Create and execute a job to solve model.lp
#' job <- client$submitJob(addAttachment(file="model.lp"))
#'
#' # Create and execute a job which model is stored
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
#' # Download solution
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
        createJob = function(attachments=NULL, compression="gzip",...) {
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
                self$uploadAttachment(joburl, attachment=a, compression=compression)
            }
            return(joburl)
        },
        uploadAttachment = function(job, attachment, compression="gzip", ...) {
            name <- attachment$getName()
            att_url <- paste(getJobUrl(job), "/attachments/", name, "/blob", sep="")
            if (self$verbose) {
                cat("Uploading attachment", name, "\n")
            }
            att_data <- attachment$getData()
            
            # attachments are all supposed to be octet-stream
            h = list();
            h["Content-Type"] <- "application/octet-stream";
            if (compression == "gzip") {
                # when compressing is wanted, and is gzip, gzip the stream
                # we need to do that in an actual file to have file headers etc
                h["Content-Encoding"] <- compression;
                output <- NULL
                tryCatch({
                    output <- tempfile()
                    gzo = gzfile(output, "wb");
                    writeBin(att_data, gzo);
                    close(gzo);
                    att_data <- readBin(file(output, "rb"), what="raw", n=file.info(output)$size)
                }, finally = {
                  if (!is.null(output)) file.remove(output)
                })
            }
            
            response <- self$makeRequest("PUT",
                                     url = att_url,
                                     fail_message = "upload attachment to url",
                                     do.call(add_headers, h),
                                     #content_type("application/octet-stream"),
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
        submitJob = function(..., compression="gzip", wait = TRUE) {
            "Submits a job for execution and wait for completion"
            dots <- list(...)
            attachments <- Filter(function(d) { inherits(d, "DOcplexcloudAttachment")}, dots)

            job <- DOcplexcloudJob$new()
            job$joburl <- self$createJob(attachments=attachments, compression=compression)
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

