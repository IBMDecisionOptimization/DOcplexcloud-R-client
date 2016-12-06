#' Creates a new instance of \code{DOcplexcloudClient}
#'
#' @param url The DOcplexcloud url. If not specified, the value of environment
#'      variable DOCPLEXCLOUD_URL is used.
#' @param key The DOcplexcloud api key. If not specified, the value of environment
#'      variable DOCPLEXCLOUD_KEY is used
#' @param verbose If TRUE, sets the verbose mode.
#' @return The DOcplexcloudClient
#' @export
DOcplexcloudClient <- function(url=NULL, key=NULL, verbose=FALSE) {
    if (is.null(url))
        url = Sys.getenv("DOCPLEXCLOUD_URL")
    if (is.null(key))
        key = Sys.getenv("DOCPLEXCLOUD_KEY")
    me <- list(url = url,
               key = key,
               verbose = verbose)
    class(me) <- append(class(me), "DOcplexcloudClient")
    return(me)
}


#' Submit a job execution.
#'
#' Once the job is executed, it can be monitored using waitForCompletion() then
#' deleted with delete()
#'
#' @param client A DOcplexcloudClient.
#' @param ... Further parameters like attachments.
#'
#' @return A DOcplexcloudJob.
#'
#' @examples
#' \dontrun{
#' # create and execute a job to solve model.lp
#' job <- submit(client, addAttachment(file="model.lp"))
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
#' job <- submit(client, addAttachment(name="model.lp",
#'                                     data=charToRaw(model)))
#' }
#' @export
submitJob <- function(client, ...) {
    UseMethod("submitJob", client)
}

#' @export
submitJob.DOcplexcloudClient <- function(client, ...)
{
    dots <- list(...)
    attachments <- Filter(function(d) { inherits(d, "DOcplexcloudAttachment")}, dots)

    job <- DOcplexcloudJob(client=client)
    job$attachments <- Map(function(a) {
                                if (is.null(a$name) || a$name == "")
                                    a$name <- a$file
                                return(a)
                            },
                            attachments)
    job <- create(job)
    job <- execute(job)
    return (job)
}


#' Submits a REST request to DOcplexcloud.
#' 
#' This makes a request to DOcplexcloud, with the relevant headers.
#' @param client The client.
#' @param verb The request verb.
#' @param url The URL.
#' @param fail_message The message to display incase of failure
#' @param ... Extra parameters passed to \code{httr}
#' @return The response.
#' @import httr
#' @keywords internal
#' @export
makeRequest <- function (client, verb, url, fail_message, ...) {
    UseMethod("makeRequest", client)
}

#' @export
makeRequest.DOcplexcloudClient <- function (client, verb, url, fail_message, ...) {
    response = RETRY(verb,
                     url = url,
                     add_headers("X-IBM-Client-Id" = client$key),
                     # content_type_json(),
                     ...)
    stop_for_status(response, paste(fail_message, url, sep=" "))
    return(response)
} 


#' Returns all jobs for the current user.
#'
#' @param client The client.
#' @param ... Extra parameters passed to \code{httr}
#' @return a list of DOcplexcloudJob
#' @export
getAllJobs <- function (client, ...) UseMethod("getAllJobs", client)

#' @export
getAllJobs.DOcplexcloudClient <- function (client, ...) {
    url <- paste(client$url, "/jobs", sep="")
    response = makeRequest(client,
                           "GET",
                           url = url,
                           content_type_json(),
                           ...)
    return(content(response))
}


#' Delete all jobs for the current user.
#'
#' @param client The client.
#' @param ... Extra parameters passed to \code{httr}
#' @return The response from the server.
#' @export
deleteAllJobs <- function (client, ...) UseMethod("deleteAllJobs", client)

#' @export
deleteAllJobs.DOcplexcloudClient <- function (client, ...) {
    url <- paste(client$url, "/jobs", sep="")
    response = makeRequest(client,
                           "DELETE",
                           url = url,
                           content_type_json(),
                           ...)
    return(response)
}