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



#' Aborts the specified job.
#'
#' @param job The job to abort.
#' @param ... Extra parameters passed to \code{httr}
#' @import httr
#' @export
abort <- function(job, ...) {
    UseMethod("abort", job)
}

#' @export
abort.DOcplexcloudJob=function(job, ...)
{
    # If job has never been created before: just create it
    url <- paste(job$joburl, "/execute", sep="")

    response = job$client$makeRequest("DELETE",
                           url = url,
                           fail_message = "abort job",
                           content_type_json(),
                           ...)
    return(job)
}



#' Attach the specified file or data to the job.
#' The attachement is just attached. Data is actually uploaded when job is
#' created.
#'
#' @param job The job
#' @param name The name of the attachment. If a \code{file} is specified,
#'   the name can be ommited and the basename of the \code{file} is used.
#' @param file The name of the file containing the data for the attachment.
#' @param data The raw data for the attachment.
#' @param ... Extra parameters passed to \code{httr}
#' @return The job
#' @export
setAttachment <- function(job, name = NULL, file = NULL, data = NULL) {
    UseMethod("setAttachment", job)
}

#' @export
setAttachment.DOcplexcloudJob=function(job, name = NULL, file = NULL, data = NULL)
{
    if (is.null(file) && is.null(name)) {
        stop("Cannot attach data without attachment name.")
    }
    if (is.null(name)) {
        name <- basename(file)
    }
    job$attachments[[length(job$attachments)+1]] <- addAttachment(name=name, file=file, data=data)
    return(job)
}



#' Downloads the specified log for a job.
#'
#' @param job The job.
#' @param ... Extra parameters passed to \code{httr}
#' @return The logs as a raw data
#' @export
getLogs <- function(job, ...) {
    UseMethod("getLogs", job)
}

#' @export
getLogs.DOcplexcloudJob=function(job, ...)
{
    log_url <- paste(job$joburl, "/log/blob", sep="")
    response <- RETRY("GET",
                      url = log_url,
                      add_headers( "X-IBM-Client-Id" = job$client$key),
                      content_type("application/text"),
                      ...)
    stop_for_status(response, paste("get logs ", log_url, sep=""))
    return(rawToChar(content(response)))
}


#' Downloads the specified attachment for a job.
#'
#' Attachments are always returned as raw data, unless
#' the attachment ends with ".json". In that case, the
#' json is downloaded, parsed and converted from JSON,
#' unless \code{convert} is false
#'
#' @param job The job.
#' @param name The name of the attachment.
#' @param convert If TRUE, the method try to convert the attachment to a data
#'        structure ready for use (example: parse JSON).
#' @param ... Extra parameters passed to \code{httr}
#' @export
getAttachment <- function(job, name, convert, ...) {
    UseMethod("getAttachment", job)
}

#' @export
getAttachment.DOcplexcloudJob=function(job, name, convert=TRUE, ...)
{
    att_url <- paste(job$joburl, "/attachments/", name, "/blob", sep="")
    response <- RETRY("GET",
                      url = att_url,
                      add_headers( "X-IBM-Client-Id" = job$client$key),
                      content_type("application/octet-stream"),
                      ...)
    stop_for_status(response, paste("get attachment ", att_url, sep=""))
    if (convert && endsWith(tolower(name), ".json")) {
        return(fromJSON(rawToChar(content(response))))
    } else {
        return(content(response))
    }
}




#' Uploads the specified attachment for a job. The data is actually uploaded.
#' The job must have been created.
#'
#' @param job The job.
#' @param attachment The attachment.
#' @param ... Extra parameters passed to \code{httr}
#' @return The job with updated attachments.
#' @export
uploadAttachment <- function(job, attachment=NULL, ...) {
    UseMethod("uploadAttachment", job)
}

#' @export
uploadAttachment.DOcplexcloudJob=function(job, attachment=NULL, ...)
{
    name <- attachment$getName()
    att_url <- paste(job$joburl, "/attachments/", name, "/blob", sep="")
    if (!is.null(attachment$file) && !(attachment$file == "")) {
        att_data <- charToRaw(readChar(attachment$file, file.info(attachment$file)$size))
    } else {
        att_data <- attachment$data
    }
    if (!is.raw(att_data)) {
      att_data <- as.raw(att_data)
    }
    if (job$client$verbose) {
        print(paste("Uploading attachment", att_url))
    }  
    response <- job$client$makeRequest("PUT",
                             url = att_url,
                             fail_message = "upload attachment to url",
                             content_type("application/octet-stream"),
                             ...,
                             body=att_data)
    stop_for_status(response, paste('error uploading attachment', url))
    # update attachment list.
    index <- -1
    for (i in 1:length(job$attachments)) {
        iname <- job$attachments[[i]]$getName()
        if (iname == name) {
            index <- i
        }
    }
    if (index == -1) {
        index <- length(job$attachments) + 1
    } 
    job$attachments[[index]] <- attachment
    return(job)
}




#' Returns job info for the specified job.
#'
#' @param job The job.
#' @param ... Extra parameters passed to \code{httr}
#' @return the job info.
#' @export
getInfo <- function(job, ...) {
    UseMethod("getInfo", job)
}

#' @export
getInfo.DOcplexcloudJob=function(job, ...)
          {
            response <- job$client$makeRequest("GET",
                                    url = job$joburl,
                                    fail_message = "get info",
                                    content_type_json(),
                                    ...)
            return(content(response))
          }


#' Create job and submit job.
#'
#' This method creates a job, upload attachments and submit a request to execute the job.
#'
#' @param job The job.
#' @param ... Extra parameters passed to \code{httr}
#' @export
#' @import rjson
create <- function(job, ...) {
    UseMethod("create", job)
}

#' @export
create.DOcplexcloudJob=function(job, ...)
{
    # If job has never been created before: just create it
    if (is.null(job$joburl)) {
        if (job$client$verbose) {
            print("creating job")
        }
        # The url to create jobs
        url <- paste(job$client$url, "/jobs", sep="")

        att_names <- Map(function(a) a$getName(),
                         job$attachments)

        if (job$client$verbose) {
            print(paste("attachment list: ", att_names))
        }
        body <- list(attachments=att_names)
        response <- job$client$makeRequest("POST",
                                url = url,
                                fail_message = "create job on url",
                                content_type_json(),
                                ...,
                                body = toJSON(body))
        stop_for_status(response, paste('could not create job', url))

        job$joburl <- headers(response)$location
        e <- strsplit(job$joburl, "/", fixed=TRUE)[[1]]
        job$jobid <- e[[length(e)]]
        
        # now upload attachments
        for (a in job$attachments)
        {
            job = uploadAttachment(job, attachment=a)
        }
    }
    # If job was already created: TODO: Issue error ?
    return(job)
}


#' Creates a new job by copying an existing job.
#'
#' The existing job must not be running or waiting for execution. All creation
#' data are copied over to the new job. By default, the input attachments and
#' their contents are copied in the new job. If a shallow copy is requested the
#' new attachment will point to the existing job, and if it is deleted accessing
#' the attachment wil raise an exception. Output attachments are not copied.
#' Optionally, a job creation data can be passed to override the parameters and
#' declare additional or replacement input attachments.
#'
#' @param job The job.
#' @param shallow Indicates if the copy is shallow.
#' @param override_data Data to override as a JSON formatted string.
#' @param ... parameters to override
#' @export
#' @import rjson
copyJob <- function(job, shallow=FALSE, override_data="", ...) {
    UseMethod("copyJob", job)
}

#' @export
copyJob.DOcplexcloudJob=function(job, shallow=FALSE, override_data="", ...)
{

    shallow_value = ifelse(shallow, "true", "false")
    shallow_option = ifelse(shallow, paste("?shallow=", shallow_value, sep=""),
                            "")
    
    url <- paste(job$joburl, "/copy", shallow_option, sep="")

    if (job$client$verbose) {
        print(paste("copying job, url =", url))
    }
    
    response = job$client$makeRequest("POST",
                           url = url,
                           fail_message = "copy job",
                           content_type_json(),
                           body = override_data,
                           ...)

    copy_url =  headers(response)$location
    new_job = DOcplexcloudJob(client=job$client, joburl=copy_url,
                              attachments=job$attachments) 
    return(new_job)
}



#' Delete the job.
#'
#' @param job The job.
#' @param ... Extra parameters passed to \code{httr}
#' @return the job.
#' @export
delete <- function(job, ...) {
    UseMethod("delete", job)
}

#' @export
delete.DOcplexcloudJob=function(job, ...)
          {
                # If job has never been created before: just create it
                if (job$joburl != "") {
                    response = job$client$makeRequest("DELETE",
                                           url = job$joburl,
                                           fail_message = "delete job",
                                           content_type_json(),
                                           ...)
                }
                return(job)
          }


#' Submits an execution request for a job.
#'
#' @param job The job.
#' @param ... Extra parameters passed to \code{httr}
#' @return The job.
#' @export
execute <- function(job, ...) {
    UseMethod("execute", job)
}

#' @export
execute.DOcplexcloudJob=function(job, ...)
{
    # If job has never been created before: just create it
    url = paste(job$joburl, "/execute", sep="")

    response = job$client$makeRequest("POST",
                           url = url,
                           fail_message = "execute job",
                           content_type_json(),
                           ...)
    return(job)
}



#' Returns the execution status of a job.
#'
#' @param job The job.
#' @param ... Extra parameters passed to \code{httr}
#' @export
status <- function(job, ...) {
    UseMethod("status", job)
}

#' @export
status.DOcplexcloudJob=function(job, ...)
{
    # If job has never been created before: just create it
    url = paste(job$joburl, "/execute", sep="")
    response = job$client$makeRequest("GET",
                           url = url,
                           fail_message = "get status",
                           content_type_json(),
                           ...)
    return(content(response)$executionStatus)
}


#' Wait for the specified job to complete.
#'
#' @param job The job
#' @param waittime The maximum time to wait. This default to \code{Inf} if not specified.
#' @param ... Extra parameters passed to \code{httr}
#'
#' @return the status as a string value.
#' @export
waitForCompletion <- function(job, waittime=Inf, ...) {
    UseMethod("waitForCompletion", job)
}

#' @export
waitForCompletion.DOcplexcloudJob=function(job, waittime=Inf, ...)
{
    elapsed <- 0
    sleep_duration <- 2
    repeat {
        st <- status(job)
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
}


