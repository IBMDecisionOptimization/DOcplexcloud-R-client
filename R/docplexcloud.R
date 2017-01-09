require(httr)
require(rjson)

#' IBM Decision Optimization on Cloud R Client
#'
#' IBM Decision Optimization on Cloud (DOcplexcloud) allows you to solve
#' optimization problems on the cloud without installing or configuring a
#' solver.
#'
#' This client handle the connection for you so that you can jump into coding
#' faster.
#'
#' @docType package
#' @name docplexcloud
#' @examples
#' \dontrun{
#' # create a new client, using environment variables for url and api key
#' client <- DOcplexCloudClient$new()
#'
#' # create and execute a job to solve model.lp
#' job <- client$submitJob(addAttachment(file="model.lp"))
#'
#' # wait for the job to complete
#' status <- client$waitForCompletion(job)
#'
#' # When solving a .lp file, DOcplexcloud creates an output attachment
#' # called 'solution.json' with the CPLEXSolution. This download the attachment
#' # and parse the JSON to a hierarchy of list.
#' solution <- client$getAttachment(job, "solution.json")
#'
#' # In particular, you can access variables with this:
#' variables = solution$CPLEXSolution$variables
#' }
NULL