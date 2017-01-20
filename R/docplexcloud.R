require(httr)
require(rjson)

#' IBM Decision Optimization on Cloud R Client
#'
#' IBM Decision Optimization on Cloud (DOcplexcloud) enables you to solve
#' optimization problems on the cloud without installing or configuring a
#' solver.
#'
#' This client handles the connection for you so that you can jump into coding
#' faster.
#'
#' @docType package
#' @name docplexcloud
#' @examples
#' \dontrun{
#' # Create a new client, using environment variables for URL and API key
#' client <- DOcplexCloudClient$new()
#'
#' # Create and run a job to solve model.lp
#' job <- client$submitJob(addAttachment(file="model.lp"))
#'
#' # Wait for the job to complete
#' status <- client$waitForCompletion(job)
#'
#' # When solving an .lp file, DOcplexcloud creates an output attachment
#' # called 'solution.json' with the CPLEXSolution. This downloads the attachment
#' # and parses the JSON to a hierarchy of lists.
#' solution <- client$getAttachment(job, "solution.json")
#'
#' # In particular, you can access variables like this:
#' variables = solution$CPLEXSolution$variables
#' }
NULL
