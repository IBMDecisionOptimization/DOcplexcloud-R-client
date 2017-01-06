#
# simple demo:
# - uploads a .lp model
# - submit job and wait for it to complete
# - download the solution as solution.json
#
require('docplexcloud')

job <- NULL
tryCatch({
    # uses environment variables DOCPLEXCLOUD_URL and DOCPLEXCLOUD_KEY
    # for \code{url} and \code{key} parameters
    client <- DOcplexcloudClient$new(verbose=TRUE)

    # create job, upload attachment, submit execution
    # and wait for completion
    job <- client$submitJob(addAttachment(file="sample_diet.lp"))
    if (job$executionStatus == "PROCESSED") {
        # Download attachment
        solution = client$getAttachment(job$joburl, "solution.json")
        # at this point, the json solution has ben parsed
        # and can be accessed using solution$CPLEXSolution
        # we can write it for future use or whatever
        write(toJSON(solution), "solution.json")
    } else {
        # An error occured
        cat("Job finished with status", job$executionStatus)
    }
}, finally = {
    if (!is.null(job))  client$deleteJob(job$joburl)
})
