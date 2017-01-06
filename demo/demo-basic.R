#
# simple demo:
# - uploads a .lp model
# - submit job and wait for it to complete
# - download the solution as solution.json
#
require('docplexcloud')

response <- NULL
tryCatch({
    # uses environment variables DOCPLEXCLOUD_URL and DOCPLEXCLOUD_KEY
    client <- DOcplexcloudClient$new(verbose=TRUE)

    # create job, upload attachment, submit execution
    # and wait for completion
    response <- client$submitJob(addAttachment(file="sample_diet.lp"))
    if (response$executionStatus == "PROCESSED") {
        # Download attachment
        solution = client$getAttachment(response$joburl, "solution.json")
        # at this point, the json solution has ben parsed
        # and can be accessed using solution$CPLEXSolution
        # we can write it for future use or whatever
        write(toJSON(solution), "solution.json")
    } else {
        # An error occured
        cat("Job finished with status", job$executionStatus)
    }
}, finally = {
    if (!is.null(response))  client$deleteJob(jobResponse$joburl)
})
