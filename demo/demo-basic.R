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
    client <- DOcplexcloudClientR6$new(verbose=TRUE)
    job <- client$submitJob(addAttachment(file="sample_diet.lp"))
    status <- waitForCompletion(job)
    if (status == "PROCESSED") {
        # Download attachment
        solution = getAttachment(job, "solution.json")
        # at this point, the json solution has ben parsed
        # and can be accessed using solution$CPLEXSolution
        # we can write it for future use or whatever
        write(toJSON(solution), "solution.json")
    } else {
        # An error occured
        print(paste("Job finished with status ", status, sep=""))
    }
}, finally = {
    if (!is.null(job))  delete(job)
})
