require(docplexcloud)

###################################################################
# Testing a model as string
###################################################################

source('credentials.R')




testModelAsString <- function() {
    print("-------------------------- test-model_as_string.R")

    client <- TestClient()
    model <- "Minimize\n
                obj: x + y\n
              Subject To\n
\n
              Bounds\n
                3 <= x <= 17\n
                2 <= y\n
              End\n"
    job <- client$submitJob(addAttachment(name="model.lp",
                                             data=charToRaw(model)),
                                 wait = FALSE)
    status <- client$waitForCompletion(job)
    print("DONE")
    if (status == "PROCESSED") {
        print("downloading and writing solution.json")
        solution = client$getAttachment(job, "solution.json")
        write(toJSON(solution), "solution.json")
    } else {
        # maybe an error ?
        print(paste("Job finished with status ", status, sep=""))
    }
    logs <- client$getJobLogs(job)
    cat("LOGS\n")
    cat(logs)
    info <- client$getJobInfo(job)
    client$deleteJob(job)
}

test_that("model as string", {
    testModelAsString()
})


