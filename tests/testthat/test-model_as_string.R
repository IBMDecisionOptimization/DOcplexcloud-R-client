require(docplexcloud)

###################################################################
# Testing a model as string
###################################################################

source('credentials.R')




testModelAsString <- function() {
    client <- TestClient()
    model <- "Minimize\n
                obj: x + y\n
              Subject To\n
\n
              Bounds\n
                3 <= x <= 17\n
                2 <= y\n
              End\n"
    job <- submitJob(client,
                     addAttachment(name="model.lp",
                                   data=charToRaw(model)))
    status <- waitForCompletion(job)
    print("DONE")
    if (status == "PROCESSED") {
        print("downloading and writing solution.json")
        solution.json = getAttachment(job, "solution.json")
        writeBin(solution.json, "solution.json")
    } else {
        # maybe an error ?
        print(paste("Job finished with status ", status, sep=""))
    }
    logs <- getLogs(job)
    print("LOGS")
    print(rawToChar(logs))
    info <- getInfo(job)
    print("INFO")
    print(info)
    delete(job)
}

test_that("model as string", {
    testModelAsString()
})


