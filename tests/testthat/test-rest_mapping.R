require(docplexcloud)

###################################################################
# Testing the simple REST mapping
###################################################################

source('credentials.R')


test_that("basic REST api works", {
    if (is.null(baseUrl) || baseUrl == "" || is.null(apiKey) || apiKey == "")
        skip("No URL")
    job <- NULL
    tryCatch({
        client <- TestClient()
        print("create job")
        job <- DOcplexcloudJob(client=client)
        print("set attachment")
        job <- setAttachment(job, file="sample_nurse.lp")
        print("submit job")
        job <- create(job)
        print("execute job")
        job <- execute(job)
        status <- waitForCompletion(job)
        print(paste("Job finished with status ", status, sep=""))

        solution.json = getAttachment(job, "solution.json")
        writeBin(solution.json, "solution.json")
        info <- getInfo(job)
        for(a in info$attachments) {
            print(paste("attachment ", a$name, sep=""))
        }
    }, finally = {
        delete(job)
    })
    expect_equal(status, "PROCESSED")
 })
