require(docplexcloud)

###################################################################
# Testing the simple REST mapping
###################################################################

source('credentials.R')


test_that("basic REST api works", {
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

        solution = getAttachment(job, "solution.json")
        write(solution, toJSON("solution.json"))
        info <- getInfo(job)
        for(a in info$attachments) {
            print(paste("attachment ", a$name, sep=""))
        }
    }, finally = {
        delete(job)
    })
    expect_equal(status, "PROCESSED")
 })
