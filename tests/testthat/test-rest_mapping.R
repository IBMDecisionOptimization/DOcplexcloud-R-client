require(docplexcloud)

###################################################################
# Testing the simple REST mapping
###################################################################

source('credentials.R')


test_that("basic REST api works", {
    joburl <- NULL
    client <- TestClient()
    status <- NULL
    tryCatch({
        print("create job")
        joburl <- client$createJob(attachments=c(addAttachment(file="sample_nurse.lp")))

        print("execute job")
        response <- client$executeJob(joburl)
        status <- client$waitForCompletion(joburl)
        print(paste("Job finished with status ", status, sep=""))

        solution <- client$getAttachment(joburl, "solution.json")
        write(toJSON(solution), "solution.json")
        info <- client$getJobInfo(joburl)
        for(a in info$attachments) {
            print(paste("attachment ", a$name, sep=""))
        }
    }, finally = {
        client$deleteJob(joburl)
    })
    expect_equal(status, "PROCESSED")
 })
