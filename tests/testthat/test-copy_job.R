require(docplexcloud)

source('credentials.R')

createJob <- function(client) {
    job <- NULL
    tryCatch({
        print("create job")
        job <- client$createJob(attachments=c(addAttachment(file="sample_diet.lp")))
        print("execute job")
        client$executeJob(job)
        status <- client$waitForCompletion(job)
        print(paste("Job finished with status ", status, sep=""))
        
        solution <- client$getAttachment(job, "solution.json")
        write(toJSON(solution), "solution1.json")
    },
    finally = {
        return(job)
    })
}

testCopy <- function(client, job) {
    job_copy <- NULL
    tryCatch({      
        # copy job
        job_copy <- client$copyJob(job)
        print("execute job copy")
        client$executeJob(job_copy)
        status <- client$waitForCompletion(job_copy)
        print(paste("Job copy finished with status ", status, sep=""))
        solution <- client$getAttachment(job_copy, "solution.json")
        write(toJSON(solution), "solution2.json")
    }, finally = {
        return(job_copy)
    }) 
}

test_that("copy job and recreate work", {
    job <- NULL
    job_copy <- NULL
    client <- TestClient()
    # let's submit a job and solve it.
    # Theb copy the job, execute it and get solution
    # The two solutions should match. 
    tryCatch({
        job <- createJob(client)
        job_copy <- testCopy(client, job)
    }, finally = {
        if (!is.null(job))  client$deleteJob(job)
        if (!is.null(job_copy))  client$deleteJob(job_copy)
    })
    sol1 <- readChar("solution1.json", file.info("solution1.json")$size)
    sol2 <- readChar("solution2.json", file.info("solution2.json")$size)
    expect_equal(sol1, sol2)
})
