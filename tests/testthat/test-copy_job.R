require(docplexcloud)

source('credentials.R')

createJob <- function(client) {
    job <- NULL
    tryCatch({
        print("create job")
        job <- DOcplexcloudJob(client=client)
        print("set attachment")
        job <- setAttachment(job, file="sample_diet.lp")
        print("submit job")
        job <- create(job)
        print("execute job")
        job <- execute(job)
        status <- waitForCompletion(job)
        print(paste("Job finished with status ", status, sep=""))
        
        solution <- getAttachment(job, "solution.json")
        write(toJSON(solution), "solution1.json")
    },
    finally = {
        return(job)
    })
}

testCopy <- function(job) {
    job_copy <- NULL
    tryCatch({
        client <- job$client
      
        # copy job
        job_copy = copyJob(job)
        print("execute job copy")
        job_copy <- execute(job_copy)
        status <- waitForCompletion(job_copy)
        print(paste("Job copy finished with status ", status, sep=""))
        solution <- getAttachment(job_copy, "solution.json")
        write(toJSON(solution), "solution2.json")
    }, finally = {
        return(job_copy)
    }) 
}

test_that("copy job and recreate work", {
    job <- NULL
    job_copy <- NULL
    # let's submit a job and solve it.
    # Theb copy the job, execute it and get solution
    # The two solutions should match. 
    tryCatch({
        client <- TestClient()
        job <- createJob(client)
        job_copy <- testCopy(job)
    }, finally = {
        if (!is.null(job))  delete(job)
        if (!is.null(job_copy))  delete(job_copy)
    })
    sol1 <- readChar("solution1.json", file.info("solution1.json")$size)
    sol2 <- readChar("solution2.json", file.info("solution2.json")$size)
    expect_equal(sol1, sol2)
})
