require(docplexcloud)

###################################################################
# Testing a model with attachments as data.frames
###################################################################

source('credentials.R')

testModelAsString <- function() {
    print("-------------------------- test-data.frame-attachments.R")

    # parameters
    params.df = read.csv("params.csv")
    # accidents
    accidents_list = read.csv("predicted-accidents.csv")
    
    client <- TestClient()
    job <- client$submitJob(addAttachment("ambulances.py"),
                            addAttachment(name="params.csv"),
                            addAttachment(name="predicted-accidents.csv", data=accidents_list))
    status <- job$executionStatus
    print("DONE")
    if (status == "PROCESSED") {
        solution = client$getAttachment(job, "ambulances.csv")
        write(rawToChar(solution), "ambulances.csv")
        
        amb.df = read.csv("ambulances.csv")        
        expect_equal(nrow(amb.df), 5)
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

test_that("model with data.frames", {
    testModelAsString()
})
