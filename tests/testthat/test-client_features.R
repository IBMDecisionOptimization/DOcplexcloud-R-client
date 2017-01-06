require(docplexcloud)

###################################################################
# Testing some client features, like list jobs etc...
###################################################################

source('credentials.R')


testClientFeatures <- function() {
   
}

test_that("client features work", {
    print("-------------------------- test-client features.R")

    jobs <- list()
    tryCatch({
        client <- TestClient()
        
        now <- gsub("[ :]", "_", Sys.time())
        
        # create some jobs
        jobs <- list()
        for (i in 1:3) {
            name <- paste("job", i, "_", now , ".lp", sep="")
            joburl <- client$createJob(attachments=c(addAttachment(name=name,
                                                                   file="sample_diet.lp")))
            jobs[[length(jobs)+1]] <- joburl
            print(paste("created job", joburl))
        }
        # list jobs
        jlist <- client$getAllJobs()
    }, finally = {
        for (job in jobs) {
            job <- client$deleteJob(job)
        }
    })
    expect_equal(length(jobs), 3)
})

