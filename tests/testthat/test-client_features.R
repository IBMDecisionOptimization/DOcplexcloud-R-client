require(docplexcloud)

###################################################################
# Testing some client features, like list jobs etc...
###################################################################

source('credentials.R')


testClientFeatures <- function() {
   
}

test_that("client features work", {
    jobs <- list()
    tryCatch({
        client <- TestClient()
        
        now <- gsub("[ :]", "_", Sys.time())
        
        # create some jobs
        jobs <- list()
        for (i in 1:3) {
            job <- DOcplexcloudJob(client=client)
            name <- paste("job", i, "_", now , ".lp", sep="")
            job <- setAttachment(job, name=name, file="sample_diet.lp")
            job <- create(job)
            jobs[[length(jobs)+1]] <- job
            print(paste("created job", job$joburl))
        }
        # list jobs
        jlist <- getAllJobs(client)
    }, finally = {
        for (job in jobs) {
            job <- delete(job)
        }
    })
    expect_equal(length(jobs), 3)
})

