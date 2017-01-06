require(docplexcloud)

###################################################################
# Testing the api that most people will use
###################################################################

source('credentials.R')

variables.df <- NULL

test_that("execute on client", {
    print("-------------------------- test-docplexcloud.R")
    client <- TestClient()
    variables.df <- NULL
    job <- NULL
    tryCatch({
        job <- client$submitJob(addAttachment(file="sample_diet.lp"))
        if (job$executionStatus == "PROCESSED") {
            # This automatically parse the data
            solution = client$getAttachment(job, "solution.json")

            # transform the list to list with named fields
            dc <- lapply(solution$CPLEXSolution$variables, unlist)

            # make a data frame from that
            variables.df <- as.data.frame(do.call("rbind", dc))
            # print(variables.df)
            print(variables.df[variables.df$name == 'x10', "value"])
        } else {
            # maybe an error ?
            print(paste("Job finished with status ", job$executionStatus, sep=""))
        }
    }, error = function(err) {
      print(paste("ERROR:  ",err))
    }, finally = {
        if (!is.null(job))  client$deleteJob(job)
    })
    expect_equal(job$executionStatus, "PROCESSED")
})

