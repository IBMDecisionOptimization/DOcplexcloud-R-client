require(docplexcloud)

###################################################################
# Testing the api that most people will use
###################################################################

source('credentials.R')

variables.df <- NULL

test_that("execute on client", {
    variables.df <- NULL
    job <- NULL
    tryCatch({
        client <- TestClient()
        job <- submitJob(client, addAttachment(file="sample_diet.lp"))
        status <- waitForCompletion(job)
        print("DONE")
        if (status == "PROCESSED") {
            # This automatically parse the data
            solution = getAttachment(job, "solution.json")

            # transform the list to list with named fields
            dc <- lapply(solution$CPLEXSolution$variables, unlist)

            # make a data frame from that
            variables.df <- as.data.frame(do.call("rbind", dc))
            print(variables.df)
            variables.df[variables.df$name == 'x10',]["value"]
        } else {
            # maybe an error ?
            print(paste("Job finished with status ", status, sep=""))
        }
    }, error = function(err) {
      print(paste("ERROR:  ",err))
    }, finally = {
        if (!is.null(job))  delete(job)
    })
    expect_equal(status, "PROCESSED")
})

