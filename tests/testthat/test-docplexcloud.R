require(docplexcloud)

###################################################################
# Testing the api that most people will use
###################################################################

source('credentials.R')

variables.df <- NULL

test_that("execute on client", {
    client <- TestClient()
    variables.df <- NULL
    resp <- NULL
    tryCatch({
        resp <- client$submitJob(addAttachment(file="sample_diet.lp"))
        if (resp$executionStatus == "PROCESSED") {
            # This automatically parse the data
            solution = client$getAttachment(resp$joburl, "solution.json")

            # transform the list to list with named fields
            dc <- lapply(solution$CPLEXSolution$variables, unlist)

            # make a data frame from that
            variables.df <- as.data.frame(do.call("rbind", dc))
            # print(variables.df)
            print(variables.df[variables.df$name == 'x10', "value"])
        } else {
            # maybe an error ?
            print(paste("Job finished with status ", resp$executionStatus, sep=""))
        }
    }, error = function(err) {
      print(paste("ERROR:  ",err))
    }, finally = {
        if (!is.null(resp))  client$deleteJob(resp$joburl)
    })
    expect_equal(resp$executionStatus, "PROCESSED")
})

