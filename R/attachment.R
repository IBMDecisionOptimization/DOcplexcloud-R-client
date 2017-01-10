library(R6)

#' The base class for attachments.
#'
#' @field name The name of the attachment. If a \code{file} is specified,
#'   the name can be ommited and the basename of the \code{file} is used.
#' @field file The name of the file containing the data for the attachment.
#' @field data The raw data for the attachment.
#'
#' @keywords internal
#' @export
DOcplexcloudAttachment <- R6Class("DOcplexcloudAttachment",
    public = list(
        name = NULL,
        file = NULL,
        data = NULL,
        initialize = function(name=NULL, file=NULL, data=NULL) {
            #if (!is.null(name) && is.null(file) && !is.null(data)) {
            #    file <- name
            #    name <- NULL
            #}
            self$name <- name
            self$file <- file
            self$data <- data
        },
        getName = function() {
            "Returns a name for the attachment.
            
            The name of the attachment is the \\code{name} field of the attachement.
            If that field is empty or NULL, then the name is the basename of
            \\code{attachment$file}
            "
            n <- self$name
            if (is.null(self$name) || self$name == "") {
                n <- basename(self$file)
            }
            return(n)
        },
        getData = function() {
            if (is.null(self$file) && is.null(self$data) && !is.null(self$name)) {
                att_data <- charToRaw(readChar(self$name, file.info(self$name)$size))
            }
            else if (is.null(self$data) && !is.null(self$file) && !(self$file == "")) {
                # no data specified -> read file
                att_data <- charToRaw(readChar(self$file, file.info(self$file)$size))
            } else {
                # data was specified
                att_data <- self$data
                if (is.list(att_data)) {
                    output <- NULL
                    tryCatch({
                        output <- tempfile()
                        write.csv(att_data, output)
                        att_data <- charToRaw(readChar(output, file.info(output)$size))
                    }, finally = {
                        if (!is.null(output)) file.remove(output)
                    }) 
                }
            }
            if (!is.raw(att_data)) {
              att_data <- as.raw(att_data)
            }
            return(att_data)
        }
    ) 
)

#' Creates a new attachment.
#'
#' @param name The name of the attachment. If a \code{file} is specified,
#'   the name can be ommited and the basename of the \code{file} is used.
#'   If \code{name} is a string pointing to a file, and \code{file} is not
#'   specified, the content of the file pointed by \code{name} is used as
#'   attachment data.
#' @param file The name of the file containing the data for the attachment.
#' @param data The data for the attachment. If \code{data} is a list which
#'   \code{names(data)} is not empty, the data is first converted to csv.
#' @return An attachment object suitable to use with methods of
#'    DOcplexcloudClient and DOcplexcloudJob
#'
#' @examples
#' # Creates an attachment which content is the content of this file,
#' # and which name is "model.lp" (the 3 expressions are equivalent):
#' a <- addAttachment(file = "/home/joe/models/model.lp")
#' a <- addAttachment(name = "/home/joe/models/model.lp")
#' a <- addAttachment("/home/joe/models/model.lp")
#'
#' # Creates an attachment which content is the specified file, and
#' # which name is "model.lp"
#' a <- addAttachment(name = "model.lp",
#'                    file = "/home/joe/models/model_1231.lp")
#'
#' # create an attachment which data is stored in memory
#' model <- "Minimize
#'             obj: x + y
#'           Subject To
#'           Bounds
#'             3 <= x <= 17
#'             2 <= y
#'           End"
#' a <- addAttachment(name="model.lp",
#'                    data=charToRaw(model))
#' \dontrun{
#' # submits and run the specified model.lp:
#' job <- client$submitJob(addAttachment("/home/joe/models/model.lp"))
#' }
#' @export
addAttachment <- function(name = NULL, file = NULL, data = NULL) {
    return(DOcplexcloudAttachment$new(name=name, file=file, data=data))
}




