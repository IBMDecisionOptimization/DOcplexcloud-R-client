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
        }
    ) 
)

#' Creates a new attachment.
#'
#' @param name The name of the attachment. If a \code{file} is specified,
#'   the name can be ommited and the basename of the \code{file} is used.
#' @param file The name of the file containing the data for the attachment.
#' @param data The raw data for the attachment.
#' @return An attachment object suitable to use with methods of
#'    DOcplexcloudClient and DOcplexcloudJob
#'
#' @examples
#' # Creates an attachment which content is the content of this file,
#' # and which name is "model.lp"
#' a <- addAttachment(file = "/home/joe/models/model.lp")
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
#' @export
addAttachment <- function(name = NULL, file = NULL, data = NULL) {
    return(DOcplexcloudAttachment$new(name=name, file=file, data=data))
}




