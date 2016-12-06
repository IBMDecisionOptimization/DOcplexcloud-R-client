#' Creates a new attachment.
#'
#' @param name The name of the attachment. If a \code{file} is specified,
#'   the name can be ommited and the basename of the \code{file} is used.
#' @param file The name of the file containing the data for the attachment.
#' @param data The raw data for the attachment.
#'
#' @examples
#' # Creates an attachment which content is the content of this file,
#' # and which name is "model.lp"
#' a <- DOcplexcloudAttachment(file = "/home/joe/models/model.lp")
#'
#' # Creates an attachment which content is the specified file, and
#' # which name is "model.lp"
#' a <- DOcplexcloudAttachment(name = "model.lp",
#'                             file = "/home/joe/models/model_1231.lp")
#'
#' # create an attachment which data is stored in memory
#' model <- "Minimize
#'             obj: x + y
#'           Subject To
#'           Bounds
#'             3 <= x <= 17
#'             2 <= y
#'           End"
#' a <- DOcplexcloudAttachment(name="model.lp",
#'                             data=charToRaw(model))
#' @keywords internal
#' @export
DOcplexcloudAttachment <- function(name = NULL, file = NULL, data = NULL) {
    if (is.null(file) && is.null(name)) {
        stop("Cannot attach data without attachment name.")
    }
    if (is.null(name)) {
        name = basename(file)
    }
    me <- list(name = name,
               file = file,
               data = data)
    class(me) <- append(class(me), "DOcplexcloudAttachment")
    return(me)
}

#' Creates a new attachment.
#'
#' @param name The name of the attachment. If a \code{file} is specified,
#'   the name can be ommited and the basename of the \code{file} is used.
#' @param file The name of the file containing the data for the attachment.
#' @param data The raw data for the attachment.
#' @return an instance of DOcplexcloudAttachment with the specified attachment data.
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
    return(DOcplexcloudAttachment(name=name, file=file, data=data))
}


#' Returns a name for the attachment.
#'
#' The name of the attachment is the \code{name} field of the attachement.
#' If that field is empty or NULL, then the name is the basename of
#' \code{attachment$file}
#'
#' @param attachment The attachment
#' @return A name for the attachment.
#' @export
getAttachmentName <- function(attachment) {
    UseMethod("getAttachmentName", attachment)
}

#' @export
getAttachmentName.DOcplexcloudAttachment=function(attachment)
{
    name <- attachment$name
    if (is.null(attachment$name) || attachment$name == "") {
        name <- basename(attachment$file)
    }
    return(name)
}


