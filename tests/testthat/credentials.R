
TestClient <- function() {
    if (is.null(baseUrl) || baseUrl == "" || is.null(apiKey) || apiKey == "")
        skip("No URL")
    return(DOcplexcloudClient(verbose = TRUE))
}
