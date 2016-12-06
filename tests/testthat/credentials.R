
TestClient <- function() {
    baseUrl <- Sys.getenv("DOCPLEXCLOUD_URL")
    apiKey <- Sys.getenv("DOCPLEXCLOUD_KEY")
    if (is.null(baseUrl) || baseUrl == "" || is.null(apiKey) || apiKey == "")
        skip("No URL")
    return(DOcplexcloudClient(verbose = TRUE))
}
