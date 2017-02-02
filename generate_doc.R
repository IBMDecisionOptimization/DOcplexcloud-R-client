#
# script used to generate html docs using pkgdown
#
if (!require(pkgdown)) {
  if(!require(devtools)){
    install.packages("devtools",
                     dependencies=TRUE,
                     repos="http://cran.us.r-project.org")
  }
  library("devtools")
  devtools::install_github('hadley/pkgdown')
  # needed too
  install.packages('rjson')
}


pkgdown::build_site()
