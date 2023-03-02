#' Title
#'
#' @param userpwd 
#' @param ifn 
#' @param type 
#' @param region 
#'
#' @return
#' @export
#'
#' @examples
fetch_ifn <- function(ifn = 2, type = "trees", region = "spain", userpwd = NULL) {

  ifn <- match.arg(as.character(ifn), c(2, 3))
  type <- match.arg(tolower(type), c("saplings", "trees"))
  region <- match.arg(tolower(region), c("spain", "catalonia"))
  
  if (is.null(userpwd)) stop("Please provide username and password with the format user:pass")
  
  # Set the handle with the user and the pass
  library(curl)
  emf_handle <- new_handle(
    use_ssl = TRUE,
    userpwd = userpwd
  )
  
  # Subdirectories.
  a <- "ftp://data-emf.creaf.cat:22111/emf/datasets/ForestInventories/IFN/Products/IFN/"
  
  a <- paste0(a, ifelse(ifn == 2, "IFN2/", "IFN3/"))
  
  # Data type.
  if (ifn == 2) {
    b <- ifelse(type == "saplings", "piesMenoresDataIFN2", "piesMayoresDataIFN2")
    b <- paste0(b, ifelse(region == "catalonia", "_Catalunya.csv", "_Spain.csv"))
  } else if (ifn == 3) {
    b <- ifelse(type == "saplings", "regDataIFN3", "treeDataIFN3")
    b <- paste0(b, ifelse(region == "catalonia", "_Catalunya.csv", "_Spain.csv"))
  }
  
  # h <- curl::new_handle()
  # curl::handle_setopt(
  #   handle = h,
  #   httpauth = 1,
  #   userpwd = "rmolowny:AlurPalma1988"
  # )
  # a <- "ftp://data-emf.creaf.cat:22111/rmolowny/prueba.csv"
  # kk<-read.csv(curl(url = a, handle = h))
  # browser()

  # Fetch data.
  cat("\n-> Retrieving ", b, " from data server...\n")
  dat <- read.csv(curl::curl(url = paste0(a,b), handle = emf_handle))
  cat("\n\n")
  
  return()
  
  
}