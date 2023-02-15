#' Nonlinear regression for tree growth
#'
#' @description
#' It applies a non-linear regression scheme to determine tree diameter growth as a function
#' of a set of predictors.
#'
#' @param df_trees \code{data.frame} containing the sizes of every tree at two different
#' time points. See \code{Details} below for more information.
#' @param df_pred 
#' @param species character vector containing the species for which a regression is sought.
#' @param form_expected formula with the expression 
#' @param form_variance 
#' @param country character, origin of the data set. Only \code{"spain"} has been implemented.
#' It can be given in lower or upper case letters, or a mixture of the two.
#'
#' @details 
#' 
#' If the input data sets correspond to the Spanish Inventario Forestal Nacional,
#' the columns in \code{df_trees} must be named \code{"dbh"}, \code{"dbh1"},
#' \code{"species"} and \code{"tdiff"}, corresponding respectively to first and second
#' tree diameter, tree species and time difference (in years strictly positive) between the first and the
#' second time points. 
#'
#' @return
#' A \code{list} whose elements correspond to each \code{species}. Each element, in turn, includes
#' two regression objects for the expected growth and its variance.
#' 
#' @export
#'
#' @examples
reg_fpm_growth <- function(df_trees, df_pred, form_expected, form_variance, species, country) {
  
  # General checks.
  if (!is.data.frame(df_trees)) stop("'df_trees' must be a data.frame")
  if (!is.data.frame(df_pred)) stop("'df_pred' must be a data.frame")
  if (class(form_expected) != "formula") stop("Class of 'form_expected' input must be 'formula'")
  if (class(form_variance) != "formula") stop("Class of 'form_variance' input must be 'formula'")
  if (!all(all.vars(form_expected)[-1] %in% colnames(df_pred)))
    stop("At least one predictor from 'form_expected' is missing in 'df_pred'")
  if (!all(all.vars(form_variance)[-1] %in% colnames(df_pred)))
    stop("At least one predictor from 'form_variance' is missing in 'df_pred'")
  
  country <- match.arg(tolower(country), c("spain"))
  
  # Spanish Inventario Forestal Nacional.
  if (tolower(country) == "spain") {
    
    # Checks.
    if (c("dbh", "dbh1", "species", "tdiff") %in% colnames(df_trees)) 
      stop("Wrong column names in 'df_trees'")
    
    all_species <- unique(df_trees$species)
    if (!(species %in% all_species)) stop("'species' cannot be found in 'df_trees'")
    
    if (any(df$dbh1 > df$dbh2)) stop("Variable 'dbh1' cannot be larger than 'dbh2'")
    
    # Non-linear regressions for expected growth.
    reg_expected <- list()
    for (i in species) {
      reg[[i]] <- 1
    }

    # Linear regression for variance.
    reg_variance <- list()
    for (i in species) {
      reg[[i]] <- 1
    }
  }
  
  return(list(expected = reg_expected, variance = reg_variance))
}