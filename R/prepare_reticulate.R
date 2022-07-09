#' #' Prepare reticulate
#' #'
#' #' @import reticulate
#' #'
#'
#' ## initiation file ##
#' libraries_py <- list("pandas", "numpy", "scipy")
#' ## ##
#'
# ## create conda env ##
# if (!("r-conda-ICC" %in% reticulate::conda_list()$name)) {
#   reticulate::conda_create("r-conda-ICC")
#   reticulate::conda_install(envname = "r-conda-ICC", packages = libraries_py)
# }
# ## use env ##
# reticulate::use_condaenv("r-conda-ICC")
#'
#' ## load python functions ##
#' reticulate::source_python("./python/MFCF_new/ICC.py")
#' np <- reticulate::import("numpy", convert = FALSE)
#' ## ##
