#' Make and save test data
#'
#' Wrapper for load_and_preprocess function that also saves the data on the local machine.
#'
#' @import data.table
#'
#' @param PATH An input data path.
#' @param filename The name of the file that will be saved.
#' @param convert_from_epoch_date Should the data be converted to POSIXct from Epoch/UNIX timestamp? Defaults to TRUE.
#' @param scale_iv Should IV be scaled down? Do this when IV values are e.g. 80 instead of 0.8. Defaults to FALSE.
#'
#' @return Outputs pre-processed data.table.

#' @export
make_and_save_test_data <- function(PATH, filename, convert_from_epoch_date = TRUE, scale_iv = FALSE){
  DT_tmp <- load_and_preprocess(PATH, filename = filename, convert_from_epoch_date = convert_from_epoch_date, scale_iv = scale_iv)

  file_path <- paste("./data/",filename,".csv",sep = "")
  save_file <- !file.exists(file_path)

  if (save_file) {

    if (!dir.exists("./data")) {
      print("Created directory './data'")
      dir.create("./data")
    }

    print("Saving file to local machine")
    fwrite(DT_tmp, file_path)
  }

  return(DT_tmp)
}
