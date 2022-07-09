#' Load and preprocess
#'
#' Loads and preprocesses data from the specified input path.
#'
#' @import data.table
#' @import stringr
#' @import readr
#'
#' @param DATA An input data path.
#' @param filename The name of the file to be generated.
#' @param convert_from_epoch_date Should the data be converted to POSIXct from Epoch/UNIX timestamp? Defaults to TRUE.
#' @param scale_iv Should IV be scaled down? Do this when IV values are e.g. 80 instead of 0.8. Defaults to FALSE.
#'
#' @return Outputs preprocessed data.table.

#' @export
load_and_preprocess <- function(DATA, filename, convert_from_epoch_date = TRUE, scale_iv = FALSE){

  file_path <- paste("./data/",filename,".csv",sep = "")

  if (exists(filename)) {
    stop("File already in environment. Use rm('filename') if this is not the correct file.")
  } else if (file.exists(file_path)) {
    print(paste("File found in '", file_path, "' loading the file now!", sep = ""))

    DT_tmp <- fread(file_path)
    return(DT_tmp)
  }


  DT_tmp <- fread(DATA)[,c("timestamp",
                           "underlying_index",
                           "underlying_price",
                           "last_price",
                           "instrument_name",
                           "index_price",
                           "interest_rate",
                           "mark_iv")]

  if (scale_iv) {
    DT_tmp[,"mark_iv" := mark_iv / 100]
  }

  if (convert_from_epoch_date) {
    # when downloaded from online source (deactivate by convert_from_epoch_date = FALSE)
    if (!(DT_tmp[,class(timestamp)] %in% "POSIXct" || DT_tmp[,class(timestamp)] %in% "POSIXt")) {
      DT_tmp[, timestamp := lubridate::as_datetime(timestamp/1e3,  tz = "UTC")]
    }
  }

  if (!("date" %in% names(DT_tmp)) || !("hour" %in% names(DT_tmp)) || !("minute" %in% names(DT_tmp)))
  {
    DT_tmp[, `:=`(date = as.Date(timestamp),
                  hour = hour(timestamp),
                  minute = minute(timestamp))]
  }

  if (!("strike" %in% names(DT_tmp))) {
    DT_tmp[, "strike" := abs(readr::parse_number(substring(instrument_name, 12)))]
  }

  if (!("maturity" %in% names(DT_tmp))) {
    DT_tmp[, "maturity" := readr::parse_date(stringr::str_match(instrument_name, "-\\s*(.*?)\\s*-")[,2], format = "%d%b%y")]
  }

  if (!("maturity_timestamp" %in% names(DT_tmp))) {
    DT_tmp[, "maturity_timestamp" := lubridate::ymd_hms(paste(maturity, "08:00:00"))]
  }

  if (!("moneyness" %in% names(DT_tmp))) {
    DT_tmp[, "moneyness" := strike / index_price]
  }

  if (!("log_moneyness" %in% names(DT_tmp))) {
    DT_tmp[, "log_moneyness" := log(moneyness)]
  }

  if (!("date_to_maturity" %in% names(DT_tmp))) {
    DT_tmp[, "date_to_maturity" := as.numeric(make_difftime(maturity_timestamp - timestamp, units = "day"))]
  }

  if (!("put_option" %in% names(DT_tmp))) {
    DT_tmp[, "put_option" := stringr::str_sub(instrument_name, start = -1)]
    DT_tmp[, "put_option" := as.numeric(put_option == "P")]
  }

  setkey(DT_tmp, timestamp)
  return(DT_tmp)
}
