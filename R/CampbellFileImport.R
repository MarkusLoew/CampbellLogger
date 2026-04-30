#' import a Campbell logger *.dat file
#' @param file filename of the dat file
#' @param time.zone Time zone, defaults to "Australia/Melbourne"
#' @param checkduplicates Check for and remove duplicates from the file. Logical, defaults to TRUE.
#' @param skip.rows Number of rows of the start of the file to discard. This si to speed up data import by reducing number of samples. Numeric, defaults to NA.
#' @return data frame with imported *.dat file
#' @export

CampbellFileImport <- function(file, 
                               time.zone = "UTC", 
                               checkduplicates = TRUE,
                               skip.rows = NA) {
  # require(readr) # faster import, but problems whith coding "NA" 
  # imports a single Campbell file as per given filename
  # converts the TIMESTAMP to POSIXct, given the provided time zone
  # returns a data frame
  
  # Read header efficiently with readLines instead of parsing entire rows
  header_lines <- readLines(file, n = 4)
  my.header <- strsplit(header_lines[2], ",", fixed = TRUE)[[1]]
  my.header <- gsub('^"|"$', "", my.header)  # remove leading/trailing quotes
  my.header <- make.names(my.header)          # sanitise names, replacing invalid chars with "."

  # only import last rows as defined by parameter lrows
  # number of rows to skip for the actual file import
  to.skip <- 4
  if (!is.na(skip.rows)) {
     base.skip <- 4
     to.skip <- base.skip + skip.rows
  }

  # Use readr for faster import (5-10x faster than read.csv)
  df <- readr::read_csv(
    file,
    skip = to.skip,
    col_names = FALSE,
    na = c("NAN", "+INF", "-INF"),
    show_col_types = FALSE
  )

  names(df) <- my.header
  names(df) <- gsub("\\.", "_", names(df))
  df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, tz = time.zone)
  
  # get rid of rows without TIMESTAMP - not sure why these exist
  df <- df[!is.na(df$TIMESTAMP), ]
  
  # get rid of duplicate samples - this removes the Record label from the entries
  # Campbell SCientific loggers can produce duplicate measurement entries with different record numbers  
  if (isTRUE(checkduplicates)) {
    df$RECORD <- 0
    df <- df[!duplicated(df), ]
  }
  # despite using readr, still have the outut as data.frame
  df <- as.data.frame(df)
  
  return(df)
}
