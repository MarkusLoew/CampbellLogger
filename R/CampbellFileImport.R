#' import a Campbell Scientific data logger *.dat (TOA5) file
#' @param file filename of the dat (TOA5) file
#' @param time.zone Time zone, defaults to "UTC"
#' @param checkduplicates Check for and remove duplicate measurements from the file. Logical, defaults to TRUE. A quick pre-check on TIMESTAMP only is done initially. The full deduplication is skipped if no duplicate timestamps exist.
#' @param importunits Import the units row (row 3 of the .dat file) as a separate data frame. Logical, defaults to FALSE. When TRUE, the function returns a named list with elements \code{data} (the measurements data frame) and \code{units} (a single-row data frame of units for each column).
#' @return When \code{importunits = FALSE} (default), a data frame with the imported measurements. When \code{importunits = TRUE}, a named list with two data frames: \code{data} (measurements) and \code{units} (column units).
#' @export

CampbellFileImport <- function(file, 
                               time.zone = "UTC", 
                               checkduplicates = TRUE,
                               skip.rows = NA,
                               importunits = FALSE) {
  # require(readr) # faster import, but problems whith coding "NA" 
  # imports a single Campbell file as per given filename
  # converts the TIMESTAMP to POSIXct, given the provided time zone
  # returns a data frame
  
  # Read header efficiently with readLines instead of parsing entire rows
  header_lines <- readLines(file, n = 4)
  my.header <- strsplit(header_lines[2], ",", fixed = TRUE)[[1]]
  my.header <- gsub('^"|"$', "", my.header)  # remove leading/trailing quotes
  my.header <- make.names(my.header)         # sanitise names, replacing invalid chars with "." similar to read.csv()
  # Parse units from row 3 of the .dat file
  my.units <- strsplit(header_lines[3], ",", fixed = TRUE)[[1]]
  my.units <- gsub('^"|"$', "", my.units)  # remove leading/trailing quotes
  units.df <- as.data.frame(t(my.units), stringsAsFactors = FALSE)
  names(units.df) <- gsub("\\.", "_", my.header)

  # only import last rows as defined by parameter lrows
  # number of rows to skip for the actual file import
  to.skip <- 4
  if (!is.na(skip.rows)) {
     base.skip <- 4
     to.skip <- base.skip + skip.rows
  }

  # Use readr instead of read.csv() for faster import
  df <- readr::read_csv(
    file,
    skip = to.skip,
    col_names = FALSE,
    na = c("NAN", "+INF", "-INF"),
    show_col_types = FALSE
  )

  df <- as.data.frame(df) # convert to data frame instead of tibble
  
  names(df) <- my.header
  names(df) <- gsub("\\.", "_", names(df))
  df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, tz = time.zone)
  
  # get rid of rows without TIMESTAMP - not sure why these exist
  df <- df[!is.na(df$TIMESTAMP), ]
  
  # get rid of duplicate samples - this removes the Record label from the entries temporarily to check for duplicates, then puts the record number back in the data frame for the non-duplicated entries
  # Campbell Scientific loggers can produce duplicate measurement entries with different record numbers  
    if (isTRUE(checkduplicates)) {
    # Quick pre-check on TIMESTAMP only; skip full dedup if no duplicate timestamps exist
    if (anyDuplicated(df$TIMESTAMP) > 0) {
      records <- df$RECORD # keep records numbers
      dedup.df <- df
      dedup.df$RECORD <- 0
      not.duplicated <- !duplicated(dedup.df)

      # remove duplicates
      df <- df[not.duplicated, ]
      # put record number for the non-duplicated entries back in the data frame
      df$RECORD <- records[not.duplicated]
      warning(sum(!not.duplicated), " duplicate(s) found and removed from ", basename(file))
    }
  }
  
  if (isTRUE(importunits)) {
    return(list(data = df, units = units.df))
  }
  return(df)
}