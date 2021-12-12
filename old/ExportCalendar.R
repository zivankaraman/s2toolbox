#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param cal PARAM_DESCRIPTION
#' @param utc PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[calendar]{ic_event}}
#' @export 
#' @source \url{http://somewhere.important.com/}
#' @importFrom calendar ic_event
MakeCalendar <-
function(cal, utc = FALSE)
{
    N <- nrow(cal)
    AbsOrbit <- as.integer(sapply(strsplit(cal$ID, "-", fixed = TRUE), "[[", 1))
    if (utc) {
        start <- cal$ObservationStartUTC
        end <- cal$ObservationStopUTC
    } else {
        start <- cal$ObservationStartLocal
        end <- cal$ObservationStopLocal
    }

    desc <- with(cal, sprintf("Satellite %s / Absolute Orbit A%6.6d / Relative Orbit R%4.4d",
                              Mission, AbsOrbit, as.integer(RelativeOrbit)))
    tiles <- ""
    loc <- ""
    if (!is.null(attr(cal, "Tiles")))
        tiles <- paste("Tiles:", paste(attr(cal, "Tiles"), collapse = " / "))
    if (!is.null(attr(cal, "Position")))
        loc <- attr(cal, "Position")
    lst <- vector("list", N)
    for (k in 1:N) {
        lst[[k]] <- calendar::ic_event(
            start_time = start[k],
            end_time = end[k],
            summary = cal$Mission[k])
    }
    out <- do.call(rbind, lst)
    out[, "DESCRIPTION"] <- paste(desc, tiles, sep = " - ")
    out[, "LOCATION"] <- loc
    return(out)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param cal PARAM_DESCRIPTION
#' @param fileName PARAM_DESCRIPTION, Default: 'Sentinel2.ics'
#' @param utc PARAM_DESCRIPTION, Default: FALSE
#' @param strip_time PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export 
#' @source \url{http://somewhere.important.com/}
ExportCalendar <-
function(cal, fileName = "Sentinel2.ics", utc = FALSE, strip_time = FALSE)
{
    ic <- MakeCalendar(cal, utc = utc)
    con <- file(fileName, encoding = "UTF-8")
    cal_write(ic, con, strip_time = strip_time)
    close(con)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param cal PARAM_DESCRIPTION
#' @param file PARAM_DESCRIPTION, Default: 'Sentinel2.ics'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export 
#' @source \url{http://somewhere.important.com/}
ExportCalendar2 <-
function(cal, file = "Sentinel2.ics")
{
    N <- nrow(cal)
    desc <- with(cal, sprintf("Satellite %s / Absolute Orbit A%6.6d / Relative Orbit R%4.4d",
                              Mission, AbsOrbit, RelOrbit))
    loc <- paste(attr(cal, "Tiles"), collapse = " / ")
    lst <- vector("list", N)
    for (k in 1:N) {
        lst[[k]] <- ic_event(
                    start_time = cal$Begin.Locale[k],
                    end_time = cal$End.Locale[k],
                    summary = cal$Mission[k])
    }
    out <- do.call(rbind, lst)
    out[, "DESCRIPTION"] <- desc
    out[, "LOCATION"] <- loc
    ic_write(out, file)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param cal PARAM_DESCRIPTION
#' @param file PARAM_DESCRIPTION, Default: 'Sentinel2.ics'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export 
#' @source \url{http://somewhere.important.com/}
ExportCalendar1 <-
    function(cal, file = "Sentinel2.ics")
    {
        N <- nrow(cal)
        lst <- vector("list", N)
        for (k in 1:N) {
            lst[[k]] <- ic_event(start_time = cal$Begin.Locale[k],
                                 end_time = cal$End.Locale[k],
                                 summary = paste(cal$Mission[k], cal$AbsOrbit[k], cal$RelOrbit[k]))
        }
        out <- do.call(rbind, lst)
        ic_write(out, file)
    }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ic PARAM_DESCRIPTION
#' @param file PARAM_DESCRIPTION
#' @param strip_time PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export 
#' @source \url{http://somewhere.important.com/}
cal_write <- function (ic, file, strip_time = FALSE)
{
    ic_char <- cal_character(ic, strip_time = strip_time)
    if (strip_time) {
        ic_char <- ic_char[-grep("DTEND", ic_char)]
    }
    writeLines(ic_char, file)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ic PARAM_DESCRIPTION
#' @param strip_time PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export 
#' @source \url{http://somewhere.important.com/}
cal_character <- function(ic, strip_time = FALSE)
{
    char_attributes <- paste(names(attributes(ic)$ical), attributes(ic)$ical,
                             sep = ":")
    char_events <- cal_char_event(ic, strip_time = strip_time)
    c(char_attributes, char_events, "END:VCALENDAR")
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ic PARAM_DESCRIPTION
#' @param strip_time PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export 
#' @source \url{http://somewhere.important.com/}
cal_char_event <- function(ic, strip_time = FALSE)
{
    date_cols <- grepl(pattern = "VALUE=DATE", x = names(ic))
    if (any(date_cols)) {
        ic[date_cols] <- lapply(ic[, date_cols], ic_char_date)
    }
    datetime_cols <- names(ic) %in% c("DTSTART", "DTEND")
    if (any(datetime_cols)) {
        ic[datetime_cols] <- lapply(ic[, datetime_cols], cal_char_datetime, strip_time = strip_time)
    }
    char_names <- c(rep(c("BEGIN", names(ic), "END"), nrow(ic)))
    char_contents <- apply(ic, 1, function(x) c("VEVENT", as.character(x),
                                                "VEVENT"))
    paste(char_names, char_contents, sep = ":")
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param zulu PARAM_DESCRIPTION, Default: FALSE
#' @param strip_time PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export 
#' @source \url{http://somewhere.important.com/}
cal_char_datetime <- function (x, zulu = FALSE, strip_time = FALSE)
{
    yr <- format.POSIXct(x, "%Y%m%d")
    if (strip_time) {
        return(yr)
    } else {
        ti <- format.POSIXct(x, "%H%M%S")
        yr_ti <- paste0(yr, "T", ti)
        if (zulu) {
            yr_ti <- paste0(yr_ti, "Z")
        }
        return(yr_ti)
    }
}
