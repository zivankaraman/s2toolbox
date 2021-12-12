#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param cal PARAM_DESCRIPTION
#' @param fill PARAM_DESCRIPTION, Default: TRUE
#' @param title PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[ggplot2]{scale_colour_discrete}},\code{\link[ggplot2]{facet_wrap}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{theme}},\code{\link[ggplot2]{margin}}
#' @export 
#' @source \url{http://somewhere.important.com/}
#' @importFrom ggplot2 scale_fill_discrete facet_wrap ggtitle theme element_text
PlotAcquisitionCalendar <-
function(cal, fill = TRUE, title)
{
  # namespaceImportFrom(.GlobalEnv, vars = ":=", ns = "data.table")
  cal <- cal[, c("Date", "Mission")]
  if (fill) {
    first <- as.Date(paste0(format(min(cal$Date), "%Y-%m"), "-01"))
    last <- (seq(as.Date(paste0(format(max(cal$Date), "%Y-%m"), "-01")), length = 2, by = "months") - 1)[2]
    days <- seq(first, last,  by = "days")
    dat <- data.frame(Date = days)
    cal <- merge.data.frame(dat, cal, all.x = TRUE)
  }
  cal$Year <- format(cal$Date, "%Y")

  p <- calendar_heatmap(cal, "Date", "Mission", vcGroupingColumnNames = "Year")
  p <- p + ggplot2::scale_fill_discrete(na.translate = FALSE, na.value = "white") +
      ggplot2::facet_wrap(~Year, ncol = 1)
  if (!missing(title))
    p <- p + ggplot2::ggtitle(title) + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  return(p)
}



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param dtDateValue PARAM_DESCRIPTION
#' @param cDateColumnName PARAM_DESCRIPTION, Default: ''
#' @param cValueColumnName PARAM_DESCRIPTION, Default: ''
#' @param vcGroupingColumnNames PARAM_DESCRIPTION, Default: 'Year'
#' @param dayBorderSize PARAM_DESCRIPTION, Default: 0.25
#' @param dayBorderColour PARAM_DESCRIPTION, Default: 'black'
#' @param monthBorderSize PARAM_DESCRIPTION, Default: 2
#' @param monthBorderColour PARAM_DESCRIPTION, Default: 'black'
#' @param monthBorderLineEnd PARAM_DESCRIPTION, Default: 'round'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[data.table]{copy}},\code{\link[data.table]{data.table-package}},\code{\link[data.table]{setattr}},\code{\link[data.table]{setkey}}
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_raster}},\code{\link[ggplot2]{aes_}},\code{\link[ggplot2]{coord_fixed}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{facet_wrap}},\code{\link[ggplot2]{geom_segment}},\code{\link[ggplot2]{scale_continuous}}
#'  \code{\link[lubridate]{day}}
#' @export 
#' @source \url{http://somewhere.important.com/}
#' @importFrom data.table copy data.table setnames setkeyv
#' @importFrom ggplot2 ggplot aes geom_tile aes_string coord_fixed xlab ylab facet_wrap geom_segment scale_x_continuous scale_y_continuous
#' @importFrom lubridate wday
calendar_heatmap <-
function (dtDateValue, cDateColumnName = "", cValueColumnName = "",
          vcGroupingColumnNames = "Year", dayBorderSize = 0.25, dayBorderColour = "black",
          monthBorderSize = 2, monthBorderColour = "black", monthBorderLineEnd = "round")
{
  Year <- ""
  MonthOfYear <- ""
  WeekOfYear <- ""
  DayOfWeek <- ""
  as.formula <- ""
  MonthChange <- ""
  meanWeekOfYear <- ""
  dtDateValue <- data.table::copy(data.table::data.table(dtDateValue))
  dtDateValue[, `:=`(Year, as.integer(strftime(get(cDateColumnName),
                                               "%Y")))]
  vcGroupingColumnNames <- unique(c(vcGroupingColumnNames,
                                    "Year"))
  dtDateValue <- merge(dtDateValue, data.table::setnames(dtDateValue[,
                                                                     list(DateCol = seq(min(get(cDateColumnName)), max(get(cDateColumnName)),
                                                                                        "days")), vcGroupingColumnNames], "DateCol", cDateColumnName),
                       c(vcGroupingColumnNames, cDateColumnName), all = T)
  dtDateValue[, `:=`(MonthOfYear, as.integer(strftime(get(cDateColumnName),
                                                      "%m")))]
  dtDateValue[, `:=`(WeekOfYear, 1 + as.integer(strftime(get(cDateColumnName),
                                                         "%W")))]
  dtDateValue[, `:=`(DayOfWeek, as.integer(strftime(get(cDateColumnName),
                                                    "%w")))]
  dtDateValue[DayOfWeek == 0L, `:=`(DayOfWeek, 7L)]
  ggplotcalendar_heatmap <- ggplot2::ggplot(data = dtDateValue[, list(WeekOfYear,
                                                                      DayOfWeek)], ggplot2::aes(x = WeekOfYear, y = DayOfWeek)) +
    ggplot2::geom_tile(data = dtDateValue, ggplot2::aes_string(fill = cValueColumnName), color = dayBorderColour, size = dayBorderSize) +
    ggplot2::coord_fixed() + ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::facet_wrap(as.formula(paste("~", paste(vcGroupingColumnNames, collapse = "+"))))
  data.table::setkeyv(dtDateValue, c(vcGroupingColumnNames, "DayOfWeek",
                                     "WeekOfYear", "MonthOfYear"))
  dtDateValue[, `:=`(MonthChange, c(1, diff(MonthOfYear))),
              c(vcGroupingColumnNames, "DayOfWeek")]
  dtMonthChangeDatasetBetweenWeeks <- dtDateValue[MonthChange ==
                                                    1]
  dtMonthChangeDatasetBetweenWeeks[, `:=`(WeekOfYear, WeekOfYear -
                                            0.5)]
  dtMonthChangeDatasetBetweenWeeks <- rbind(dtMonthChangeDatasetBetweenWeeks[, c("DayOfWeek", "WeekOfYear", vcGroupingColumnNames), with = F],
                                            dtDateValue[, list(WeekOfYear = 0.5 + max(WeekOfYear)), c(vcGroupingColumnNames, "DayOfWeek")])
  if (nrow(dtMonthChangeDatasetBetweenWeeks) > 0) {
    ggplotcalendar_heatmap <- ggplotcalendar_heatmap +
      ggplot2::geom_segment(data = dtMonthChangeDatasetBetweenWeeks,
                            ggplot2::aes(x = WeekOfYear, xend = WeekOfYear, y = DayOfWeek - 0.5, yend = DayOfWeek + 0.5),
                            size = monthBorderSize, colour = monthBorderColour, lineend = monthBorderLineEnd)
  }
  data.table::setkeyv(dtDateValue, c(vcGroupingColumnNames, "WeekOfYear",
                                     "DayOfWeek", "MonthOfYear"))
  dtDateValue[, `:=`(MonthChange, c(1, diff(MonthOfYear))),
              vcGroupingColumnNames]
  MonthChangeDatasetWithinWeek <- dtDateValue[MonthChange ==
                                                1 & (DayOfWeek != 1)]
  MonthChangeDatasetWithinWeek[, `:=`(DayOfWeek, DayOfWeek -
                                        0.5)]
  MonthChangeDatasetWithinWeek <- rbind(MonthChangeDatasetWithinWeek[, c("DayOfWeek", "WeekOfYear", vcGroupingColumnNames), with = F],
                                        dtDateValue[, list(DayOfWeek = c(min(DayOfWeek) - 0.5, max(DayOfWeek) + 0.5)),
                                                    c(vcGroupingColumnNames, "WeekOfYear")])
  if (nrow(MonthChangeDatasetWithinWeek) > 0) {
    ggplotcalendar_heatmap <- ggplotcalendar_heatmap +
      ggplot2::geom_segment(data = MonthChangeDatasetWithinWeek,
                            ggplot2::aes(x = WeekOfYear - 0.5, xend = WeekOfYear + 0.5, y = DayOfWeek, yend = DayOfWeek),
                            size = monthBorderSize, colour = monthBorderColour, lineend = monthBorderLineEnd)
  }
  dtMonthLabels <- dtDateValue[, list(meanWeekOfYear = mean(WeekOfYear)),
                               by = c("MonthOfYear")]
  month.abb <- gsub("\\.", "", format(ISOdate(1970, 1:12, 1), "%b"))
  wday.abb <- levels(lubridate::wday(ISOdate(1970, 1, 1:7), week_start = 1, label = TRUE))
  wday.abb <- gsub("\\.", "", gsub('\\\\', "", wday.abb))
  dtMonthLabels[, `:=`(MonthOfYear, month.abb[MonthOfYear])]
  ggplotcalendar_heatmap <- ggplotcalendar_heatmap +
    ggplot2::scale_x_continuous(breaks = dtMonthLabels[, meanWeekOfYear], labels = dtMonthLabels[, MonthOfYear], expand = c(0, 0)) +
    ggplot2::scale_y_continuous(trans = "reverse", breaks = c(1:7), labels = wday.abb, expand = c(0, 0))
  return(ggplotcalendar_heatmap)
}

