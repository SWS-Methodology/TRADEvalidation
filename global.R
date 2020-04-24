.libPaths(c(.libPaths(), '/work/SWS_R_Share/shiny/Rlib/3.1', '/usr/local/lib64/R-3.1.2/library', '/home/mongeau/R/x86_64-unknown-linux-gnu-library/3.1'))

library(data.table)
library(shiny)
library(faosws)
library(faoswsUtil)
library(ggplot2)
library(faoswsFlag)
library(rhandsontable)
library(plotly)

SERVER <- "https://hqlqasws1.hq.un.fao.org:8181/sws"

DEFAULT_THRESHOLD <- 1000L

DOMAIN      <- "trade"
DATASET_TOT <- "total_trade_cpc_m49"
DATASET_BIL <- "completed_tf_cpc_m49"

CIFFOB <- 1.12

MAX_YEAR <- as.numeric(format(Sys.Date(), "%Y")) - 1

FILES_LOCATION <- paste0(normalizePath("./files/"), "/")

TYPES_CORRECTION <- c(
  'None',
  'Measurement factor',
  'Mirror flow',
  'Outlier correction',
  'Publication/website',
  'Expert knowledge'
)

REMOTE <- 
  "HOSTNAME" %in% names(Sys.getenv()) &&
    grepl("hq.un.fao.org", Sys.getenv()[['HOSTNAME']])

if (REMOTE == TRUE) {
  # on server
  #CONFIG_CERTIFICATES <- "/srv/shiny-server/PRODvalidation/files/certificates/qa"
  CONFIG_CERTIFICATES <- "/srv/shiny-server/.R/QA"
  CORRECTIONS_DIR <- "/work/SWS_R_Share/trade/validation_tool_files/new_tool_test"
  TRADEMAP_DIR <- "/work/SWS_R_Share/trade/validation_tool_files/rawtrade"
} else {
  # local
  #CONFIG_CERTIFICATES <- "C:/Users/Mongeau/Documents/certificates/qa"
  CONFIG_CERTIFICATES <- "C:/Users/chr/Documents/certificates/qa"
  CORRECTIONS_DIR <- "d:/trade/validation_tool_files"
  TRADEMAP_DIR <- "d:/" ### XXX: nothing here
}

publication_sources <-
  as.data.table(
    matrix(
      c(
        'National publication/website', '', 'p',
        'UNCOMTRADE official', '', 'p',
        'UNCOMTRADE estimation', 'X', 'p',
        'Trademap official', '', 'p',
        'Trademap mirror data', 'T', 'p',
        'Other international/regional publication/website', 'T', 'p'
      ),
      ncol = 3,
      byrow = TRUE
    )
  )

names(publication_sources) <- c("source", "flag_obs", "flag_method")

bilateral_plot <- function(data, title = NULL, xlab = "Year", ylab = NULL) {
  ggplot(data, aes(x = timePointYears, y = value, group = variable)) +
    geom_line(aes(color = factor(variable, levels = c("Value", "original", "mirror", "movav", "median", "median_world"))), size = 1) +
    geom_point(aes(color = factor(variable, levels = c("Value", "original", "mirror", "movav", "median", "median_world"))), size = 4) +
    geom_ribbon(aes(ymin = thresh_min, ymax = thresh_max), alpha = 0.2) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    labs(title = title, x = xlab, y = ylab)
}

show_corrections_table <- function(data, codelists) {
  d <- data[, .(partner, year, item, flow, data_original, data_type, correction_input, correction_type, correction_note, note_analyst, name_analyst, date_correction)]

  d[, id := 1:nrow(d)]

  d <- merge(d, codelists$countries, by.x = "partner", by.y = "code", all.x = TRUE)
  d[, partner := paste(partner, " - ", description)]
  d[nchar(partner) > 30, partner := paste0(substr(partner, 1, 27), "...")]
  d[, description := NULL]

  d <- merge(d, codelists$items, by.x = "item", by.y = "code", all.x = TRUE)
  d[, item := paste(item, " - ", description)]
  d[nchar(item) > 40, item := paste0(substr(item, 1, 37), "...")]
  d[, description := NULL]

  d[, flow := as.character(flow)]
  d[flow == "1", flow := "import"]
  d[flow == "2", flow := "export"]

  d[, date_correction := as.Date(substr(date_correction, 1, 10))]

  setorderv(d, "id")

  d[, id := NULL]

  d <- DT::datatable(d, selection = 'single', rownames = FALSE, filter = 'top', options = list(pageLength = 50, dom = 'ptip', stateSave = TRUE, serverSide = TRUE))
  d <- DT::formatCurrency(d, c("data_original", "correction_input"), digits = 1, currency = "")

  return(d)
}

# Like saveRDS, setting permissions.
# (this is because at some point the tool was saving the file
# with wrong permissions, so now set irrespespective of umask)
save_rds <- function(data, file) {
  saveRDS(data, file)
  Sys.chmod(file, mode = '0666', use_umask = FALSE)
  # TODO: return TRUE or FALSE if saved or not, respectively
}

fmtnum <- function(x, digits = 1) {
  if (all(is.na(x))) {
    return(x)
  } else {
    res <- scales::comma(round(x, digits))
    res[is.na(x)] <- NA
    return(res)
  }
}

powers <- function(to_check, benchmark) {

  if (is.na(to_check)) stop('Missing unit value.')

  if (is.na(benchmark)) stop('Missing median unit value.')

  ratio <- to_check / benchmark

  ratio10 <- 10^(round(log10(ratio)))

  test <- (ratio >= ratio10 * (100-10)/100) &
          (ratio <= ratio10 * (100+10)/100)

  # Remove 1 from possible factors of 10
  test <- test & ratio10 != 1

  message <- paste0('UV/median = ', round(ratio, 3), '.')

  if (test) {
    message <- paste0(message, 'The most likely factor is ', ratio10, '.')
  } else {
    message <- paste0(message, "It doesn't seem that a factor of 10 should be used.")
  }

  return(message)
}


#####################################################
## Adapted from: https://gitlab.com/snippets/16220 ##
#####################################################
tooltip_well <- function(hover, add_top = 0) {
 if (is.null(hover$y)) {
   NULL
 } else {
   # calculate point position INSIDE the image as percent of total
   # dimensions from left (horizontal) and from top (vertical)
   left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
   top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

   # calculate distance from left and bottom side of the picture in pixels
   left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
   top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top) - 60

   # create style property for tooltip
   # background color is set so tooltip is a bit transparent
   # z-index is set so we are sure are tooltip will be on top
   style <- paste0("position: absolute; z-index: 100; background-color: rgba(245, 245, 245, 0.85); ",
     "left:", left_px + 2, "px; top:", top_px + 2 + add_top, "px; padding: 1px; margin: 1px;")

   # actual tooltip created as wellPanel
   wellPanel(style = style, p(HTML(fmtnum(hover$y))))
 }
}

# Insert TO THE LEFT of position
elem_insert <- function(vec, pos, elem) {
  c(vec[1:(pos-1)], elem, vec[pos:length(vec)])
}

`%+%` <- function(x, y) paste(x, y)

# Add a row with NAs.
add_na_rows <- function(data, split = NA_character_) {
  stopifnot(is.data.table(data))
  stopifnot(!is.na(split))
  stopifnot(all(split %in% names(data)))

  levs <- unique(data[, split, with = FALSE])

  res <- rbindlist(lapply(seq_len(nrow(levs)), function(x) rbind(data[levs[x], on = split], as.data.table(lapply(data[1], function(x) NA)))))

  return(res[-nrow(res)])
}

# Will add an "outlier" column saying whether it is an outlier or not.
# When it enters here, the data should already be ordered.
detect_outliers <- function(data, method = "simple", params = list()) {

  orig_vars <- names(data)

  d <- copy(data)

  # NOTE: "geographicAreaM49" is not here as this works by single country
  setorderv(d, c("measuredItemCPC", "measuredElementTrade", "timePointYears"))

  if (method == "simple") {

    if (!("geographicAreaM49" %in% names(d))) {
      d[, geographicAreaM49 := params$country]
    }

    d <- merge(d, params$elements, by = "measuredElementTrade", all.x = TRUE)

    d[,
      `:=`(
        meanOld     = mean(Value[timePointYears %in% params$interval], na.rm = TRUE),
        growth_rate = Value / shift(Value) - 1
      ),
      by = c("geographicAreaM49", "measuredItemCPC", "measuredElementTrade")
    ]

    d[, flow := substr(measuredElementTrade, 1, 2)]

    d <- merge(d, params$outlier_thresholds, by.x = "geographicAreaM49", by.y = "area", all.x = TRUE)

    d[is.na(threshold), threshold := params$default_threshold]

    # XXX
    d[,
      big_qty := meanOld[grepl("Quantity \\[t\\]", description)] > threshold,
      by = c("geographicAreaM49", "measuredItemCPC", "flow", "timePointYears")
    ]

    d[, ratio := Value / meanOld]

    d[,
      outlier :=
        grepl("Unit Value", description) & # Only UVs
          (!data.table::between(ratio, params$outliers_ratio[1], params$outliers_ratio[2]) | # Carlos'
          !data.table::between(growth_rate, params$outliers_growth[1] / 100, params$outliers_growth[2] / 100)) & # Growth rates
          big_qty == TRUE & # All need to be big quantities in validated years
          timePointYears >= params$min_year
    ]

    d[is.na(outlier), outlier := FALSE]

    return(d[, c(orig_vars, "outlier"), with = FALSE])
  }
}

# Type is a hack so to put the sparkline in a different column:
# https://stackoverflow.com/questions/39154627/two-rhandsontables-in-one-tabpanel-in-shiny-app
add_sparkline <- function(data, type = NA_character_) {
  stopifnot(!is.na(type))
  d <- copy(data) # XXX copy() 

  d[is.na(d)] <- 0

  pos <- grep("^Value_", names(d))[1] - (type == "bilat")

  order_with_plot <- elem_insert(names(d), pos, "plot")

  plot <- apply(d[, grepl("^Value", names(d)), with = FALSE], 1, function(x) jsonlite::toJSON(list(values = x, options = list(type = "line"))))

  return(list(plot = plot, order = order_with_plot))
}

#XXX: key?
mydenormalise <- function(data) {
  if ("geographicAreaM49" %in% names(data)) {
    lhs <- "geographicAreaM49 +"
  } else {
    lhs <- "geographicAreaM49Reporter + geographicAreaM49Partner +"
  }
  myformula <- as.formula(paste(lhs, "measuredElementTrade + measuredItemCPC ~ timePointYears"))
  res <- dcast.data.table(data, myformula, value.var = c("Value", "flagObservationStatus", "flagMethod"))

  return(res)
}

set_hot_colnames <- function(data) {
  headers <- colnames(data)
  headers <- sub("Value_", "", headers)
  headers[grep("geographicAreaM49Partner", headers)] <- "Partner"
  headers[grep("^geographicAreaM49(Reporter)?$", headers)] <- "Reporter"
  headers[grep("^flag", headers)] <- ""

  return(headers)
}

set_hot_colwidths <- function(data, hide = FALSE) {
  ncols <- ncol(data)
  widths <- rep(100, ncols + 1)
  widths[grep("^geographicAreaM49(Reporter)?$", colnames(data))] <- 0.1
  widths[grep("measuredItemCPC", colnames(data))] <- 0.1

  widths[grep("Partner", colnames(data))] <- 150

  if (hide == TRUE) {
    widths[grep("^flag", colnames(data))] <- 0.1
  } else {
    widths[grep("^flag", colnames(data))] <- 15
  }
  
  return(widths)
}

# XXX: use denormalise
set_standard_cols <- function(data) {
  data <- copy(data)
  orig_names <- names(data)
  values_pos <- grep("^Value", names(data))
  flagob_pos <- grep("^flagObservationStatus", names(data))
  flagme_pos <- grep("^flagMethod", names(data))

  rhs_ord <- names(data)[c(rbind(values_pos, flagob_pos, flagme_pos))]

  if (!("geographicAreaM49Partner" %in% names(data))) {
    data[, geographicAreaM49Partner := "TOTAL"]
  }

  rep_pos <- grepl("geographicAreaM49", names(data)) & !grepl("geographicAreaM49Partner", names(data))

  final_ord <- c(names(data)[rep_pos], c("geographicAreaM49Partner", "measuredElementTrade", "measuredItemCPC"), rhs_ord)

  setcolorder(data, final_ord)

  return(data)
}


comtrade_classif_file <- paste0(FILES_LOCATION, 'classificationHS.json')

hs_descr <-
  data.table(
    hs = unlist(lapply(RJSONIO::fromJSON(comtrade_classif_file)$results, function(x) x[['id']])),
    description = unlist(lapply(RJSONIO::fromJSON(comtrade_classif_file)$results, function(x) x[['text']]))
  )

hs_descr_hs6 <-
  hs_descr[
    nchar(hs) == 6,
    .(hs6 = hs, description = stringr::str_replace(description, '[^-]+  *- *', ''))
  ]

