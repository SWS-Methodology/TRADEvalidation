# TODO: add a lock system on SaveData() as if there is a concurrent saving
# it **may** happen that the faosws environment comes from another user.

source("global.R")

ui <-
  fluidPage(

    theme = "bootstrap.css",

    tags$head(tags$script(HTML('
      Shiny.addCustomMessageHandler("jsCode",
        function(message) {
          eval(message.value);
        }
      );'))),

    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),

    tags$head(tags$style("#helpdoc img {border-style: solid; border-width: 3px;}")),

    tags$head(tags$style("#helpdoc li {padding: 6px;}")),

    tags$head(tags$style(".shiny-notification {width: 500px;}")),

    tags$head(tags$style(HTML('td {white-space: nowrap !important;}'))),

    tags$head(tags$style(HTML('#table_outliers .htCommentCell {background-color: #f00 !important;}'))),

    fluidRow(
      column(2, uiOutput("gettoken_ui")),
      column(3, uiOutput("query_country_ui")),
      column(1, selectInput("query_flow", "Flow", c("", "import", "export"))),
      column(3, uiOutput("query_item_ui")),
      column(3, sliderInput("query_years", "Years to show", min = 2000, max = MAX_YEAR, c(MAX_YEAR - 10, MAX_YEAR), sep = ""))
    ),

    tabsetPanel(
      id = "tab_total",
      type = "pills",

      tabPanel(
        "Welcome",
        uiOutput("welcome", style = "width: 1000px; margin: auto;")
      ),

      tabPanel(
        "Outliers",
        verbatimTextOutput("out_parameters_info"),
        fluidRow(
          column(4, sliderInput("out_years", "Years to check", min = MAX_YEAR - 10, max = MAX_YEAR, c(MAX_YEAR - 5, MAX_YEAR), sep = "")),
          column(4, uiOutput("out_ratio_ui")),
          column(4, uiOutput("out_growth_ui"))
        ),
        uiOutput("table_outliers_ui")
      ),

      tabPanel(
        "Main items",
        textOutput("text_total_info"),
        checkboxInput("show_info_also_values", "Also main items by monetary value?"),
        rHandsontableOutput("table_total_info")
      ),

      tabPanel(
        "Total",
        fluidRow(
          column(2, selectInput("opt_hideflags", "Show flags?", c(FALSE, TRUE))),
          column(2, selectInput("opt_showmirror", "Show mirror?", c(FALSE, TRUE))),
          column(2, selectInput("opt_showprod", "Show Production?", c(FALSE, TRUE))),
          column(4, sliderInput("share_trade", "Minimum trade share of partner", min = 0, max = 50, 5, step = 5, post = "%", width = "100%")),
          column(2, actionButton("undo", "Undo last modification"))
        ),
        actionButton("toggle_table_total", "show/hide", class = "toggle_visibility", width = "100%"),
        rHandsontableOutput("table_total"),
        uiOutput("toggle_plot_total_ui"),
        actionButton("toggle_plot_total", "show/hide", class = "toggle_visibility", width = "100%"),
        plotlyOutput("plot_total", height = "auto"),
        actionButton("toggle_table_bilat", "show/hide", class = "toggle_visibility", width = "100%"),
        rHandsontableOutput("table_bilat")
      ),

      tabPanel(
        "Bilateral",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "corr_bil_vars_to_show",
              "Show also (if available):",
              choices =
                c(
                  "Original" = "original",
                  "Mirror" = "mirror",
                  "Moving average" = "movav",
                  "Median partners" = "median",
                  "Median world" = "median_world"
                ),
              selected = c("original", "mirror", "movav", "median", "median_world"),
              multiple = TRUE
            ),
            uiOutput('query_bil_partner_ui'),
            uiOutput('corr_check_flags'),
            p(),
            p(),
            uiOutput('corrections_separator'),
            uiOutput('corr_year2correct_ui'),
            uiOutput('corr_variable2correct_ui'),
            uiOutput('corr_current'),
            uiOutput('corr_choose_correction_ui'),
            uiOutput('corr_suggest10'),
            conditionalPanel(
              condition = 'input.corr_choose_correction === "Expert knowledge"',
              uiOutput('corr_suggestexpert') # By how much will change? Seme for publication
            ),
            conditionalPanel(
              condition = 'input.corr_choose_correction === "Mirror flow" & input.year2correct !== ""',
              uiOutput('corr_suggestmirror')
            ),
            conditionalPanel(
              condition = 'input.corr_choose_correction === "Outlier correction" & input.corr_year2correct !== ""',
              div(tableOutput('corr_suggestoutlier'), style = "font-size: 80%")
            ),
            uiOutput('corr_correction10'),
            uiOutput('corr_correction_outlier_ui'),
            conditionalPanel(
              condition = 'input.corr_choose_correction ===  "Publication/website"',
              numericInput("corr_correction_publication", "Input a number:", 0, min = 0)
            ),
            conditionalPanel(
              condition = 'input.corr_choose_correction ===  "Expert knowledge"',
              numericInput("corr_correction_expert", "Input a number:", 0, min = 0)
            ),
            conditionalPanel(
              condition = 'input.query_bil_partner !== "" & input.query_item !== "" & input.query_year2correct !== ""',
              uiOutput("corr_note_by_analyst_ui")
            ),
            conditionalPanel(
              condition = 'input.corr_choose_correction === "Measurement factor" | input.corr_choose_correction === "Mirror flow"',
            uiOutput("corr_multiplecorrections_ui")
            ),
            uiOutput('corr_go10'),
            uiOutput('corr_apply_correction_ui'),
            uiOutput('corr_confirm_correction_ui'),
            actionButton("undo_bilat", "Undo last modification"),
            width = 3
          ),
          mainPanel(
            uiOutput("corr_selection_title"),
            plotOutput("corr_plotUV", hover = "corr_plotUV_hover"),
            uiOutput("corr_plotUV_coords"),
            plotOutput("corr_plotQTY", hover = "corr_plotQTY_hover"),
            uiOutput("corr_plotQTY_coords"),
            plotOutput("corr_plotValue", hover = "corr_plotValue_hover"),
            uiOutput("corr_plotValue_coords"),
            HTML('<p></p><h1>Number of partners by year</h1><p></p>'),
            plotOutput("n_partners"),
            HTML('<p></p><h1><strong>Bilateral</strong> reporter data</h1><p></p>'),
            rHandsontableOutput("corr_datasnapshot_reporter"),
            HTML('<p></p><h1><strong>Bilateral</strong> partner data</h1>
                 <p>Note: Mirrored flows of corrections will be updated when corrections are saved.</p>'),
            rHandsontableOutput("corr_datasnapshot_partner"),
            HTML('<p></p><h1>History</h1><p></p>'),
            rHandsontableOutput("corr_history"),
            HTML('<p></p><h1>Flags legend</h1><p></p>'),
            tableOutput("corr_myflags")
          )
        )
      ),

      tabPanel(
        "TO COMMIT",
        actionButton("send_to_datasets", "Send to SWS datasets"),
        verbatimTextOutput("save_result_bil"),
        verbatimTextOutput("save_result_tot"),
        verbatimTextOutput("save_result_corr"),
        verbatimTextOutput("save_result_mirr"),
        h2("List of corrections ready to be saved:"),
        DT::dataTableOutput("new_corrections"),
        h2("List of partner/item/flow/years that will be updated because they are mirrored:"),
        DT::dataTableOutput("new_mirrored")
      ),

      tabPanel(
        "Corrections saved",
        actionButton("corr_delete_correction", "Delete selected correction"),
        h2("List of corrections currently saved:"),
        DT::dataTableOutput("corr_corrections_table")
      ),

      tabPanel(
        "Raw data / MAP",
        h1(span("Bilateral"), span("raw", style = "color: red;"), span("data (ONLY 2014-2018)")),
        HTML('<p></p><strong>In the following table you can see the RAW bilateral SHIPMENTS corresponding to the bilateral flow you chose in the "Bilateral" tab</strong>.<p></p>'),
        DT::dataTableOutput("rawdata"),
        h1("Change the CPC and FCL code"),
        HTML("<p></p><p><strong>In the small table below you can change the CPC/FCL code assigned to the HS code above by clicking on a cell and entering a new code (AFTER YOU INSERT EITHER THE CPC OR THE FCL CODE, CLICK ON ENTER (the other code should appear automatically)).</p><p>NOTE: it will change the code for ALL bilateral flows attached to that HS, not only the one partner-specific that you see above.</strong></p>"),
        rHandsontableOutput("changelink"),
        HTML("<p></p>"),
        actionButton("applychangelink", "Save change to trademap", style = "background-color: lightgreen; font-weight: bold;"),
        uiOutput("confirm_linkchange"),
        h1("CPC codes assigned to the HS 6 digits in other countries"),
        HTML("<p></p><strong>Below &darr; there are (eventually) other CPC codes assigned to other HS code starting with the same 6 digits as the code above &uarr;</strong><p></p>"),
        DT::dataTableOutput("maphelper")
      ),

      tabPanel(
        "Help",
        div(HTML(markdown::markdownToHTML("files/help.Rmd", fragment.only = TRUE)), id = "helpdoc", style = "width: 1000px; margin: auto;")
      ) #,

      #tabPanel(
      #  "debug",
      #  textOutput("debug"),
      #  textOutput("debug1"),
      #  textOutput("tokenValidator")
      #)
    )
  )

server <- function(input, output, session) {

  # BUG: show/display is being sometimes triggered twice. See alert.
  myscript <- "$(function(){
          
              $('.wtHolder').scroll(function() {
                  $('.wtHolder').scrollLeft($(this).scrollLeft());    
              });

          $('.toggle_visibility').on('click', function() {
                    var status = $(this).next().css('display');
                    var new_status = (status !== 'none') ? 'none' : 'block';
                    // alert(status + new_status);
                    $(this).next().css('display', new_status)}
          );
          
          })"


  values <-
    reactiveValues(
      error                 = "",
      initial_country       = NULL,
      codelists             = NULL,
      query_years           = NULL,
      query_country         = NULL,
      query_item            = NULL,
      query_bil_partner     = NULL,
      outliers_params       = NULL,
      outliers_thresholds   = NULL,
      do_not_check          = NULL,
      new_sws_data_total    = NULL,
      new_sws_data_mirr_tot = NULL,
      new_sws_data_bilat    = NULL,
      fcl_2_cpc             = NULL,
      data_outliers         = NULL,
      data_total            = NULL,
      data_mirr_tot         = NULL,
      data_mirr_bilat       = NULL,
      data_world            = NULL,
      data_bilat            = NULL,
      data_prod             = NULL,
      data_multiple         = NULL,
      corrections           = NULL,
      corr_opts_out_method  = NULL,
      corr_cant_correct     = FALSE,
      multiplecorrections   = FALSE,
      new_figure            = NA_real_,
      new_10                = NA_real_,
      new_mirror            = NA_real_,
      threshold_used        = NA_real_,
      current_data          = NULL,
      data_uncorrect        = NULL,
      people_in_charge      = NULL,
      hs                    = NA_character_,
      hs6                   = NA_character_,
      user                  = NA_character_,
      email                 = NA_character_,
      link_msg              = NA_character_,
      existing_partners     = NA_character_,
      out_defaults          = list(out_years = c(MAX_YEAR - 4, MAX_YEAR), out_ratio = c(0.25, 4), out_growth = c(-50, 100)),
      rendered_outliers     = list(),
      corrections_to_save   = list(),
      mirror_to_save        = list(),
      modif_history         = list(),
      sel_bilat =
        list(
          partner  = NA_character_,
          element  = NA_character_,
          year     = NA_character_ # yes, character
        )
    )

  output$out_parameters_info <-
    renderText({
      req(values$threshold_used)
      info_thresh <- paste("quantities greater than", fmtnum(values$threshold_used))
      info_ratio <- paste("ratio lower than", values$out_defaults$out_ratio[1], "or greater than", values$out_defaults$out_ratio[2])
      info_growth <- paste("growth rate lower than", paste0(values$out_defaults$out_growth[1], "%"), "or greater than", paste0(values$out_defaults$out_growth[2], "%"))
      print(paste0("Outliers found for ", input$query_country, ". Paramaters used: ", info_thresh, "; ", info_ratio, "; ", info_growth))
    })

  output$rawdata <-
    DT::renderDataTable({

      req(values$query_country, values$sel_bilat$partner, values$sel_bilat$year)

      dataReadProgress_trademap <- Progress$new(session, min = 0, max = 100)
      dataReadProgress_trademap$set(value = 100, message = "Downloading detailed tariff line")
      on.exit(dataReadProgress_trademap$close())

      f <- file.path(TRADEMAP_DIR, paste0(values$sel_bilat$year, "/", values$query_country, ".rds"))

      if (file.exists(f)) {

        d <- readRDS(f)

        setDT(d)

        res <-
          d[
            reporter == values$query_country &
              partner == values$sel_bilat$partner &
              flow == ifelse(input$query_flow == "import", 1L, 2L) &
              cpc == values$query_item
          ]

        values$hs <- unique(res$hs)

        res[, hs6 := substr(hs, 1, 6)]

        res <- merge(res, hs_descr_hs6, by = "hs6", all.x = TRUE)

        values$hs6 <- unique(res$hs6)

        values$current_data <- data.table(hs = unique(res$hs), cpc = NA_character_, fcl = NA_character_)

        res[, hs6 := NULL]

        DT::datatable(res[], rownames = FALSE, selection = "single")
      }
    })

  output$changelink <-
    renderRHandsontable({

      req(values$query_country, values$current_data)

      d <- rhandsontable(values$current_data, readOnly = FALSE)

      d <- hot_col(d, col = "hs", readOnly = TRUE)

      return(d)
    })

  observeEvent(
    input$applychangelink,
    {
      if (!anyNA(values$current_data)) {

        dataReadProgress_tab <- Progress$new(session, min = 0, max = 100)
        dataReadProgress_tab$set(value = 100, message = "Loading map helper from the SWS")
        on.exit(dataReadProgress_tab$close())

        changeset <- Changeset(paste0("ess_trademap_", values$sel_bilat$year))

        dat_orig <-
          ReadDatatable(
            paste0("ess_trademap_", values$sel_bilat$year),
            where = paste0("hs IN (",
                           paste(shQuote(values$hs, type = "sh"), collapse = ", "),
                           ") AND area IN ('", values$query_country,
                           "') AND flow IN (", ifelse(values$query_flow == "import", 1, 2), ")"),
            readOnly = FALSE
          )

        orig_names <- copy(names(dat_orig))

        dat <- copy(dat_orig)

        dat_new <- copy(values$current_data)

        setDT(dat_new)

        dat_new[, hs6 := substr(hs, 1, 6)]

        dat_new <- merge(dat_new, hs_descr_hs6, by = "hs6", all.x = TRUE)

        cpc_descr <- values$codelists$item[, .(cpc = code, cpc_description = description)]

        dat_new <- merge(dat_new, cpc_descr, by = "cpc", all.x = TRUE)

        setnames(
          dat_new,
          c("cpc", "fcl", "description", "cpc_description"),
          c("cpc_new", "fcl_new", "hs_description_new", "cpc_description_new")
        )

        dat <- merge(dat, dat_new, by = "hs", all.x = TRUE)

        if (nrow(dat[cpc != cpc_new | fcl != fcl_new]) > 0) {

          # NOTE: FCL needs to be defined. If FCL is defined, CPC should be as well
          dat[
            !is.na(cpc_new) & !is.na(fcl_new),
            `:=`(
              cpc             = cpc_new,
              fcl             = fcl_new,
              hs_description  = hs_description_new,
              cpc_description = cpc_description_new,
              map_src         = "manual"
            )
          ]

          dat <- dat[, orig_names, with = FALSE]

          dat[, `__id` := NULL]
          dat[, `__ts` := NULL]

          added_del <- try(AddDeletions(changeset, dat_orig))

          if (inherits(added_del, "try-error")) {
            values$link_msg <- "Something went wrong when deleting the link."
          } else {
            added_insert <- try(AddInsertions(changeset, dat))
            if (inherits(added_insert, "try-error")) {
              values$link_msg <- "Something went wrong when inserting the link."
            } else {
              finalised <- try(Finalise(changeset))
              if (inherits(finalised, "try-error")) {
                values$link_msg <- "The link was NOT changed."
              } else {
                values$link_msg <- "The link was correctly changed. Updated data will appear after nex plugin run."
              }
            }
          }
        }
      }
    }
  )

  output$confirm_linkchange <-
    renderUI({

      req(values$link_msg)

      style_col <- ifelse(grepl("correct", values$link_msg), "green;", "red;")
      p(values$link_msg, style = paste("font-weight: bolder; color:", style_col))
    })

  output$out_growth_ui <-
    renderUI({

      req(values$query_country, values$user, values$people_in_charge)


      if (values$email %in% values$people_in_charge) {
        sliderInput("out_growth", "Growth rate thresholds", min = -200 , max = 200, step = 10, c(-50, 100), post = "%")
      } else {
        NULL
      }
    })

  output$out_ratio_ui <-
    renderUI({

      req(values$query_country, values$user, values$people_in_charge)

      if (values$email %in% values$people_in_charge) {
        sliderInput("out_ratio", "Ratio thresholds", min = 0.1, max = 10, step = 0.15, c(0.25, 4))
      } else {
        NULL
      }
    })

  observeEvent(
    input$out_years,
    {
      values$out_defaults$out_years <- input$out_years
    }
  )

  observeEvent(
    input$out_ratio,
    {
      values$out_defaults$out_ratio <- input$out_ratio
    }
  )

  observeEvent(
    input$out_growth,
    {
      values$out_defaults$out_growth <- input$out_growth
    }
  )

  output$corr_datasnapshot_reporter <-
    renderRHandsontable({

      req(values$query_country, values$query_bil_partner, values$query_item,
          input$query_flow, values$new_sws_data_bilat)

      d <-
        data_corr_bilat_plots()[
          geographicAreaM49Partner == values$sel_bilat$partner,
          .(
            element = measuredElementTrade,
            timePointYears,
            Value,
            flags = paste(flagObservationStatus, flagMethod, sep = "-")
          )
        ]

      d[!(substr(element, 3, 3) %in% 2:3), element := "Quantity"]
      d[substr(element, 3, 3) == "3", element := "Unit_value"]
      d[substr(element, 3, 3) == "2", element := "Value"]

      d <- dcast.data.table(d, timePointYears ~ element, value.var = c("Value", "flags"))

      d[d == "NA-NA"] <- ""

      names(d) <- sub("^Value_", "", names(d))

      setcolorder(d, c("timePointYears", "Quantity", "Value", "Unit_value", "flags_Quantity", "flags_Value", "flags_Unit_value"))

      # TODO: add column widths
      rhandsontable(d, rowHeaders = FALSE) %>%
        hot_col(c("Quantity", "Value", "Unit_value"), format = "0,0")
    })

  output$corr_datasnapshot_partner <-
    renderRHandsontable({

      req(values$query_country, values$query_bil_partner, values$query_item,
          input$query_flow, values$data_mirr_bilat)

      if (nrow(values$data_mirr_bilat) > 0) {

        d <-
          values$data_mirr_bilat[,
            .(
              element = measuredElementTrade,
              timePointYears,
              Value,
              flags = paste(flagObservationStatus, flagMethod, sep = "-")
            )
          ]

        d[!(substr(element, 3, 3) %in% 2:3), element := "Quantity"]
        d[substr(element, 3, 3) == "3", element := "Unit_value"]
        d[substr(element, 3, 3) == "2", element := "Value"]

        d <- dcast.data.table(d, timePointYears ~ element, value.var = c("Value", "flags"))

        d[d == "NA-NA"] <- ""

        names(d) <- sub("^Value_", "", names(d))

        setcolorder(d, c("timePointYears", "Quantity", "Value", "Unit_value",
                         "flags_Quantity", "flags_Value", "flags_Unit_value"))

        # TODO: add column widths
        rhandsontable(d, rowHeaders = FALSE) %>%
          hot_col(c("Quantity", "Value", "Unit_value"), format = "0,0")
      } else {
        rhandsontable(data.table(info = "No data"), rowHeaders = FALSE)
      }
    })

  output$new_mirrored <-
    DT::renderDataTable({
      if (length(values$mirror_to_save) > 0) {
        d <- rbindlist2(values$mirror_to_save)
        if (nrow(d) > 0) {
          unique(d[, .(geographicAreaM49Reporter, measuredItemCPC, timePointYears)])
        } else {
          data.table(info = "No corrections, or no mirrored partners for the corrections above.")
        }
      }
    })

  output$new_corrections <-
    DT::renderDataTable({
      if (length(values$corrections_to_save) > 0) {
        d <- rbindlist2(values$corrections_to_save)
        if (nrow(d) > 0) {
          show_corrections_table(d, values$codelists)
        } else {
          show_corrections_table(values$corrections[0], values$codelists)
        }
      }
    })

  output$corr_current <-
    renderUI({

      req(values$query_country, values$query_bil_partner, values$query_item,
          input$query_flow, input$corr_year2correct)

      d <-
        data_corr_bilat_plots()[
          geographicAreaM49Partner == values$query_bil_partner &
            timePointYears == values$sel_bilat$year
        ]

      uv  <- d[substr(measuredElementTrade, 3, 3) == "3"]$Value
      d   <- d[substr(measuredElementTrade, 3, 3) != "3"]
      val <- d[substr(measuredElementTrade, 3, 3) == "2"]$Value
      qty <- d[substr(measuredElementTrade, 3, 3) != "2"]$Value

      list(
        p(paste("Current UV:", fmtnum(uv))),
        p(paste("Current qty:", fmtnum(qty))),
        p(paste("Current value:", fmtnum(val)))
      )
    })

  output$corr_suggest10 <-
    renderUI({

      req(values$query_country, values$query_bil_partner, values$query_item,
          input$query_flow, !values$corr_cant_correct, input$corr_year2correct,
          input$corr_correction10)

      if (input$corr_choose_correction == "Measurement factor") {
        d_uv <- data_corr_bilat_plots()[substr(measuredElementTrade, 3, 3) == "3" & timePointYears == values$sel_bilat$year]

        uv <- d_uv[geographicAreaM49Partner == values$sel_bilat$partner]$Value

        # Median, excluding itself
        median_partners <- median(d_uv[geographicAreaM49Partner != values$query_bil_partner]$Value, na.rm = TRUE)

        d_vals <-
          data_corr_bilat_plots()[
            substr(measuredElementTrade, 3, 3) != "3" &
              timePointYears == values$sel_bilat$year &
              geographicAreaM49Partner == values$sel_bilat$partner
          ]

        old_value <- d_vals[substr(measuredElementTrade, 3, 3) == "2"]$Value
        old_qty <- d_vals[substr(measuredElementTrade, 3, 3) != "2"]$Value

        if (input$corr_variable2correct == "Value") {
          res_var <- old_value * as.numeric(input$corr_correction10)
          res_uv <- res_var / old_qty * 1000
          old <- old_value
        } else {
          res_var <- old_qty * as.numeric(input$corr_correction10)
          res_uv <- old_value / res_var * 1000
        }

        msg_var <- paste0("New ", tolower(input$corr_variable2correct), ": ", fmtnum(res_var))
        msg_uv <- paste("New UV:", fmtnum(res_uv))

        values$new_10 <- res_var

        list(
          p(powers(uv, median_partners)),
          p(msg_var),
          p(msg_uv)
        )
      } else {
        NULL
      }
    })

  output$corr_correction_outlier_ui <-
    renderUI({

      req(values$query_country, values$query_bil_partner, values$query_item,
          input$query_flow, !values$corr_cant_correct, input$corr_year2correct,
          input$corr_choose_correction)

      if (input$corr_choose_correction == "Outlier correction") {

        d_var <-
          data_corr_bilat_plots()[
            substr(measuredElementTrade, 3, 3) != "3" &
              timePointYears == values$sel_bilat$year &
              geographicAreaM49Partner == values$sel_bilat$partner
          ]

        d_uv <-
          data_corr_bilat_plots()[
            !is.na(Value) &
              substr(measuredElementTrade, 3, 3) == "3" &
              timePointYears == values$sel_bilat$year
          ]

        movav <- d_uv[geographicAreaM49Partner == values$sel_bilat$partner]$movav

        # Median, excluding itself
        median_partners <- median(d_uv[geographicAreaM49Partner != values$query_bil_partner]$Value, na.rm = TRUE)

        median_world <- median(values$data_world[timePointYears == values$sel_bilat$year & substr(measuredElementTrade, 3, 3) == "3"]$Value, na.rm = TRUE)

        if (input$corr_variable2correct == "Value") {
          zq <- d_var[substr(measuredElementTrade, 3, 4) != "22"]$Value
          z_movav           <- (zq / 1000) * movav
          z_median_partners <- (zq / 1000) * median_partners
          z_median_world    <- (zq / 1000) * median_world
        } else { # quantity
          zv <- d_var[substr(measuredElementTrade, 3, 4) == "22"]$Value
          z_movav           <- (1000 * zv) / movav
          z_median_partners <- (1000 * zv) / median_partners
          z_median_world    <- (1000 * zv) / median_world
        }

        res <-
          data.table(
            how      = c("Moving average", "Median partners", "Median world"),
            uv       = c(movav, median_partners, median_world),
            variable = c(z_movav, z_median_partners, z_median_world)
          )

        res <- res[!is.na(uv)]

        res_vec <- res$variable
        names(res_vec) <- res$how

        values$corr_opts_out_method <- res_vec

        output$corr_suggestoutlier <-
          renderTable({

            req(!values$corr_cant_correct)

            res[, uv := fmtnum(uv)]
            res[, variable := fmtnum(variable)]
            setnames(res, "variable", tolower(input$corr_variable2correct))
            res[]
          }, spacing = "xs") # XXX align = c("l", "l", "r", "r") doesn't work

        selectInput(
          "corr_correction_outlier",
          "How to correct:",
          res_vec 
        )
      } else {
        NULL
      }
    })

  output$corr_multiplecorrections_ui <-
    renderUI({

      req(values$query_country, values$query_bil_partner,
          values$query_item, input$query_flow, !values$corr_cant_correct,
          input$corr_choose_correction)

      checkboxInput("corr_multiplecorrections", "Multiple corrections?")
    })

  observeEvent(
    input$corr_multiplecorrections,
    {
      values$multiplecorrections <- input$corr_multiplecorrections
    }
  )

  output$corr_suggestmirror <-
    renderUI({

      req(values$query_country, values$query_bil_partner, values$query_item,
          input$query_flow, input$corr_year2correct, !values$corr_cant_correct)

      d_mirr <- values$data_mirr_bilat[timePointYears == values$sel_bilat$year & substr(measuredElementTrade, 3, 3) == "3"]$Value

      if (input$corr_variable2correct == "Value") {
        z_mirr <- values$data_mirr_bilat[timePointYears == values$sel_bilat$year & substr(measuredElementTrade, 3, 3) == "2"]$Value
      } else {
        z_mirr <- values$data_mirr_bilat[timePointYears == values$sel_bilat$year & !(substr(measuredElementTrade, 3, 3) %in% 2:3)]$Value
      }

      if (length(d_mirr) == 0 | is.na(d_mirr)) {
        values$new_mirror <- NA_real_
        p('No mirror quantity available.')
      } else {
        values$new_mirror <- z_mirr
        div(
          p('Mirror UV: ', strong(fmtnum(d_mirr))),
          p(paste0('Mirror ', tolower(input$corr_variable2correct), ': ', fmtnum(z_mirr)))
        )
      }
    })

  output$corr_note_by_analyst_ui <-
    renderUI({

      req(values$query_bil_partner, values$query_item, input$query_flow,
          !values$corr_cant_correct, input$corr_year2correct,
          input$corr_choose_correction)

      if (input$corr_choose_correction %in% c("Publication/website", "None")) {
        if (input$corr_choose_correction == "Publication/website") {
          choices <- publication_sources$source
        } else {
          choices <-
            c('Not significant quantity/value',
              'Unit value confirmed by partner data',
              'Unit value within the historical range of unit values for this partner',
              'Unit values have no historic data flows for comparison, but are reasonable compared to partner/global values',
              'Unit value outlier follows larger global trend in unit value movements',
              'Unit values are in line with median unit values for this reporter',
              'Quantity confirmed by partner data',
              'Non-food product'
            )
        }
        selectInput("corr_note_by_analyst", "Optional comment/note:", choices)
      } else if (input$corr_choose_correction == "Expert knowledge") {
        textInput("corr_note_by_analyst", "Mandatory comment/note:")
      } else {
        textInput("corr_note_by_analyst", "Optional comment/note:")
      }
    })

  output$n_partners <-
    renderPlot({

      req(values$query_country, values$query_item, input$query_flow,
          values$new_sws_data_bilat)

      d <- copy(values$new_sws_data_bilat)

      n_partners <- unique(d[, .(geographicAreaM49Partner, timePointYears)])[, .N, keyby = timePointYears]

      ggplot(n_partners, aes(x = as.character(timePointYears), y = N)) + geom_col()
    })

  output$corr_corrections_table <-
    DT::renderDataTable({

      req(values$query_country, values$corrections)

      show_corrections_table(values$corrections, values$codelists)
    })

  output$corr_variable2correct_ui <-
    renderUI({

      req(values$query_country, values$query_item, input$query_flow,
          values$query_bil_partner, input$corr_year2correct,
          !values$corr_cant_correct)

      selectInput(
        "corr_variable2correct",
        "Variable to correct:",
        c('Quantity', 'Value')
      )
    })

  output$corr_year2correct_ui <-
    renderUI({

      req(values$query_country, values$query_item, input$query_flow,
          values$query_bil_partner, values$data_bilat)

      x <- values$data_bilat[!is.na(Value) & geographicAreaM49Partner == values$query_bil_partner]

      years <- c("", rev(sort(unique(x$timePointYears))))

      years <- years[years >= 2014]

      if (length(years) == 0) {
        p("No years to correct.")
      } else {
        selectInput("corr_year2correct", "Year to correct:", years)
      }

    })

  data_corr_bilat_plots <-
    reactive({
      d <- copy(values$new_sws_data_bilat)

      d <- d[CJ(geographicAreaM49Reporter = unique(d$geographicAreaM49Reporter), geographicAreaM49Partner = unique(d$geographicAreaM49Partner), measuredElementTrade = unique(d$measuredElementTrade), measuredItemCPC = unique(d$measuredItemCPC), timePointYears = as.character(values$query_years[1]:values$query_years[2])), on = c("geographicAreaM49Reporter", "geographicAreaM49Partner", "measuredElementTrade", "measuredItemCPC", "timePointYears")]

      d[,
        movav := RcppRoll::roll_mean(shift(Value), 3, fill = NA, align = 'right'),
        by = c("geographicAreaM49Reporter", "geographicAreaM49Partner", "measuredElementTrade", "measuredItemCPC")
      ]

      # FIXME: if there are more than two quantities (tons and heads) this will break or do unexpected stuff

      d[,
        movav_uv := movav[substr(measuredElementTrade, 3, 3) == "2"] / movav[!substr(measuredElementTrade, 3, 3) %in% 2:3] * 1000,
        by = c("geographicAreaM49Reporter", "geographicAreaM49Partner", "measuredItemCPC")
      ]

      d[substr(measuredElementTrade, 3, 3) == "3", movav := movav_uv]

      d[, movav_uv := NULL]

      d[]

    })

  output$corr_plotUV <-
    renderPlot({

      req(values$query_country, values$query_item,
          input$query_flow, values$query_bil_partner)

      d <- data_corr_bilat_plots()[substr(measuredElementTrade, 3, 3) == "3"]

      # median, excluding itself
      d_uv_median <- d[geographicAreaM49Partner != values$query_bil_partner]
      d_uv_median <- d_uv_median[Value > 0, .(median = median(Value, na.rm = TRUE)), c("geographicAreaM49Reporter", "measuredItemCPC", "measuredElementTrade", "timePointYears")]

      d <- d[geographicAreaM49Partner == values$query_bil_partner]

      d <- merge(d, d_uv_median, by = c("geographicAreaM49Reporter", "measuredItemCPC", "measuredElementTrade", "timePointYears"), all.x = TRUE)

      d <- melt.data.table(d, id.vars = "timePointYears", measure.vars = c("Value", "movav", "median"))

      d_orig <- values$data_bilat[geographicAreaM49Partner == values$query_bil_partner & substr(measuredElementTrade, 3, 3) == "3"][, .(timePointYears, variable = "original", value = Value)]

      d <- rbind(d, d_orig, fill = TRUE)

      d[variable == "movav", `:=`(thresh_min = value / 1.5, thresh_max = value * 1.5)]

      if (nrow(values$data_mirr_bilat[!is.na(Value)]) > 0) {
        d_mirr <- values$data_mirr_bilat[substr(measuredElementTrade, 3, 3) == "3", .(timePointYears, variable = "mirror", value = Value)]
        d <- rbind(d, d_mirr, fill = TRUE)
      }

      if (nrow(values$data_world[!is.na(Value)]) > 0) {
        if (length(unique(values$data_world$measuredElement)) > 1) {
          stop("Stooop please $&%@!")
        }
        d_world <- values$data_world[Value > 0, .(variable = "median_world", value = median(Value)), timePointYears]
        d <- rbind(d, d_world, fill = TRUE)
      }

      d <- d[variable %in% c("Value", input$corr_bil_vars_to_show)]

      bilateral_plot(d, ylab = "Unit value")
    })


  output$corr_plotQTY <-
    renderPlot({

      req(values$query_country, values$query_item,
          input$query_flow, values$query_bil_partner)

      # XXX two quantities ...
      d <- data_corr_bilat_plots()[!(substr(measuredElementTrade, 3, 3) %in% 2:3)]

      d <- d[geographicAreaM49Partner == values$query_bil_partner]

      d <- melt.data.table(d, id.vars = "timePointYears", measure.vars = c("Value", "movav"))

      d_orig <- values$data_bilat[geographicAreaM49Partner == values$query_bil_partner & !(substr(measuredElementTrade, 3, 3) %in% 2:3)][, .(timePointYears, variable = "original", value = Value)]

      d <- rbind(d, d_orig, fill = TRUE)

      d[variable == "movav", `:=`(thresh_min = value / 1.5, thresh_max = value * 1.5)]

      # XXX: three elements...
      if (nrow(values$data_mirr_bilat[!is.na(Value)]) > 0) {
        d_mirr <- values$data_mirr_bilat[!(substr(measuredElementTrade, 3, 3) %in% 2:3), .(timePointYears, variable = "mirror", value = Value)]
        d <- rbind(d, d_mirr, fill = TRUE)
      }

      d <- d[variable %in% c("Value", input$corr_bil_vars_to_show)]

      bilateral_plot(d, ylab = "Quantity")
    })


  output$corr_plotValue <-
    renderPlot({

      req(values$query_country, values$query_item,
          input$query_flow, values$query_bil_partner)

      # XXX two quantities ...
      d <- data_corr_bilat_plots()[substr(measuredElementTrade, 3, 3) == "2"]

      d <- d[geographicAreaM49Partner == values$query_bil_partner]

      d <- melt.data.table(d, id.vars = "timePointYears", measure.vars = c("Value", "movav"))

      d_orig <- values$data_bilat[geographicAreaM49Partner == values$query_bil_partner & substr(measuredElementTrade, 3, 3) == "2"][, .(timePointYears, variable = "original", value = Value)]

      d <- rbind(d, d_orig, fill = TRUE)

      d[variable == "movav", `:=`(thresh_min = value / 1.5, thresh_max = value * 1.5)]

      if (nrow(values$data_mirr_bilat[!is.na(Value)]) > 0) {
        d_mirr <- values$data_mirr_bilat[substr(measuredElementTrade, 3, 3) == "2", .(timePointYears, variable = "mirror", value = Value)]
        d <- rbind(d, d_mirr, fill = TRUE)
      }

      d <- d[variable %in% c("Value", input$corr_bil_vars_to_show)]

      bilateral_plot(d, ylab = "Monetary value")
    })

  # Below, top_px will be set keeping in mind the order of the plots:
  # 0 for first, 400 for second, 800 for third (400px is default plot height)

  output$corr_plotUV_coords <-
    renderUI({

      req(values$query_country, values$query_item,
          input$query_flow, values$query_bil_partner)

      tooltip_well(input$corr_plotUV_hover, 0)
    })

  output$corr_plotQTY_coords <-
    renderUI({

      req(values$query_country, values$query_item,
          input$query_flow, values$query_bil_partner)

      tooltip_well(input$corr_plotQTY_hover, 400)
    })

  output$corr_plotValue_coords <-
    renderUI({

      req(values$query_country, values$query_item,
          input$query_flow, values$query_bil_partner)

      tooltip_well(input$corr_plotValue_hover, 800)
    })

  output$corr_go10 <-
    renderUI({

      req(!values$corr_cant_correct, input$corr_year2correct)

      if (identical(input$corr_choose_correction, "Measurement factor")) {
        tens <- c(0.0001, 0.001, 0.01, 0.1, 10, 100, 1000, 10000)
        names(tens) <- sub("\\.?0+$", "", scales::comma(tens))
        selectInput("corr_correction10", "Choose a factor:", tens)
      } else {
        NULL
      }
    })

  output$corr_apply_correction_ui <-
    renderUI({

      req(values$query_country, values$query_item, input$query_flow,
          values$query_bil_partner, !values$corr_cant_correct,
          input$corr_year2correct)

      actionButton("corr_apply_correction", "Apply correction")
    })

  output$corr_confirm_correction_ui <-
    renderUI({

      req(values$query_country, values$query_item, input$query_flow,
          values$query_bil_partner, !values$corr_cant_correct,
          input$corr_year2correct, input$corr_apply_correction)

      if (input$corr_apply_correction > 0) {
        actionButton("corr_confirm_correction", "Confirm correction")
      } else {
        NULL
      }
    })

  observeEvent(
    input$corr_confirm_correction,
    {
      if (input$corr_choose_correction == 'Expert knowledge' & input$corr_note_by_analyst == '') {
        showModal(
          modalDialog(
            title = "Missing comment",
            "A comment/note is required."
          )
        )
      } else {
        add_message <-
          ifelse(
            values$multiplecorrections == TRUE,
            'You are applying multiple corrections, so it will take a while.
            As soon as the corrections are applied, this dialog will be closed.',
            ''
          )

        showModal(
          modalDialog(
            title = "Confirm correction",
              p("By clicking OK you confirm that the correction should be
              applied and it will be saved."), p(add_message),
            footer = tagList(
              modalButton("Cancel"),
              actionButton("corr_okCorrection", "OK")
            )
          )
        )
      }
    }
  )

  output$corr_selection_title <-
    renderUI({

      req(values$query_country, values$query_bil_partner, values$query_item,
          input$query_flow, input$corr_year2correct, values$data_bilat,
          input$corr_variable2correct)

      d <- values$data_bilat[geographicAreaM49Partner == values$query_bil_partner & substr(measuredElementTrade, 3, 3) != "3" & timePointYears == input$corr_year2correct]

      if (input$corr_variable2correct == "Value") {
        d <- d[substr(measuredElementTrade, 3, 3) == "2"]
      } else {
        d <- d[substr(measuredElementTrade, 3, 3) != "2"]
      }

      if (nrow(values$corrections) > 0) {
        d_corrs <- values$corrections[partner == values$query_bil_partner & flow == ifelse(input$query_flow == "import", 1, 2) & item == values$query_item & year == input$corr_year2correct]
      } else {
        d_corrs <- data.table()
      }

      if (d$flagObservationStatus == "T") {
        values$corr_cant_correct <- TRUE
        div('NOTE: this is a mirrored flow, what needs adjustment is the mirror.', style = "font-size: 20px; background-color: red; font-weight: bold;")
      } else if (nrow(d_corrs) > 0) {
        values$corr_cant_correct <- TRUE
        div('NOTE: a correction already exists. If you need to re-correct it (!), remove it first in the "Corrections saved" tab.', style = "font-size: 20px; background-color: red; font-weight: bold;")
      } else {
        NULL
      }
    })

  output$gettoken_ui <- renderUI({
    if (is.na(values$user)) {
      actionButton("gettoken", "Token", width = "100%", value = "5411e05a-1792-4bad-aeee-a6145f82bff5", style = "background-color: red;")
    } else {
      actionButton("gettoken", values$user, width = "100%", value = "5411e05a-1792-4bad-aeee-a6145f82bff5", style = "background-color: green; color: white;")
    }
  })

  output$corr_choose_correction_ui <-
    renderUI({

      req(values$query_bil_partner, values$query_item, input$query_flow,
          !values$corr_cant_correct, input$corr_year2correct)

      selectInput(
        "corr_choose_correction",
        "Type of correction:",
        TYPES_CORRECTION
      )
    })

  output$query_bil_partner_ui <-
    renderUI({
      choices <- ""
      if (all(!is.na(values$existing_partners))) {
        choices <- append(choices, values$codelists$countries[code %in% values$existing_partners, .(country = paste(code, description, sep = " - "))]$country)
      }
      selectInput("query_bil_partner", "Partner:", choices)
    })

  output$query_country_ui <- renderUI({
    if (!is.null(values$codelists)) {
      choices <- values$codelists$countries[, .(country = paste(code, description, sep = " - "))]$country
      selectInput("query_country", "Country", choices, selected = values$initial_country)
    } else {
      selectInput("query_country", "Country", "")
    }
  })

  output$query_item_ui <- renderUI({
    choices <- ""
    if (!is.null(values$codelists)) {
      choices <- append(choices, values$codelists$items[, .(item = paste(code, description, sep = " - "))]$item)
    }
    selectInput("query_item", "Item", choices, width = "100%")
  })

  output$plot_total <-
    renderPlotly({

      req(TokenValidator(), values$query_country, values$query_item,
          input$query_flow, values$data_total, nrow(values$data_total) > 0)

      if (!is.null(values$outliers_found)) {
        by_vars <- c("geographicAreaM49", "measuredElementTrade", "measuredItemCPC", "timePointYears")

        d_total <-
          merge(
            values$data_total,
            values$outliers_found[, c(by_vars, "outlier"), with = FALSE],
            by = by_vars
          )

        d_total[, outlier_value := ifelse(outlier == TRUE, Value, NA_real_)]

        d_total <- d_total[, .(partner = "TOTAL", measuredElementTrade, timePointYears, Value, outlier_value)]

      } else {
        d_total <- values$data_total[, .(partner = "TOTAL", measuredElementTrade, timePointYears, Value, outlier_value = NA_real_)]
      }

      d_total_new <- values$new_sws_data_total[, .(partner = "NEWTOTAL", measuredElementTrade, timePointYears, Value)]

      d <- rbind(d_total, d_total_new, fill = TRUE)

      if (input$opt_showmirror == TRUE) {
        # Exclude mirror flows (flag T)
        d_mirr_total <- values$data_mirr_tot[flagObservationStatus != "T", .(partner = "MIRROR", Value = sum(Value)), keyby = .(measuredElementTrade, timePointYears)]
        d_mirr_total[, measuredElementTrade := paste0(ifelse(substr(measuredElementTrade, 2, 2) == "6", "59", "56"), substr(measuredElementTrade, 3, 4))]
        d_mirr_total[substr(measuredElementTrade, 3, 4) == "22", Value := Value * CIFFOB, Value / CIFFOB]
        d_mirr_total[, Value := ifelse(substr(measuredElementTrade, 3, 3) == "3", Value[substr(measuredElementTrade, 3, 3) == "2"] / Value[substr(measuredElementTrade, 3, 3) == "1"] * 1000, Value), timePointYears]

        d <- rbind(d, d_mirr_total, fill = TRUE)
      }

      if (input$opt_showprod == TRUE) {

        element <- unique(d[!(substr(measuredElementTrade, 3, 3) %in% 2:3)]$measuredElementTrade)

        d_prod <- values$data_prod[, .(partner = "PRODUCTION", measuredElementTrade = element, timePointYears, Value)]

        d <- rbind(d, d_prod, fill = TRUE)
      }

      if (!is.null(input$table_bilat_select)) {
        d_bilat <- values$new_sws_data_bilat[, c("geographicAreaM49Partner", "measuredElementTrade", "timePointYears", "Value"), with = FALSE]
        d_bilat <- d_bilat[geographicAreaM49Partner == values$sel_bilat$partner]
        setnames(d_bilat, "geographicAreaM49Partner", "partner")
        d <- rbind(d, d_bilat, fill = TRUE)
      }

      ggplot(d,
             aes(x = timePointYears)) + # XXX: year as character?
                 geom_point(aes(y = outlier_value), size = 5, color = "red") +
                 geom_line(aes(y = Value, group = partner, color = partner)) +
                 geom_point(aes(y = Value, group = partner, color = partner)) +
                 facet_wrap(~measuredElementTrade, scales = 'free', nrow = 3) +
                 scale_x_discrete(expand = c(0.02, 0.02))
    })

  observeEvent(
    c(TokenValidator(), values$query_country, values$query_item,
      input$query_flow, values$query_years, input$opt_showmirror),
    {
      ################## MIRROR #####################
      values$data_mirr_tot <- {

        req(TokenValidator(), values$query_country,
            values$query_item, input$query_flow)

        if (input$opt_showmirror == TRUE) {
           cl_elem <- values$codelists$elements[substr(code, 1, 2) == ifelse(input$query_flow == "import", "59", "56")] # mirror, so we need the opposite

           key <-
             DatasetKey(
               domain = DOMAIN,
               dataset = DATASET_BIL,
               dimensions =
                 list(
                   geographicAreaM49Reporter = Dimension("geographicAreaM49Reporter", values$codelists$countries$code),
                   geographicAreaM49Partner  = Dimension("geographicAreaM49Partner",  values$query_country),
                   measuredElementTrade      = Dimension("measuredElementTrade",      cl_elem$code),
                   measuredItemCPC           = Dimension("measuredItemCPC",           values$query_item),
                   timePointYears            = Dimension("timePointYears",            as.character(values$query_years[1]:values$query_years[2]))
                 )
             )

           swsProgress_mirr <- Progress$new(session, min = 0, max = 100)
           on.exit(swsProgress_mirr$close())
           swsProgress_mirr$set(value = 100, message = "Loading mirror data from SWS")

           d <- GetData(key) # XXX

           values$new_sws_data_mirr_tot <- exclude_items_more_elements(d, type = "bilateral")

           values$new_sws_data_mirr_tot
        } else {
          NULL
        }
      }
      ################## / MIRROR #####################
    }
  )

  observeEvent(
    c(TokenValidator(), values$query_country, values$query_item,
      input$query_flow, values$query_years, input$opt_showprod),
    {
      ################# PRODUCTION #################
      values$data_prod <- {

        req(TokenValidator(), values$query_country, values$query_item)

        if (input$opt_showprod == TRUE) {

          key <-
            DatasetKey(
              domain = 'suafbs',
              dataset = 'sua_unbalanced',
              dimensions =
                list(
                  geographicAreaM49     = Dimension("geographicAreaM49",     values$query_country),
                  measuredElementSuaFbs = Dimension("measuredElementSuaFbs", "5510"),
                  measuredItemFbsSua    = Dimension("measuredItemFbsSua",    values$query_item),
                  timePointYears        = Dimension("timePointYears",        as.character(values$query_years[1]:values$query_years[2]))
                )
            )

          swsProgress_prod <- Progress$new(session, min = 0, max = 100)
          on.exit(swsProgress_prod$close())
          swsProgress_prod$set(value = 100, message = "Loading production data from SWS")

          d <- GetData(key)

        } else {
          NULL
        }
      }
      ################# / PRODUCTION #################
    }
  )

  observeEvent(
    c(TokenValidator(), values$query_country, values$query_item,
      input$query_flow, values$query_years),
    {
      ################# TOTAL #####################
      values$data_total <- {

        req(TokenValidator(), values$query_country,
            values$query_item, input$query_flow)

        cl_elem <- values$codelists$elements[substr(code, 1, 2) == ifelse(input$query_flow == "import", "56", "59")] 

        key <-
          DatasetKey(
            domain = DOMAIN,
            dataset = DATASET_TOT,
            dimensions =
              list(
                geographicAreaM49    = Dimension("geographicAreaM49",    values$query_country),
                measuredElementTrade = Dimension("measuredElementTrade", cl_elem$code),
                measuredItemCPC      = Dimension("measuredItemCPC",      values$query_item),
                timePointYears       = Dimension("timePointYears",       as.character(values$query_years[1]:values$query_years[2]))
              )
          )

        swsProgress_tot <- Progress$new(session, min = 0, max = 100)
        swsProgress_tot$set(value = 100, message = "Loading total data from SWS")

        # TODO: error handling
        d <- GetData(key)

        values$new_sws_data_total <- exclude_items_more_elements(d, type = "total")

        if (nrow(values$new_sws_data_total) > 0) {
          values$query_years[2] <- max(values$new_sws_data_total$timePointYears)
        }

        values$new_sws_data_total
      }
      ################# / TOTAL #####################


      ################# WORLD #####################
      values$data_world <- {

        req(TokenValidator(), values$query_country,
            values$query_item, input$query_flow, values$data_total)

        if (nrow(values$data_total) > 0) {
          uv_element <- unique(values$data_total[substr(measuredElementTrade, 3, 3) == "3"]$measuredElementTrade)

          key <-
            DatasetKey(
              domain = DOMAIN,
              dataset = DATASET_TOT,
              dimensions =
                list(
                  geographicAreaM49    = Dimension("geographicAreaM49",    values$codelists$countries$code),
                  measuredElementTrade = Dimension("measuredElementTrade", uv_element),
                  measuredItemCPC      = Dimension("measuredItemCPC",      values$query_item),
                  timePointYears       = Dimension("timePointYears",       as.character(values$query_years[1]:values$query_years[2]))
                )
            )

          swsProgress_world <- Progress$new(session, min = 0, max = 100)
          swsProgress_world$set(value = 100, message = "Loading total world data from SWS")

          # TODO: error handling; complete as well ?
          d <- GetData(key)

          exclude_items_more_elements(d, type = "total")
        }
      }
      ################# / WORLD #####################

      ################# BILATERAL #####################
      values$data_bilat <- {

        req(TokenValidator(), values$query_country,
            values$query_item, input$query_flow)

        cl_elem <- values$codelists$elements[substr(code, 1, 2) == ifelse(input$query_flow == "import", "56", "59")] 

        key <-
          DatasetKey(
            domain = DOMAIN,
            dataset = DATASET_BIL,
            dimensions =
              list(
                geographicAreaM49Reporter = Dimension("geographicAreaM49Reporter", values$query_country),
                geographicAreaM49Partner  = Dimension("geographicAreaM49Partner",  values$codelists$countries$code),
                measuredElementTrade      = Dimension("measuredElementTrade",      cl_elem$code),
                measuredItemCPC           = Dimension("measuredItemCPC",           values$query_item),
                timePointYears            = Dimension("timePointYears",            as.character(values$query_years[1]:values$query_years[2]))
              )
          )

        swsProgress_bilat <- Progress$new(session, min = 0, max = 100)
        swsProgress_bilat$set(value = 100, message = "Loading bilateral data from SWS")

        d <- GetData(key)

        d <- exclude_items_more_elements(d, type = "bilateral")

        values$existing_partners <- unique(d$geographicAreaM49Partner)

        # TODO: error handling
        values$new_sws_data_bilat <- copy(d)

        values$new_sws_data_bilat
      }
      ################# / BILATERAL #####################

      on.exit({
        if (exists("swsProgress_tot"))   swsProgress_tot$close()
        if (exists("swsProgress_bilat")) swsProgress_bilat$close()
        if (exists("swsProgress_world")) swsProgress_world$close()
      })

    }
  )

  output$table_bilat <-
    renderRHandsontable({

      req(values$data_bilat)

      d_bil <- copy(values$new_sws_data_bilat)

      if (nrow(d_bil) > 0) {
        shares_trade <- values$new_sws_data_bilat[!(substr(measuredElementTrade, 3, 3) %in% 2:3), .(geographicAreaM49Partner, share = Value / sum(Value)), .(measuredElementTrade, measuredItemCPC, timePointYears)]

        myshare <- input$share_trade
        d <- d_bil[geographicAreaM49Partner %in% shares_trade[share > myshare / 100]$geographicAreaM49Partner]

        while (nrow(d) == 0 & myshare >= 0) {
          myshare <- myshare - 1
          d <- d_bil[geographicAreaM49Partner %in% shares_trade[share > myshare / 100]$geographicAreaM49Partner]

          if (nrow(d) > 0) {
            updateSliderInput(session, "share_trade", value = myshare)
          }
        }

        d <-
          d[
            CJ(
              geographicAreaM49Reporter = unique(d$geographicAreaM49Reporter),
              geographicAreaM49Partner = unique(d$geographicAreaM49Partner),
              measuredElementTrade = unique(d$measuredElementTrade),
              measuredItemCPC = unique(d$measuredItemCPC),
              timePointYears = as.character(values$query_years[1]:values$query_years[2])
            ),
            on = c("geographicAreaM49Reporter", "geographicAreaM49Partner", "measuredElementTrade", "measuredItemCPC", "timePointYears")
          ]

        d <- mydenormalise(d)

        d <- set_standard_cols(d)

        spark <- add_sparkline(d, type = "bilat")

        d$plot <- spark$plot

        setcolorder(d, spark$order)

        col_headers <- set_hot_colnames(d)

        col_widths <- set_hot_colwidths(d, show = input$opt_hideflags)

        prev_order <- names(d)

        d <- merge(d, values$codelists$countries, by.x = "geographicAreaM49Partner", by.y = "code", all.x = TRUE)

        d[, geographicAreaM49Partner := paste(geographicAreaM49Partner, "-", description)]

        d[, geographicAreaM49Partner := ifelse(nchar(geographicAreaM49Partner) > 20, paste0(substr(geographicAreaM49Partner, 1, 17), "..."), geographicAreaM49Partner)]

        d[, description := NULL]

        setcolorder(d, prev_order)

        d <- add_na_rows(d, split = "geographicAreaM49Partner")

        rhandsontable(d, colHeaders = col_headers, selectCallback = TRUE, row_highlight = 1:3, rowHeaders = FALSE) %>%
          hot_col("plot", renderer = htmlwidgets::JS("renderSparkline")) %>%
          hot_cols(fixedColumnsLeft = 4, colWidths = col_widths) %>%
          hot_col(col_headers[grep("^\\d{4}$", col_headers)], format = "0,0")
      } else {
        rhandsontable(data.table(info = "No data"))
      }
  })

  output$text_total_info <-
    renderText({
      text <-
        "The table below shows the top-10 items by element type" %+%
        "(top-10 import values, export tons, etc.) during the period" %+%
        (values$out_defaults$out_years[2] - 10) %+% "-" %+% values$out_defaults$out_years[2] %+%
        ". Items for which the maximum is less than 1,000 tons have been filtered out."

      print(text)
    })

  output$table_total_info <-
    renderRHandsontable({
      d <- copy(all_data_total())

      if (input$show_info_also_values == FALSE) {
        d <- d[substr(measuredElementTrade, 3, 4) != "22"]
      }

      # Remove weight for livestock
      d <- d[!(substr(measuredItemCPC, 1, 3) == "021" & substr(measuredElementTrade, 3, 3) == "1")]

      d <- d[substr(measuredElementTrade, 3, 3) != "3"]

      by_vars <- c("measuredElementTrade", "measuredItemCPC")

      d_miss <- d[, .(missing = sum(is.na(Value))), by = by_vars]

      d_stats <- d[!is.na(Value), .(sum = sum(Value), min = min(Value), max = max(Value), median = median(Value)), by = by_vars]

      d_stats <- merge(d_stats, d_miss, by = by_vars, all.x = TRUE)

      d_perc <- d_stats[, .(measuredItemCPC, percentage = sum / sum(sum)), by = "measuredElementTrade"]

      d_stats <- merge(d_perc, d_stats, by = by_vars, all.x = TRUE)

      # TODO: use the country specific thresholds
      d_stats <- d_stats[median >= 1000]

      d_stats[, sum := NULL]

      setorderv(d_stats, c("measuredElementTrade", "percentage"), order = c(1, -1))

      d_stats <- d_stats[, .SD[1:10], by = "measuredElementTrade"][!is.na(percentage)]

      d_stats[, .(perc_sum = sum(percentage)), by = "measuredElementTrade"]

      setkey(d_stats, NULL) 

      d_stats <- values$codelists$items[d_stats, on = c("code" = "measuredItemCPC")]

      setnames(d_stats, c("code", "description"), c("item", "item_name"))

      d_stats <- values$codelists$elements[d_stats, on = c("code" = "measuredElementTrade")]

      setnames(d_stats, c("code", "description"), c("element", "element_name"))

      setcolorder(d_stats, c("element", "element_name", "item", "item_name", "percentage", "min", "max", "median", "missing"))

      d_stats <- add_na_rows(d_stats, split = "element")

      rhandsontable(d_stats, selectCallback = TRUE, rowHeaders = FALSE) %>%
        hot_col("item_name", width = 400) %>%
        hot_col("element_name", width = 200) %>%
        hot_col("percentage", format = "0.0%", width = 100) %>%
        hot_col("min", format = "0,0", width = 100) %>%
        hot_col("max", format = "0,0", width = 100) %>%
        hot_col("median", format = "0,0", width = 100)
    })

  output$table_total <-
    renderRHandsontable({

      req(values$new_sws_data_total)

      d <- copy(values$new_sws_data_total)

      if (nrow(d) > 0) {
        # XXX: probably this just should throw an error so that everything stops
        # (the tool itself does not make sense if there are no totals)
        d <- d[CJ(geographicAreaM49 = unique(d$geographicAreaM49), measuredElementTrade = unique(d$measuredElementTrade), measuredItemCPC = unique(d$measuredItemCPC), timePointYears = as.character(values$query_years[1]:values$query_years[2])), on = c("geographicAreaM49", "measuredElementTrade", "measuredItemCPC", "timePointYears")]

        d <- mydenormalise(d)

        d <- set_standard_cols(d)

        spark <- add_sparkline(d, type = "total")

        d$plot <- spark$plot

        setcolorder(d, spark$order)

        col_headers <- set_hot_colnames(d)

        col_widths <- set_hot_colwidths(d, show = input$opt_hideflags)

        # Notice that there is an additional column fixed, with respect to bilaral plot
        rhandsontable(d, colHeaders = col_headers, selectCallback = TRUE, rowHeaders = FALSE) %>%
          hot_col("plot", renderer = htmlwidgets::JS("renderSparkline")) %>%
          hot_cols(fixedColumnsLeft = 5, colWidths = col_widths, columnSorting = TRUE) %>%
          hot_col(col_headers[grep("^\\d{4}$", col_headers)], format = "0,0")
      } else {
        rhandsontable(data.table(info = "No data"))
      }
    })

  observeEvent(
    input$table_bilat_select$select$r,
    {
      idrow <- input$table_bilat_select$select$r

      if (input$table_bilat_select$data[[idrow]][[2]] != "NA") {
        d <- as.data.table(hot_to_r(input$table_bilat))

        idcol <- input$table_bilat_select$select$c

        values$sel_bilat$partner  <- strsplit(d[idrow]$geographicAreaM49Partner, " - ")[[1]][1]
        values$query_item         <- d[idrow]$measuredItemCPC
        values$sel_bilat$element  <- d[idrow]$measuredElementTrade
        values$sel_bilat$year     <- sub(".*(\\d{4})$", "\\1", colnames(d[, idcol, with = FALSE]))

        partner <- paste(values$sel_bilat$partner, values$codelists$countries[code == values$sel_bilat$partner]$description, sep = " - ")

        updateSelectInput(session, "query_bil_partner", selected = partner)
      }
    }
  )

  observeEvent(
    input$undo,
    {
      n_modifs <- length(values$modif_history)

      if (n_modifs > 0) {
        values$new_sws_data_total <- values$modif_history[[n_modifs]]$total
        values$new_sws_data_bilat <- values$modif_history[[n_modifs]]$bilat

        if (as.character(n_modifs) %in% names(values$corrections_to_save)) {
          values$corrections_to_save <- values$corrections_to_save[setdiff(names(values$corrections_to_save), as.character(n_modifs))]
          values$mirror_to_save <- values$mirror_to_save[setdiff(names(values$mirror_to_save), as.character(n_modifs))]
        }

        if (n_modifs == 1) {
          values$modif_history <- list()
        } else {
          values$modif_history <- values$modif_history[-n_modifs]
        }
      } else {
        showModal(
          modalDialog(
            title = "Initial state",
            "There are no modifications to undo.",
            footer = modalButton("OK")
          )
        )
      }
    }
  )

  observeEvent(
    input$undo_bilat,
    {
      n_modifs <- length(values$modif_history)

      if (n_modifs > 0) {
        values$new_sws_data_total <- values$modif_history[[n_modifs]]$total
        values$new_sws_data_bilat <- values$modif_history[[n_modifs]]$bilat

        if (as.character(n_modifs) %in% names(values$corrections_to_save)) {
          values$corrections_to_save <- values$corrections_to_save[setdiff(names(values$corrections_to_save), as.character(n_modifs))]
          values$mirror_to_save <- values$mirror_to_save[setdiff(names(values$mirror_to_save), as.character(n_modifs))]
        }

        if (n_modifs == 1) {
          values$modif_history <- list()
        } else {
          values$modif_history <- values$modif_history[-n_modifs]
        }
      } else {
        showModal(
          modalDialog(
            title = "Initial state",
            "There are no modifications to undo.",
            footer = modalButton("OK")
          )
        )
      }
    }
  )

  observeEvent(
    input$table_total_info_select$select,
    {
      d <- hot_to_r(input$table_total_info)
      rowid <- input$table_total_info_select$select$r
      item <- d$item[rowid]
      element <- d$element[rowid]
      if (item != "" & element != "") {
        item <- paste(item, values$codelists$items[code == item]$description, sep = " - ")
        updateTabsetPanel(session, "tab_total", "Total")
        updateSelectInput(session, "query_item", selected = item)
        updateSelectInput(session, "query_flow", selected = ifelse(substr(element, 1, 2) == "56", "import", "export"))
      }
    }
  )
    
  observeEvent(
    input$table_outliers_select$select,
    {
      d <- as.data.table(hot_to_r(input$table_outliers))

      d[, measuredItemCPC := sub(" - .*", "", measuredItemCPC)]

      item <- d$measuredItemCPC[input$table_outliers_select$select$r]

      if (item != "NA") { # Yes, a character
        flow <- substr(d$measuredElementTrade[input$table_outliers_select$select$r], 1, 2)
        d <- d[measuredItemCPC == item & substr(measuredElementTrade, 1, 2) == flow]
        values$data_outliers <- melt.data.table(d, id.vars = c("measuredItemCPC", "measuredElementTrade"), value.name = "Value", variable.name = "timePointYears", variable.factor = FALSE)

        d_outliers <-
          detect_outliers(
            values$data_outliers,
            method = "simple",
            params = values$outliers_params
          )

        d_outliers[, value_outlier := ifelse(outlier == TRUE, Value, NA_real_)]

        showModal(
          modalDialog(
            renderPlotly({
              ggplot(d_outliers, aes(x = timePointYears, group = 1)) + geom_line(aes(y = Value)) + geom_point(aes(y = value_outlier, size = 2)) + facet_wrap(~measuredElementTrade, scales = 'free', nrow = 3) + scale_x_discrete(expand = c(0, 0))
            }),
            footer =
              tagList(
                actionButton("showtotals", "Analyze"),
                #actionButton("not_outlier", "Not an outlier")
                modalButton("Ignore")
              )
          )
        )
      }
    }
  )

  observeEvent(
    input$showtotals,
    {
      removeModal()
      updateTabsetPanel(session, "tab_total", "Total")
      item <- values$data_outliers$measuredItemCPC[1]
      item <- paste(item, values$codelists$items[code == item]$description, sep = " - ")
      updateSelectInput(session, "query_item", selected = item)
      updateSelectInput(session, "query_flow", selected = ifelse(substr(values$data_outliers$measuredElementTrade[1], 1, 2) == "56", "import", "export"))
    }
  )

  #observeEvent(
  #  input$table_bilat$changes$changes[[1]][[4]],
  #  {
  #    values$new_figure <- as.numeric(input$table_bilat$changes$changes[[1]][[4]][[1]])
  #  }
  #)

  output$welcome <-
    renderUI({
      list(
        h1("Trade validation"),
        p('This is an interactive tool for trade data validation.', 'In order to use the app you need to authenticate. Click on the "Token" button above and input a token taken from the completed trade dataset.'),
        p(),
        p("The tool has the following tabs:"),
        p(strong("Outliers:"), "All outliers."),
        p(strong("Main items:"), "The most important items for the country."),
        p(strong("Total:"), "Total and main bilateral flows."),
        p(strong("Bilateral:"), "Bilateral flow correction."),
        p(strong("TO COMMIT:"), "Save NEW corrections."),
        p(strong("Corrections to save:"), "Table with ALREADY SAVED corrections."),
        p(strong("Raw data / MAP:"), "To check raw data or change the mapping."),
        p(strong("Help:"), "Help page."),
        h2("Recommended workflow"),
        p("The recommended workflow is (you need to authenticate in advance, see above.):"),
        p("0. Select a country in the upper side of the tool."),
        p('1. Click on the "Outliers" tab. The items there are those that you necessarily need to check. Click on a cell on this table to select a specific item/flow.'),
        p('2. Click on the "Main items" tab. The items there are the most important for the country, so even if no outliers are found in these, a double check that data seems fine is useful. Click on a cell on this table to select a specific item/flow.'),
        p('3. Go to the "Total" tab and check that everything is OK. (If you clicked on one cell in the "Outlier" or "Main items" tab, this should happen automatically.)'),
        p('4. If the inspection on the "Total" tab suggest a correction, go to the "Bilateral" tab and do corrections there, by choosing the desired options. (KEEP YOUR BATCH OF INTERVENTIONS TO A SPECIFIC FLOW/ITEM COMBINATION. SEE BELOW.)'),
        p('5. Click on the "Apply correction" button in the "Bilateral tab" when you want to seee the effect of a correction.'),
        p('6. Inspect the plot in the "Bilateral" tab to see whether the correction had the expected effect on the bilateral flow.'),
        p('6. Inspect the tabs and plots in the "Total" tab to see whether the correction had the expected effect on the total.'),
        p('7. At this point you have two options:'),
        p('7.1. Undo the correction, by clicking on the "undo" button in the "Total" tab.'),
        p('7.2. Confirm the correction by clicking on the "Confirm correction" button in the "Bilateral" tab.'),
        p('8. Once you finish corrections go to the "TO COMMIT" tab and click on "Send to SWS datasets".'),
        p('9. You can now go back and start again from point 1 or 2, depending whether you want to act on identified outliers or main items, respectively.'),
        h2("Correct one item/flow at a time"),
        p('It is possible for you to accumulate different corrections, but only if you do not change the item/flow combination. If you do that, all corrections done in the session will be removed. So, correct one item/flow at a time, then when you are ready to commit changes click on "Send to SWS datasets" in the "TO COMMIT" tab, where you see the corrections ready to be committed.')
      )
    })

  output$debug <-
    renderText({
      paste(c("IGNORE THIS TAB.", input$corr_apply_correction, values$query_country, values$sel_bilat$partner, values$query_item, values$sel_bilat$element, values$sel_bilat$year, values$error), collapse = ",")
    })

  TokenValidator <-
    eventReactive(
      input$token,
      {
        validate(
          {
          need(
            grepl(
              sprintf("%1$s{8}-%1$s{4}-%1$s{4}-%1$s{4}-%1$s{11}", "[a-f0-9]"),
              input$token
            ),
            "Please supply a valid token")
          }
        )

        swsProgress_auth <- Progress$new(session, min = 0, max = 100)
        on.exit(swsProgress_auth$close())
        swsProgress_auth$set(value = 100, message = "Authenticating")

        SetClientFiles(CONFIG_CERTIFICATES)

        GetTestEnvironment(SERVER, input$token)

        validate(need(try(!is.null(swsContext.baseRestUrl)), "The token is invalid"))

        values$user <- strsplit(swsContext.username, "/")[[1]][[2]]

        values$email <- tolower(swsContext.userEmail)

        initial_country_code <- swsContext.datasets[[1]]@dimensions$geographicAreaM49Reporter@keys[1]

        values$fcl_2_cpc <- ReadDatatable("fcl2cpc_ver_2_1")

        values$people_in_charge <- ReadDatatable("ess_trade_people")$fao_email
        values$people_in_charge <- tolower(values$people_in_charge)
        values$people_in_charge <- gsub(" ", "", values$people_in_charge)

        values$codelists           <- list()
        values$codelists$elements  <- GetCodeList(DOMAIN, DATASET_TOT, "measuredElementTrade")[nchar(code) == 4]
        values$codelists$items     <- GetCodeList(DOMAIN, DATASET_TOT, "measuredItemCPC")[nchar(code) >= 4 & nchar(code) <= 8]
        values$codelists$countries <- GetCodeList(DOMAIN, DATASET_BIL, "geographicAreaM49Reporter")[type == "country"][order(description)]

        # Remove stuff we do not use; maybe this can be done positively, instead of negatively
        values$codelists$elements <- values$codelists$elements[!grepl("Re-", description)]
        values$codelists$elements <- values$codelists$elements[!grepl("m3|US|PJ|kg|#|1000 t|Unit]|l]", description)]
        values$codelists$elements <- values$codelists$elements[!grepl("Quantity$", description)]

        # XXX: how to remove more items? First idea was remove those with missing "type", but not all can be removed
        values$codelists$items <- values$codelists$items[!grepl("\\b(tree|fish|fertil)", description, ignore.case = TRUE)]
        values$codelists$items <- values$codelists$items[!grepl("Item|List|CPC|FBS|Agric|Produc|Populat|Total|metal", description)]

        setorderv(values$codelists$items, "code")

        values$codelists$items     <- values$codelists$items[, .(code, description)]
        values$codelists$elements  <- values$codelists$elements[, .(code, description)]
        values$codelists$countries <- values$codelists$countries[, .(code, description)]

        # XXX: remove for not these, once the standard gets enforced
        values$codelists$countries <- values$codelists$countries[!(code %in% c(156, 1249))]

        values$initial_country <- values$codelists$countries[code == initial_country_code, paste(code, "-", description)]

        # To be able to label the outliers based on some threshold over the
        # quantity (if the trade size of a commodity is really low, it is not
        # an interesting outlier for us), we will use the datatable below.
        # It contains the threshold identified for each country.
        values$outlier_thresholds <- ReadDatatable("trade_outlier_country_thresholds")

        values$do_not_check <- ReadDatatable("ess_trade_exclude_outlier_check")

        # XXX: this was supposed to cycle bewteen tabs so that bilateral is composed and can be updated later
        ##### updateTabsetPanel(session, "tab_total", "Total") # TODO: change "tab_total" name
        ##### updateTabsetPanel(session, "tab_total", "Bilateral")
        ##### updateTabsetPanel(session, "tab_total", "Welcome")

        return(TRUE)
      }
    )

  all_data_total <-
    reactive({

      req(values$query_country)

      items <- setdiff(values$codelists$items$code, values$do_not_check$cpc)

      key <-
        DatasetKey(
          domain = DOMAIN,
          dataset = DATASET_TOT,
          dimensions =
            list(
              geographicAreaM49    = Dimension("geographicAreaM49",    values$query_country),
              measuredElementTrade = Dimension("measuredElementTrade", values$codelists$elements$code),
              measuredItemCPC      = Dimension("measuredItemCPC",      items),
              timePointYears       = Dimension("timePointYears",       as.character((values$out_defaults$out_years[2] - 10):values$out_defaults$out_years[2]))
            )
        )

      swsProgress_out <- Progress$new(session, min = 0, max = 100)
      on.exit(swsProgress_out$close())
      swsProgress_out$set(value = 100, message = "Loading all data from SWS")

      d <- GetData(key)

      d <- exclude_items_more_elements(d, type = "total")

      ymax <- max(d$timePointYears)

      d <- d[CJ(geographicAreaM49 = unique(d$geographicAreaM49), measuredElementTrade = unique(d$measuredElementTrade), measuredItemCPC = unique(d$measuredItemCPC), timePointYears = as.character(values$query_years[1]:ymax)), on = c("geographicAreaM49", "measuredElementTrade", "measuredItemCPC", "timePointYears")]

      d[, any := any(!is.na(Value)), by = c("geographicAreaM49", "measuredElementTrade", "measuredItemCPC")]

      d <- d[any == TRUE][, any := NULL]

      setorderv(d, c("geographicAreaM49", "measuredItemCPC", "measuredElementTrade", "timePointYears"))

      d[]
    })

  output$table_outliers_ui <-
    renderUI({

      req(values$query_country, values$out_defaults)

      values$outliers_params <-
        list(
          country = values$query_country,
          elements = values$codelists$elements[, .(measuredElementTrade = code, description)],
          interval = 2009:2013,
          outlier_thresholds = values$outlier_thresholds,
          default_threshold = DEFAULT_THRESHOLD,
          outliers_ratio = values$out_defaults$out_ratio,
          outliers_growth = values$out_defaults$out_growth,
          min_year = values$out_defaults$out_years[1]
        )

      d <- copy(all_data_total())

      # XXX: parameterise interval (actually the mean at this point should be completely moving)
      values$outliers_found <- detect_outliers(d, method = "simple", params = values$outliers_params)

      threshold_used <- values$outliers_params$outlier_threshold[area == values$query_country]

      if (nrow(threshold_used) > 0) {
        values$threshold_used <- threshold_used$threshold
      } else {
        values$threshold_used <- DEFAULT_THRESHOLD
      }

      if (nrow(values$outliers_found) > 0 && TRUE %in% values$outliers_found$outlier) {

        # The fuction could return already the data, but in case it is generalised...
        d <- merge(d, values$outliers_found, by = names(d))

        d[, flow := substr(measuredElementTrade, 1, 2)]

        d[, show := any(outlier), by = c("geographicAreaM49", "measuredItemCPC", "flow")]

        d <- d[show == TRUE]

        setnames(d, c("flagObservationStatus", "flagMethod"), c("Status", "Method"))

        common <- c("measuredElementTrade", "measuredItemCPC", "timePointYears")

        d_data <- dcast.data.table(d[, c(common, "Value"), with = FALSE], measuredItemCPC + measuredElementTrade ~ timePointYears, value.var = "Value")
        d_out <- dcast.data.table(d[, c(common, "outlier"), with = FALSE], measuredItemCPC + measuredElementTrade ~ timePointYears, value.var = "outlier")

        setkey(d_data, NULL)
        setkey(d_out, NULL)

        d_data[, flow := substr(measuredElementTrade, 1, 2)]
        d_out[, flow := substr(measuredElementTrade, 1, 2)]

        d_data <- merge(d_data, values$codelists$items, by.x = "measuredItemCPC", by.y = "code", all.x = TRUE)

        d_data[!is.na(measuredItemCPC), measuredItemCPC := paste(measuredItemCPC, description, sep = " - ")]

        d_data[, description := NULL]

        d_data <- add_na_rows(d_data, split = c("flow", "measuredItemCPC"))
        d_out <- add_na_rows(d_out, split = c("flow", "measuredItemCPC"))

        d_data[, flow := NULL]
        d_out[, flow := NULL]

        m_out <- as.matrix(d_out)

        m_out[!grepl("TRUE", m_out)] <- NA

        m_out[grepl("TRUE", m_out)] <- "outlier"

        attributes(m_out)$dimnames <- NULL

        values$rendered_outliers <- list(d_data = d_data, m_out = m_out)

        rHandsontableOutput("table_outliers")
      } else {
        list(
          h1("No outliers found."),
          p('You may want in any case to have a look at the "Main items"')
        )
      }

    })

  # FIXME: the table should be blanked when the country is changed
  output$table_outliers <-
    renderRHandsontable({

      req(values$query_country, values$rendered_outliers)

      rhandsontable(
        values$rendered_outliers$d_data,
        comments = values$rendered_outliers$m_out,
        rowHeaders = FALSE,
        selectCallback = TRUE) %>%
          hot_col(names(values$rendered_outliers$d_data)[grep("^\\d{4}$", names(values$rendered_outliers$d_data))], format = "0,0") %>%
          hot_col("measuredItemCPC", width = "300")

    })

  output$tokenValidator <-
    renderText({

      req(TokenValidator())

      print(paste("HELLO", input$token))
    })

  observeEvent(
    input$gettoken,
    {
      showModal(
        modalDialog(
          textInput("token", "Enter token below, then press OK."),
          title = "Authentication",
          "Please enter a token obtained in the completed trade dataset + TRADEvalidation plugin",
          footer = modalButton("OK")
        )
      )
    }
  )


  output$debug1 <-
    renderText({

      req(input$token)

      print(ifelse(exists("swsContext.baseRestUrl") && !is.null(swsContext.username), swsContext.username, "No connection."))
    })

  observeEvent(
    c(input$table_bilat, input$table_total),
    {
      session$onFlushed(function() {
        session$sendCustomMessage(type='jsCode', list(value = myscript))
      })
    }
  )

  observeEvent(
    input$query_flow,
    {
      values$corrections_to_save <- list()
      values$mirror_to_save <- list()
      values$corr_cant_correct <- FALSE
    }
  )

  observeEvent(
    input$query_item,
    {
      values$query_item <- strsplit(input$query_item, " - ")[[1]][1]
      values$corrections_to_save <- list()
      values$mirror_to_save <- list()
      values$corr_cant_correct <- FALSE
    }
  )

  observeEvent(
    input$query_country,
    {
      values$query_country <- strsplit(input$query_country, " - ")[[1]][1]
      values$corrections_to_save <- list()
      values$mirror_to_save <- list()
      values$corr_cant_correct <- FALSE
    }
  )

  observeEvent(
    input$query_bil_partner,
    {
      values$query_bil_partner <- strsplit(input$query_bil_partner, " - ")[[1]][1]

      values$data_mirr_bilat <- {

        req(TokenValidator(), values$query_country, values$query_item,
            input$query_flow, values$query_years, input$query_bil_partner)

        d <- data_corr_bilat_plots()

        flows_orig <- unique(d$measuredElementTrade)

        flows_mirr <- sub(ifelse(input$query_flow == "import", "56", "59"), ifelse(input$query_flow == "import", "59", "56"), flows_orig)

        key <-
          DatasetKey(
            domain = DOMAIN,
            dataset = DATASET_BIL,
            dimensions =
              list(
                geographicAreaM49Reporter = Dimension("geographicAreaM49Reporter", values$query_bil_partner),
                geographicAreaM49Partner  = Dimension("geographicAreaM49Partner",  values$query_country),
                measuredElementTrade      = Dimension("measuredElementTrade",      flows_mirr),
                measuredItemCPC           = Dimension("measuredItemCPC",           values$query_item),
                timePointYears            = Dimension("timePointYears",            as.character(values$query_years[1]:values$query_years[2]))
              )
          )

        swsProgress_mirr_bilat <- Progress$new(session, min = 0, max = 100)
        on.exit(swsProgress_mirr_bilat$close())
        swsProgress_mirr_bilat$set(value = 100, message = "Loading bilateral mirror data from SWS")

        # TODO: error handling
        d <- GetData(key)

        d <- exclude_items_more_elements(d, type = "bilateral")

        if (nrow(d) > 0) {

          d <- d[CJ(geographicAreaM49Reporter = unique(d$geographicAreaM49Reporter), geographicAreaM49Partner = unique(d$geographicAreaM49Partner), measuredElementTrade = unique(d$measuredElementTrade), measuredItemCPC = unique(d$measuredItemCPC), timePointYears = as.character(values$query_years[1]:values$query_years[2])), on = c("geographicAreaM49Reporter", "geographicAreaM49Partner", "measuredElementTrade", "measuredItemCPC", "timePointYears")]
        }

        d[]

      }
    }
  )

  observeEvent(
    input$corr_choose_correction,
    {
      values$multiplecorrections <- FALSE
    }
  )

  observeEvent(
    input$corr_okCorrection,
    {
      removeModal()

      if (values$multiplecorrections == FALSE) {
        corr_note <-
          switch(
            input$corr_choose_correction,
            'None'                = NA_character_,
            'Measurement factor'  = NA_character_,
            'Mirror flow'         = NA_character_,
            'Outlier correction'  = names(which(values$corr_opts_out_method == input$corr_correction_outlier)),
            'Publication/website' = NA_character_,
            'Expert knowledge'    = NA_character_
          )

        d_orig <-
          values$data_bilat[
            geographicAreaM49Partner == values$sel_bilat$partner &
              measuredItemCPC == values$query_item &
              timePointYears == values$sel_bilat$year &
              substr(measuredElementTrade, 3, 3) != 3
          ]

        if (input$corr_variable2correct == "Value") {
          d_orig <- d_orig[substr(measuredElementTrade, 3, 3) == "2"]
        } else {
          d_orig <- d_orig[substr(measuredElementTrade, 3, 3) != "2"]
        }

        mycorrection <-
          data.table(
            reporter         = values$query_country,
            partner          = values$query_bil_partner,
            year             = as.integer(input$corr_year2correct),
            item             = values$query_item,
            flow             = ifelse(input$query_flow == "import", 1L, 2L),
            data_original    = d_orig$Value,
            data_type        = ifelse(input$corr_variable2correct == "Quantity", "qty", "value"),
            correction_level = "CPC",
            correction_hs    = NA_character_,
            correction_input = values$new_figure,
            correction_type  = input$corr_choose_correction,
            correction_note  = corr_note,
            note_analyst     = input$corr_note_by_analyst,
            note_supervisor  = NA_character_,
            name_analyst     = tolower(sub("@.*", "", values$email)),
            name_supervisor  = NA_character_,
            date_correction  = format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),
            date_validation  = NA_character_
          )

        # linked to undo
        values$corrections_to_save[[as.character(length(values$modif_history))]] <- mycorrection

        ########### MIRROR
        d_mirr_bil <- values$data_mirr_bilat[timePointYears == values$sel_bilat$year]

        d_mirr_bil[, to_fix := FALSE]
        if (input$corr_variable2correct == "Value") {
          d_mirr_bil[substr(measuredElementTrade, 3, 3) != 3, to_fix := substr(measuredElementTrade, 3, 3) == 2]
        } else {
          d_mirr_bil[substr(measuredElementTrade, 3, 3) != 3, to_fix := substr(measuredElementTrade, 3, 3) != 2]
        }
            

        if (nrow(d_mirr_bil[to_fix == TRUE & flagObservationStatus == "T"]) > 0) {
          d_mirr_bil[, to_fix := NULL]

          if (input$corr_variable2correct == "Value") {
            if (input$query_flow == "import") {
              new_mirror_value <- values$new_figure / CIFFOB
            } else {
              new_mirror_value <- values$new_figure * CIFFOB
            }

            d_mirr_bil[
              substr(measuredElementTrade, 3, 3) == 2,
              `:=`(
                Value                 = new_mirror_value,
                flagObservationStatus = "T",
                flagMethod            = "i"
              )
            ]

            # XXX 3 elements...
            uv <- new_mirror_value / d_mirr_bil[!(substr(measuredElementTrade, 3, 3) %in% 2:3)]$Value * 1000
          } else {
            d_mirr_bil[
              !(substr(measuredElementTrade, 3, 3) %in% 2:3),
              `:=`(
                Value                 = values$new_figure,
                flagObservationStatus = "T",
                flagMethod            = "c"
              )
            ]

            uv <- d_mirr_bil[substr(measuredElementTrade, 3, 3) == 2]$Value / values$new_figure * 1000
          }

          d_mirr_bil[substr(measuredElementTrade, 3, 3) == "3", Value := uv]

          # TODO: update UV flags, even though they probably stay the same

          values$mirror_to_save[[as.character(length(values$modif_history))]] <- d_mirr_bil
        }

      } else {

        mycorrection <-
          data.table(
            reporter         = values$query_country,
            partner          = NA_character_, # To be changed below
            year             = as.integer(input$corr_year2correct),
            item             = values$query_item,
            flow             = ifelse(input$query_flow == "import", 1L, 2L),
            data_original    = NA_real_, # To be changed below
            data_type        = ifelse(input$corr_variable2correct == "Quantity", "qty", "value"),
            correction_level = "CPC",
            correction_hs    = NA_character_,
            correction_input = NA_real_, # To be changed below
            correction_type  = input$corr_choose_correction, # To be changed below
            correction_note  = "Multiple corrections",
            note_analyst     = input$corr_note_by_analyst,
            note_supervisor  = NA_character_,
            name_analyst     = tolower(sub("@.*", "", values$email)),
            name_supervisor  = NA_character_,
            date_correction  = format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),
            date_validation  = NA_character_
          )

        # linked to undo: XXX: should be a list of corrections for multiple corrections?
        values$corrections_to_save[[as.character(length(values$modif_history))]] <- list()

        if (input$corr_choose_correction == "Mirror flow") {
          mycorrection[, correction_type := "Mirror flow"]

          for (mypartner in unique(values$data_multiple$geographicAreaM49Partner)) {
            mycorrection_partner = copy(mycorrection)

            d <- values$data_multiple[geographicAreaM49Partner == mypartner & substr(measuredElementTrade, 3, 3) != 3]

            d_orig <-
              values$data_bilat[
                geographicAreaM49Partner == mypartner &
                  measuredItemCPC == values$query_item &
                  timePointYears == values$sel_bilat$year &
                  substr(measuredElementTrade, 3, 3) != 3
              ]

            if (input$corr_variable2correct == "Value") {
              d      <- d[substr(measuredElementTrade, 3, 3) == "2"]
              d_orig <- d_orig[substr(measuredElementTrade, 3, 3) == "2"]
            } else {
              d      <- d[substr(measuredElementTrade, 3, 3) != "2"]
              d_orig <- d_orig[substr(measuredElementTrade, 3, 3) != "2"]
            }
            
            mycorrection_partner[,
              `:=`(
                partner = mypartner,
                data_original = d_orig$Value,
                correction_input = d$Value
              )
            ]

            values$corrections_to_save[[as.character(length(values$modif_history))]][[mypartner]] <- mycorrection_partner
          }

        } else if (input$corr_choose_correction == "Measurement factor") {
          mycorrection[, correction_type := "Measurement factor"]


          values$mirror_to_save[[as.character(length(values$modif_history))]] <- list()


          elements <- substr(unique(values$data_multiple$measuredElementTrade), 3, 4)

          # NOTE: it's the opposite
          elements <- paste0(ifelse(input$query_flow == "import", "59", "56"), elements)

          key <-
            DatasetKey(
              domain = DOMAIN,
              dataset = DATASET_BIL,
              dimensions =
                list(
                  geographicAreaM49Reporter = Dimension("geographicAreaM49Reporter", unique(values$data_multiple$geographicAreaM49Partner)),
                  geographicAreaM49Partner  = Dimension("geographicAreaM49Partner",  values$query_country),
                  measuredElementTrade      = Dimension("measuredElementTrade",      elements),
                  measuredItemCPC           = Dimension("measuredItemCPC",           values$query_item),
                  timePointYears            = Dimension("timePointYears",            input$corr_year2correct)
                )
            )

          swsProgress_mirr3 <- Progress$new(session, min = 0, max = 100)
          on.exit(swsProgress_mirr3$close())
          swsProgress_mirr3$set(value = 100, message = "Loading mirror data from SWS")

          d_mirr <- GetData(key)

          d_mirr <- exclude_items_more_elements(d_mirr, type = "bilateral")

          for (mypartner in unique(values$data_multiple$geographicAreaM49Partner)) {
            mycorrection_partner = copy(mycorrection)

            d <- values$data_multiple[geographicAreaM49Partner == mypartner & substr(measuredElementTrade, 3, 3) != 3]

            d_orig <-
              values$data_bilat[
                geographicAreaM49Partner == mypartner &
                  measuredItemCPC == values$query_item &
                  timePointYears == values$sel_bilat$year &
                  substr(measuredElementTrade, 3, 3) != 3
              ]

            if (input$corr_variable2correct == "Value") {
              d      <- d[substr(measuredElementTrade, 3, 3) == "2"]
              d_orig <- d_orig[substr(measuredElementTrade, 3, 3) == "2"]
            } else {
              d      <- d[substr(measuredElementTrade, 3, 3) != "2"]
              d_orig <- d_orig[substr(measuredElementTrade, 3, 3) != "2"]
            }
            
            mycorrection_partner[,
              `:=`(
                partner = mypartner,
                data_original = d_orig$Value,
                correction_input = d$Value
              )
            ]

            values$corrections_to_save[[as.character(length(values$modif_history))]][[mypartner]] <- mycorrection_partner

            d_mirr_bil <- d_mirr[geographicAreaM49Reporter == mypartner]

            d_mirr_bil[, to_fix := FALSE]
            if (input$corr_variable2correct == "Value") {
              d_mirr_bil[substr(measuredElementTrade, 3, 3) != 3, to_fix := substr(measuredElementTrade, 3, 3) == 2]
            } else {
              d_mirr_bil[substr(measuredElementTrade, 3, 3) != 3, to_fix := substr(measuredElementTrade, 3, 3) != 2]
            }
            
            ########### MIRROR
            if (nrow(d_mirr_bil[to_fix == TRUE & flagObservationStatus == "T"]) > 0) {

              d_mirr_bil[, to_fix := NULL]

              if (input$corr_variable2correct == "Value") {
                if (input$query_flow == "import") {
                  new_mirror_value <- d$Value / CIFFOB
                } else {
                  new_mirror_value <- d$Value * CIFFOB
                }

                d_mirr_bil[
                  substr(measuredElementTrade, 3, 3) == 2,
                  `:=`(
                    Value                 = new_mirror_value,
                    flagObservationStatus = "T",
                    flagMethod            = "i"
                  )
                ]

                # XXX 3 elements...
                uv <- new_mirror_value / d_mirr_bil[!(substr(measuredElementTrade, 3, 3) %in% 2:3)]$Value * 1000
              } else {
                d_mirr_bil[
                  !(substr(measuredElementTrade, 3, 3) %in% 2:3),
                  `:=`(
                    Value                 = d$Value,
                    flagObservationStatus = "T",
                    flagMethod            = "c"
                  )
                ]

                uv <- d_mirr_bil[substr(measuredElementTrade, 3, 3) == 2]$Value / d$Value * 1000
              }

              d_mirr_bil[substr(measuredElementTrade, 3, 3) == "3", Value := uv]

              # TODO: update UV flags, even though they probably stay the same

              values$mirror_to_save[[as.character(length(values$modif_history))]][[mypartner]] <- d_mirr_bil

            }
          }
        }
      }
    }
  )

  observeEvent(
    input$corr_year2correct,
    {
      values$sel_bilat$year <- input$corr_year2correct
      values$corr_cant_correct <- FALSE
    }
  )

  observeEvent(
    values$sel_bilat$partner,
    {
      req(TokenValidator(), values$query_country, values$query_item,
          input$query_flow, values$query_years, input$query_bil_partner,
          input$corr_year2correct, input$corr_variable2correct)

      if (input$corr_year2correct != '') { # XXX in req()?
        output$corr_history <- renderRHandsontable({

          if (input$corr_variable2correct == "Value") {
            element <- ifelse(input$query_flow == "import", "5622", "5922")
          } else {
            element <- data_corr_bilat_plots()[!is.na(Value) & !(substr(measuredElementTrade, 3, 3) %in% 2:3) & geographicAreaM49Partner == values$sel_bilat$partner][, .N, measuredElementTrade][order(-N)][1]$measuredElementTrade
          }

          key <-
            DatasetKey(
              domain = DOMAIN,
              dataset = "completed_tf_cpc_m49",
              dimensions =
                list(
                  geographicAreaM49Reporter = Dimension("geographicAreaM49Reporter", values$query_country),
                  geographicAreaM49Partner  = Dimension("geographicAreaM49Partner",  values$query_bil_partner),
                  measuredElementTrade      = Dimension("measuredElementTrade",      element),
                  measuredItemCPC           = Dimension("measuredItemCPC",           values$query_item),
                  timePointYears            = Dimension("timePointYears",            as.character(input$corr_year2correct))
                )
            )

          swsProgress_hist <- Progress$new(session, min = 0, max = 100)
          on.exit(swsProgress_hist$close())
          swsProgress_hist$set(value = 100, message = "Loading history from SWS")

          # TODO: error handling
          d <- GetHistory(key)

          # TODO: add column widths
          if (nrow(d) > 0) {
            d[, Date := as.POSIXct(StartDate/1000, origin="1970-01-01")]

            rhandsontable(d[, .SD[, .(Date, Value, flagObservationStatus, flagMethod)][1], .(Version = as.character(Version))], rowHeaders = FALSE) %>%
              hot_col("Value", format = "0,0")
          } else {
            rhandsontable(data.table(info = "No history"), rowHeaders = FALSE)
          }
        })
      }
    }
  )

  observeEvent(
    input$query_bil_partner,
    {
      values$sel_bilat$partner <- strsplit(input$query_bil_partner, " - ")[[1]][1]
      values$corr_cant_correct <- FALSE
    }
  )

  observeEvent(
    input$corr_variable2correct,
    {
      # Obtain the correct qty element
      element <- values$new_sws_data_bilat[!(substr(measuredElementTrade, 3, 3) %in% 2:3), .N, measuredElementTrade][order(-N)]$measuredElementTrade[1]

      if (length(element) > 1) {
        stop("XXX lksjslksjs")
      }

      if (input$corr_variable2correct == "Value") {
        element <- paste0(substr(element, 1, 2), "22")
      }

      values$sel_bilat$element <- element
      values$corr_cant_correct <- FALSE
    }
  )

  # NOTE: there is another observer on corr_apply_correction on purpose ("duplicated code" issue)
  observeEvent(
    input$corr_apply_correction,
    {
      if (values$multiplecorrections == FALSE) {
        if (input$corr_choose_correction == "None") {
          d_orig <-
            values$data_bilat[
              geographicAreaM49Partner == values$sel_bilat$partner &
                measuredItemCPC == values$query_item &
                timePointYears == values$sel_bilat$year &
                substr(measuredElementTrade, 3, 3) != 3
            ]

          if (input$corr_variable2correct == "Value") {
            values$new_figure <- d_orig[substr(measuredElementTrade, 3, 3) == 2]$Value
          } else {
            values$new_figure <- d_orig[substr(measuredElementTrade, 3, 3) != 2]$Value
          }
        } else {
          values$new_figure <-
            switch(
              input$corr_choose_correction,
              "Measurement factor"  = values$new_10,
              "Mirror flow"         = values$new_mirror,
              "Outlier correction"  = input$corr_correction_outlier,
              "Publication/website" = input$corr_correction_publication,
              "Expert knowledge"    = input$corr_correction_expert
            )

        }

        values$new_figure <- as.numeric(values$new_figure)
      } else {
        values$new_figure <- NULL
      }
    }
  )

  observeEvent(
    input$corr_apply_correction,
    {
      values$modif_history[[length(values$modif_history) + 1]] <-
        list(total = values$new_sws_data_total, bilat = values$new_sws_data_bilat)


      if (values$multiplecorrections == FALSE) {

        d_bilat <- copy(values$new_sws_data_bilat)

        if (input$corr_choose_correction == "None") {
          d_orig <-
            values$data_bilat[
              geographicAreaM49Partner == values$sel_bilat$partner &
                measuredItemCPC == values$query_item &
                timePointYears == values$sel_bilat$year &
                substr(measuredElementTrade, 3, 3) != 3
            ]

          if (input$corr_variable2correct == "Value") {
            d_orig <- d_orig[substr(measuredElementTrade, 3, 3) == "2"]
          } else {
            d_orig <- d_orig[substr(measuredElementTrade, 3, 3) != "2"]
          }

          new_flag_observ <- d_orig$flagObservationStatus
          new_flag_method <- d_orig$flagMethod
        } else if (input$corr_choose_correction == "Publication/website") {
          pub_flags <- publication_sources[source == input$corr_note_by_analyst]
          new_flag_observ <- pub_flags$flag_obs
          new_flag_method <- pub_flags$flag_method
        } else if (input$corr_choose_correction == "Expert knowledge") {
          new_flag_observ <- "E"
          new_flag_method <- "f"
        } else if (input$corr_choose_correction == "Mirror flow") {
          new_flag_observ <- "T"
          new_flag_method <- "c"
        } else {
          new_flag_observ <- "I"
          new_flag_method <- "e"
        }

        d_bilat[
          geographicAreaM49Reporter  == values$query_country &
            geographicAreaM49Partner == values$sel_bilat$partner &
            measuredElementTrade     == values$sel_bilat$element &
            measuredItemCPC          == values$query_item &
            timePointYears           == values$sel_bilat$year,
          `:=`(
            Value                 = values$new_figure,
            flagObservationStatus = new_flag_observ,
            flagMethod            = new_flag_method
          )
        ]

        # Update unit value
        d_bilat[
          geographicAreaM49Partner == values$sel_bilat$partner &
            timePointYears == values$sel_bilat$year,
          `:=`(
            Value =
              ifelse(
                substr(measuredElementTrade, 3, 3) == "3",
                Value[substr(measuredElementTrade, 3, 3) == "2"] / Value[!(substr(measuredElementTrade, 3, 3) %in% 2:3)] * 1000,
                Value
              ),
            flagObservationStatus =
              ifelse(
                substr(measuredElementTrade, 3, 3) == "3",
                aggregateObservationFlag(flagObservationStatus[substr(measuredElementTrade, 3, 3) != "3"], flagTable = flagWeightTable_status),
                flagObservationStatus
              ),
            flagMethod = ifelse(substr(measuredElementTrade, 3, 3) == "3", "i", flagMethod)
          )
        ]
      } else {

        d_bilat <- values$new_sws_data_bilat[timePointYears == input$corr_year2correct]

        if (input$corr_choose_correction == "Mirror flow") {

          uvs <- d_bilat[substr(measuredElementTrade, 3, 3) == 3]

          d_bilat <- d_bilat[substr(measuredElementTrade, 3, 3) != 3]

          elements <- substr(unique(d_bilat$measuredElementTrade), 3, 4)

          # NOTE: it's the opposite
          elements <- paste0(ifelse(input$query_flow == "import", "59", "56"), elements)

          key <-
            DatasetKey(
              domain = DOMAIN,
              dataset = DATASET_BIL,
              dimensions =
                list(
                  geographicAreaM49Reporter = Dimension("geographicAreaM49Reporter", unique(d_bilat$geographicAreaM49Partner)),
                  geographicAreaM49Partner  = Dimension("geographicAreaM49Partner",  values$query_country),
                  measuredElementTrade      = Dimension("measuredElementTrade",      elements),
                  measuredItemCPC           = Dimension("measuredItemCPC",           values$query_item),
                  timePointYears            = Dimension("timePointYears",            input$corr_year2correct)
                )
            )

          swsProgress_mirr2 <- Progress$new(session, min = 0, max = 100)
          on.exit(swsProgress_mirr2$close())
          swsProgress_mirr2$set(value = 100, message = "Loading mirror data from SWS")

          d_mirr <- GetData(key)

          d_mirr <- exclude_items_more_elements(d_mirr, type = "bilateral")

          d_mirr <- d_mirr[, .(geographicAreaM49Partner = geographicAreaM49Reporter, element = substr(measuredElementTrade, 3, 4), Value_mirr = Value)]

          d_bilat[, element := substr(measuredElementTrade, 3, 4)]

          d_bilat <- merge(d_bilat, d_mirr, by = c("geographicAreaM49Partner", "element"), all.x = TRUE)

          d_bilat[, element := NULL]

          if (input$corr_variable2correct == "Value") {
            d_bilat[
              substr(measuredElementTrade, 3, 3) == "2" & !is.na(Value_mirr),
              `:=`(
                Value = Value_mirr * ifelse(input$query_flow == "import", CIFFOB, 1/CIFFOB),
                flagObservationStatus = "T",
                flagMethod = "i"
              )
            ]
          } else {
            d_bilat[
              !(substr(measuredElementTrade, 3, 3) %in% 2:3) & !is.na(Value_mirr),
              `:=`(
                Value = Value_mirr,
                flagObservationStatus = "T",
                flagMethod = "c"
              )
            ]
          }

          d_bilat[, Value_mirr := NULL]

          d_bilat <- rbind(d_bilat, uvs)

        } else if (input$corr_choose_correction == "Measurement factor") {
          uvs <- d_bilat[substr(measuredElementTrade, 3, 3) == 3]

          d_bilat <- d_bilat[substr(measuredElementTrade, 3, 3) != 3]

          if (input$corr_variable2correct == "Value") {
            d_bilat[
              substr(measuredElementTrade, 3, 3) == "2",
              `:=`(
                Value = Value * as.numeric(input$corr_correction10),
                flagObservationStatus = "I",
                flagMethod = "e"
              )
            ]
          } else {
            d_bilat[
              substr(measuredElementTrade, 3, 3) != "2",
              `:=`(
                Value = Value * as.numeric(input$corr_correction10),
                flagObservationStatus = "I",
                flagMethod = "e"
              )
            ]
          }

          d_bilat <- rbind(d_bilat, uvs)

        }

        d_bilat[,
          `:=`(
            Value =
              ifelse(
                substr(measuredElementTrade, 3, 3) == "3",
                Value[substr(measuredElementTrade, 3, 3) == "2"] / Value[!(substr(measuredElementTrade, 3, 3) %in% 2:3)] * 1000,
                Value
              ),
            flagObservationStatus =
              ifelse(
                substr(measuredElementTrade, 3, 3) == "3",
                aggregateObservationFlag(flagObservationStatus, flagTable = flagWeightTable_status),
                flagObservationStatus
              ),
            flagMethod = ifelse(substr(measuredElementTrade, 3, 3) == "3", "i", flagMethod)
          ),
          by = "geographicAreaM49Partner"
        ]

        values$data_multiple <- d_bilat

        d_bilat <-
          rbind(
            d_bilat,
            values$new_sws_data_bilat[timePointYears != input$corr_year2correct]
          )
      }

      d_total <- copy(values$new_sws_data_total)

      d_total_sub <-
        d_bilat[
          timePointYears == values$sel_bilat$year,
          .(
            new_Value = sum(Value), # Yes, even UV, will be recalculated below
            # XXX Japan case
            new_flagObservationStatus = aggregateObservationFlag(flagObservationStatus, flagTable = flagWeightTable_status),
            new_flagMethod = ifelse(.N > 1, "s", flagMethod)
          ),
          by = .(geographicAreaM49 = geographicAreaM49Reporter, measuredElementTrade, measuredItemCPC, timePointYears)
        ]

      if (nrow(d_total_sub) > 3) {
        values$error <- "XXX MORE THAN THREE ELEMENTS, total"
      }

      uv <- d_total_sub[grepl("22$", measuredElementTrade)]$new_Value / d_total_sub[!grepl("..[23].", measuredElementTrade)]$new_Value * 1000

      # TODO: aggregate obs flag
      d_total_sub[grepl("..3.", measuredElementTrade), `:=`(new_Value = uv, new_flagMethod = "i")]

      d_total <- merge(d_total, d_total_sub, by = c("geographicAreaM49", "measuredElementTrade", "measuredItemCPC", "timePointYears"), all.x = TRUE)

      d_total[!is.na(new_Value), Value := new_Value]
      d_total[!is.na(new_flagObservationStatus), flagObservationStatus := new_flagObservationStatus]
      d_total[!is.na(new_flagMethod), flagMethod := new_flagMethod]

      d_total[, c("new_Value", "new_flagObservationStatus", "new_flagMethod") := NULL]

      # Synchronize
      values$new_sws_data_bilat <- copy(d_bilat)
      values$new_sws_data_total <- copy(d_total)
    }
  )


  ## XXX: see duplicated code issue
  #observeEvent(
  #  input$table_bilat$changes$changes[[1]][[4]],
  #  {
  #    values$modif_history[[length(values$modif_history) + 1]] <-
  #      list(total = values$new_sws_data_total, bilat = values$new_sws_data_bilat)

  #    d_bilat <- copy(values$new_sws_data_bilat)

  #    # TODO: mirror
  #    d_bilat[
  #      geographicAreaM49Reporter  == values$query_country &
  #        geographicAreaM49Partner == values$sel_bilat$partner &
  #        measuredElementTrade     == values$sel_bilat$element &
  #        measuredItemCPC          == values$query_item &
  #        timePointYears           == values$sel_bilat$year,
  #      `:=`(Value = values$new_figure, flagObservationStatus = "I", flagMethod = "e")
  #    ]

  #    d_bilat[
  #      geographicAreaM49Partner == values$sel_bilat$partner & timePointYears == values$sel_bilat$year,
  #      `:=`(
  #        Value =
  #          ifelse(
  #            substr(measuredElementTrade, 3, 3) == "3",
  #            Value[substr(measuredElementTrade, 3, 3) == "2"] / Value[!(substr(measuredElementTrade, 3, 3) %in% 2:3)] * 1000,
  #            Value
  #          ),
  #        flagObservationStatus =
  #          ifelse(
  #            substr(measuredElementTrade, 3, 3) == "3",
  #            aggregateObservationFlag(flagObservationStatus[substr(measuredElementTrade, 3, 3) != "3"], flagTable = flagWeightTable_status),
  #            flagObservationStatus
  #          ),
  #        flagMethod = ifelse(substr(measuredElementTrade, 3, 3) == "3", "i", flagMethod)
  #      )
  #    ]

  #    d_total <- copy(values$new_sws_data_total)

  #    d_total_sub <-
  #      d_bilat[
  #        timePointYears == values$sel_bilat$year,
  #        .(
  #          new_Value = sum(Value), # Yes, even UV, will be recalculated below
  #          # XXX Japan case
  #          new_flagObservationStatus = aggregateObservationFlag(flagObservationStatus, flagTable = flagWeightTable_status),
  #          new_flagMethod = ifelse(.N > 1, "s", flagMethod)
  #        ),
  #        by = .(geographicAreaM49 = geographicAreaM49Reporter, measuredElementTrade, measuredItemCPC, timePointYears)
  #      ]

  #    if (nrow(d_total_sub) > 3) {
  #      values$error <- "XXX MORE THAN THREE ELEMENTS, total"
  #    }

  #    uv <- d_total_sub[grepl("22$", measuredElementTrade)]$new_Value / d_total_sub[!grepl("..[23].", measuredElementTrade)]$new_Value * 1000

  #    d_total_sub[grepl("..3.", measuredElementTrade), `:=`(new_Value = uv, new_flagMethod = "i")]

  #    d_total <- merge(d_total, d_total_sub, by = c("geographicAreaM49", "measuredElementTrade", "measuredItemCPC", "timePointYears"), all.x = TRUE)

  #    d_total[!is.na(new_Value), Value := new_Value]
  #    d_total[!is.na(new_flagObservationStatus), flagObservationStatus := new_flagObservationStatus]
  #    d_total[!is.na(new_flagMethod), flagMethod := new_flagMethod]

  #    d_total[, c("new_Value", "new_flagObservationStatus", "new_flagMethod") := NULL]

  #    # Synchronize
  #    values$new_sws_data_bilat <- copy(d_bilat)
  #    values$new_sws_data_total <- copy(d_total)
  #  }
  #)

  observeEvent(
    input$query_country,
    {
      req(values$query_country)

      f <- file.path(CORRECTIONS_DIR, values$query_country, "corrections_table.rds")

      if (file.exists(f) == FALSE) {
        dir.create(dirname(f))
        i <- 1
        while(i <= 100 & file.exists(f) == FALSE) {
          country <- sample(values$codelists$countries$code, 1)
          if (file.exists(file.path(CORRECTIONS_DIR, country, "corrections_table.rds"))) {
            x <- as.data.table(readRDS(file.path(CORRECTIONS_DIR, country, "corrections_table.rds")))
            x <- dplyr::as.tbl(x[0])
            save_rds(x, f)
          }
          i <- i + 1
        }
      }

      # NOTE: the data is original stored as tibble. It will be saved
      # again as tibble, but will be worked on as data.table
      d <- as.data.table(readRDS(file.path(CORRECTIONS_DIR, values$query_country, "corrections_table.rds")))
      values$corrections <- d
    }
  )

  observeEvent(
    input$send_to_datasets,
    {
      if (length(values$corrections_to_save) > 0) {

        swsProgress_save <- Progress$new(session, min = 0, max = 100)
        on.exit(swsProgress_save$close())
        swsProgress_save$set(value = 100, message = "Saving data")

        new_corrections <- unique(rbindlist2(values$corrections_to_save))

        corrected_mirror <- unique(rbindlist2(values$mirror_to_save))

        new_corrections[,
          corrections_metadata :=
            apply(
              new_corrections[,
                c("name_analyst", "data_original", "correction_type",
                  "correction_note", "note_analyst", "note_supervisor",
                  "name_analyst", "name_supervisor", "date_correction",
                  "date_validation"),
                with = FALSE
              ],
              1,
              function(x) 
                trimws(gsub('  *', ' ',
                            paste(names(x), ifelse(x == '', NA, x),
                                  collapse = '; ', sep = ': ')))
            )
        ]

        d_bil_metad <- copy(values$new_sws_data_bilat)

        orig_order <- names(d_bil_metad)

        d_bil_metad[, data_type := "UV"]
        d_bil_metad[substr(measuredElementTrade, 3, 3) == "2", data_type := "value"]
        d_bil_metad[!(substr(measuredElementTrade, 3, 3) %in% 2:3), data_type := "qty"]

        d_bil_metad[, timePointYears := as.integer(timePointYears)]
        d_bil_metad[, flow := ifelse(substr(measuredElementTrade, 1, 2) == "56", 1L, 2L)]

        d_bil_metad <-
          merge(
            d_bil_metad,
            new_corrections[, .(reporter, partner, flow, item, year, data_type, corrections_metadata)],
            by.x = c("geographicAreaM49Reporter", "geographicAreaM49Partner",
                     "flow", "measuredItemCPC", "timePointYears", "data_type"),
            by.y = c("reporter", "partner", "flow", "item", "year", "data_type")
          )

        d_bil_metad <-
          d_bil_metad[,
            .(
              geographicAreaM49Reporter,
              geographicAreaM49Partner,
              measuredElementTrade,
              measuredItemCPC,
              timePointYears    = as.character(timePointYears),
              Metadata          = "GENERAL",
              Metadata_Element  = "COMMENT",
              Metadata_Language = "en",
              Metadata_Value    = corrections_metadata
            )
          ]

        d_bil <- copy(values$new_sws_data_bilat)

        d_bil <-
          d_bil[
            d_bil_metad[, .(geographicAreaM49Reporter, geographicAreaM49Partner, measuredItemCPC, timePointYears)],
            on = c("geographicAreaM49Reporter", "geographicAreaM49Partner", "measuredItemCPC", "timePointYears")
          ]

        d_tot <- copy(values$new_sws_data_total)

        d_tot <-
          d_tot[
            unique(d_bil[, .(geographicAreaM49 = geographicAreaM49Reporter, measuredElementTrade, measuredItemCPC, timePointYears)]),
            on = c("geographicAreaM49", "measuredElementTrade", "measuredItemCPC", "timePointYears")
          ]

        GetTestEnvironment(SERVER, input$token)
        res_bil <- SaveData(DOMAIN, DATASET_BIL, d_bil, d_bil_metad)

        GetTestEnvironment(SERVER, input$token)
        res_tot <- SaveData(DOMAIN, DATASET_TOT, d_tot)

        if (nrow(corrected_mirror) > 0) {
          GetTestEnvironment(SERVER, input$token)
          res_mirr <- SaveData(DOMAIN, DATASET_BIL, corrected_mirror)

          output$save_result_mirr <-
            renderText(
              paste0("Results on PARTNER BILATERAL dataset: ", paste(sapply(res_mirr[c("inserted", "appended", "ignored")], function(x) {paste(x, names(x))}), collapse = ", "), ". Numeric code of mirrored partners involved: ", paste(unique(corrected_mirror$geographicAreaM49Reporter), collapse = ", "), ".")
            )

        }

        output$save_result_bil <-
          renderText(
            paste0("Results on REPORTER BILATERAL dataset: ", paste(sapply(res_bil[c("inserted", "appended", "ignored")], function(x) {paste(x, names(x))}), collapse = ", "), ".")
          )

        output$save_result_tot <-
          renderText(
            paste0("Results on REPORTER TOTAL dataset: ", paste(sapply(res_tot[c("inserted", "appended", "ignored")], function(x) {paste(x, names(x))}), collapse = ", "), ".")
          )


        d <- unique(rbind(new_corrections, values$corrections, fill = TRUE))

        f <- file.path(CORRECTIONS_DIR, values$query_country, "corrections_table.rds")

        # Backup
        save_rds(values$corrections, sub('(\\.rds)', paste0('_', format(Sys.time(), '%Y%m%d%H%M%S'), '\\1'), f))
        # New
        save_rds(dplyr::as.tbl(d), f)

        values$corrections <- d

        values$corrections_to_save <- list()
        values$mirror_to_save      <- list()
        values$modif_history       <- list()

        output$save_result_corr <-
          renderText(
            paste("Corrections to reporter were correctly saved.")
          )
      }
    }
  )

  observeEvent(
    input$corr_delete_correction,
    {
      req(input$corr_corrections_table_rows_selected)

      d <- values$corrections[as.numeric(input$corr_corrections_table_rows_selected)]

      elements <- values$codelists$elements[substr(code, 1, 2) == ifelse(d$flow == 1L, "56", "59")]$code

      key <-
        DatasetKey(
          domain = DOMAIN,
          dataset = DATASET_BIL,
          dimensions =
            list(
              geographicAreaM49Reporter = Dimension("geographicAreaM49Reporter", d$reporter),
              geographicAreaM49Partner  = Dimension("geographicAreaM49Partner",  d$partner),
              measuredElementTrade      = Dimension("measuredElementTrade",      elements),
              measuredItemCPC           = Dimension("measuredItemCPC",           d$item),
              timePointYears            = Dimension("timePointYears",            as.character(d$year))
            )
        )

      d_data <- GetData(key)

      d_data <- exclude_items_more_elements(d_data, type = "bilateral")

      orig_order <- colnames(d_data)

      # XXX: 3+ elemets
      if (nrow(d_data) > 3) {
        stop("More than 3 elements")
      }

      if (d$data_type == "value") {
        element <- elements[substr(elements, 3, 3) == 2]
      } else {
        element <- elements[!(substr(elements, 3, 3) %in% 2:3)]
      }

      key@dimensions$measuredElementTrade@keys <- element

      d_history <- GetHistory(key)

      # XXX: we suppose that the last saved is the corrected one
      current_version <- d_history[grepl("data_original", Metadata_Value)][1]

      last_version <-
        d_history[
          Version < current_version$Version &
            Metadata_Element == "SUMMARY" &
            !dplyr::near(Value, current_version$Value, Value * 0.01)
        ]

      current_version <-
        current_version[,
          .(
            measuredElementTrade,
            current_Value = Value,
            corrected_Value = as.numeric(sub(".*data_original: ([^ ]+);.*", "\\1", Metadata_Value))
          )
        ]


      # if (nrow() > 0 then DO else NO

      d_data <- merge(d_data, current_version, by = "measuredElementTrade", all.x = TRUE)

      last_version <-
        last_version[,
          .(
            Version,
            measuredElementTrade,
            new_Value                 = Value,
            new_flagObservationStatus = flagObservationStatus,
            new_flagMethod            = flagMethod
          )
        ]

      last_version <- last_version[order(-Version), .SD[1], Version][1]

      last_version[, Version := NULL]

      d_data <- merge(d_data, last_version, by = "measuredElementTrade", all.x = TRUE)

      confirm <-
        d_data[
          dplyr::near(Value, current_Value, tol = Value * 0.01) &
            dplyr::near(corrected_Value, new_Value, tol = new_Value * 0.01)
        ]

      if (nrow(confirm) > 0) {

        d_data_tab <-
          d_data[
            !is.na(new_Value),
            .(
              partner   = geographicAreaM49Partner,
              elem      = measuredElementTrade,
              item      = measuredItemCPC,
              year      = timePointYears,
              Value,
              flags     = paste(flagObservationStatus, flagMethod, sep = "-"),
              Value_new = new_Value,
              flags_new = paste(new_flagObservationStatus, new_flagMethod, sep = "-")
            )
          ]

        d_data[
          !is.na(new_Value),
          `:=`(
            Value                 = new_Value,
            flagObservationStatus = new_flagObservationStatus,
            flagMethod            = new_flagMethod
          )
        ]

        d_data <- d_data[, orig_order, with = FALSE]

        uv <- d_data[substr(measuredElementTrade, 3, 3) == "2"]$Value / d_data[!(substr(measuredElementTrade, 3, 3) %in% 2:3)]$Value * 1000

        # TODO: adjust flags
        d_data[substr(measuredElementTrade, 3, 3) == "3", `:=`(Value = uv, flagMethod = "i")]

        d_data[,
          observationStatusFlag :=
            ifelse(
              substr(measuredElementTrade, 3, 3) == "3",
              aggregateObservationFlag(flagObservationStatus, flagTable = flagWeightTable_status),
              flagObservationStatus)
        ]

        values$data_uncorrect <- d_data

      } else {
        stop("Cannot rewind history")
      }

      #swsProgress_corr <- Progress$new(session, min = 0, max = 100)
      #on.exit(swsProgress_corr$close())
      #swsProgress_corr$set(value = 100, message = "Loading mirror data from SWS")

      showModal(
        modalDialog(
          renderText(
            'By clicking OK you confirm that the selected correction should
            be REMOVED. Data and flags will be set to the orignal ones,
            those marked as "_new" in the table below. Please note that if
            you delete it by mistake, it should be added again manually.'
          ),
          renderTable(d_data_tab),
          title = "Delete correction?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("corr_okDeleteCorrection", "OK")
          )
        )
      )

    }
  )

  observeEvent(
    input$corr_okDeleteCorrection,
    {
      removeModal()

      GetTestEnvironment(SERVER, input$token)
      res_uncorr <- SaveData(DOMAIN, DATASET_BIL, values$data_uncorrect)

      values$data_uncorrect <- NULL

      d <- values$corrections[-as.numeric(input$corr_corrections_table_rows_selected)]

      f <- file.path(CORRECTIONS_DIR, values$query_country, "corrections_table.rds")

      # Backup
      save_rds(values$corrections, sub('(\\.rds)', paste0('_', format(Sys.time(), '%Y%m%d%H%M%S'), '\\1'), f))
      # New
      save_rds(dplyr::as.tbl(d), f)

      values$corrections <- d

      showModal(
        modalDialog(
          title = "Done!",
          paste0("Deleted correction and sent results to SWS: ",
                 paste(sapply(res_uncorr[c("inserted", "appended", "ignored")],
                              function(x) {paste(x, names(x))}), collapse = ", "), ".")
        )
      )

    }
  )

  observeEvent(
    input$changelink$changes$changes[[1]][[1]],
    {
      d <- as.data.frame(hot_to_r(input$changelink))

      if (input$changelink$changes$changes[[1]][[2]] == 1) { # CPC
        if (input$changelink$changes$changes[[1]][[4]] %in% values$fcl_2_cpc$cpc) {
          d$fcl[d$cpc == input$changelink$changes$changes[[1]][[4]]] <-
            values$fcl_2_cpc$fcl[values$fcl_2_cpc$cpc == input$changelink$changes$changes[[1]][[4]]]
        } else {
          d$fcl[d$cpc == input$changelink$changes$changes[[1]][[4]]] <- NA_character_
          d$cpc[d$cpc == input$changelink$changes$changes[[1]][[4]]] <- NA_character_
        }

      } else if (input$changelink$changes$changes[[1]][[2]] == 2) {
        if (input$changelink$changes$changes[[1]][[4]] %in% values$fcl_2_cpc$fcl) {
          d$cpc[d$fcl == input$changelink$changes$changes[[1]][[4]]] <-
            values$fcl_2_cpc$cpc[values$fcl_2_cpc$fcl == input$changelink$changes$changes[[1]][[4]]]
        } else {
          d$cpc[d$fcl == input$changelink$changes$changes[[1]][[4]]] <- NA_character_
          d$fcl[d$fcl == input$changelink$changes$changes[[1]][[4]]] <- NA_character_
        }

        d$cpc[d$cpc == "NA"] <- NA_character_
        d$fcl[d$fcl == "NA"] <- NA_character_

      }

      values$current_data <- d
    }
  )

  output$maphelper <-
    DT::renderDataTable({

      req(values$query_country, values$sel_bilat$year,
          values$sel_bil$partner, values$hs6)

      dataReadProgress_helper <- Progress$new(session, min = 0, max = 100)
      dataReadProgress_helper$set(value = 100, message = "Loading map helper from the SWS")
      on.exit(dataReadProgress_helper$close())

      d <-
        ReadDatatable(
          paste0("ess_trademap_", values$sel_bilat$year),
          where = paste(paste0("hs like '", values$hs6, "%'"), collapse = " OR ")
        )

      # Cut at HS 6 and summarise, otherwise results can be A LOT
      d <-
        unique(
          d[,
            .(n_reporters = uniqueN(area), reporters = paste(sort(unique(area)), collapse = ", ")),
            .(hs6 = substr(hs, 1, 6), cpc, fcl, cpc_description, map_src)
          ]
        )

      DT::datatable(d, rownames = FALSE)
    })

   output$corr_myflags <-
     renderTable({
       flags
     })

   observeEvent(
     input$query_years,
     {
       values$query_years <- input$query_years
     }
   )
}

shinyApp(ui = ui, server = server)

