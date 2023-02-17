# Sanitize environment
rm(list = ls())

#___________________________________
#  Loading Libraries
#___________________________________
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(thematic) # to extend theme to ggplot
  library(plotly)
  library(DT)
  library(scales)
  library(leaflet)
  library(data.table)
  library(mapview)
  library(sf)
})

#___________________________________
#  Importing Data
#___________________________________
wfdta <- read_rds("data/wildfires1878_2019.rds")
ctdta <- read_rds("data/counties.rds")
stdta <- read_rds("data/states.rds")
medta <- read_rds("data/met_sites.rds")
wqdta <- read_rds("data/wq_sites.rds")

theme <- bs_theme()
theme <- bs_theme_update(theme,
                         base_font = font_google("Roboto"),
                         font_scale = 1.1, bootswatch = "journal",
                         heading_font = "Helvetica Neue")

# Code for User Interface (UI) ----
ui <- fluidPage(theme = theme,
                tags$style(type = "text/css", "
                .js-irs-0 .irs-grid-text {font-family: 'arial'; font-size: 9.5pt; # nolint
                           color: black; !important; z-index: 1;}
                .irs-single {color:black; background:#6666ff;}
                .irs-line {border: 1px solid black; height: 25px; border-radius: 0px;}
                           "),
                tabsetPanel(
                  # Analysis Tab -------------------------
                  tabPanel("Analysis", value = "analysis",
                           titlePanel("Spatiotemporal trends in wildland fires
                           across the continental United States: 1989 - 2019"),
                           br(),
                           sidebarLayout(
                             sidebarPanel(
                               width = 3,
                               markdown(
                                 "
                    #### Overview
                    ---
                    Here, you can examine historical trends in wildland fires across
                    the continental US over the period 1989 to 2019.
                    &nbsp;
                    "),
                               markdown(
                                 "
                    #### About the data
                    ---
                    This combined wildfire polygon dataset ranging in years from
                    1989-2019 (30 years) was built and published by the
                    <a href='https://www.sciencebase.gov/catalog/item/5ee13de982ce3bd58d7be7e7'
                    target='_blank'>U.S. Geological Survey</a>, and is one of the most comprehensive
                    wildfire datasets available. Attributes describing fires events that were utilized
                    here includes the event name, ignition year and date, acres burned, and cause of
                    the event. The data accounts for wildland fires, greater that 500 acres in size,
                    burned across the continental US over the observation period.
                    "
                                 ),
                               markdown("
                    #### Generate figures and tables
                    ---
                    "),
                               dateRangeInput(
                                 "datepick1",
                                 "Filter events by entering a date range (this range applies to all figures and tables): ",
                                 start = "1878-01-01",
                                 end = "2019-12-31",
                                 startview = "year"
                               ),
                               # br(),
                               markdown("---"),
                               selectInput(
                                 "menu1",
                                 "Select a grouping variable for the table: ",
                                 choices = c(
                                   "State" = "STATE",
                                   "Year"  = "fire_year",
                                   "Month" = "fire_month",
                                   "Fire Cause" = "fire_cause"
                                 ),
                                 selectize = TRUE,
                                 selected = "STATE"
                               ),
                               markdown("---"),
                               # markdown("The timeseries can be filtered by state boundaries."),
                               textInput("timeseries",
                                         label = "The timeseries can be subset spatially by state.
                                         Enter comma-separated state name(s) to see subset trend: ",
                                         placeholder = "All States"),
                               radioButtons("mode", "", inline = TRUE,
                                            c("Line" = "line",
                                              "Marker" = "marker",
                                              "Both" = "both")),
                               markdown(
                                 "
                    #### Preliminary Findings
                    ---
                    Overall, we observe that wildfires appear to be increasing in
                    **severity** (number acres burned) and **frequency** (number
                    of fires per year) over the period. Natural causes remain the
                    dominant cause of wilfires across the continental United States.
                    Alaska accounts for the most area burned over the observation period.
                                 "
                               )
                             ),
                             # mixing sidebarLayout and fluidRows
                             mainPanel(
                               fluidRow(
                                 column(4, DT::dataTableOutput("tabl1")),
                                 column(8, shinycssloaders::withSpinner(
                                   plotlyOutput("plotly1", height = 450),
                                   color = "#eb6864", size = 2)
                                   ),
                               ),
                               br(),
                               markdown("---"),
                               br(),
                               fluidRow(
                                 column(12,
                                        shinycssloaders::withSpinner(
                                          plotlyOutput("plotly2", height = 670),
                                          color = "#eb6864", size = 2)
                                        ),
                                 ),
                               )
                             )
                           ),
                  # Interactive Map Tab -------------------------
                  tabPanel(
                    "Interactive Map", value = "map",
                    titlePanel(
                      "Spatiotemporal trends in wildland fires
                                      across the continental United States: 1878 - 2019"
                    ),
                    br(),
                    sidebarLayout(
                      sidebarPanel(
                        width = 3,
                        markdown("
                          #### Overview

                          ---
                          Here, we examine historical trends in wildland fires across
                          the continental US over the period 1989 to 2019.
                          Use the input boxes and sliders below to apply several
                          spatial and temporal modifiers to map wildfire data.

                          ---"
                        ),
                        br(),

                        markdown("Events can be filtered by state and county boundaries (comma separated), and date ranges."),
                        markdown("---"),

                        # Input field to select polygons by state and county
                        textInput("user_text_state",
                                  label = "Enter state name(s):",
                                  placeholder = "All States"),
                        textInput("user_text_county",
                                  label = "Enter county name(s):",
                                  placeholder = "All Counties"),

                        markdown("---"),

                        dateRangeInput(
                          "firedate1",
                          "Enter a date range: ",
                          start = "2015-01-01",
                          end = "2019-12-31",
                          startview = "year"
                        ),
                        markdown("---"),
                        # Input slider to select polygons by fire size
                        sliderInput(
                          "acres",
                          "Use the slider to filter by event size (acres burned) :",
                          min = 0,
                          max = 1400000,
                          step = 100000,
                          value = c(5000, 1400000),
                          round = T,
                          dragRange = T
                        ),
                        # Input field to select polygons by fire_name
                        markdown(
                          "
                          ---
                          &nbsp;

                          If you know the name of an event (HINT: Use the
                          <span style='color:#eb6864'>**Data Explorer**</span>
                          tab to search for event names) you can enter it into
                          the text-box below to filter the map to that event. Be
                          sure that the event date is within the date range selected above."
                        ),
                        textInput("user_text_name",
                                  label = "Enter event name(s) :",
                                  placeholder = "All Events"),
                        markdown(
                          "
                          ---
                          "),
                        br(),
                        # Map Download Button
                        downloadButton("downloadMap", "Download Map", class = "butt"),
                        tags$head(tags$style(".butt{background-color:#eb6864 !important;} .butt{color: white;}"))
                      ),
                      #mixing sidebarLayout and fluidRows
                      mainPanel(
                        fluidRow(
                          column(12,
                        shinycssloaders::withSpinner(
                          leafletOutput("wfmap", width = 1600, height = 1135),
                          color = "#eb6864",
                          size = 2
                        )
                          ),
                        column(7,
                               shinycssloaders::withSpinner(
                                 DT::dataTableOutput("maptable"),
                                 color = "#eb6864",
                                 size = 1)
                        )
                        )
                      )
                    )
                  ),
                  # Raw Data Table Tab -------------------------
                  tabPanel(
                    "Data Explorer", value = "data",
                    titlePanel(
                      "Spatiotemporal trends in wildland fires
                      across the continental United States: 1989 - 2019"
                    ),
                    br(),
                    shinycssloaders::withSpinner(
                      DT::dataTableOutput("tabl2"),
                      color = "#eb6864",
                      size = 2
                    ),
                    # Download Button
                    downloadButton("downloadData", "Download Table", class = "butt"),
                    tags$head(tags$style(".butt{background-color:#eb6864 !important;} .butt{color: white;}")),
                    br(),
                    br(),
                    markdown(
                      "
                      ##### Data Source
                      ---
                      * Welty, J.L., and Jeffries, M.I., 2020, Combined wildfire
                      datasets for the United States and certain territories,
                      1878-2019:
                       <a href='https://www.sciencebase.gov/catalog/item/5ee13de982ce3bd58d7be7e7'
                    target='_blank'>U.S. Geological Survey data release,</a>
                                  "
                    )
                  )
                )
                )
# Code for server ----
server <- function(input, output, session) {
  # bs_themer()
  # thematic::thematic_shiny()
  # #this will draw some colors from the theme into ggplots (like background colors)

  #___________________________________
  # Generate outputs for Analysis Tab
  #___________________________________

  # Reactive expression to filter the data based in date inputs
  filtered_dta <- reactive({
    wfdta |>
      sf::st_set_geometry(NULL) |>
      filter(
        fire_year >= lubridate::year(!!input$datepick1[1]),
        fire_year <= lubridate::year(!!input$datepick1[2])
      )
  })

  # Reactive event to group and sum filtered data based on input variable
  res1 <- reactive({
    req(filtered_dta())
    filtered_dta() |>
      select(-c(fire_cause)) |>
      rename("fire_cause" = "fire_cause_recat") |>
      group_by(.data[[input$menu1]]) |>
      summarize(`Acres Burned` = sum(acres, na.rm = T)) |>
      arrange(desc(`Acres Burned`)) |>
      rename_with( ~ gsub('[[:punct:]]', ' ', .x)) |>
      rename_with(str_to_title) |>
      slice_head(n = 15)
  })


  # Output Table 1 takes the results from the reactive element res1 when it is triggered
  output$tabl1 <- DT::renderDataTable({
    req(res1())
    DT::datatable(res1(),
                  options = list(
                    columnDefs = list(list(
                      className = 'dt-center', targets = 0:1)),
                    rownames = FALSE,
                    searching = FALSE,
                    lengthMenu = c(5, 10, 15, 20)
                  )) %>% formatCurrency(2, '', digits = 0)
  })

  # Generate output plot 1
  output$plotly1 <- renderPlotly({
    req(filtered_dta())
    filtered_dta() |>
      dplyr::filter(
      if ((!!input$timeseries) != "") {
        STATE %in% str_to_title(str_squish(unlist(str_split((!!input$timeseries), ","
        ))))
      } else {
        STATE == STATE
      }) |>
      janitor::tabyl(fire_cause_recat) |>
      drop_na() |>
      arrange(desc(n)) |>
      plot_ly(
        x = ~ fire_cause_recat, y = ~ percent * 100, type = 'bar',
        color = ~ percent, colors = "YlOrRd"
      )  |>
      plotly::layout(
        title = list(text = '<b> Causes of wildfires in the US are mostly natural or unknown </b>',
                     font = list(size = 18), standoff = 10L),
        yaxis = list(title = list(text = '', standoff = 10L),
                     font = list(size = 18), ticksuffix = "%",
                     range = c(0, 100)),
        xaxis = list(title = list(text = '<b> Cause of Fire </b>', standoff = 20L),
                     font = list(size = 25)),
        plot_bgcolor = '#FFFFFF',
        font = list(size = 15))
  })

  # Generate output plot  2
  output$plotly2 <- renderPlotly({
    req(filtered_dta())
    filtered_dta() |>
      dplyr::filter(
        if ((!!input$timeseries) != "") {
          STATE %in% str_to_title(str_squish(unlist(str_split((!!input$timeseries), ","
          ))))
        } else {
          STATE == STATE
        }) |>
      dplyr::filter(acres > 0) |>
      group_by(fire_year) |>
      summarize(Acres = sum(acres, na.rm = T),
                Fires = n()) |>
      rename(Year = fire_year) |>
      ungroup() |>
      plot_ly() |>
      add_trace(
        x = ~ Year,  y = ~ Acres,  type = 'scatter',
        marker = switch(input$mode,
                        line = NULL,
                        marker = list(size = 6),
                        both = list(size = 6),
                        list(size = 6)),
        line = switch(input$mode,
                      line = list(width = 2.5),
                      marker = NULL,
                      both = list(width = 2.5),
                      list(width = 2.5)),
        mode = switch(input$mode,
                      line = "lines",
                      marker = "markers",
                      both = "lines+markers",
                      "lines"),
        name = "Acres Burned"
      ) |>
      add_trace(
        x = ~ Year, y = ~ Fires, type = 'scatter',
        marker = switch(input$mode,
                        line = NULL,
                        marker = list(size = 6),
                        both = list(size = 6),
                        list(size = 6)),
        line = switch(input$mode,
                      line = list(width = 2.5),
                      marker = NULL,
                      both = list(width = 2.5),
                      list(width = 2.5)),
        mode = switch(input$mode,
                      line = "lines",
                      marker = "markers",
                      both = "lines+markers",
                      "lines"),
        yaxis = "y2",
        name = "Number of Fires"
      ) |>
      plotly::layout(
        yaxis2 = list(overlaying = "y", side = "right",
                      title = list(text = '<b>Number of Fires</b>', standoff = 35L)),
        yaxis = list(title = list(text = '<b>Acres Burned</b>', standoff = 20L)),
        xaxis = list(title = list(text = '<b>Year</b>', standoff = 30L))
      ) |>
      plotly::layout(title = list(text = '<b>Overall, wildfires appear to be increasing in frequency and severity across the continental US</b>',
                                  font = list(size = 17)), font = list(size = 15),
                     legend = list(font = list(size = 18), x = 1.05, y = 0.95),
                     xaxis = list(title = list(font = list(size = 18))),
                     yaxis = list(title = list(font = list(size = 18))),
                     yaxis2 = list(title = list(font = list(size = 18))),
                     plot_bgcolor = '#FFFFFF')
  })

  #________________________________________________
  # Generate outputs for Interactive Map Tab
  #_______________________________________________

  # Create reactive element for wildfire polygons
  data_react <- reactive({
    wfdta |>
      dplyr::filter(
        fire_year >= lubridate::year(!!input$firedate1[1]),
        fire_year <= lubridate::year(!!input$firedate1[2]),
        acres >= (!!input$acres[1]),
        acres <= (!!input$acres[2]),
        if ((!!input$user_text_state) != "") {
          STATE %in% str_to_title(str_squish(unlist(str_split((!!input$user_text_state), ","
          ))))
        } else {
          STATE == "California"
        },
        if ((!!input$user_text_county) != "") {
          COUNTY %in% str_to_title(str_squish(unlist(str_split((!!input$user_text_county), ","
          ))))
        } else {
          COUNTY == COUNTY
        },
        if ((!!input$user_text_name) != "") {
          fire_name %in% str_to_title(str_squish(unlist(str_split((!!input$user_text_name), ","
          ))))
        } else {
          fire_name == fire_name
        })
  })  # |> bindCache(input$firedate1[1], input$firedate1[2], input$acres[1], input$acres[2],
      #            input$user_text_state, input$user_text_county, input$user_text_name)

  # create reactive element for state polygons ----
  data_react_st <- reactive({
    stdta |> st_as_sf() |>
      dplyr::filter(
        if ((!!input$user_text_state) != "") {
          ST_NAME %in% str_to_title(str_squish(unlist(str_split((!!input$user_text_state), ","
          ))))
        } else {
          ST_NAME == ST_NAME
        })
  })

  # create reactive element for county polygons ----
  data_react_ct <- reactive({
    ctdta |> st_as_sf() |>
      dplyr::filter(
        if ((!!input$user_text_state) != "") {
          ST_NAME %in% str_to_title(str_squish(unlist(str_split((!!input$user_text_state), ","
          ))))
        } else {
          ST_NAME == ST_NAME
        },
        if ((!!input$user_text_county) != "") {
          CNTY_NAME %in% str_to_title(str_squish(unlist(str_split((!!input$user_text_county), ","
          ))))
        } else {
          CNTY_NAME == CNTY_NAME
        })
  })

  # create reactive element for water quality site dataset ----
  data_react_wq <- reactive({
    wqdta |>
      dplyr::filter(
        if ((!!input$user_text_state) != "") {
          ST_NAME %in% str_to_title(str_squish(unlist(str_split((!!input$user_text_state), ","
          ))))
        } else {
          ST_NAME == ST_NAME
        },
        if ((!!input$user_text_county) != "") {
          CNTY_NAME %in% str_to_title(str_squish(unlist(str_split((!!input$user_text_county), ","
          ))))
        } else {
          CNTY_NAME == CNTY_NAME
        })
  })

  my_comma <- scales::label_comma(accuracy = 1,
                                  big.mark = ",",
                                  decimal.mark = ".")

  # Create reactive leaflet map ----
  map_react <- reactive({
    req(data_react())
    req(data_react_ct())
    req(data_react_st())
    req(data_react_wq())
    firedta <- data_react()
    st_data <- data_react_st()
    ct_data <- data_react_ct()
    wq_data <- data_react_wq()
    mypalette <-  colorBin(palette = heat.colors(7, alpha = 1),
                           domain = firedta$acres, na.color = "transparent",
                           bins = 7, pretty = TRUE, reverse = TRUE)
    firedta %>%
      leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$Stamen.Terrain,
                       group = "Terrain",
                       options = providerTileOptions(noWrap = TRUE, updateWhenZooming = FALSE,
                                                     updateWhenIdle = FALSE)) %>%
      fitBounds(lng1 = min(firedta$lon),
                lat1 = min(firedta$lat),
                lng2 = max(firedta$lon),
                lat2 = max(firedta$lat),
                options = list(padding = c(10, 10))) %>%
      addPolygons(weight = 0.8, fillColor = ~ mypalette(acres),
                  color = "#A14000", fillOpacity = 0.7, group = "Burn Area",
                  popup = paste("<b>Name: </b>", firedta$fire_name, "<br>",
                                "<b>State: </b>", firedta$STATE, "<br>",
                                "<b>County: </b>", firedta$COUNTY, "<br>",
                                "<b>Year: </b>", firedta$fire_year, "<br>",
                                "<b>Ignition Date </b>", firedta$ignt_date, "<br>",
                                "<b>Acres Burned </b>: ", my_comma(firedta$acres), "<br>",
                                "<b>Cause: </b>", firedta$fire_cause_recat)) %>%
      addPolygons(data = st_data, weight = 1.0, color = "#454545",
                  fill = FALSE, group = "State",
                  highlightOptions = highlightOptions(color = "black", weight = 2.5, bringToFront = TRUE),
                  label = ~ ST_NAME, labelOptions = labelOptions(textsize = "18px", opacity = 0.7)) %>%
      addPolygons(data = ct_data, weight = 0.7, color = "#454545", fill = FALSE, group = "County",
                  highlightOptions = highlightOptions(color = "black", weight = 2.5, bringToFront = TRUE),
                  label = ~ CNTY_NAME, labelOptions = labelOptions(textsize = "17px", opacity = 0.7)) %>%
      addCircleMarkers(~lon, ~lat, group = "Fire Location", fillColor = "red", radius = 3,
                       fillOpacity = 0.8, stroke = F) %>%
      # addCircleMarkers(data = wq_data, ~lon, ~lat, group = "WQ Sites", fillColor = "blue",
      #                  fillOpacity = 0.8, stroke = F, radius = 4,
      #                  popup = paste("<b>Name: </b>", wq_data$station_nm, "<br>",
      #                                "<b>Site No: </b>", wq_data$site_no, "<br>",
      #                                "<b>County: </b>", wq_data$CNTY_NAME, "<br>",
      #                                "<b>State: </b>", wq_data$ST_NAME, "<br>")) %>%
      addLayersControl(baseGroups = c("OSM", "Terrain"),
                       overlayGroups = c("Burn Area", "Fire Location", "State", "County"), #"WQ Sites","ADI"
                       options = layersControlOptions(collapsed = FALSE, autoZIndex = F)) %>%
      addLegend(pal = mypalette, values = ~ acres, opacity = 0.8,
                title = "Acres Burned", position = "bottomleft") %>%
      hideGroup("State") %>%
      hideGroup("County") %>%
      hideGroup("ADI") %>%
      hideGroup("WQ Sites") %>%
      hideGroup("Fire Location")
    })  %>% bindCache(data_react())

  # Render reactive leaflet map ----
  output$wfmap <- renderLeaflet({
    map_react()
  })

  # store the current user-created version of the Leaflet map for download in a reactive expression ----
  user_map <- reactive({
    # call the reactive Leaflet map
    map_react() %>%
      # store the view based on UI
      setView(lng = input$wfmap_center$lng,
              lat = input$wfmap_center$lat,
              zoom = input$wfmap_zoom
      )
  }) # end of creating user.created.map()

  # output maptable takes the results from the reactive element res1 when it is triggered
  output$maptable <- DT::renderDataTable({
    req(data_react())
    mpdta <- data_react() %>%
      sf::st_set_geometry(NULL) %>%
      select(-c(lon, lat, STUSPS, fire_cause, fire_year, fire_month)) %>%
      rename("fire_cause" = "fire_cause_recat") %>%
      rename("fire_date" = "ignt_date") %>%
      rename("acres_burned" = "acres") %>%
      rename_with( ~ gsub('[[:punct:]]', ' ', .x)) %>%
      rename_with(str_to_title) %>%
      mutate_if(is.numeric, round, 1) %>%
      mutate_at(6, as.factor)
    DT::datatable(mpdta,
                  options = list(
                    columnDefs = list(list(
                      className = 'dt-center', targets = 0:1)),
                    rownames = FALSE,
                    searching = FALSE,
                    lengthMenu = c(5, 10, 15, 20),
                    initComplete = htmlwidgets::JS(
                      "function(settings, json) {",
                      paste0("$(this.api().table().container()).css({'font-size': '", '13.5px', "'});"),
                      "}")
                    )) %>% formatCurrency(3, '', digits = 0)
  })


  # # Download csv of filtered data from map data table ---
  # output$downloadMapData <- downloadHandler(
  #   filename = function() {
  #     paste("wildfire_map_data_", Sys.Date(), ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     r = input$maptable_rows_all
  #     write.csv(data_react()[r, ], file, row.names = FALSE)
  #   }
  # )

  # Download screenshot of current map display state as .pdf ---
  # create the output file name
  # and specify how the download button will take
  # a screenshot - using the mapview::mapshot() function
  # and save as a PDF
  output$downloadMap <- downloadHandler(
    filename = paste0("custom_fire_map", Sys.Date(), ".pdf"),
    content = function(file) {
      mapview::mapshot(x = user_map(),
               file = file,
               cliprect = "viewport", # the clipping rectangle matches the height & width from the viewing port
               selfcontained = FALSE  # when this was not specified, the function for produced a PDF
                                      # of two pages: one of the leaflet map, the other a blank page.
      )
    } # end of content() function
  ) # end of downloadHandler() function

  #________________________________________________
  # Generate outputs for Raw Data Tab
  #________________________________________________

  # Reactive for raw data table.
  raw1 <- reactive({
    wfdta |>
      dplyr::filter(acres > 0) |>
      sf::st_set_geometry(NULL) |>
      select(-c(lon, lat, STUSPS, fire_cause)) |>
      rename("fire_cause" = "fire_cause_recat") |>
      rename("fire_date" = "ignt_date") |>
      rename("acres_burned" = "acres") |>
      dplyr::filter(acres_burned > 0) |>
      rename_with( ~ gsub('[[:punct:]]', ' ', .x)) |>
      rename_with(str_to_title) |>
      mutate_if(is.numeric, round, 1) |>
      mutate_at(c(2:3,8), as.factor)
  })

  # Raw data table
  output$tabl2 <- DT::renderDataTable({
    req(raw1())
    DT::datatable(data = raw1(),
                  # extensions = 'Buttons',
                  options = list(
                    autoWidth = TRUE,
                    rownames = FALSE,
                    sDom  = "ilftpr",
                    lengthMenu = c(15, 20, 50, 100),
                    dom = 'lfrtipB',
                    # buttons = c("copy"),
                    columnDefs = list(list(
                      className = 'dt-center', targets = 0:8
                    ))
                  ), filter = "top", selection = "multiple", escape = FALSE) %>%
      formatCurrency(5, '', digits = 0) %>%
      formatDate(4,  method = 'toLocaleDateString')
  })

  # Download csv of filtered data from raw data table----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("wildfire_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      r = input$tabl2_rows_all
      write.csv(raw1()[r, ], file, row.names = FALSE)
    }
  )

}

shinyApp(ui = ui, server = server)

