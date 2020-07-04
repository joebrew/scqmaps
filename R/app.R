options(shiny.maxRequestSize=512*1024^2) # need to also set in nginx   client_max_body_size 512M;


##################################################
# UI
##################################################
#' @import shiny
#' @import shinydashboard
#' @import shinyjs
#' @import dplyr
app_ui <- function(request) {
  options(scipen = '999')
  
  tagList(
    mobile_golem_add_external_resources(),
    dashboardPage(
      dashboardHeader (title = "SCQmaps"),
      dashboardSidebar(),
      dashboardBody(
        fluidRow(
          column(4, align = 'center',
                 h3('Controls'),
                 textInput('name', 'Name'),
                 textInput('person', 'Person'),
                 textInput('comments', 'Comments'), 
                 actionButton('draw', 'Draw'),
                 actionButton('clear', 'Clear'),
                 actionButton('submit', 'Submit'),
                 checkboxInput('show_previous', 'Show previous')),
          column(8, align = 'center',
                 leafletOutput('leaf'))
        ),
        fluidRow(
          column(12,
                 textOutput('show_polygon_text'))
        )
      )
    )
  )
  
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
mobile_golem_add_external_resources <- function(){
  addResourcePath(
    'www', system.file('app/www', package = 'scqmaps')
  )

  
  # share <- list(
  #   title = "Databrew's COVID-19 Data Explorer",
  #   url = "https://datacat.cc/covid19/",
  #   image = "http://www.databrew.cc/images/blog/covid2.png",
  #   description = "Comparing epidemic curves across countries",
  #   twitter_user = "data_brew"
  # )
  
  tags$head(
    
    # # Facebook OpenGraph tags
    # tags$meta(property = "og:title", content = share$title),
    # tags$meta(property = "og:type", content = "website"),
    # tags$meta(property = "og:url", content = share$url),
    # tags$meta(property = "og:image", content = share$image),
    # tags$meta(property = "og:description", content = share$description),
    # 
    # # Twitter summary cards
    # tags$meta(name = "twitter:card", content = "summary"),
    # tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
    # tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
    # tags$meta(name = "twitter:title", content = share$title),
    # tags$meta(name = "twitter:description", content = share$description),
    # tags$meta(name = "twitter:image", content = share$image),
    # 
    # # golem::activate_js(),
    # # golem::favicon(),
    # # Add here all the external resources
    # # Google analytics script
    # includeHTML(system.file('app/www/google-analytics-mini.html', package = 'covid19')),
    # includeScript(system.file('app/www/script.js', package = 'covid19')),
    # includeScript(system.file('app/www/mobile.js', package = 'covid19')),
   
    tags$link(rel="stylesheet", type="text/css", href="www/styles.css")
    
    # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}





##################################################
# SERVER
##################################################
#' @import shiny
#' @import leaflet
#' @import leaflet.extras
#' @import sf
#' @import rgeos
#' @import sp
#' @import readr
#' @import dplyr
app_server <- function(input, output, session){

  # Create an empty data object if it doesn't exist
  data_path <- 'file.csv'
  if(!file.exists(data_path)){
    df <- tibble(x = c(1:9, 8:1), 
                 y = c(1, 2*(5:3), 2, -1, 17, 9, 8, 2:9)) %>%
      mutate(name = 'Dummy', details = 'Dummy')
    write_csv(df, data_path)
  } 
  # Read in the previous data
  df <- read_csv(data_path)
  
  # Get previous polygons
  # previous <- reactiveValues(data = list())
  previous <- shiny::reactiveValues(data = df)
  
  # Write data to database
  observeEvent(input$submit,{
    data_path <- 'file.csv'
    # Get old data
    old_df <- previous$data
    # Get new data 
    feat <- input$leaf_draw_new_feature
    coords <- unlist(feat$geometry$coordinates)
    polygon_text(coords)
    coords <- matrix(coords, ncol = 2, byrow = T)
    coords <- data.frame(coords); names(coords) <- c('x', 'y')
    coords$name <- input$name
    coords$person <- input$person
    coords$details <- input$details
    new_df <- coords
    replacement <- bind_rows(old_df, new_df)
    write_csv(replacement, data_path)
    previous$data <- replacement
  })
  
  
  output$leaf <- renderLeaflet({
    input$clear
    input$submit
    leaflet() %>%
      setView(lng = 1.3829, lat = 41.5322, zoom = 14) %>%
      # Base groups
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      # Layers control
      addLayersControl(
        baseGroups = c("OSM (default)", 'Satellite', "Toner", "Toner Lite"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observeEvent(input$show_previous,{
    isp <- input$show_previous
    if(isp){
      the_previous <- previous$data
      if(length(the_previous) > 0){
        previous_names <- sort(unique(the_previous$name))
        for(i in 1:length(previous_names)){
          print(head(the_previous))
          this_previous_name <- previous_names[[i]]
          message('This previous name: ', this_previous_name)
          sub_data <- the_previous %>% dplyr::filter(name == this_previous_name)
          leafletProxy('leaf', session) %>%
            addPolygons(lng = sub_data$x,
                        lat = sub_data$y,
                        popup = paste0(sub_data$name[1]))
        }
      }
    } else {
      leafletProxy('leaf', session) %>%
        clearShapes()
    }
    
    
  })
  
  observeEvent(input$draw,{
    leafletProxy('leaf', session) %>%
      addDrawToolbar(polylineOptions = F, 
                     circleOptions = F, 
                     markerOptions = F,
                     circleMarkerOptions = F,
                     editOptions = F,
                     # singleFeature = T,
                     # polygonOptions = T)
                     polygonOptions = drawPolygonOptions(showArea = T,
                                                         metric = T,
                                                         shapeOptions = drawShapeOptions(fillColor = 'red')
                     ))
  })
  
  polygon_text <- reactiveVal(value = '')
  
  observeEvent(input$leaf_draw_new_feature, {
    feat <- input$leaf_draw_new_feature
    coords <- unlist(feat$geometry$coordinates)
    polygon_text(coords)
    coords <- matrix(coords, ncol = 2, byrow = T)
    poly <- st_sf(st_sfc(st_polygon(list(coords))), crs = 4326)
    print(st_bbox(poly))
  })
  observeEvent(input$leaf_selected,{
    message('Selected')
  })
  
  output$show_polygon_text <- renderText({
    p <- polygon_text()
    p
  })
  
  observeEvent(input$clear, {
    message('Clearing map')
    leafletProxy('leaf', session) %>%
      removeDrawToolbar(clearFeatures = TRUE) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls()
  })
  
  observeEvent(input$submit,{
    updateCheckboxInput(session = session,
                        inputId = 'show_previous',
                        label = 'Show previous',
                        value = FALSE)
  })
  
}


app <- function(){
  # Detect the system. If on AWS, don't launch browswer
  is_aws <- grepl('aws', tolower(Sys.info()['release']))
  shinyApp(ui = app_ui,
           server = app_server,
           options = list('launch.browswer' = !is_aws))
}