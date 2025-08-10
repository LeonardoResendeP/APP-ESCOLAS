# scripts/diag_leaflet_runtime.R
run_app_diag_mapa <- function() {
  library(shiny)
  library(leaflet)
  ui <- fluidPage(
    tags$h3("Diagnóstico Leaflet (tiles & CORS)"),
    leafletOutput("m", height = "520px"),
    tags$hr(),
    tags$small("Log do navegador (tile load/error):"),
    verbatimTextOutput("tile_log")
  )
  server <- function(input, output, session){
    output$m <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 2)) |>
        addProviderTiles(providers$Esri.WorldStreetMap, group="Esri.WorldStreetMap") |>
        addProviderTiles(providers$CartoDB.Positron, group="CartoDB.Positron") |>
        addTiles(group = "OSM Padrão") |>
        addLayersControl(
          baseGroups = c("Esri.WorldStreetMap","CartoDB.Positron","OSM Padrão"),
          options = layersControlOptions(collapsed = TRUE)
        ) |>
        setView(-46.64, -23.578, 14) |>
        htmlwidgets::onRender(sprintf("
          function(el,x){
            var map = this;
            function wire(layer){
              if(layer && layer.on){
                layer.on('tileloadstart', function(e){
                  Shiny.setInputValue('%s', {ev:'start', url:e.tile.src, t:Date.now()}, {priority:'event'});
                });
                layer.on('tileload', function(e){
                  Shiny.setInputValue('%s', {ev:'ok', url:e.tile.src, t:Date.now()}, {priority:'event'});
                });
                layer.on('tileerror', function(e){
                  Shiny.setInputValue('%s', {ev:'err', url:e.tile.src, t:Date.now()}, {priority:'event'});
                });
              }
            }
            map.eachLayer(wire);
            map.on('baselayerchange', function(e){ wire(e.layer); });
            setTimeout(function(){ map.invalidateSize(true); }, 200);
          }
        ", session$ns("tile_evt"), session$ns("tile_evt"), session$ns("tile_evt")))
    })
    
    # mantém só as últimas 10 linhas
    log <- reactiveVal(character())
    observeEvent(input$tile_evt, {
      evt <- input$tile_evt
      row <- sprintf("[%s] %s  %s", format(Sys.time(), "%H:%M:%S"), toupper(evt$ev), evt$url)
      cur <- tail(c(log(), row), 10)
      log(cur)
      if (evt$ev == "err") cat("TILE ERROR:", evt$url, "\n")
    })
    output$tile_log <- renderText(paste(log(), collapse = "\n"))
  }
  shinyApp(ui, server)
}
