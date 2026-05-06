run_app <- function(interactive = TRUE){

app <- shiny::shinyApp(
  ui = app_ui,
  server = app_server
)

if(interactive){
  shiny::runApp(app)
} else{
  app
}

}
