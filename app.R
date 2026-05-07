if(!interactive()){
  library(shinyappDemo)
} else{
  devtools::load_all()
}

options(shiny.maxRequestSize = 50 * 1024^2)
run_app(interactive())
