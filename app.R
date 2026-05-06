if(!interactive()){
  library(shinyappDemo)
} else{
  devtools::load_all()
}

run_app(interactive())
