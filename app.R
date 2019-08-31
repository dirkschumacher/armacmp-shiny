library(shiny)
library(shinyAce)

initial_fun <- "function(X) {
  result <- t(X) %*% X
  return(result, type = type_scalar_numeric())
}"

ui <- fluidPage(
  tags$head(
    tags$style(HTML("

body {
  background-color: black;
}
h2 {
  color: white;
}
p {
  color: white;
}
      "))
  ),
  titlePanel("Armacmp compiler tools"),
  fluidRow(
    column(
      12,
      p(
        "Use the R package ",
        a("armacmp", href = "https://github.com/dirkschumacher/armacmp"),
        " to compile R to C++."
      )
    )
  ),
  fluidRow(
    column(6, aceEditor(
      outputId = "r_code",
      theme = "tomorrow_night_blue",
      mode = "r",
      value = initial_fun
    )),
    column(6, aceEditor(
      outputId = "cpp_code",
      theme = "tomorrow_night_blue",
      mode = "c_cpp",
      readOnly = TRUE,
      value = initial_fun
    ))
  ),
  fluidRow(
    column(12, aceEditor(
      outputId = "rcpp_code_text",
      theme = "tomorrow_night_blue",
      mode = "r",
      readOnly = TRUE,
      value = initial_fun
    ))
  )
)

server <- function(input, output, session) {
  cpp_code <- reactive({
    tryCatch({
      fun <- parse(text = input$r_code)[[1L]]
      stopifnot(is.call(fun), fun[[1]] == "function")
      envir <- new.env(parent = emptyenv())
      envir$`function` <- `function`
      fun <- eval(fun, envir = envir)
      stopifnot(is.function(fun))
      armacmp::translate(fun, "fun")$cpp_code
    }, error = function(e) {
      "[compiler error]"
    })
  })
  rcpp_code <- reactive({
    req(is.character(cpp_code()))
    paste0(deparse(bquote(
      Rcpp::cppFunction(.(cpp_code()),
        depends = "RcppArmadillo",
        plugins = "cpp11"
      )
    )), collapse = "\n")
  })

  observe({
    code <- cpp_code()
    updateAceEditor(
      session,
      "cpp_code",
      value = code
    )
  })

  observe({
    updateAceEditor(
      session,
      "rcpp_code_text",
      value = rcpp_code()
    )
  })
}

shinyApp(ui = ui, server = server)
