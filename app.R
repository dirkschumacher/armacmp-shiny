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
h2,h4 {
  color: white;
}
p {
  color: white;
}
      "))
  ),
  titlePanel("Translate and Compile R code into C++"),
  fluidRow(
    column(
      12,
      p(
        "R code is translated into C++ using the",
        a("armacmp", href = "https://github.com/dirkschumacher/armacmp"),
        " R package."
      )
    )
  ),
  fluidRow(
    column(6,
           h4("R Code"),
           aceEditor(
             outputId = "r_code",
             theme = "tomorrow_night_blue",
             mode = "r",
             value = initial_fun
           )),
    column(6,
           h4("Translated to C++"),
           aceEditor(
             outputId = "cpp_code",
             theme = "tomorrow_night_blue",
             mode = "c_cpp",
             readOnly = TRUE,
             value = initial_fun
           ))
  ),
  fluidRow(
    column(12,
           h4("Compilation Embedded in R"),
           aceEditor(
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

    header = 'Rcpp::cppFunction("\n'
    footer = '", depends = "RcppArmadillo", plugins = "cpp11")'

    paste0(header, paste0(cpp_code(), collapse = "\n"), footer, collapse = "\n")

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
