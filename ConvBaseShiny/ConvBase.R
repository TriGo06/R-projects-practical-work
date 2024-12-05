DecodeDigits <- function(word, alphabet = c(0:9, LETTERS)) {
  digits <- unlist(strsplit(toupper(word), NULL)) 
  return(match(digits, alphabet)-1)
}

DecodeNumber <- function(word, base) {
  n <- 0
  p <- 0
  l <- nchar(word)
  if(base < 2 || base > 36){return(NA)}
  for (i in l:1) {
    if(DecodeDigits(substr(word,i,i)) >= base){return(NA)}
    n <- n + DecodeDigits(substr(word,i,i)) * base**p
    p <- p + 1
  }
  return(n)
}

EncodeNumber <- function(n, base) {
  if(base < 2 || base > 36){return(NA)}
  if(n == 0){return(0)}
  word <- ""
  while(n > 0){
    word <- paste0(EncodeDigits(n%%base),word)
    n <- floor(n/base)
  }
  return(word)
}

EncodeDigits <- function(n, alphabet = c(0:9, LETTERS)) {
  if(n < 0 || n > 35){return(NA)}
  return(alphabet[n+1])
}


EncodeBibi <- function(n) {
  acc <- c()
  bibi <- c('HO','HA','HE','HI','BO','BA','BE','BI','KO','KA','KE','KI','DO','DA','DE','DI')
  symboles <- c(0:9,LETTERS)
  while (n>0) {
      acc <- append(acc,symboles[(n%%16)+1])
      n <- n%/%16
  }
  res <- c()
  acc <- rev(acc)
  x <- c(DecodeDigits(acc))
  for (i in 1:length(x)) {
    res <- append(res,bibi[x[i] + 1])
  }
  res <- paste(res,collapse = '')
  return(res)
}



library(shiny)
#' La fonction server d'une application shiny réalise le traitement des données et la génération des graphiques/tableaux.
server <- function(input, output) {
  CheckBase <- function(base) base >= 2 && base <= 36
  fromBase <- reactive( {
    validate(
      need(!is.na(input$fromBase), "Base d'origine manquante"),
      need(CheckBase(input$fromBase), "Base d'origine doit être entre 2 et 36.")
    )
    input$fromBase
  })
  number <- reactive( {
    number <- trimws(input$number)
    validate(
      need(nchar(number) > 0, "Pas de nombre en entrée."), 
      need(!grepl("[^a-zA-Z0-9]", number), "Format de nombre incorrect") 
    )
    number <- DecodeNumber(number, fromBase())
    validate(
      need(!is.na(number), "Chiffres invalides dans le nombre.")
      )
    number
  })

  ConvertNumber <- function(n, base) {
    validate(
      need(!is.na(base), "Base de destination manquante")
    )
    if( CheckBase(base) ) {
      return(EncodeNumber(n, base))
    } else {
      return(EncodeBibi(n))
    }
  }
  output$toBase1 <- renderText({ ConvertNumber(number(), input$toBase1)})
  output$toBase2 <- renderText({ ConvertNumber(number(), input$toBase2)})
  output$toBase3 <- renderText({ ConvertNumber(number(), input$toBase3)})
}

#' La fonction server d'une application shiny construit l'interface graphique à partir de ses entrées/sorties.
ui <- fluidPage(
  titlePanel("Convertisseur à Bibi"),
  sidebarLayout(
    ## Barre latérale contenant les entrées de l'application
    sidebarPanel(
      textInput("number", "Nombre", "26"),
      numericInput("fromBase", "Depuis la base:", "10")
    ),
    ## Panneau principal contenant les sorties de l'application
    mainPanel(
      column(4, 
             numericInput("toBase1", "Vers la base:", "10"),
             verbatimTextOutput("toBase1")
             ),
      column(4, 
             numericInput("toBase2", "Vers la base:", "2"),
             verbatimTextOutput("toBase2")
             ),
    column(4, 
           numericInput("toBase3", "Vers la base:", "16"),
           verbatimTextOutput("toBase3")
           )
    )
  )
)

## Construit un objet représentant l'application
shinyApp(ui = ui, server = server)
