DecodeDigits <- function(word, alphabet = c(0:9, LETTERS)) {
  digits <- unlist(strsplit(toupper(word), NULL)) 
  return(match(digits, alphabet)-1)
}

#' Décode une chaîne de caractère word représentant un nombre n écrit dans une base comprise entre 2 et 36. 
#'
#' @param word la chaîne de caractère représentant le nombre ne contenant que des caractères alphanumériques.
#' @param base la base dans laquelle est écrite le nombre
#' @return le nombre n ou NA si le format de la chaîne est invalide.
DecodeNumber <- function(word, base) {
  n <- 0
  l <- nchar(word)
  p <- 0
  if(base < 2 || base > 36){return(NA)}
  for (i in l:1) {
    if(DecodeDigits(substr(word,i,i)) >= base){return(NA)}
    n <- n + DecodeDigits(substr(word,i,i)) * base**p
    p <- p + 1
  }
  return(n)
}

#' Écrit le nombre n dans une chaîne de caractères en utilisant un base comprise entre 2 et 36
#' @param n le nombre à encoder
#' @param base la base dans laquelle écrire le nombre
#' @return la chaîne de caractère contenant le nombre n écrit dans la base
EncodeNumber <- function(n, base) {
  if(base<2 || base>36){return(NA)}
  acc <- c()
  symboles <- c(0:9,LETTERS)
  while (n>0) {
    if(n%%base > 9){
      acc <- append(acc,symboles[(n%%base)+1])
      n <- n%/%base
      }
    acc <- append(acc,n%%base)
    n <- n%/%base
  }
  res <- paste(rev(acc),collapse = '')
  return(res)
}

#' Écrit un nombre n dans une chaîne de caractère avec le système bibi-binaire
#' @param n le nombre à encoder
#' @return la chaîne de caractère contenant le nombre n écrit en bibi-binaire
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


