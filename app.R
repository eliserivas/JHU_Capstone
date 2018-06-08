# Next Word Predictor App

library(dplyr)
# library(RWeka)
library(SnowballC)
library(tm)
library(stringr)
library(qdap)
library(shiny)
library(shinythemes)
library(rsconnect)
library(packrat)
library(pacman)
library(curl)
library(RCurl)

# Load N-Gram frequency tables
bigram_freq <- read.csv("bigrams.csv", stringsAsFactors = F)
trigram_freq <- read.csv("trigrams.csv", stringsAsFactors = F)
quad_freq <- read.csv("quadgrams.csv", stringsAsFactors = F)
pent_freq <- read.csv("pentgrams.csv", stringsAsFactors = F)
hex_freq <- read.csv("hexgrams.csv", stringsAsFactors = F)

# Function that predicts next word
nextWordPred <- function(input){
  # Check if it's the right data type
  if (is.character(input)==F){
    stop("the input text is not a character")
  } else if (input=="") {
    return("~ no text ~")
  } else {
    # Make content in right form
    input <- replace_contraction(input)
    input <- tolower(input)
    input <- removePunctuation(input)
    input <- removeNumbers(input)
    input <- stripWhitespace(input)
  }
  # Get length of input
  l <- str_count(input, boundary("word"))
  if (l==0){
    stop("Sorry, cannot make prediction")
  }
  ## JUST ONE WORD ### "I"
  if (l==1){
    # First try bigram, then first word of trigram, quad, pent, and hex
    if (word(input, 1, l) %in% bigram_freq$first_Nmin1){
      next_word <- bigram_freq %>% subset(first_Nmin1==word(input, l, l)) %>% select(last) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
    } else if (word(input, 1, l) %in% trigram_freq$first){
      next_word <- trigram_freq %>% subset(first==word(input, l, l)) %>% select(second)  %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
    } else if (word(input, 1, l) %in% quad_freq$first){
      next_word <- quad_freq %>% subset(first==word(input, l, l)) %>% select(second) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
    } else if (word(input, 1, l) %in% pent_freq$first){
      next_word <- pent_freq %>% subset(first==word(input, l, l)) %>% select(second) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
    } else {
      return("the")
    }
    ## TWO WORDS ### "I like"  
  } else if (l==2){
    # First try trigram, then go to quad, pent, hex
    if (word(input, 1, l) %in% trigram_freq$first_Nmin1){
      next_word <- trigram_freq %>% subset(first_Nmin1==word(input, 1, l)) %>% select(last) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
    } else if (word(input, 1, l) %in% quad_freq$first2){
      next_word <- quad_freq %>% subset(first2==word(input, 1, l)) %>% select(third) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
    } else if (word(input, 1, l) %in% pent_freq$first2){
      next_word <- pent_freq %>% subset(first2==word(input, 1, l)) %>% select(third) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
    } else if (word(input, 1, l) %in% hex_freq$first2){
      next_word <- hex_freq %>% subset(first2==word(input, 1, l)) %>% select(third) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
      # If testing the higher levels doesn't work, then go to bigram
    } else if (word(input, l, l) %in% bigram_freq$first_Nmin1){
      next_word <- bigram_freq %>% subset(first_Nmin1==word(input, l, l)) %>% select(last) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
    } else {
      return("the")
    }
    ## THREE WORDS ### "I like when"
  } else if (l==3){
    # First go to quad-gram, the pent, and hex
    if (word(input, 1, l) %in% quad_freq$first_Nmin1){
      next_word <- quad_freq %>% subset(first_Nmin1==word(input, 1, l)) %>% select(last) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
    } else if (word(input, 1, l) %in% pent_freq$first3){
      next_word <- pent_freq %>% subset(first3==word(input, 1, l)) %>% select(fourth) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
    } else if (word(input, 1, l) %in% hex_freq$first3){
      next_word <- hex_freq %>% subset(first3==word(input, 1, l)) %>% select(fourth) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
      # If that doesn't work, go to trigram
    } else if (word(input, l-1, l) %in% trigram_freq$first_Nmin1){
      next_word <- trigram_freq %>% subset(first_Nmin1==word(input, l-1, l)) %>% select(last) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
      # Then the bigram
    } else if (word(input, l, l) %in% bigram_freq$first_Nmin1){
      next_word <- bigram_freq %>% subset(first_Nmin1==word(input, l, l)) %>% select(last) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
    } else {
      return("the")
    }
    ## FOUR WORDS ### "I like when you"
  } else if (l==4){
    # First go to the pent-gram, then hex
    if (word(input, 1, l) %in% pent_freq$first_Nmin1){
      next_word <- pent_freq %>% subset(first_Nmin1==word(input, 1, l)) %>% select(last) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
    } else if (word(input, 1, l) %in% hex_freq$first4){
      next_word <- hex_freq %>% subset(first4==word(input, 1, l)) %>% select(fifth) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
      # If that doesn't work, then go to quad, tri, and bi
    } else if (word(input, l-2, l) %in% quad_freq$first_Nmin1){
      next_word <- quad_freq %>% subset(first_Nmin1==word(input, l-2, l)) %>% select(last) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
    } else if (word(input, l-1, l) %in% trigram_freq$first_Nmin1){
      next_word <- trigram_freq %>% subset(first_Nmin1==word(input, l-1, l)) %>% select(last) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
    } else if (word(input, l, l) %in% bigram_freq$first_Nmin1){
      next_word <- bigram_freq %>% subset(first_Nmin1==word(input, l, l)) %>% select(last) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
    } else {
      return("the")
    }
    ## FIVE + WORDS ### "I like when you speak"
  } else if (l>=5) {
    # First go to hex-gram, then 5, 4, 3, 2, 1
    if (word(input, l-4, l) %in% hex_freq$first_Nmin1){
      next_word <- hex_freq %>% subset(first_Nmin1==word(input, l-4, l)) %>% select(last) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
    } else if (word(input, l-3, l) %in% pent_freq$first_Nmin1){
      next_word <- pent_freq %>% subset(first_Nmin1==word(input, l-3, l)) %>% select(last) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
    } else if (word(input, l-2, l) %in% quad_freq$first_Nmin1){
      next_word <- quad_freq %>% subset(first_Nmin1==word(input, l-2, l)) %>% select(last) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
    } else if (word(input, l-1, l) %in% trigram_freq$first_Nmin1){
      next_word <- trigram_freq %>% subset(first_Nmin1==word(input, l-1, l)) %>% select(last) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
    } else if (word(input, l, l) %in% bigram_freq$first_Nmin1){
      next_word <- bigram_freq %>% subset(first_Nmin1==word(input, l, l)) %>% select(last) %>% slice(1)
      nw <- as.character(next_word)
      if (nw=="i"){nw <- "I"}
      return(nw)
    } else {
      return("the")
    }
  } else {
    return("the")
  }
}

# UI ####
ui <- fluidPage(
  titlePanel("Word Prediction Application"),
  sidebarLayout(
    sidebarPanel(
      h3(em("About the Application")),
      br(),
      h3(strong("Instructions")),
      p("Type in your text into the box to see what the next word is. The predicted word and complete text will appear beneath"),
      h3(strong("About")),
      p("This is the Capstone project for the Coursera/JHU Data Science Specialization, utilizing texts provided by SwiftKey from blogs, news, and tweets"),
      br(),
      p("Using these sources, we create a model and app that predict the next word of a phrase typed by the user. The algorithm leverages N-Gram subsets of the text and the Katz Backoff model"),
      br(),
      p("Text is first matched to phrases in the 6-gram matrix. If there is no match, it reverts, or 'backs off' (a nod to Katz) to the five-gram, four-gram, trigram, then bigram lists"),
      br(),
      br(),
      br(),
      br(),
      br()
    ),
    mainPanel(
      h2(strong("Your Phrase")),
      fluidRow(column(width=12, div(style = "height:5px;width:100%;background-color: black;"))),
      br(),
      textInput(inputId = "phrase", label = "Place your phrase here!"),
      br(),
      br(),
      h4(strong("Next Word")),
      fluidRow(column(width=12, div(style = "height:5px;width:100%;background-color: green;"))),
      fluidRow(column(width=12),
               tags$blockquote(textOutput("nextword"))),
      h2(em(strong("Entire Phrase"))),
      fluidRow(column(width=12, div(style = "height:5px;width:100%;background-color: turquoise;"))),
      fluidRow(column(width=12),
               tags$blockquote(textOutput("totalphrase"))
    )
  )
)
  )

                


# SERVER ####
server <- function(input, output) {
  output$nextword <- renderText(nextWordPred(input$phrase))
  
  output$totalphrase <- renderText(paste(input$phrase, nextWordPred(input$phrase), sep=" "))
}

# Run the application 
shinyApp(ui = ui, server = server)


