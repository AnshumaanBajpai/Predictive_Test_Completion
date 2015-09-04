## loading required libraries
library(shiny)
library(data.table)

source("models.R")


shinyServer(function(input, output){
        output$out_str <- renderText(input$inp_str)
        output$n_pred <- renderText(input$n_pred)
        predictions <- reactive({
                pred <- StupidBackOff(input$inp_str, input$n_pred)
        })
        output$prediction <- renderText({
                paste(predictions(), collapse = " > ")
        })
})