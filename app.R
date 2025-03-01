library(shiny)
library(tidyverse)
library(mlr3verse)
library(mlr3proba)
library(mlr3extralearners)
library(survival)
library(randomForestSRC)

newsample_input <- function(xname_ml, xname_ui, traindata){
  xi <- traindata[[xname_ml]]
  if (is.numeric(xi)) {
    numericInput(inputId = xname_ml, 
                 label = xname_ui, 
                 value = max(xi))
  } else {
    selectInput(inputId = xname_ml, 
                label = xname_ui, 
                choices = levels(xi),
                selected = levels(xi)[2])
  }
}


set.seed(1234)
load("rsf.RData")
xname_train4sadata <- colnames(traindata4sadata)[3:14]
xname_input4sadata <- colnames(traindata4sadata)[3:14]
interesttimepoints <- c(12 * c(1, 3, 5))


###############################################################
# Define UI
ui <- fluidPage(
  column(width = 1),
  column(
    width = 8,
    navbarPage(
      title = strong("Random Survival Forest Model-Based Tool"),
      ########################################
      tabPanel(
        "Predicting Postoperative Survival Rates in Patients with Locally Advanced Gastric Cancer",
        sidebarLayout(
          sidebarPanel(
            #h2("Input the information of the patient to be predicted"),
            map2(xname_train4sadata, 
                 xname_input4sadata, 
                 newsample_input, 
                 traindata = traindata4sadata),
            actionButton("predict4sadata", "predict", class = "btn-primary")
          ),
          mainPanel(
            
            h2("Survival Rate Prediction"),
            fluidRow(
              ###################################
              column(
                width = 8,
                span(style = "font-size:25px;", 
                     textOutput("prediction4sadata1_rsf")), 
                span(style = "font-size:25px;", 
                     textOutput("prediction4sadata2_rsf")), 
                span(style = "font-size:25px;", 
                     textOutput("prediction4sadata3_rsf"))
              ),
              ###################################
            ),
            hr(),
            h2("Estimated Survival Curve"),
            plotOutput("plot4sadata", height = "400px"),
            hr(),
            h2("Prediction Summary"),
            DT::DTOutput("newsample4sadata")
          )
        )
      )
    )
  ),
  column(width = 1)
)

###############################################################

# Define server logic
server <- function(input, output) {
  values <- reactiveValues(samples = data.frame(),
                           samples2 = data.frame(),
                           tempdf = data.frame())
  observeEvent(input$predict4sadata, {
    new_sample4sadata <-
      data.frame(matrix(NA,
                        nrow = 1,
                        ncol = length(xname_train4sadata)))
    for (i in seq_along(xname_train4sadata)) {
      valuei4sadata <- input[[xname_train4sadata[i]]]
      new_sample4sadata[1, i] <- valuei4sadata
    }
    names(new_sample4sadata) <- xname_train4sadata
    traindata4sadata[nrow(traindata4sadata),xname_train4sadata] <- new_sample4sadata
    new_sample24sadata <- traindata4sadata
    lastN4sadata <- nrow(new_sample24sadata)
    
    ###################################
    predtrain_rsf <- 
      learner_rsf4sadata$predict_newdata(new_sample24sadata)
    predprobtrain_rsf <- 
      predtrain_rsf$distr[
        1:nrow(new_sample24sadata)
      ]$survival(interesttimepoints) %>%
      t() %>%
      as.data.frame() %>%
      mutate(id = 1,
             model = "rsf")
    prediction4sadata_rsf <- predprobtrain_rsf[lastN4sadata,]
    output$prediction4sadata1_rsf <- renderText({
      paste("The 1-year survival probability:", 
            round(prediction4sadata_rsf[[1]], 4))
    })
    output$prediction4sadata2_rsf <- renderText({
      paste("The 3-year survival probability:", 
            round(prediction4sadata_rsf[[2]], 4))
    })
    output$prediction4sadata3_rsf <- renderText({
      paste("The 5-year survival probability:", 
            round(prediction4sadata_rsf[[3]], 4))
    })
    values$samples <- rbind(
      values$samples,
      predtrain_rsf$distr[
        1:nrow(new_sample24sadata)
      ]$survival(unique(traindata4sadata$rfstime)) %>%
        t() %>%
        as.data.frame() %>%
        mutate(model = "rsf") %>%
        tail(1)
    )
    
    
    ###################################
    
    output$plot4sadata <- renderPlot({
      values$samples %>%
        mutate(id = 1:n()) %>%
        pivot_longer(cols = 1:length(unique(traindata4sadata$rfstime))) %>%
        mutate(name = as.numeric(name),
               id = factor(id)) %>%
        ggplot(aes(x = name, y = value, group = id, color = id)) +
        geom_line(linewidth = 1) +
        scale_y_continuous(limits = c(0, 1)) +
        labs(x = "Time (Months)", y = "Survival Probability") +
        theme_minimal()
    })
    values$samples2 <- rbind(
      values$samples2,
      # new_sample4sadata
      cbind(new_sample4sadata, 
            round(prediction4sadata_rsf[, c(1,2,3)], 4))
    )
    values$tempdf <- values$samples2 %>%
      select(any_of(xname_train4sadata), everything())
    colnames(values$tempdf)[1:length(xname_input4sadata)] <- 
      xname_input4sadata
    rownames(values$tempdf) <- NULL
  
  })
  output$newsample4sadata <- DT::renderDT({
    values$tempdf
  }, options = list(scrollX = T, pageLength = 3, dom = "tp"))
  
}

###############################################################

# Run the application 
shinyApp(ui = ui, server = server)
