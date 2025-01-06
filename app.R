library(shiny)

ui <- fluidPage(
  titlePanel("Survey Viability Score Calculator"),
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = "sampling_frame",
        label = "Sampling Frame:",
        value = 100000,
        min = 1
      ),
      numericInput(
        inputId = "response_rate",
        label = "Response Rate (%):",
        value = 2.5,
        min = 0.01
      ),
      numericInput(
        inputId = "target_sample_size",
        label = "Target Sample Size:",
        value = 400,
        min = 1
      ),
      br(),
      p("The survey viability (SV) score is simply the ratio of survey invites needed to the survey invites available."),
      p("Survey Viability = Sampling Frame   / (Target Sample Size / Response Rate)"),
      p("If your score is above 1, you have enough invites to get the responses you need. If your score is below 1, then your study will be underpowered."),
      p("If you're not sure how to get the right values to enter in the calculator, ",
        a("read the full article here", href = "https://carljpearson.com/the-survey-viability-score-my-first-question-in-survey-planning/", target = "_blank"))
    ),
    mainPanel(
      uiOutput("viabilityText")
    )
  )
)

server <- function(input, output) {
  calculate_viability <- reactive({
    sampling_frame <- input$sampling_frame
    response_rate <- input$response_rate / 100  # Convert percentage to proportion
    target_sample_size <- input$target_sample_size
    
    viability <- sampling_frame / (target_sample_size / response_rate)
    min(viability, 100)  # Cap the value at 100
  })
  
  output$viabilityText <- renderUI({
    viability <- calculate_viability()
    viability_message <- NULL
    
    if (viability >= 1) {
      viability_message <- "Your SV score is above 1 so you're likely to get the number of responses you need for analysis based on the invites you can send."
    } else if (viability >= 0.9) {
      viability_message <- "Your SV score is slightly below 1. You could run your survey as is but will have decreased power and will exhaust your survey invite pool."
    } else if (viability >= 0.5) {
      viability_message <- "Your SV score is moderately below 1. Your analysis will be underpowered and you run the risk of missing a real effect or inflating the effect size that you do find. Consider ways to increase your SV score."
    } else {
      viability_message <- "Your SV score is far below 1. Your analysis is unlikely to detect a true effect and an effect that is found is likely to be highly inflated. Consider ways to increase your SV score or what methods other than a survey may be more effective with your resources."
    }
    
    tagList(
      h1(paste("Survey viability score =", round(viability, 2))),
      p(style = "font-size: 14px;", viability_message)
    )
  })
}

shinyApp(ui = ui, server = server)
