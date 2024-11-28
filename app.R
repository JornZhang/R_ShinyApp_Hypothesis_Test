library(shiny)
library(ggplot2)
library(plotly)

# Define UI
ui <- fluidPage(
  titlePanel("Hypothesis Testing Simulation"),
  sidebarLayout(
    sidebarPanel(
      selectInput("distribution", "Select Distribution:", choices = c("Normal", "Uniform", "Exponential")),
      numericInput("mu", HTML("Population Mean (&mu;):"), value = 0),
      numericInput("mu0", HTML("Hypothesized Mean (&mu;<sub>0</sub>):"), value = 0),
      sliderInput("n", "Sample Size:", min = 10, max = 500, value = 30),
      numericInput("nsim", "Number of Simulations:", value = 100),
      actionButton("go", "Run Simulation")
    ),
    mainPanel(
      plotlyOutput("intervalPlot"),
      plotlyOutput("tStatPlot"),
      textOutput("successRate")  # Display for the success rate
    )
  )
)



# Define server logic
server <- function(input, output) {
  observeEvent(input$go, {
    nsim <- input$nsim
    n <- input$n
    mu <- input$mu
    mu0 <- input$mu0
    distribution <- input$distribution
    
    simulateData <- reactive({
      set.seed(123)
      samples <- lapply(1:nsim, function(i) {
        if (distribution == "Normal") {
          rnorm(n, mean = mu, sd = 1)
        } else if (distribution == "Uniform") {
          runif(n, min = mu - sqrt(3), max = mu + sqrt(3))
        } else {
          rexp(n, rate = 1/mu)
        }
      })
      
      t_stats <- sapply(samples, function(sample) {
        sample_mean <- mean(sample)
        sample_sd <- sd(sample)
        se <- sample_sd / sqrt(n)
        t <- (sample_mean - mu0) / se
        lower_ci <- sample_mean - qt(0.975, df = n-1) * se
        upper_ci <- sample_mean + qt(0.975, df = n-1) * se
        c(lower_ci, upper_ci, t)
      })
      
      data.frame(
        LowerCI = t_stats[1, ],
        UpperCI = t_stats[2, ],
        T = t_stats[3, ],
        ContainsMu0 = t_stats[1, ] <= mu0 & t_stats[2, ] >= mu0
      )
    })
    
    output$intervalPlot <- renderPlotly({
      data <- simulateData()
      plot_data <- data.frame(
        Sample = 1:nsim,
        LowerCI = data$LowerCI,
        UpperCI = data$UpperCI,
        ContainsMu0 = data$ContainsMu0
      )
      p <- ggplot(plot_data, aes(x = Sample, ymin = LowerCI, ymax = UpperCI, color = ContainsMu0)) +
        geom_linerange() +
        scale_color_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
        labs(title = "Confidence Intervals", 
             y = "Interval", 
             x = "Sample Index",
             color = "capture mu_0")
      ggplotly(p)
    })
    
    output$tStatPlot <- renderPlotly({
      data <- simulateData()
      hist_data <- hist(data$T, breaks = 40, plot = FALSE)
      p <- ggplot(data = data, aes(x = T)) +
        geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "blue", alpha = 0.5) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "red", size = 1.5) +
        labs(title = "T-Statistics Distribution", x = "T-Statistic", y = "Density")
      ggplotly(p)
    })
    
    
    output$successRate <- renderText({
      data <- simulateData()
      success_rate <- mean(data$ContainsMu0)
      sprintf("Success Rate of CIs to capture mu_0: %.2f%%", success_rate * 100)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
