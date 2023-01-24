library(ggplot2)
library(tidyr)
library(gridExtra)
library(shiny)


# Data generator for a normal distribution:
generate_samples_normal <- function() {
  data_frame <- data.frame(
    matrix(
      rnorm(n_sample * n_trial, mean_actual, stddev_actual), 
      nrow = n_sample
    )
  )
  
  return(data_frame)
}


# Data generator for a Poisson distribution:
generate_samples_poisson <- function() {
  data_frame <- data.frame(
    matrix(
      rpois(n_sample * n_trial, lambda = poisson_lambda),
      nrow = n_sample
    )
  )
  
  return(data_frame)
}


# Mean value confidence interval calculator:
mean_interval_estimator <- function(sample) {
  mean_estimator <- mean(sample)
  std_dev_estimator <- sd(sample)
  
  alpha <- 1 - mean_confidence
  
  student_quantile <- qt(1 - alpha / 2, n_sample-1)
  std_error <- std_dev_estimator / sqrt(n_sample - 1)
  
  mean_confidence <- c(
    mean_estimator - student_quantile * std_error, 
    mean_estimator + student_quantile * std_error
  )
  
  return(mean_confidence)
}


# Variance confidence interval calculator:
variance_interval_estimator <- function(sample) {
  variance_estimator <- var(sample)
  
  alpha <- 1 - variance_confidence
  
  chisq_left_quantile <- qchisq(1 - alpha / 2, n_sample - 1)
  chisq_right_quantile <- qchisq(alpha / 2, n_sample - 1)
  
  variance_confidence <- c(
    (n_sample-1) * variance_estimator / chisq_left_quantile,
    (n_sample-1) * variance_estimator / chisq_right_quantile
  )
  
  return(variance_confidence)
}


# Sample data frame to mean value interval data frame mapper:
mean_interval_data_frame <- function(data_frame) {
  
  mean_frame = data.frame(
    trial = 1:n_trial,
    value = apply(data_frame, 2, mean),
    low = apply(data_frame, 2, mean_interval_estimator)[1,],
    high = apply(data_frame, 2, mean_interval_estimator)[2,]
  )
  
  return(mean_frame)
}


# Sample data frame to variance confidence interval data frame mapper:
variance_interval_data_frame <- function(data_frame) {
  
  var_frame = data.frame(
    trial = 1:n_trial,
    value = apply(data_frame, 2, var),
    low = apply(data_frame, 2, variance_interval_estimator)[1,],
    high = apply(data_frame, 2, variance_interval_estimator)[2,]
  )
  
  return(var_frame)
}


# Confidence interval data frame plot function:
confidence_interval_plot <- function(interval_frame, line_intercept) { 
  plot <- ggplot() +  
    geom_pointrange(
      data = interval_frame,
      mapping = aes(
        x = trial,
        y = value,
        ymin = low,
        ymax = high
      )
    ) + 
    geom_abline(
      aes(
        intercept = line_intercept, 
        slope = 0
      )
    )
  
  return(plot)
}


# A simple Shiny app user interface:
ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      h2("Parametry"),
      numericInput(
        inputId = "n_trial_input",
        label = "Liczba prób",
        value = 100,
        min = 1,
        step = 1
      ),
      
      numericInput(
        inputId = "n_sample_input",
        label = "Liczba liczb w próbie",
        value = 1000,
        min = 1,
        step = 1
      ),
      
      selectInput(
        inputId = "distribution_type",
        label = "Typ rozkładu",
        choices = c("Normalny", "Poissona"),
        selected = "Rozkład normalny"
      ),
    
      numericInput(
        inputId = "mean_actual_input",
        label = "Wartość oczekiwana w rozkładzie normalnym",
        value = 3.0,
        min = 0.0,
        max = 5.0,
        step = 0.01
      ),
      
      numericInput(
        inputId = "variance_actual_input",
        label = "Wariancja w rozkładzie normalnym",
        value = 2.0,
        min = 0.01,
        max = 5.0,
        step = 0.01
      ),
      
      numericInput(
        inputId = "poisson_lambda_input",
        label = "Prawdopodobieństwo sukcesu w rozkładzie Poissona",
        value = 3.0,
        min = 0.01,
        max = 5.0,
        step = 0.01
      ),
      
      numericInput(
        inputId = "mean_confidence_input",
        label = "Współczynnik ufności dla średniej",
        value = 0.95,
        min = 0.01,
        max = 0.99,
        step = 0.01
      ),
      
      numericInput(
        inputId = "variance_confidence_input",
        label = "Współczynnik ufności dla wariancji",
        value = 0.95,
        min = 0.01,
        max = 0.99,
        step = 0.01
      ),
      
      actionButton("start", "Start")
    ), 
  
    mainPanel(
      h2("Wartość oczekiwana:"),
      plotOutput(outputId = "mean_plot"),
      h2("Wariancja:"),
      plotOutput(outputId = "variance_plot")
    )
  )
  
)


# A server function for Shiny app:
server <- function(input, output, session) {
  
  result_mean <- eventReactive(
    input$start,
    {
      n_trial <<- input$n_trial_input
      n_sample <<- input$n_sample_input
      mean_actual <<- input$mean_actual_input
      variance_actual <<- input$variance_actual_input
      stddev_actual <<- sqrt(variance_actual)
      poisson_lambda <<- input$poisson_lambda_input
      mean_confidence <<- input$mean_confidence_input
      variance_confidence <<- input$variance_confidence_input
      
      mean_intercept <- 0
      
      if(input$distribution_type == "Normalny") {
        data_frame <- generate_samples_normal()
        mean_intercept <- mean_actual
      }
      else {
        data_frame <- generate_samples_poisson()
        mean_intercept <- poisson_lambda
      }
      
      mean_data_frame <- mean_interval_data_frame(data_frame)
      
      mean_plot <- confidence_interval_plot(mean_data_frame, mean_intercept)
      
      return(mean_plot)
    }
  )
  
  result_variance <- eventReactive(
    input$start,
    {
      #n_trial <<- input$n_trial_input
      #n_sample <<- input$n_sample_input
      #mean_actual <<- input$mean_actual_input
      #variance_actual <<- input$variance_actual_input
      #stddev_actual <<- sqrt(variance_actual)
      #poisson_lambda <<- input$poisson_lambda_input
      #variance_confidence <<- input$variance_confidence_input
      
      variance_intercept <- 0
      
      if(input$distribution_type == "Normalny") {
        data_frame <- generate_samples_normal()
        variance_intercept <- variance_actual
      }
      else {
        data_frame <- generate_samples_poisson()
        variance_intercept <- poisson_lambda
      }
      
      variance_data_frame <- variance_interval_data_frame(data_frame)

      variance_plot <- confidence_interval_plot(variance_data_frame, variance_intercept)
      
      return(variance_plot)
    }
  )
  

  output$mean_plot <- renderPlot(result_mean())
  output$variance_plot <- renderPlot(result_variance())
}


shinyApp(ui = ui, server = server)
