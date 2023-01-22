library(ggplot2)
library(tidyr)

n_sample <<- 1000
n_trial <<- 10

mean_actual <<- 5.0
variance_actual <<- 1.0
stddev_actual <<- sqrt(variance_actual)


mean_interval_estimator <- function(sample, confidence_level = 0.95) {
    mean_estimator <- mean(sample)
    
    print(mean_estimator)
    
    student_quantile <- qt(1 - confidence_level / 2, n_sample-1)
    
    mean_confidence <- c(
        mean_estimator - student_quantile, 
        mean_estimator + student_quantile
    )
    
    return(mean_confidence)
}


variance_interval_estimator <- function(sample, confidence_level = 0.95) {
    variance_estimator <- var(sample)
    
    print(variance_estimator)
    
    chisq_left_quantile <- qchisq(1 - confidence_level / 2, n_sample - 1)
    chisq_right_quantile <- qchisq(confidence_level / 2, n_sample - 1)
    
    variance_confidence <- c(
        (n_sample-1) * variance_estimator / chisq_left_quantile,
        (n_sample-1) * variance_estimator / chisq_right_quantile
    )
    
    return(variance_confidence)
}


generate_samples <- function() {
    data_frame <- data.frame(
        matrix(
            rnorm(n_sample * n_trial, mean_actual, stddev_actual), 
            nrow = n_sample)
    )
    
    return(data_frame)
}


data_frame <- generate_samples()

data_frame_tidy <- gather(data_frame, cols, value) 

density_plot <- ggplot(data_frame_tidy, aes(x = value)) + geom_density(aes(color=cols))

