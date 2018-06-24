# require some helpful packages
require(psych)
require(ggplot2)

targetMean <- 41
targetSD <- 23.55
targetCor <- 0.4

# Makes function for running the tests
runSimulation <- function(targetMean, targetSD, targetCor) {
  # Generates initial scores 
  true <- rnorm(n = 10000, mean = targetMean, sd = targetSD)
  random <- rnorm(n = 10000, mean = targetMean, sd = targetSD)
  
  # Creates an input variable with a specified correlation with the true data
  input <- true
  for (i in 1:10000){
    error <- rnorm(n = 10000, mean = 0, sd = (SD(input)*.1))
    input <- input + error
    newCor <- cor(true, input)

    if (newCor < targetCor){
      print(newCor)
      break;
    }
  }
  
  # Puts together the three variabels from above into a data frame
  df <- data.frame(true=true, random=random, input=input)
  
  # Runs a regression model to predict the true scores from the input scores
  m <- lm(true~input, df)
  
  # Generates predictions using the model
  df$predicted <- predict(m, df)
  
  # Graphs the true data with the predicted data
  temp <- data.frame(score=df$true, label="true")
  temp2 <- data.frame(score=df$predicted, label="predicted")
  full <- rbind(temp, temp2)
  
  ggplot(full, aes(score, fill = label)) + geom_density(alpha = 0.2)
  
  # Corrects for the predicted data's unrealistic distribution (too squeezed in)
  df$predicted.corrected <- (df$predicted - mean(df$predicted))*SD(df$true)/SD(df$predicted) + mean(df$predicted)

  # Calculates the absolute error--difference between people's true scores and some predicton
  df$diff.random <- abs(df$true - df$random) # Error for a random guess
  df$diff.mean <- abs(df$true - targetMean) # Error for assuming everyone is the average
  df$diff.predicted <- abs(df$true - df$predicted) # Error for the predicted data (squeezed in)
  df$diff.corrected <- abs(df$true - df$predicted.corrected) # Error for the corrected predicted data

  # Looks at the average absolute error  
  print(paste0("Error for random guess: ", round(mean(df$diff.random), digits=2)))
  print(paste0("Error for assuming everyone is average: ", round(mean(df$diff.mean), digits=2)))
  print(paste0("Error for predictions (not corrected for unrealistic distribution): ", round(mean(df$diff.predicted), digits=2)))
  print(paste0("Error for predictions (corrected): ", round(mean(df$diff.corrected), digits=2)))
}

# Runs simluations at different levels of accuracy for age (average and SD for USA)
runSimulation(targetMean = 41, targetSD = 23.55, targetCor = 0.1)
runSimulation(targetMean = 41, targetSD = 23.55, targetCor = 0.3)
runSimulation(targetMean = 41, targetSD = 23.55, targetCor = 0.5)
runSimulation(targetMean = 41, targetSD = 23.55, targetCor = 0.7)
runSimulation(targetMean = 41, targetSD = 23.55, targetCor = 0.998)

# Runs simluations at different levels of accuracy for personality trait (measured on scale of 1 to 7, with mean of 4, SD of 1)
runSimulation(targetMean = 4, targetSD = 1, targetCor = 0.1)
runSimulation(targetMean = 4, targetSD = 1, targetCor = 0.3)
runSimulation(targetMean = 4, targetSD = 1, targetCor = 0.5)
runSimulation(targetMean = 4, targetSD = 1, targetCor = 0.7)
runSimulation(targetMean = 4, targetSD = 1, targetCor = 0.9)
