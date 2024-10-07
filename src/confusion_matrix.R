get_confusion_matrix <- function(predictand, predicted) {
  confusion_matrix <- table(predictand, predicted)
  
  # Accuracy
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  # Precision
  precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  
  # Recall (Sensitivity)
  recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  
  # F1 Score
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # Results
  results <- data.frame(
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1_Score = f1_score
  )
  
  return (results)
}