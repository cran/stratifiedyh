#' Stratify and Assign Labels to Data
#'
#' This function stratifies data based on a specified grouping column and assigns "Yes" or "No" labels according to a given percentage.
#'
#' @param df A data frame to be stratified.
#' @param group_col A character string specifying the column name to group by.
#' @param yes_percentage A numeric value between 0 and 100 indicating the percentage of "Yes" labels to assign within each group.
#'
#' @return A data frame with an additional column "Sampled_Yes_No" containing the stratified "Yes"/"No" labels.
#' @export
#' @examples
#' # Example with the iris dataset
#' result <- stratified_labels(iris, group_col = "Species", yes_percentage = 50)
stratified_labels <- function(df, group_col, yes_percentage) {
  # Check if the selected column exists
  if (!(group_col %in% colnames(df))) {
    stop("The specified group column does not exist in the dataframe.")
  }

  # Check if the percentage is valid
  if (is.na(yes_percentage) || yes_percentage < 0 || yes_percentage > 100) {
    stop("Yes percentage must be a number between 0 and 100.")
  }

  # Convert the selected column to a factor if it is not already
  df[[group_col]] <- as.factor(df[[group_col]])

  # Get unique groups from the selected column
  groups <- unique(df[[group_col]])

  # Initialize an empty vector to store the Yes/No results
  yes_no_vector <- character(nrow(df))

  # Loop through each group to perform stratified sampling
  for (group in groups) {
    # Subset the dataframe for the current group
    group_indices <- which(df[[group_col]] == group)
    group_size <- length(group_indices)

    # Calculate the number of "Yes" entries needed
    num_yes <- round(group_size * yes_percentage / 100)
    num_yes <- min(num_yes, group_size)  # Ensure num_yes does not exceed group_size
    num_no <- group_size - num_yes  # Calculate the number of "No" entries

    # Create the Yes/No labels for the current group
    yes_no_labels <- c(rep("Yes", num_yes), rep("No", num_no))

    # Shuffle the labels to avoid patterns
    yes_no_labels <- sample(yes_no_labels)

    # Assign the labels to the corresponding rows in the vector
    yes_no_vector[group_indices] <- yes_no_labels
  }

  # Add the Yes/No column to the dataframe
  df$Sampled_Yes_No <- yes_no_vector

  # Return the modified dataframe
  return(df)
}

#' Stratify and Assign Custom Labels to Data
#'
#' This function stratifies data based on a specified grouping column and assigns custom labels according to a given percentage.
#'
#' @param df A data frame to be stratified.
#' @param group_col A character string specifying the column name to group by.
#' @param label_percentage A numeric value between 0 and 100 indicating the percentage of the first label to assign within each group.
#' @param label1 A character string representing the first label.
#' @param label2 A character string representing the second label.
#'
#' @return A data frame with an additional column "Custom_Labels" containing the stratified custom labels.
#' @export
#' @examples

#' result <- stratified_custom_labels(iris, group_col = "Species",
#'                                    label_percentage = 50,
#'                                    label1 = "High", label2 = "Low")

stratified_custom_labels <- function(df, group_col, label_percentage, label1, label2) {
  # Check if the selected column exists
  if (!(group_col %in% colnames(df))) {
    stop("The specified group column does not exist in the dataframe.")
  }

  # Check if the percentage is valid
  if (is.na(label_percentage) || label_percentage < 0 || label_percentage > 100) {
    stop("Please enter a valid percentage between 0 and 100.")
  }

  # Convert the selected column to a factor if it is not already
  df[[group_col]] <- as.factor(df[[group_col]])

  # Get unique groups from the selected column
  groups <- unique(df[[group_col]])

  # Initialize an empty vector to store the custom labels
  custom_labels_vector <- character(nrow(df))

  # Loop through each group to perform stratified sampling with custom labels
  for (group in groups) {
    # Subset the dataframe for the current group
    group_indices <- which(df[[group_col]] == group)
    group_size <- length(group_indices)

    # Calculate the number of first label entries needed
    num_label1 <- round(group_size * label_percentage / 100)
    num_label1 <- min(num_label1, group_size)  # Ensure num_label1 does not exceed group_size
    num_label2 <- group_size - num_label1  # Calculate the number of second label entries

    # Create the custom labels for the current group
    custom_labels <- c(rep(label1, num_label1), rep(label2, num_label2))

    # Shuffle the labels to avoid patterns
    custom_labels <- sample(custom_labels)

    # Assign the labels to the corresponding rows in the vector
    custom_labels_vector[group_indices] <- custom_labels
  }

  # Add the custom label column to the dataframe
  df$Custom_Labels <- custom_labels_vector

  # Return the modified dataframe
  return(df)
}

#' Apply Custom Transformation to Data Column
#'
#' This function allows the user to apply a custom transformation (scaling, normalization, log transform, or custom function) to a specified numeric column.
#'
#' @param df A data frame containing the data.
#' @param selected_column A character string specifying the column to be transformed.
#' @param transformation_type A character string representing the transformation type: "scale", "normalize", "log", or a custom R function.
#'
#' @return A data frame with the transformed column.
#' @export
#' @examples
#' result <- custom_transform(iris, selected_column = "Sepal.Length", transformation_type = "scale")
custom_transform <- function(df, selected_column, transformation_type) {
  if (!(selected_column %in% colnames(df))) {
    stop("The selected column does not exist in the dataframe.")
  }

  if (!is.numeric(df[[selected_column]]) && !is.integer(df[[selected_column]])) {
    stop("The selected column is not numeric or integer.")
  }

  if (transformation_type == "scale") {
    df[[selected_column]] <- scale(df[[selected_column]])
  } else if (transformation_type == "normalize") {
    min_val <- min(df[[selected_column]], na.rm = TRUE)
    max_val <- max(df[[selected_column]], na.rm = TRUE)
    df[[selected_column]] <- (df[[selected_column]] - min_val) / (max_val - min_val)
  } else if (transformation_type == "log") {
    df[[selected_column]] <- log(df[[selected_column]] + 1)
  } else if (is.function(transformation_type)) {
    df[[selected_column]] <- transformation_type(df[[selected_column]])
  } else {
    stop("Invalid transformation type.")
  }

  return(df)
}

#' Cluster Sampling and Labeling
#'
#' This function performs cluster sampling on the dataframe and assigns "Yes" or "No" labels to rows based on selected clusters.
#'
#' @param df A data frame containing the data.
#' @param group_col A character string specifying the column to use for clustering.
#' @param yes_percentage A numeric value between 0 and 100 indicating the percentage of clusters to label as "Yes".
#'
#' @return A data frame with an additional column "Clustered_Yes_No" containing the cluster-sampled "Yes"/"No" labels.
#' @export
#' @examples
#' result <- cluster_labels(iris, group_col = "Species", yes_percentage = 50)
cluster_labels <- function(df, group_col, yes_percentage) {
  if (!(group_col %in% colnames(df))) {
    stop("The selected column does not exist in the dataframe.")
  }

  if (is.na(yes_percentage) || yes_percentage < 0 || yes_percentage > 100) {
    stop("Yes percentage must be a number between 0 and 100.")
  }

  df[[group_col]] <- as.factor(df[[group_col]])
  clusters <- unique(df[[group_col]])
  num_clusters <- length(clusters)
  num_yes_clusters <- round(num_clusters * yes_percentage / 100)
  num_yes_clusters <- min(num_yes_clusters, num_clusters)

  yes_clusters <- sample(clusters, num_yes_clusters)
  yes_no_vector <- character(nrow(df))

  for (i in 1:nrow(df)) {
    if (df[[group_col]][i] %in% yes_clusters) {
      yes_no_vector[i] <- "Yes"
    } else {
      yes_no_vector[i] <- "No"
    }
  }

  df$Clustered_Yes_No <- yes_no_vector

  return(df)
}

#' Systematic Sampling and Labeling
#'
#' This function performs systematic sampling on the dataframe and assigns "Yes" or "No" labels to rows based on the specified interval.
#'
#' @param df A data frame containing the data.
#' @param group_col A character string specifying the column to use for grouping.
#' @param sampling_interval A numeric value representing the interval for systematic sampling.
#'
#' @return A data frame with an additional column "Systematic_Yes_No" containing the systematically sampled "Yes"/"No" labels.
#' @export
#' @examples
#' result <- systematic_labels(iris, group_col = "Species", sampling_interval = 2)
systematic_labels <- function(df, group_col, sampling_interval) {
  if (!(group_col %in% colnames(df))) {
    stop("The selected column does not exist in the dataframe.")
  }

  if (is.na(sampling_interval) || sampling_interval <= 0) {
    stop("Please enter a valid sampling interval greater than 0.")
  }

  df <- df[order(df[[group_col]]), ]
  n <- nrow(df)
  yes_no_vector <- rep("No", n)
  start_pos <- sample(1:sampling_interval, 1)
  yes_indices <- seq(start_pos, n, by = sampling_interval)
  yes_no_vector[yes_indices] <- "Yes"

  df$Systematic_Yes_No <- yes_no_vector

  return(df)
}
