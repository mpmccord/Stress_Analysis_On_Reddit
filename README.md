# Stress_Analysis_On_Reddit
Analyzing the [Kaggle Stress Analysis on Reddit dataset:](https://www.kaggle.com/ruchi798/stress-analysis-in-social-media), wrangling the data and creating some visualizations of the data.
## Overview of Major Folders and Files:
_ __Stress_Analysis_On_Social_Media.Rmd__: RMarkdown file that runs the functions listed above on the dataset.
- __reddit_stress_data__: folder that stores the dataset.
- __data_visualization_functions__:
  - plot_frequency_distribution: functions for plotting the top n words from the dataset.
  
- __feature_creation_functions__:
  - create_bag_of_words_model.R: creates a bag of words model of the dataset
  - create_sentiment_model.R:    creates a sentiment model of the dataset
  
- __feature_selection_functions__:
  - remove_overly_correlated_features.R: feature selection by removing overly correlated features.
  
- __model_training_functions__:
  - train_test_split.R: partition the data randomly into train and test.

