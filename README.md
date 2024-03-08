# Critical Infrastructure Protection: Behaviour-Based Intrusion Detection

This repository contains code and solutions for a project on critical infrastructure protection, focusing on enhancing resilience against cyber threats through behaviour-based online intrusion detection. The project utilizes real-time monitoring and analysis of control signals from a cyber-physical system to detect anomalies. The provided dataset, extracted from supervisory control data describing household electricity consumption, serves as the initial dataset for analysis.

## Project Overview

The project aims to explore and prepare the dataset, laying the groundwork for subsequent analyses. The initial focus is on understanding basic data characteristics, such as trends, seasonality, and feature correlation. The dataset consists of multivariate time series data describing various aspects of power consumption behavior observed over time.

## Tasks

### Data Exploration and Preparation

1. **Statistical Anomaly Detection**:
   - Fill missing values using linear interpolation.
   - Identify anomalies using Z-scores based on standard deviation.

2. **Correlation Analysis**:
   - Compute Pearson correlation coefficients for all pairs of features.
   - Visualize the correlation matrix with color-coding for significance.

3. **Time Windows Analysis**:
   - Determine typical power consumption patterns for day and night hours.
   - Compute average Global_intensity values for weekdays and weekends within specified time windows.
   - Perform linear and polynomial regression for each time window to illustrate Global_intensity behavior.

## Code Overview

The provided R script addresses the tasks outlined above. Below is a summary of the code implementation:

- **Question 1**: Linear interpolation for missing value imputation, followed by Z-score calculation for anomaly detection.
- **Question 2**: Computation of Pearson correlation coefficients and visualization using correlation matrix.
- **Question 3**: Analysis of representative time windows for day and night hours, calculation of average Global_intensity, and regression analysis.

## How to Run

1. Ensure R is installed on your system.
2. Install required packages (`ggplot2`, `corrplot`).
3. Place the provided dataset (`Group_Assignment_Dataset.txt`) in the same directory.
4. Execute the R script to perform data exploration and preparation tasks.
5. Review the generated visualizations and results.

## Cybersecurity Relevance

This project aligns with cybersecurity objectives by leveraging advanced analytics to detect anomalies in critical infrastructure systems. By monitoring and analyzing control signals in real-time, potential intrusions or threats can be identified early, enhancing overall cybersecurity resilience.

