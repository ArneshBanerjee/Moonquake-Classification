# Moonquake Classification Dashboard

A comprehensive Shiny dashboard for analyzing and classifying moonquake data using machine learning models.

## 🌟 Features

- **Interactive Data Visualization**
  - Time series plots of various seismic features
  - Feature analysis with scatter plots
  - Seismogram and spectrogram visualization
  - Data distribution analysis (histogram and pie chart)

- **Machine Learning Results**
  - CatBoost model performance metrics
  - Random Forest classification results
  - ANN (H2O) model analysis
  - Confusion matrices for all models
  - Feature importance visualization

- **User Interface**
  - Modern landing page with animations
  - Intuitive navigation
  - Interactive plots with zoom and hover capabilities
  - Responsive design

## 📊 Data Analysis

The dashboard provides insights into:
- Moonquake type distribution
- Feature importance for classification
- Model performance comparison
- Raw seismic data visualization

## 🛠️ Technical Stack

- **R Packages**
  - shiny
  - tidyverse
  - plotly
  - DT
  - signal
  - viridis
  - shinyjs

## 🚀 Getting Started

1. **Prerequisites**
   - R (version 4.0 or higher)
   - Required R packages (install using `install.packages()`)

2. **Installation**
   ```R
   # Install required packages
   install.packages(c("shiny", "tidyverse", "plotly", "DT", "signal", "viridis", "shinyjs"))
   ```

3. **Running the App**
   ```R
   # Clone the repository
   git clone https://github.com/ArneshBanerjee/Moonquake-Classification.git
   
   # Navigate to the project directory
   cd Moonquake-Classification
   
   # Run the Shiny app
   R -e "shiny::runApp('app.R')"
   ```

## 📈 Model Performance

### CatBoost
- Accuracy: 73.33%
- Precision (impact_mq): 85%
- Recall (impact_mq): 85%
- F1-score (impact_mq): 85%

### Random Forest
- Accuracy: 93.33%
- Precision (impact_mq): 93.33%
- Recall (impact_mq): 100%
- F1-score (impact_mq): 96.55%

### ANN (H2O)
- Accuracy: 14.29%
- Precision (impact_mq): 85.71%
- Recall (impact_mq): 100%
- F1-score (impact_mq): 92.31%

## 👥 Team Members

- Arnesh Banerjee
- Ayushi Bhattachrjee

## 📝 License

This project is licensed under the MIT License - see the LICENSE file for details.

## 🙏 Acknowledgments

- NASA Space Apps Challenge 2024
- All contributors and team members
- Open-source community for their valuable tools and libraries 
