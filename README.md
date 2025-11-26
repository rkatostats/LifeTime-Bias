# LifeTime-Bias 📊

> **Interactive Survival Analysis Simulation with Bias**  
> An R Shiny application to simulate and visualize the impact of censoring and sample removal on Kaplan-Meier survival curves.

[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![R](https://img.shields.io/badge/R-%3E%3D4.0.0-276DC3?logo=r)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-Framework-blue?logo=rstudio)](https://shiny.rstudio.com/)

---

## 📋 Table of Contents

- [About the Project](#-about-the-project)
- [Features](#-features)
- [Demo](#-demo)
- [Installation](#-installation)
- [Usage](#-usage)
- [Project Structure](#-project-structure)
- [Technical Details](#-technical-details)
- [Development](#-development)
- [Roadmap](#-roadmap)
- [Contributing](#-contributing)
- [License](#-license)
- [Author](#-author)

---

## 🔬 About the Project

**LifeTime-Bias** is an application developed as a continuation of **Rodrigo Reginato Kakuta Kato**'s Bachelor's Thesis (TCC), focusing on analyzing bias in survival studies caused by censoring and sample removal.

### Objective

This tool enables researchers and students to:

- Interactively explore how different **censoring** patterns affect survival estimates
- Simulate **sample removal** by time intervals to study selection bias
- Compare treatment groups (A vs B) under different censoring conditions
- Generate reproducible R scripts for customized analyses

### Academic Context

This project extends previous research on:

- **Survival analysis**: Kaplan-Meier methods and log-rank tests
- **Censoring bias**: Impact of non-random censoring patterns on estimates
- **Statistical simulation**: Generation of synthetic data for method validation

---

## ✨ Features

### Interactive Interface

- **Interval-based Censoring Controls**: Adjust censoring percentages independently for 5 time intervals [(0,1], (1,2], (2,3], (3,4], (4,5]]
- **Sample Removal**: Simulate loss to follow-up by interval
- **Group Comparison**: Configure Group A and Group B independently
- **Real-time Visualization**: Dynamically updated Kaplan-Meier curves

### Statistical Analysis

- **Kaplan-Meier Curves**: With confidence intervals
- **Log-rank Test**: P-value for group comparison
- **Censoring Statistics**: Detailed summary of events and censoring
- **Data Table**: Complete visualization of processed data

### Export

- **R Script Download**: Complete code to reproduce the analysis
- **Plot Download**: High-resolution image (JPEG) of the survival curve
- **Customizable Names**: Define names for exported files

---

## 🎨 Demo

### Main Interface

The application features:

1. **Fixed Sidebar**: Censoring and removal controls for both groups
2. **Central Panel**: Survival plot, statistics, and data table
3. **Download Buttons**: Export scripts and plots

### Usage Example

```r
# Scenario: Cancer study with differential censoring
# Group A: 20% censoring in interval (2,3]
# Group B: 40% censoring in interval (2,3]
# Question: How does this affect group comparison?
```

---

## 🚀 Installation

### Prerequisites

- **R** (version ≥ 4.0.0)
- **RStudio** (recommended, but not required)

### Dependencies

Install the required R packages:

```r
# Install required packages
install.packages(c(
  "shiny",
  "tidyverse",
  "survminer",
  "survival",
  "DT",
  "glue"
))
```

### Clone the Repository

```bash
git clone https://github.com/rkatostats/LifeTime-Bias.git
cd LifeTime-Bias
```

### File Structure

Ensure the following files are present:

- `app.R` - Main Shiny application
- `data.csv` - Base survival data
- `fnc_gen_data.R` - Data generation functions (optional, for regeneration)

---

## 📖 Usage

### Running the Application

#### Option 1: Via RStudio

1. Open `app.R` in RStudio
2. Click the **"Run App"** button at the top of the editor

#### Option 2: Via R Command Line

```r
# In the project directory
shiny::runApp("app.R")
```

#### Option 3: Via Terminal

```bash
Rscript -e "shiny::runApp('app.R')"
```

### Analysis Workflow

1. **Configure Censoring**
   - Adjust censoring sliders for each time interval
   - Configure separately for Group A and Group B

2. **Configure Sample Removal**
   - Define removal percentages to simulate loss to follow-up
   - Observe how this impacts sample size

3. **Analyze Results**
   - Visualize updated survival curves
   - Check the log-rank test p-value
   - Examine the processed data table

4. **Export Analysis**
   - Download the generated R script for documentation
   - Save the plot for reports

---

## 📂 Project Structure

```
LifeTime-Bias/
├── app.R                 # Main Shiny application (UI + Server)
├── fnc_gen_data.R        # Functions for synthetic data generation
├── data.csv              # Base survival dataset
├── LICENSE               # Apache 2.0 License
├── README.md             # This file
└── .gitignore            # Files ignored by Git
```

### Main Files

#### `app.R`

- **UI**: Interface with sliders for censoring/removal, plot visualization
- **Server**: Reactive logic, data processing, plot generation
- **Features**:
  - Fixed and responsive sidebar
  - Efficient reactive processing
  - Download handlers for scripts and plots

#### `data.csv`

- Dataset with `Time` and `Group` variables
- 200 observations (100 per group)
- Survival times uniformly distributed in (0, 5]

#### `fnc_gen_data.R`

- Functions to generate synthetic data without duplicates
- Ensures unique times between groups
- Based on exponential distribution

---

## 🔧 Technical Details

### Processing Algorithm

1. **Group Equalization**: Group B receives sorted times from Group A
2. **Interval Categorization**: Use of `cut()` to create intervals
3. **Removal Application**: Random sampling with `set.seed(123)` for reproducibility
4. **Censoring Application**: Marking events as censored
5. **Kaplan-Meier Fitting**: `survfit(Surv(Time, Event) ~ Group)`
6. **Visualization**: `ggsurvplot()` with color and theme customization

### Used Packages

| Package      | Version   | Function                            |
|--------------|-----------|-------------------------------------|
| `shiny`      | ≥ 1.7.0   | Web application framework           |
| `tidyverse`  | ≥ 1.3.0   | Data manipulation (dplyr, ggplot2)  |
| `survival`   | ≥ 3.2.0   | Survival analysis (Kaplan-Meier, log-rank) |
| `survminer`  | ≥ 0.4.9   | Survival curve visualization        |
| `DT`         | ≥ 0.19    | Interactive tables                  |
| `glue`       | ≥ 1.6.0   | String/script generation            |

### Performance Considerations

- **Reactivity**: Use of `reactive()` to avoid unnecessary reprocessing
- **Fixed Seed**: `set.seed(123)` ensures reproducible results
- **Incremental Processing**: Removal and censoring applied sequentially

---

## 💻 Development

### Future Features Roadmap

As a continuation of the thesis, the following features are planned:

- [ ] **Multiple Scenarios**: Save and compare different censoring configurations
- [ ] **Sensitivity Analysis**: Automatically simulate multiple scenarios
- [ ] **Alternative Distributions**: Allow Weibull, log-normal, etc.
- [ ] **Cox Models**: Implement Cox regression for covariates
- [ ] **PDF Export**: Complete report with analysis and plots
- [ ] **More Intervals**: Dynamic configuration of interval numbers
- [ ] **Data Upload**: Allow upload of custom datasets
- [ ] **Additional Tests**: Wilcoxon, Fleming-Harrington, etc.

### Development Setup

```r
# Install development version
# remotes::install_github("rkatostats/LifeTime-Bias")
```

### Testing

```r
# Run tests (when implemented)
# testthat::test_dir("tests/")
```

---

## 🤝 Contributing

Contributions are welcome! This is an academic project under active development.

### How to Contribute

1. **Fork** the project
2. Create a **branch** for your feature (`git checkout -b feature/NewFeature`)
3. **Commit** your changes (`git commit -m 'Add new feature X'`)
4. **Push** to the branch (`git push origin feature/NewFeature`)
5. Open a **Pull Request**

### Guidelines

- Keep code documented (comments can be in English or Portuguese)
- Follow tidyverse style for R code
- Test your changes before submitting PR
- Update documentation as needed

---

## 📄 License

This project is licensed under the **Apache License 2.0** - see the [LICENSE](LICENSE) file for details.

### License Summary

- ✅ Commercial use
- ✅ Modification
- ✅ Distribution
- ✅ Patent use
- ❗ Must include license and copyright notice
- ❗ Changes must be documented

---

## 👨‍🎓 Author

**Rodrigo Reginato Kakuta Kato**  
Statistics/Data Science Student  
Bachelor's Thesis (TCC)

### Contact

- GitHub: [@rkatostats](https://github.com/rkatostats)
- Project: [LifeTime-Bias](https://github.com/rkatostats/LifeTime-Bias)

---

## 📚 References

### Fundamental Articles

- Kaplan, E. L., & Meier, P. (1958). Nonparametric estimation from incomplete observations. *Journal of the American Statistical Association*.
- Mantel, N. (1966). Evaluation of survival data and two new rank order statistics. *Cancer Chemotherapy Reports*.

### Additional Resources

- [Survival Analysis in R](https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html)
- [Shiny Documentation](https://shiny.rstudio.com/)
- [survminer Package](https://rpkgs.datanovia.com/survminer/)

---

<div align="center">

**Developed with** 💙 **using R and Shiny**

*If this project was useful for your research, consider giving it a ⭐ on the repository!*

</div>
