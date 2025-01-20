# COVID-19 Impact on Digital Learning

## Overview
This repository contains an in-depth analysis of the impact of COVID-19 on digital learning in the United States. The study examines engagement data from school districts, focusing on trends, challenges, and actionable recommendations for improving digital education strategies.

## Objectives
- Analyze the disruptions caused by COVID-19 on education and the shift to digital learning platforms.
- Perform data cleaning and preprocessing on district and engagement datasets.
- Visualize key trends in engagement and connectivity across various states and demographics.
- Provide recommendations for policymakers and stakeholders to enhance digital learning outcomes.

## Dataset
The analysis uses multiple datasets:
1. **Districts Dataset**: Contains district-specific information (e.g., state, locale, demographics).
2. **Products Dataset**: Details the educational tools and platforms used during the pandemic.
3. **Engagement Dataset**: Tracks student engagement metrics such as access rates and page-load events.

### Key Features of the Data:
- **District Information**: Locale, minority representation, free/reduced lunch eligibility.
- **Engagement Metrics**: Page loads and access rates for digital platforms.
- **Product Information**: Usage of tools categorized by sector and functionality.

## Repository Structure
```
.
├── data/               # Raw and processed datasets
├── scripts/            # Scripts for data cleaning, preprocessing, and visualization
├── results/            # Visualizations and analysis results
├── README.md           # Documentation (this file)
└── Report.pdf          # Detailed report of the analysis
```

## Key Analysis Steps
1. **Data Cleaning**:
   - Corrected state names and standardized locale classifications.
   - Handled missing values using appropriate imputation techniques.
   - Simplified complex column names for better usability.

2. **Data Processing**:
   - Transformed categorical data into numeric formats for analysis.
   - Merged district and engagement datasets using unique identifiers.
   - Added derived columns (e.g., year, month) for time-series analysis.

3. **Visualization and Insights**:
   - Identified states and districts with the highest and lowest engagement.
   - Correlation analysis between engagement metrics and demographic factors.
   - Highlighted popular educational products and their usage patterns.

## Key Findings
- **Digital Divide**: Significant disparities in engagement were observed, especially in districts with higher minority populations or lower connectivity.
- **Suburban Dominance**: Suburban districts exhibited the highest engagement rates, suggesting better infrastructure and resources.
- **Product Trends**: Learning and Curriculum (LC) products were the most widely used, indicating a focus on direct educational content during the pandemic.

## Recommendations
- **Invest in Connectivity**: Enhance high-speed internet access in underserved regions to bridge the digital divide.
- **Focus on Effective Products**: Promote the adoption of proven educational tools, such as LC platforms, across districts.
- **Targeted Funding**: Allocate resources to states and districts with lower engagement metrics to improve outcomes.

## Usage
To replicate the analysis:
1. Clone this repository:
   ```bash
   git clone https://github.com/yourusername/covid19-digital-learning.git
   ```
2. Install the required dependencies:
   ```bash
   pip install -r requirements.txt
   ```
3. Run the scripts in the `scripts/` directory for data processing and visualization.

## Contributions
Contributions are welcome! If you have ideas for further analysis or improvements, feel free to open an issue or submit a pull request.

## License
This project is licensed under [MIT License](LICENSE).

## Acknowledgments
This analysis was conducted using data from Edunomics Lab, FCC, and NCES. Special thanks to all contributors and reviewers involved in the project.
