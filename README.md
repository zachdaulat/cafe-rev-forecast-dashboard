# Café Revenue Forecast Dashboard

Interactive revenue forecasting dashboard for café operations, demonstrating time series analysis and operational planning tools.

## Features

- **Real-time Prophet forecasting:** Train models dynamically based on user-selected locations
- **Interactive date selection:** Choose forecast periods from 1-30 days ahead
- **Confidence intervals:** Toggle between 80% and 95% intervals
- **Staffing recommendations:** Translate revenue predictions into operational staffing needs

## Live Dashboard

View Dashboard

## Technology Stack

- R + Shiny for interactive web application
- Facebook Prophet for time series forecasting
- bslib for modern UI components
- Deployed on Posit Connect Cloud

## Data

Synthetic café transaction data (6 months, 149K transactions) across three New York locations. Demonstrates forecasting methodology transferable to real point-of-sale systems.

## Related Work

This dashboard is part of a larger Café Transaction Analysis project covering exploratory analysis, seasonal decomposition, and product optimization.

## Local Development
```r
# Install dependencies
install.packages(c("shiny", "bslib", "tidyverse", "readxl", "prophet", "lubridate"))

# Run app
shiny::runApp()
```

## Contact

Zachary Daulat 
- [LinkedIn](https://www.linkedin.com/in/zachary-daulat/) 
- [Portfolio](https://zdaulat.quarto.pub)
```

---

### 6. Update .gitignore

**Make sure .gitignore includes:**
```
# R
.Rproj.user
.Rhistory
.RData
.Ruserdata

# Shiny
rsconnect/

# OS
.DS_Store
Thumbs.db

# Don't ignore data folder for this project
!data/
!data/cafe_txns.xlsx
