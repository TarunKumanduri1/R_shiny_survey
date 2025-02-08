# Social Vulnerability & Environmental Justice Survey App

## Overview
This Shiny application is designed to collect survey responses regarding **social vulnerability and environmental justice** in **Charleston, South Carolina**. The app integrates **interactive maps**, **survey questions**, and a **database connection to Supabase** for securely storing responses.

## Features
- 📍 **Interactive Maps** – Users can select areas of social vulnerability and environmental injustice.
- 📊 **Dynamic Survey Forms** – Includes dropdowns, checkboxes, and text input fields.
- 📡 **Database Integration** – Stores responses securely in a Supabase database using **PostgreSQL**.
- 🎨 **Custom UI Styling** – Enhanced with **CSS** for a clean and modern design.

## Installation & Setup
### Prerequisites
Ensure you have **R (4.0 or later)** and **RStudio** installed.

### Install Dependencies
To install the required R packages, run:
```r
install.packages(c("shiny", "leaflet", "sf", "dplyr", "shinyjs", "DBI", "RPostgres", "png", "uuid"))
```

### Running the App
Clone this repository and run the following command in **RStudio**:
```r
shiny::runApp("app.R")
```

## Folder Structure
```
project-root/
│── resources/               # Contains images, shapefiles, and other assets
│── app.R                    # Main Shiny app script
│── requirements.txt         # Dependencies for easy installation
│── README.md                # Documentation
```

## Database Connection
This app connects to a **Supabase PostgreSQL** database to store survey responses. Ensure you update **dbConnect()** with valid credentials in `app.R`.

## Future Enhancements
🚀 Possible improvements:
- **Real-time data visualization** for response analysis
- **Multi-region support** beyond Charleston
- **More advanced ML-based insights** on social vulnerability patterns

## License
This project is licensed under the **MIT License**.

## Contact
For questions or contributions, reach out at: **tarun.kumanduri99@gmail.com**.
