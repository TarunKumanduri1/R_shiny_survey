# Load necessary libraries
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(shinyjs)
library(DBI)
library(RPostgres)
library(png)
library(uuid)


# Define Supabase database connection settings
db <- tryCatch({
  dbConnect(
    RPostgres::Postgres(),
    host = "aws-0-us-east-1.pooler.supabase.com",
    dbname = "postgres",
    port = 6543,
    user = "postgres.slvdrlzylihfhrfgiqqo",
    password = "FEWSLab1Charleston*"
  )
}, error = function(e) {
  message("Database connection failed: ", e$message)
  NULL
})

# Check if connection is successful
if (is.null(db)) stop("Failed to connect to the database. Please check credentials or network access.")

# Debug database connection validity
if (dbIsValid(db)) {
  message("Database connection is valid.")
} else {
  stop("Database connection is invalid.")
}

css <- "
  body {
    font-family: Arial, sans-serif;
    color: #333;
    background-color: #f9f9f9;
  }
  .title-panel {
    text-align: center;
    margin-top: 20px;
    color: #1e76b6;
  }
  .panel-content {
    max-width: 800px; /* Increased panel width */
    margin: 0 auto;
    padding: 30px; /* Increased padding for a larger appearance */
    background-color: #ffffff;
    border-radius: 10px;
    box-shadow: 0px 4px 12px rgba(0, 0, 0, 0.1);
  }
  p {
    text-align: left;
    color: #333;
  }
  .btn {
    background-color: #1e76b6;
    color: white;
    font-size: 16px;
    font-weight: bold;
    margin-top: 10px;
  }
  .leaflet-container {
    height: 400px;
    border-radius: 10px;
    margin: 20px auto;
  }
  textarea {
    width: 100%;
    min-height: 100px;
    border-radius: 5px;
    padding: 10px;
  }
  .center-textarea {
    display: flex;
    justify-content: center;
    align-items: center;
    width: 100%;
  }

  /* Centering dropdowns */
  .center-dropdown {
    display: flex;
    justify-content: center;
    align-items: center;
    width: 100%;
  }
  .center-dropdown .selectize-input, .center-dropdown select {
    width: 50%; /* Adjust this width as needed */
    text-align: center; /* Ensure dropdown text is centered */
  }

  /* Center radio and checkbox groups */
  .center-options {
    display: flex;
    justify-content: center;
    align-items: center;
  }

  .center-options .radio-inline,
  .center-options .checkbox-inline {
    text-align: center;
    display: inline-block;
    margin: 5px;
  }

  /* Center alignment for Question 9 and 10 checkboxes */
  .center-checkbox-group {
    display: flex;
    flex-direction: column; /* Stack items vertically */
    justify-content: center; /* Center align group vertically */
    align-items: flex-start; /* Align checkboxes and labels horizontally */
    margin: 0 auto; /* Center group in container */
  }

  .center-checkbox-group label {
    display: flex;
    align-items: center; /* Vertically align checkbox and label */
    gap: 5px; /* Add space between checkbox and label */
  }

  /* Styling for radio buttons and checkboxes */
  .radio-inline,
  .checkbox-inline {
    margin: 10px 0; /* Add spacing between items */
  }
"

# Define UI
ui <- fluidPage(
  useShinyjs(),
  tags$style(HTML(css)),

  # Main container for centering content
  tags$div(class = "panel-content",

           conditionalPanel(
             condition = "output.page === 'welcome'",
             tags$div(
               style = "text-align: center; font-size: 1.2em;",

               # Welcome Heading
               tags$h3(tags$b(tags$span(style = "color: #1e76b6;", "Welcome!"))),

               # Welcome Message
               tags$p("Thank you for taking part in this survey. Your responses will remain confidential and aid in ongoing research on understanding social vulnerability and environmental injustice in Charleston, South Carolina."),

               tags$br(),

               # Eligibility Confirmation Heading
               tags$h4(tags$i(style = "color: #1e76b6;", "Eligibility Confirmation")),

               # Eligibility Confirmation Message
               tags$p("As a participant in this survey, you confirm that you are 18 years of age or older and currently live or work in the Charleston, South Carolina, area. If you do not meet these criteria, please exit the survey now."),

               tags$br(),

               # Button
               div(style = "text-align: center;", actionButton("next0", "Proceed to the Survey", class = "btn"))
             )
           ),

           # Page 1 - Social Vulnerability Map
           conditionalPanel(
             condition = "output.page === 'page1'",
             h3(HTML("<div style='text-align: center;'><span style='color: #1e76b6; font-weight: bold;'>Question 1:</span> <span style='font-size: 1.1em;'><b>Social Vulnerability</b> is how likely people are to face greater harm or impact on their lives after disruptive hazard events like flooding based on social or economic conditions like income or transportation access.</span></div>")),
             p(style = "font-size: 1.2em;", "Please review the map of the City of Charleston, South Carolina, and select the areas you believe are more socially vulnerable to the impacts of hazardous events."),
             leafletOutput("SVIMAP"),
             div(style = "text-align: center;", actionButton("next1", "Next Question", class = "btn"))
           ),

           # Page 2 - Social Vulnerability Map Comparison
           conditionalPanel(
             condition = "output.page === 'page2'",
             h3(HTML("<div style='text-align: center;'><span style='color: #1e76b6; font-weight: bold;'>Question 2:</span> <span style='font-size: 1.1em;'>From the provided definition of <b>social vulnerability</b>, please review the two maps below and select which map best illustrates the spatial patterns of social vulnerability.</span></div>")),
             fluidRow(
               column(6, img(src = "CDC_SVI_withBasemap.jpg", width = "100%"), div(style = "text-align: center;",
                                                                                     radioButtons("map_select", NULL, choices = list(" " = "cdcsvi"), inline = TRUE))),
               column(6, img(src = "HVRI_SoVI_Basemap.jpg", width = "100%"), div(style = "text-align: center;",
                                                                                       radioButtons("map_select", NULL, choices = list(" " = "hvrisovi"), inline = TRUE)))
             ),
             div(style = "text-align: center;", actionButton("next2", "Next Question", class = "btn"))
           ),

           # Page 3 - Environmental Justice Map
           conditionalPanel(
             condition = "output.page === 'page3'",
             h3(HTML("<div style='text-align: center;'><span style='color: #1e76b6; font-weight: bold;'>Question 3:</span> <span style='font-size: 1.1em;'><b>Environmental justice</b> is the fair treatment of all people, regardless of race, ethnicity, or income. Communities facing environmental justice concerns refer to those lacking environmental benefits, such as clean water or air. It can also refer to communities with unequal impacts of environmental burdens, such as living near landfills or toxic waste facilities.</span></div>")),
             p(style = "font-size: 1.2em;", "Please review the map of the City of Charleston, South Carolina, and select areas you believe are experiencing environmental injustice."),
             leafletOutput("EJMAP"),
             div(style = "text-align: center;", actionButton("next3", "Next Question", class = "btn"))
           ),

           # Page 4 - Environmental Justice Map Ranking
           conditionalPanel(
             condition = "output.page === 'page4'",
             h3(HTML("<div style='text-align: center;'><span style='color: #1e76b6; font-weight: bold;'>Question 4:</span> <span style='font-size: 1.1em;'>From your understanding of communities facing <b>environmental injustice</b>, please review the maps below and rank them from 1 (most accurate) to 4 (least accurate) based on how well they represent the spatial patterns of environmental injustice.</span></div>")),
             fluidRow(
               column(6, img(src = "CEJST_Basemap.jpg", width = "80%", height = "300px"),
                      sliderInput("rank_map1", "Rank:", min = 1, max = 4, value = 1, width = "80%")),
               column(6, img(src = "CDC_EJIndex_Basemap.jpg", width = "80%", height = "300px"),
                      sliderInput("rank_map2", "Rank:", min = 1, max = 4, value = 1, width = "80%"))
             ),
             fluidRow(
               column(6, img(src = "EPA_IRA_Basemap.jpg", width = "80%", height = "300px"),
                      sliderInput("rank_map3", "Rank:", min = 1, max = 4, value = 1, width = "80%")),
               column(6, img(src = "EJScreen_Basemap.jpg", width = "80%", height = "300px"),
                      sliderInput("rank_map4", "Rank:", min = 1, max = 4, value = 1, width = "80%"))
             ),
             div(style = "text-align: center;", actionButton("next4", "Next Question", class = "btn"))
           ),

           # Page 5 - Residency Duration
           conditionalPanel(
             condition = "output.page === 'page5'",
             h3(HTML("<div style='text-align: center;'><span style='color: #1e76b6; font-weight: bold;'>Question 5:</span> <span style='font-size: 1.1em;'>How long have you lived in Charleston, SC?</span></div>")),
             div(style = "display: flex; justify-content: center; align-items: center;",
                 selectInput("lengthSCResident", NULL, choices = c(
                   "Less than a year" = "less_than_a_year",
                   "1-5 years" = "1-5_years",
                   "5-10 years" = "5-10_years",
                   "10-15 years" = "10-15_years",
                   "15-20 years" = "15-20_years",
                   "20-25 years" = "20_25_years",
                   "25 years or more" = "25+_years"
                 ), width = "50%")
             ),
             div(style = "text-align: center;", actionButton("next5", "Next Question", class = "btn"))
           ),

           # Page 6 - Racial Background
           conditionalPanel(
             condition = "output.page === 'page6'",
             h3(HTML("<div style='text-align: center;'><span style='color: #1e76b6; font-weight: bold;'>Question 6:</span> <span style='font-size: 1.1em;'>What best describes your racial or ethnic background?</span></div>")),
             div(style = "display: flex; justify-content: center; align-items: center; width: 100%;",
                 checkboxGroupInput("race", NULL, choices = c(
                   "American Indian or Alaskan Native" = "aian",
                   "Asian" = "asian",
                   "Black or African American" = "black_or_aa",
                   "Native Hawaiian or Other Pacific Islander" = "nh_pi",
                   "Hispanic or Latino" = "hisp_latino",
                   "White" = "white_caucasian",
                   "Other" = "other",
                   "Prefer not to answer" = "prefer_not_answer"
                 ))
             ),
             conditionalPanel(
               condition = "input.race.indexOf('other') >= 0",
               div(style = "display: flex; justify-content: center; align-items: center;",
                   textInput("other_race_specify", "Please specify your racial or ethnic background:", width = "50%")
               )
             ),
             div(style = "text-align: center;", actionButton("next6", "Next Question", class = "btn"))
           ),

           conditionalPanel(
             condition = "output.page === 'page7'",
             h3(HTML("<div style='text-align: center;'><span style='color: #1e76b6; font-weight: bold;'>Question 7:</span> <span style='font-size: 1.1em;'>How long have you worked in the City of Charleston?</span></div>")),
             div(
               style = "display: flex; justify-content: center; align-items: center;",
               radioButtons(
                 inputId = "charleston_experience",
                 label = NULL,
                 choices = list(
                   "Less than a year" = "less_than_year",
                   "1-5 years" = "1_5_years",
                   "5-10 years" = "5_10_years",
                   "10-15 years" = "10_15_years",
                   "15-20 years" = "15_20_years",
                   "20-25 years" = "20_25_years",
                   "25 or more years" = "25_or_more_years"
                 )
               )
             ),
             div(style = "text-align: center;", actionButton("next7", "Next Question", class = "btn"))
           ),

           # Page 10 - Professional Role
           conditionalPanel(
             condition = "output.page === 'page8'",
             h3(HTML("<div style='text-align: center;'><span style='color: #1e76b6; font-weight: bold;'>Question 8:</span> <span style='font-size: 1.1em;'>Which of the following best describes your professional role or field?</span></div>")),
             div(style = "display: flex; justify-content: center; align-items: center;",
                 checkboxGroupInput("professional_role", NULL, choices = c(
                   "City Government Employee" = "city_government",
                   "Researcher" = "researcher",
                   "GIS Analyst" = "gis_analyst",
                   "Emergency Manager" = "emergency_manager",
                   "Floodplain Manager" = "floodplain_manager",
                   "Engineer (please specify)" = "engineer",
                   "Urban Planner" = "urban_planner",
                   "Hydrologist" = "hydrologist",
                   "Climate Resilience Specialist" = "climate_resilience",
                   "Environmental Consultant" = "environmental_consultant",
                   "Public Health Official" = "public_health",
                   "Nonprofit" = "nonprofit",
                   "Other (please specify)" = "other"
                 ))
             ),
             conditionalPanel(
               condition = "input.professional_role.indexOf('engineer') >= 0",
               div(style = "display: flex; justify-content: center; align-items: center;",
                   textInput("engineer_specify", "Please specify your engineering discipline:", width = "50%")
               )
             ),
             conditionalPanel(
               condition = "input.professional_role.indexOf('other') >= 0",
               div(style = "display: flex; justify-content: center; align-items: center;",
                   textInput("other_specify", "Please specify your role:", width = "50%")
               )
             ),
             div(style = "text-align: center;", actionButton("next8", "Next Question", class = "btn"))
           ),
           conditionalPanel(
             condition = "output.page === 'page9'",

             # Thank You and Comments Section
             div(
               style = "text-align: center; margin-top: 20px;",
               tags$h4(tags$b("Thank you for your participation!")),
               tags$p("Please share any comments or feedback in the space provided below. Click 'Submit Survey' to complete the survey."),
               textAreaInput("comments", label = NULL, width = "100%", height = "100px", placeholder = "Enter your feedback here..."),
               tags$br()
             ),

             # Submit Button for Completing the Survey
             div(style = "text-align: center;", actionButton("submit_survey", "Submit Survey", class = "btn"))
           )
  )
)

# Define Server
server <- function(input, output, session) {
  current_page <- reactiveVal("welcome")

  # Helper to navigate pages
  navigate_to <- function(page) { current_page(page) }

  # Update the reactive output to control the displayed page
  output$page <- reactive(current_page())
  outputOptions(output, "page", suspendWhenHidden = FALSE)

  # Page navigation events
  observeEvent(input$next0, {navigate_to("page1")})
  observeEvent(input$next1, { navigate_to("page2") })
  observeEvent(input$next2, { navigate_to("page3") })
  observeEvent(input$next3, { navigate_to("page4") })
  observeEvent(input$next4, { navigate_to("page5") })
  observeEvent(input$next5, { navigate_to("page6") })
  observeEvent(input$next6, { navigate_to("page7") })
  observeEvent(input$next7, { navigate_to("page8") })
  observeEvent(input$next8, { navigate_to("page9") })
  observeEvent(input$submit_survey, {
    showNotification("Survey completed. Thanks for participating!", type = "message")
  })

  # Load the shapefile
  shapefile <- tryCatch({
    st_read("resources/shapefile/Sf_CityofCharleston.shp")
  }, error = function(e) {
    stop("Error loading shapefile: ", e$message)
  })

  if (!"GEOID" %in% colnames(shapefile)) stop("Column 'GEOID' not found in shapefile.")

  # Social Vulnerability Map with Click Highlight
  selected_tracts_SVI <- reactiveVal(c())
  output$SVIMAP <- renderLeaflet({
    leaflet(data = shapefile) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        color = "#444444",
        weight = 1,
        fillOpacity = 0.3,
        layerId = ~GEOID
      )
  })

  observeEvent(input$SVIMAP_shape_click, {
    click <- input$SVIMAP_shape_click
    if (!is.null(click$id)) {
      selected_tract <- click$id
      current_selection <- selected_tracts_SVI()
      current_selection <- if (selected_tract %in% current_selection) setdiff(current_selection, selected_tract) else c(current_selection, selected_tract)
      selected_tracts_SVI(current_selection)

      # Update map colors to indicate selection
      leafletProxy("SVIMAP") %>%
        clearShapes() %>%
        addPolygons(data = shapefile, color = "#444444", weight = 1, fillOpacity = 0.3, layerId = ~GEOID) %>%
        addPolygons(data = shapefile[shapefile$GEOID %in% selected_tracts_SVI(), ],
                    color = "yellow", weight = 2, fillOpacity = 0.5, layerId = ~GEOID)
    }
  })

  # Environmental Justice Map with Click Highlight
  selected_tracts_EJ <- reactiveVal(c())
  output$EJMAP <- renderLeaflet({
    leaflet(data = shapefile) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        color = "#444444",
        weight = 1,
        fillOpacity = 0.3,
        layerId = ~GEOID
      )
  })

  observeEvent(input$EJMAP_shape_click, {
    click <- input$EJMAP_shape_click
    if (!is.null(click$id)) { # Check if a shape was clicked
      selected_tract <- click$id
      current_selection <- selected_tracts_EJ()

      # Toggle the selection of the clicked tract
      if (selected_tract %in% current_selection) {
        current_selection <- setdiff(current_selection, selected_tract) # Remove if already selected
      } else {
        current_selection <- c(current_selection, selected_tract) # Add if not selected
      }
      selected_tracts_EJ(current_selection) # Update reactive value

      # Update map with selected tracts highlighted
      leafletProxy("EJMAP") %>%
        clearShapes() %>%
        addPolygons(
          data = shapefile,
          color = "#444444",
          weight = 1,
          fillOpacity = 0.3,
          layerId = ~GEOID
        ) %>%
        addPolygons(
          data = shapefile[shapefile$GEOID %in% selected_tracts_EJ(), ],
          color = "yellow",
          weight = 2,
          fillOpacity = 0.5,
          layerId = ~GEOID
        )
    }
  })

  observeEvent(input$submit_survey, {
    responses1 <- data.frame(
      timestamp = Sys.time(),
      selected_SVI_tracts = ifelse(length(selected_tracts_SVI()) > 0, paste(selected_tracts_SVI(), collapse = ","), NA),
      selected_EJ_tracts = ifelse(length(selected_tracts_EJ()) > 0, paste(selected_tracts_EJ(), collapse = ","), NA),
      map_select = ifelse(!is.null(input$map_select), input$map_select, NA),
      CDC_EJI_rank_map1 = ifelse(!is.null(input$rank_map1), input$rank_map1, NA),
      CEJST_rank_map2 = ifelse(!is.null(input$rank_map2), input$rank_map2, NA),
      EJSCREEN_rank_map3 = ifelse(!is.null(input$rank_map3), input$rank_map3, NA),
      EPAIRADL_rank_map4 = ifelse(!is.null(input$rank_map4), input$rank_map4, NA),
      lengthSCResident = ifelse(!is.null(input$lengthSCResident), input$lengthSCResident, NA),
      race = ifelse(length(input$race) > 0, paste(input$race, collapse = ","), NA),
      race_other = ifelse(!is.null(input$other_race_specify) && input$other_race_specify != "", input$other_race_specify, NA),
      charleston_experience = ifelse(!is.null(input$charleston_experience), input$charleston_experience, NA),
      professional_role = ifelse(length(input$professional_role) > 0, paste(input$professional_role, collapse = ","), NA),
      engineer_specify = ifelse(!is.null(input$engineer_specify) && input$engineer_specify != "", input$engineer_specify, NA),
      other_profession_specify = ifelse(!is.null(input$other_specify) && input$other_specify != "", input$other_specify, NA),
      comments = ifelse(!is.null(input$comments) && input$comments != "", input$comments, NA)
    )

    # Debugging: Print the data frame
    message("Prepared responses1 data frame:")
    print(responses1)

    # Attempt to write to the database
    tryCatch({
      dbWriteTable(db, "responses1", responses1, append = TRUE, row.names = FALSE)
      showNotification("Thank you for completing the survey! Your responses have been saved.", type = "message")
      message("Data successfully written to database.")
    }, error = function(e) {
      showNotification("Error submitting survey. Please try again later.", type = "error")
      message("Database write error: ", e$message)
    }, warning = function(w) {
      showNotification("Warning while submitting survey. Check logs.", type = "warning")
      message("Database write warning: ", w$message)
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
