library(shiny)
source("classes.R")
source("functions.R")
library("ggplot2")
library("tidyr")
library("dplyr")
library("leaflet")
library("jsonlite")
library("sf")




theme_dark_finance <- function() {
  theme_minimal(base_family = "Manrope") +
    theme(
      plot.background = element_rect(fill = "#0f1116", color = NA),
      panel.background = element_rect(fill = "#0f1116"),
      panel.grid = element_blank(),
      axis.text = element_text(color = "#C5C6C7"),
      axis.title = element_text(color = "#66FCF1"),
      plot.title = element_text(color = "#66FCF1", face = "bold"),
      legend.background = element_rect(fill = "#1F2833"),
      legend.text = element_text(color = "#C5C6C7")
    )
}


# UI
ui <- fluidPage(
  titlePanel("Personal Finance Simulator"),
  
  tags$head(
    tags$style(HTML("
    body {
      background-color: #0f1116;
      color: #FFFFFF;
      font-family: 'Manrope', sans-serif;
    }
    
    .well {
      background-color: #1F2833 !important;
      border: none;
      border-radius: 12px;
      box-shadow: none;
    }

    .btn, .btn-primary {
      background-color: #45A29E;
      border: none;
      color: white;
      border-radius: 20px;
      padding: 10px 20px;
      font-weight: bold;
    }

    .btn-warning {
      background-color: #C5C6C7;
      color: black;
    }

    .nav-tabs > li > a {
      background-color: #1F2833 !important;
      color: #66FCF1 !important;
      border-radius: 10px 10px 0 0;
    }

    .nav-tabs > li.active > a {
      background-color: #0f1116 !important;
      color: white !important;
      font-weight: bold;
    }

    h4, h3, label {
      color: #66FCF1;
    }

    input, select, .form-control {
      background-color: #1F2833;
      color: #C5C6C7;
      border: 1px solid #45A29E;
      border-radius: 8px;
    }

    .table {
      color: #C5C6C7;
    }
    
    #summary_text, #abroad_result {
      background-color: #1F2833;
      padding: 15px;
      border-radius: 12px;
      border-left: 4px solid #66FCF1;
      font-size: 16px;
      color: #C5C6C7;
      font-family: 'Manrope', sans-serif;
      white-space: pre-line;
    }

    .table {
      background-color: #1F2833;
      color: #C5C6C7;
      border-collapse: collapse;
      width: 100%;
      font-family: 'Manrope', sans-serif;
    }
    
    .table th {
      background-color: #0f1116;
      color: #66FCF1;
      font-weight: 600;
      padding: 10px;
      border-bottom: 1px solid #45A29E;
    }
    
    .table td {
      padding: 10px;
      border-bottom: 1px solid #2c3e50;
    }
    
    .table-striped tbody tr:nth-of-type(odd) {
      background-color: #19232d;
    }

    h4, h3 {
      margin-top: 20px;
      margin-bottom: 10px;
    }

  "))
  ),
  
  
  # Reset button
  fluidRow(
    column(12, align = "right",
           actionButton("reset_button", "Reset Simulator", class = "btn btn-warning")
    )
  ),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      h4("User Information"),
      numericInput("age", "Current Age", value = 25, min = 18, max = 80),
      numericInput("retire_age", "Desired Retirement Age", value = 65, min = 30, max = 100),
      numericInput("life_expectancy", "How long you want to live?", value = 85, min = 50, max = 120),
      
      h4("Financial Parameters"),
      numericInput("income", "Monthly Income (PLN)", value = 7500),
      numericInput("income_growth", "Annual Income Growth Rate (%)", value = 3),
      numericInput("expenses", "Fixed Monthly Expenses (PLN)", value = 3000),
      
      
      h4("Investment & Inflation"),
      sliderInput("saving_rate", "Savings Rate (%)", min = 0, max = 100, value = 20),
      selectInput("strategy", "Investment Strategy (Bank/Bonds/Stocks)",
                  choices = list(
                    "Keep all in bank (100/0/0)" = "bank",
                    "Some bonds (60/40/0)" = "some_bonds",
                    "Investor beginner (10/60/30)" = "beginner",
                    "Finance bro (0/35/65)" = "finance_bro",
                    "Wolf of Wall Street (0/0/100)" = "wolf"
                  )),
      numericInput("inflation", "Annual Inflation Rate (%)", value = 3),
      
      h4("Debt"),
      checkboxInput("has_loan", "Simulate a Loan?", value = FALSE),
      conditionalPanel(
        condition = "input.has_loan == true",
        numericInput("loan_amount", "Initial Loan Amount", value = 300000),
        numericInput("loan_rate", "Annual Interest Rate (%)", value = 4),
        numericInput("loan_term", "Loan Term (Years)", value = 15)
      ),
      
      actionButton("simulate", "Run Simulation", 
                   class = "btn-primary",
                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
    
    # Main panel
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 conditionalPanel(
                   condition = "output.showResults",
                   verbatimTextOutput("summary_text"),
                   tableOutput("summary_table")
                 )
        ),
        tabPanel("Projections",
                 conditionalPanel(
                   condition = "output.showResults",
                   plotOutput("cashflow_plot"),
                   plotOutput("networth_plot"),
                   plotOutput("investment_split_plot")
                 )
        ),
        tabPanel("Debt Simulator",
                 conditionalPanel(
                   condition = "input.has_loan == true",
                   plotOutput("debt_plot"),
                   tableOutput("debt_schedule")
                 )
        ),
        tabPanel("Retirement Abroad",
                 h3("How long could you live abroad with your retirement savings?"),
                 
                 selectInput("retirement_destination", 
                             "Choose your dream retirement destination:",
                             choices = c(
                               "Thailand (3,000 PLN/month)" = 3000,
                               "Portugal (4,000 PLN/month)" = 4000,
                               "Spain (4,500 PLN/month)" = 5000,
                               "Mexico (6,000 PLN/month)" = 6000,
                               "Japan (8,000 PLN/month)" = 8000,
                               "Switzerland (12,000 PLN/month)" = 12000,
                               "Your OWN place" = "custom"
                             ),
                             selected = 3000),
                 
                 conditionalPanel(
                   condition = "input.retirement_destination == 'custom'",
                   numericInput("custom_monthly_cost", 
                                "Enter monthly living cost (PLN):",
                                value = 3000, min = 1)
                 ),
                 
                 actionButton("calculate_abroad", "Calculate"),
                 
                 conditionalPanel(
                   condition = "output.showResults",
                   verbatimTextOutput("abroad_result"),
                   uiOutput("abroad_life_check"),
                   uiOutput("retirement_card"),
                   plotOutput("retirement_cost_plot"),
                   leafletOutput("retirement_leaflet_map", height = "300px")
                   
                 )
        )
      )
    )
  )
)


# Server
server <- function(input, output, session) {
  state <- reactiveValues(show_results = FALSE)
  state <- reactiveValues(show_results = FALSE)
  
  investment_allocation <- reactive({
    switch(input$strategy,
           "bank" = c(bank = 1, bonds = 0, stocks = 0),
           "some_bonds" = c(bank = 0.6, bonds = 0.4, stocks = 0),
           "beginner" = c(bank = 0.1, bonds = 0.6, stocks = 0.3),
           "finance_bro" = c(bank = 0, bonds = 0.35, stocks = 0.65),
           "wolf" = c(bank = 0, bonds = 0, stocks = 1)
    )
  })
  
  simulation <- eventReactive(input$simulate, {
    if (input$has_loan) {
      loan_obj <- new("Loan", amount = input$loan_amount,
                      rate = input$loan_rate,
                      term = input$loan_term)
    } else {
      loan_obj <- new("Loan", amount = 0, rate = 0, term = 0)
    }
    
    user <- new("UserProfile",
                age = input$age,
                retire_age = input$retire_age,
                life_expectancy = input$life_expectancy,
                income = input$income,
                expenses = input$expenses,
                income_growth = input$income_growth / 100,
                inflation = input$inflation,
                saving_rate = input$saving_rate,
                has_loan = input$has_loan,
                loan = loan_obj)
    
    observe({
      if (input$retire_age <= input$age) {
        showNotification("Retirement age must be greater than current age!", type = "error")
        updateNumericInput(session, "retire_age", value = input$age + 1)
      }
      if (input$life_expectancy <= input$retire_age) {
        showNotification("Life expectancy must be greater than retirement age!", type = "error")
        updateNumericInput(session, "life_expectancy", value = input$retire_age + 1)
      }
    })
    
    alloc <- investment_allocation()
    
    user <- new("UserProfile",
                age = input$age,
                retire_age = input$retire_age,
                life_expectancy = input$life_expectancy,
                income = input$income,
                expenses = input$expenses,
                income_growth = input$income_growth / 100,
                saving_rate = input$saving_rate,
                inflation = input$inflation / 100,
                has_loan = input$has_loan,
                loan = if (input$has_loan) new("Loan",
                                               amount = input$loan_amount,
                                               rate = input$loan_rate / 100,
                                               term = input$loan_term)
                else new("Loan", amount = 0, rate = 0, term = 0)
    )
    
    simulate_finances(user, alloc_bank = alloc["bank"], alloc_bonds = alloc["bonds"], alloc_stocks = alloc["stocks"])
    
  })
  
  yearly_summary <- reactive({
    req(input$simulate > 0)
    
    df <- simulation()@data
    if (nrow(df) == 0) return(data.frame())
    
    df$YearRounded <- floor(df$Year)
    
    # End-of-year values 
    yearly_data <- df[!duplicated(df$YearRounded, fromLast = TRUE), ]
    

    yearly_data$NetWorth <- yearly_data$Bank + yearly_data$Bonds + yearly_data$Stocks - yearly_data$Debt
    yearly_data$TotalInvestment <- yearly_data$Bank + yearly_data$Bonds + yearly_data$Stocks
    
    yearly_data <- yearly_data[, c("YearRounded", "Income", "Expenses", 
                                   "Bank", "Bonds", "Stocks", "TotalInvestment", 
                                   "Debt", "NetWorth")]
    yearly_data
  })
  

  # Retirement abroad calculator
  observeEvent(input$calculate_abroad, {
    state$show_results <- TRUE
    df <- simulation()@data
    retirement_row <- df[df$Year >= input$retire_age, ]
    
    if (nrow(retirement_row) == 0) {
      showNotification("Run the simulation first to see retirement savings!", type = "error")
      return()
    }
    
    savings <- round(as.numeric(retirement_row[1, "NetWorth"]), 2)
    
    # Monthly cost of fancy retirement
    if (input$retirement_destination == "custom") {
      monthly_cost <- input$custom_monthly_cost
      location <- "your chosen destination"
    } else {
      monthly_cost <- as.numeric(input$retirement_destination)
      location <- strsplit(names(which(c(
        "Thailand (3,000 PLN/month)" = 3000,
        "Portugal (4,000 PLN/month)" = 4000,
        "Spain (4,500 PLN/month)" = 5000,
        "Mexico (6,000 PLN/month)" = 6000,
        "Japan (8,000 PLN/month)" = 8000,
        "Switzerland (12,000 PLN/month)" = 12000
      ) == input$retirement_destination)), " ")[[1]][1]
    }
    
    # Calculating duration
    total_months <- floor(savings / monthly_cost)
    years <- floor(total_months / 12)
    months <- total_months %% 12
    
    # Result text
    output$abroad_result <- renderText({
      paste0(
        "At retirement (age ", input$retire_age, "), you'll have ", 
        format(savings, big.mark = ","), " PLN savings\n",
        "In ", location, " (", monthly_cost, " PLN/month), you could comfortably live for:\n",
        years, " years and ", months, " months"
      )
    })
    
    output$abroad_life_check <- renderUI({
      req(input$calculate_abroad)  
      
      retire_age <- input$retire_age
      desired_age <- input$life_expectancy
      
      # Sanity check :D
      if (is.null(retire_age) || is.null(desired_age) || retire_age >= desired_age) return(NULL)
      
      years_needed <- desired_age - retire_age
      
      cost <- if (input$retirement_destination == "custom") {
        input$custom_monthly_cost
      } else {
        as.numeric(input$retirement_destination)
      }
      
      
      if (is.null(cost) || is.na(cost) || cost <= 0) return(NULL)
      
      df <- simulation()@data
      retirement_year <- df[df$Year >= retire_age, ][1, ]
      
      savings <- as.numeric(retirement_year$NetWorth)
      if (is.na(savings)) return(NULL)
      
      years_affordable <- savings / (12 * cost)
      
      if (years_affordable >= years_needed) {
        div(
          style = "border: 2px solid green; padding: 10px; border-radius: 5px; background-color: #e0fce0; color: darkgreen;",
          strong("✅ You could afford living there till the end of your life.")
        )
      } else {
        div(
          style = "border: 2px solid red; padding: 10px; border-radius: 5px; background-color: #fde0e0; color: darkred;",
          strong("❌ You couldn't afford living there till the end of your life.")
        )
      }
    })
    
    output$showResults <- reactive({
      state$show_results
    })
    outputOptions(output, "showResults", suspendWhenHidden = FALSE)
    observeEvent(input$simulate, {
      state$show_results <- TRUE
    })
    
    output$retirement_card <- renderUI({
      req(input$retirement_destination != "custom")
      
      destination_info <- list(
        "3000" = list(flag = "🇹🇭", country = "Thailand", quote = "Simplify your life, amplify your peace."),
        "4000" = list(flag = "🇵🇹", country = "Portugal", quote = "Where time flows as slowly as the Douro."),
        "5000" = list(flag = "🇪🇸", country = "Spain", quote = "Sunshine, siestas, and savings."),
        "6000" = list(flag = "🇲🇽", country = "Mexico", quote = "Your pesos stretch further under palm trees."),
        "8000" = list(flag = "🇯🇵", country = "Japan", quote = "Retire in harmony and honor."),
        "12000" = list(flag = "🇨🇭", country = "Switzerland", quote = "Luxury meets longevity.")
      )
      
      info <- destination_info[[as.character(input$retirement_destination)]]
      if (is.null(info)) return(NULL)
      
      tags$div(
        style = "margin-top: 20px; padding: 20px; border-radius: 12px; background-color: #1F2833; box-shadow: 0 4px 12px rgba(0,0,0,0.4); text-align: center;",
        tags$h2(style = "font-size: 2em; color: #66FCF1;", paste(info$flag, info$country)),
        tags$p(style = "font-style: italic; color: #C5C6C7; font-size: 16px;", paste0("\"", info$quote, "\""))
      )
    })
    
    # plot with costs per country
    output$retirement_cost_plot <- renderPlot({
      cost_data <- data.frame(
        Country = c("Thailand", "Portugal", "Spain", "Mexico", "Japan", "Switzerland"),
        MonthlyCost = c(3000, 4000, 5000, 6000, 8000, 12000)
      )
      
      ggplot(cost_data, aes(x = reorder(Country, MonthlyCost), y = MonthlyCost, fill = Country)) +
        geom_col(show.legend = FALSE, width = 0.6) +
        coord_flip() +
        scale_fill_manual(values = c(
          "Thailand"    = "#0051BA",  
          "Portugal"    = "#006600", 
          "Spain"       = "#FFC400",  
          "Mexico"      = "#006847",  
          "Japan"       = "#FFFFFF",  
          "Switzerland" = "#D52B1E"   
        )) +
        labs(
          title = "Monthly Retirement Cost by Country",
          x = NULL,
          y = "Cost (PLN)"
        ) +
        theme_dark_finance() +
        theme(axis.text.y = element_text(face = "bold"))
    })
    
    # map with selected country
    output$retirement_leaflet_map <- renderLeaflet({
      req(input$retirement_destination != "custom")
      
      country_codes <- list(
        "3000" = "TH",
        "4000" = "PT",
        "5000" = "ES",
        "6000" = "MX",
        "8000" = "JP",
        "12000" = "CH"
      )
      
      selected_code <- country_codes[[as.character(input$retirement_destination)]]
      if (is.null(selected_code)) return(NULL)
      
      geojson_path <- "www/retirement_destinations.geojson"
      countries <- sf::st_read(geojson_path, quiet = TRUE)
      selected_country <- countries[countries$code == selected_code, ]
      
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles("CartoDB.DarkMatter") %>%
        addPolygons(
          data = countries,
          fillColor = "#1a1a1a",
          fillOpacity = 0.3,
          color = "#444", weight = 1,
          label = ~name
        ) %>%
        addPolygons(
          data = selected_country,
          fillColor = "#00fff7",    
          fillOpacity = 0.95,       
          color = "#ffffff",      
          weight = 6,               
          opacity = 1,
          label = ~name,
          labelOptions = labelOptions(
            style = list(
              "color" = "#111",
              "font-weight" = "bold",
              "font-size" = "18px",
              "background-color" = "#fff"
            )
          )
        ) %>%
        fitBounds(lng1 = -180, lat1 = -60, lng2 = 180, lat2 = 85)
    })
    
  })
  
  
  # Summary Text Output
  output$summary_text <- renderPrint({
    df <- simulation()@data
    retirement_row <- df[df$Year >= input$retire_age, ]
    
    if (nrow(retirement_row) == 0) {
      retirement_savings <- 0
      retirement_year <- input$retire_age
    } else {
      retirement_savings <- round(as.numeric(retirement_row[1, "NetWorth"]), 0)
      retirement_year <- round(retirement_row[1, "Year"], 0)
    }
    
    cat(
      "At the time of retirement you will have",
      format(retirement_savings, big.mark = ",", scientific = FALSE),
      "PLN savings!"
    )
  })
  
  
  
  
  # Summary Table Output
  output$summary_table <- renderTable({
    ysum <- yearly_summary()
    if (nrow(ysum) == 0) return()
    
    retire_row <- which(ysum$YearRounded == input$retire_age)
    if (length(retire_row) == 0) retire_row <- which.min(abs(ysum$YearRounded - input$retire_age))
    
    idxs <- retire_row:(retire_row + 4)
    idxs <- idxs[idxs <= nrow(ysum)] 
    table_subset <- ysum[idxs, ]
    
    num_cols <- sapply(table_subset, is.numeric)
    table_subset[num_cols] <- lapply(table_subset[num_cols], function(x) format(round(x, 2), big.mark = ",", scientific = FALSE))
    
    table_subset <- table_subset %>%
      mutate(across(where(is.numeric), ~ format(round(.x, 2), big.mark = ",", scientific = FALSE)))
    table_subset
  })
  
  
  
  
  # Net Worth Plot
  output$networth_plot <- renderPlot({
    df <- yearly_summary()
    if (nrow(df) == 0) return()
    
    ggplot(df, aes(x = YearRounded, y = NetWorth)) +
      geom_line(color = "#FFD600", size = 1.5) +
      geom_vline(xintercept = input$retire_age, linetype = "dashed", color = "white", linewidth = 1.2) +
      annotate("text", 
               x = input$retire_age, 
               y = max(df$NetWorth) * 0.7,
               label = paste0("Retirement (", input$retire_age, ")"),
               hjust = -0.05,
               color = "#FFFFFF",
               size = 4,
               fontface = "bold") +
      labs(
        title = "Projected Savings (Net Worth)",
        x = "Year",
        y = "Net Worth (PLN)"
      ) +
      theme_dark_finance()
  })
  
  
  
  # Cashflow Plot
  output$cashflow_plot <- renderPlot({
    df <- yearly_summary()
    df <- df[df$YearRounded < input$life_expectancy, ]
    if (!all(c("Income", "Expenses") %in% names(df))) return()
    
    df_long <- df %>%
      pivot_longer(cols = c("Income", "Expenses"), names_to = "Type", values_to = "Value")
    
    ggplot(df_long, aes(x = YearRounded, y = Value, color = Type)) +
      geom_line(size = 1.3) +
      scale_color_manual(values = c("Income" = "#39FF14", "Expenses" = "#FF073A")) +
      geom_vline(xintercept = input$retire_age, linetype = "dashed", color = "white", linewidth = 1.2) +
      annotate("text",
               x = input$retire_age,
               y = max(df_long$Value, na.rm = TRUE) * 0.7,
               label = paste0("Retirement (", input$retire_age, ")"),
               hjust = -0.05,
               color = "#FFFFFF",
               size = 4,
               fontface = "bold") +
      labs(
        title = "Income vs. Expenses Over Time",
        x = "Year",
        y = "PLN",
        color = NULL
      ) +
      theme_dark_finance()
  })
  
  
  # Invetment Plot
  output$investment_split_plot <- renderPlot({
    df <- yearly_summary()
    df <- df[df$YearRounded < input$life_expectancy, ]
    if (!all(c("Bank", "Bonds", "Stocks") %in% names(df))) return()
    
    df_long <- df %>%
      pivot_longer(cols = c("Bank", "Bonds", "Stocks"), names_to = "Asset", values_to = "Value")
    
    ggplot(df_long, aes(x = YearRounded, y = Value, color = Asset)) +
      geom_line(size = 1.3) +
      scale_color_manual(values = c("Bank" = "#00FFFF", "Bonds" = "#FF5F1F", "Stocks" = "#BC13FE")) +
      geom_vline(xintercept = input$retire_age, linetype = "dashed", color = "white", linewidth = 1.2) +
      annotate("text",
               x = input$retire_age,
               y = max(df_long$Value, na.rm = TRUE) * 0.7,
               label = paste0("Retirement (", input$retire_age, ")"),
               hjust = -0.05,
               color = "#FFFFFF",
               size = 4,
               fontface = "bold") +
      labs(
        title = "Investment Composition Over Time",
        x = "Year",
        y = "PLN",
        color = "Asset Type"
      ) +
      theme_dark_finance() +
      theme(
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white")
      )
  })
  
  
  # Debt Plot (Conditional)
  output$debt_plot <- renderPlot({
    if (!input$has_loan) return(NULL)
    
    df <- simulation()@data
    df$YearRounded <- round(df$Year)
    
    debt_df <- df %>%
      group_by(YearRounded) %>%
      summarise(Debt = max(Debt, na.rm = TRUE)) %>%
      ungroup()
    
    ggplot(debt_df, aes(x = YearRounded, y = Debt)) +
      geom_line(color = "#FF6B6B", size = 1.5) +
      labs(
        title = "Debt Amortization Over Time",
        x = "Year",
        y = "Remaining Debt (PLN)"
      ) +
      theme_dark_finance()
  })
  
  
  # Debt Schedule Table
  output$debt_schedule <- renderTable({
    if (!input$has_loan) return(NULL)
    df <- simulation()@data
    
    loan_term <- input$loan_term
    loan_years <- 0:loan_term
    
    # Finding monthly indexes for start of each loan year
    start_age <- input$age
    loan_months <- loan_years * 12 + 1
    
    df_loan <- df[loan_months, ]
    
    data.frame(
      "User age" = round(df_loan$Year, 0),
      'Loan year' = loan_years,
      'Debt left' = round(df_loan$Debt, 0)
    )
  })
  
  observeEvent(input$reset_button, {
    state$show_results <- FALSE
    updateNumericInput(session, "age", value = 25)
    updateNumericInput(session, "retire_age", value = 65)
    updateNumericInput(session, "life_expectancy", value = 85)
    updateNumericInput(session, "income", value = 7500)
    updateNumericInput(session, "expenses", value = 3000)
    updateNumericInput(session, "income_growth", value = 2)
    updateSliderInput(session, "saving_rate", value = 20)
    updateSelectInput(session, "retirement_destination", selected = 3000)
    updateNumericInput(session, "custom_monthly_cost", value = 3000)
  })
  
  output$showResults <- reactive({
    state$show_results
  })
  outputOptions(output, "showResults", suspendWhenHidden = FALSE)
  observeEvent(input$simulate, {
    state$show_results <- TRUE
  })
  
  
}

shinyApp(ui = ui, server = server)