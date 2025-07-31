library(shiny)
library(tidyverse)
library(bslib)
library(ggplot2)
library(plotly)
library(readr)

Netflix_data <- read_csv("netflix_customer_churn.csv")

my_theme <- bs_theme(
  version = 5,
  bg = "#000000",
  fg = "#FFFFFF",
  primary = "#000000",      # Black primary
  secondary = "#FF0000",    # Red secondary
  base_font = font_google("Roboto"),
  heading_font = font_google("Montserrat")
)

ui <- fluidPage(
  theme = my_theme,
  
  tags$head(
    tags$style(HTML("
    body {
      background: linear-gradient(-45deg, #0f0f1a, #1b1b2f, #0f0f1a, #10101a);
      background-size: 500% 500%;
      animation: gradientBG 15s ease infinite;
      background-attachment: fixed;
      background-repeat: no-repeat;
    }

    @keyframes gradientBG {
      0% { background-position: 0% 50%; }
      50% { background-position: 100% 50%; }
      100% { background-position: 0% 50%; }
    }

    .btn,
    .btn-primary,
    .btn-secondary,
    .btn-default {
      background-color: #FF0000 !important;
      border-color: #FF0000 !important;
      color: #FFFFFF !important;
    }

    .btn:hover,
    .btn-primary:hover,
    .btn-secondary:hover,
    .btn-default:hover {
      background-color: #cc0000 !important;
      border-color: #cc0000 !important;
      color: #FFFFFF !important;
    }

    .nav-pills .nav-link {
      color: #FFFFFF !important;
      background-color: transparent !important;
    }

    .nav-pills .nav-link.active {
      background-color: #FF0000 !important;
      color: #FFFFFF !important;
      font-weight: bold !important;
    }

    .nav-pills .nav-link:hover {
      color: #FFAAAA !important;
    }
  "))
  )
  
  ,
  
  tags$div(
    style = "position: absolute; top: 15px; right: 20px; z-index: 9999;",
    tags$img(
      src = "https://upload.wikimedia.org/wikipedia/commons/0/08/Netflix_2015_logo.svg",
      height = "24px",
      style = "filter: drop-shadow(0 0 3px #E50914);"
    )
  ),
  
  div(
    h1("Netflix Data Analysis", 
       style = "text-align:center; font-size: 40px; color: #E50914; margin-top: 30px;")
  ),
  
  # Tabs
  navset_pill(
    id = "tab",
    
    tabPanel("Introduction", 
             textOutput("intro")
    ), 
    
    tabPanel("Graphics",
             sidebarLayout(
               sidebarPanel(
                 selectInput("fav_genre", "gender", choices = unique(Netflix_data$gender)),
                 sliderInput("watch_region", "number_of_profiles", min = 1, max = 5, value = 2),
                 selectInput("age_region", "device", choices = unique(Netflix_data$device))
               ),
               mainPanel(
                 h4("Favorite Genre vs Hours"),
                 plotOutput("plot1"),
                 h4("Watch Hours Based on Region"),
                 plotlyOutput("plot2"),
                 h4("Age and Region"),
                 plotOutput("plot3")
               )
             )
    ), 
    
    tabPanel("Statistics", 
             h4("Average Watch Hours and Age by Subscription Type"),
             tableOutput("stats1"),
             h4("Linear Regression (Age ~ Watch Hours), Age = 43.37+0.04*×Watch Hours"),
             tableOutput("stats2"),
             h4("Churn Confidence Interval"),
             tableOutput("stats3")
    ), 
    
    tabPanel("Full Data", 
             dataTableOutput("data")
    ), 
    
    tabPanel("Conclusion", 
             textOutput("conclusions")
    )
  )
)

# Server
server <- function(input, output) {
  
  output$intro <- renderText({ 
    "About the dataset: This data set includes age, gender, subscription type, watch hours, last login days, region by continent, device, fee, churned status, number of profiles, average watch time per day, and favorite genre. It provides insights into people’s viewing habits across the globe." 
  })
  
  output$plot1 <- renderPlot({
    Netflix_data |>
      filter(gender == input$fav_genre,
             subscription_type == "Standard",
             region == "North America") |>
      ggplot(aes(x = favorite_genre, fill = favorite_genre)) +
      geom_bar() +
      labs(x = "Genre", y = "Hours") +
      theme_minimal()
  })
  
  output$plot2 <- renderPlotly({
    p <- Netflix_data |>
      filter(number_of_profiles == input$watch_region,
             subscription_type == "Standard") |>
      ggplot(aes(x = region, fill = region)) +
      geom_bar() +
      labs(x = "Region", y = "Hours") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot3 <- renderPlot({
    Netflix_data |>
      filter(device == input$age_region) |>
      ggplot(aes(x = age, y = watch_hours, color = region)) +
      geom_point() +
      labs(x = "Age", y = "Hours") +
      theme_minimal() +
      facet_wrap(vars(region))
  })
  
  output$stats1 <- renderTable({
    Netflix_data |>
      group_by(subscription_type) |>
      summarize(`Watch Hours` = mean(watch_hours, na.rm = TRUE),
                `Age` = mean(age, na.rm = TRUE),
                .groups = "drop")
  })
  
  output$stats2 <- renderTable({
    model <- lm(age ~ watch_hours, data = Netflix_data)
    summary(model)$coefficients
  })
  
  output$stats3 <- renderTable({
    churn_vals <- Netflix_data$churned
    n <- sum(!is.na(churn_vals))
    mean_churn <- mean(churn_vals, na.rm = TRUE)
    std_error <- sd(churn_vals, na.rm = TRUE) / sqrt(n)
    error_margin <- qt(0.975, df = n - 1) * std_error
    lower <- round(mean_churn - error_margin, 4)
    upper <- round(mean_churn + error_margin, 4)
    
    tibble(
      `Mean Churn Rate` = round(mean_churn, 4),
      `Lower 95% CI` = lower,
      `Upper 95% CI` = upper
    )
  })
  
  output$data <- renderDataTable({
    Netflix_data
  })
  
  output$conclusions <- renderText({
    paste(
      "This analysis of Netflix customer data reveals several meaningful behavioral patterns. From the graphics, we observe that favorite genres differ by gender, and that North American Standard subscribers tend to show strong preferences for certain genres. Device usage also plays a role in viewing behavior, as seen in the variation of watch hours by age and region. The number of profiles on an account correlates with regional distribution, suggesting that family or group viewing habits might influence account structure. The regression analysis indicates a statistically significant, albeit small, positive relationship between watch hours and user age, hinting that older users tend to spend slightly more time watching Netflix.",
      
      "From a business perspective, these findings can help inform targeted marketing, device-specific optimization, and recommendation strategies. Subscription types show clear differences in average age and engagement, and the churn confidence interval provides a reliable estimate of expected customer attrition. Together, these insights offer Netflix an opportunity to reduce churn by enhancing personalized experiences and tailoring content or promotional offers to user profiles. Future studies could incorporate time-based trends or content categories to refine these findings even further.",
      sep = "\n\n"
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
