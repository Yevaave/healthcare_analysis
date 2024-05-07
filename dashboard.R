library(shinydashboard)
library(shiny)
library(maps)
library(dplyr)
library(raster)
library(ggpubr)
library(ggplot2)
library(viridis)
library(ggrepel)
library(reshape)
library(stargazer)
library(hrbrthemes)
library(wesanderson)
library(magrittr)
library(plotly)
library(ggthemes)
library(viridis)
library(RColorBrewer)
library(data.table)
library(summarytools)
library(parameters)
library(DT)
library(readr)
library(readxl)
library(plotly)
library(grid)
library(gridExtra)
library(rsconnect)
library(countrycode)
library(tidyr)
library(thematic)
library(ggrepel)
library(jsonlite)
library(sf)
library(rnaturalearth)
library(shinythemes)
extrafont::loadfonts(device="win")
options( scipen = 999 )





data <- read.csv("C:/Users/User/Downloads/capstone/cleaned_data.csv")

colnames(data)[which(names(data) == "Work.sphere")] <- "work_sphere"
colnames(data)[which(names(data) == "Start.Date")] <- "start_date"
colnames(data)[which(names(data) == "End.Date")] <- "end_date"
colnames(data)[which(names(data) == "DOB")] <- "dob"
colnames(data)[which(names(data) == "Sex")] <- "sex"
colnames(data)[which(names(data) == "Region")] <- "region"
colnames(data)[which(names(data) == "Claim.Code")] <- "claim_code"
colnames(data)[which(names(data) == "Claim.Status")] <- "claim_status"
colnames(data)[which(names(data) == "Illness.ICD.Code")] <- "illness_code"
colnames(data)[which(names(data) == "Illness.Category")] <- "illness_category"
colnames(data)[which(names(data) == "Claim.Amount")] <- "claim_amount"
colnames(data)[which(names(data) == "Contract.Longevity")] <- "contract_longevity"
colnames(data)[which(names(data) == "Age")] <- "age"
colnames(data)[which(names(data) == "BirthYear")] <- "byear"
colnames(data)[which(names(data) == "Short.Work.Sphere")] <- "short_work_sphere"

data$sex=factor(data$sex, levels = c("Female", "Male", "Unknown"))
data$claim_status=factor(data$claim_status, levels = c("Claim creation", "Initial", "Final claim confirmation", "Satisfied", "Unsatisfied", "Canceled"))

# technical adjustments
data$log_claim_amount <- log(data$claim_amount + 1)

#####


ui <- dashboardPage(
  header = dashboardHeader(title = "Capstone Project"),
  skin = "blue",
  sidebar = dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("About", tabName = "about"),
      menuItem("Demographic Insights", tabName = "demographics"),
      menuItem("Healthcare Claims Analysis", tabName = "claim_analysis"),
      menuItem("Box Plot Analysis", tabName = "box_plot_analysis"),
      menuItem("Interesting Insights", tabName = "top_illnesses"),
      menuItem("Fun Facts / Issues", tabName = "fun_facts")
    )
  ),
  body = dashboardBody(
    tags$style(
      HTML("
        /* Custom CSS styles */
        body {
          font-family: Arial, sans-serif;
          background-color: #f2f2f2;
        }
        .sidebar {
          background-color: #333;
        }
        .sidebar-menu li a {
          color: #fff;
        }
        .content-wrapper {
          background-color: #fff;
        }
        ")
    ),
    tabItems(
      tabItem(tabName = "about",
              fluidRow(
                column(width = 12,
                       HTML("
                      <div style='padding: 20px; background-color: transparent;'>
                      
                        <h1 style='font-size: 24px; color: #333; text-align: center;'>Pulse of the Nation: A Statistical Investigation into the Vital Signs of Armenian Healthcare</h1>
                        <p style='font-size: 18px; color: #666; text-align: center;'>Anush Aghinyan, Yeva Avetisyan</p>
                        <p style='font-size: 16px; color: #999; text-align: justify;'>This project aims to analyze healthcare data in Armenia to uncover insights into demographic trends, healthcare utilization, and prevalent illnesses.</p>
                        <h2 style='font-size: 20px; color: #333; text-align: center;'>Key Features:</h2>
                        <ul style='font-size: 16px; color: #666;'>
                          <li>Interactive visualizations of demographic data</li>
                          <li>Analysis of healthcare claims and illnesses</li>
                          <li>Insights into regional healthcare disparities</li>
                        </ul>
                        <p style='font-size: 14px; color: #999; position: absolute; bottom: 10px; right: 10px;'>Yerevan, 2024</p>
                      </div>
                      ")
                )
              )
      ),
      tabItem(tabName = "demographics",
              fluidRow(
                column(width = 6,
                       plotlyOutput("age_distribution"),
                       helpText("This plot visualizes the distribution of ages among the dataset.")),
                column(width = 6,
                       plotlyOutput("sex_distribution"),
                       helpText("This plot visualizes the distribution of sexes (gender) within the dataset."))
              ),
              fluidRow(
                column(width = 6,
                       plotlyOutput("age_sex_distribution"),
                       helpText("This plot shows the distributional differences of age between different sexes. It allows for the comparison of age distributions across genders.")),
                column(width = 6,
                       plotlyOutput("region_distribution"),
                       helpText("This plot shows the distribution of individuals across different regions."))
              ),
              fluidRow(
                column(width = 12, 
                       plotlyOutput("work_sphere_distribution"),
                       helpText("This plot visualizes the distribution of individuals across various work spheres."))
              )
      ),
      tabItem(tabName = "claim_analysis",
              fluidRow(
                column(width = 6,
                       plotlyOutput("claim_amount_distribution"),
                       helpText("This plot shows the distribution of claim amounts.")),
                column(width = 6,
                       plotlyOutput("claim_status_distribution"),
                       helpText("This plot illustrates the distribution of claim statuses within the dataset."))
              ),
              fluidRow(
                column(width = 12,
                       plotlyOutput("illness_category_distribution"),
                       helpText("This plot displays the distribution of individuals across different illness categories."))
              )),
      tabItem(tabName = "box_plot_analysis",
              fluidRow(
                column(width = 12,
                       plotlyOutput("claim_amounts_by_sex"),
                       helpText("This plot presents the distribution of claim amounts categorized by gender.")
                )
              ),
              fluidRow(
                column(width = 12,
                       uiOutput("variable_selector"),
                       helpText("Select variables to analyze:")
                )
              ),
              fluidRow(
                column(width = 12,
                       plotlyOutput("claim_amounts_by_sex_status"),
                       helpText("This plot illustrates the distribution of claim amounts across different sexes, segmented by claim status.")
                )
              ),
              fluidRow(
                column(width = 12,
                       plotlyOutput("claim_amounts_by_region"),
                       helpText("This plot presents the distribution of claim amounts across various regions.")
                )
              )
      ),
      tabItem(tabName = "top_illnesses",
              fluidRow(
                column(width = 12,
                       radioButtons("gender", "Select Gender:", choices = c("Both", "Male", "Female"), selected = "Both"),
                       plotOutput("illness_plot"),
                       helpText("This plot showcases the top five most popular illnesses within the dataset.")
                       )),
              fluidRow(
                column(width = 6,
                       radioButtons("gender", "Select Gender:", choices = c("Both", "Male", "Female"), selected = "Both"),
                       plotOutput("top_age_oncology_plot"),
                       helpText("This plot visualizes the top age groups with oncological claims.")),
                column(width = 6,
                       radioButtons("gender", "Select Gender:", choices = c("Both", "Male", "Female"), selected = "Both"),
                       plotOutput("top_age_dentistry_plot"),
                       helpText("This plot displays the top age groups with dental claims."))
                      ),
              fluidRow(
                column(width = 12,
                       radioButtons("gender", "Select Gender:", choices = c("Both", "Male", "Female"), selected = "Both"),
                       plotOutput("top_work_eye_doctor_plot"),
                       helpText("This plot presents the top work spheres or industries where individuals seek medical attention from eye doctors."))
              )
      
      ),
      tabItem(tabName = "fun_facts",
              fluidRow(
                column(width = 6,
                       plotlyOutput("male_gynecology"),
                       helpText("This plot visualizes cases of Gynecology Claims where Male individuals are present too.")),
                column(width = 6,
                       plotlyOutput("female_urology"),
                       helpText("This plot shows cases Urology Claims where Female individuals are present too."))
            ),
            fluidRow(
              column(width = 6,
                     plotlyOutput("male_pregnancy"),
                     helpText("This plot visualizes cases of Pregnancy Claims where Male individuals are present too.")
              )
            )
    )
    
  )
)
)

# Define server logic
server <- function(input, output, session) {

  # Reactive expression to filter data based on selected gender
  filtered_data <- reactive({
    if (input$gender == "Both") {
      filtered <- data
    } else {
      filtered <- data %>% filter(sex == input$gender)
    }
    return(filtered)
  })

  # Reactive expression to filter data based on selected variables for boxplots
  filtered_boxplot_data <- reactive({
    if (!is.null(input$variable)) {
      filtered <- data %>% filter(claim_status %in% input$variable)
    } else {
      filtered <- data
    }
    return(filtered)
  })
  
  # Plotly plots
  
  output$age_distribution <- renderPlotly({
    # Age distribution plot
    ggplotly(ggplot(data, aes(x = age)) +
      geom_histogram(binwidth = 5, fill = "skyblue", color = "black") + theme_classic() +
      labs(title = "Age Distribution", x = "Age", y = "Frequency")
    )
  })
  
  output$sex_distribution <- renderPlotly({
    # Sex distribution plot
    ggplotly(ggplot(data, aes(x = sex)) + geom_bar(fill = "lightgreen", color = "black") +
               theme_classic() +
               labs(title = "Sex Distribution", x = "Sex", y = "Frequency")
    )
  })
  
  output$age_sex_distribution <- renderPlotly({
    # Distributional difference of Age and Sex plot
    ggplotly(ggplot(data, aes(x = age, fill = sex)) + 
               geom_density(alpha = 0.5) +
               theme_classic() + 
               scale_x_continuous(limits = c(0,100)) +
               labs(x = "Age", title = "Distributional Difference of Age and Sex", fill = "Sex")
    )
  })
  
  output$region_distribution <- renderPlotly({
    # Region distribution plot
    ggplotly(ggplot(data, aes(x = region)) +
               geom_bar(stat = 'count', fill = "salmon") +
               theme_classic() +
               labs(title = "Region Distribution", x = "Region", y = "Frequency") +
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })
  
  output$work_sphere_distribution <- renderPlotly({
    # Work Sphere distribution plot
    ggplotly(ggplot(data, aes(x = short_work_sphere)) + 
               geom_bar(fill = "orange") +
               theme_classic() +
               labs(title = "Work Sphere Distribution", x = "Work Sphere", y = "Frequency") +
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })
  
  output$claim_amount_distribution <- renderPlotly({
    # Claim Amount Distribution
    ggplotly(ggplot(data %>% filter(log_claim_amount > 0), aes(x = log_claim_amount)) +
               theme_classic() +
               geom_histogram(binwidth = 0.1, fill = "skyblue") +
               labs(title = "Claim Amount Distribution", x = "Claim Amount", y = "Frequency")
    )
  })
  
  output$claim_status_distribution <- renderPlotly({
    # Claim Status Distribution
    ggplotly(ggplot(data, aes(x = claim_status)) +
               geom_bar(fill = "purple") + theme_classic() +
               labs(title = "Claim Status Distribution", x = "Claim Status", y = "Frequency")
    )
  })
  
  output$illness_category_distribution <- renderPlotly({
    # Illness Category Distribution
    ggplotly(ggplot(data, aes(x = illness_category)) +
               geom_bar(fill = "#CDF77A") + theme_classic() +
               labs(title = "Illness Category Distribution", x = "Illness Category", y = "Frequency") +
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
               geom_text(stat='count', aes(label=..count..), parse = FALSE, position = position_dodge(width = 0.9), vjust = -0.2, size = 2)
    )
  })
  
  output$claim_amounts_by_sex <- renderPlotly({
    # Claim Amounts by Sex
    ggplotly(ggplot(data, aes(x = sex, y = log_claim_amount, fill = sex)) +
               geom_boxplot() +
               theme_classic() +
               labs(title = "Claim Amounts by Sex", x = "Sex",y = "Claim Amount", fill = "Sex"))
  })
  
  output$claim_amounts_by_sex_status <- renderPlotly({
    # Claim Amount by Sex and Claim Status
    ggplotly(ggplot(filtered_boxplot_data() %>% filter(claim_status %in% c("Claim creation", "Initial", "Satisfied")), aes(x = sex, y = log_claim_amount, fill = sex)) +
               geom_boxplot() +
               facet_grid(. ~ claim_status) +
               labs(title = "Claim Amounts by Sex and Claim Status", x = "Sex", y = "Claim Amount") +
               theme(axis.text.x = element_text(angle = 45)) + theme_classic())
  })
  
  output$claim_amounts_by_region <- renderPlotly({
    # Claim Amount by Region
    ggplotly(ggplot(data, aes(x = region, y = log_claim_amount, fill = region)) +
               geom_boxplot() +
               theme_classic() +
               labs(title = "Claim Amounts by Region", x = "Region", y = "Claim Amount", fill = "Region") +
               theme(axis.text.x = element_text(angle = 45)))
  })
  
  output$variable_selector <- renderUI({
    selectInput("variable", "Select Variable:",
                choices = c("Claim creation", "Initial", "Satisfied"),
                selected = NULL,
                multiple = TRUE)
  })
  
  output$illness_plot <- renderPlot({
    # Top 5 illnesses
    top_5_illnesses <- filtered_data() %>%
      group_by(illness_category) %>%
      summarise(Count = n()) %>%
      top_n(5, Count)
    
    ggplot(top_5_illnesses, aes(x = reorder(illness_category, Count), y = Count, fill = illness_category)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 5 Illnesses", x = "Illness", y = "Frequency", fill = "Illness category") +
      theme_classic() +
      coord_flip() +
      scale_fill_manual(values = c("#3333cc", "#cc00cc", "#ffcc00", "#00994d", "#4da6ff"))
  })
  
  output$top_age_oncology_plot <- renderPlot({
    # Top age groups oncology
    top_age_oncology <- filtered_data() %>%
      filter(illness_category == "Oncology") %>%
      group_by(age) %>%
      summarise(Count = n()) %>%
      arrange(age) %>%
      top_n(5, Count)
    
    ggplot(top_age_oncology, aes(x = factor(age), y = Count, fill = as.factor(age))) +
      geom_col() +
      labs(title = "Top Age Groups with Oncological Claims", x = "Age Group", y = "Frequency", fill = "Age Group") +
      theme_classic() +
      scale_fill_manual(values = c("#b3003b", "#0066cc", "#e6b800", "#339933", "#2e2eb8"))
  })
  
  output$top_age_dentistry_plot <- renderPlot({
    # Top age groups dentistry
    top_age_dentistry <- filtered_data() %>%
      filter(illness_category == "Dentistry") %>%
      group_by(age) %>%
      summarise(Count = n()) %>%
      arrange(age) %>%
      top_n(5, Count)
    
    ggplot(top_age_dentistry, aes(x = factor(age), y = Count, fill = as.factor(age))) +
      geom_col() +
      labs(title = "Top Age Groups with Dental Claims", x = "Age Group", y = "Frequency", fill = "Age Group") +
      theme_classic() +
      scale_fill_manual(values = c("#ff7733", "#b30059", "#0099cc", "#669900", "#cccc00"))
    
  })
  
  output$top_work_eye_doctor_plot <- renderPlot({
    # Top work spheres eye visiting the eye doctor
    filtered_eye_doctor <- filtered_data() %>%
      filter(illness_category == "Ophthalmology") %>%
      group_by(short_work_sphere) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      top_n(3, Count)
    
    ggplot(filtered_eye_doctor, aes(x = reorder(short_work_sphere, Count), y = Count, fill = short_work_sphere)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 3 Work Spheres Visiting the Eye Doctor", x = "Work Sphere", y = "Number of Visits", fill = "Work Sphere") +
      theme_classic() +
      coord_flip() +
      scale_fill_manual(values = c("#ff9933", "#bf80ff", "#33adff"))
  })
  
  output$male_gynecology <- renderPlotly({
    # Males with Gynecology Claims
    male_gynecology_cases <- data %>%
      filter(illness_category == "Gynecology")
    
    ggplotly(ggplot(male_gynecology_cases, aes(x = sex, fill = illness_category)) +
               geom_bar() + theme_classic() +
               labs(title = "Cases of Males with Gynecology Claims", x = "Sex", y = "Frequency", fill = "Illness Category") +
               scale_fill_manual(values = "#b30059"))
  })
  
  output$female_urology <- renderPlotly({
    # Females with Urology Claims
    female_urology_cases <- data %>%
      filter(illness_category == "Urology")
    
    ggplotly(ggplot(female_urology_cases, aes(x = sex, fill = illness_category)) +
               geom_bar() + theme_classic() +
               labs(title = "Cases of Females with Urology Claims", x = "Sex", y = "Frequency", fill = "Illness Category") +
               scale_fill_manual(values = "#005ce6"))
  })
  
  output$male_pregnancy <- renderPlotly({
    # Males with Pregnancy Claims
    male_pregnancy_cases <- data %>%
      filter(illness_category == "Pregnancy")
    
    ggplotly(ggplot(male_pregnancy_cases, aes(x = sex, fill = illness_category)) +
               geom_bar() + theme_classic() +
               labs(title = "Cases of Males with Pregnancy Claims", x = "Sex", y = "Frequency", fill = "Illness Category") +
               scale_fill_manual(values = "#e68a00"))
  })
  
}


shinyApp(ui, server)

