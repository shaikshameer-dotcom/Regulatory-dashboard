# Install if needed
# install.packages(c("shiny","shinydashboard","plotly","DT","dplyr","ggplot2"))

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(ggplot2)

# -----------------------------
# Simulated Dataset
# -----------------------------
set.seed(123)
n <- 60

data <- data.frame(
  site_id = paste0("SITE_", 1:n),
  study_id = paste0("STUDY_", sample(101:105, n, replace = TRUE)),
  EC_days = sample(5:60, n, replace = TRUE),
  SAE_hours = sample(1:72, n, replace = TRUE),
  PD = sample(c("Yes","No"), n, replace = TRUE),
  CAPA_status = sample(c("Open","In Progress","Closed"), n, replace = TRUE),
  CAPA_days_open = sample(1:60, n, replace = TRUE),
  audit_date = Sys.Date() - sample(1:120, n, replace = TRUE)
)

# -----------------------------
# Compliance + Risk Engine
# -----------------------------
data <- data %>%
  mutate(
    EC_score = ifelse(EC_days <= 30, 1, 0),
    SAE_score = ifelse(SAE_hours <= 24, 1, 0),
    PD_score = ifelse(PD == "No", 1, 0),
    CAPA_score = ifelse(CAPA_status == "Closed", 1, 0),
    
    # Weighted Risk Score (0–100)
    risk_score = 100 - (
      (1 - EC_score)*20 +
        (1 - SAE_score)*40 +
        (1 - PD_score)*20 +
        (1 - CAPA_score)*20
    ),
    
    # Risk Level
    risk_level = case_when(
      risk_score < 50 ~ "Critical",
      risk_score < 75 ~ "Major",
      TRUE ~ "Minor"
    ),
    
    # CAPA overdue
    CAPA_overdue = ifelse(CAPA_days_open > 30 & CAPA_status != "Closed", "Yes","No"),
    
    # Predictive Risk Flag
    predicted_risk = ifelse(SAE_hours > 24 & CAPA_days_open > 20, "High Risk","Normal")
  )

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("Regulatory Intelligence & Risk-Based QA System"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("study", "Select Study", choices = unique(data$study_id)),
      selectInput("risk", "Risk Level", choices = c("All","Critical","Major","Minor"))
    ),
    
    mainPanel(
      
      h3("📊 Risk KPIs"),
      fluidRow(
        valueBoxOutput("critical"),
        valueBoxOutput("major"),
        valueBoxOutput("minor")
      ),
      
      h3("📈 Risk Distribution"),
      plotlyOutput("riskPlot"),
      
      h3("🔥 CAPA Aging"),
      plotlyOutput("capaPlot"),
      
      h3("📋 Audit Log System"),
      DTOutput("auditTable")
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output) {
  
  filtered <- reactive({
    df <- data %>% filter(study_id == input$study)
    if(input$risk != "All"){
      df <- df %>% filter(risk_level == input$risk)
    }
    df
  })
  
  # KPI Boxes
  output$critical <- renderValueBox({
    valueBox(sum(filtered()$risk_level=="Critical"), "Critical Sites", color="red")
  })
  
  output$major <- renderValueBox({
    valueBox(sum(filtered()$risk_level=="Major"), "Major Sites", color="orange")
  })
  
  output$minor <- renderValueBox({
    valueBox(sum(filtered()$risk_level=="Minor"), "Minor Sites", color="green")
  })
  
  # Risk Plot
  output$riskPlot <- renderPlotly({
    p <- ggplot(filtered(), aes(x=risk_level, fill=risk_level)) +
      geom_bar() + theme_minimal()
    ggplotly(p)
  })
  
  # CAPA Aging Plot
  output$capaPlot <- renderPlotly({
    p <- ggplot(filtered(), aes(x=CAPA_days_open)) +
      geom_histogram(bins=10) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Audit Table
  output$auditTable <- renderDT({
    df <- filtered() %>%
      select(site_id, study_id, risk_score, risk_level,
             CAPA_status, CAPA_overdue, predicted_risk, audit_date)
    
    datatable(df, options = list(pageLength=10))
  })
}

shinyApp(ui, server)