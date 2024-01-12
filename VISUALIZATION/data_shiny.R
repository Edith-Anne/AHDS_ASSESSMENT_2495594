
demo_data <- read.csv("DEMO_D.csv")
bmi_data <- read.csv("BMI.csv")
library(dplyr)

# Loading the datasets required 
bmi_data <- na.omit(bmi_data)  # Removes rows with NA values
data_merg <- merge(bmi_data, demo_data, by="SEQN")

# Selecting the necessary columns
data_merg <- data_merg %>% select(SEQN, RIDAGEYR, BMXBMI)

# Cleaning the data 
data_merg <- data_merg %>% filter(!is.na(BMXBMI), !is.na(RIDAGEYR))

# Correlation
correlation <- cor(data_merg$RIDAGEYR, data_merg$BMXBMI, use = "complete.obs")

# Simple Linear Regression
fit <- lm(BMXBMI ~ RIDAGEYR, data = data_merg)

summary(fit)

# Creating age groups
data_merg$AgeGroup <- cut(data_merg$RIDAGEYR, breaks = c(0, 18, 35, 50, 65, Inf), right = FALSE, labels = c("0-17", "18-34", "35-49", "50-64", "65+"))

# Grouped summary
grouped_summary <- aggregate(BMXBMI ~ AgeGroup, data = data_merg, FUN = mean)

library(ggplot2)

# Scatter plot with regression line
ggplot(data_merg, aes(x = AgeGroup, y = BMXBMI)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = lm, color = "blue") +
  labs(x = "Age", y = "BMI", title = "Relationship between Age and BMI")

# Boxplot for BMI across age groups
ggplot(data_merg, aes(x = AgeGroup, y = BMXBMI)) +
  geom_boxplot() +
  labs(x = "Age Group", y = "BMI", title = "BMI Distribution across Age Groups")

install.packages("shiny")
library(shiny)

# Define the Shiny UI (User Interface)
ui <- fluidPage(
  titlePanel("BMI and Age Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Displays the relationship between Age and BMI"),
      
      # Input: Select the type of plot
      selectInput("plotType", "Select Plot Type:",
                  choices = c("Scatter Plot", "Box Plot"))
    ),
    
    # Output: Show the plot
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Generate the plot based on input
  output$plot <- renderPlot({
    # Scatter plot
    if (input$plotType == "Scatter Plot") {
      ggplot(data_merg, aes(x = AgeGroup, y = BMXBMI)) +
        geom_point(alpha = 0.4) +
        geom_smooth(method = lm, color = "blue") +
        labs(x = "Age", y = "BMI", title = "Relationship between Age and BMI")
    } 
    # Box plot
    else if (input$plotType == "Box Plot") {
      ggplot(data_merg, aes(x = AgeGroup, y = BMXBMI)) +
        geom_boxplot() +
        labs(x = "Age Group", y = "BMI", title = "BMI Distribution across Age Groups")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
