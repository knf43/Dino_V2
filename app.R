# Load necessary libraries
library(shiny)
library(future)
library(rsconnect)
library(googlesheets4)
library(dplyr)


plan(multisession)  # Enable background processing

# Function to save responses locally
save_response <- function(data) {
  library(googlesheets4)
  library(gargle)
  
  # Authenticate with Google Sheets (Using Cached Credentials)
  gs4_auth(email = "knf43@scarletmail.rutgers.edu", cache = ".secrets")
  
  # Your Google Sheet ID
  sheet_id <- "1QPi2nj2gz_JqFx8HtSUXkdSpsukiKg9OPY9X7aXiJrE"
  
  tryCatch({
    # Append new data to Google Sheets
    sheet_append(sheet_id, data)
    print("✅ Response successfully saved to Google Sheets!")
  }, error = function(e) {
    print(paste("❌ Error saving to Google Sheets:", e$message))
  })
}


# Define UI
ui <- fluidPage(
  titlePanel("Background Questionnaire"),
  
  # Questions
  textInput("name", "What is your name?", ""),
  textInput("instructor", "What is your instructor's name?", ""),
  textInput("email", "What is your email?", ""),
  numericInput("age", "How old are you?", value = NA, min = 18, max = 100),
  
  selectInput("gender", "What is your gender?", 
              choices = c("Male", "Female", "Other")),
  
  radioButtons("immigration_status", "Have you immigrated to the USA?",
               choices = c("Yes", "No"), inline = TRUE),
  conditionalPanel(
    condition = "input.immigration_status == 'Yes'",
    dateInput("immigration_date", "Date of immigration to the USA:", value = Sys.Date())
  ),
  
  numericInput("age_spanish", "At what age did you begin learning Spanish?", 
               value = NA, min = 0, max = 100),
  
  textInput("time_country", "Time spent in a country where Spanish is spoken (years, months):", ""),
  textInput("time_family", "Time spent in a family where Spanish is spoken (years, months):", ""),
  textInput("time_school", "Time spent in a school or workplace where Spanish is spoken (years, months):", ""),
  
  h4(HTML("Rate your proficiency in Spanish:")),
  p("0 = Not Proficient"),
  p("5 = Somewhat Proficient"),
  p("10 = Highly Proficient"),
  sliderInput("speak", HTML("<b>Speaking</b> Spanish"), 0, 10, 5),
  sliderInput("understand", HTML("<b>Understanding</b> Spanish"), 0, 10, 5),
  sliderInput("read", HTML("<b>Reading</b> Spanish"), 0, 10, 5),
  
  h4(HTML("Please rate to what extent you are <b>currently</b> exposed to Spanish in the following contexts:")),
  p("0 = Never"),
  p("5 = Half of the Time"),
  p("10 = Always"),
  
  sliderInput("friends", "Interacting with friends", 0, 10, 5),
  sliderInput("family", "Interacting with family", 0, 10, 5),
  sliderInput("reading", "Reading", 0, 10, 5),
  sliderInput("apps", "Language apps/websites/self-instruction", 0, 10, 5),
  sliderInput("tv", "Watching TV", 0, 10, 5),
  sliderInput("radio", "Listening to radio/music", 0, 10, 5),
  sliderInput("work_school", "At work/school", 0, 10, 5),
  
  textInput("languages_acquisition", "List all the languages you know in order of acquisition:", ""),
  textInput("languages_dominance", "List all the languages you know in order of dominance:", ""),
  
  actionButton("submit", "Submit"),
  textOutput("confirmation")
)

# Define Server
server <- function(input, output, session) {
  observeEvent(input$submit, {
    print("🚀 Submit button clicked")  # Debugging message
    
    output$confirmation <- renderText("✅ Your response is being recorded...")
    
    future({
      tryCatch({
        # Use isolate() to safely access input values inside future()
        proficiency_avg <- mean(c(isolate(input$speak), isolate(input$understand), isolate(input$read)))
        language_use_avg <- mean(c(isolate(input$friends), isolate(input$family), isolate(input$reading), 
                                   isolate(input$apps), isolate(input$tv), isolate(input$radio), isolate(input$work_school)))
        
        response <- data.frame(
          Name = isolate(input$name),
          Instructor = isolate(input$instructor),
          Email = isolate(input$email),
          Age = isolate(input$age),
          Gender = isolate(input$gender),
          Immigration_Status = isolate(input$immigration_status),
          Immigration_Date = ifelse(isolate(input$immigration_status) == "Yes", as.character(isolate(input$immigration_date)), "N/A"),
          Age_Spanish = isolate(input$age_spanish),
          Time_Country = isolate(input$time_country),
          Time_Family = isolate(input$time_family),
          Time_School = isolate(input$time_school),
          Speak_Spanish = isolate(input$speak),
          Understand_Spanish = isolate(input$understand),
          Read_Spanish = isolate(input$read),
          Proficiency_Avg = round(proficiency_avg, 2),
          Friends_Exposure = isolate(input$friends),
          Family_Exposure = isolate(input$family),
          Reading_Exposure = isolate(input$reading),
          Apps_Exposure = isolate(input$apps),
          TV_Exposure = isolate(input$tv),
          Radio_Exposure = isolate(input$radio),
          Work_School_Exposure = isolate(input$work_school),
          Language_Use_Avg = round(language_use_avg, 2),
          Languages_Acquisition = isolate(input$languages_acquisition),
          Languages_Dominance = isolate(input$languages_dominance),
          Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          stringsAsFactors = FALSE
        )
        
        print("📄 Response data frame created")  # Debugging message
        print(response)  # Print to check if the data is correct
        
        # Save response to Google Sheets
        save_response(response)
        
        output$confirmation <- renderText("✅ Response recorded in Google Sheets!")
        
      }, error = function(e) {
        print(paste("❌ Error:", e$message))
        output$confirmation <- renderText("❌ Error saving response.")
      })
    })
  })
  
  
  
}  # <-- Add this closing bracket to properly close the 'server' function

# Run the application
shinyApp(ui, server)
