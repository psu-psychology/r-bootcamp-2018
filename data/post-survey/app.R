# http://rstudio-pubs-static.s3.amazonaws.com/302891_3a8f5170171545248977bbb7b015f546.html
# http://rpubs.com/msteiner/ShinyPsych_TextfileTutorial

# Example of a Survey using the ShinyPsych package
#
# Code sections:
#   - Section 0: Load Libraries
#   - Section A: assign external values
#   - Section B: Define overall layout
#   - Section C: Define reactive values
#   - Section D: Page layouts
#   - Section F: Event (e.g. button) actions
#       - Section F1: Page navigation button
#       - Section F2: Event Control
#   - Section G: Save Data

# Section 0: Load Libraries ====================================================

library(shiny)
library(shinyjs)
library(ShinyPsych)

# Section A: assign external values ============================================

# Directory to save data
outputDir <- "./post-survey-data/"

# Vector with page ids used to later access objects
idsVec <- c("Instructions", "Survey", "Survey_two", "Goodbye")

# create page lists for the instructions and the last page
instructions.list <- createPageList(fileName = "Instructions.txt",
                                    globId = "Instructions",
                                    defaulttxt = FALSE)
survey.list <- createPageList(fileName = "Survey.txt",
                              globId = "Survey",
                              defaulttxt = FALSE)
survey_two.list <- createPageList(fileName = "Survey_two.txt",
                              globId = "Survey_two",
                              defaulttxt = FALSE)
goodbye.list <- createPageList(fileName = "Goodbye.txt",
                               defaulttxt = FALSE)


# Section B: Define overall layout =============================================

ui <- fixedPage(
  
  # App title
  title = "2018 R Bootcamp Survey",
  uiOutput("MainAction"),
  
  # For Shinyjs functions
  useShinyjs(),
  
  # include appropriate css and js scripts
  includeScriptFiles()
  
)

server <- function(input, output, session) {
  
  output$MainAction <- renderUI( {
    PageLayouts()
    
  })
  
  # Section C: Define Reactive Values ==========================================
  
  # CurrentValues controls page setting such as which page to display
  CurrentValues <- createCtrlList(firstPage = "instructions", # id of the first page
                                  globIds = idsVec,           # ids of pages for createPage
                                  complCode = TRUE,           # create a completion code
                                  complName = "2018_R_Bootcamp_Survey")    # first element of completion code
  
  # Section D: Page Layouts ====================================================
  
  PageLayouts <- reactive({
    
    # insert created completion code that it can later be displayed
    goodbye.list <- changePageVariable(pageList = goodbye.list, variable = "text",
                                       oldLabel = "completion.code",
                                       newLabel = CurrentValues$completion.code)
    
    # display instructions page
    if (CurrentValues$page == "instructions") {
      
      return(
        # create html logic of instructions page
        createPage(pageList = instructions.list,
                   pageNumber = CurrentValues$Instructions.num,
                   globId = "Instructions", ctrlVals = CurrentValues)
      )}
    
    # display survey page
    if (CurrentValues$page == "survey") {
      
      return(
        # create html logic of instructions page
        createPage(pageList = survey.list,
                   pageNumber = CurrentValues$Survey.num,
                   globId = "Survey", ctrlVals = CurrentValues)
      )}
    
    
    if (CurrentValues$page == "survey_two"){
      
      return(
        createPage(pageList = survey_two.list, pageNumber = CurrentValues$Survey_two.num,
                   globId = "Survey_two", ctrlVals = CurrentValues)
      )}
    
    
    # P5) Goodbye
    if (CurrentValues$page == "goodbye") {
      
      return(
        createPage(pageList = goodbye.list, pageNumber = CurrentValues$Goodbye.num,
                   globId = "Goodbye", ctrlVals = CurrentValues, continueButton = FALSE)
      )}
    
  })
  
  
  # Section F: Event (e.g.; button) actions ======================================
  
  # Section F1: Page Navigation Buttons ----------------------
  
  
  observeEvent(input[["Instructions_next"]],{
    nextPage(pageId = "instructions", ctrlVals = CurrentValues, nextPageId = "survey",
             pageList = instructions.list, globId = "Instructions")
  })
  
  observeEvent(input[["Survey_next"]],{
    nextPage(pageId = "survey", ctrlVals = CurrentValues,
             nextPageId = "survey_two", pageList = survey.list,
             globId = "Survey")
  })
  
  
  # Section F2: Event Control ----------------------
  
  # Make sure answers are selected
  observeEvent(reactiveValuesToList(input),{

    onInputEnable(pageId = "instructions", ctrlVals = CurrentValues,
                  pageList = instructions.list, globId = "Instructions",
                  inputList = input)

    onInputEnable(pageId = "survey", ctrlVals = CurrentValues,
                  pageList = survey.list, globId = "Survey",
                  inputList = input, charNum = 4)

    onInputEnable(pageId = "survey_two", ctrlVals = CurrentValues,
                  pageList = survey_two.list, globId = "Survey_two",
                  inputList = input)

  })

  # Section G: Save data =========================================================
  
  observeEvent(input[["Survey_two_next"]], {(
    
    # Create progress message
    withProgress(message = "Saving data...", value = 0, {
      
      incProgress(.25)
      
      # Create a list to save data
      data.list <- list(  "title" = input$Survey_title,
                          "area" = input$Survey_area,
                          "tracks" = input$Survey_tracks,
                          "slowr1_intro" = input$Survey_slowr1_intro,
                          "slowr1_diff" = input$Survey_slowr1_diff,
                          "slowr2_index" = input$Survey_slowr2_index,
                          "slowr2_diff" = input$Survey_slowr2_diff,
                          "slowr3_funcs" = input$Survey_slowr3_funcs,
                          "slowr3_diff" = input$Survey_slowr3_diff,
                          "fastr1_ef" = input$Survey_fastr1_ef,
                          "fastr1_diff" = input$Survey_fastr1_diff,
                          "fastr2_sem" = input$Survey_fastr2_sem,
                          "fastr2_diff" = input$Survey_fastr2_diff,
                          "fastr3_pc" = input$Survey_fastr3_pc,
                          "fastr3_diff" = input$Survey_fastr3_diff,
                          "rer_rate" = input$Survey_rer_rate,
                          "rer_diff" = input$Survey_rer_diff,
                          
                          "day2_1_dwrangling" = input$Survey_day2_1_dwrangling,
                          "day21_diff_" = input$Survey_day2_1_diff,
                          "day2_2_vis" = input$Survey_day2_2_vis,
                          "day2_2_diff" = input$Survey_day2_2_diff,
                          "day2_3_core" = input$Survey_day2_3_core,
                          "day2_3_diff" = input$Survey_day2_3_diff,
                          "day2_4_analyses" = input$Survey_day2_4_analyses,
                          "day2_4_diff" = input$Survey_day2_4_diff,
                          "other_topics" = input$Survey_two_other_topics,
                          "other_feedback" = input$Survey_two_other_feedback
                          )
      
      # save Data
      saveData(data.list, location = "local", outputDir = outputDir,
               partId = data.list$id, suffix = "_s")
      
      CurrentValues$page <- "goodbye"
      
    })
    
  )})
  
}

# Create app!
shinyApp(ui = ui, server = server)