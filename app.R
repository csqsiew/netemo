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

# Dropbox directory to save data
outputDir <- "ShinyPsych/netemo"

# Vector with page ids used to later access objects
idsVec <- c("Instructions", "Survey", "Demographics", "Goodbye") 

# create page lists for the instructions and the last page
instructions.list <- createPageList(fileName = "Instruction.txt",
                                    globId = "Instructions", defaulttxt = F) # set defaulttxt to false

survey.list <- createPageList(fileName = "Survey.txt",
                              globId = "Survey", defaulttxt = F)

demographics.list <- createPageList(fileName = "Demographics.txt", globId = 'Demographics', defaulttxt = F)

goodbye.list <- createPageList(fileName = "Goodbye.txt",  globId = 'Goodbye', defaulttxt = F)

### set up stim list 
filenames <- read.csv('filenames.csv')
x <- as.character(filenames$x)

# Section B: Define overall layout =============================================

ui <- fixedPage(
  
  # App title
  title = "Network Emoji Survey",
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
  CurrentValues <- createCtrlList(firstPage = "Instructions", # id of the first page
                                  globIds = idsVec,           # ids of pages for createPage
                                  complCode = F) #,           # create a completion code
  # complName = "Survey")    # first element of completion code
  
  # Section D: Page Layouts ====================================================
  
  ### select and randomize words 
  
  set.seed(as.numeric(Sys.time())) # needs to be in this location for set.seed to work and force a true randomization 
  
  selected <- sample(x, 10, replace = F) # choose 10 pics
  test_set <- sample(selected, length(selected), replace = F) # randomize order
  
  PageLayouts <- reactive({
    
    # insert created completion code that it can later be displayed
    #goodbye.list <- changePageVariable(pageList = goodbye.list, variable = "text",
    #                                   oldLabel = "completion.code",
    #                                   newLabel = CurrentValues$completion.code)
    
    # display instructions page
    if (CurrentValues$page == "Instructions") {
      
      return(
        # create html logic of instructions page
        createPage(pageList = instructions.list,
                   pageNumber = CurrentValues$Instructions.num,
                   globId = "Instructions", ctrlVals = CurrentValues)
      )}
    
    # display survey page
    if (CurrentValues$page == "Survey") {
      
      # update the list with the randomly selected words 
      for (i in 1:length(test_set)) {
        survey.list <- changePageVariable(pageList = survey.list, variable = 'text',
                                           oldLabel = paste0('x', i), newLabel = test_set[i])
      }
      
      return(
        # create html logic of instructions page
        createPage(pageList = survey.list,
                   pageNumber = CurrentValues$Survey.num,
                   globId = "Survey", ctrlVals = CurrentValues)
      )}
    
    # display demographics page
    if (CurrentValues$page == "Demographics"){
      
      return(
        createPage(pageList = demographics.list, pageNumber = CurrentValues$Demographics.num,
                   globId = "Demographics", ctrlVals = CurrentValues)
      )}
    
    # display goodbye page
    if (CurrentValues$page == "Goodbye") {
      
      return(
        createPage(pageList = goodbye.list, pageNumber = CurrentValues$Goodbye.num,
                   globId = "Goodbye", ctrlVals = CurrentValues, continueButton = FALSE)
      )}
    
  })
  
  
  # Section F: Event (e.g.; button) actions ======================================
  
  # Section F1: Page Navigation Buttons ----------------------
  
  
  observeEvent(input[["Instructions_next"]],{
    nextPage(pageId = "Instructions", ctrlVals = CurrentValues, nextPageId = "Survey",
             pageList = instructions.list, globId = "Instructions")
  })
  
  observeEvent(input[["Survey_next"]],{
    nextPage(pageId = "Survey", ctrlVals = CurrentValues,
             nextPageId = "Demographics", pageList = survey.list,
             globId = "Survey")
  })
  
  # Section F2: Event Control ----------------------
  
  
  # Make sure answers are selected
  observeEvent(reactiveValuesToList(input),{
    
    onInputEnable(pageId = "Instructions", ctrlVals = CurrentValues,
                  pageList = instructions.list, globId = "Instructions",
                  inputList = input, charNum = 1)
    
    onInputEnable(pageId = "Survey", ctrlVals = CurrentValues,
                  pageList = survey.list, globId = "Survey",
                  inputList = input, charNum = 1)
    
    onInputEnable(pageId = "Demographics", ctrlVals = CurrentValues,
                  pageList = demographics.list, globId = "Demographics",
                  inputList = input, charNum = 1)
    
  })
  
  # Section G: Save data =========================================================
  
  observeEvent(input[["Demographics_next"]], {(
    
    # Create progress message
    withProgress(message = "Saving data...", value = 0, {
      
      incProgress(.25)
      
      resp <- c(input$Survey_net1, input$Survey_net2, input$Survey_net3, input$Survey_net4,
                input$Survey_net5, input$Survey_net6, input$Survey_net7, input$Survey_net8,
                input$Survey_net9, input$Survey_net10)
      data.list <- data.frame(pic = test_set, resp = resp)
      # Create a list to save data
      # data.list <- list(  
      #   #"id" = input$Instructions_workerid,
      #   #"code" = completion.code,
      #   "net1" = input$Survey_net1,
      #   "net2" = input$Survey_net2,
      #   "net3" = input$Survey_net3,
      #   "net4" = input$Survey_net4,
      #   "net5" = input$Survey_net5
      # )
      # 
      # data.list <- as.data.frame(data.list)
      # 
      # colnames(data.list) <- test_set
      
      saveData(data.list, location = "dropbox", outputDir = outputDir,
               partId = 'netemo_')
      
      CurrentValues$page <- "Goodbye"
      
    })
    
  )})
  
}

# Create app!
shinyApp(ui = ui, server = server)
