#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(readr)


Master <- read_csv("Master.csv")
Batting <- read_csv("Batting.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(includeScript("www/google-analytics.js")),
   # Application title
   titlePanel(" Baseball Player: Home Run Comparison"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        actionButton(inputId = "go",label = "Create Home Run Graph"),
        br(),
        br(),
        actionButton(inputId = "get",label = "Get Player Stats"),
        hr(),
         textInput("firstone",label = "Player One",value = "Babe Ruth"),
         #textInput("firsttwo",label = "Player One Last Name",value = "Ruth"),
         textInput("secondone",label = "Player Two",value = "Hank Aaron"),
         #textInput("secondtwo",label = "Player Two Last Name",value = "Aaron"),
         textInput("thirdone",label = "Player Three",value = "Barry Bonds"),
         #textInput("thirdtwo",label = "Player Three Last Name",value = "Bonds"),
         textInput("fourthone",label = "Player Four",value = "Mark McGwire")
         #textInput("fourthtwo",label = "Player Four Last Name",value = "McGwire")
        
        
        
      ),
      
      
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
         tabPanel("Home Run Comparison",plotOutput("distPlot",width = "100%", height = "600px")),
         tabPanel("Player One Stats",dataTableOutput("tableone")),
         tabPanel("Player Two Stats",dataTableOutput("tabletwo")),
         tabPanel("Player Three Stats",dataTableOutput("table3")),
         tabPanel("Player Four Stats",dataTableOutput("table4"))
         
      ))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  # getinfo <- function(firstname, lastname){
  #   playerline <- subset(Master,
  #                        nameFirst==firstname & nameLast==lastname)
  #   name.code <- as.character(playerline$playerID)
  #   birthyear <- playerline$birthYear
  #   birthmonth <- playerline$birthMonth
  #   birthday <- playerline$birthDay
  #   byear <- ifelse(birthmonth <= 6, birthyear, birthyear + 1)
  #   list(name.code=name.code, byear=byear)}
  # 
  
 
  
 
   
  observeEvent(input$go,{output$distPlot <- renderPlot({
    
    withProgress(message = 'Creating Home Run Comparison Graph',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
     
     getinfo <- function(name){
       # playerline <- subset(Master,
       #                      nameFirst==firstname & nameLast==lastname)
       playerline <- subset(Master,NAMES==name)
       name.code <- as.character(playerline$playerID)
       birthyear <- playerline$birthYear
       birthmonth <- playerline$birthMonth
       birthday <- playerline$birthDay
       byear <- ifelse(birthmonth <= 6, birthyear, birthyear + 1)
       list(name.code=name.code, byear=byear)}
     
     
     
     playerone <- getinfo(input$firstone)
     
     playertwo <- getinfo(input$secondone)
     
     playerthree <- getinfo(input$thirdone)
     
     playerfour <- getinfo(input$fourthone)
     
     # playeroneinfo <- getinfo("Babe", "Ruth")
     # 
     # 
     # 
     # playertwoinfo <- reactive({getinfo(input$secondone, input$secondtwo)})
     # playerthreeinfo <- reactive({getinfo(input$thirdone, input$thirdtwo)})
     # playerfourinfo <- reactive({getinfo(input$fourthone, input$fourthtwo)})
     # 
     
     playeronedata <- subset(Batting, playerID == playerone$name.code)
     playeronedata$Age <- playeronedata$yearID - playerone$byear
     
     playertwodata <- subset(Batting, playerID == playertwo$name.code)
     playertwodata$Age <- playertwodata$yearID - playertwo$byear
     
     playerthreedata <- subset(Batting, playerID == playerthree$name.code)
     playerthreedata$Age <- playerthreedata$yearID - playerthree$byear
     
     playerfourdata <- subset(Batting, playerID == playerfour$name.code)
     playerfourdata$Age <- playerfourdata$yearID - playerfour$byear
     
     
     
     with(playeronedata, plot(Age, cumsum(HR), type="l", lty=3, lwd=2,
                          xlab="Age", ylab="Career Home Runs",
                          xlim=c(18, 45), ylim=c(0, 800),col = "blue"))
     with(playertwodata, lines(Age, cumsum(HR), lty=2, lwd=2,col="green"))
     with(playerthreedata, lines(Age, cumsum(HR), lty=1, lwd=2,col ="red"))
     with(playerfourdata, lines(Age, cumsum(HR), lty=4, lwd=2,col="orange"))
     legend(20, 700, legend=c(input$firstone, input$secondone, input$thirdone, input$fourthone),
            lty=1 : 4, lwd=2,col=c("blue","green","red","orange"))
   })})
  
  
  ##Player One Stats
  observeEvent(input$get,{output$tableone <- renderDataTable({
    
    withProgress(message = paste0("Getting ",input$firstone,"Stats"),
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    
    playerinfo1 <- subset(Master,NAMES == input$firstone)
    
    playerone <- playerinfo1$playerID
    
    playerstats <- subset(Batting,Batting$playerID == playerone)
    
    playerstats <- as.data.frame(playerstats)
    
    playerstats <- playerstats[,c(2,4:14)]
    
    colnames(playerstats) <- c("Year","Team","League","G","AB","R","H","2B","3B","HR","RBI","SB")
    
    playerstats
    
  })})
  
  ##Player Two Stats
  observeEvent(input$get,{output$tabletwo <- renderDataTable({
    
    withProgress(message = paste0("Getting ",input$secondone," Stats"),
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    
    playerinfo2 <- subset(Master,NAMES == input$secondone)
    
    playertwo <- playerinfo2$playerID
    
    playerstats2 <- subset(Batting,Batting$playerID == playertwo)
    
    playerstats2 <- as.data.frame(playerstats2)
    
    playerstats2 <- playerstats2[,c(2,4:14)]
    
    colnames(playerstats2) <- c("Year","Team","League","G","AB","R","H","2B","3B","HR","RBI","SB")
    
    playerstats2
    
  })})
  
  ##Player Three Stats ####
  observeEvent(input$get,{output$table3 <- renderDataTable({
    withProgress(message = paste0("Getting ",input$thirdone,"Stats"),
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    
    playerinfo3 <- subset(Master,NAMES == input$thirdone)
    
    playerthree <- playerinfo3$playerID
    
    playerstats3 <- subset(Batting,Batting$playerID == playerthree)
    
    playerstats3 <- as.data.frame(playerstats3)
    
    playerstats3 <- playerstats3[,c(2,4:14)]
    
    colnames(playerstats3) <- c("Year","Team","League","G","AB","R","H","2B","3B","HR","RBI","SB")
    
    playerstats3
    
  })})
  
  ##Player Four Stats
  observeEvent(input$get,{output$table4 <- renderDataTable({
    
    withProgress(message = paste0("Getting ",input$fourthone,"Stats"),
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    playerinfo4 <- subset(Master,NAMES == input$fourthone)
    
    playerfour <- playerinfo4$playerID
    
    playerstats4 <- subset(Batting,Batting$playerID == playerfour)
    
    playerstats4 <- as.data.frame(playerstats4)
    
    playerstats4 <- playerstats4[,c(2,4:14)]
    
    colnames(playerstats4) <- c("Year","Team","League","G","AB","R","H","2B","3B","HR","RBI","SB")
    
    playerstats4
    
  })})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

