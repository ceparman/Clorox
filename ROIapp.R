## Setup ##

## Libraries

library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(shinyWidgets)




## Load Julian function
source("ROIhelpers.R")

library(shinydashboard)


## Set timeout length to 1 hour

inactivity <- "function idleTimer() {
  var t = setTimeout(logout, 3600000);
  window.onmousemove = resetTimer; // catches mouse movements
  window.onmousedown = resetTimer; // catches mouse movements
  window.onclick = resetTimer;     // catches mouse clicks
  window.onscroll = resetTimer;    // catches scrolling
  window.onkeypress = resetTimer;  //catches keyboard actions

  function logout() {
    window.close();  //close the window
  }

  function resetTimer() {
    clearTimeout(t);
    t = setTimeout(logout, 3600000);  // time is in milliseconds (1000 is 1 second)
  }
}
idleTimer();"

### color

clorox_dk_blue = "#0F4C92"

clorox_lt_blue = "#008DD1"

clorox_yellow = "#FFD92A"

      
ui <- fluidPage(
    tags$script(inactivity),
   
    useShinydashboard(),
     
     ## Clorox Theme
    # theme = bs_theme(
    #   bg = "#215daa",
    #   fg = "white",
    #   primary = "#ffcd00",
    #   base_font = font_google("Open Sans"),
    #   code_font = font_google("Open Sans")
    # ),
    
    ## Clorox Them 
    theme = bs_theme(bg =   "white" ,
                     fg = clorox_dk_blue, 
                     primary = "#B29612",
                     secondary = clorox_lt_blue , 
                     info=clorox_yellow,
                     base_font = font_google("Lato"),
                     code_font = font_google("Lato"),
                     heading_font = font_google("Tinos"),
                   font_scale = 1.
          ),
      
               titlePanel(windowTitle = "CloroxPro" ,
                          fluidRow(
                            
                                     column(2,align="left",
                                            img( src="clorox.JPG",height="100px")
                                            ),
                                     column(8,align="center",
                                     h1("Cost Benefit to Adding Cleaning Time", align = "center")
                                      ),
                          )            
                          ),
    hr(style= paste("border-color:",clorox_yellow)),
               sidebarLayout(
                 
                 sidebarPanel(width = 4,
                   
                    fluidRow(
                      
                              column(6,
                                    verticalLayout( 
                                   #   numericInput(
                                  #      inputId = "V1",
                                  #      label = "Cost (in $) to treat a hospital-acquired C. diff infection",
                                  #      value = 12313
                                  #    ),
                                      
                                      numericInputIcon(
                                        inputId = "V1",
                                        label = "Cost (in $) to treat a hospital-acquired C. diff infection",
                                        value = 12313,
                                        icon = list(icon("dollar-sign"),NULL)
                                      ),
                                      
                                      
                                      numericInput(
                                        inputId = "PRnorm",
                                        label = "Base risk (in %) of acquiring a C. diff infection",
                                        value = 11,
                                      ),
                                      
                                      numericInput(
                                        inputId = "PRintv",
                                        label = "Risk (in %) of acquiring a C. diff infection after enhanced cleaning and disinfecting",
                                        value = 4.6,
                                      ),    numericInput(
                                        inputId = "Time",
                                        label = "Time (in minutes) to perform enhanced disinfection",
                                        value = 15,
                                      ),
                                      
                                      
                                      radioGroupButtons(
                                        inputId = "N_enhanced_peryear",
                                        label = "Number of times each year you expect to perform an enhanced clean with an electrostatic spray device",
                                        choiceNames = c("daily","weekly","bi-weekly","monthly"),
                                        selected=52,
                                        choiceValues = c(365,52,21,12)
                                      )
                                      
                                      )  
                                     ),
                              column(6,
                                     verticalLayout(
                                     numericInput(
                                       inputId = "Housekeeper_wage",
                                       label = "Housekeeper wage per hour (in $)",
                                       value = 12.89,
                                     ),
                                     
                                     numericInput(
                                       inputId = "Cleaning_standard",
                                       label = "Cost of supplies per standard room clean (in $)",
                                       value = 0.38,
                                     ),
                                     
                                     numericInput(
                                       inputId = "Cleaning_enhanced",
                                       label = "Cost of supplies per enhanced room clean (in $)",
                                       value = 0.81,
                                     ),
                                     numericInput(
                                       inputId = "Hospital_rent",
                                       label = "Cost per day (in $) for a patient to stay in hospital room",
                                       value = 2502,
                                     ),
                                     
                                     numericInput(
                                       inputId = "Device_cost",
                                       label = "Cost of electrostatic spray device (in $)",
                                       value = 4500,
                                       )
                                    ) 
                                     
                              )  
                    ) #end Row       
                              
                
                 ),
                 mainPanel(
                  
                   fluidRow(
                    column(6,
                           wellPanel(
                            
                             
                             uiOutput("results")
                           )
                    ),
                    
                    column(6,
                           wellPanel(
                             h3('A Title'),
                             tags$text("Some text describing the project")
                            
                            
                    )
                    )
                   )
                   
                 
               )
          
                          
 
),
hr(style= paste("border-color:",clorox_yellow))
)
  


      
      
        


server <- function(input, output, session){
 
  
  output$results <- renderUI({
 
    V1 <- input$V1
    PRnorm <- input$PRnorm
    PRintv <- input$PRintv
    Time <- input$Time
    Hospital_rent <- input$Hospital_rent
    Housekeeper_wage <- input$Housekeeper_wage
    Cleaning_standard <- input$Cleaning_standard
    Cleaning_enhanced <- input$Cleaning_enhanced
    Device_cost <- input$Device_cost
    N_enhanced_peryear <- as.numeric( input$N_enhanced_peryear )
    
    result <- ROI_calculator(V1 = V1, PRnorm = PRnorm, PRintv = PRintv, Time = Time, Hospital_rent = Hospital_rent, 
                             Housekeeper_wage = Housekeeper_wage, Cleaning_standard = Cleaning_standard, Cleaning_enhanced = Cleaning_enhanced,
                             Device_cost = Device_cost, N_enhanced_peryear = N_enhanced_peryear)
    
    str1 <- result[[1]]
    str2 <- result[[2]]
    v1 <- result[[3]]
    v2 <- result[[4]]
    
    tagList(
      valueBox( paste("$",v1),subtitle =paste("Cost Reduction /n", str1),width=12,
               color="green"
           ) ,
      valueBox( paste(v2,"%"), subtitle = paste("Infection Reduction", str2), width=12,
     color= "blue"
          )
    )
  })
  

}

shinyApp(ui = ui, server = server)
