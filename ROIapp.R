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
    
    ## Clorox Theme
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
                                            tags$a(img( src="clorox.JPG",height="100px"),
                                                   href="https://www.cloroxpro.com", target="_blank")
                                            
                                            ),
                                     column(8,align="center",
                                     h1("Cost Benefit of an Enhanced Disinfection Step to Prevent Hospital Aquired Infections", align = "center")
                                      ),
                          )            
                          ),
                 hr(style= paste("border-color:",clorox_yellow)),
               sidebarLayout(
                 
                 sidebarPanel(width = 4,
                   
                    fluidRow(
                      
                              column(6,
                                   
                                   #   numericInput(
                                  #      inputId = "V1",
                                  #      label = "Cost (in $) to treat a hospital-acquired C. diff infection",
                                  #      value = 12313
                                  #    ),
                                      # 
                                      
                                      awesomeRadio(
                                        inputId = "i_type",
                                        label = "Pathogen", 
                                        choices = c("C. diff","VRE","MRSA"),
                                        selected = "C. diff",
                                        inline = TRUE, 
                                        status = "success"
                                      ),
                                      
                                      hr(),
                                       # numericInputIcon(
                                       #   inputId = "V1",
                                       #   label = "Cost (in $) to treat a hospital-acquired C. diff infection",
                                       #   value = 12313,
                                       #   icon = list(icon("dollar-sign"),NULL)
                                       # ),
                                       
                                    
                                      # numericInput(
                                      #   inputId = "PRnorm",
                                      #   label = "Base risk (in %) of acquiring a C. diff infection",
                                      #   value = 11,
                                      # ),
                                      # 
                                      # numericInput(
                                      #   inputId = "PRintv",
                                      #   label = "Risk (in %) of acquiring a C. diff infection after enhanced cleaning and disinfecting",
                                      #   value = 4.6,
                                      # ),
                                      # 
                                      
                                      numericInputIcon(
                                        inputId = "Time",
                                        label = "Total time for electrostatic spraying per patient room",
                                        value = 15,
                                        icon = list(NULL,"Minutes")
                                      ),
                                      
                                     hr(),
                                      radioGroupButtons(
                                        inputId = "N_enhanced_per_year",
                                        label = "Frequency of electrostatic sprayer use in patient rooms",
                                        choiceNames = c("daily","weekly","monthly","custom"),
                                        selected=52,
                                        choiceValues = c(365,52,12,0)
                                      ),
                                      
                                      uiOutput("custom_period")
                                      
                                      
                                     ),
                                    
                              column(5,offset=1,
                                     verticalLayout(
                                       numericInputIcon(
                                       inputId = "Housekeeper_wage",
                                       label = "EVS Wage per hour",
                                       value = 12.89,
                                       icon = list(icon("dollar-sign"),NULL)
                                     ),
                                     
                                     # numericInputIcon(
                                     #   inputId = "V1",
                                     #   label = "Cost (in $) to treat a hospital-acquired C. diff infection",
                                     #   value = 12313,
                                     #   icon = list(icon("dollar-sign"),NULL)
                                     # ),
                                     
                                     
                                     numericInputIcon(
                                       inputId = "Cleaning_standard",
                                       label = "Standard cleaning cost per room ",
                                       value = 0.38,
                                       icon = list(icon("dollar-sign"),NULL)
                                     ),
                                     
                                     numericInputIcon(
                                       inputId = "Cleaning_enhanced",
                                       label = "Electochemistry cost per patient room",
                                       value = 0.81,
                                       icon = list(icon("dollar-sign"),NULL)
                                     ),
                                     numericInputIcon(
                                       inputId = "Hospital_rent",
                                       label = "Average patient cost per day",
                                       value = 2502,
                                       icon = list(icon("dollar-sign"),NULL)
                                     ),
                                     
                                     numericInputIcon(
                                       inputId = "Device_cost",
                                       label = "Cost of electrostatic sprayer",
                                       value = 4500,
                                       icon = list(icon("dollar-sign"),NULL)
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
hr(style= paste("border-color:",clorox_yellow)),

h6(footnote)
)
  


      
      
        


server <- function(input, output, session){
 
  
  output$custom_period <- renderUI({
    
    
    if( input$N_enhanced_per_year != 0) {
      
      tagList(
      NULL
      )
      
    }else{
      tagList(
      fluidRow(
        column(6,
               numericInput("cust_freq",label = "Frequency",value = 1,min = 1,max = 10)
               ),
        column(6,
               selectInput("cust_period",label = "Period",choices = c("daily" = 365 ,"weekly" = 52 ,"monthly" = 12 ))
               )
        

        )
      )
      
      
      
    }
    
    
    
    
  })
  
  
cost <- reactive({
  
  path_parms%>% dplyr::filter(Pathogen ==  input$i_type) %>% pull(cost)
  
  
  
})

base_risk <- reactive({
  
  path_parms %>% dplyr::filter(Pathogen ==  input$i_type) %>% pull(base_risk)
  
})
  
  
improve_risk <- reactive({
  
  path_parms %>% dplyr::filter(Pathogen ==  input$i_type) %>% pull(improve_risk)
  
})

cleaning_per_year <- reactive({
  

  
if (input$N_enhanced_per_year != "0") {
  
  return(as.numeric(input$N_enhanced_per_year))
}else{
  
  return( ( input$cust_freq * as.numeric(input$cust_period)) )
  
}
  
  
})  

  
  output$results <- renderUI({
 
  req( cost(),improve_risk(),cleaning_per_year())
    
    V1 <- cost()
    PRnorm <- base_risk()
    PRintv <- improve_risk()
    Time <- input$Time
    Hospital_rent <- input$Hospital_rent
    Housekeeper_wage <- input$Housekeeper_wage
    Cleaning_standard <- input$Cleaning_standard
    Cleaning_enhanced <- input$Cleaning_enhanced
    Device_cost <- input$Device_cost
    N_enhanced_per_year <- cleaning_per_year()
    
    result <- ROI_calculator(V1 = V1, PRnorm = PRnorm, PRintv = PRintv, Time = Time, Hospital_rent = Hospital_rent, 
                             Housekeeper_wage = Housekeeper_wage, Cleaning_standard = Cleaning_standard, Cleaning_enhanced = Cleaning_enhanced,
                             Device_cost = Device_cost, N_enhanced_per_year = N_enhanced_per_year)
    
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
