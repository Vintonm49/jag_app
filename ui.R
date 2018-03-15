library(dplyr)



shinyUI(
  fluidPage(
    titlePanel("DEMO DATA - Military Justice Redesign Metrics - DEMO DATA"),
  tabsetPanel(
    tabPanel(
      title = "Overview",
      fluidRow(column(3, style="padding-top:50px;",
                      p(strong("This chart shows the average number of days that each stage in the Courts Martial process takes."))),
               column(9, style="padding-top:50px;border-style:solid; border-color:lightgray; width:800px;", 
                      plotOutput(outputId = "stage_days", width = "750px"))),
      fluidRow(column(3, style="padding-top:50px;",
                      p(strong("This chart shows the number of Courts Martial cases for each Installation."))),
               column(9, style="padding-top:50px;border-style:solid; border-color:lightgray; width:800px;", 
                      plotOutput(outputId = "inst_cases", width = "750px"))),
      fluidRow(column(3, style="padding-top:50px;",
                      p(strong("This chart shows the average time for each stage in the Courts Martial process by individual trial counsel."))),
               column(9, style="padding-top:50px;border-style:solid; border-color:lightgray;width:800px;", 
                      plotOutput(outputId = "stacked_tc", width = "750px")))
      
    ),
    tabPanel(
      title = "By Crime Type",
      fluidRow(column(3, style="padding-top:50px;",
                      p(strong("This chart compares the average time for Court Martial stages between General and Sex crimes."))),
               column(9, style="padding-top:50px;border-style:solid; border-color:lightgray;width:800px;", 
                      plotOutput(outputId = "stage_days_crime", width = "750px"))),
      fluidRow(column(3, style="padding-top:50px;",
                      p(strong("This chart compares the number of cases per installation between General and Sex crimes."))),
               column(9, style="padding-top:50px;border-style:solid; border-color:lightgray;width:800px;", 
                      plotOutput(outputId = "inst_cases_crime", width = "750px")))
    
      
      
      # verticalLayout(
      #   plotOutput(outputId = "stage_days_crime", width = "750px"),
      #   br(),
      #   br(),
      #   plotOutput(outputId = "inst_cases_crime", width = "750px")
      #   )
      ),
    
    tabPanel(title = "By Installation",
            
             sidebarLayout(
               sidebarPanel(
                 p("This chart shows the average number of days for the stages of the Court Martial process and other metrics, by installation.  
                    You can filter for the different stages and metrics.  You can also filter for type of case - completed or resolved.
                   The default is the average time for the total process, regardless of type of case."),
                 selectInput(inputId = "yAxis_inst", label = "Select Chart Topic", choices = list("Total Days - RoI to Resolution/Adjournment","RoI to Preferral","Preferral to Hearing",
                                                                                                 "Hearing to Referral", "Preferral to Referral",
                                                                                                 "Referral to Arraignment", "Arraignment to Resolution",
                                                                                                 "Arraignment to Adjournment", "Days in Court")),
                 selectInput(inputId = "case_type_inst", label = "Type of Cases", 
                             choices = list("Completed", "Resolved","Completed and Resolved"),
                             selected = "Completed and Resolved"),
                 checkboxInput("inst_number_check","See Number of Cases", value = FALSE)
               ),
               mainPanel(
                 h4("Average Number of Days for Stages of the Process by Installation"),
                 plotOutput(outputId = "myplot2", height = "500px"),
                 tableOutput("inst_number_table")
               )
             )
    ),
    tabPanel(title = "By Trial Counsel",
             sidebarLayout(
               sidebarPanel(
                 p("This chart can be used to compare individual trial counsels.  It shows how long on average a case assigned to 
                   that trial counsel takes, or you can filter by stage of the process to see the average time for that stage.
                   You can also filter by type of case - completed and resolved."),
                 selectInput(inputId = "yAxis", label = "Select Chart Topic", choices = list("Total Days - RoI to Resolution/Adjournment","RoI to Preferral","Preferral to Hearing",
                                                                                             "Hearing to Referral", "Preferral to Referral",
                                                                                             "Referral to Arraignment", "Arraignment to Resolution",
                                                                                             "Arraignment to Adjournment", "Days in Court")),
                 selectInput(inputId = "case_type", label = "Type of Cases", 
                             choices = list("Completed", "Resolved","Completed and Resolved"),
                             selected = "Completed and Resolved"),
                 checkboxInput("tc_number_check","See Number of Cases", value = FALSE)
               ),
               mainPanel(
                 plotOutput(outputId = "myplot", height = "500px"),
                 
                 tableOutput("tc_number_table")
                 
               )
             )
    )
  )
  
))



