library(shiny)
shortdata <- read.csv("./data/metro_short.csv")
shinyUI(navbarPage("FHFA HPI",
                   tabPanel("Historic HPI",
                            pageWithSidebar(
                                    headerPanel("FHFA House Price Index, Metro-area"),
                                    sidebarPanel(
                                            h3("Select Metro-area"),
                                            checkboxInput("Denver", "Denver", value = TRUE),
                                            checkboxInput("NewYork", "NewYork", value = TRUE),
                                            checkboxInput("Miami", "Miami", value = TRUE),
                                            checkboxInput("SanFrancisco", "SanFrancisco", value = TRUE),
                                            
                                            submitButton('Submit')
                                    ),
                                    mainPanel(
                                            plotOutput("plot1"))
                                                    
                                            )
                                    ),
                   tabPanel("Corr to Econ",
                   pageWithSidebar(
                           headerPanel("FHFA HPI Correlation to Macro-economy"),
                           sidebarPanel(
                                   h4("Select Metro-area, macroeconomic Indicator and its lag"),
                                   selectInput("metro", "Metro-Area", 
                                               choices=c("Denver","NewYork","Miami","SanFrancisco")),
                                   hr(),
                                   selectInput("econ", "Macro-economic Indicator:", 
                                               choices=c("RealGDP","CaseShiller_HPI","UnemplRate")),
                                   hr(),
                                   selectInput("lag", "Lag for Macro-economy (quarters)", 
                                               choices=c("0", "2", "3", "4", "6")),
                                   submitButton('Submit')
                           ),
                           mainPanel(
                                   h4("Metro-area HPI vs Macroeconomic Indicator"),
                                   hr(),
                                   textOutput("text"),
                                   hr(),
                                   plotOutput("plot2")
                        )
                   )
),
                   
                   tabPanel("Reference", a("User Guide",target="_blank",href="FHFA_HPI_app_UserGuide.pdf")
                   )
))

# library(rsconnect)
# rsconnect::deployApp('/Users/nikolaydobrinov/Documents/work/Courses/R/WorkDirectory/Course9_CourseProject/FHFA_HPI_app')
# Application successfully deployed to https://nikdobri.shinyapps.io/fhfa_hpi_app/