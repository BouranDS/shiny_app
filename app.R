#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
base::source("test_file.R")
#~/scepticemie
# Define my shiny project to visualize scepsis data from Mali
ui <- navbarPage(title = "FMOS",
                 shiny::tabPanel("About",
                                 shiny::fluidRow(
                                     shiny::column(9, 
                                                   h1("Presentation"),
                                                   hr(),
                                                   h2("Data"),
                                                   p(
                                                       "The data are from a medical survey."),
                                                   p("The data observed the evolution 
                                                   and the drawback of",span("scepticaemia",style = "color:blue"),
                                                                             "disease in Bamako throughout of some cases at CNAM."),
                                                   p("The data were collected by", span("Mrs. Doumbouya", style="color:blue"),
                                                   "during her thesis"),
                                                   hr(),
                                                   h2("Septicemia"),
                                                   p("Septicemia stands for", span("systemic", style = "color:blue"),
                                                     "(bodywide) illness with", span("toxicity", style = "color:blue"),
                                                     "due to invasion of the bloodstream by",  span("virulent bacteria",
                                                     style = "color:blue"),"coming from a local site of", 
                                                     span("infection", style = "color:blue")),
                                                   hr(),
                                                   h2("survey Detail"),
                                                   tags$ul(
                                                       tags$li("Area: CNAM at Bamako"),
                                                       tags$li("When: from 2010 to 2015"),
                                                       tags$li("By: Mrs Doumbouya")
                                                   ),
                                                   
                                                   
                                                   hr(), 
                                                   offset = 3)
                                 ),
                                 shiny::fluidRow(
                                     shiny::column(3,
                                                   img(src="logocentre.png", height=50, width=20)
                                                   , offset = 3),
                                     shiny::column(3,
                                                   img(src="usttb.png", height=50, width=20)
                                     ),
                                     shiny::column(3,
                                                   h5("Centre National", style = "color:blue"),
                                                   h5("de Lutte Contre", style = "color:blue"),
                                                   h5("les Maladies", style = "color:blue")
                                     )
                                     
                                 )
                                 ),   
    shiny::tabPanel("Data",
                    fluidRow(
                        column(3,
                shiny::h4("Information", style = "color:blue"),       
                shiny::helpText("Herein, some widgets are available in order to make 
                                mapping of the data table easy."),
                # p("Filter can be use on several variables. Such as:"),
                # tags$ul(
                #     tags$li("Number of rows"),
                #     tags$li("Date range of observation"),
                #     tags$li("Age or age range of patiente"),
                #     tags$li("Sexe: Female or Male"),
                #     tags$li("or other")
                # ),
                            
                hr(),
                shiny::numericInput(
                    "ligneNum", h4("Number of rows", style="color:blue"),
                    value = 10,
                    min = 1,
                    max = nrow(new_scepticemie),
                    step = 1
                ), 
                shiny::sliderInput(
                    "ageInclude",h4("Year range",style="color:blue"),
                    min = base::min(new_scepticemie$AGE_ANS, na.rm = TRUE), 
                    max = base::max(new_scepticemie$AGE_ANS, na.rm = TRUE),
                    value = c(0,base::max(new_scepticemie$AGE_ANS, na.rm = TRUE)/5)
                ),
                
                
                shiny::selectInput("sexeChoice",
                                   h4("Sexe", style = "color:blue"),
                                   choices = sexe
                                   ,
                                   selected = sexe["All"]),
                shiny::checkboxGroupInput(
                    "origin", 
                    h4("Origin of patient", style="color:blue"),
                    choices = mode,
                    selected = mode
                    ),
                
                shiny::radioButtons(
                    "showText", h4("Show details on table",style="color:blue"),
                    choices = list("Yes" = 1, "No" = 2),selected = 1
                ),
                               
                hr(),
                            shiny::actionButton("Compute", "Valide"),
                hr()
                        ),
                        column(9,
                               shiny::dataTableOutput("tableau")
                               )
                        
                    ),
                    fluidRow(
                        column(6,
                               shiny::verbatimTextOutput("infoTable")
                        ),
                        column(3,
                               p("copyright--@IBS@--", style="color:blue"
                               )
                    )
    )
    ),
    shiny::tabPanel("Analysis",
                    shiny::fluidRow(
                        shiny::column(
                            6,
                            h5("Univariate Analysis", style = "font-size:20px;color:blue"),
                       offset = 6),
                        fluidPage(
                            shiny::sidebarLayout(
                                shiny::sidebarPanel(
                                  h5("Univariate by use of selection",
                                     style = "color:blue; family:serif; font:italic"),
                                  p("A select Input is available above in order to 
                                    permit choosing among variable name", 
                                    style = " color:black; font:italic"),
                                  hr(),
                                  shiny::selectInput("variableChoice",
                                                     h5("Variable", style = "color:blue"),
                                                     choices = mes_variable
                                                     ,
                                                     selected = mes_variable["Year"]),
                                  hr(),
                                  p('Accordingly, an approprieted analysis is performed',
                                    style = "color:black; font-style:italic")
                                ),
                                shiny::mainPanel(
                                    
                                    shiny::verbatimTextOutput(
                                        "unistat"                                                             )
                                    
                                )
                            )
                            
                        )
                    ),
                    shiny::fluidRow(
                        shiny::column(
                            6,
                            h5("Bivariate Analysis", style = "font-size:20px;color:blue"),
                            offset = 6),
                        fluidPage(
                            shiny::sidebarLayout(
                                shiny::sidebarPanel(
                                    h5("Herein, a bivariate can be performed by using selection",
                                       style = "color:blue; family:serif; font:italic"),
                                    p("Two select Input are available below in order to 
                                    permit choosing among variable name", 
                                      style = " color:black; font-style:italic"),
                                    hr(),
                                    shiny::selectInput("variableChoiceX",
                                                       h5("Variable X", style = "color:blue"),
                                                       choices = mes_variable
                                                       ,
                                                       selected = mes_variable["Sexe"]),
                                    shiny::selectInput("variableChoiceY",
                                                       h5("Variable Y", style = "color:blue"),
                                                       choices = mes_variable
                                                       ,
                                                       selected = mes_variable["Recruitement_mode"]),
                                    hr(),
                                    p('A suitable analysis is performed
                      according to the type of selected variables',
                                      style = "color:black; font-style:italic")
                                ),
                                shiny::mainPanel(
                                    shiny::verbatimTextOutput(
                                        "unistat_2"                                                             )
                                ),
                            position = "left")
                            
                        )
                    
                    ) 
    ),
    #########

   
    
    ########
    shiny::tabPanel("Plotting",
                    shiny::fluidRow(
                        shiny::column(4,
                                      h4("Plot system:", style = "color:blue;
                                         font-size:bold;font-family:serif"),
                                      p("Several graphics systems are used such as:"),
                                      shiny::tags$ul(
                                          tags$li("R base plot"),
                                          tags$li("ggplot2"),
                                          tags$li("plotly")
                                      ),
    #                                  hr()
                                      ),
                        shiny::column(8,
                                      h4("Graph : ", style = "color:blue;
                                         font-size:bold;font-family:serif"),
                                      
                        shiny::tabsetPanel(
                            shiny::tabPanel(title = "Univariate Num",
                                            shiny::fluidRow(
                                                column(5,
                                                      shiny::radioButtons(
                                                          "checknum",
                                                          label = "Geometry: Numerical",
                                                          choices = c("histogram"="histogram",
                                                                      "density" = "density",
                                                                      "boxplot" = "boxplot"),
                                                          selected = "histogram", inline = TRUE
                                                      )
                                                      ),
                                                      
                                                      column(3,
                                                             shiny::selectInput("plotnum",
                                                                                "Numerical",
                                                                                choices = mes_variable[select_type],
                                                                                selected = mes_variable[select_type][2]),
                                                      
                                                      offset = 3)
                                            
                                            ),
                                            fluidRow(
                                           
                                            shiny::plotOutput("simplegraphN")
                                            )
                            
                                            ),
                            shiny::tabPanel(title = "Univariate Cat",
                                            shiny::fluidRow(
                                                column(5,
                                                       shiny::radioButtons(
                                                           "checkcat",
                                                           label = "Geometry: Category",
                                                           choices = c("barplot H" = "barplot",
                                                                       "barplot V" = "barplotother",
                                                                       "circular" = "circular"),
                                                           selected = "barplot", inline = TRUE
                                                       )
                                                       ),
                                                       column(3,
                                                              shiny::selectInput("plotcat",
                                                                                 "Categorical",
                                                                                 choices = mes_variable[!select_type],
                                                                                 selected = mes_variable[!select_type][1]),
                                                              
                                                       
                                                offset = 3)
                                            ),
                                            fluidRow(
                                        
                                                    # shiny::verbatimTextOutput('mytest'),
                                                    shiny::plotOutput("simplegraphC")  
                                                )
                            
                                            ),
                            
                            shiny::tabPanel(title = "Bivariate",
                                            shiny::fluidRow(
                                              column(3,
                                                     shiny::selectInput("selectbiX",
                                                                        "Variable X",
                                                                        choices = mes_variable,
                                                                        selected = mes_variable[1])
                                                     ),
                                              column(3,
                                                     shiny::selectInput("selectbiY",
                                                                        "Variable Y",
                                                                        choices = mes_variable,
                                                                        selected = mes_variable[1])
                                                     )
                                       
                                            ),
                                            shiny::fluidRow(
                                              column(2,
                                                     shiny::actionButton("bivariate", "Draw Figure")
                                              )),
                                            shiny::fluidRow(
                                              
                                              shiny::plotOutput("doublegraph")
                                              
                                            )
                                            )
                                            )
                                            )
                                            ) #plotting
    ),
  
    position = "static-top", inverse = FALSE
)



# Define server logic 
#
# scepticemie <- 
#     utils::read.csv2(
#         file = "D:/MyR/scepticemie/sceptiemie_Donnee.csv", 
#         header = TRUE, stringsAsFactors = FALSE
#     )
# columIndex <- base::colnames(
#     scepticemie
# )
# new_scepticemie 
server <- function(input, output) {
    data_select <- shiny::eventReactive(input$Compute,
                                        {
                # row number # ageInclude  origin
                    n_row <- input$ligneNum                     
                # range of age
                    range_age <- input$ageInclude  
                    # print(range_age)
                # sexe
                    sexe_sel <- input$sexeChoice
                    
                # origin of patient
                    origin_patient <- input$origin
                
                    # print(origin_patient)
                    test_age <- new_scepticemie$AGE_ANS> range_age[1] & 
                                new_scepticemie$AGE_ANS<= range_age[2]
                    data_choisi <- new_scepticemie[test_age,]
                    if(sexe_sel == "Feminin"){
                        data_choisi <- data_choisi[data_choisi$sexe == "Feminin",]
                    }else if(sexe_sel == "Masculin"){
                        data_choisi <- data_choisi[data_choisi$sexe == "Masculin", ]
                    }
                     if(length(origin_patient)==1){
                         data_choisi <- data_choisi[data_choisi$Modederecrutement == origin_patient, ]
                     }
                    #     data_choisi <- data_choisi[data_choisi$sexe == "Feminin"]
                    # }else if(sexe_sel == "Masculin"){
                    #     data_choisi <- data_choisi[data_choisi$sexe == "Feminin"]
                    # }
                    data_choisi[1:n_row,]
                    
                                        }
        
    )
    # data_select <- shiny::reactive(
    #     {
    #         new_scepticemie
    #     }
    # )
      #  head(new_scepticemie)
    output$tableau <- shiny::renderDataTable(
        {
            data_select()
        },
            options =list(
                scrollX = TRUE
                
            )
            
            #new_scepticemie
       
    )
    my_text <- shiny::eventReactive(input$Compute,
                                    {
                                        range_age      <- input$ageInclude 
                                        origin_patient <- input$origin
                                        sexe_sel       <- input$sexeChoice
                                        n_row          <- input$ligneNum  
                                        
                                    base::paste(
                                            paste0("Year range : ","\n", "min = ",
                                                   range_age[1],"\t",
                                                   "max = ", range_age[2]),
                                            
                                            ifelse(sexe_sel=="Tous","Sexe : Female & Male",
                                                   paste0("Sexe :",
                                                          ifelse(sexe_sel=="Feminin",
                                                                 "Female only",
                                                                 "Male only"))),
                                            ifelse(length(origin_patient) == 2,
                                                   "Origin : Urgence & Consultation",
                                                   paste0("Origin : ",origin_patient)),
                                            paste0("Number of lines : ", base::toString(n_row)),
                                            sep = "\n")
                                        
                                        
                                    }
    )
    output$infoTable <- shiny::renderPrint(
        base::cat(
            my_text()
            )
    )
    # 
    unistatvar <- shiny::reactive(
        input$variableChoice
    )
    output$unistat <- renderPrint({
        if(base::is.numeric(
            new_scepticemie[,unistatvar()])
           ){
            base::print(
            base::summary(new_scepticemie[,unistatvar()])
            )
        }else{
            base::print(
            functionCatvariable(var_name = unistatvar(),
                                dataframe = new_scepticemie)
            )
            }
    })
    unistatvarX <- shiny::reactive(
        input$variableChoiceX
    )
    unistatvarY <- shiny::reactive(
        input$variableChoiceY
    )
    output$unistat_2 <- renderPrint({
        #shiny::isolate(
        functionBivariable(var_nameX=unistatvarX(),
                           var_nameY=unistatvarY(), 
                           dataframe=new_scepticemie)
       # )
    })
    # plotting 
    
    geometrynum <- shiny::reactive(
        input$checknum
    )

    
    vartoplotnum <- shiny::reactive(
        
        input$plotnum
    )

    output$simplegraphN <- renderPlot(
        plotnum(
            vartoplot = vartoplotnum(),
            dataframe = new_scepticemie,
            typeofgeom = geometrynum()
        )
        
    )
    
#  
    
    geometrycat <- shiny::reactive(
        input$checkcat
    )
    vartoplotcat <- shiny::reactive(
        
        input$plotcat
    )
    # output$mytest <- shiny::renderPrint(
    #     vartoplotcat()
    # )
    
    output$simplegraphC <- renderPlot(
        plotcat(
            vartoplot = vartoplotcat(),
            dataframe = new_scepticemie,
            typeofgeom = geometrycat()
        )
    )
    
    # 
    shiny::observeEvent(input$bivariate,
      {
        varx <- input$selectbiX
        vary <- input$selectbiY
        output$doublegraph<- renderPlot(
          doubleplot(varx,vary, new_scepticemie)
        )
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
