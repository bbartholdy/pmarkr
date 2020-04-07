ui <- fluidPage(
  tags$h1(
          tags$style(
            "h1 {
            font-family: Calibri (bold);
            font-weight: 400;
            line-height: 1.0;
            color: #9309bd;
            }")
  ),
  #tags$hr(),
  navbarPage("pmarkr",
#---------------------------------PMark Calculator tab---------------------------------
             tabPanel("PMark Calculator",
  #sidebarLayout(
    #sidebarPanel(width = 5,
  fluidRow(
    column(4,
           wellPanel(
                 tags$h4("Model settings"),
                 selectInput("dat1", "Select data", c("MB11", "Upload...")),
                 conditionalPanel(
                   condition = "input.dat1 == 'Upload...'",
                   helpText("Indicate whether or not your file columns have headers"),
                   checkboxInput("header1", "Header", TRUE),
                   fluidRow(
                     column(5, radioButtons("sep1", "Separator",
                                            choices = c(Comma = ",",
                                                        Semicolon = ";",
                                                        Tab = ""),
                                            selected = ",")),
                     column(4, radioButtons("dec1", "Decimal",
                                            choices = c(Comma = ",",
                                                        Point = "."),
                                            selected = ","))
                   ),
                   fileInput("up1", "Upload file", buttonLabel = "Browse",
                             multiple = FALSE,
                             accept = c("text/csv",
                                        "text/comma-seperated-values, text/plain",
                                        ".csv")),
                   tags$hr()
                 ),
        p("Variables:"),
        verbatimTextOutput("vars2"),
        textInput("form1", "Insert formula", placeholder = "Example: Sex ~ HeadD + EpiB"),
        checkboxInput("prior1", "Set prior probability", value = FALSE),
        conditionalPanel(
          condition = "input.prior1 == true",
        numericInput("priorF", "Female", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("priorM", "Male", value = 0.5, min = 0, max = 1, step = 0.01),
        verbatimTextOutput("priors1")
        ),
        sliderInput("prob1", "PMark probability", value = 0.8, min = 0.01, max = 0.99, step = 0.01)
  ),
           wellPanel(
        tags$h4("Resampling settings"),
        numericInput("n1", "Subsample size", value = 80),
        p("Recommended size:"),
        verbatimTextOutput("nout1"),
        numericInput("iter1", "Iterations", 500),
        tags$hr(),
        actionButton("calc1", "Calculate")
          )
  ),

    #),
  #mainPanel(width = 6,
  column(8,
            textOutput("group1"),
            textOutput("group2"),
            tags$br(),
            plotOutput("plot1"),
            tags$style("#group1{
                          color: #fc322b;
                       font-size: 22px;
                       }"),
            tags$style("#group2{
                          color: #02d6d6;
                       font-size: 22px;
                       }")
        )
)
),
#---------------------------------Sex Estimation tab---------------------------------
tabPanel("Sex Estimation",
         sidebarLayout(
                      sidebarPanel(width = 4,
                                   tags$h4("Variables"),
                                   checkboxInput("length2", "Max humeral length", value = FALSE),
                                    conditionalPanel(
                                      condition = "input.length2 == true",
                                      numericInput("maxL2", label = "length (mm)", value = NA)
                                    ),

                                   checkboxInput("head2", "Humeral head diameter", value = FALSE),
                                    conditionalPanel(
                                      condition = "input.head2 == true",
                                      numericInput("headD2", label = "diameter (mm)", value = NA)
                                   ),
                                   checkboxInput("epi2", "Humeral epicondylar breadth", value = FALSE),
                                    conditionalPanel(
                                      condition = "input.epi2 == true",
                                      numericInput("epiB2", label = "breadth (mm)", value = NA)
                                   ),
                                   checkboxInput("prior2", "Set prior probability", value = FALSE),
                                   conditionalPanel(
                                     condition = "input.prior2 == true",
                                     numericInput("priorF2", "Female", value = 0.5, min = 0, max = 1, step = 0.01),
                                     numericInput("priorM2", "Male", value = 0.5, min = 0, max = 1, step = 0.01)
                                   ),
                                   actionButton("calc2", "Estimate")

                      ),
                      mainPanel(width = 8,
                                tags$h3("Logistic regression"),
                                    textOutput("logr2"),
                                    tags$br(),
                                tags$h3("Probit"),
                                    textOutput("probit2"),
                                    tags$br(),
                                tags$h3("Linear Discriminant Analysis"),
                                    textOutput("lda2"),
                                    tags$br(),
                                tags$h3("Quadratic Discriminant Analysis"),
                                    textOutput("qda2"),
                                tags$style(
                                  "h4 {
                                  font-family: Calibri (bold);
                                  font-weight: 400;
                                  line-height: 1.0;
                                  color: #1b8bd1;
                                  }"),
                                tags$style(
                                  "#logr2 {
                                  color: #9309bd;
                                  font-size: 18px;
                                  }"),
                                tags$style(
                                  "#probit2 {
                                  color: #9309bd;
                                  font-size: 18px;
                                  }"),
                                tags$style(
                                  "#lda2 {
                                  color: #9309bd;
                                  font-size: 18px;
                                  }"),
                                tags$style(
                                  "#qda2 {
                                  color: #9309bd;
                                  font-size: 18px;
                                  }")

                      )

         )
         ),
#---------------------------------Help tab---------------------------------
tabPanel("Help",
         tags$h3("Select data"),
         tags$h4("First, select the desired dataset for which the PMark(s) should be calculated. This can either be the Middenbeemster dataset (“MB11”) or an uploaded dataset (“Upload…”). If “Upload…” is selected, additional options will appear. The uploaded file should be a .txt file (similar to the MBhum file).
                 Indicate if the file contains headers (highly recommended); if the columns are separated by commas, semicolons, or tabs; and if the decimals are represented as commas or points (if the data do not contain decimals, leave as is). Once the settings have been set, click “Browse” to upload your file."),
         tags$h3("Insert formula"),
         tags$h4("The variables used to calculate the PMarks must be entered into the formula. This is done by inserting them into the app as a formula, with the response variable (e.g. sex) on the left, and the predictor variables on the right (e.g. humeral measurements).
                  The response variable should be separated from the predictor variables with a tilde (~). Predictor variables should be separated by a plus (+) symbol. Example (will work for the MB11 dataset): Sex ~ HeadD + EpiB"),
         tags$h4("Variable names in the formula must match the variable names in the dataset (case sensitive). The names of the variables can be found in the variables box above the formula box."),
         tags$h3("Prior probability"),
         tags$h4("If selected, allows you to specify the prior probability. Both values must add up to 1. If not selected,  the prior defaults to an uninformative prior, i.e. c(0.5, 0.5)"),
         tags$h3("PMark probability"),
         tags$h4("This specifies the location of the cut-off points based on posterior probabilities. The recommended probability is 0.8, 0.95, or 0.99."),
         tags$h3("Subsample size"),
         tags$h4("This determines the size of the subsample drawn from the main dataset for bootstrapping. It must be smaller than the main dataset. Recommended is N - 5."),
  textOutput("help1")
)
)
)

#)
