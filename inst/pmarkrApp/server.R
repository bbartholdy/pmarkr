server <- function(input, output){
#----------------PMark calculator tab------------------
  output$vars1 <- renderText({
    req(input$up1)
    df <- read.table(input$up1$datapath,
                     header = input$header1,
                     sep = input$sep,
                     dec = input$dec)
    print(c("Variables: ", paste(names(df), sep = ", ", collapse = ", ")))
  })

  nres1 <- reactive({
    if(input$dat1 == "Upload..."){
      nrow(df) - 5
    } else {
      80
    }
  })
  output$nout1 <- renderText({nres1 <- nres1()
      nres1})

  vars2 <- reactive({
    if(input$dat1 == "Upload..."){
      df <- read.table(input$up1$datapath,
                       header = input$header1,
                       sep = input$sep,
                       dec = input$dec)
      paste(names(df), collapse = ", ")
    } else {
      paste(names(pmarkr::MBhum[,2:5]), collapse = ", ")
    }
  })
  output$vars2 <- renderText({vars2 <- vars2()
  vars2})

    mark2 <- eventReactive(input$calc1, {
              if(input$dat1 == "Upload..."){
                  inDat1 <- read.table(input$up1$datapath,
                               header = input$header1,
                               sep = input$sep1,
                               dec = input$dec1)
                  pmarkr:::PMark_shiny(formula = as.formula(input$form1), data = inDat1, n = input$n1, cut_p = input$prob1, iter = input$iter1, prior = c(input$priorF, input$priorM))

                } else {
                  #if MB11 is selected
                    #f <- input$form1
                    pmarkr:::PMark_shiny(formula = as.formula(input$form1), data = input$dat1, n = input$n1, cut_p = input$prob1, iter = input$iter1, prior = c(input$priorF, input$priorM))
                }
      })

    output$result1 <- renderText({ res <- mark2()
    paste("PMark: ", "+-", res$pmark)
    })
    output$plot1 <- renderPlot({ res2 <- mark2()
    res2$plot
    })

#----------------Sex estimation tab------------------
    est2 <- eventReactive(input$calc2, {
      pmarkr:::pred_sex(MaxL = input$maxL2, HeadD = input$headD2, EpiB = input$epiB2, prior = c(input$priorF2, input$priorM2))
    })
    output$logr2 <- renderText({pred_sex2 <- est2()
    paste(pred_sex2$sex[1], "with probability", pred_sex2$probability[1])
    })
    output$probit2 <- renderText({pred_sex2 <- est2()
    paste(pred_sex2$sex[2], "with probability", pred_sex2$probability[2])
    })
    output$lda2 <- renderText({pred_sex2 <- est2()
    paste(pred_sex2$sex[3], "with probability", pred_sex2$probability[3])
    })
    output$qda2 <- renderText({pred_sex2 <- est2()
    paste(pred_sex2$sex[4], "with probability", pred_sex2$probability[4])
    })

}
