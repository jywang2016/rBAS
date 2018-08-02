#' Run shiny interface for BAS/BSAS algorithms
#'
#' @description `run_BAS_App` is built based on `shiny` and `shinythemes`. All you have to do is building your objective function
#' in R code. Then, you can change the parameters in Shiny interface and run BAS/BSAS algorithm.
#' @param func objective function; you need build `func` in R code.
#' @param theme shiny interface themes; `theme` should be one of c("cerulean","cosmo","cyborg","darkly","flatly",
#' "journal","lumen","paper","readable","sandstone","simplex","slate","spacelab","superhero","united","yeti")
#' @import shiny
#' @import shinythemes
#' @import ggplot2
#' @importFrom reshape2 melt
#' @examples
#' #======== examples start =======================
#' # BSAS application on Michalewicz function
#' library(rBAS)
#' mich <- function(x){
#'    y1 <- -sin(x[1])*(sin((x[1]^2)/pi))^20
#'    y2 <- -sin(x[2])*(sin((2*x[2]^2)/pi))^20
#'    return(y1+y2)
#' }
#' #run_BAS_App(func = mich, theme = 'united')
#' #======== examples end =======================
#' @export
run_BAS_App <- function(func, theme){
  shinyApp(
    ui = tagList(
      #shinythemes::themeSelector(),
      navbarPage(
        theme = shinytheme(theme),
        title = "Shiny interface for BAS algorithms",
        tabPanel("BAS",
                 sidebarPanel(
                   #report
                   #textInput(inputId = "txt", label = "Report Title for markdown :",
                   #          value = "Report test1"),
                   #parms
                   #parm.init
                   textInput('init',label = 'Initial parameters-init:',value = ' '),
                   #parms.lower
                   textInput(inputId = 'lower',label = 'Lower of params-lower:',value = '-6,0'),
                   #parms.upper
                   textInput(inputId = 'upper',label = 'Upper of params-upper:',value = '-1,2'),
                   #parms.d0
                   numericInput('d0',label = 'Smallest antenna length-d0:',value = 0.001),
                   #parms.d1
                   numericInput('d1',label = 'Initial antenna length-d1:',value = 3),
                   #parms.eta_d
                   numericInput('eta_d',label = 'Attenuation constant-eta_d:',value = 0.95),
                   #parms.step
                   numericInput('step',label = 'Beetle step length-step:',value = 0.8),
                   #parms.eta_step
                   numericInput('eta_step',label = 'Attenuation constant for step-eta_step:',value = 0.95),
                   #parms.n
                   numericInput('n',label = 'Iteration numbers-n:',value = 200),
                   #parms.seed
                   numericInput('seed',label = 'Random seed-seed:',value = 1),
                   #parms.trace
                   #parms.steptol
                   numericInput('steptol',label = 'Step tolerance-steptol:',value = 0.01),
                   #action
                   actionButton("action", "Run BAS", class = "btn-primary")
                 ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel(title = 'Optimization Progress',
                              h3("Optimization Progress:"),
                              verbatimTextOutput("progress")
                     ),
                     tabPanel(title = 'Optimization Parameters',
                              h3("Optimization Parameters:"),
                              verbatimTextOutput("params")
                     ),
                     tabPanel(title = 'Progress Plot',
                              h3('Progress Plot:'),
                              plotOutput('plot')
                     )
                   )


                 )
        ),
        tabPanel("BSAS",
                 sidebarPanel(
                   #report
                   #textInput(inputId = "txt", label = "Report Title for markdown :",
                   #          value = "Report test1"),
                   #parms
                   #parm.init
                   textInput('init2',label = 'Initial parameters-init:',value = ' '),
                   #parms.lower
                   textInput(inputId = 'lower2',label = 'Lower of params-lower:',value = '-6,0'),
                   #parms.upper
                   textInput(inputId = 'upper2',label = 'Upper of params-upper:',value = '-1,2'),
                   #parms.d0
                   numericInput('d02',label = 'Smallest antenna length-d0:',value = 0.001),
                   #parms.d1
                   numericInput('d12',label = 'Initial antenna length-d1:',value = 3),
                   #parms.eta_d
                   numericInput('eta_d2',label = 'Attenuation constant-eta_d:',value = 0.95),
                   #parms.step
                   numericInput('step2',label = 'Beetle step length-step:',value = 0.8),
                   #parms.eta_step
                   numericInput('eta_step2',label = 'Attenuation constant for step-eta_step:',value = 0.95),
                   #parms.n
                   numericInput('n2',label = 'Iteration numbers-n:',value = 100),
                   #parms.seed
                   numericInput('seed2',label = 'Random seed-seed:',value = 12),
                   #parms.trace
                   #parms.steptol
                   numericInput('steptol2',label = 'Step tolerance-steptol:',value = 0.01),
                   #parms.k
                   numericInput('k2',label = 'Number of beetles-k:',value = 5),
                   #parms.p_min
                   numericInput('p_min2',label = 'p_min(in [0,1)):',value = 0.2),
                   #parms.p_step
                   numericInput('p_step2',label = 'p_step(in [0,1)):',value = 0.2),
                   #parms.n_flag
                   numericInput('n_flag2',label = 'n_flag(positive integer):',value = 2),
                   #action
                   actionButton("action2", "Run BSAS", class = "btn-primary")
                 ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel(title = 'Optimization Progress',
                              h3("Optimization Progress:"),
                              verbatimTextOutput("progress2")
                     ),
                     tabPanel(title = 'Optimization Parameters',
                              h3("Optimization Parameters:"),
                              verbatimTextOutput("params2")
                     ),
                     tabPanel(title = 'Progress Plot',
                              h3('Progress Plot:'),
                              plotOutput('plot2')
                     )
                   )


                 )
        ),
        tabPanel("BAS-WPT", "Under development..."),
        tabPanel(title = "Authors",
                 uiOutput('aut')
        )
      )
    ),
    server = function(input, output,session) {
      # value
      value <- reactiveValues()

      # authors
      url1 <- a('Jiangyu Wang', href = 'https://github.com/jywang2016')
      url2 <- a('Shuai Li', href = 'http://www4.comp.polyu.edu.hk/~cssli/')
      output$aut <- renderUI({
        tagList('Authors:',url1,' & ',url2)
      })

      # BAS
      progress_reactive <- eventReactive(input$action,{
        if(input$init == ' '){
          init <- NULL
        }else{
          init <- as.numeric(unlist(strsplit(input$init,",")))
        }
        if(length(input$lower)!=0){
          lower <- as.numeric(unlist(strsplit(input$lower,",")))
        }
        if(length(input$upper)!=0){
          upper <- as.numeric(unlist(strsplit(input$upper,",")))
        }
        result <- BASoptim(fn = func, init = init,
                           lower = lower,
                           upper = upper,
                           d0 = input$d0, d1 = input$d1, eta_d = input$eta_d,
                           step = input$step, eta_step = input$eta_step,
                           n = input$n, steptol = input$steptol,
                           seed = input$seed)
        value[['result']] <- result
      })

      output$progress <- renderPrint({
        progress_reactive()
      })
      output$params <- renderPrint({
        c(value[['result']][1],value[['result']][2])
      })
      output$plot <- renderPlot({
        df_f <- value[['result']]$df$f
        df_fb <- value[['result']]$df$fbest
        plot_d <- data.frame(Iter = 1:length(df_f),
                             f = df_f,
                             fbest = df_fb)
        plot_d2 <- melt(data = plot_d,id = 'Iter')
        ggplot(plot_d2,aes(x = Iter, y = value, color = variable)) +
          geom_line(size = 1,alpha = 0.8)+
          geom_point()+
          theme_bw()+
          theme(
            legend.position = c(0.9,0.8)
          )+
          labs(x = 'Iterations',y = 'Objective function values',color = 'Results')
      })

      # BSAS
      progress_reactive2 <- eventReactive(input$action2,{
        if(input$init2 == ' '){
          init2 <- NULL
        }else{
          init2 <- as.numeric(unlist(strsplit(input$init2,",")))
        }
        if(length(input$lower2)!=0){
          lower2 <- as.numeric(unlist(strsplit(input$lower2,",")))
        }
        if(length(input$upper2)!=0){
          upper2 <- as.numeric(unlist(strsplit(input$upper2,",")))
        }
        result2 <- BSASoptim(fn = func, init = init2,
                             lower = lower2,
                             upper = upper2,
                             d0 = input$d02, d1 = input$d12, eta_d = input$eta_d2,
                             step = input$step2, eta_step = input$eta_step2,
                             n = input$n2, steptol = input$steptol2,
                             seed = input$seed2,
                             p_min = input$p_min2, p_step = input$p_step2,
                             n_flag = input$n_flag2,
                             k = input$k2)
        value[['result2']] <- result2
      })

      output$progress2 <- renderPrint({
        progress_reactive2()
      })
      output$params2 <- renderPrint({
        c(value[['result2']][1],value[['result2']][2])
      })
      output$plot2 <- renderPlot({
        df_f <- value[['result2']]$df$f
        plot_d <- data.frame(Iter = 1:length(df_f),
                             f = df_f)
        ggplot(plot_d,aes(x = Iter, y = f)) +
          geom_line(size = 1,alpha = 0.8,color = 'steelblue')+
          geom_point(alpha = 0.6, color = 'steelblue')+
          theme_bw()+
          labs(x = 'Iterations',y = 'Objective function values')
      })
    }
  )
}
