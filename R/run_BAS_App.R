#' Run shiny interface for BAS/BSAS algorithms
#'
#' @description `run_BAS_App` is built based on `shiny` and `shinythemes`. All you have to do is building your objective function
#' in R code. Then, you can change the parameters in Shiny interface and run BAS/BSAS algorithm.
#' @param func objective function; see example for more informations
#' @param constr constraints function; see example for more informations
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
run_BAS_App <- function(func,constr = NULL, theme = NULL){
  shinyApp(
    ui = tagList(
      #shinythemes::themeSelector(),
      navbarPage(
        theme = ifelse(is.null(theme),shinytheme('united'),shinytheme(theme)),
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
                   #parms.pen
                   numericInput('pen',label = 'penalty conefficient(A lager positive number):',value = 1e5),
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
                   #parms.pen
                   numericInput('pen2',label = 'penalty conefficient(A lager positive number):',value = 1e5),
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
        tabPanel("BSAS-WPT",
                 sidebarPanel(
                   #parms
                   #parm.init
                   textInput('init3',label = 'Initial parameters-init:',value = ' '),
                   #parms.lower
                   textInput(inputId = 'lower3',label = 'Lower of params-lower:',value = '-6,0'),
                   #parms.upper
                   textInput(inputId = 'upper3',label = 'Upper of params-upper:',value = '-1,2'),
                   #parms.constr
                   #parms.c2
                   numericInput('c2',label = 'ratio of step-size and searching distance C2:',value = 5),
                   #parms.step
                   numericInput('step3',label = 'Beetle step length-step:',value = 1),
                   #parms.eta_step
                   numericInput('eta_step3',label = 'Attenuation constant for step-eta_step:',value = 0.95),
                   #parms.n
                   numericInput('n3',label = 'Iteration numbers-n:',value = 200),
                   #parms.seed
                   numericInput('seed3',label = 'Random seed-seed:',value = 11),
                   #parms.trace
                   #parms.steptol
                   numericInput('steptol3',label = 'Step tolerance-steptol:',value = 0.01),
                   #parms.k
                   numericInput('k3',label = 'Number of beetles-k:',value = 5),
                   #parms.p_min
                   numericInput('p_min3',label = 'p_min(in [0,1)):',value = 0.2),
                   #parms.p_step
                   numericInput('p_step3',label = 'p_step(in [0,1)):',value = 0.2),
                   #parms.n_flag
                   numericInput('n_flag3',label = 'n_flag(positive integer):',value = 2),
                   #parms.pen
                   numericInput('pen3',label = 'penalty conefficient(A lager positive number):',value = 1e5),
                   #action
                   actionButton("action3", "Run BAS-WPT", class = "btn-primary")
                 ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel(title = 'Optimization Progress',
                              h3("Optimization Progress:"),
                              verbatimTextOutput("progress3")
                     ),
                     tabPanel(title = 'Optimization Parameters',
                              h3("Optimization Parameters:"),
                              verbatimTextOutput("params3")
                     ),
                     tabPanel(title = 'Progress Plot',
                              h3('Progress Plot:'),
                              plotOutput('plot3')
                     )
                   )


                 )
        ),
        tabPanel(title = "Authors",
                 uiOutput('aut')
        )
      )
    ),
    server = function(input, output,session) {
      # value
      value <- reactiveValues()

      # authors
      output$aut <- renderUI({
        tagList(tags$h3('Authors:'),
                tags$h4('Jiangyu Wang : BSAS & Package Creator'),
                a('Github Page', href = 'https://github.com/jywang2016'),
                tags$h4('Shuai Li : BAS & BAS-WPT'),
                a('Personal Homepage', href = 'http://www4.comp.polyu.edu.hk/~cssli/'),
                '&',
                a('Googlescholar',href = 'http://scholar.google.com/citations?hl=zh-CN&user=H8UOWqoAAAAJ'),
                tags$h4('Xiangyuan Jiang : BAS & BAS-WPT'),
                tags$hr(),
                tags$h3('Contributors:'),
                tags$h4('Xiaoxiao Li : second-order-BAS'),
                tags$h4('Tiantian Wang : beetle-swarm-optimization'),
                tags$h4('Yue Ruan : binary-BAS'),
                tags$h4('Xiaojuan Mo : multi-bar mechanism optimization')
        )
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
                           constr = constr,
                           d0 = input$d0, d1 = input$d1, eta_d = input$eta_d,
                           step = input$step, eta_step = input$eta_step,
                           n = input$n, steptol = input$steptol,
                           seed = input$seed,
                           pen = input$pen)
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
                             constr = constr,
                             d0 = input$d02, d1 = input$d12, eta_d = input$eta_d2,
                             step = input$step2, eta_step = input$eta_step2,
                             n = input$n2, steptol = input$steptol2,
                             seed = input$seed2,
                             p_min = input$p_min2, p_step = input$p_step2,
                             n_flag = input$n_flag2,
                             k = input$k2,
                             pen = input$pen2)
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

      #BSAS-WPT
      progress_reactive3 <- eventReactive(input$action3,{
        if(input$init3 == ' '){
          init3 <- NULL
        }else{
          init3 <- as.numeric(unlist(strsplit(input$init3,",")))
        }
        if(length(input$lower3)!=0){
          lower3 <- as.numeric(unlist(strsplit(input$lower3,",")))
        }
        if(length(input$upper3)!=0){
          upper3 <- as.numeric(unlist(strsplit(input$upper3,",")))
        }
        result3 <- BSAS_WPT(fn = func, init = init3,
                            lower = lower3,
                            upper = upper3,
                            c2 = input$c2,
                            constr = constr,
                            step = input$step3, eta_step = input$eta_step3,
                            n = input$n3,
                            steptol = input$steptol3,
                            seed = input$seed3,
                            p_min = input$p_min3, p_step = input$p_step3,
                            n_flag = input$n_flag3,
                            k = input$k3,
                            pen = input$pen3)
        value[['result3']] <- result3
      })

      output$progress3 <- renderPrint({
        progress_reactive3()
      })
      output$params3 <- renderPrint({
        c(value[['result3']][1],value[['result3']][2])
      })
      output$plot3 <- renderPlot({
        df_f <- value[['result3']]$df$f
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
