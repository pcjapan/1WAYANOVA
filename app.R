# Required Modules
library("shiny")
library("shinyWidgets")
library("ggplot2")
library("onewaytests")
library("WRS2")
library("DescTools")
library("compute.es")

Sys.setenv(TZ = 'Asia/Tokyo') ## Change server time zone
options(digits = 3)
source("ui.R", local = TRUE)

## server

server <- function(input, output, session) {
  ## Set up data
  
  Score <- reactive({
    scr <- input$scr
    scr <- as.numeric(unlist(strsplit(scr, "[\n, \t]")))
  })
  
  Group <- reactive({
    grp <- input$grp
    grp <- as.factor(unlist(strsplit(grp, "[\n, \t]")))
  })
  
  df <- reactive({
    data.frame(Score(), Group())
  })
  
  pwMethod <- reactive({
    pwMethod <- input$postHocMethod
  })
  
  trim <- reactive({
    trm <- input$trm
  })
  
  btstpN <- reactive({
    trm <- input$btsn
  })
  
  
  
  ##### Analysis #####
  
  ## descriptives - from onewaytests
  
  
  output$descStats2 <-
    output$descStats1 <-
    renderPrint({
      describe(Score() ~ Group(), df())
    })
  
  
  # Robust Bootstrapped & trimmed means
  
  robustANOVA <-
    reactive({
      t1way(Score() ~ Group(), df(), tr = trim(), nboot = btstpN())
    })
  output$robustANOVA <- renderPrint(robustANOVA())
  
  output$robustANOVAinfo <- renderPrint({cat(
    "Bootstrapped 1-way ANOVA for independent samples with", btstpN(), " bootstrapped samples and ", trim(), " trimmed mean." )})
    
  robustPostHoc <-
    reactive({
      lincon(Score() ~ Group(), df(), tr = trim(), nboot = btstpN(), method = pwMethod())
    })
  output$robustPostHoc <- renderPrint(robustPostHoc())
  
  output$robustPosthocinfo <- renderPrint({cat(
    "Bootstrapped Post-hoc tests with", btstpN(), "bootstrapped samples and", trim(), "trimmed means, using the", pwMethod(), "adjustment method." )})
  
  ## Run the ANOVA omnibus test
  
  res.aov <- reactive({
    aov(Score() ~ Group(), data = df())
  })
  output$res.aov <- renderPrint({
    summary.lm(res.aov())
  })
  
  ## Checking Assumptions
  ## normality of residuals shapiro-wilks
  
  aov_residuals <- reactive({
    residuals(object = res.aov())
  })
  output$shapTest  <-
    renderPrint({
      shapiro.test(x = aov_residuals())
    })
  
  ## normality of each group
  
  output$shapTestG <-
    renderPrint({
      nor.test(Score() ~ Group(), df())
    })
  
  ## Nonparametric
  
  source("function.R", local = TRUE)
  
  ghT <- reactive({
    games.howell(df()$Group, df()$Score)
  })
  
  output$ghT <- renderPrint({
    ghT()
  })
  
  nonPar <- reactive({
    kruskal.test(Score() ~ Group(), data = df())
  })
  output$nonPar <- renderPrint({
    nonPar()
  })
  
  ## Levene's test for homogeneity
  
  output$leveneT <- renderPrint({
    homog.test(Score() ~ Group(), df())
  })
  
  # homogeneity of variance
  
  output$WelchT <-
    renderPrint({
      welch.test(Score() ~ Group(), df(), rate = .1)
    })
  
  ## Pairwise comparisons
  
  output$scheffe <-
    renderPrint({
      ScheffeTest(x = res.aov(), which = "Group()")
    })
  
  output$tukeyHSD <- renderPrint({
    TukeyHSD(res.aov())
  })
  output$pwtt <-
    renderPrint({
      pairwise.t.test(df()$Score, df()$Group, p.adjust.method = pwMethod())
    })
  
  output$pairwise.t.adjustment <- renderPrint({cat(
    "Post-hoc tests using the", pwMethod(), "adjustment method." )})
  
  ## Graphs
  ## assumptions
  
  ggP <-  reactive({
    ggplot(df(), aes(sample = Score())) +
      stat_qq() +
      stat_qq_line() +
      facet_grid(cols = vars(Group())) +
      theme_minimal() +
      ggtitle("Normal Q-Q Plot for Groups")
  })
  output$ggP <- renderPlot({
    ggP()
  })
  output$homV <- renderPlot({
    plot(res.aov(), 1)
  })
  output$gNorm <- renderPlot({
    plot(res.aov(), 2)
  })
  
  ## Vizualise data
  # Violin /boxplot
  
  vplot <- reactive({
    ggplot(df(), aes(
      x = Group(),
      y = Score(),
      fill = Group()
    )) +
      geom_violin() +
      geom_boxplot(width = .1) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle("Violin Plot of Data")
  })
  output$vp <- renderPlot({
    vplot()
  })
  
  # dotplot
  
  dots <- reactive({
    ggplot(df(), aes(
      x = Group(),
      y = Score(),
      color = Group()
    )) +
      geom_boxplot(width = .3, color = "darkgoldenrod4") +
      geom_dotplot(
        binaxis = "y",
        binwidth = 0.5,
        stackdir = "center",
        stackratio = 1,
        fill = NA
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      ggtitle("Dotplot of Data")
  })
  output$dp <- renderPlot({
    dots()
  })
  
  #effect size calculator
  
  observeEvent(eventExpr = input[["submit_data"]],
               handlerExpr = {
                 m1 <- reactive({
                   m1 <- input$m1
                   as.numeric(m1)
                 })
                 m2 <- reactive({
                   m2 <- input$m2
                   as.numeric(m2)
                 })
                 sd1 <- reactive({
                   sd1 <- input$sd1
                   as.numeric(sd1)
                 })
                 sd2 <- reactive({
                   sd2 <- input$sd2
                   as.numeric(sd2)
                 })
                 n1 <- reactive({
                   n1 <- input$n1
                   as.numeric(n1)
                 })
                 n2 <- reactive({
                   n2 <- input$n2
                   as.numeric(n2)
                 })
                 
                 output$ES <-
                   renderPrint({
                     mes(m1(), m2(), sd1(), sd2(), n1(), n2())
                   })
               })
}
shinyApp(ui, server)