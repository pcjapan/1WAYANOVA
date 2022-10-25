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
  

  
##### Analysis #####  

## descriptives - from onewaytests 
  

  output$descStats <-
    renderPrint({
      describe(Score() ~ Group(), df())
    })
  
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

output$WelchT <- renderPrint({welch.test(Score() ~ Group(), df(), rate = .1)})
  
## Pairwise comparisons

output$scheffe <- renderPrint({ScheffeTest(x = res.aov(), which = "Group()")})

  output$tukeyHSD <- renderPrint({
    TukeyHSD(res.aov())
  })
  output$pwtt <-
    renderPrint({
      pairwise.t.test(df()$Score, df()$Group, p.adjust.method = pwMethod())
    })
  
# Robust Bootstrapped & trimme means

robustANOVA <-  reactive({t1way(Score() ~ Group(), df(), tr = trim(), nboot = 10)})
output$robustANOVA <- renderPrint(robustANOVA())

robustPostHoc <-  reactive({lincon(Score() ~ Group(), df(), tr = trim(), nboot = 10)})
output$robustPostHoc <- renderPrint(robustPostHoc())


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
    ggplot(df(), aes(x = Group(), y = Score())) +
      geom_violin() +
      geom_boxplot(width = .1) +
      theme_minimal()
  })
  output$plot1 <- renderPlot({
    vplot()
  })
  
# dotplot
  
  dots <- reactive({
    ggplot(df(), aes(x = Group(), y = Score())) +
      stat_summary(
        fun = median,
        fun.min = median,
        fun.max = median,
        geom = "crossbar",
        width = 0.6,
        size = 0.4,
        color = "black",
        alpha = 0.6
      ) +
      geom_dotplot(
        binaxis = "y",
        binwidth = 0.3,
        stackdir = "center",
        stackratio = 1
      ) +
      theme_minimal()
  })
  output$plot2 <- renderPlot({
    dots()
  })
  
#effect size calculator

  observeEvent(eventExpr = input[["submit_data"]],
               handlerExpr = {
m1 <- reactive({m1 <- input$m1
as.numeric(m1)})
m2 <- reactive({m2 <- input$m2
as.numeric(m2)})
sd1 <- reactive({sd1 <- input$sd1
as.numeric(sd1)})
sd2 <- reactive({sd2 <- input$sd2
as.numeric(sd2)})
n1 <- reactive({n1 <- input$n1
as.numeric(n1)})
n2 <- reactive({n2 <- input$n2
as.numeric(n2)})

output$ES <- renderPrint({mes(m1(), m2(), sd1(), sd2(), n1(), n2())})
               }
)
}
shinyApp(ui, server)