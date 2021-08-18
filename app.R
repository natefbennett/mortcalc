# Mortgage App: https://brian-clark-rpi.shinyapps.io/mort-app/

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(reshape2)
library(scales)
library(gridExtra)

options(scipen = 999)

# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Mortgage Calculator"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      numericInput("house", "House Price", 200000, min = 0, step = 1000),
      hr(),
      numericInput("principal", "Principal (loan amount)", 50000, min = 0, step = 1000),
      hr(),
      # sliderInput("down", "Percentage Down Payment (%)",
      #             min = 0,
      #             max = 100,
      #             value = 20,
      #             step = 1
      # ),
      # hr(),
      numericInput("interest", "Annual interest rate (in %)", 3.5, min = 0, max = 100, step = 0.125),
      hr(),
      # numericInput("downpayment", "Down Payment", 200000, min = 0, step = 500),
      # hr(),
      radioButtons("length", "Duration of the mortgage",
                   choices = list("15 Year" = 15, 
                                  "20 Year" = 20,
                                  "30 Year" = 30),
                   selected = 20
      ),
      hr(),
      p(HTML('<p>How much will I owe if I sell my house in X years?</p>')),
      sliderInput("yearsowned", "Years Owned Before Selling",
                  min = 1,
                  max = 29,
                  value = 15,
                  step = 1
      ),
      checkboxInput("selling", "Selling the house before the mortgage is paid?", FALSE),
      hr(),
      checkboxInput("plot", "Display plot?", TRUE),
      hr(),
      # checkboxInput("tax", "Include Tax Effects", TRUE),
      # hr(),
      # hr(),
      # radioButtons(
      #   inputId = "tax",
      #   label = "Include Tax Effects?",
      #   choices = c("Yes","No"),
      #   selected = "No",
      #   inline = FALSE,
      #   width = NULL,
      #   choiceNames = NULL,
      #   choiceValues = NULL
      # ),
      # hr(),
      # sliderInput("taxrate", "Marginal Tax Rate (%)",
      #             min = 0,
      #             max = 50,
      #             value = 30,
      #             step = 0.5
      # ),
      # hr(),
      # HTML('<p>Please report any bugs to me (clarkb2@rpi.edu).</p>')
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      uiOutput("text"),
      br(),
      plotOutput("distPlot"),
      br(),
      p(HTML('<h3>Notes:</h3>')),
      p(HTML('1. The payments do not include property tax, insurance, or any other costs associated with owning a house.')),
      # p(HTML('2. The tax benefits reflect the tax deductability of interest payments.')),
      br(),
      DT::dataTableOutput("tbl"),
      br(),
      p(em("Disclosure: Note that this application does not include investment advice or recommendations")),
      p(em("This R Shiny app is based on the R code of Antoine Soetewey.")),
      br(),
      br()
    )
  )
)

# Define server logic for slider examples ----
server <- function(input, output) {
  mortgage <- function(P = 500000, I = 6, L = 15, amort = TRUE, plotData = TRUE, taxPlot = TRUE, taxRate = 0) {
    J <- I / (12 * 100)
    N <- 12 * L
    M <- P * J / (1 - (1 + J)^(-N))
    M15 <- P * J / (1 - (1 + J)^(-(12*15)))
    M20 <- P * J / (1 - (1 + J)^(-(12*20)))
    M30 <- P * J / (1 - (1 + J)^(-(12*30)))
    monthPay <<- M
    monthPay15 <<- M15
    monthPay20 <<- M20
    monthPay30 <<- M30
    # Calculate Amortization for each Month
    if (amort == TRUE) {
      Pt <- P # current principal or amount of the loan
      currP <- NULL
      while (Pt >= 0) {
        H <- Pt * J # this is the current monthly interest
        C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
        Q <- Pt - C # this is the new balance of your principal of your loan
        Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
        currP <- c(currP, Pt)
      }
      monthP <- c(P, currP[1:(length(currP) - 1)]) - currP
      aDFmonth <<- data.frame(
        Month = 1:length(currP),
        Year = sort(rep(1:ceiling(N / 12), 12))[1:length(monthP)],
        Balance = c(currP[1:(length(currP))]),
        Payment = monthP + c((monthPay - monthP)[1:(length(monthP))]),
        Principal = monthP,
        Interest = c((monthPay - monthP)[1:(length(monthP))])
      )
      aDFmonth <<- subset(aDFmonth, Year <= L * 12)
      aDFyear <- data.frame(
        Amortization = tapply(aDFmonth$Balance, aDFmonth$Year, max),
        Annual_Payment = tapply(aDFmonth$Payment, aDFmonth$Year, sum),
        Annual_Principal = tapply(aDFmonth$Principal, aDFmonth$Year, sum),
        Annual_Interest = tapply(aDFmonth$Interest, aDFmonth$Year, sum),
        Year = as.factor(na.omit(unique(aDFmonth$Year)))
      )
      aDFyear <<- aDFyear
    }
    
    if (plotData == TRUE) {
      aDFyear2 <- aDFyear %>%
        rename(
          Interest = Annual_Interest,
          Payment = Annual_Payment,
          Principal = Annual_Principal
        )
      aDFyear2$Year <- as.factor(aDFyear2$Year)
      aDFyear2 <- melt(aDFyear2[, c("Interest", "Principal", "Year")], id.vars = "Year")
      
      p1 <- ggplot(aDFyear2, aes(x = Year, y = value, fill = variable)) +
        geom_bar(position = "fill", stat = "identity") +
        labs(y = "Payment") +
        scale_y_continuous(labels = percent) +
        theme_minimal() +
        theme(legend.title = element_blank(), legend.position = "top") +
        scale_fill_manual("legend", values = c("Principal" = "black", "Interest" = "red"))
      plts <- list(p1)
      
      # if (taxPlot == TRUE) {
      #   aDFyear3 <- aDFyear %>%
      #     rename(
      #       Interest = Annual_Interest,
      #       Payment = Annual_Payment,
      #       Principal = Annual_Principal
      #     )
      #   aDFyear3$Effective_Interest <- aDFyear3$Interest * (1 - taxRate/100)
      #   aDFyear3$TaxSavings <- aDFyear3$Interest * taxRate/100
      #   aDFyear3$Year <- as.factor(aDFyear3$Year)
      #   aDFyear3 <- melt(aDFyear3[, c("TaxSavings", "Effective_Interest", "Principal", "Year")], id.vars = "Year")
      
      #   p2 <- ggplot(aDFyear3, aes(x = Year, y = value, fill = variable)) +
      #     geom_bar(position = "fill", stat = "identity") +
      #     labs(y = "Payment") +
      #     scale_y_continuous(labels = percent) +
      #     theme_minimal() +
      #     theme(legend.title = element_blank(), legend.position = "top") +
      #     scale_fill_manual("legend", values = c("Principal" = "black", "Effective_Interest" = "red", "TaxSavings" = "green"))
      
      
      #   plts <- list(p1,p2)
      # }
      grid.arrange(grobs=plts,ncol=length(plts))
      
    }
    
    
  }
  payment <- function( interestRate = .05, numPayment = 1, loanValue = 0 ) {
    paymentValue <- loanValue*(interestRate*((1+interestRate)^numPayment))/(((1+interestRate)^numPayment)-1)
    return( paymentValue )
  }
  # ------------------------
  
  # --------------------------
  
  output$text <- renderUI({
    prin <- input$principal
    mortgage(P = prin, I = input$interest, L = as.integer(input$length), plotData = FALSE)
    HTML(  
      paste0(
        "<h3>", "Summary", "</h3>",
        "Principal (loan amount): ", format(round(prin, 2), big.mark = ","),
        "<br>",
        "Annual interest rate: ", input$interest, "%",
        "<br>",
        "Term: ", as.integer(input$length), " years (", as.integer(input$length) * 12, " months)",
        "<br>",
        "<b>", "Monthly payment: ", format(round(monthPay, digits = 2), big.mark = ","), "</b>",
        "<br>",
        "<b>", "Total cost: ", "</b>", format(round(prin, 2), big.mark = ","), " (principal) + ", format(round(monthPay * 12 * as.integer(input$length) - prin, 2), big.mark = ","), " (interest) = ", "<b>", format(round(monthPay * 12 * as.integer(input$length), digits = 2), big.mark = ","), "</b>"
      ),
      if (input$selling == TRUE) {
        
        yearsLeft <- as.integer(input$length) - as.integer(input$yearsowned)
        n <- 12*yearsLeft
        totalPayments <- 12*as.integer(input$length)
        interestPercent <- input$interest / 100
        monthlyInterest <- interestPercent / 12
        monthlyPayment <- payment(interestRate = monthlyInterest, numPayment = totalPayments, loanValue = prin)
        amountPaid <- monthlyPayment/monthlyInterest*(1-1/(1+monthlyInterest)^(totalPayments-n))
        
        if (yearsLeft <= 0) {
          amountLeft <- 0
          futureValue <- 0
        }
        else {
          amountLeft <- prin-amountPaid
          futureValue <- amountLeft*(1+interestPercent)^yearsLeft
        }
        
        HTML(paste0(
          "<br>",
          "<br>",
          "<u>How much will I owe if I sell my house in X years?</u>",
          "<br>",
          "Amount left to Pay: <b>", format(round(amountLeft, digits = 2), big.mark = ","), "</b>",
          "<br>",
          "Future Value: <b>", format(round(futureValue, digits = 2), big.mark = ","), "</b>",
          "<br>"
        ))
      }
      else { paste0("") }
    )
  })
  
  output$distPlot <- renderPlot({
    prin <- input$principal
    mortgage(P = prin, I = input$interest, L = as.integer(input$length), plotData = input$plot, taxPlot = input$tax, taxRate = input$taxrate)
  })
  
  
  # Data output
  output$tbl <- DT::renderDataTable({
    prin <- input$principal
    mortgage(P = prin, I = input$interest, L = as.integer(input$length), plotData = FALSE)
    df_month <- DT::datatable(data.frame(round(aDFmonth, 2)),
                              extensions = "Buttons",
                              options = list(
                                lengthChange = TRUE,
                                dom = "Blrtip",
                                buttons = c("copy", "csv", "excel", "pdf", "print"),
                                
                                lengthMenu = list(c(-1, 10, 12, 15, 25, 50, 100), c("All", "10", "12", "15", "25", "50", "100"))
                              ),
                              rownames = FALSE
    ) %>%
      formatCurrency(c("Balance", "Payment", "Principal", "Interest"), currency = "", interval = 3, mark = ",")
  })
}

# Run the application
shinyApp(ui = ui, server = server)