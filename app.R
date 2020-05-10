## Author: Ed Wilkes
## Year: 2020
## Required packages ----
library(dice)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinydashboard)
set.seed(123)

## Required local functions/variables ----
# plotTheme ----
plotTheme <- function(font_size) {
    return(ggplot2::theme(
        panel.background = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.grid.major = element_blank()
        ,axis.text = element_text(size = font_size, colour = "black")
        ,axis.title = element_text(size = font_size, colour = "black")
        ,panel.border = element_rect(fill = NA, colour = "black")
        ,strip.background = element_rect(fill = "grey80", colour = "black")
        ,strip.text = element_text(size = font_size, colour = "black")
        ,legend.background = element_rect(fill = NA, colour = "black")
        ,legend.title = element_text(face = "bold", size = font_size, colour = "black")
        ,legend.text = element_text(size = font_size, colour = "black"))
    )
}

## Header ----
header <- dashboardHeader(
  title = tags$a(
    href='https://dnd.wizards.com/'
    ,tags$img(src = 'dicer-logo-long.png', width = '75%')
  )
)

## Sidebar ----
sidebar <- dashboardSidebar(
  collapsed = TRUE
  ,sidebarMenu(
    menuItem("Probability distributions", tabName = "dice_distribution", icon = icon("cubes"))
    # ,menuItem("Multi-roll probabilities", tabName = "dice_multiroll", icon = icon("dice"))
  )
  
)

## Body ----
body <- dashboardBody(
    
    # Tags ----
    tags$style(
        HTML(
            ".skin-blue .main-header .logo {
              color:#FFFFFF;
              background-color:#581845;
            }
            .skin-blue .main-header .logo:hover {
              background-color:#581845;
            }
            .skin-blue .main-header .navbar {
              color:#FFFFFF;
              background-color:#581845;
            }
            .small-box.bg-blue {
              background-color:#FF5733 !important;
            }
            .box.box-solid.box-primary>.box-header {
              color:#FFFFFF;
              background: #900C3F;
            }
            .box.box-solid.box-primary {
              border-bottom-color:#900C3F;
              border-left-color:#900C3F;
              border-right-color:#900C3F;
              border-top-color:#900C3F;
            }"
        )
    )
    
    # Tabs ----
    ,tabItems(
        
      # "Probability distributions" ----
      tabItem(
        tabName = "dice_distribution"
        ,p()
        ,column(
          width = 4
          ,fluidRow(
            box(
              title = "Dice roll options"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,width = 12
              ,textInput(
                inputId = "dice_rolls"
                ,label = "How many times is your die being rolled?"
                ,value = 1
              )
              ,textInput(
                inputId = "dice_sides"
                ,label = "How many sides does your die have?"
                ,value = 20
              )
              ,textInput(
                inputId = "dice_mod"
                ,label = "What is your modifier for the roll?"
                ,value = 0
              )
              ,selectInput(
                inputId = "dice_k"
                ,label = "Choose your advantage/disadvantage:"
                ,choices = c("None", "Advantage", "Disadvantage")
                ,selected = "None"
                ,multiple = FALSE
              )
              ,uiOutput("slider_dice_threshold")
              ,actionButton(
                inputId = "dice_go"
                ,label = "Roll!"
                ,width = "100%"
                ,icon = icon("play")
                ,style="color: #fff; background-color: #C70039; border-color: #fff"
              )
            )
            ,conditionalPanel(
              condition = "input.dice_go != 0"
              ,uiOutput("value_box_probability")
            )
          )
        )
        ,column(
          width = 7
          ,fluidRow(
            conditionalPanel(
              condition = "input.dice_go != 0"
              ,box(
                title = "Dice roll probability distribution"
                ,solidHeader = TRUE
                ,collapsible = TRUE
                ,status = "primary"
                ,width = 12
                ,height = 620
                ,plotlyOutput("dice_roll_plot", height = 545)
              )
            )
          )
        )
      )      
    )

    # Window sizing code ----
    ,tags$head(
        tags$script('
                var height = 0;
                var width = 0;
                $(document).on("shiny:connected", function(e) {
                  height = window.innerHeight;
                  width = window.innerWidth;
                  Shiny.onInputChange("height", height);
                  Shiny.onInputChange("width", width);
                });
                $(window).resize(function(e) {
                  height = window.innerHeight;
                  width = window.innerWidth;
                  Shiny.onInputChange("height", height);
                  Shiny.onInputChange("width", width);
                });
                '
        )
    )
    
) # closes dashboardBody()

## UI ----
ui <- dashboardPage(
  title = "DiceR"
  ,header
  ,sidebar
  ,body
  ,skin = "blue"
)

## Server ----
server <- function(input, output, session) {
    
    # General functions ----
    window_height <- reactive({ifelse(is.null(input$height), 0, as.numeric(input$height))})
    window_width <- reactive({ifelse(is.null(input$width), 0, as.numeric(input$width))})
    
    # "Probability distributions" ----
    dice_output <- reactiveValues(data = NULL)
    
    eventListener <- reactive({
      
      # Reactive expression to monitor if button is pressed or slider is moved
      list(input$dice_go, input$dice_slider)
      
    })
    
    # UI slider ----
    output$slider_dice_threshold <- renderUI({
      
      # Requirements
      req(input$dice_sides, input$dice_rolls)
      
      # Get inputs
      n_sides <- as.integer(input$dice_sides)
      n_rolls <- as.integer(input$dice_rolls)
      modifier <- as.integer(input$dice_mod)
      adv_mod <- input$dice_k
      if (adv_mod == "None") {
        max_value <- n_sides * n_rolls
        value_holder <- (n_sides * n_rolls) * 0.6
      } else if (adv_mod != "None") {
        max_value <- n_sides
        value_holder <- n_sides * 0.6
      }
      
      # Checks and error handling
      if (is.na(n_sides)) {
        stop("Please enter an integer for the number of sides your die has!")
      }
      if (is.na(n_rolls)) {
        stop("Please enter an integer for the number of rolls you want to perform!")
      }
      if (is.na(modifier)) {
        stop("Please enter an integer for your modifier!")
      }
      
      # Generate dynamic sliderInput
      slider <- sliderInput(
        inputId = "dice_slider"
        ,label = "What value do you need (or higher)?"
        ,min = 1
        ,max = max_value
        ,step = 1
        ,value = value_holder
      )
      return(slider)
      
    })
    
    # Dice rolling ----
    observeEvent(eventListener(), ignoreInit = TRUE, {
      
      # On press of "dice_go" create and return data.frame object to dice_output reactiveValue
      # Get input values
      n_sides <- as.integer(input$dice_sides)
      n_rolls <- as.integer(input$dice_rolls)
      n_kept <- n_rolls
      modifier <- as.integer(input$dice_mod)
      target_value <- input$dice_slider
      if (input$dice_k == "None") {
        adv_mod <- NULL
      } else if (input$dice_k == "Advantage") {
        adv_mod <- TRUE
        n_kept <- 1
      } else if (input$dice_k == "Disadvantage") {
        adv_mod <- FALSE
        n_kept <- 1
      }
      
      # Checks and error handling
      if (is.na(n_sides)) {
        stop("Please enter an integer for the number of sides your die has!")
      }
      if (is.na(n_rolls)) {
        stop("Please enter an integer for the number of rolls you want to perform!")
      }
      if (is.na(modifier)) {
        stop("Please enter an integer for your modifier!")
      }
      
      # Generates rolls based on size of die
      if (!is.null(adv_mod)) {
        df_rolls <- as.data.frame(
          getSumProbs(ndicePerRoll = n_rolls
                      ,nsidesPerDie = n_sides
                      ,nkept = n_kept
                      ,dropLowest = adv_mod
                      ,sumModifier = modifier)$probabilities
        ) %>% 
          mutate(Success = if_else(Sum >= target_value, true = "Yes", false = "No"))
      } else if (is.null(adv_mod)) {
        df_rolls <- as.data.frame(
          getSumProbs(ndicePerRoll = n_rolls
                      ,nsidesPerDie = n_sides
                      ,nkept = n_kept
                      ,sumModifier = modifier)$probabilities
        ) %>% 
          mutate(Success = if_else(Sum >= target_value, true = "Yes", false = "No"))
      }
      colnames(df_rolls)[1] <- "Result"
      dice_output$data <- df_rolls # store output to reactiveValue
      
    })
    
    # Plotting ----
    output$dice_roll_plot <- renderPlotly({
      
      # Requirements
      req(dice_output$data, input$dice_slider)
      
      # Get inputs
      df <- dice_output$data # this is a reactiveValue
      target_value <- input$dice_slider
      max_x <- max(df$Result)
      
      # Plot data and return ggplotly object
      p <- ggplot2::ggplot(df
                           ,aes(x = Result, y = Probability, fill = Success))+
        geom_bar(stat = "identity"
                 ,colour = "black"
                 ,alpha = 0.5
                 ,width = 1)+
        xlab("Roll result")+
        ylab("Probability")+
        scale_x_continuous(breaks = seq(from = 1, to = max_x, by = 1))+
        scale_fill_manual(values = c("#ffffff", "red2"))+
        geom_vline(xintercept = target_value - 0.5, colour = "red2")+
        plotTheme(font_size = 14)+
        theme(legend.position = "none")
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
    # Value boxes ----
    output$value_box_probability <- renderUI({
      
      # Requirements
      req(dice_output$data, input$dice_slider)
      
      # Get inputs
      df <- dice_output$data # this is a reactiveValue
      prob_sum <- sum(dplyr::filter(df, Success == "Yes")$Probability)
      
      value_box <- list(
        valueBox(
          value = paste0(round(prob_sum, 3), " (~", round(prob_sum * 10, 0), "/10)")
          ,subtitle = "Probability of your target result (or greater)"
          ,icon = icon("dice")
          ,color = "blue"
          ,width = 12
        )
      )
      return(value_box)
      
    })
    
}

## Run the application ----
shinyApp(ui = ui, server = server)
