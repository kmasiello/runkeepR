#' Show a summary of the Runkeeper(TM) data
#'
#' @param rundata the data to process
#' @param by grouping category. Default is \code{trkname}; the unique tracks. 
#' @param dashboard present the information in a shinydashboard?
#'
#' @import dplyr
#' @import ggplot2
#' @import shiny
#' @import shinydashboard
#'
#' @return NULL
#' 
#' @export
#'
summarise_runs <- function(rundata, dashboard = TRUE) {
  
  rundata$yday <- lubridate::yday(rundata$Date)
  
  numcols         <- sapply(rundata, is.numeric)
  numcols["Date"] <- TRUE
  numcols["trkname"]     <- TRUE
  
  numdata <- rundata[numcols] %>% select(-latitude, -longitude) %>% unique
  
  numdata$monthBin <- as.character(cut(as.Date(numdata$Date), breaks = "month"))
  
  cols_to_process <- c("Duration_sec", "Distance (mi)", 
                       "Calories Burned", "Climb (ft)", "elevation")
  
  # mins  <- numdata %>% group_by_(by) %>% summarise_each_(funs(min(., na.rm=TRUE)),    vars=cols_to_process)
  # mins1  <- numdata %>% group_by_(by) %>% summarise_each_(funs(min(., na.rm=TRUE)),    vars=cols_to_process)
  
  mins  <- numdata %>% group_by(Date) %>% summarise_at(cols_to_process, min)
  means <- numdata %>% group_by(Date) %>% summarise_at(cols_to_process, mean, na.rm = TRUE)
  sums  <- numdata %>% group_by(Date) %>% summarise_at(cols_to_process, sum, na.rm = TRUE)
  maxs  <- numdata %>% group_by(Date) %>% summarise_at(cols_to_process, max)
  

  
  if (!dashboard) {
    
    numdata_sum <- numdata %>% select(monthBin,"Duration_sec", "Distance (mi)", 
                                       "Calories Burned", "Climb (ft)", "elevation") %>% 
      group_by(monthBin) %>% 
      summarise_all(sum, na.rm = TRUE)
    
    numdata_long <- numdata_sum %>% ungroup %>% tidyr::gather("QUANTITY", "VALUE", -1)
    gg <- ggplot(numdata_long, aes_(x = ~as.Date(monthBin), y = ~VALUE)) 
    gg <- gg + geom_bar(stat = "identity", fill = "steelblue1") 
    gg <- gg + facet_wrap(~QUANTITY, scales = "free_y") 
    gg <- gg + theme_bw()
    gg <- gg + labs(title = "Runkeeper Data", 
                    subtitle = paste0(nrow(rundata), 
                                      " records processed, ",
                                      length(unique(rundata$trkname)),
                                      " events tracked."), 
                    x = "YYYY-MM", 
                    y = "Value")
    gg <- gg + scale_x_date(date_breaks = "1 month")
    gg <- gg + theme(axis.text.x=element_text(angle = 90, hjust = 0))
    print(gg)
    
  } else {
    
    ## app.R ##
    # library(shinydashboard)
    
    ui <- dashboardPage(
      dashboardHeader(title = "runkeepR Data"),
      dashboardSidebar(),
      dashboardBody(
        fluidRow(box(plotOutput("plot1"), width = 12)),
        
        fluidRow(
          box(title = "Year",
              sliderInput("slider", "Year:", min(unique(rundata$Year)), max(unique(rundata$Year)), 1, animate = TRUE), width = 4
          ),
          box(title = "Function",
              radioButtons("fn", "Function:", c("sum", "mean", "max"), selected="mean"), width = 4
          ),
          box(title = "Window",
              radioButtons("window", "Window:", c("daily", "monthly"), selected="monthly"), width = 4
          )
          
          
        )
        
      )
    )
    
    server <- function(input, output, session) {
      
      ## no good for CRAN      
      # library(magrittr)
      # library(dplyr)
      # library(magrittr)
      # library(tidyr)
      # library(shiny)
      # library(ggplot2)
      
      
      # library(shinyjs)
      
      # observeEvent(input$showSidebar, {
      #   shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      # })
      # observeEvent(input$hideSidebar, {
      #   shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      # })
      
      output$plot1 <- renderPlot({
        if (input$window == "monthly") {
          numdata_sum <- numdata %>% filter_(~Year == input$slider) %>% 
            select_("monthBin", "Duration_sec", "Distance (mi)", 
                    "Calories.Burned", "Climb (ft)", "elevation") %>% 
            group_by_("monthBin") %>% 
            summarise_each_(funs_(input$fn), 
                            vars = lazyeval::interp(~everything()))
          numdata_long <- numdata_sum %>% ungroup %>% tidyr::gather_("QUANTITY", "VALUE", -1) 
          ## NB: this usage of gather_ is possibly wrong; http://stackoverflow.com/a/29605376/4168169
          gg <- ggplot(numdata_long, aes_(x = ~as.Date(monthBin), y = ~VALUE)) 
          gg <- gg + scale_x_date(date_breaks = "1 month", date_labels = "%B")
          gg <- gg + labs(title = paste0(input$slider," Runkeeper Data"), 
                          subtitle = paste0(nrow(rundata %>% filter_(~Year == input$slider)), 
                                            " location records processed, ",
                                            length(unique(rundata %>% 
                                                            filter_(~Year == input$slider) %>% 
                                                            use_series("trkname"))),
                                            " events tracked."), 
                          x = "Month", 
                          y = paste0(input$fn,"(Value)"))
        } else if (input$window == "daily") {
          numdata_sum <- numdata %>% filter_(~Year == input$slider) %>% 
            select_("yday", "Duration_sec", "Distance (mi)", "Calories.Burned", 
                    "Climb (ft)", "elevation") %>% 
            group_by_("yday") %>% 
            summarise_each_(funs_(input$fn), 
                            vars = lazyeval::interp(~everything()))
          # numdata_long     <- numdata_sum %>% ungroup %>% tidyr::gather_("QUANTITY", "VALUE", quote(-yday))
          numdata_long <- numdata_sum %>% ungroup %>% tidyr::gather_("QUANTITY", "VALUE", -1)
          # numdata_long     <- numdata_sum %>% ungroup %>% ungroup %>% tidyr::gather(QUANTITY, VALUE, -yday)
          gg <- ggplot(numdata_long, aes_(x = ~yday, y = ~VALUE)) 
          gg <- gg + xlim(c(0,365))
          gg <- gg + labs(title = paste0(input$slider," Runkeeper Data"), 
                          subtitle = paste0(nrow(rundata %>% 
                                                   filter_(~Year == input$slider)), 
                                            " location records processed, ",
                                            length(unique(rundata %>% 
                                                            filter_(~Year == input$slider) %>% 
                                                            use_series("trkname"))),
                                            " events tracked."), 
                          x = "Day of Year", 
                          y = paste0(input$fn,"(Value)"))
          # gg <- gg + scale_x_date(date_breaks="1 month")
        }
        gg <- gg + geom_bar(stat = "identity", fill = "steelblue1") 
        gg <- gg + facet_wrap(~QUANTITY, scales = "free_y") 
        gg <- gg + theme_bw()
        gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 0))
        print(gg)
      })
    }
    
    shinyApp(ui, server)
    
  }
  
}