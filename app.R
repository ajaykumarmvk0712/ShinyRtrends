
library(reshape2)
library(DT)
library(shiny)
library(shinydashboard)
library(readr)
library(tidyr)
library(magrittr)
library(plyr)
library(dplyr)
library(scales)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)

# create a few dummy objects

set.seed(54)
fake_vector <<- 1:10
fake_matrix <<- matrix(rnorm(50), nrow = 10, ncol = 5)
fake_mtcars <<- mtcars %>% select(mpg, cyl, disp, gear)
fake_list <<- list(numbers = 1:50, 
                   lower_case_letters = letters)

# modify iris data to make it suitable for
# a logistic regression type analysis

new_iris <<- iris %>% mutate(
  is_setosa_flag = case_when(.$Species == "setosa" ~ 1,
                             TRUE ~ 0)
)

# read in logistic regression from UCLA website
ucla_data <<- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
ucla_data$rank %<>% as.character()

# create a vector of all existing R objects in the session
r_objects <<- data.frame(r_objects = ls(globalenv()),
                         stringsAsFactors = FALSE)

######################################################
# start making dashboard
######################################################

######################################################
# code for ui
######################################################

ui <- dashboardPage(
  
  dashboardHeader(title = "Interactive Bivariate"),
  
  dashboardSidebar(
    
    selectInput("robject", h3("Choose R object"),
                choices = r_objects$r_objects),
    br(),
    uiOutput("Yvar"),
    uiOutput("Xvars"),
    
    # Copy the chunk below to make a group of checkboxes
    radioButtons("radioGroup", 
                 label = h3("Missing Value Imputation"), 
                 choices = list("Median" = 1, 
                                "Mean" = 2,
                                "No Imputation" = 3),
                 selected = 3),
    
    htmlOutput("no_bands"),
    
    htmlOutput("cutpoints"),
    htmlOutput("combine_categories"),
    br() 
  ),
  
  dashboardBody(
    uiOutput("infoboxes"),
    # fluidRow(
    #   valueBox( paste((round(1490 / 333410 , 4) * 100), "%", sep = ""),
    #             "Overall Response Rate",
    #             color = "blue"),
    #   valueBox( paste("1,490", sep = ""),
    #             "Total Responders",
    #             color = "blue"),
    #   valueBox( paste("3,33,410", sep = ""),
    #             "Total Population",
    #             color = "blue")
    # ),
    
    fluidRow(
      box(title = "Table",
          solidHeader = TRUE,
          collapsible = TRUE,
          status = "primary",
          width = 250,
          tabBox(
            id = "tabset1",
            # height = "420px",
            width = 800,
            tabPanel("Tabular Distribution",
                     downloadButton('downloadData', 'Download the data'),
                     br(),br(),
                     verbatimTextOutput("value"),
                     dataTableOutput("view")),
            tabPanel("For Combining Categories",
                     uiOutput("mytabs")
                     #verbatimTextOutput("newcheck")
            ),
            tabPanel("Descriptions and Help",
                     h2("Purpose of Application"),
                     "This is an application to help analyse trends of variables interactively.
                     The app is designed for trend analyses involving binary response variables.", br(),
                     h2("Data frames which will give valid results"), 
                     "A valid data set has class data.frame. It will have a column for the dependent variable.
                     The unique values will be 0 and 1.
                     It's class will be numeric.
                     The user will select a 'Y' and an 'X' variable, A tabular and a graphic
                     representation of the trends will be seen. These can be downloaded.",
                     h2("Important Features."),
                     tags$ul(
                      tags$li(" The", tags$u("Choose R Object"), "dropdown will identify all R objects in the user's workspace and the user will choose
                     which data to analyse."),
                     
                       tags$li(" For continuous variables the user can decide if he/she wants the variable to be
                     categorized."),
                     
                       tags$li("For categorical variables the user can decide if categories need to be combined.
                     The dropdown for number of combinations can be used to decide the number 
                     of combinations. For example- if a variable has levels A, B, C, D and E
                     and if the user wants to combine A and C into one category and
                     B and D into another category, the user can select 2 as the number of combinations.
                     2 drop downs will appear in the for_combining_categories tabpanel."),
                     
                       tags$li("Both the tabular and graphical representation of the trends can be downloaded.")),
                     
                     h2("More descriptions"),
                     
                     h3("fake_list "),
                     "is a dummy list which contains 2 components.
                     The first component is a vector of numbers from 1 to 50
                     The second component is a vector of lower case letters", br(),
                     
                     h3("fake_matrix"), 
                     "is a dummy matrix of random numbers distributed Uniformly
                     between 0 and 1",br(),
                     
                     h3("ake_vector"), 
                     "is a dummy vectpr of numbers from 1 to 10",br(),
                     
                     h3("fake_mtcars"), 
                     "is a dataframe which selects the variables
                     mpg, cyl, disp, gear from the mtcars data. The dashboard will not 
                     present results for this data set.", br(),
                     
                     h3("new_iris "),
                     "is a modification of the iris dataset.
                     The variable is_setosa_flag is the response variable of interest.
                     This is a binary variable indicating if the species is setosa.
                     The dashboard will present results for this data.",br(),
                     
                     h3("ucla_data"),
                     "is a dataset available online from the IDRE website.
                     The dataset is an example to help understand logistic regression.
                     The response variable is admit. This indicates if a student is admitted to a university.
                     The explanatory variables are gre, gpa and rank of undergraduate institution (a factor variable)"
            ))),
      
      fluidRow(
        box(
          title = "Plot", status = "primary", 
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          downloadButton('downloadPlot', 'Download Plot'),
          br(),br(),
          plotOutput("myplot"),
          width = 150
)))))
  
  
  ######################################################
  # code for server
  ######################################################
  
  server <- function(input, output, session) {
    
    shinyServer(function(input, output, session){
      session$onSessionEnded(function() {
        stopApp()
      })
    })
    
    current_data <- reactive({
      current_data <- get(input$robject)
    })
    
    output$Yvar <- renderUI({
      
      #################################################
      # validation message
      #################################################
      
      validate(
        need(class(current_data()) == "data.frame",
             "Dashboard will work for valid data frames")
      )
      
      selectInput("response", h3("Choose response variable:"),
                  choices = colnames(current_data()))
    })
    
    output$Xvars <- renderUI({
      selectInput("variable", h3("Choose an X variable:"), 
                  choices = colnames(current_data())[which(
                    colnames(current_data()) != input$response )]
      )
    })
    
    output$infoboxes <- renderUI({
      
      validate(
        need(class(current_data()) == "data.frame",
             "Dashboard will work for valid data frames")
      )
      
      info <- current_data() %>% dplyr::select_(input$response)
      y <- info[, 1]
      
      validate(
        need(unique(info[, 1]) %in% c(0, 1),
             "Response variable must only contain values 0 or 1")
      )
      
      ones <- y[y == 1]
      zeros <- y[y == 0]
      total <- length(y)
      nr <- sum(ones, na.rm = T)
      
      fluidRow(
        valueBox( paste(comma(round(nr / total , 4) * 100), "%", 
                        sep = ""),
                  "Overall Response Rate",
                  color = "blue"),
        valueBox( paste(comma(nr), sep = ""),
                  "Total Responders",
                  color = "blue"),
        valueBox( paste(comma(total), sep = ""),
                  "Total Population",
                  color = "blue")
      )
    })
    
    check_data <- reactive({
      
      validate(
        need(class(current_data()) == "data.frame",
             "Dashboard will work for valid data frames")
      )
      
      req(input$variable)
      
      current_data() %>% dplyr::select_(input$variable)
    })
    
    output$no_bands <- renderUI({
      
      req(input$response)
      req(input$variable)
      
      if(is.numeric(check_data()[,1]) == TRUE)  {
        selectizeInput("no_of_bands", 
                       h3("Choose number of bands:"), 
                       choices = 1:5)
      }
    })
    
    output$cutpoints <- renderUI({
      
      req(input$response)
      req(input$variable)
      
      if(is.numeric(check_data()[,1]) == TRUE)  {
        if(input$no_of_bands == 2)  
        {textInput("num2", label = h4("Cut Point"),
                   value = quantile(check_data()[, 1], 0.5, na.rm = T),
                   placeholder = "Enter numerical values")} else if
        (input$no_of_bands == 3) 
                   {
          fluidRow( 
            column(5,
                   textInput("num3a", label = h4("Cut point 1"),
                             value = quantile(check_data()[, 1], 0.25, 
                                              na.rm = T),
                             placeholder = "Enter numerical values")),
            column(5,
                   textInput("num3b", label = h4("Cut point 2"),
                             value = quantile(check_data()[, 1], 0.75,
                                              na.rm = T),
                             placeholder = "Enter numerical values")))
        } else if(input$no_of_bands == 4) 
        {
          fluidRow( 
            column(5,
                   textInput("num4a", label = h4("Cut point 1"),
                             value = quantile(check_data()[, 1], 0.20, 
                                              na.rm = T),
                             placeholder = "Enter numerical values")),
            column(5,
                   textInput("num4b", label = h4("Cut point 2"),
                             value = quantile(check_data()[, 1], 0.5,
                                              na.rm = T),
                             placeholder = "Enter numerical values")),
            column(5,
                   textInput("num4c", label = h4("Cut point 3"),
                             value = quantile(check_data()[, 1], 0.75,
                                              na.rm = T),
                             placeholder = "Enter numerical values")))
        } else if(input$no_of_bands == 5) 
        {
          fluidRow( 
            column(5,
                   textInput("num5a", label = h4("Cut point 1"),
                             value = quantile(check_data()[, 1], 0.20, 
                                              na.rm = T),
                             placeholder = "Enter numerical values")),
            column(5,
                   textInput("num5b", label = h4("Cut point 2"),
                             value = quantile(check_data()[, 1], 0.4,
                                              na.rm = T),
                             placeholder = "Enter numerical values")),
            column(5,
                   textInput("num5c", label = h4("Cut point 3"),
                             value = quantile(check_data()[, 1], 0.6,
                                              na.rm = T),
                             placeholder = "Enter numerical values")),
            column(5,
                   textInput("num5d", label = h4("Cut point 4"),
                             value = quantile(check_data()[, 1], 0.8,
                                              na.rm = T),
                             placeholder = "Enter numerical values")))
        }
        
      }
    })
    
    output$combine_categories <- renderUI({
      
      req(input$response)
      req(input$variable)
      
      if(is.numeric(check_data()[,1]) == FALSE)  {
        selectizeInput("new_tot_classes", h3("Total number of resulting classes"),
                       choices = 0:4,
                       # choices = 0 : (length(unique(check_data()[, 1])) - 1 ),
                       selected = 0) 
      }
    })
    
    output$mytabs <- renderUI({
      
      req(input$response)
      req(input$variable)
      
      check1 <- class(check_data()[,1])
      
      if(check1 == "numeric"){check1 <- F}
      
      if(is.numeric(check_data()[,1]) == FALSE) {
        if(as.numeric(input$new_tot_classes) <= 3) {
          # tabBox(id= "ttabs", 
          #     width = 30, height = "420px",
          tabPanel("Files", 
                   uiOutput("tab_combined_classes")
          ) 
        } else if(as.numeric(input$new_tot_classes) > 3) {
          # tabBox(id= "ttabs", 
          #     width = 40, height = "420px",
          tabPanel("Files", 
                   uiOutput("tab_combined_classes")
          )
        }
      } else {return(NULL)}
    })
    
    output$tab_combined_classes <- renderUI({
      
      req(input$response)
      req(input$variable)
      numAssets <- as.numeric(input$new_tot_classes)
      
      # this is a dynamic way to bring the required number
      # of dropdowns,
      # basically if a user wants to see 3 new classes after combining
      # original values, there will be 3 drop downs.
      
      if(numAssets != 0) {
        lapply(1:numAssets, function(i) {
          list(tags$p(tags$u(h4(paste0("Class ", i, ":")))),
               
               # input#combine1, input$combine2 etc get created 
               # dynamically
               selectizeInput(paste("combine", i, sep = ""), 
                              h3("Combine Categories"),
                              choices = unique(check_data()[, 1]),
                              selected = unique(check_data()[, 1])[1],
                              multiple = TRUE)
          )
        }
        )
      } else {return(NULL)}
    })
    
    subset_data <- reactive({
      
      # validation of error
      req(input$response)
      req(input$variable)
      
      validate(
        need(class(current_data()) == "data.frame",
             "Dashboard will work for valid data frames")
      )
      
      s <- current_data() %>% dplyr::select_(input$variable, 
                                             input$response)
      
      validate(
        need(unique(s[, 2]) %in% c(0, 1),
             "Response variable must only contain values 0 or 1")
      )
      
      validate(
        need(length(s[, 1]) != length(unique(s[, 1])) ,
             "All values of the variable are distinct. 
             Analysis may not be meaningful")
        )
      
      if(input$radioGroup == 1) {
        s[, 1][is.na(s[, 1])] <- median(s[, 1], na.rm = T)
      } else if(input$radioGroup == 2) {
        s[, 1][is.na(s[, 1])] <- mean(s[, 1], na.rm = T)
      }
      return(s)
    })
    
    output$value <- renderText({
      
      w <- sum(is.na(subset_data()[, 1]))
      n <- nrow(subset_data())
      
      paste("The variable", input$variable ,
            "has", w, paste("(",
                            round( (w / n),2 ) * 100, "%)", sep = ""),
            "missing values", sep = " ")
    })
    
    reactivedata <- reactive({
      
      req(input$response)
      req(input$variable)
      
      validate(
        need(class(current_data()) == "data.frame",
             "Dashboard will work for valid data frames")
      )
      
      numAssets <- as.numeric(input$new_tot_classes)
      
      if(numAssets == 0) {
        out = subset_data() %>% mutate(
          var_bands  = subset_data()[, 1])
        out = as.data.frame(out)
      } else if(numAssets == 1) {
        out = subset_data() %>% mutate(
          var_bands  =
            ifelse(subset_data()[, 1] %in% unlist(input$combine1),
                   paste0(unlist(input$combine1), collapse = ""),
                   subset_data()[, 1]))
        out = as.data.frame(out)
      } else if(numAssets == 2) {
        out = subset_data() %>% mutate(
          var_bands  =
            ifelse(subset_data()[, 1] %in% unlist(input$combine1),
                   paste0(unlist(input$combine1), collapse = ""),
                   ifelse(subset_data()[, 1] %in% unlist(input$combine2),
                          paste0(unlist(input$combine2), collapse = ""),
                          subset_data()[, 1])))
        out = as.data.frame(out)
      }
      else if(numAssets == 3) {
        out = subset_data() %>% mutate(
          var_bands  =
            ifelse(subset_data()[, 1] %in% unlist(input$combine1),
                   paste0(unlist(input$combine1), collapse = ""),
                   ifelse(subset_data()[, 1] %in% unlist(input$combine2),
                          paste0(unlist(input$combine2), collapse = ""),
                          ifelse(subset_data()[, 1] %in% unlist(input$combine3),
                                 paste0(unlist(input$combine3), collapse = ""),
                                 subset_data()[, 1]))))
        out = as.data.frame(out)
      }else if(numAssets == 4) {
        out = subset_data() %>% mutate(
          var_bands  =
            ifelse(subset_data()[, 1] %in% unlist(input$combine1),
                   paste0(unlist(input$combine1), collapse = ""),
                   ifelse(subset_data()[, 1] %in% unlist(input$combine2),
                          paste0(unlist(input$combine2), collapse = ""),
                          ifelse(subset_data()[, 1] %in% unlist(input$combine3),
                                 paste0(unlist(input$combine3), collapse = ""),
                                 ifelse(subset_data()[, 1] %in% unlist(input$combine4),
                                        paste0(unlist(input$combine4), collapse = ""),
                                        subset_data()[, 1])))))
        out = as.data.frame(out)
      } else {out = data.frame("V1" = "Yo")}
      return(out)
    })
    
    # output$newcheck <- renderText({
    #  numAssets <- as.numeric(input$new_tot_classes)
    
    #  validate(
    #    need(is.numeric(check_data()[, 1]) == F,
    #         "This message will be displayed only for character columns")
    #  )
    
    #  paste0("Numassets has value ", numAssets)
    #  
    # })
    
    newtable <- reactive({
      
      #################################################
      # validation of error
      #################################################
      
      validate(
        need(class(current_data()) == "data.frame",
             "Dashboard will work for valid data frames")
      )
      
      if(input$no_of_bands == 3) {
        validate(
          need(as.numeric(input$num3a) <= as.numeric(input$num3b),
               "Cut points provided must be in increasing order.")
        ) }
      
      if(input$no_of_bands == 4) {
        validate(
          need(as.numeric(input$num4a) <= as.numeric(input$num4b) &
                 as.numeric(input$num4b) <= as.numeric(input$num4c),
               "Cut points provided must be in increasing order.")
        ) }
      
      if(input$no_of_bands == 5) {
        validate(
          need(as.numeric(input$num5a) <= as.numeric(input$num5b) &
                 as.numeric(input$num5b) <= as.numeric(input$num5c) &
                 as.numeric(input$num5c) <= as.numeric(input$num5d) ,
               "Cut points provided must be in increasing order.")
        ) }
      
      validate(
        need(unique(subset_data()[, 2]) %in% c(0, 1),
             "Response variable must only contain values 0 or 1")
      )
      
      #################################################
      # start making categories of continuus variable
      #################################################
      
      new_var <- subset_data()[, 1]
      if(is.numeric(new_var) == T) {
        if(input$no_of_bands == 1) {
          new_var <- subset_data()[, 1]
          bands <- ifelse(is.na(new_var) == T, "Z. Missing",
                          new_var)
          table_data <- subset_data() %>% mutate(var_bands = bands) %>% 
            as.data.frame()
          
          return(table_data) } else if(input$no_of_bands == 2) {
            new_var <- subset_data()[, 1]
            bands <- ifelse(is.na(new_var) == T, "Z. Missing",
                            ifelse(new_var <= as.numeric(input$num2) ,
                                   paste("A. <=", comma(as.numeric(input$num2)), sep = " "),
                                   paste("B. >=", comma(as.numeric(input$num2)), sep = " ")))
            
            table_data <- subset_data() %>% mutate(var_bands = bands) %>% 
              as.data.frame()
            return(table_data)
            
          } else if(input$no_of_bands == 3) {
            new_var <- subset_data()[, 1]
            bands <- ifelse(is.na(new_var) == T, "Z. Missing",
                            ifelse(new_var <= as.numeric(input$num3a) ,
                                   paste("A. <=", comma(as.numeric(input$num3a)),
                                         sep = " "),
                                   ifelse(new_var <= as.numeric(input$num3b) ,
                                          paste("B.", comma(as.numeric(input$num3a)) ,
                                                "-",
                                                comma(as.numeric(input$num3b)) ,
                                                sep = " "),
                                          paste("C. >=", comma(as.numeric(input$num3b)),
                                                sep = " ")
                                   )))
            
            table_data <- subset_data()  %>% mutate(var_bands = bands) %>% 
              as.data.frame()
            return(table_data) }  else if(input$no_of_bands == 4) {
              new_var <- subset_data()[, 1]
              bands <- ifelse(is.na(new_var) == T, "Z. Missing",
                              ifelse(new_var <= as.numeric(input$num4a) ,
                                     paste("A. <=", comma(as.numeric(input$num4a)),
                                           sep = " "),
                                     ifelse(new_var <= as.numeric(input$num4b) ,
                                            paste("B.", comma(as.numeric(input$num4a)) ,
                                                  "-",
                                                  comma(as.numeric(input$num4b)) ,
                                                  sep = " "),
                                            ifelse(new_var <= as.numeric(input$num4c) ,
                                                   paste("C.", comma(as.numeric(input$num4b)) ,
                                                         "-",
                                                         comma(as.numeric(input$num4c)) ,
                                                         sep = " "),
                                                   paste("D. >=", comma(as.numeric(input$num4c)),
                                                         sep = " "))
                                     )))
              
              table_data <- subset_data()  %>% mutate(var_bands = bands) %>% 
                as.data.frame()
              return(table_data) } else if(input$no_of_bands == 5) {
                new_var <- subset_data()[, 1]
                bands <- ifelse(is.na(new_var) == T, "Z. Missing",
                                ifelse(new_var <= as.numeric(input$num5a) ,
                                       paste("A. <=", comma(as.numeric(input$num5a)),
                                             sep = " "),
                                       ifelse(new_var <= as.numeric(input$num5b) ,
                                              paste("B.", comma(as.numeric(input$num5a)) ,
                                                    "-",
                                                    comma(as.numeric(input$num5b)) ,
                                                    sep = " "),
                                              ifelse(new_var <= as.numeric(input$num5c) ,
                                                     paste("C.", comma(as.numeric(input$num5b)) ,
                                                           "-",
                                                           comma(as.numeric(input$num5c)) ,
                                                           sep = " "),
                                                     ifelse(new_var <= as.numeric(input$num5d) ,
                                                            paste("C.", comma(as.numeric(input$num5c)) ,
                                                                  "-",
                                                                  comma(as.numeric(input$num5d)) ,
                                                                  sep = " "),
                                                            paste("D. >=", comma(as.numeric(input$num5d)),
                                                                  sep = " "))
                                              ))))
                
                table_data <- subset_data()  %>% mutate(var_bands = bands) %>% 
                  as.data.frame()
                
              } } else if (is.numeric(subset_data()[, 1]) == F) {
                table_data <- reactivedata()
                # table_data <- subset_data()
                # table_data$var_bands <- table_data[,1]
                # table_data <- as.data.frame(table_data)
                
                return(table_data)
              }
    })
    
    final_view <- reactive({
      
      validate(
        need(class(current_data()) == "data.frame",
             "Dashboard will work for valid data frames")
      )
      
      bv_data <- newtable() %>% group_by_("var_bands", input$response) %>%
        summarise(tot = n()) %>% 
        spread_(key = input$response, value = "tot") %>%
        dplyr::rename(zeros = `0`, ones = `1`)
      
      bv_data[is.na(bv_data)] <- 0
      n <- nrow(subset_data())
      
      bv_data <- mutate(bv_data, tot = zeros + ones,
                        res_rate = round((ones / tot), 4),
                        cust_perc = round((tot / n), 4) ) %>% 
        as.data.frame()
      
      bv_data$tot <- format(bv_data$tot, big.mark=',', 
                            scientific=FALSE)
      
      bv_data$zeros <- format(bv_data$zeros, big.mark=',', 
                              scientific=FALSE) 
      
      bv_data$ones <- format(bv_data$ones, big.mark=',', 
                             scientific=FALSE) 
      
      bv_data$res_rate <- percent(bv_data$res_rate)
      bv_data$cust_perc <- percent(bv_data$cust_perc)
      
      colnames(bv_data)[1] <- "Attribute"
      return(bv_data)
    })
    
    output$view <- renderDataTable({
      datatable(final_view())
    })
    
    # output$value2 <- renderText({
    #   c(paste("Class of Final view is", 
    #           class(final_view()) ))
    # })
    
    datasetInput <- reactive({
      nbv_data <- newtable() %>% group_by_("var_bands", input$response) %>%
        summarise(tot = n()) %>% 
        spread_(key = input$response, value = "tot") %>%
        dplyr::rename(zeros = `0`, ones = `1`)
      
      nbv_data[is.na(nbv_data)] <- 0
      n <- nrow(subset_data())
      
      nbv_data <- mutate(nbv_data, tot = zeros + ones,
                         res_rate = round((ones / tot), 4),
                         cust_perc = round((tot / n), 4) ) %>% 
        as.data.frame()
      
      colnames(nbv_data)[1] <- "Attribute"
      new_var <- rep(input$variable, nrow(nbv_data))
      new_var <- data.frame(new_var, 
                            stringsAsFactors = FALSE)
      
      colnames(new_var) <- "Variable_name"
      good_data <- bind_cols(new_var, nbv_data)
      return(good_data)
    })
    
    # For downloading the table
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$variable, ".csv", sep = "")
      },
      content = function(file) {
        write_csv(datasetInput(), file, append = FALSE)
      },
      contentType = "csv"
    )
    
    
    reactive_plot <- function(){
      
      f <- datasetInput()
      f$Attribute %<>% as.character()
      
      p1 <- ggplot(melt(f[, c(2, 5)]), 
                   aes(x = Attribute, y = value)) + 
        geom_bar( stat="identity", position="dodge",
                  colour = "sienna3", fill = "tan1") +
        # scale_fill_grey(start =.1, end = .7 ) +
        xlab("Attribute") + 
        ylab("Total Population") +
        geom_text(aes(label = value, x = Attribute, y = 0.95 * value), 
                  colour = "black") #+ 
      # theme(legend.position="top")
      
      g1 <- ggplotGrob(p1)
      
      # Lineplot ------------------------------------------------
      p2 <- ggplot(f, 
                   aes(x = Attribute, y = res_rate, 
                       group = Variable_name)) + 
        geom_line()  + 
        ylab("Response Rate") +
        geom_text(aes(label = res_rate, x = Attribute, 
                      y = res_rate,
                      group = Variable_name), 
                  colour = "black") +
        theme(panel.background = element_rect(fill = NA),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank())
      g2 <- ggplotGrob(p2)
      
      
      # Add plots together
      pp <- c(subset(g1$layout, name == "panel", se = t:r))
      g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                           pp$l, pp$b, pp$l)
      
      
      # Add second axis for accuracy
      ia <- which(g2$layout$name == "axis-l")
      ga <- g2$grobs[[ia]]
      ax <- ga$children[[2]]
      ax$widths <- rev(ax$widths)
      ax$grobs <- rev(ax$grobs)
      ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
      g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
      g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
      
      
      # Add second y-axis title 
      ia <- which(g2$layout$name == "ylab-l")
      ax <- g2$grobs[[ia]]
      # str(ax) # you can change features (size, colour etc for these - 
      # change rotation below 
      ax$rot <- 270
      g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
      g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
      
      grid.draw(g)
      
      
      # ggplot(datasetInput())  + 
      #   geom_bar(aes(x = Attribute, y = tot,
      #                group = Variable_name),
      #            stat = "identity", fill = "tan1", colour = "sienna3") +
      #   geom_line(aes(x = Attribute, 
      #                 y = res_rate * max(datasetInput()$tot),
      #                 group = Variable_name),
      #                 stat = "identity") +
      #   geom_text(aes(label = res_rate, x = Attribute, 
      #                 y = res_rate * max(datasetInput()$tot)), 
      #             colour = "black") +
      #   geom_text(aes(label = tot, x = Attribute, y = 0.95 * tot), 
      #             colour = "black") +
      #   scale_y_continuous(sec.axis = sec_axis(~./max(datasetInput()$tot)))
    }
    
    output$myplot <- renderPlot({
      reactive_plot()
    })
    
    output$downloadPlot <- downloadHandler(
      filename = "Shinyplot.png",
      content = function(filename) {
        png(filename)
        print(reactive_plot())
        dev.off()
      }
    )
    # output$menuitem <- renderMenu({
    #   menuItem("Menu item", icon = icon("calendar"))
    # })
  }
  
shinyApp(ui, server)
