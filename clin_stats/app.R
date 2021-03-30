packages <- c("shiny", "shinythemes", "shinyjs", "shinyBS", "table1", "MatchIt", "htmltools", "htmlTable", "boot", "DT")
lapply(packages,library,character.only = TRUE)

options(shiny.maxRequestSize= 500*1024^2)
options(shiny.sanitize.errors = TRUE)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = jsResetCode, functions = "reset"),
  tags$head(tags$link(rel="stylesheet", type="text/css",href="styles.css")),
  # Include CSS for table formatting
  includeCSS(system.file(package="table1", "table1_defaults_1.0/table1_defaults.css")),
  theme = shinytheme("superhero"),
  
  # Application title
  titlePanel(title = div("Clinical Descriptive Statistics Application", img(src="gc_logo.png", height=80, width=160, style = "float:right; padding-right:25px"))),
  
  sidebarPanel(
    h4(strong("Input Data File")),
    
    selectInput("file",label= "Select example data file or upload your own data", 
                choices = c("Use example data" = "example", "Upload your data" = "load_your_own"), selected = "load_your_own"),
    
    conditionalPanel("input.file == 'load_your_own'",
                     fileInput('file1', 'Upload data (maximum size 500 MB).', accept=c('.xlsx', '.csv'))),    
    
    conditionalPanel("input.file == 'example'",
                     downloadButton('downloadEx', 'Download example data')),
    hr(),
    
    tags$h4("Overall Variable Section:"),
    
    checkboxInput("split_flag", "Check to report by group", value = TRUE),
    
    uiOutput("pop_grpvar"),
    
    # textAreaInput("grp_var", "Enter Group Variable Label", value = ""),
    
    bsTooltip("grp_var", "Entered label must be seperated by comma", placement = "bottom", trigger = "hover"),
    
    uiOutput("pop_tblvars"),
    
    hr(),
    tags$h4("Continuous Variable Section:"),
    
    actionButton("sel", "Add Variable Dropdown Menu"),
    bsTooltip("sel", "Click however many times as the number of continuous variables in your output shell", placement = "bottom", trigger = "hover"),
    actionButton("obs","Add Label Box"),
    bsTooltip("obs", "This button must be clicked the same times as you click the left button", placement = "bottom", trigger = "hover"),
    
    # actionButton("add_units", "Add Variable Units"),
    
    htmlOutput("selectInputs"),
    
    htmlOutput("selectInputs2"),
    
    bsTooltip("selectInputs2", "Entered labels must be seperated by comma", placement = "bottom", trigger = "hover"),
    
    # htmlOutput("selectInputs4"),
    
    # radioButtons("stat_method", label = "Statistical Method", choices = c("T-Test", "ANOVA"), selected = "ANOVA", inline = TRUE),
    uiOutput("pop_method"),
    
    hr(),
    tags$h4("Categorical Variable Section:"),
    
    actionButton("sel_cat", "Add Variable Dropdown Menu"),
    bsTooltip("sel_cat", "Click however many times as the number of categorical variables in your output shell", placement = "bottom", trigger = "hover"),
    actionButton("obs1","Add Label Box"),
    
    bsTooltip("obs1", "This button must be clicked as double times as you click the left button", placement = "bottom", trigger = "hover"),
    
    htmlOutput("selectInputs1"),
    
    htmlOutput("selectInputs3"),
    
    bsTooltip("selectInputs3", "Entered labels must be seperated by comma", placement = "bottom", trigger = "hover"),
    
    # radioButtons("stat_method1", label = "Statistical Method", choices = c("Chi-Sq Test", "Fisher's Exact Test"), selected = "Chi-Sq Test", inline = TRUE),
    uiOutput("pop_method1"),
    
    hr(),
    
    tags$h4("Other Section:"),
    
    # radioButtons("last_col", label = "Last Column Choice", choices = c("P-Value", "Total"), selected = "P-Value", inline = TRUE),
    uiOutput("pop_lastcol"),
    textAreaInput("footnote", "Enter table footenote here", value = ""),
    
    actionButton("goButton", "Generate Table"),
    
    hr(),
    tags$h4("Restart Application"),

    actionButton("reset_button", "Reset")
    
  ),
  
  mainPanel(
    uiOutput("tb")
  )
)

server <- function(input, output, session) {
  observeEvent(input$reset_button, {js$reset()})
  
  data_input <- reactive({
    if(input$file == 'example'){
      data <- melanoma
    }
    else if(input$file == 'load_your_own'){
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      else if(grepl(".xlsx", inFile[1])) { data = read.xlsx(as.character(inFile$datapath), colNames = TRUE, rowNames = F, as.is = T) }
      else if(grepl(".csv", inFile[1])) { data = read.csv(as.character(inFile$datapath), header = TRUE, sep = ",", stringsAsFactors = F, as.is = T, fill = T) }
    }
    else 
      return(NULL)
    # dim(data)
    dataset <- data.frame(data)
    return(as.data.frame(dataset))
  })
  
  output$downloadEx <- downloadHandler(
    
    filename <- function() {
      paste('Example ds data', Sys.time(),'.csv', sep='')
    },
    content <- function(file) {
      ds <- data_input()
      write.csv(ds, file, row.names = FALSE)
    }
  )
  
  # Displaying Data  
  output$data_display <- DT::renderDataTable({
    data_input()
  })
  
  output$pop_grpvar <- renderUI({
    if (is.null(data_input()) | input$split_flag == FALSE) {return(NULL)}
    else {
      return(list(selectInput("sel_grpvar", "Select Group Variable", choices = colnames(data_input()) ),
                  textAreaInput("grp_var", "Enter Group Variable Label", value = "")
                  ))
    }
  })
  
  output$pop_tblvars <- renderUI({
    if (is.null(data_input())) {return(NULL)}
    else {
      return(selectInput("allvars", "Select both continuous and categorical variables", choices = colnames(data_input()), multiple = TRUE))
    }
  })
  
  output$pop_lastcol <- renderUI({
    if (input$split_flag == FALSE) {return(NULL)}
    else {
      return(
        radioButtons("last_col", label = "Last Column Choice", choices = c("P-Value", "Total"), selected = "P-Value", inline = TRUE)
      )
    }
  })
  
  output$pop_method <- renderUI({
    if (input$split_flag == FALSE) {return(NULL)}
    else {
      return(
        radioButtons("stat_method", label = "Statistical Method", choices = c("T-Test", "ANOVA"), selected = "ANOVA", inline = TRUE)
      )
    }
  })
  
  output$pop_method1 <- renderUI({
    if (input$split_flag == FALSE) {return(NULL)}
    else {
      return(
        radioButtons("stat_method1", label = "Statistical Method", choices = c("Chi-Sq Test", "Fisher's Exact Test"), selected = "Chi-Sq Test", inline = TRUE)
      )
    }
  })
  
  observe({
    if (input$sel == 0)
      return(NULL)
    isolate({

      output$selectInputs <- renderUI({
        s <- ""
        for(i in 1:input$sel) {
          s <- paste(s, selectInput(paste("cont_var", i, sep = ""), paste("Variable", i, sep = ""), choices = colnames(data_input()) ))
        }
        HTML(s)
      })
      outputOptions(output, 'selectInputs', suspendWhenHidden=FALSE)
    })
  })
  
  observe({
    
    if (input$obs == 0) 
      return(NULL)
    isolate({
      
      output$selectInputs2 <- renderUI({
        w <- ""
        for(i in 1:input$obs) {
          w <- paste(w, textInput(paste("a", i, sep = ""), paste("Label", i, sep = ""), value = input[[sprintf("a%d",i)]]))
        }
        HTML(w)
      })
      outputOptions(output, 'selectInputs2', suspendWhenHidden=FALSE)
    })
  })
  
  # observe({
  #   
  #   if (input$add_units == 0) 
  #     return(NULL)
  #   isolate({
  #     
  #     output$selectInputs4 <- renderUI({
  #       k <- ""
  #       for(i in 1:input$add_units) {
  #         k <- paste(k, textInput(paste("b", i, sep = ""), paste("Label", i, sep = ""), value = input[[sprintf("b%d",i)]]))
  #       }
  #       HTML(k)
  #     })
  #     outputOptions(output, 'selectInputs4', suspendWhenHidden=FALSE)
  #   })
  # })
  
  observe({
    if (input$sel_cat == 0)
      return(NULL)
    isolate({
      
      output$selectInputs1 <- renderUI({
        r <- ""
        for(i in 1:input$sel_cat) {
          r <- paste(r, selectInput(paste("cat_var", i, sep = ""), paste("Variable", i, sep = ""), choices = colnames(data_input()) ))
        }
        HTML(r)
      })
      outputOptions(output, 'selectInputs1', suspendWhenHidden=FALSE)
    })
  })
  
  observe({
    
    if (input$obs1 == 0) 
      return()
    isolate({
      
      output$selectInputs3 <- renderUI({
        w <- ""
        for(i in 1:input$obs1) {
          w <- paste(w, textInput(paste("aa", i, sep = ""), paste("Label", i, sep = ""), value = input[[sprintf("aa%d",i)]]))
        }
        HTML(w)
      })
      
      outputOptions(output, 'selectInputs3', suspendWhenHidden=FALSE)
    })
  })
  
  selectedData <- eventReactive(input$goButton, {
    
    melanoma2 <- data_input()
    
    # Factor the basic variables that
    # we're interested in
    if (input$split_flag == TRUE) {
      melanoma2[[input$sel_grpvar]] <- 
        factor(melanoma2[[input$sel_grpvar]], 
               levels=sort(unique(melanoma2[[input$sel_grpvar]])),   #2,1,3
               labels=unlist(strsplit(input$grp_var, ","))
               )
    }
    
    # melanoma2$sex <- 
    #   factor(melanoma2$sex, levels=c(1,0),
    #          labels= unlist(strsplit(input$aa2, ",")))
    # 
    # melanoma2$ulcer <- 
    #   factor(melanoma2$ulcer, levels=c(0,1),
    #          labels= unlist(strsplit(input$aa4, ",")))
    
      for (j in seq(input$sel_cat)) {
        melanoma2[[input[[paste("cat_var", j, sep = "")]]]] <- 
          factor(melanoma2[[input[[paste("cat_var", j, sep = "")]]]], levels=sort(unique(melanoma2[[input[[paste("cat_var", j, sep = "")]]]])),
                 labels= unlist(strsplit(input[[paste("aa", 2*j, sep = "")]], ",")))
      }
    
    
    for (i in seq(input$sel)) {
      label(melanoma2[[input[[paste("cont_var", i, sep = "")]]]]) <- input[[paste("a", i, sep = "")]]
    }
    
    # label(melanoma2[[input[["cont_var1"]]]]) <- input[["a1"]]
    # label(melanoma2[[input[["cont_var2"]]]]) <- input[["a2"]]
    
    
      for(j in seq(input$sel_cat)) {
          label(melanoma2[[input[[paste("cat_var", j, sep = "")]]]]) <- input[[paste("aa", 2*j-1, sep = "")]]
      }
    
    
    
    # label(melanoma2$sex)       <- input$aa1
    # label(melanoma2$ulcer)     <- input$aa3
    
    # units(melanoma2$age)       <- "years"
    # units(melanoma2$thickness) <- "mm"
    
    
    
    # for (i in seq(input$sel)) {
    #   units(melanoma2[[input[[paste("cont_var", i, sep = "")]]]]) <- input[[paste("b", i, sep = "")]]
    # }
    
    pvalue <- function(x, ...) {
      # Construct vectors of data y, and groups (strata) g
      y <- unlist(x)
      g <- factor(rep(1:length(x), times=sapply(x, length)))
      if (is.numeric(y) & input$stat_method=="T-Test") {
        # For numeric variables with 2 groups, perform a standard 2-sample t-test
        p <- t.test(y ~ g)$p.value
      }
      else if (is.numeric(y) & input$stat_method=="ANOVA") {
        # For numeric variables with more than 2 groups, perform a standard ANOVA test
        p <- summary(aov(y ~ g))[[1]][["Pr(>F)"]][1]
      } 
      else {
        if (input$stat_method1=="Chi-Sq Test") {
          # For categorical variables, perform a chi-squared test of independence
          p <- chisq.test(table(y, g))$p.value
        }
        else if (input$stat_method1=="Fisher's Exact Test") {
          # For categorical variables with small sample size(usually less than 10), perform Fisher's exact test
          p <- fisher.test(table(y, g))$p.value
        }
      }
      # Format the p-value, using an HTML entity for the less-than sign.
      # The initial empty string places the output on the line below the variable label.
      c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
    }
    
    if (input$split_flag == TRUE) {
      if (input$last_col == "P-Value") {
        out <- table1(as.formula(paste(paste(" ~ ", paste(input$allvars, collapse = " + "), "|", input$sel_grpvar))), data=melanoma2, overall=F, extra.col=list(`P-value`=pvalue), footnote = input$footnote)
      }
      else if (input$last_col == "Total") {
        out <- table1(as.formula(paste(paste(" ~ ", paste(input$allvars, collapse = " + "), "|", input$sel_grpvar))), data=melanoma2, overall="Total", footnote = input$footnote)
      }
    }
    else {
      out <- table1(as.formula(paste(" ~ ", paste(input$allvars, collapse = " + "))), data=melanoma2, overall="No. of patients", footnote = input$footnote)
    }
  
    
    # html_out <- HTML(
    #   htmlTable(out)
    # )

    css_rule <- '<link href="data:text/css,%0A%2ERtable1%20table%2C%20table%2ERtable1%20%7B%0Afont%2Dfamily%3A%20%22Arial%22%2C%20Arial%2C%20sans%2Dserif%3B%0Afont%2Dsize%3A%2010pt%3B%0Aborder%2Dcollapse%3A%20collapse%3B%0Apadding%3A%200px%3B%0Amargin%3A%200px%3B%0A%0A%7D%0A%2ERtable1%20td%20%7B%0Awhite%2Dspace%3Anowrap%3B%0A%7D%0A%2ERtable1%20th%2C%20%2ERtable1%20td%20%7B%0Aborder%3A%200%3B%0Atext%2Dalign%3A%20center%3B%0Apadding%3A%200%2E5ex%201%2E5ex%3B%0Amargin%3A%200px%3B%0A%7D%0A%2ERtable1%20thead%3Etr%3Afirst%2Dchild%3Eth%20%7B%0Aborder%2Dtop%3A%202pt%20solid%20black%3B%0A%7D%0A%2ERtable1%20thead%3Etr%3Alast%2Dchild%3Eth%20%7B%0Aborder%2Dbottom%3A%201pt%20solid%20black%3B%0A%7D%0A%2ERtable1%20tbody%3Etr%3Alast%2Dchild%3Etd%20%7B%0Aborder%2Dbottom%3A%202pt%20solid%20black%3B%0A%7D%0A%2ERtable1%20th%2Egrouplabel%20%7B%0Apadding%2Dleft%3A%200%3B%0Apadding%2Dright%3A%200%3B%0A%7D%0A%2ERtable1%20th%2Egrouplabel%3Ediv%20%7B%0Amargin%2Dleft%3A%201%2E5ex%3B%0Amargin%2Dright%3A%201%2E5ex%3B%0Aborder%2Dbottom%3A%201pt%20solid%20black%3B%0A%7D%0A%2ERtable1%20th%2Egrouplabel%3Alast%2Dchild%3Ediv%20%7B%0Amargin%2Dright%3A%200%3B%0A%7D%0A%2ERtable1%20%2Erowlabel%20%7B%0Atext%2Dalign%3A%20left%3B%0Apadding%2Dleft%3A%202%2E5ex%3B%0A%7D%0A%2ERtable1%20%2Efirstrow%2Erowlabel%20%7B%0Apadding%2Dleft%3A%200%2E5ex%3B%0Afont%2Dweight%3A%20bold%3B%0A%7D%0A%0A%2ERtable1%2Dzebra%20tbody%20tr%3Anth%2Dchild%28odd%29%20%7B%0Abackground%2Dcolor%3A%20%23eee%3B%0A%7D%0A%0Atable%2ERtable1%2Dtimes%20%7B%0Afont%2Dfamily%3A%20%22Times%20New%20Roman%22%2C%20Times%2C%20serif%3B%0A%7D%0A%0A%2ERtable1%2Dshade%20th%20%7B%0Abackground%2Dcolor%3A%20%23ccc%3B%0A%7D%0A%0A%2ERtable1%2Dgrid%20th%2C%20%2ERtable1%2Dgrid%20td%20%7B%0Aborder%2Dleft%3A%201pt%20solid%20black%3B%0Aborder%2Dright%3A%201pt%20solid%20black%3B%0A%7D%0A%2ERtable1%2Dgrid%20thead%3Etr%3Afirst%2Dchild%3Eth%20%7B%0Aborder%2Dtop%3A%201pt%20solid%20black%3B%0A%7D%0A%2ERtable1%2Dgrid%20thead%3Etr%3Alast%2Dchild%3Eth%20%7B%0Aborder%2Dbottom%3A%201pt%20solid%20black%3B%0A%7D%0A%2ERtable1%2Dgrid%20tbody%3Etr%3Alast%2Dchild%3Etd%20%7B%0Aborder%2Dbottom%3A%201pt%20solid%20black%3B%0A%7D%0A%2ERtable1%2Dgrid%20%2Efirstrow%2C%20%2ERtable1%2Dgrid%20%2Efirstrow%20%7E%20td%20%7B%0Aborder%2Dtop%3A%201pt%20solid%20black%3B%0A%7D%0A%2ERtable1%2Dgrid%20th%2Egrouplabel%3Ediv%20%7B%0Amargin%2Dleft%3A%200%3B%0Amargin%2Dright%3A%200%3B%0Aborder%2Dbottom%3A%200%3B%0A%7D%0A%0A%2ERtable1%2Dcenter%20td%2Erowlabel%2C%20%2ERtable1%2Dcenter%20td%2Efirstrow%2Erowlabel%20%7B%0Afont%2Dweight%3A%20bold%3B%0Atext%2Dalign%3A%20center%3B%0Apadding%3A%200%2E5ex%201%2E5ex%3B%0A%7D%0A%0A%2ERtable1%2Dfootnote%20%7B%0Afont%2Dsize%3A%20smaller%3B%0Apadding%3A%200px%3B%0Amargin%3A%200px%3B%0A%7D%0A" rel="stylesheet">'
    out[1] <- paste0(out[1], css_rule)
    save_html(out, "/Users/jian.yang/desktop/Work/R Shiny  Stuff/clinical_report_app/www/report.html", lang = "en")
    return(out)
    # return(includeHTML("/Users/jian.yang/desktop/Work/R Shiny  Stuff/clinical_report_app/www/report.html"))
    
  })  
  
  output$table <- renderUI({
    selectedData()
  })
  
  output$downloadrpt <- downloadHandler(
    filename <- function() {
      paste("report", "html", sep=".")
    },
    
    content <- function(file) {
      file.copy("www/report.html", file, overwrite = TRUE)
    }
  )
  
  output$caption <- renderText({paste("This APP is developed by Jason Yang. It is used to generate clinical descriptive statistics report. For any technical issue, please contact jian.yang@genomicarebio.com, thank you!")})
  
  
  output$tb <- renderUI({
    if(is.null(data_input()))
      h5("Powered by", tags$img(src='R_logo.png', height=200, width=200), tags$img(src='Shiny1.png', heigth=210, width=210))
    else
      tabsetPanel(
        tabPanel("Data Table", DT::dataTableOutput("data_display")),
        tabPanel("Descriptive Statistics Report", htmlOutput("table"), br(), downloadButton("downloadrpt", label = "Download Table")),
        tabPanel("About the APP", textOutput("caption"))
      )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)


