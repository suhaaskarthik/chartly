library(shiny)
library(plotly)
library(dplyr)
library(lubridate)
library(jsonlite)
library(tidyverse)
library(readxl)
library(randomcoloR)
library(colourpicker)
library(shinyFiles)
no_of_clicks<<-0
no_of_filters<<-0
ui<- fluidPage(
    sidebarPanel( selectInput('type','Type of file', choices = c('excel','text file')),uiOutput('url'),tableOutput('t'),uiOutput('sep'), uiOutput('sep2'),uiOutput('tables'),uiOutput('file'),uiOutput('username'),uiOutput('header'), actionButton('go', 'retrieve data')),
    tabsetPanel(id = 'tabs' ,type = 'tab', tabPanel('File',icon=icon('file-alt'),uiOutput('text'), uiOutput('textly'), tags$ul(style="list-style: none;",tags$li( uiOutput('filter_column1'),uiOutput('filter_type1'),uiOutput('filter_sign1'),uiOutput('filter_value1'),uiOutput('filter_value_x1'),uiOutput('filter_value_y1'),uiOutput('filter_value_z1')), tags$style('#filter_column1{display:inline-block;}#filter_type1{display:inline-block;width:200px;}#filter_sign1{display:inline-block; width:100px;}#filter_value1{display:inline-block;}#filter_value_x1{display:inline-block;}#filter_value_y1{display:inline-block;}#filter_value_z1{display:inline-block;}'),tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; }"),tags$li(uiOutput('filter_column2'),uiOutput('filter_type2'),uiOutput('filter_sign2'),uiOutput('filter_value2'),uiOutput('filter_value_x2'),uiOutput('filter_value_y2'),uiOutput('filter_value_z2')), tags$style('#filter_column2{display:inline-block;}#filter_type2{display:inline-block;width:200px;}#filter_sign2{display:inline-block; width:100px;}#filter_value2{display:inline-block;}#filter_value_x2{display:inline-block;}#filter_value_y2{display:inline-block;}#filter_value_z2{display:inline-block;}'), tags$li(uiOutput('filter_column3'),uiOutput('filter_type3'),uiOutput('filter_sign3'),uiOutput('filter_value3'),uiOutput('filter_value_x3'),uiOutput('filter_value_y3'),uiOutput('filter_value_z3')), tags$style('#filter_column3{display:inline-block;}#filter_type3{display:inline-block;width:200px;}#filter_sign3{display:inline-block; width:100px;}#filter_value3{display:inline-block;}#filter_value_x3{display:inline-block;}#filter_value_y3{display:inline-block;}#filter_value_z3{display:inline-block;}'),tags$li(uiOutput('filter_column4'),uiOutput('filter_type4'),uiOutput('filter_sign4'),uiOutput('filter_value4'),uiOutput('filter_value_x4'),uiOutput('filter_value_y4'),uiOutput('filter_value_z4')), tags$style('#filter_column4{display:inline-block;}#filter_type4{display:inline-block;width:200px;}#filter_sign4{display:inline-block; width:100px;}#filter_value4{display:inline-block;}#filter_value_x4{display:inline-block;}#filter_value_y4{display:inline-block;}#filter_value_z4')),uiOutput('add_filter'),uiOutput('remove_filter'), dataTableOutput('input_file')),tabPanel('Options',icon=icon('align-justify'), uiOutput('x_axis'),uiOutput('y_axis'), uiOutput('aggregate'), uiOutput('date_format'),uiOutput('xlab'),uiOutput('ylab'), uiOutput('title'),uiOutput('graph_image'),uiOutput('graph_type'),uiOutput('graph_icon'),  tags$style('#graph_image{display:inline-block}#graph_type{display:inline-block}')), tabPanel('More Features',icon=icon('plus'), colourInput('chart_colour', 'Chart colour', 'blue'),uiOutput('categorise') ,uiOutput('date_format_cat'),uiOutput('size'), tags$ul(style="list-style: none;",tags$li(uiOutput('preceding1'),uiOutput('column1'),uiOutput('succeeding1'), uiOutput('unit1')), tags$style('#preceding1{display:inline-block;width:150px;}#column1{display:inline-block}#succeeding1{display:inline-block; width:150px;}#unit1{display:inline-block; width:150px;}'),tags$li(uiOutput('preceding2'),uiOutput('column2'),uiOutput('succeeding2'),uiOutput('unit2')),  tags$style('#preceding2{display:inline-block;width:150px;}#column2{display:inline-block;}#succeeding2{display:inline-block;width:150px;}#unit2{display:inline-block;width:150px;}'), tags$li(uiOutput('preceding3'), uiOutput('column3'),uiOutput('succeeding3'),uiOutput('unit3')), tags$style('#preceding3{display:inline-block;width:150px;}#column3{display:inline-block}#succeeding3{display:inline-block;width:150px;}#unit3{display:inline-block;width:150px;}'),tags$li(uiOutput('preceding4'),uiOutput('column4'),uiOutput('succeeding4'),uiOutput('unit4'))), tags$style('#preceding4{display:inline-block;width:150px;}#column4{display:inline-block}#succeeding4{display:inline-block;width:150px;}#unit4{display:inline-block;width:150px;}'),uiOutput('add_label'),uiOutput('remove_label'),uiOutput('colour_scale'), uiOutput('categorise_as_label'),uiOutput('cat_preceding'), uiOutput('cat_column'),uiOutput('cat_succeeding'),uiOutput('x_axis_as_label'),uiOutput('x_preceding'), uiOutput('x_column'),uiOutput('x_succeeding'),  uiOutput('label_text'),  uiOutput('labels'),uiOutput('scope'), uiOutput('category_type'), uiOutput('a_d'),uiOutput('gridline'), uiOutput('animate'),tags$div( uiOutput('animate_column'),uiOutput('transition_time'),uiOutput('slider_visibility'),uiOutput('date_format_animate'), style='background-color:lightgrey;width:400px;border-left: 10px solid lightgrey;border-top: 10px solid lightgrey;border-bottom: 10px solid lightgrey;'), uiOutput('bin_size'),uiOutput('hole'),uiOutput('trend_lines'),uiOutput('customised_val'),colourInput('line_colour', 'Target Line colour', 'black'),uiOutput('trend_line_as_label'),uiOutput('trend_preceding'),uiOutput('trend_column'), uiOutput('trend_succeeding'),uiOutput('trend_unit'), tags$style('#trend_preceding{display:inline-block;width:150px;}#trend_column{display:inline-block}#trend_succeeding{display:inline-block; width:150px;}#unit1{display:inline-block; width:150px;}#trend_unit{display:inline-block;width:150px;}'),tags$style('#cat_preceding{display:inline-block;width:150px;}#cat_column{display:inline-block}#cat_succeeding{display:inline-block;width:150px;}#cat_unit{display:inline-block;width:150px;}'),br(),tags$style('#x_preceding{display:inline-block;width:150px;}#x_column{display:inline-block}#x_succeeding{display:inline-block;width:150px;}')) ,tabPanel('Graph',icon= icon('chart-bar'),uiOutput('refresh'),uiOutput('export'), uiOutput('par_num'),uiOutput('par_cat'),downloadButton('download','Download as html'), plotlyOutput( 'plotput'))))
server<- function(input,output,session){
  
  is_prime <- function(num) {
    not_prime<-vector()
    prime<-vector()
    for(val in num){
      if(is.na(val)|is.null(val)){
        not_prime[length(not_prime)+1]<- val
      }else{
        if (val == 2) {
          prime[length(prime)+1]<- val
        } else if (any(val %% 2:(val-1) == 0)) {
          not_prime[length(not_prime)+1]<- val
        } else { 
          prime[length(prime)+1]<- val
        }
      }
    }
    prime
  }
  vowel_count<- function(vec){
    words<- vector()
    for(word in vec){
      if(is.na(word)){
        count<- 0
      }else{
        count<- 0
        for(letter in strsplit(word, '')[[1]]){
          if(tolower(letter)%in%c('a', 'e', 'i', 'o', 'u')){
            count<- count+1
          }
        }
      }
      words[length(words)+1]<-count
    }
    words
  }
  consonant_count<- function(vec){
    '%ni%'<-Negate('%in%')
    words<- vector()
    for(word in vec){
      count<- 0
      for(letter in strsplit(word, '')[[1]]){
        if(tolower(letter)%ni%c('a', 'e', 'i', 'o', 'u')){
          count<- count+1
        }
      }
      words[length(words)+1]<- count
    }
    words
  }
  output$header<- renderUI({
    checkboxInput('header', 'Header',value=T)
  })
  output$sep<- renderUI({
    if (input$type == 'text file'){
      radioButtons('sep', 'separator(delimiter)',choices = c(comma = ',', tab = '\t', new_line = '\n', minus = '-', period = '.', tilde = '~','Not here'))
    }
    
  })


  output$file<- renderUI({
    if(input$type=='excel'){
      fileInput('file' , 'browse from your pc',accept = '.xlsx')
    }else{
      fileInput('file' , 'browse from your pc')
    }
    
    
  })
  output$url<- renderUI({
    if (input$type == 'google sheets'|input$type == 'from the web'|input$type == 'pdf'){
      
      textInput('url' , 'Please give your url')
    }
    
  })
  output$username<- renderUI({
    if (input$type == 'google sheets'){
      textInput('username' , 'Please email id')
    }
  })

    
  output$sep2<- renderUI({
    if (input$type == 'text file'){
      if(input$sep == 'Not here'){
        textInput('sep2', 'Is delimiter not available? write it here',width = '100')
      }
      
    }
    
  })


  observeEvent(input$go,{
    read_this<- function(file_type, separator, file_, separator2){
      if (file_type == 'text file'){
        if(separator == 'Not here'){
          chartly_dataset<<- read.table((file_)$datapath, sep = separator2, header = input$header)
        }else{
          chartly_dataset<<- read.table((file_)$datapath, sep = separator, header = input$header)
        }
      }
      else if (file_type == 'excel'){
        chartly_dataset<<- read_excel((file_)$datapath,col_names = input$header)
      }
    }
    tidying<- function(dataset_){
      date_format<<-vector()
      for(collumn in names(dataset_)){
        NAdata<- ifelse(as.character(dataset_[[collumn]]) == ''|is.null(as.character(dataset_[[collumn]])) | is.nan(as.character(dataset_[[collumn]])), NA,as.character(dataset_[[collumn]]) )
        dataset_<- dataset_%>%mutate(!!collumn := NAdata)
      }
      '%ni%'<- Negate('%in%')
      dates<- vector()
      for(collumn in names(dataset_)){
        pat1<- '[[:punct:]]\\s*\\d{1,4}\\s*[[:punct:]]'
        date_or_not<- str_detect(dataset_[[collumn]], pat1)
        if(mean(date_or_not, na.rm = TRUE)<=1 & mean(date_or_not, na.rm = TRUE)>=0.9){
          dates[length(dates)+1]<- collumn
        }
      }
      for(date in dates){
        if(mean(is.na(dmy(dataset_[[date]])) == TRUE)<=0.01){
          values<- dmy(dataset_[[date]])
          dataset_<- dataset_%>%mutate(!!date := dmy(dataset_[[date]]))
          date_format[date]<<-'dd-mm-yyyy'
        }
        else if(mean(is.na(mdy(dataset_[[date]])) == TRUE)<=0.01){
          values<- mdy(dataset_[[date]])
          dataset_<- dataset_%>%mutate(!!date := mdy(dataset_[[date]]))
          date_format[date]<<-'mm-dd-yyyy'
        }
        else if(mean(is.na(ymd(dataset_[[date]])) == TRUE)<=0.01){
          values<- ymd(dataset_[[date]])
          dataset_<- dataset_%>%mutate(!!date := values)
          date_format[date]<<-'yyyy-mm-dd'
        }
        else if(mean(is.na(ydm(dataset_[[date]])) == TRUE)<=0.01){
          values<- ydm(dataset_[[date]])
          dataset_<- dataset_%>%mutate(!!date := ydm(dataset_[[date]]))
          date_format[date]<<-'yyyy-dd-mm'
        }
        else if(mean(is.na(myd(dataset_[[date]])) == TRUE)<=0.01){
          values<- myd(dataset_[[date]])
          dataset_<- dataset_%>%mutate(!!date := myd(dataset_[[date]]))
          date_format[date]<<-'mm-yyyy-dd'
        }
        else if(mean(is.na(dym(dataset_[[date]])) == TRUE)<=0.01){
          values<- dym(dataset_[[date]])
          dataset_<- dataset_%>%mutate(!!date := dym(dataset_[[date]]))
          date_format[date]<<-'dd-yyyy-mm'
        }
      }
      for(collumn in names(dataset_)){
        if(collumn %ni% dates){
          parsed_num<- gsub(',', '', dataset_[[collumn]])
          parsed_num<- gsub('%', '', parsed_num)
          parsed_num<- gsub('$', '', parsed_num)
          parsed_num<- gsub('£', '', parsed_num)
          dataset_<- dataset_%>%mutate(!!collumn := parsed_num)
        }
      }
      
      for(collumn in names(dataset_)){
        if(collumn %ni% dates){
          if(mean(is.na(as.numeric(dataset_[[collumn]][is.na(dataset_[[collumn]]) == FALSE])) == TRUE)<= 0.1){
            dataset_<- dataset_%>%mutate(!!collumn := as.numeric(dataset_[[collumn]]))
          }
          else if(mean(str_detect(dataset_[[collumn]][is.na(dataset_[[collumn]]) == FALSE], '^[[:punct:]]?[[:digit:]]+[[:blank:]]+') == FALSE)<=0.1 | mean(str_detect(dataset_[[collumn]][is.na(dataset_[[collumn]]) == FALSE], '[[:blank:]]+[[:punct:]]?[[:digit:]]+$') == FALSE)<=0.1){
            cleaned_collumn<- parse_number(dataset_[[collumn]])
            dataset_<- dataset_%>%mutate(!!collumn := cleaned_collumn)
          }else{
            dataset_<- dataset_%>%mutate(!!collumn := dataset_[[collumn]])
          }
        }
      }
      collumn_datatype<<- vector()
      collumn_dc<<- vector()
      is_year<<-vector()
      for(collumn in names(dataset_)){
        
        collumn_datatype[length(collumn_datatype)+1]<<- paste('(',class(dataset_[[collumn]]),')', sep = '')
        if(class(dataset_[[collumn]]) == 'character'){
          collumn_dc[length(collumn_dc)+1] <<- 'categorical'
        }else if (class(dataset_[[collumn]]) == 'numeric'){
          if(length(unique(dataset_[[collumn]]))/length(dataset_[[collumn]])<= 0.07){
            collumn_dc[length(collumn_dc)+1] <<- 'categorical'
            dataset_<- dataset_%>%mutate(!!collumn := as.character(dataset_[[collumn]]))
          }else{
            collumn_dc[length(collumn_dc)+1] <<- 'numeric'
          }
          
        }else{
          collumn_dc[length(collumn_dc)+1] <<- 'Date'
        }
        
      }
      dataset_
    }
    updateTabsetPanel(session,'tabs', 'Options')
    var<- FALSE
    tryCatch({
      read_this(input$type,input$sep, input$file, input$sep2)
      tidying(chartly_dataset)
    },
    error = function(err){
      print('there is a problem')
      var<<- TRUE
      showNotification('There is an error, maybe this is beause you chose \n you exceeded the file size limit \n or chose the wrong format \n or did not choose the header option (for text file)', type = 'error')
    })
    print(var)
    if(var==FALSE){
      print('hi there')
      col_type<- vector()
      data<- tidying(chartly_dataset)
      options<- paste(names(data), '(', collumn_dc, ')')
      col_type[options]<- names(data)
      
      
      no_of_clicks<<-0
      output$file<- renderUI({
        print('')
        
        
      })
      
      output$x_axis<- renderUI({
        
        if(is.null(input$file)==TRUE){
          return()
        }
        options<- paste(names(data), '(',collumn_dc, ')')
        selectInput('x_axis', 'X axis', choices = options)
        
      })
      output$graph_type<- renderUI({
        options<- paste(names(data), '(', collumn_dc, ')')
        col_type<- vector()
        col_type[options]<- names(data)
        collumn_type<- vector()
        collumn_type[names(data)]<- collumn_datatype
        col_dc<- vector()
        col_dc[names(data)]<- collumn_dc
        graph_options<- vector()
        usa_data<- read_excel('usa_code.xlsx')
        country_data<- read.csv('country_code.csv')
        if(col_dc[col_type[input$x_axis]] == 'categorical' |col_dc[col_type[input$x_axis]] == 'Date'){
          graph_options[length(graph_options)+1]<- 'Box plot'
        }
        if(col_dc[col_type[input$x_axis]] == 'categorical' |col_dc[col_type[input$x_axis]] == 'Date'){
          graph_options[length(graph_options)+1]<- 'Bar graph'
        }
        if(col_dc[col_type[input$x_axis]] == 'categorical' |col_dc[col_type[input$x_axis]] == 'Date'){
          graph_options[length(graph_options)+1]<- 'Pie chart'
        }
        if(col_dc[col_type[input$x_axis]] == "numeric" ){
          graph_options[length(graph_options)+1]<- 'Scatter plot'
        }
        if(col_dc[col_type[input$x_axis]] == "Date"){
          graph_options[length(graph_options)+1]<- 'Time series plot'
        }
        is_year<- vector()
        for(collumn in names(data)){
          if(mean(is.na(as.numeric(data[[collumn]]))==TRUE)<=0.07&col_dc[collumn] != "Date"){
            if(mean(nchar(data[[collumn]])==4,na.rm = TRUE)>=0.93&mean(all.equal(as.numeric(data[[collumn]]), as.integer(data[[collumn]]))==TRUE, na.rm = TRUE)>0.93){
              is_year[length(is_year)+1]<- collumn
            }
          }
        }
        print(is_year)
        if(col_type[input$x_axis]%in%is_year){
          graph_options[length(graph_options)+1]<- 'Time series plot'
        }
        if(col_dc[col_type[input$x_axis]] == "numeric"){
          graph_options[length(graph_options)+1]<- 'Histogram'
        }
        if(col_dc[col_type[input$x_axis]] == "numeric"){
          graph_options[length(graph_options)+1]<- 'Histogram'
        }
        if(col_dc[col_type[input$x_axis]]=='categorical'){
          count<-0
          for(val in unique(data[[col_type[input$x_axis]]])){
            if(tolower(val)  %in%tolower(country_data$Country)){
              count<- count+1
            }
          }
          print(count)
          if(count>100){
            graph_options[length(graph_options)+1]<- 'World Map'
          }
          
        }
        if(col_dc[col_type[input$x_axis]]=='categorical'){
          count<-0
          for(val in unique(data[[col_type[input$x_axis]]])){
            if(tolower(val)  %in%tolower(usa_data$State)){
              count<- count+1
            }
          }
          print(count)
          if(count>40){
            graph_options[length(graph_options)+1]<- 'USA Map'
          }
          
        }
        selectInput('graph_type', 'Graph', choices =  graph_options, selected = 'Bar graph')
      })
      output$aggregate<- renderUI({
        if(input$graph_type=='Histogram'|input$graph_type=='Box plot'|input$graph_type=='Scatter plot'){
          return()
        }
        aggregate<- c('Sum', 'Count', 'Median', 'Average')
        selectInput('aggregate', 'Group by', choices = aggregate)
      })
      output$y_axis<- renderUI({
        if(input$graph_type == 'Histogram'|input$graph_type == 'Density plot'){
        }else{
          
          if(input$graph_type=='Histogram'|input$graph_type=='Density plot'|input$graph_type=='Scatter plot'|input$graph_type== 'Box plot' ){
            
          }else{
            if(input$aggregate=='Count'){
              return()
            }
          }
          col_type<- vector()
          options<- paste(names(data), '(', collumn_dc, ')')
          col_type[options]<- names(data)
          numeric_or_not<- str_detect(options, '( numeric )')
          selectInput('y_axis', 'Y axis', choices = options[numeric_or_not])
        }
        
      })
      
      output$scope<- renderUI({
        if(input$graph_type=='World Map'){
          radioButtons('scope', 'Scope', choices=c('all','asia', 'africa', 'south america', 'north america', 'europe'))
        }
      })
      
      output$animate<- renderUI({
        if(input$graph_type=='Histogram'|input$graph_type=='Time series plot'){
          return()
        }
        if(input$graph_type=='Bar graph'|input$graph_type=='Box plot'){
          if(input$categorise!='None'){
            return()
          }
        }
        columns<-vector()
        col_type<- vector()
        options<- paste(names(data), '(', collumn_dc, ')')
        col_type[options]<- names(data)
        for(col in names(data)){
          if(col==col_type[input$x_axis]|col==col_type[input$y_axis]){
            
          }else{
            columns[length(columns)+1]<- col
          }
        }
        if(length(columns)==0){
          return()
        }
        checkboxInput('animate', 'Animate')
      })
      output$animate_column<- renderUI({
        if(input$graph_type=='Histogram'|input$graph_type=='Time series plot'){
          return()
        }
        if(input$graph_type=='Bar graph'|input$graph_type=='Box plot'){
          if(input$categorise!='None'){
            return()
          }
        }
        columns<-vector()
        col_type<- vector()
        options<- paste(names(data), '(', collumn_dc, ')')
        col_type[options]<- names(data)
        col_<- vector()
        col_[names(data)]<- collumn_dc
        
        for(col in names(data)){
          if(col==col_type[input$x_axis]|col==col_type[input$y_axis]){
            
          }else{
            if(col_[col]!='numeric'){
              columns[length(columns)+1]<- col
            }
            
          }
        }
        if(length(columns)==0){
          return()
        }
        if(input$animate==FALSE){
          return()
        }
        
        
        selectInput('animate_column', 'Column', choices = columns)
      })
      output$transition_time<- renderUI({
        if(input$graph_type=='Histogram'|input$graph_type=='Time series plot'){
          return()
        }
        if(input$graph_type=='Bar graph'|input$graph_type=='Box plot'){
          if(input$categorise!='None'){
            return()
          }
        }
        columns<-vector()
        col_type<- vector()
        options<- paste(names(data), '(', collumn_dc, ')')
        col_type[options]<- names(data)
        for(col in names(data)){
          if(col_type[input$x_axis]==col|col_type[input$y_axis]==col){
            
          }else{
            columns[length(columns)+1]<- col
          }
        }
        if(length(columns)==0){
          return()
        }
        if(input$animate==FALSE){
          return()
        }
        numericInput('transition_time', 'Transition time', value=100, min=100)
      })
      output$slider_visibility<- renderUI({
        if(input$graph_type=='Histogram'|input$graph_type=='Time series plot'){
          return()
        }
        if(input$graph_type=='Bar graph'|input$graph_type=='Box plot'){
          if(input$categorise!='None'){
            return()
          }
        }
        columns<-vector()
        col_type<- vector()
        options<- paste(names(data), '(', collumn_dc, ')')
        col_type[options]<- names(data)
        for(col in names(data)){
          if(col==col_type[input$x_axis]|col==col_type[input$y_axis]){
            
          }else{
            columns[length(columns)+1]<- col
          }
        }
        if(length(columns)==0){
          return()
        }
        if(input$animate==FALSE){
          return()
        }
        checkboxInput('slider_visibility', 'Slider visibility')
      })
      output$add_label<- renderUI({
        var<-TRUE
        if(input$graph_type=='Histogram'){
          var<-FALSE
        }
        if(input$graph_type=='Bar graph'|input$graph_type=='Box plot'){
          if(input$categorise!='None'){
            var<-FALSE
          }
        }
        columns<-vector()
        col_type<- vector()
        options<- paste(names(data), '(', collumn_dc, ')')
        col_type[options]<- names(data)
        for(col in names(data)){
          if(col==col_type[input$x_axis]|col==col_type[input$y_axis]){
            
          }else{
            columns[length(columns)+1]<- col
          }
        }
        if(length(columns)==0){
          var<-FALSE
        }
        if(input$graph_type!='Time series plot'){
          if(var==TRUE){
            if(input$animate==TRUE){
              return()
            }
          }
          
        }
        if(input$graph_type == 'Circles'| input$graph_type == 'Bar graph'|input$graph_type == 'Pie chart'|input$graph_type == 'Time series plot'|input$graph_type == 'World Map'|input$graph_type == 'USA Map'){
          actionButton('add_label', 'Add label',icon=icon('tags'))
        }
        
      })
      output$add_filter<- renderUI({
        actionButton('add_filter', 'Add filter',icon=icon('filter'))
      })
      observeEvent(input$add_label,{
        units<- c('Original'= 1, 'Thousands' = 1000, 'Millions'= 1000000, 'Billions' = 1000000000)
        no_of_clicks<<- no_of_clicks+1
        if(no_of_clicks>=5){
          no_of_clicks<<-5
        }
        prec<- paste('preceding', as.character(no_of_clicks), sep = '')
        output$remove_label<- renderUI({
          var<-TRUE
          if(input$graph_type=='Histogram'){
            var<-FALSE
          }
          if(input$graph_type=='Bar graph'|input$graph_type=='Box plot'){
            if(input$categorise!='None'){
              var<-FALSE
            }
          }
          columns<-vector()
          col_type<- vector()
          options<- paste(names(data), '(', collumn_dc, ')')
          col_type[options]<- names(data)
          for(col in names(data)){
            if(col==col_type[input$x_axis]|col==col_type[input$y_axis]){
              
            }else{
              columns[length(columns)+1]<- col
            }
          }
          if(length(columns)==0){
            var<-FALSE
          }
          if(input$graph_type!='Time series plot'){
            if(var==TRUE){
              if(input$animate==TRUE){
                return()
              }
            }
            
          }
          actionButton('remove_label', 'Remove label',icon = icon('times-circle'))
        })
        output[[prec]]<- renderUI({
          textInput(prec, '')
        })
        succ<- paste('succeeding', as.character(no_of_clicks), sep = '')
        output[[succ]]<- renderUI({
          textInput(succ, '')
        })
        col<- paste('column', as.character(no_of_clicks), sep = '')
        output[[col]]<- renderUI({
          numeric_or_not<- collumn_dc == 'numeric'
          options<- names(data)[numeric_or_not]
          selectInput(col, '', choices = options)
        })
        unit<- paste('unit', as.character(no_of_clicks), sep = '')
        output[[unit]]<- renderUI({
          selectInput(unit, 'Units', choices = units)
        })
        print(no_of_clicks)
        
      })
      observeEvent(input$remove_label,{
        prec<- paste('preceding', as.character(no_of_clicks), sep = '')
        print(prec)
        output[[prec]]<- renderUI({
          print('')
        })
        succ<- paste('succeeding', as.character(no_of_clicks), sep = '')
        print(succ)
        output[[succ]]<- renderUI({
          print('')
        })
        col<- paste('column', as.character(no_of_clicks), sep = '')
        print(col)
        output[[col]]<- renderUI({
          print('')
        })
        units<- paste('unit', as.character(no_of_clicks), sep = '')
        output[[units]]<- renderUI({
          print('')
        })
        no_of_clicks<<- no_of_clicks-1
        print(no_of_clicks)
        if(no_of_clicks<=0){
          no_of_clicks<<-0
        }
      })
      
      observeEvent(input$add_filter,{
        no_of_filters<<- no_of_filters+1
        print(no_of_filters)
        if(no_of_filters>=4){
          no_of_filters<<-4
        }
        print(no_of_clicks)
        print(no_of_filters)
        output$remove_filter<- renderUI({
          actionButton('remove_filter', 'Remove filter',icon=icon('times-circle'))
        })
        prec<- paste('filter_column', as.character(no_of_filters), sep = '')
        output[[prec]]<- renderUI({
          selectInput(prec, '', choices=names(data))
        })
        succ<- paste('filter_type', as.character(no_of_filters), sep = '')
        output[[succ]]<- renderUI({
          if(class(data[[input[[prec]]]])=='numeric'|class(data[[input[[prec]]]])=='character'){
            if(class(data[[input[[prec]]]])=='numeric'){
              choices<- c('Original', 'Length', 'Is Prime', 'Is Perfect Square', 'Rounded natural log','Top', 'Bottom')
            }
            if(class(data[[input[[prec]]]])=='character'){
              choices<- c('Original', 'Length', 'Number of Vowels', 'Number of Consonants')
            }
            selectInput(succ, '', choices=choices)
          }
        })
        col<- paste('filter_sign', as.character(no_of_filters), sep = '')
        output[[col]]<- renderUI({
          if(class(data[[input[[prec]]]])=='numeric'){
            if(input[[succ]]=='Is Prime'|input[[succ]]=='Is Perfect Square'|input[[succ]]=='Is Perfect Cube'|input[[succ]]=='Top'|input[[succ]]=='Bottom'){
              return()
            }
          }
          if(class(data[[input[[prec]]]])=='character'){
            if(input[[succ]]=='Original'){
              choices=c('=', 'not =')
            }else{
              choices<- c('=','not =', '<', '>', '<=', '>=')
            }
          }
          if(class(data[[input[[prec]]]])=='numeric'|class(data[[input[[prec]]]])=='Date'){
            choices<- c('=','not =' , '<', '>', '<=', '>=')
          }
          selectInput(col, '', choices=choices)
        })
        unit<- paste('filter_value', as.character(no_of_filters), sep = '')
        output[[unit]]<- renderUI({
          if(class(data[[input[[prec]]]])=='numeric'){
            if(input[[succ]]=='Is Prime'|input[[succ]]=='Is Perfect Square'|input[[succ]]=='Is Perfect Cube'){
              checkboxInput(unit, 'TRUE/FALSE')
            }
          }
        })
        n_unit<- paste('filter_value_x', as.character(no_of_filters), sep = '')
        output[[n_unit]]<- renderUI({
          if(class(data[[input[[prec]]]])=='numeric'|class(data[[input[[prec]]]])=='character'){
            if(input[[succ]]=='Is Prime'|input[[succ]]=='Is Perfect Square'|input[[succ]]=='Is Perfect Cube'){
              return()
            }
            if(class(data[[input[[prec]]]])=='character'){
              if(input[[succ]]=='Original'){
                return()
              }
            }
            numericInput(n_unit, '',value=0)
          }
          
        })
        d_unit<- paste('filter_value_y', as.character(no_of_filters), sep = '')
        output[[d_unit]]<- renderUI({
          
          if(class(data[[input[[prec]]]])=='Date'){
            if(input[[col]] == '='|input[[col]] == 'not ='){
              arg<- TRUE
            }else{
              arg<- FALSE
            }
            selectInput(d_unit, '', choices  = unique(data[[input[[prec]]]]), multiple = arg)
          }
          
        })
        t_unit<- paste('filter_value_z', as.character(no_of_filters), sep = '')
        output[[t_unit]]<- renderUI({
          if(class(data[[input[[prec]]]])=='character'){
            if(input[[succ]]=='Length'|input[[succ]]=='Number of Vowels'|input[[succ]]=='Number of Consonants'){
              return()
            }
            selectInput(t_unit, '', choices=unique(data[[input[[prec]]]]), multiple=TRUE)
          }
          
        })
        
        
        
      })
      observeEvent(input$remove_filter,{
        
        prec<- paste('filter_column', as.character(no_of_filters), sep = '')
        output[[prec]]<- renderUI({
          print('')
        })
        succ<- paste('filter_type', as.character(no_of_filters), sep = '')
        output[[succ]]<- renderUI({
          print('')
        })
        col<- paste('filter_sign', as.character(no_of_filters), sep = '')
        output[[col]]<- renderUI({
          print('')
          
        })
        unit<- paste('filter_value', as.character(no_of_filters), sep = '')
        output[[unit]]<- renderUI({
          print('')
        })
        n_unit<- paste('filter_value_x', as.character(no_of_filters), sep = '')
        output[[n_unit]]<- renderUI({
          print('')
          
        })
        d_unit<- paste('filter_value_y', as.character(no_of_filters), sep = '')
        output[[d_unit]]<- renderUI({
          print('')
          
        })
        t_unit<- paste('filter_value_z', as.character(no_of_filters), sep = '')
        output[[t_unit]]<- renderUI({
          print('')
          
        })
        no_of_filters<<- no_of_filters-1
        print(no_of_filters)
        if(no_of_filters<=0){
          no_of_filters<<-0
        }
        print(no_of_clicks)
      })
      output$refresh<- renderUI({
        actionButton('refresh', 'Refresh',icon=icon('sync'))
      })
      
      
      output$xlab<- renderUI({
        
        if(input$graph_type == 'Pie chart'|input$graph_type == 'World Map'|input$graph_type == 'USA Map'){
          return()
        }
        textInput('xlab', 'X axis label')
      })
      output$ylab<- renderUI({
        if(input$graph_type == 'Pie chart'|input$graph_type == 'World Map'|input$graph_type == 'USA Map'){
          return()
        }
        
        textInput('ylab', 'Y axis label')
      })
      output$label_code<- renderUI({
        textInput('label_code', 'Label')
        
      })
      output$title<- renderUI({
        textInput('title', 'Title')
      })
      output$Measure<- renderUI({
        if(input$graph_type!='Box plot'){
          selectInput('Measure', 'Measure', choices = c('Sum', 'Average', 'Median', 'Count'))
        }
        
      })
      
      output$bin_size<- renderUI({
        if(input$graph_type=='Histogram'){
          sliderInput('bin_size', 'Bin size', min=3, max=100, value=5,step=1)
        }
      })
      output$trend_lines<- renderUI({
        if(input$graph_type=='Histogram'|input$graph_type=='Scatter plot'|input$graph_type=='World Map'| input$graph_type=='Pie chart'|input$graph_type =='USA Map'){
          return()
        }
        if(input$graph_type!='Box plot'){
          if(input$graph_type=='Bar graph'){
            if(input$animate){
              return()
            }
            if(input$categorise!='None'){
              return()
            }
          }
          trend_lines<- c('None','Average', 'Median', 'Customise')
          selectInput('trend_lines', 'Target Line', choices = trend_lines)
        }
      })
      output$trend_line_as_label<- renderUI({
        if(input$graph_type=='Histogram'|input$graph_type=='Scatter plot'|input$graph_type=='World Map'| input$graph_type=='Pie chart'|input$graph_type =='USA Map'){
          return()
        }
        if(input$graph_type!='Box plot'){
          if(input$graph_type=='Bar graph'){
            if(input$animate){
              return()
            }
            if(input$categorise!='None'){
              return()
            }
          }
          if(input$trend_lines =='None'){
            
          }else{
            checkboxInput('trend_line_as_label', 'Target Line as label')
          }
        }
      })
      
      output$customised_val<- renderUI({
        if(input$graph_type=='Histogram'|input$graph_type=='Scatter plot'|input$graph_type=='World Map'| input$graph_type=='Pie chart'|input$graph_type =='USA Map'|input$graph_type =='World Map'){
          return()
        }
        if(input$graph_type!='Box plot'){
          if(input$graph_type=='Bar graph'){
            if(input$animate){
              return()
            }
            if(input$categorise!='None'){
              return()
            }
          }
          if(input$trend_lines == 'Customise'){
            numericInput('customised_val', 'value', value = 0)
          }
        }
      })
      output$trend_preceding<- renderUI({
        if(input$graph_type=='Histogram'|input$graph_type=='Scatter plot'|input$graph_type=='World Map'| input$graph_type=='Pie chart'|input$graph_type =='USA Map'){
          return()
        }
        if(input$graph_type!='Box plot'){
          if(input$graph_type=='Bar graph'){
            if(input$animate){
              return()
            }
            if(input$categorise!='None'){
              return()
            }
          }
          if(input$trend_lines !='None'){
            if(is.null(input$trend_line_as_label) ==FALSE){
              if(input$trend_line_as_label == TRUE){
                textInput('trend_preceding', '')
              }
            }
          }
        }
      })
      output$trend_column<- renderUI({
        if(input$graph_type=='Histogram'|input$graph_type=='Scatter plot'|input$graph_type=='World Map'| input$graph_type=='Pie chart'|input$graph_type =='USA Map'){
          return()
        }
        if(input$graph_type!='Box plot'){
          if(input$graph_type=='Bar graph'){
            if(input$animate){
              return()
            }
            if(input$categorise!='None'){
              return()
            }
          }
          if(input$trend_lines !='None'){
            if(is.null(input$trend_line_as_label) ==FALSE){
              if(input$trend_line_as_label == TRUE){
                paste(input$trend_lines)
              }
            }
          }
        }
      })
      output$trend_succeeding<- renderUI({
        if(input$graph_type=='Histogram'|input$graph_type=='Scatter plot'|input$graph_type=='World Map'| input$graph_type=='Pie chart'|input$graph_type =='USA Map'){
          return()
        }
        if(input$graph_type!='Box plot'){
          if(input$graph_type=='Bar graph'){
            if(input$animate){
              return()
            }
            if(input$categorise!='None'){
              return()
            }
          }
          if(input$trend_lines !='None'){
            if(is.null(input$trend_line_as_label) ==FALSE){
              if(input$trend_line_as_label == TRUE){
                textInput('trend_succeeding', '')
              }
            }
          }
        }
      })
      output$trend_unit<- renderUI({
        if(input$graph_type=='Histogram'|input$graph_type=='Scatter plot'|input$graph_type=='World Map'| input$graph_type=='Pie chart'|input$graph_type == 'USA Map'){
          return()
        }
        if(input$graph_type!='Box plot'){
          if(input$graph_type=='Bar graph'){
            if(input$animate){
              return()
            }
            if(input$categorise!='None'){
              return()
            }
          }
          units<- c('Original'=1,'Thousands'=1000,'Millions'=1000000,'Billions'=1000000000)
          if(input$trend_lines !='None'){
            if(is.null(input$trend_line_as_label) ==FALSE){
              if(input$trend_line_as_label == TRUE){
                selectInput('trend_unit', '', choices =units)
              }
            }
          }
        }
      })
      # output$parameter_cat<- renderUI({
      #   options<- vector()
      #   opt<- vector()
      #   options<- names(data)
      #   opt<- paste('#',options[collumn_dc=='categorical'|collumn_dc=='Date'])
      #   opt[length(opt)+1]<- 'None'
      #   selectInput('parameter_cat', 'Add a parameter(categorical)', choices = opt,selected = 'None')
      # })
      # output$parameter_num<- renderUI({
      #   options<- vector()
      #   opt<- vector()
      #   options<- names(data)
      #   opt<- paste('#',options[collumn_dc=='numeric'])
      #   opt[length(opt)+1]<- 'None'
      #   selectInput('parameter_num', 'Add a parameter(numeric)', choices = opt,selected = 'None')
      # })
      output$gridline<- renderUI({
        if(input$graph_type =='World Map'|input$graph_type =='USA Map'|input$graph_type=='Pie chart'){
          return()
        }
        checkboxInput('gridline', 'Gridlines?')
        
        
      })
      output$colour_scale<- renderUI({
        if(input$graph_type=='Bar graph'| input$graph_type=='Scatter plot'){
          col_type<- vector()
          options<- paste(names(data), '(', collumn_dc, ')')
          col_type[options]<- collumn_dc
          col_type['None']<- 'None'
          if(col_type[input$categorise]=='numeric'){
            
          }else{
            return()
          }
        }
        if(input$graph_type=='Bar graph'| input$graph_type=='Scatter plot'| input$graph_type=='World Map'| input$graph_type=='USA Map'){
          colours<- c('Reds', 'Blues', 'Purples', 'Greens', 'Yellows', 'Oranges')
          selectInput('colour_scale', 'Colour scale', choices=colours)
        }
      })
      output$size<- renderUI({
        if(input$graph_type=='Scatter plot'){
          nums<- vector()
          nums[length(nums)+1]<- 'None'
          for(collumn in names(data)){
            if(mean(is.na(as.numeric(data[[collumn]]))==TRUE)<=0.07&class(data[[collumn]])!='Date'){
              nums[length(nums)+1]<- collumn
            }
          }
          
          selectInput('size', 'Plot size', choices = nums,selected = 'None')
        }
      })
      output$label_text<- renderUI({
        if(input$graph_type == 'Density plot'|input$graph_type == 'Histogram'|input$graph_type == 'Box plot'|input$graph_type=='World Map'|input$graph_type=='USA Map'|input$graph_type=='Time series plot'){
          return()
        }else{
          col_type<- vector()
          options<- paste(names(data), '(', collumn_dc, ')')
          col_type[options]<- collumn_dc
          col_type['None']<- 'None'
          checkboxInput('label_text', 'Labels as text?')
          
        }
      })
      output$date_format<- renderUI({
        formats<- c('Day Month Year', 'Day Month', 'Month Year','Month', 'Year')
        col_type<- vector()
        col_type2<- vector()
        options<- paste(names(data), '(', collumn_dc, ')')
        col_type[options]<- collumn_dc
        col_type2[names(data)]<- collumn_dc
        col_type['None']<- 'None'
        print(input$categorise)
        if(col_type[input$x_axis] == 'Date'){
          selectInput('date_format', 'Date format for X axis', choices = formats)
        }
        
      })
      output$date_format_animate<- renderUI({
        if(input$graph_type=='Histogram'| input$graph_type=='Time series plot'){
          return()
        }
        if(input$graph_type=='Bar graph'|input$graph_type=='Box plot'){
          if(input$categorise!='None'){
            return()
          }
        }
        columns<-vector()
        col_type<- vector()
        options<- paste(names(data), '(', collumn_dc, ')')
        col_type[options]<- names(data)
        for(col in names(data)){
          if(col==col_type[input$x_axis]|col==col_type[input$y_axis]){
            
          }else{
            columns[length(columns)+1]<- col
          }
        }
        if(length(columns)==0){
          return()
        }
        if(input$animate==FALSE){
          return()
        }
        formats<- c('Day Month Year', 'Day Month', 'Month Year','Month', 'Year')
        if(class(data[[input$animate_column]]) == 'Date'){
          selectInput('date_format_animate', 'Date format for column', choices = formats)
        }
        
      })
      output$date_format_cat<- renderUI({
        formats<- c('Day Month Year', 'Day Month', 'Month Year','Month', 'Year')
        col_type<- vector()
        col_type2<- vector()
        options<- paste(names(data), '(', collumn_dc, ')')
        col_type[options]<- collumn_dc
        col_type2[names(data)]<- collumn_dc
        col_type['None']<- 'None'
        print(input$categorise)
        if(is.null(input$categorise) == FALSE){
          if(col_type[input$categorise] == 'Date'){
            selectInput('date_format_cat', 'Date format for category', choices = formats)
          }
        }
      })
      
      output$a_d<- renderUI({
        if(input$graph_type == 'Bar graph'|input$graph_type == 'Box plot'){
          radioButtons('a_d', '',choices = c('Ascending', 'Descending'))
        }
      })
      
      
      output$category_type<- renderUI({
        col_type<- vector()
        options<- paste(names(data), '(', collumn_dc, ')')
        col_type[options]<- collumn_dc
        col_type['None']<- 'None'
        if(input$graph_type == 'Bar graph'){
          if(col_type[input$categorise] == 'categorical'|col_type[input$categorise] == 'Date'){
            radioButtons('category_type','',choices = c('group', 'stack'))
          }
        }
      })
      output$hole<- renderUI({
        if(input$graph_type == 'Pie chart'){
          sliderInput('hole', 'Hole size', min = 0, max = 0.9, value = 0, step = 0.1)
        }
      })
      units<- c('Original' = 1, 'Thousands' = 1000, 'Millions' = 1000000, 'Billions' = 1000000000)
      output$x_axis_as_label<- renderUI({
        val<- TRUE
        if(input$graph_type=='Histogram'){
          val<- FALSE
        }
        if(input$graph_type=='Bar graph'|input$graph_type=='Box plot'){
          if(input$categorise!='None'){
            val<- FALSE
          }
        }
        columns<-vector()
        col_type<- vector()
        options<- paste(names(data), '(', collumn_dc, ')')
        col_type[options]<- names(data)
        for(col in names(data)){
          if(col==col_type[input$x_axis]|col==col_type[input$y_axis]){
            
          }else{
            columns[length(columns)+1]<- col
          }
        }
        if(length(columns)==0){
          val<- FALSE
        }
        if(input$graph_type!='Time series plot'){
          if(val){
            if(input$animate==TRUE){
              return()
            }
          }
          
        }
        if(input$graph_type =='Histogram'){
          checkboxInput('x_axis_as_label', 'Frequency as label?')
        }else if(input$graph_type =='Box plot'){
          checkboxInput('x_axis_as_label', 'Labels?')
        }else{
          checkboxInput('x_axis_as_label', 'X axis as label?')
        }
        
      })
      output$categorise_as_label<- renderUI({
        col_type<- vector()
        options<- paste(names(data), '(', collumn_dc, ')')
        col_type[options]<- collumn_dc
        col_type['None']<- 'None'
        if(input$graph_type == 'Bar graph' |input$graph_type=='Histogram'|input$graph_type=='Density plot'|input$graph_type=='Scatter plot'){
          if (col_type[input$categorise] == 'categorical' | col_type[input$categorise] == 'Date'){
            checkboxInput('categorise_as_label', 'Categorise as label?')
          }
        }
      })
      output$input_file<- renderDataTable(options = list(paging = FALSE, searchable = FALSE, bFilter = FALSE),{
        if(is.null(input$file)==TRUE){
          return()
        }
        tryCatch({
          dataset<- read_this(input$type,input$sep, input$file, input$sep2)
          head(dataset,10)
        },
        error = function(err){
          showNotification('There is an error, maybe this is beause you chose \n you exceeded the file size limit \n or chose the wrong format \n or did not choose the header option (for text file)', type = 'error')
        })
      })
      output$cat_preceding<- renderUI({
        col_type<- vector()
        options<- paste(names(data), '(', collumn_dc, ')')
        col_type[options]<- collumn_dc
        col_type['None']<- 'None'
        if(is.null(input$categorise_as_label) == FALSE){
          if (input$categorise_as_label ==TRUE){
            if (col_type[input$categorise] == 'categorical' | col_type[input$categorise] == 'Date'){
              if(input$graph_type =='Pie chart'|input$graph_type =='World Map'|input$graph_type =='USA Map'){
                
              }else{
                textInput('cat_preceding', '')
              }
              
            }
          }
        }
        
      })
      output$cat_column<- renderUI({
        col_type<- vector()
        options<- paste(names(data), '(', collumn_dc, ')')
        col_type[options]<- collumn_dc
        col_type['None']<- 'None'
        if(is.null(input$categorise_as_label) == FALSE){
          if (input$categorise_as_label ==TRUE){
            if (col_type[input$categorise] == 'categorical' | col_type[input$categorise] == 'Date'){
              col_type<- vector()
              options<- paste(names(data), '(', collumn_dc, ')')
              col_type[options]<- names(data)
              if( input$graph_type =='Pie chart'|input$graph_type =='World Map'|input$graph_type =='USA Map'){
                
              }else{
                print(col_type[input$categorise])
              }
            }
          }
        }
      })
      output$cat_succeeding<- renderUI({
        col_type<- vector()
        options<- paste(names(data), '(', collumn_dc, ')')
        col_type[options]<- collumn_dc
        col_type['None']<- 'None'
        if(is.null(input$categorise_as_label) == FALSE){
          if (input$categorise_as_label ==TRUE){
            if (col_type[input$categorise] == 'categorical' | col_type[input$categorise] == 'Date'){
              if( input$graph_type =='Pie chart'|input$graph_type =='World Map'|input$graph_type =='USA Map'){
                
              }else{
                textInput('cat_succeeding', '')
              }
              
            }
          }
        }
      })
      
      output$x_preceding<- renderUI({
        if(input$graph_type== 'Box plot'){
          return()
        }
        if(input$graph_type=='Histogram'|input$graph_type=='Density plot'|input$graph_type=='Box plot'){
          
        }else{
          if (input$x_axis_as_label ==TRUE){
            textInput('x_preceding', '')
          }
        }
      })
      output$x_column<- renderUI({
        print(input$graph_type)
        if(input$graph_type== 'Box plot'){
          return()
        }
        if(input$graph_type=='Histogram'|input$graph_type=='Density plot'){
          if (input$x_axis_as_label ==TRUE){
            print('frequency')
          }
        }else{
          if (input$x_axis_as_label ==TRUE){
            col_type<- vector()
            options<- paste(names(data), '(', collumn_dc, ')')
            col_type[options]<- names(data)
            print(col_type[input$x_axis])
          }
        }
      })
      output$x_succeeding<- renderUI({
        if(input$graph_type== 'Box plot'){
          return()
        }
        if (input$x_axis_as_label ==TRUE){
          textInput('x_succeeding', '')
        }
      })
      
      output$categorise<- renderUI({
        if(input$graph_type == 'Bar graph' |input$graph_type=='Scatter plot'){
          if(input$graph_type == 'Time series plot'){
            options<- paste(names(data), '(', collumn_dc, ')')
            col_type<- vector()
            col_type['None']<- 'none'
            col_type[options]<- names(data)
            collumn_dctype<- vector()
            collumn_dctype['none']<- 'none'
            
            collumn_dctype[names(data)]<- collumn_dc
            options[length(options)+1]<- 'None'
            
            selectInput('categorise', 'Colour', choices =  options[collumn_dctype[col_type[options]] != 'numeric'], selected = 'None')
          }else{
            options<- paste(names(data), '(', collumn_dc, ')')
            col_type<- vector()
            col_type[options]<- names(data)
            collumn_dctype<- vector()
            collumn_dctype[names(data)]<- collumn_dc
            options[length(options)+1]<- 'None'
            selectInput('categorise', 'Colour', choices =  options, selected = 'None')
          }
        }else if(input$graph_type=='Histogram' | input$graph_type=='Density plot'|input$graph_type=='Box plot'){
          options<- vector()
          opt<- vector()
          options<- paste(names(data), '(',collumn_dc,')')
          opt<- options[collumn_dc!='numeric']
          opt[length(opt)+1]<- 'None'
          selectInput('categorise', 'Colour', choices = opt, selected = 'None')
        }
      })
    
      observeEvent(input$refresh,{
        output$export<- renderUI({
          radioButtons('export', 'Export(click the camera button)',choices=c('jpeg', 'png', 'svg'))
        })
        if(is.null(input$x_axis_as_label)){
          updateTabsetPanel(session = session,'tabs', 'More Features')
        }
        '%ni%'<- Negate('%in%')
        if(no_of_filters>0){
          if(class(data[[input$filter_column1]])=='Date'){
            if(input$filter_sign1=='='){
              data<-data%>%filter(data[[input$filter_column1]]%in%as.Date(input$filter_value_y1))
            }
            if(input$filter_sign1=='>'){
              data<-data%>%filter(data[[input$filter_column1]]>input$filter_value_y1)
            }
            if(input$filter_sign1=='<'){
              data<-data%>%filter(data[[input$filter_column1]]<input$filter_value_y1)
            }
            if(input$filter_sign1=='>='){
              data<-data%>%filter(data[[input$filter_column1]]>=input$filter_value_y1)
            }
            if(input$filter_sign1=='<='){
              data<-data%>%filter(data[[input$filter_column1]]<=input$filter_value_y1)
            }
            if(input$filter_sign1=='not ='){
              data<-data%>%filter(data[[input$filter_column1]]%ni%as.Date(input$filter_value_y1))
            }
          }
          if(class(data[[input$filter_column1]])=='numeric'){
            if(input$filter_type1=='Original'){
              if(input$filter_sign1=='='){
                data<-data%>%filter(data[[input$filter_column1]]==input$filter_value_x1)
              }
              if(input$filter_sign1=='>'){
                data<-data%>%filter(data[[input$filter_column1]]>input$filter_value_x1)
              }
              if(input$filter_sign1=='<'){
                data<-data%>%filter(data[[input$filter_column1]]<input$filter_value_x1)
              }
              if(input$filter_sign1=='>='){
                data<-data%>%filter(data[[input$filter_column1]]>=input$filter_value_x1)
              }
              if(input$filter_sign1=='<='){
                data<-data%>%filter(data[[input$filter_column1]]<=input$filter_value_x1)
              }
              if(input$filter_sign1=='not ='){
                data<-data%>%filter(data[[input$filter_column1]]!=input$filter_value_x1)
              }
            }
            if(input$filter_type1=='Length'){
              if(input$filter_sign1=='='){
                data<-data%>%filter(nchar(data[[input$filter_column1]])==input$filter_value_x1)
              }
              if(input$filter_sign1=='>'){
                data<-data%>%filter(nchar(data[[input$filter_column1]])>input$filter_value_x1)
              }
              if(input$filter_sign1=='<'){
                data<-data%>%filter(nchar(data[[input$filter_column1]])<input$filter_value_x1)
              }
              if(input$filter_sign1=='>='){
                data<-data%>%filter(nchar(data[[input$filter_column1]])>=input$filter_value_x1)
              }
              if(input$filter_sign1=='<='){
                data<-data%>%filter(nchar(data[[input$filter_column1]])<=input$filter_value_x1)
              }
              if(input$filter_sign1=='not ='){
                data<-data%>%filter(nchar(data[[input$filter_column1]])!=input$filter_value_x1)
              }
            }
            if(input$filter_type1=='Rounded natural log'){
              if(input$filter_sign1=='='){
                data<-data%>%filter(round(log(data[[input$filter_column1]]))==input$filter_value_x1)
              }
              if(input$filter_sign1=='>'){
                data<-data%>%filter(round(log(data[[input$filter_column1]]))>input$filter_value_x1)
              }
              if(input$filter_sign1=='<'){
                data<-data%>%filter(round(log(data[[input$filter_column1]]))<input$filter_value_x1)
              }
              if(input$filter_sign1=='>='){
                data<-data%>%filter(round(log(data[[input$filter_column1]]))>=input$filter_value_x1)
              }
              if(input$filter_sign1=='<='){
                data<-data%>%filter(round(log(data[[input$filter_column1]]))<=input$filter_value_x1)
              }
              if(input$filter_sign1=='not = '){
                data<-data%>%filter(round(log(data[[input$filter_column1]]))!=input$filter_value_x1)
              }
            }
            if(input$filter_type1=='Is Prime'){
              if(input$filter_value1==FALSE){
                data<-data%>%filter(data[[input$filter_column1]]!=is_prime(data[[input$filter_column1]]))
              }
              if(input$filter_value1==TRUE){
                data<-data%>%filter(data[[input$filter_column1]]==is_prime(data[[input$filter_column1]]))
              }
            }
            if(input$filter_type1=='Top'){
              data<- data%>%filter(is.na(data[[input$filter_column1]])==FALSE)
              data<- data%>%arrange(data[[input$filter_column1]])
              data<- data%>%mutate(rank=length(data[[input$filter_column1]]):1)
              View(data)
              data<-data%>%filter(rank<=input$filter_value_x1)
            }
            if(input$filter_type1=='Bottom'){
              data<- data%>%filter(is.na(data[[input$filter_column1]])==FALSE)
              data<- data%>%arrange(data[[input$filter_column1]])
              data<- data%>%mutate(rank=1:length(data[[input$filter_column1]]))
              View(data)
              data<-data%>%filter(rank<=input$filter_value_x1)
            }
            if(input$filter_type1=='Is Perfect Square'){
              if(input$filter_value1==FALSE){
                val<-data[[input$filter_column1]][sqrt(data[[input$filter_column1]])-floor(data[[input$filter_column1]])==0]
                data<-data%>%filter(data[[input$filter_column1]]!=val)
              }
              if(input$filter_value1==TRUE){
                val<-data[[input$filter_column1]][sqrt(data[[input$filter_column1]])-floor(data[[input$filter_column1]])==0]
                data<-data%>%filter(data[[input$filter_column1]]==val)
              }
            }
          }
          if(class(data[[input$filter_column1]])=='character'){
            if(input$filter_type1=='Original'){
              if(input$filter_sign1=='='){
                data<- data%>%filter(data[[input$filter_column1]]%in%input$filter_value_z1)
              }else{
                data<- data%>%filter(data[[input$filter_column1]]%ni%input$filter_value_z1)
              }
              
            }
            if(input$filter_type1=='Length'){
              if(input$filter_sign1=='='){
                data<-data%>%filter(nchar(data[[input$filter_column1]])==input$filter_value_x1)
              }
              if(input$filter_sign1=='>'){
                data<-data%>%filter(nchar(data[[input$filter_column1]])>input$filter_value_x1)
              }
              if(input$filter_sign1=='<'){
                data<-data%>%filter(nchar(data[[input$filter_column1]])<input$filter_value_x1)
              }
              if(input$filter_sign1=='>='){
                data<-data%>%filter(nchar(data[[input$filter_column1]])>=input$filter_value_x1)
              }
              if(input$filter_sign1=='<='){
                data<-data%>%filter(nchar(data[[input$filter_column1]])<=input$filter_value_x1)
              }
              if(input$filter_sign1=='not ='){
                data<-data%>%filter(nchar(data[[input$filter_column1]])!=input$filter_value_x1)
              }
            }
            if(input$filter_type1=='Number of Vowels'){
              if(input$filter_sign1=='='){
                print(vowel_count(data[[input$filter_column1]]))
                data<-data%>%filter(vowel_count(data[[input$filter_column1]])==input$filter_value_x1)
              }
              if(input$filter_sign1=='>'){
                data<-data%>%filter(vowel_count(data[[input$filter_column1]])>input$filter_value_x1)
              }
              if(input$filter_sign1=='<'){
                data<-data%>%filter(vowel_count(data[[input$filter_column1]])<input$filter_value_x1)
              }
              if(input$filter_sign1=='>='){
                data<-data%>%filter(vowel_count(data[[input$filter_column1]])>=input$filter_value_x1)
              }
              if(input$filter_sign1=='<='){
                data<-data%>%filter(vowel_count(data[[input$filter_column1]])<=input$filter_value_x1)
              }
              if(input$filter_sign1=='not = '){
                data<-data%>%filter(vowel_count(data[[input$filter_column1]])!=input$filter_value_x1)
              }
            }
            if(input$filter_type1=='Number of Consonants'){
              if(input$filter_sign1=='='){
                data<-data%>%filter(consonant_count(data[[input$filter_column1]])==input$filter_value_x1)
              }
              if(input$filter_sign1=='>'){
                data<-data%>%filter(consonant_count(data[[input$filter_column1]])>input$filter_value_x1)
              }
              if(input$filter_sign1=='<'){
                data<-data%>%filter(consonant_count(data[[input$filter_column1]])<input$filter_value_x1)
              }
              if(input$filter_sign1=='>='){
                data<-data%>%filter(consonant_count(data[[input$filter_column1]])>=input$filter_value_x1)
              }
              if(input$filter_sign1=='<='){
                data<-data%>%filter(consonant_count(data[[input$filter_column1]])<=input$filter_value_x1)
              }
              if(input$filter_sign1=='not ='){
                data<-data%>%filter(consonant_count(data[[input$filter_column1]])!=input$filter_value_x1)
              }
            }
          }
        }
        if(no_of_filters>1){
          if(class(data[[input$filter_column2]])=='Date'){
            if(input$filter_sign2=='='){
              data<-data%>%filter(data[[input$filter_column2]]%in%as.Date(input$filter_value_y2))
            }
            if(input$filter_sign2=='>'){
              data<-data%>%filter(data[[input$filter_column2]]>input$filter_value_y2)
            }
            if(input$filter_sign2=='<'){
              data<-data%>%filter(data[[input$filter_column2]]<input$filter_value_y2)
            }
            if(input$filter_sign2=='>='){
              data<-data%>%filter(data[[input$filter_column2]]>=input$filter_value_y2)
            }
            if(input$filter_sign2=='<='){
              data<-data%>%filter(data[[input$filter_column2]]<=input$filter_value_y2)
            }
            if(input$filter_sign2=='not ='){
              data<-data%>%filter(data[[input$filter_column2]]%ni%as.Date(input$filter_value_y2))
            }
          }
          if(class(data[[input$filter_column2]])=='numeric'){
            if(input$filter_type2=='Original'){
              if(input$filter_sign2=='='){
                data<-data%>%filter(data[[input$filter_column2]]==input$filter_value_x2)
              }
              if(input$filter_sign2=='>'){
                data<-data%>%filter(data[[input$filter_column2]]>input$filter_value_x2)
              }
              if(input$filter_sign2=='<'){
                data<-data%>%filter(data[[input$filter_column2]]<input$filter_value_x2)
              }
              if(input$filter_sign2=='>='){
                data<-data%>%filter(data[[input$filter_column2]]>=input$filter_value_x2)
              }
              if(input$filter_sign2=='<='){
                data<-data%>%filter(data[[input$filter_column2]]<=input$filter_value_x2)
              }
              if(input$filter_sign2=='not ='){
                data<-data%>%filter(data[[input$filter_column2]]!=input$filter_value_x2)
              }
            }
            if(input$filter_type2=='Length'){
              if(input$filter_sign2=='='){
                data<-data%>%filter(nchar(data[[input$filter_column2]])==input$filter_value_x2)
              }
              if(input$filter_sign2=='>'){
                data<-data%>%filter(nchar(data[[input$filter_column2]])>input$filter_value_x2)
              }
              if(input$filter_sign2=='<'){
                data<-data%>%filter(nchar(data[[input$filter_column2]])<input$filter_value_x2)
              }
              if(input$filter_sign2=='>='){
                data<-data%>%filter(nchar(data[[input$filter_column2]])>=input$filter_value_x2)
              }
              if(input$filter_sign2=='<='){
                data<-data%>%filter(nchar(data[[input$filter_column2]])<=input$filter_value_x2)
              }
              if(input$filter_sign2=='not ='){
                data<-data%>%filter(nchar(data[[input$filter_column2]])!=input$filter_value_x2)
              }
            }
            if(
              input$filter_type2=='Rounded natural log'){
              if(input$filter_sign2=='='){
                data<-data%>%filter(round(log(data[[input$filter_column2]]))==input$filter_value_x2)
              }
              if(input$filter_sign2=='>'){
                data<-data%>%filter(round(log(data[[input$filter_column2]]))>input$filter_value_x2)
              }
              if(input$filter_sign2=='<'){
                data<-data%>%filter(round(log(data[[input$filter_column2]]))<input$filter_value_x2)
              }
              if(input$filter_sign2=='>='){
                data<-data%>%filter(round(log(data[[input$filter_column2]]))>=input$filter_value_x2)
              }
              if(input$filter_sign2=='<='){
                data<-data%>%filter(round(log(data[[input$filter_column2]]))<=input$filter_value_x2)
              }
              if(input$filter_sign2=='not ='){
                data<-data%>%filter(round(log(data[[input$filter_column2]]))!=input$filter_value_x2)
              }
            }
            if(input$filter_type2=='Top'){
              data<- data%>%filter(is.na(data[[input$filter_column2]])==FALSE)
              data<- data%>%arrange(data[[input$filter_column2]])
              data<- data%>%mutate(rank=length(data[[input$filter_column2]]):1)
              View(data)
              data<-data%>%filter(rank<=input$filter_value_x2)
            }
            if(input$filter_type2=='Bottom'){
              data<- data%>%filter(is.na(data[[input$filter_column2]])==FALSE)
              data<- data%>%arrange(data[[input$filter_column2]])
              data<- data%>%mutate(rank=1:length(data[[input$filter_column2]]))
              View(data)
              data<-data%>%filter(rank<=input$filter_value_x2)
            }
            if(input$filter_type2=='Is Prime'){
              if(input$filter_value2==FALSE){
                data<-data%>%filter(data[[input$filter_column2]]!=is_prime(data[[input$filter_column2]]))
              }
              if(input$filter_value2==TRUE){
                data<-data%>%filter(data[[input$filter_column2]]==is_prime(data[[input$filter_column2]]))
              }
            }
            if(input$filter_type2=='Is Perfect Square'){
              if(input$filter_value2==FALSE){
                val<-data[[input$filter_column2]][sqrt(data[[input$filter_column2]])-floor(data[[input$filter_column2]])==0]
                data<-data%>%filter(data[[input$filter_column2]]!=val)
              }
              if(input$filter_value2==TRUE){
                val<-data[[input$filter_column2]][sqrt(data[[input$filter_column2]])-floor(data[[input$filter_column2]])==0]
                data<-data%>%filter(data[[input$filter_column2]]==val)
              }
            }
          }
          if(class(data[[input$filter_column2]])=='character'){
            if(input$filter_type2=='Original'){
              print(input$filter_value2)
              if(input$filter_sign2=='='){
                data<- data%>%filter(data[[input$filter_column2]]%in%input$filter_value_z2)
              }else{
                data<- data%>%filter(data[[input$filter_column2]]%ni%input$filter_value_z2)
              }
            }
            if(input$filter_type2=='Length'){
              if(input$filter_sign2=='='){
                data<-data%>%filter(nchar(data[[input$filter_column2]])==input$filter_value_x2)
              }
              if(input$filter_sign2=='>'){
                data<-data%>%filter(nchar(data[[input$filter_column2]])>input$filter_value_x2)
              }
              if(input$filter_sign2=='<'){
                data<-data%>%filter(nchar(data[[input$filter_column2]])<input$filter_value_x2)
              }
              if(input$filter_sign2=='>='){
                data<-data%>%filter(nchar(data[[input$filter_column2]])>=input$filter_value_x2)
              }
              if(input$filter_sign2=='<='){
                data<-data%>%filter(nchar(data[[input$filter_column2]])<=input$filter_value_x2)
              }
              if(input$filter_sign2=='not ='){
                data<-data%>%filter(nchar(data[[input$filter_column2]])!=input$filter_value_x2)
              }
            }
            if(input$filter_type2=='Number of Vowels'){
              if(input$filter_sign2=='='){
                print(vowel_count(data[[input$filter_column2]]))
                data<-data%>%filter(vowel_count(data[[input$filter_column2]])==input$filter_value_x2)
              }
              if(input$filter_sign2=='>'){
                data<-data%>%filter(vowel_count(data[[input$filter_column2]])>input$filter_value_x2)
              }
              if(input$filter_sign2=='<'){
                data<-data%>%filter(vowel_count(data[[input$filter_column2]])<input$filter_value_x2)
              }
              if(input$filter_sign2=='>='){
                data<-data%>%filter(vowel_count(data[[input$filter_column2]])>=input$filter_value_x2)
              }
              if(input$filter_sign2=='<='){
                data<-data%>%filter(vowel_count(data[[input$filter_column2]])<=input$filter_value_x2)
              }
              if(input$filter_sign2=='not ='){
                data<-data%>%filter(vowel_count(data[[input$filter_column2]])!=input$filter_value_x2)
              }
            }
            if(input$filter_type2=='Number of Consonants'){
              if(input$filter_sign2=='='){
                data<-data%>%filter(consonant_count(data[[input$filter_column2]])==input$filter_value_x2)
              }
              if(input$filter_sign2=='>'){
                data<-data%>%filter(consonant_count(data[[input$filter_column2]])>input$filter_value_x2)
              }
              if(input$filter_sign2=='<'){
                data<-data%>%filter(consonant_count(data[[input$filter_column2]])<input$filter_value_x2)
              }
              if(input$filter_sign2=='>='){
                data<-data%>%filter(consonant_count(data[[input$filter_column2]])>=input$filter_value_x2)
              }
              if(input$filter_sign2=='<='){
                data<-data%>%filter(consonant_count(data[[input$filter_column2]])<=input$filter_value_x2)
              }
              if(input$filter_sign2=='not ='){
                data<-data%>%filter(consonant_count(data[[input$filter_column2]])!=input$filter_value_x2)
              }
            }
          }
        }
        if(no_of_filters>2){
          if(class(data[[input$filter_column3]])=='Date'){
            if(input$filter_sign3=='='){
              data<-data%>%filter(data[[input$filter_column3]]%in%as.Date(input$filter_value_y3))
            }
            if(input$filter_sign3=='>'){
              data<-data%>%filter(data[[input$filter_column3]]>input$filter_value_y3)
            }
            if(input$filter_sign3=='<'){
              data<-data%>%filter(data[[input$filter_column3]]<input$filter_value_y3)
            }
            if(input$filter_sign3=='>='){
              data<-data%>%filter(data[[input$filter_column3]]>=input$filter_value_y3)
            }
            if(input$filter_sign3=='<='){
              data<-data%>%filter(data[[input$filter_column3]]<=input$filter_value_y3)
            }
            if(input$filter_sign3=='not ='){
              data<-data%>%filter(data[[input$filter_column3]]%ni%as.Date(input$filter_value_y3))
            }
          }
          if(class(data[[input$filter_column3]])=='numeric'){
            if(input$filter_type3=='Original'){
              if(input$filter_sign3=='='){
                data<-data%>%filter(data[[input$filter_column3]]==input$filter_value_x3)
              }
              if(input$filter_sign3=='>'){
                data<-data%>%filter(data[[input$filter_column3]]>input$filter_value_x3)
              }
              if(input$filter_sign3=='<'){
                data<-data%>%filter(data[[input$filter_column3]]<input$filter_value_x3)
              }
              if(input$filter_sign3=='>='){
                data<-data%>%filter(data[[input$filter_column3]]>=input$filter_value_x3)
              }
              if(input$filter_sign3=='<='){
                data<-data%>%filter(data[[input$filter_column3]]<=input$filter_value_x3)
              }
              if(input$filter_sign3=='not ='){
                data<-data%>%filter(data[[input$filter_column3]]!=input$filter_value_x3)
              }
            }
            if(input$filter_type3=='Length'){
              if(input$filter_sign3=='='){
                data<-data%>%filter(nchar(data[[input$filter_column3]])==input$filter_value_x3)
              }
              if(input$filter_sign3=='>'){
                data<-data%>%filter(nchar(data[[input$filter_column3]])>input$filter_value_x3)
              }
              if(input$filter_sign3=='<'){
                data<-data%>%filter(nchar(data[[input$filter_column3]])<input$filter_value_x3)
              }
              if(input$filter_sign3=='>='){
                data<-data%>%filter(nchar(data[[input$filter_column3]])>=input$filter_value_x3)
              }
              if(input$filter_sign3=='<='){
                data<-data%>%filter(nchar(data[[input$filter_column3]])<=input$filter_value_x3)
              }
              if(input$filter_sign3=='not ='){
                data<-data%>%filter(nchar(data[[input$filter_column3]])!=input$filter_value_x3)
              }
            }
            if(
              input$filter_type3=='Rounded natural log'){
              if(input$filter_sign3=='='){
                data<-data%>%filter(round(log(data[[input$filter_column3]]))==input$filter_value_x3)
              }
              if(input$filter_sign3=='>'){
                data<-data%>%filter(round(log(data[[input$filter_column3]]))>input$filter_value_x3)
              }
              if(input$filter_sign3=='<'){
                data<-data%>%filter(round(log(data[[input$filter_column3]]))<input$filter_value_x3)
              }
              if(input$filter_sign3=='>='){
                data<-data%>%filter(round(log(data[[input$filter_column3]]))>=input$filter_value_x3)
              }
              if(input$filter_sign3=='<='){
                data<-data%>%filter(round(log(data[[input$filter_column3]]))<=input$filter_value_x3)
              }
              if(input$filter_sign3=='not ='){
                data<-data%>%filter(round(log(data[[input$filter_column3]]))!=input$filter_value_x3)
              }
            }
            if(input$filter_type3=='Top'){
              data<- data%>%filter(is.na(data[[input$filter_column3]])==FALSE)
              data<- data%>%arrange(data[[input$filter_column3]])
              data<- data%>%mutate(rank=length(data[[input$filter_column3]]):1)
              View(data)
              data<-data%>%filter(rank<=input$filter_value_x3)
            }
            if(input$filter_type3=='Bottom'){
              data<- data%>%filter(is.na(data[[input$filter_column3]])==FALSE)
              data<- data%>%arrange(data[[input$filter_column3]])
              data<- data%>%mutate(rank=1:length(data[[input$filter_column3]]))
              View(data)
              data<-data%>%filter(rank<=input$filter_value_x3)
            }
            if(input$filter_type3=='Is Prime'){
              if(input$filter_value3==FALSE){
                data<-data%>%filter(data[[input$filter_column3]]!=is_prime(data[[input$filter_column3]]))
              }
              if(input$filter_value3==TRUE){
                data<-data%>%filter(data[[input$filter_column3]]==is_prime(data[[input$filter_column3]]))
              }
            }
            if(input$filter_type3=='Is Perfect Square'){
              if(input$filter_value3==FALSE){
                val<-data[[input$filter_column3]][sqrt(data[[input$filter_column3]])-floor(data[[input$filter_column3]])==0]
                data<-data%>%filter(data[[input$filter_column3]]!=val)
              }
              if(input$filter_value3==TRUE){
                val<-data[[input$filter_column3]][sqrt(data[[input$filter_column3]])-floor(data[[input$filter_column3]])==0]
                data<-data%>%filter(data[[input$filter_column3]]==val)
              }
            }
          }
          if(class(data[[input$filter_column3]])=='character'){
            if(input$filter_type3=='Original'){
              print(input$filter_value1)
              if(input$filter_sign3=='='){
                data<- data%>%filter(data[[input$filter_column3]]%in%input$filter_value_z3)
              }else{
                data<- data%>%filter(data[[input$filter_column3]]%ni%input$filter_value_z3)
              }
            }
            if(input$filter_type3=='Length'){
              if(input$filter_sign3=='='){
                data<-data%>%filter(nchar(data[[input$filter_column3]])==input$filter_value_x3)
              }
              if(input$filter_sign3=='>'){
                data<-data%>%filter(nchar(data[[input$filter_column3]])>input$filter_value_x3)
              }
              if(input$filter_sign3=='<'){
                data<-data%>%filter(nchar(data[[input$filter_column3]])<input$filter_value_x3)
              }
              if(input$filter_sign3=='>='){
                data<-data%>%filter(nchar(data[[input$filter_column3]])>=input$filter_value_x3)
              }
              if(input$filter_sign3=='<='){
                data<-data%>%filter(nchar(data[[input$filter_column3]])<=input$filter_value_x3)
              }
              if(input$filter_sign3=='not ='){
                data<-data%>%filter(nchar(data[[input$filter_column3]])!=input$filter_value_x3)
              }
            }
            if(input$filter_type3=='Number of Vowels'){
              if(input$filter_sign3=='='){
                print(vowel_count(data[[input$filter_column3]]))
                data<-data%>%filter(vowel_count(data[[input$filter_column3]])==input$filter_value_x3)
              }
              if(input$filter_sign3=='>'){
                data<-data%>%filter(vowel_count(data[[input$filter_column3]])>input$filter_value_x3)
              }
              if(input$filter_sign3=='<'){
                data<-data%>%filter(vowel_count(data[[input$filter_column3]])<input$filter_value_x3)
              }
              if(input$filter_sign3=='>='){
                data<-data%>%filter(vowel_count(data[[input$filter_column3]])>=input$filter_value_x3)
              }
              if(input$filter_sign3=='<='){
                data<-data%>%filter(vowel_count(data[[input$filter_column3]])<=input$filter_value_x3)
              }
              if(input$filter_sign3=='not ='){
                data<-data%>%filter(vowel_count(data[[input$filter_column3]])!=input$filter_value_x3)
              }
            }
            if(input$filter_type3=='Number of Consonants'){
              if(input$filter_sign3=='='){
                data<-data%>%filter(consonant_count(data[[input$filter_column3]])==input$filter_value_x3)
              }
              if(input$filter_sign3=='>'){
                data<-data%>%filter(consonant_count(data[[input$filter_column3]])>input$filter_value_x3)
              }
              if(input$filter_sign3=='<'){
                data<-data%>%filter(consonant_count(data[[input$filter_column3]])<input$filter_value_x3)
              }
              if(input$filter_sign3=='>='){
                data<-data%>%filter(consonant_count(data[[input$filter_column3]])>=input$filter_value_x3)
              }
              if(input$filter_sign3=='<='){
                data<-data%>%filter(consonant_count(data[[input$filter_column3]])<=input$filter_value_x3)
              }
              if(input$filter_sign3=='not ='){
                data<-data%>%filter(consonant_count(data[[input$filter_column3]])!=input$filter_value_x3)
              }
            }
          }
        }
        if(no_of_filters>3){
          if(class(data[[input$filter_column]])=='Date'){
            if(input$filter_sign4=='='){
              data<-data%>%filter(data[[input$filter_column4]]%in%as.Date(input$filter_value_y4))
            }
            if(input$filter_sign4=='>'){
              data<-data%>%filter(data[[input$filter_column4]]>input$filter_value_y4)
            }
            if(input$filter_sign4=='<'){
              data<-data%>%filter(data[[input$filter_column4]]<input$filter_value_y4)
            }
            if(input$filter_sign4=='>='){
              data<-data%>%filter(data[[input$filter_column4]]>=input$filter_value_y4)
            }
            if(input$filter_sign4=='<='){
              data<-data%>%filter(data[[input$filter_column4]]<=input$filter_value_y4)
            }
            if(input$filter_sign4=='not ='){
              data<-data%>%filter(data[[input$filter_column4]]%ni%as.Date(input$filter_value_y4))
            }
          }
          if(class(data[[input$filter_column4]])=='numeric'){
            if(input$filter_type4=='Original'){
              if(input$filter_sign4=='='){
                data<-data%>%filter(data[[input$filter_column4]]==input$filter_value_x4)
              }
              if(input$filter_sign4=='>'){
                data<-data%>%filter(data[[input$filter_column4]]>input$filter_value_x4)
              }
              if(input$filter_sign4=='<'){
                data<-data%>%filter(data[[input$filter_column4]]<input$filter_value_x4)
              }
              if(input$filter_sign4=='>='){
                data<-data%>%filter(data[[input$filter_column4]]>=input$filter_value_x4)
              }
              if(input$filter_sign4=='<='){
                data<-data%>%filter(data[[input$filter_column4]]<=input$filter_value_x4)
              }
              if(input$filter_sign4=='not ='){
                data<-data%>%filter(data[[input$filter_column4]]!=input$filter_value_x4)
              }
            }
            if(input$filter_type4=='Length'){
              if(input$filter_sign4=='='){
                data<-data%>%filter(nchar(data[[input$filter_column4]])==input$filter_value_x4)
              }
              if(input$filter_sign4=='>'){
                data<-data%>%filter(nchar(data[[input$filter_column4]])>input$filter_value_x4)
              }
              if(input$filter_sign4=='<'){
                data<-data%>%filter(nchar(data[[input$filter_column4]])<input$filter_value_x4)
              }
              if(input$filter_sign4=='>='){
                data<-data%>%filter(nchar(data[[input$filter_column4]])>=input$filter_value_x4)
              }
              if(input$filter_sign4=='<='){
                data<-data%>%filter(nchar(data[[input$filter_column4]])<=input$filter_value_x4)
              }
              if(input$filter_sign4=='not ='){
                data<-data%>%filter(nchar(data[[input$filter_column4]])!=input$filter_value_x4)
              }
            }
            if(input$filter_type4=='Top'){
              data<- data%>%filter(is.na(data[[input$filter_column4]])==FALSE)
              data<- data%>%arrange(data[[input$filter_column4]])
              data<- data%>%mutate(rank=length(data[[input$filter_column4]]):1)
              View(data)
              data<-data%>%filter(rank<=input$filter_value_x4)
            }
            if(input$filter_type4=='Bottom'){
              data<- data%>%filter(is.na(data[[input$filter_column4]])==FALSE)
              data<- data%>%arrange(data[[input$filter_column4]])
              data<- data%>%mutate(rank=1:length(data[[input$filter_column4]]))
              View(data)
              data<-data%>%filter(rank<=input$filter_value_x4)
            }
            if(
              input$filter_type4=='Rounded natural log'){
              if(input$filter_sign4=='='){
                data<-data%>%filter(round(log(data[[input$filter_column4]]))==input$filter_value_x4)
              }
              if(input$filter_sign4=='>'){
                data<-data%>%filter(round(log(data[[input$filter_column4]]))>input$filter_value_x4)
              }
              if(input$filter_sign4=='<'){
                data<-data%>%filter(round(log(data[[input$filter_column4]]))<input$filter_value_x4)
              }
              if(input$filter_sign4=='>='){
                data<-data%>%filter(round(log(data[[input$filter_column4]]))>=input$filter_value_x4)
              }
              if(input$filter_sign4=='<='){
                data<-data%>%filter(round(log(data[[input$filter_column4]]))<=input$filter_value_x4)
              }
              if(input$filter_sign4=='not ='){
                data<-data%>%filter(round(log(data[[input$filter_column4]]))!=input$filter_value_x4)
              }
            }
            if(input$filter_type4=='Is Prime'){
              if(input$filter_value4==FALSE){
                data<-data%>%filter(data[[input$filter_column4]]!=is_prime(data[[input$filter_column4]]))
              }
              if(input$filter_value4==TRUE){
                data<-data%>%filter(data[[input$filter_column4]]==is_prime(data[[input$filter_column4]]))
              }
            }
            if(input$filter_type1=='Is Perfect Square'){
              if(input$filter_value4==FALSE){
                val<-data[[input$filter_column4]][sqrt(data[[input$filter_column4]])-floor(data[[input$filter_column4]])==0]
                data<-data%>%filter(data[[input$filter_column4]]!=val)
              }
              if(input$filter_value4==TRUE){
                val<-data[[input$filter_column4]][sqrt(data[[input$filter_column4]])-floor(data[[input$filter_column4]])==0]
                data<-data%>%filter(data[[input$filter_column4]]==val)
              }
            }
          }
          if(class(data[[input$filter_column4]])=='character'){
            if(input$filter_type4=='Original'){
              print(input$filter_value1)
              if(input$filter_sign4=='='){
                data<- data%>%filter(data[[input$filter_column4]]%in%input$filter_value_z4)
              }else{
                data<- data%>%filter(data[[input$filter_column4]]%ni%input$filter_value_z4)
              }
            }
            if(input$filter_type4=='Length'){
              if(input$filter_sign4=='='){
                data<-data%>%filter(nchar(data[[input$filter_column4]])==input$filter_value_x4)
              }
              if(input$filter_sign4=='>'){
                data<-data%>%filter(nchar(data[[input$filter_column4]])>input$filter_value_x4)
              }
              if(input$filter_sign4=='<'){
                data<-data%>%filter(nchar(data[[input$filter_column4]])<input$filter_value_x4)
              }
              if(input$filter_sign4=='>='){
                data<-data%>%filter(nchar(data[[input$filter_column4]])>=input$filter_value_x4)
              }
              if(input$filter_sign4=='<='){
                data<-data%>%filter(nchar(data[[input$filter_column4]])<=input$filter_value_x4)
              }
              if(input$filter_sign4=='not ='){
                data<-data%>%filter(nchar(data[[input$filter_column4]])!=input$filter_value_x4)
              }
            }
            if(input$filter_type4=='Number of Vowels'){
              if(input$filter_sign4=='='){
                print(vowel_count(data[[input$filter_column4]]))
                data<-data%>%filter(vowel_count(data[[input$filter_column4]])==input$filter_value_x4)
              }
              if(input$filter_sign4=='>'){
                data<-data%>%filter(vowel_count(data[[input$filter_column4]])>input$filter_value_x4)
              }
              if(input$filter_sign4=='<'){
                data<-data%>%filter(vowel_count(data[[input$filter_column4]])<input$filter_value_x4)
              }
              if(input$filter_sign4=='>='){
                data<-data%>%filter(vowel_count(data[[input$filter_column4]])>=input$filter_value_x4)
              }
              if(input$filter_sign4=='<='){
                data<-data%>%filter(vowel_count(data[[input$filter_column4]])<=input$filter_value_x4)
              }
              if(input$filter_sign4=='not ='){
                data<-data%>%filter(vowel_count(data[[input$filter_column4]])!=input$filter_value_x4)
              }
            }
            if(input$filter_type4=='Number of Consonants'){
              if(input$filter_sign4=='='){
                data<-data%>%filter(consonant_count(data[[input$filter_column4]])==input$filter_value_x4)
              }
              if(input$filter_sign4=='>'){
                data<-data%>%filter(consonant_count(data[[input$filter_column4]])>input$filter_value_x4)
              }
              if(input$filter_sign4=='<'){
                data<-data%>%filter(consonant_count(data[[input$filter_column4]])<input$filter_value_x4)
              }
              if(input$filter_sign4=='>='){
                data<-data%>%filter(consonant_count(data[[input$filter_column4]])>=input$filter_value_x4)
              }
              if(input$filter_sign4=='<='){
                data<-data%>%filter(consonant_count(data[[input$filter_column4]])<=input$filter_value_x4)
              }
              if(input$filter_sign4=='not ='){
                data<-data%>%filter(consonant_count(data[[input$filter_column4]])!=input$filter_value_x4)
              }
            }
          }
        }
        # if(is.null(input$x_axis_as_label)==FALSE){
        #   output$par_cat<- renderUI({
        #     if(input$parameter_cat!='None'){
        #       if(class(data[[str_remove(input$parameter_cat, '# ')]])=='Date'){
        #         opt<- unique(grouped_data$x_axis)
        #         opt[length(opt)+1]<- 'All'
        #         dateInput('par_cat', input$parameter_cat,format = date_format[str_remove(input$parameter_cat, '# ')],value=max(data[[str_remove(input$parameter_cat, '# ')]], na.rm=TRUE),max = max(data[[str_remove(input$parameter_cat, '# ')]], na.rm=TRUE), min=min(data[[str_remove(input$parameter_cat, '# ')]], na.rm=TRUE))
        #       
        #       }else{
        #         opt<- paste('# ',unique(data[[str_remove(input$parameter_cat, '# ')]]))
        #         opt[length(opt)+1]<- 'All'
        #         selectInput('par_cat', str_remove(input$parameter_cat, '# '), choices = opt,selected = 'All', multiple = TRUE)
        #       }
        #     }
        #   })
        #   output$par_num<- renderUI({
        #     if(input$parameter_num!='None'){
        #       opt<- data[[str_remove(input$parameter_num, '# ')]]
        #       paste('max', max(opt,na.rm = TRUE))
        #       paste('min', min(opt, na.rm = TRUE))
        #       sliderInput('par_num', input$parameter_num, min=min(opt, na.rm = TRUE), max=max(opt, na.rm = TRUE), value=c(min(opt, na.rm = TRUE),min(opt, na.rm = TRUE)))
        #     }
        #   })
        # }
        
        units<- c('Original' = 1, 'Thousands' = 1000, 'Millions' = 1000000, 'Billions' = 1000000000)
        
        labels<- ''
        
        options<- paste(names(data), '(', collumn_dc,')')
        col_type<- vector()
        col_type[options]<- names(data)
        options<- paste(names(data), '(',collumn_dc,')')
        catornum<- vector()
        catornum[options]<- collumn_dc
        coldc_type<- vector()
        coldc_type[options]<- names(data)
        colourss<- vector()
        text_colour<- vector()
        
        labels<- ''
        labelx<- ''
        labelcat<- ''
        
        # if(is.null(input$parameter_cat)==FALSE){
        #   if(input$parameter_num!='None'){
        #     data<- data%>%filter(data[[str_remove(input$parameter_num, '# ')]]<=input$par_num[2]&data[[str_remove(input$parameter_num, '# ')]]>=input$par_num[1])
        #   }
        #   if(input$parameter_cat!='None'){
        #     '%ni%'<- Negate('%in%')
        #     
        #     if(class(data[[str_remove(input$parameter_cat, '# ')]])=='Date'){
        #       data<- data%>%filter(data[[str_remove(input$parameter_cat, '# ')]]==input$par_cat )
        #     }else{
        #       if('All'%ni%input$par_cat){
        #         print(str_remove(input$par_cat, '# '))
        #         data<- data%>%filter(data[[str_remove(input$parameter_cat, '# ')]]%in%str_remove(input$par_cat, '#  '))
        #       }
        #     }
        #   }
        # }
        '%ni%'<- Negate('%in%')
        if(input$graph_type == 'Bar graph'){
          columns<-vector()
          col_type<- vector()
          options<- paste(names(data), '(', collumn_dc, ')')
          col_type[options]<- names(data)
          for(col in names(data)){
            if(col==col_type[input$x_axis]|col==col_type[input$y_axis]){
              
            }else{
              columns[length(columns)+1]<- col
            }
          }
          if(input$categorise=='None'&length(columns)!=0&input$animate){
            
            if(class(data[[col_type[input$x_axis]]]) != 'Date'){
              data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
            }else{
              if(input$date_format == 'Month Year'){
                data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }else if(input$date_format == 'Day Month Year'){
                data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }else if(input$date_format == 'Day Month'){
                data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
              }else if(input$date_format == 'Month'){
                data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
              }else if(input$date_format == 'Year'){
                data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
              }
              
            }
            data<-data%>%mutate(label1 = data[[col_type[input$y_axis]]])
            data<-data%>%mutate(animate = data[[input$animate_column]])
            nums<-vector()
            for(collumn in names(data)){
              if(mean(is.na(as.numeric(data[[collumn]]))==TRUE)<=0.07&class(data[[collumn]])!='Date'){
                nums[length(nums)+1]<- collumn
              }
            }
            if(class(data[[input$animate_column]])=='Date'|input$animate_column%in%nums){
              if(input$animate_column%in%nums){
                data<- data%>%arrange(as.numeric(animate))
              }
              if(class(data[[input$animate_column]])=='Date'){
                data<- data%>%arrange(animate)
              }
              data<- data%>%mutate(rank = 1:length(animate))
              
              if(class(data[[input$animate_column]])=='Date'){
                if(input$date_format_animate == 'Month Year'){
                  data<-data%>%mutate(date = paste(month(data[['animate']], label=TRUE), year(data[['animate']])))
                }else if(input$date_format_animate == 'Day Month Year'){
                  data<-data%>%mutate(date = paste(day(data[['animate']]), month(data[['animate']], label=TRUE), year(data[['animate']])))
                }else if(input$date_format_animate == 'Day Month'){
                  data<-data%>%mutate(date = paste(day(data[['animate']]), month(data[['animate']], label=TRUE)))
                }else if(input$date_format_animate == 'Month'){
                  data<-data%>%mutate(date = paste(month(data[['animate']], label=TRUE)))
                }else if(input$date_format_animate == 'Year'){
                  data<-data%>%mutate(date = paste(year(data[['animate']])))
                }
              }else if(input$animate_column%in%nums){
                data<-data%>%mutate(date = data[[input$animate_column]])
              }
              grouped_data<-data%>%group_by(x_axis,date)
              if(input$aggregate == 'Sum'){
                vized_data<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE), rank = max(rank, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                vized_data<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE), rank = max(rank, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                vized_data<- summarise(grouped_data, sum = length(label1), rank = max(rank, na.rm = TRUE))
              }else{
                vized_data<- summarise(grouped_data, sum = median(label1, na.rm = TRUE), rank= max(rank, na.rm = TRUE))
              }
              
              if(input$slider_visibility==TRUE){
                if(input$a_d=='Ascending'){
                  plt<- vized_data%>%plot_ly(x=~reorder(x_axis,sum), y=~sum, text = '', hoverinfo = 'text',type='bar',frame=~reorder(date,rank),marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(currentvalue = list(prefix = "", font = list(color="red")))%>%animation_button(x = 4, xanchor = "right", y = 0, yanchor = "bottom")
                }else{
                  plt<- vized_data%>%plot_ly(x=~reorder(x_axis,-sum), y=~sum, text = '', hoverinfo = 'text',type='bar',frame=~reorder(date,rank),marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(currentvalue = list(prefix = "", font = list(color="red")))%>%animation_button(x = 4, xanchor = "right", y = 0, yanchor = "bottom")
                }
              }else{
                if(input$a_d=='Ascending'){
                  plt<- vized_data%>%plot_ly(x=~reorder(x_axis,sum), y=~sum, text = '', hoverinfo = 'text',type='bar',frame=~reorder(date,rank),marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>%animation_slider(hide = TRUE)%>%animation_button(x = 4, xanchor = "right", y = 0, yanchor = "bottom")
                }else{
                  plt<- vized_data%>%plot_ly(x=~reorder(x_axis,-sum), y=~sum, text = '', hoverinfo = 'text',type='bar',frame=~reorder(date,rank),marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>%animation_slider(hide = TRUE)%>%animation_button(x = 4, xanchor = "right", y = 0, yanchor = "bottom")
                }
              }
            }else{
              data<-data%>%mutate(date = data[[input$animate_column]])
              grouped_data<-data%>%group_by(x_axis,date)
              if(input$aggregate == 'Sum'){
                vized_data<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                vized_data<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                vized_data<- summarise(grouped_data, sum = length(label1))
              }else{
                vized_data<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              
              if(input$slider_visibility==TRUE){
                if(input$a_d=='Ascending'){
                  plt<- vized_data%>%plot_ly(x=~reorder(x_axis,sum), y=~sum, text = '', hoverinfo = 'text',type='bar',frame=~date,marker=list(color=input$chart_colour), marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(currentvalue = list(prefix = "", font = list(color="red")))%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
                }else{
                  plt<- vized_data%>%plot_ly(x=~reorder(x_axis,-sum), y=~sum, text = '', hoverinfo = 'text',type='bar',frame=~date,marker=list(color=input$chart_colour), marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(currentvalue = list(prefix = "", font = list(color="red")))%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
                }
              }else{
                if(input$a_d=='Ascending'){
                  plt<- vized_data%>%plot_ly(x=~reorder(x_axis,sum), y=~sum, text = '', hoverinfo = 'text',type='bar',frame=~date,marker=list(color=input$chart_colour), marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>%animation_slider(hide = TRUE)%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
                }else{
                  plt<- vized_data%>%plot_ly(x=~reorder(x_axis,-sum), y=~sum, text = '', hoverinfo = 'text',type='bar',frame=~date,marker=list(color=input$chart_colour), marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>%animation_slider(hide = TRUE)%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
                }
              }
            }
            
          }else{
            
            if(input$categorise != 'None'){
              for(i in 1:length(unique(data[[coldc_type[input$categorise]]]))){
                ran_col<-randomColor()
                while(ran_col %in% colourss){
                  ran_col<- randomColor()
                }
                colourss[i]<- randomColor()
              }
              for(i in 1:length(unique(data[[coldc_type[input$categorise]]]))){
                ran_col<-randomColor()
                while(ran_col %in% text_colour & ran_col %in% colourss){
                  ran_col<- randomColor()
                }
                text_colour[i]<- randomColor()
              }
            }
            if (input$categorise == 'None'){
              
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                if(input$date_format == 'Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                }
                
              }
              grouped_data<-grouped_data%>%mutate(label1 = data[[col_type[input$y_axis]]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                vized_data<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                vized_data<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                vized_data<- summarise(grouped_data, sum = length(label1))
              }else{
                vized_data<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              if(input$x_axis_as_label == TRUE){
                labelx<- paste(input$x_preceding, vized_data$x_axis, input$x_succeeding)
              }
              
              if(no_of_clicks == 1){
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                
                grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
                grouped_data<-grouped_data%>%group_by(x_axis)
                if(input$aggregate == 'Sum'){
                  l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l1<- summarise(grouped_data, sum = length(label1))
                }else{
                  l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
                }
                print(class(l1$sum))
                labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2) ,input$succeeding1)
              }else if(no_of_clicks == 2){
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
                grouped_data<-grouped_data%>%group_by(x_axis)
                if(input$aggregate == 'Sum'){
                  l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l1<- summarise(grouped_data, sum = length(label1))
                }else{
                  l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
                grouped_data<-grouped_data%>%group_by(x_axis)
                
                if(input$aggregate == 'Sum'){
                  l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l2<- summarise(grouped_data, sum = length(label2))
                }else{
                  l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
                }
                labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2) ,input$succeeding1, '\n',
                               input$preceding2,round(l2$sum/as.numeric(input$unit2),2) ,input$succeeding2)
                
              }else if(no_of_clicks == 3){
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
                grouped_data<-grouped_data%>%group_by(x_axis)
                if(input$aggregate == 'Sum'){
                  l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l1<- summarise(grouped_data, sum = length(label1))
                }else{
                  l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
                grouped_data<-grouped_data%>%group_by(x_axis)
                if(input$aggregate == 'Sum'){
                  l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l2<- summarise(grouped_data, sum = length(label2))
                }else{
                  l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label3 = data[[input$column3]])
                grouped_data<-grouped_data%>%group_by(x_axis)
                if(input$aggregate == 'Sum'){
                  l3<- summarise(grouped_data, sum = sum(label3, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l3<- summarise(grouped_data, sum = mean(label3, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l3<- summarise(grouped_data, sum = length(label3))
                }else{
                  l3<- summarise(grouped_data, sum = median(label3, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }
                labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2) ,input$succeeding1, '\n',
                               input$preceding2,round(l2$sum/as.numeric(input$unit2),2),input$succeeding2,'\n',
                               input$preceding3,round(l3$sum/as.numeric(input$unit3),2) ,input$succeeding3)
                
              } else if(no_of_clicks ==4){
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
                grouped_data<-grouped_data%>%group_by(x_axis)
                if(input$aggregate == 'Sum'){
                  l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l1<- summarise(grouped_data, sum = length(label1))
                }else{
                  l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
                grouped_data<-grouped_data%>%group_by(x_axis)
                
                if(input$aggregate == 'Sum'){
                  l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l2<- summarise(grouped_data, sum = length(label2))
                }else{
                  l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label3 = data[[input$column3]])
                grouped_data<-grouped_data%>%group_by(x_axis)
                
                if(input$aggregate == 'Sum'){
                  l3<- summarise(grouped_data, sum = sum(label3, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l3<- summarise(grouped_data, sum = mean(label3, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l3<- summarise(grouped_data, sum = length(label3))
                }else{
                  l3<- summarise(grouped_data, sum = median(label3, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label4 = data[[input$column4]])
                grouped_data<-grouped_data%>%group_by(x_axis)
                
                if(input$aggregate == 'Sum'){
                  l4<- summarise(grouped_data, sum = sum(label4, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l4<- summarise(grouped_data, sum = mean(label4, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l4<- summarise(grouped_data, sum = length(label4))
                }else{
                  l4<- summarise(grouped_data, sum = median(label4, na.rm = TRUE))
                }
                labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2),input$succeeding1, '\n',
                               input$preceding2,round(l2$sum/as.numeric(input$unit2),2) ,input$succeeding2,'\n',
                               input$preceding3,round(l3$sum/as.numeric(input$unit3),2) ,input$succeeding3,'\n',
                               input$preceding4,round(l4$sum/as.numeric(input$unit1),4) ,input$succeeding4)
              }
              label_line<- ''
              if(input$trend_lines =='Average'){
                if(input$trend_line_as_label ==TRUE){
                  label_line<- paste(input$trend_preceding, round(mean(vized_data$sum, na.rm=TRUE)/as.numeric(input$trend_unit),1),input$trend_succeeding )
                }
              }
              if(input$trend_lines =='Median'){
                if(input$trend_line_as_label ==TRUE){
                  label_line<- paste(input$trend_preceding, round(median(vized_data$sum, na.rm=TRUE)/as.numeric(input$trend_unit),1),input$trend_succeeding )
                }
              }
              if(input$trend_lines =='Customise'){
                if(input$trend_line_as_label ==TRUE){
                  label_line<- paste(input$trend_preceding, as.character(input$customised_val/as.numeric(input$trend_unit)),input$trend_succeeding )
                }
              }
              
              if(input$label_text == TRUE){
                if(input$a_d == 'Ascending'){
                  if(input$trend_lines == 'Average'){
                    print('this one')
                    plt<-vized_data%>%plot_ly(x=~reorder(x_axis, sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text',texttemplate = '%{text}',textposition = 'outside', outsidetextfont = list(color = input$colour_text), marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')%>%add_lines(y=~mean(sum, na.rm= TRUE), line =~list(color = input$line_colour),marker =~list(color = input$line_colour),text=~label_line, hoverinfo = 'text',showlegend = FALSE)
                  }else if(input$trend_lines == 'Median'){
                    plt<-vized_data%>%plot_ly(x=~reorder(x_axis, sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text',texttemplate = '%{text}',textposition = 'outside', outsidetextfont = list(color = input$colour_text), marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')%>%add_lines(y=~median(sum, na.rm= TRUE), line =~list(color = input$line_colour),text=~label_line, hoverinfo = 'text',showlegend = FALSE, marker=list(color=input$line_colour))
                  }else if(input$trend_lines == 'Customise'){
                    plt<-vized_data%>%plot_ly(x=~reorder(x_axis, sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text',texttemplate = '%{text}',textposition = 'outside', outsidetextfont = list(color = input$colour_text), marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')%>%add_lines(y=~as.numeric(input$customised_val), line =~list(color = input$line_colour),text=~label_line, hoverinfo = 'text',showlegend = FALSE, marker=list(color=input$line_colour))
                  }else{
                    plt<-vized_data%>%plot_ly(x=~reorder(x_axis, sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text',texttemplate = '%{text}',textposition = 'outside', outsidetextfont = list(color = input$colour_text), marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')
                  }
                }else if(input$a_d == 'Descending'){
                  if(input$trend_lines == 'Average'){
                    plt<-vized_data%>%plot_ly(x=~reorder(x_axis, -sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text',texttemplate = '%{text}',textposition = 'outside', outsidetextfont = list(color = input$colour_text), marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')%>%add_lines(y=~mean(sum, na.rm= TRUE), line =~list(color = input$line_colour),text=~label_line, hoverinfo = 'text',showlegend = FALSE, marker=list(color=input$line_colour))
                  }else if(input$trend_lines == 'Median'){
                    plt<-vized_data%>%plot_ly(x=~reorder(x_axis, -sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text',texttemplate = '%{text}',textposition = 'outside', outsidetextfont = list(color = input$colour_text), marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')%>%add_lines(y=~median(sum, na.rm= TRUE), line =~list(color = input$line_colour),text=~label_line, hoverinfo = 'text',showlegend = FALSE, marker=list(color=input$linet_colour))
                  }else if(input$trend_lines == 'Customise'){
                    plt<-vized_data%>%plot_ly(x=~reorder(x_axis, -sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text',texttemplate = '%{text}',textposition = 'outside', outsidetextfont = list(color = input$colour_text), marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')%>%add_lines(y=~as.numeric(input$customised_val), line =~list(color = input$line_colour),text=~label_line, hoverinfo = 'text',showlegend = FALSE, marker=list(color=input$line_colour))
                  }else{
                    plt<-vized_data%>%plot_ly(x=~reorder(x_axis, -sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text',texttemplate = '%{text}',textposition = 'outside', outsidetextfont = list(color = input$colour_text), marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')
                  }
                }
              }else if(input$label_text == FALSE){
                if(input$a_d == 'Ascending'){
                  if(input$trend_lines == 'Average'){
                    plt<-vized_data%>%plot_ly(x=~reorder(x_axis, sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text', marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')%>%add_lines(y=~mean(sum, na.rm= TRUE), line =~list(color = input$line_colour),text=~label_line, hoverinfo = 'text',showlegend = FALSE, marker=list(color=input$line_colour))
                  }else if(input$trend_lines == 'Median'){
                    plt<-vized_data%>%plot_ly(x=~reorder(x_axis, sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text', marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')%>%add_lines(y=~median(sum, na.rm= TRUE), line =~list(color = input$line_colour),text=~label_line, hoverinfo = 'text',showlegend = FALSE, marker=list(color=input$line_colour))
                  }else if(input$trend_lines == 'Customise'){
                    plt<-vized_data%>%plot_ly(x=~reorder(x_axis, sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text', marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')%>%add_lines(y=~as.numeric(input$customised_val), line =~list(color = input$line_colour),text=~label_line, hoverinfo = 'text',showlegend = FALSE, marker=list(color=input$line_colour))
                  }else{
                    plt<-vized_data%>%plot_ly(x=~reorder(x_axis, sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text', marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')
                  }
                }else if(input$a_d == 'Descending'){
                  if(input$trend_lines == 'Average'){
                    plt<-vized_data%>%plot_ly(x=~reorder(x_axis, -sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text', marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')%>%add_lines(y=~mean(sum, na.rm= TRUE), line =~list(color = input$line_colour),text=~label_line, hoverinfo = 'text',showlegend = FALSE, marker=list(color=input$line_colour))
                  }else if(input$trend_lines == 'Median'){
                    plt<-vized_data%>%plot_ly(x=~reorder(x_axis, -sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text', marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')%>%add_lines(y=~median(sum, na.rm= TRUE), line =~list(color = input$line_colour),text=~label_line, hoverinfo = 'text',showlegend = FALSE, marker=list(color=input$line_colour))
                  }else if(input$trend_lines == 'Customise'){
                    plt<-vized_data%>%plot_ly(x=~reorder(x_axis, -sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text', marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')%>%add_lines(y=~as.numeric(input$customised_val), line =~list(color = input$line_colour),text=~label_line, hoverinfo = 'text',showlegend = FALSE, marker=list(color=input$line_colour))
                  }else{
                    plt<-vized_data%>%plot_ly(x=~reorder(x_axis, -sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text', marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')
                  }
                }
                
              }
              
            }else if(catornum[input$categorise] == 'categorical'){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                if(input$date_format == 'Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                }
                
              }
              grouped_data<-grouped_data%>%mutate(label1 = data[[col_type[input$y_axis]]])
              grouped_data<-grouped_data%>%mutate(grouped_val = data[[coldc_type[input$categorise]]])
              grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
              if(input$aggregate == 'Sum'){
                vized_data<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                vized_data<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                vized_data<- summarise(grouped_data, sum = length(label1))
              }else{
                vized_data<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              if(input$x_axis_as_label == TRUE){
                labelx<- paste(input$x_preceding, vized_data$x_axis, input$x_succeeding)
              }
              if(input$categorise_as_label == TRUE){
                labelcat<- paste(input$cat_preceding, vized_data$grouped_val, input$cat_succeeding)
              }
              if(no_of_clicks == 1){
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
                grouped_data<-grouped_data%>%mutate(grouped_val = data[[coldc_type[input$categorise]]])
                grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
                if(input$aggregate == 'Sum'){
                  l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l1<- summarise(grouped_data, sum = length(label1))
                }else{
                  l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
                }
                labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2) ,input$succeeding1)
              }
              else if(no_of_clicks == 2){
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
                grouped_data<-grouped_data%>%mutate(grouped_val = data[[coldc_type[input$categorise]]])
                grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
                if(input$aggregate == 'Sum'){
                  l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l1<- summarise(grouped_data, sum = length(label1))
                }else{
                  l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
                grouped_data<-grouped_data%>%mutate(grouped_val = data[[coldc_type[input$categorise]]])
                grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
                if(input$aggregate == 'Sum'){
                  l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l2<- summarise(grouped_data, sum = length(label2))
                }else{
                  l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
                }
                labels<- paste(input$preceding1, round(l1$sum/as.numeric(input$unit1),2), input$succeeding1, '\n',
                               input$preceding2, round(l2$sum/as.numeric(input$unit2),2), input$succeeding2)
              }
              else if(no_of_clicks == 3){
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
                grouped_data<-grouped_data%>%mutate(grouped_val = data[[coldc_type[input$categorise]]])
                grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
                if(input$aggregate == 'Sum'){
                  l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l1<- summarise(grouped_data, sum = length(label1))
                }else{
                  l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
                grouped_data<-grouped_data%>%mutate(grouped_val = data[[coldc_type[input$categorise]]])
                grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
                if(input$aggregate == 'Sum'){
                  l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l2<- summarise(grouped_data, sum = length(label2))
                }else{
                  l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label3 = data[[input$column3]])
                grouped_data<-grouped_data%>%mutate(grouped_val = data[[coldc_type[input$categorise]]])
                grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
                if(input$aggregate == 'Sum'){
                  l3<- summarise(grouped_data, sum = sum(label3, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l3<- summarise(grouped_data, sum = mean(label3, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l3<- summarise(grouped_data, sum = length(label3))
                }else{
                  l3<- summarise(grouped_data, sum = median(label3, na.rm = TRUE))
                }
                labels<- paste(input$preceding1, round(l1$sum/as.numeric(input$unit1),2), input$succeeding1, '\n',
                               input$preceding2, round(l2$sum/as.numeric(input$unit2),2), input$succeeding2,'\n',
                               input$preceding3, round(l3$sum/as.numeric(input$unit3),2), input$succeeding3)
              }
              else if(no_of_clicks == 4){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
                grouped_data<-grouped_data%>%mutate(grouped_val = data[[coldc_type[input$categorise]]])
                grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
                if(input$aggregate == 'Sum'){
                  l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l1<- summarise(grouped_data, sum = length(label1))
                }else{
                  l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
                grouped_data<-grouped_data%>%mutate(grouped_val = data[[coldc_type[input$categorise]]])
                grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
                if(input$aggregate == 'Sum'){
                  l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l2<- summarise(grouped_data, sum = length(label2))
                }else{
                  l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label3 = data[[input$column3]])
                grouped_data<-grouped_data%>%mutate(grouped_val = data[[coldc_type[input$categorise]]])
                grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
                if(input$aggregate == 'Sum'){
                  l3<- summarise(grouped_data, sum = sum(label3, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l3<- summarise(grouped_data, sum = mean(label3, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l3<- summarise(grouped_data, sum = length(label3))
                }else{
                  l3<- summarise(grouped_data, sum = median(label3, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label4 = data[[input$column4]])
                grouped_data<-grouped_data%>%mutate(grouped_val = data[[coldc_type[input$categorise]]])
                grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
                if(input$aggregate == 'Sum'){
                  l4<- summarise(grouped_data, sum = sum(label4, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l4<- summarise(grouped_data, sum = mean(label4, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l4<- summarise(grouped_data, sum = length(label4))
                }else{
                  l4<- summarise(grouped_data, sum = median(label4, na.rm = TRUE))
                }
                labels<- paste(input$preceding1, round(l1$sum/as.numeric(input$unit1),2), input$succeeding1, '\n',
                               input$preceding2, round(l2$sum/as.numeric(input$unit2),2), input$succeeding2,'\n',
                               input$preceding3, round(l3$sum/as.numeric(input$unit3),2), input$succeeding3,'\n',
                               input$preceding4, round(l4$sum/as.numeric(input$unit4),2), input$succeeding4)
              }
              
              
              if(input$a_d == 'Ascending'){
                plt<-vized_data%>%plot_ly(x=~reorder(x_axis, sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text', color =~grouped_val,colors=~colourss)%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = input$category_type)
                
              }else if(input$a_d == 'Descending'){
                plt<-vized_data%>%plot_ly(x=~reorder(x_axis, -sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text', color =~grouped_val, colors=~colourss)%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = input$category_type)
              }
              
            }else if (catornum[input$categorise] == 'numeric'){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                if(input$date_format == 'Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                }
                
              }
              grouped_data<-grouped_data%>%mutate(label1 = grouped_data[[col_type[input$y_axis]]])
              grouped_data<-grouped_data%>%mutate(grouped_val = grouped_data[[col_type[input$categorise]]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                vized_data<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE), hue = sum(grouped_val, na.rm=TRUE))
              }else if(input$aggregate == 'Average'){
                vized_data<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE), hue = mean(grouped_val, na.rm=TRUE))
              }else if(input$aggregate == 'Count'){
                vized_data<- summarise(grouped_data, sum = length(label1), hue = length(grouped_val))
              }else{
                vized_data<- summarise(grouped_data, sum = median(label1, na.rm = TRUE), hue = median(grouped_val, na.rm=TRUE))
              }
              if(input$x_axis_as_label == TRUE){
                labelx<- paste(input$x_preceding, vized_data$x_axis, input$x_succeeding)
              }
              
              if(no_of_clicks == 1){
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
                grouped_data<-grouped_data%>%group_by(x_axis)
                if(input$aggregate == 'Sum'){
                  l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l1<- summarise(grouped_data, sum = length(label1))
                }else{
                  l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
                }
                labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2) ,input$succeeding1)
              }else if(no_of_clicks == 2){
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
                grouped_data<-grouped_data%>%group_by(x_axis)
                if(input$aggregate == 'Sum'){
                  l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l1<- summarise(grouped_data, sum = length(label1))
                }else{
                  l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
                grouped_data<-grouped_data%>%group_by(x_axis)
                
                if(input$aggregate == 'Sum'){
                  l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l2<- summarise(grouped_data, sum = length(label2))
                }else{
                  l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
                }
                labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2) ,input$succeeding1, '\n',
                               input$preceding2,round(l2$sum/as.numeric(input$unit2),2) ,input$succeeding2)
                
              }else if(no_of_clicks == 3){
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
                grouped_data<-grouped_data%>%group_by(x_axis)
                if(input$aggregate == 'Sum'){
                  l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l1<- summarise(grouped_data, sum = length(label1))
                }else{
                  l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
                grouped_data<-grouped_data%>%group_by(x_axis)
                if(input$aggregate == 'Sum'){
                  l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l2<- summarise(grouped_data, sum = length(label2))
                }else{
                  l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label3 = data[[input$column3]])
                grouped_data<-grouped_data%>%group_by(x_axis)
                if(input$aggregate == 'Sum'){
                  l3<- summarise(grouped_data, sum = sum(label3, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l3<- summarise(grouped_data, sum = mean(label3, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l3<- summarise(grouped_data, sum = length(label3))
                }else{
                  l3<- summarise(grouped_data, sum = median(label3, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2) ,input$succeeding1, '\n',
                               input$preceding2,round(l2$sum/as.numeric(input$unit2),2) ,input$succeeding2,'\n',
                               input$preceding3,round(l3$sum/as.numeric(input$unit3),2) ,input$succeeding3)
                
              } else if(no_of_clicks ==4){
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
                grouped_data<-grouped_data%>%group_by(x_axis)
                if(input$aggregate == 'Sum'){
                  l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l1<- summarise(grouped_data, sum = length(label1))
                }else{
                  l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
                grouped_data<-grouped_data%>%group_by(x_axis)
                
                if(input$aggregate == 'Sum'){
                  l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l2<- summarise(grouped_data, sum = length(label2))
                }else{
                  l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label3 = data[[input$column3]])
                grouped_data<-grouped_data%>%group_by(x_axis)
                
                if(input$aggregate == 'Sum'){
                  l3<- summarise(grouped_data, sum = sum(label3, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l3<- summarise(grouped_data, sum = mean(label3, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l3<- summarise(grouped_data, sum = length(label3))
                }else{
                  l3<- summarise(grouped_data, sum = median(label3, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label4 = data[[input$column4]])
                grouped_data<-grouped_data%>%group_by(x_axis)
                
                if(input$aggregate == 'Sum'){
                  l4<- summarise(grouped_data, sum = sum(label4, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l4<- summarise(grouped_data, sum = mean(label4, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l4<- summarise(grouped_data, sum = length(label4))
                }else{
                  l4<- summarise(grouped_data, sum = median(label4, na.rm = TRUE))
                }
                labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2) ,input$succeeding1, '\n',
                               input$preceding2,round(l2$sum/as.numeric(input$unit2),2) ,input$succeeding2,'\n',
                               input$preceding3,round(l3$sum/as.numeric(input$unit3),2) ,input$succeeding3,'\n',
                               input$preceding4,round(l4$sum/as.numeric(input$unit4),2) ,input$succeeding4)
              }
              print(class(vized_data$hue))
              View(vized_data)
              if(input$label_text == TRUE){
                if(input$a_d == 'Ascending'){
                  plt<-vized_data%>%plot_ly(x=~reorder(x_axis,sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text', color =as.character(hue),colors= input$colour_scale, textposition = 'outside',texttemplate = '%{text}',outsidetextfont = list(color = input$colour_text),colors=input$colour_scale)%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline,range = reorder(x_axis, sum),colors=input$colour_scale), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                  
                }else if(input$a_d == 'Descending'){
                  plt<-vized_data%>%plot_ly(x=~reorder(x_axis,-sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text', color =~as.character(hue),colors = input$colour_scale, textposition = 'outside',texttemplate = '%{text}',outsidetextfont = list(color = input$colour_text),colors=input$colour_scale)%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline,range = reorder(x_axis, sum)), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }
                
                
              }else if (input$label_text == FALSE){
                View(vized_data)
                if(input$a_d == 'Ascending'){
                  plt<-vized_data%>%plot_ly(x=~reorder(x_axis, sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text', color =~as.character(hue),colors=input$colour_scale)%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = input$category_type)
                  
                }else if(input$a_d == 'Descending'){
                  plt<-vized_data%>%plot_ly(x=~reorder(x_axis, -sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text', color =~as.character(hue), colors = input$colour_scale)%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = input$category_type)
                }
              }
              
            }else if(catornum[input$categorise] == 'Date'){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                if(input$date_format == 'Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                }
                
              }
              
              grouped_data<-grouped_data%>%mutate(label1 = grouped_data[[col_type[input$y_axis]]])
              if(input$date_format_cat == 'Month Year'){
                grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(grouped_data[[col_type[input$categorise]]], label=TRUE), year(grouped_data[[col_type[input$categorise]]])))
              }else if(input$date_format_cat == 'Day Month Year'){
                grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(grouped_data[[col_type[input$categorise]]]), month(grouped_data[[col_type[input$categorise]]], label=TRUE), year(grouped_data[[col_type[input$categorise]]])))
              }else if(input$date_format_cat == 'Day Month'){
                grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(grouped_data[[col_type[input$categorise]]]), month(grouped_data[[col_type[input$categorise]]], label=TRUE)))
              }else if(input$date_format_cat == 'Month'){
                grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(grouped_data[[col_type[input$categorise]]], label=TRUE)))
              }else if(input$date_format_cat == 'Year'){
                grouped_data<-grouped_data%>%mutate(grouped_val = paste(year(grouped_data[[col_type[input$categorise]]])))
              }
              
              grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
              if(input$aggregate == 'Sum'){
                vized_data<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                vized_data<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                vized_data<- summarise(grouped_data, sum = length(label1))
              }else{
                vized_data<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              if(input$x_axis_as_label == TRUE){
                labelx<- paste(input$x_preceding, vized_data$x_axis, input$x_succeeding)
              }
              if(input$categorise_as_label == TRUE){
                labelcat<- paste(input$cat_preceding, vized_data$grouped_val, input$cat_succeeding)
              }
              if(no_of_clicks == 1){
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
                if(input$date_format_cat == 'Month Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(grouped_data[[col_type[input$categorise]]], label=TRUE), year(grouped_data[[col_type[input$categorise]]])))
                }else if(input$date_format_cat == 'Day Month Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(grouped_data[[col_type[input$categorise]]]), month(grouped_data[[col_type[input$categorise]]], label=TRUE), year(grouped_data[[col_type[input$categorise]]])))
                }else if(input$date_format_cat == 'Day Month'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(grouped_data[[col_type[input$categorise]]]), month(grouped_data[[col_type[input$categorise]]], label=TRUE)))
                }else if(input$date_format_cat == 'Month'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(grouped_data[[col_type[input$categorise]]], label=TRUE)))
                }else if(input$date_format_cat == 'Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(year(grouped_data[[col_type[input$categorise]]])))
                }
                grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
                if(input$aggregate == 'Sum'){
                  l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l1<- summarise(grouped_data, sum = length(label1))
                }else{
                  l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
                }
                labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2) ,input$succeeding1)
              }
              else if(no_of_clicks == 2){
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
                if(input$date_format_cat == 'Month Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(grouped_data[[col_type[input$categorise]]], label=TRUE), year(grouped_data[[col_type[input$categorise]]])))
                }else if(input$date_format_cat == 'Day Month Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(grouped_data[[col_type[input$categorise]]]), month(grouped_data[[col_type[input$categorise]]], label=TRUE), year(grouped_data[[col_type[input$categorise]]])))
                }else if(input$date_format_cat == 'Day Month'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(grouped_data[[col_type[input$categorise]]]), month(grouped_data[[col_type[input$categorise]]], label=TRUE)))
                }else if(input$date_format_cat == 'Month'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(grouped_data[[col_type[input$categorise]]], label=TRUE)))
                }else if(input$date_format_cat == 'Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(year(grouped_data[[col_type[input$categorise]]])))
                }
                grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
                if(input$aggregate == 'Sum'){
                  l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l1<- summarise(grouped_data, sum = length(label1))
                }else{
                  l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
                if(input$date_format_cat == 'Month Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(grouped_data[[col_type[input$categorise]]], label=TRUE), year(grouped_data[[col_type[input$categorise]]])))
                }else if(input$date_format_cat == 'Day Month Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(grouped_data[[col_type[input$categorise]]]), month(grouped_data[[col_type[input$categorise]]], label=TRUE), year(grouped_data[[col_type[input$categorise]]])))
                }else if(input$date_format_cat == 'Day Month'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(grouped_data[[col_type[input$categorise]]]), month(grouped_data[[col_type[input$categorise]]], label=TRUE)))
                }else if(input$date_format_cat == 'Month'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(grouped_data[[col_type[input$categorise]]], label=TRUE)))
                }else if(input$date_format_cat == 'Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(year(grouped_data[[col_type[input$categorise]]])))
                }
                grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
                if(input$aggregate == 'Sum'){
                  l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l2<- summarise(grouped_data, sum = length(label2))
                }else{
                  l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
                }
                labels<- paste(input$preceding1, round(l1$sum/as.numeric(input$unit1),2), input$succeeding1, '\n',
                               input$preceding2, round(l2$sum/as.numeric(input$unit2),2), input$succeeding2)
              }
              else if(no_of_clicks == 3){
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
                if(input$date_format_cat == 'Month Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(grouped_data[[col_type[input$categorise]]], label=TRUE), year(grouped_data[[col_type[input$categorise]]])))
                }else if(input$date_format_cat == 'Day Month Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(grouped_data[[col_type[input$categorise]]]), month(grouped_data[[col_type[input$categorise]]], label=TRUE), year(grouped_data[[col_type[input$categorise]]])))
                }else if(input$date_format_cat == 'Day Month'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(grouped_data[[col_type[input$categorise]]]), month(grouped_data[[col_type[input$categorise]]], label=TRUE)))
                }else if(input$date_format_cat == 'Month'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(grouped_data[[col_type[input$categorise]]], label=TRUE)))
                }else if(input$date_format_cat == 'Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(year(grouped_data[[col_type[input$categorise]]])))
                }
                grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
                if(input$aggregate == 'Sum'){
                  l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l1<- summarise(grouped_data, sum = length(label1))
                }else{
                  l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
                
                if(input$date_format_cat == 'Month Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(grouped_data[[col_type[input$categorise]]], label=TRUE), year(grouped_data[[col_type[input$categorise]]])))
                }else if(input$date_format_cat == 'Day Month Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(grouped_data[[col_type[input$categorise]]]), month(grouped_data[[col_type[input$categorise]]], label=TRUE), year(grouped_data[[col_type[input$categorise]]])))
                }else if(input$date_format_cat == 'Day Month'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(grouped_data[[col_type[input$categorise]]]), month(grouped_data[[col_type[input$categorise]]], label=TRUE)))
                }else if(input$date_format_cat == 'Month'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(grouped_data[[col_type[input$categorise]]], label=TRUE)))
                }else if(input$date_format_cat == 'Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(year(grouped_data[[col_type[input$categorise]]])))
                }
                grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
                if(input$aggregate == 'Sum'){
                  l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l2<- summarise(grouped_data, sum = length(label2))
                }else{
                  l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label3 = data[[input$column3]])
                if(input$date_format_cat == 'Month Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(grouped_data[[col_type[input$categorise]]], label=TRUE), year(grouped_data[[col_type[input$categorise]]])))
                }else if(input$date_format_cat == 'Day Month Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(grouped_data[[col_type[input$categorise]]]), month(grouped_data[[col_type[input$categorise]]], label=TRUE), year(grouped_data[[col_type[input$categorise]]])))
                }else if(input$date_format_cat == 'Day Month'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(grouped_data[[col_type[input$categorise]]]), month(grouped_data[[col_type[input$categorise]]], label=TRUE)))
                }else if(input$date_format_cat == 'Month'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(grouped_data[[col_type[input$categorise]]], label=TRUE)))
                }else if(input$date_format_cat == 'Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(year(grouped_data[[col_type[input$categorise]]])))
                }
                grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
                if(input$aggregate == 'Sum'){
                  l3<- summarise(grouped_data, sum = sum(label3, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l3<- summarise(grouped_data, sum = mean(label3, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l3<- summarise(grouped_data, sum = length(label3))
                }else{
                  l3<- summarise(grouped_data, sum = median(label3, na.rm = TRUE))
                }
                labels<- paste(input$preceding1, round(l1$sum/as.numeric(input$unit1),2), input$succeeding1, '\n',
                               input$preceding2, round(l2$sum/as.numeric(input$unit2),2), input$succeeding2,'\n',
                               input$preceding3, round(l3$sum/as.numeric(input$unit3),2), input$succeeding3)
              }
              else if(no_of_clicks == 4){
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
                if(input$date_format_cat == 'Month Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(grouped_data[[col_type[input$categorise]]], label=TRUE), year(grouped_data[[col_type[input$categorise]]])))
                }else if(input$date_format_cat == 'Day Month Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(grouped_data[[col_type[input$categorise]]]), month(grouped_data[[col_type[input$categorise]]], label=TRUE), year(grouped_data[[col_type[input$categorise]]])))
                }else if(input$date_format_cat == 'Day Month'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(grouped_data[[col_type[input$categorise]]]), month(grouped_data[[col_type[input$categorise]]], label=TRUE)))
                }else if(input$date_format_cat == 'Month'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(grouped_data[[col_type[input$categorise]]], label=TRUE)))
                }else if(input$date_format_cat == 'Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(year(grouped_data[[col_type[input$categorise]]])))
                }
                grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
                if(input$aggregate == 'Sum'){
                  l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l1<- summarise(grouped_data, sum = length(label1))
                }else{
                  l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
                if(input$date_format_cat == 'Month Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(grouped_data[[col_type[input$categorise]]], label=TRUE), year(grouped_data[[col_type[input$categorise]]])))
                }else if(input$date_format_cat == 'Day Month Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(grouped_data[[col_type[input$categorise]]]), month(grouped_data[[col_type[input$categorise]]], label=TRUE), year(grouped_data[[col_type[input$categorise]]])))
                }else if(input$date_format_cat == 'Day Month'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(grouped_data[[col_type[input$categorise]]]), month(grouped_data[[col_type[input$categorise]]], label=TRUE)))
                }else if(input$date_format_cat == 'Month'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(grouped_data[[col_type[input$categorise]]], label=TRUE)))
                }else if(input$date_format_cat == 'Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(year(grouped_data[[col_type[input$categorise]]])))
                }
                grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
                if(input$aggregate == 'Sum'){
                  l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l2<- summarise(grouped_data, sum = length(label2))
                }else{
                  l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
                }
                
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label3 = data[[input$column3]])
                if(input$date_format_cat == 'Month Year'){
                  grouped_data<-data%>%mutate(grouped_val = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format_cat == 'Day Month Year'){
                  grouped_data<-data%>%mutate(grouped_val = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format_cat == 'Day Month'){
                  grouped_data<-data%>%mutate(grouped_val = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format_cat == 'Month'){
                  grouped_data<-data%>%mutate(grouped_val = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format_cat == 'Year'){
                  grouped_data<-data%>%mutate(grouped_val = paste(year(data[[col_type[input$x_axis]]])))
                }
                grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
                if(input$aggregate == 'Sum'){
                  l3<- summarise(grouped_data, sum = sum(label3, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l3<- summarise(grouped_data, sum = mean(label3, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l3<- summarise(grouped_data, sum = length(label3))
                }else{
                  l3<- summarise(grouped_data, sum = median(label3, na.rm = TRUE))
                }
                if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                  grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
                }else{
                  if(input$date_format == 'Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                  }else if(input$date_format == 'Day Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Month'){
                    grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                  }else if(input$date_format == 'Year'){
                    grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                  }
                  
                }
                grouped_data<-grouped_data%>%mutate(label4 = data[[input$column4]])
                if(input$date_format_cat == 'Month Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(grouped_data[[col_type[input$categorise]]], label=TRUE), year(grouped_data[[col_type[input$categorise]]])))
                }else if(input$date_format_cat == 'Day Month Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(grouped_data[[col_type[input$categorise]]]), month(grouped_data[[col_type[input$categorise]]], label=TRUE), year(grouped_data[[col_type[input$categorise]]])))
                }else if(input$date_format_cat == 'Day Month'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(grouped_data[[col_type[input$categorise]]]), month(grouped_data[[col_type[input$categorise]]], label=TRUE)))
                }else if(input$date_format_cat == 'Month'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(grouped_data[[col_type[input$categorise]]], label=TRUE)))
                }else if(input$date_format_cat == 'Year'){
                  grouped_data<-grouped_data%>%mutate(grouped_val = paste(year(grouped_data[[col_type[input$categorise]]])))
                }
                grouped_data<-grouped_data%>%group_by(x_axis, grouped_val)
                if(input$aggregate == 'Sum'){
                  l4<- summarise(grouped_data, sum = sum(label4, na.rm = TRUE))
                }else if(input$aggregate == 'Average'){
                  l4<- summarise(grouped_data, sum = mean(label4, na.rm = TRUE))
                }else if(input$aggregate == 'Count'){
                  l4<- summarise(grouped_data, sum = length(label4))
                }else{
                  l4<- summarise(grouped_data, sum = median(label4, na.rm = TRUE))
                }
                labels<- paste(input$preceding1, round(l1$sum/as.numeric(input$unit1),2), input$succeeding1, '\n',
                               input$preceding2, round(l2$sum/as.numeric(input$unit2),2), input$succeeding2,'\n',
                               input$preceding3, round(l3$sum/as.numeric(input$unit3),2), input$succeeding3,'\n',
                               input$preceding4, round(l4$sum/as.numeric(input$unit4),2), input$succeeding4)
              }
              
              
              if(input$a_d == 'Ascending'){
                plt<-vized_data%>%plot_ly(x=~reorder(x_axis, sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text', color =~grouped_val, colors = colourss)%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline))
              }else if(input$a_d == 'Descending'){
                plt<-vized_data%>%plot_ly(x=~reorder(x_axis, -sum), y=~sum, type = 'bar', text =~paste(labels,'\n', labelx), hoverinfo = 'text', label = 'text', color =~grouped_val, colors = colourss)%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline))
              }
              
            }
            
          }
        }else if(input$graph_type == 'Histogram'){
          units<- c('Original' = 1, 'Thousands' = 1000, 'Millions' = 1000000, 'Billions' = 1000000000)
          labels<- ''
          options<- paste(names(data), '(', collumn_dc,')')
          col_type<- vector()
          col_type[options]<- names(data)
          options<- paste(names(data), '(',collumn_dc,')')
          catornum<- vector()
          catornum[options]<- collumn_dc
          coldc_type<- vector()
          coldc_type[options]<- names(data)
          colourss<- vector()
          if(is.null(input$categorise)){
            data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]], type = 'histogram',text =~'', hoverinfo= 'text', nbinsx=input$bin_size)%>%config(displayModeBar = FALSE)%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
            
          }else{
            if(input$categorise != 'None'){
              for(i in 1:length(unique(data[[coldc_type[input$categorise]]]))){
                ran_col<-randomColor()
                while(ran_col %in% colourss){
                  ran_col<- randomColor()
                }
                colourss[i]<- randomColor()
              }
            }
            if (input$categorise == 'None'){
              if(input$x_axis_as_label == TRUE){
                plt<-data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]], type = 'histogram',text =~paste(input$x_succeeding), hoverinfo= 'y+text', marker=list(color=input$chart_colour), nbinsx=input$bin_size)%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
              }else{
                plt<-data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]], type = 'histogram',text =~'', hoverinfo= 'text', marker=list(color=input$chart_colour), nbinsx=input$bin_size)%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
              }
              
            }else if(catornum[input$categorise] == 'Date'){
              if(input$date_format_cat == 'Month Year'){
                grouped_data<-data%>%mutate(grouped_val = paste(month(grouped_data[[col_type[input$categorise]]], label=TRUE), year(grouped_data[[col_type[input$categorise]]])))
              }else if(input$date_format_cat == 'Day Month Year'){
                grouped_data<-data%>%mutate(grouped_val = paste(day(data[[coldc_type[input$categorise]]]), month(data[[coldc_type[input$categorise]]], label=TRUE), year(data[[coldc_type[input$categorise]]])))
              }else if(input$date_format_cat == 'Day Month'){
                grouped_data<-data%>%mutate(grouped_val = paste(day(data[[coldc_type[input$categorise]]]), month(data[[coldc_type[input$categorise]]], label=TRUE)))
              }else if(input$date_format_cat == 'Month'){
                grouped_data<-data%>%mutate(grouped_val = paste(month(data[[coldc_type[input$categorise]]], label=TRUE)))
              }else if(input$date_format_cat == 'Year'){
                grouped_data<-data%>%mutate(grouped_val = paste(year(data[[coldc_type[input$categorise]]])))
              }
              
              if(input$x_axis_as_label == TRUE & input$categorise_as_label == TRUE){
                plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]], type = 'histogram',text =~paste(input$x_succeeding,'\n', '\n',input$cat_preceding, grouped_val, input$cat_succeeding), hoverinfo= 'y+text', color=~grouped_val, colors=~colourss, nbinsx=input$bin_size)%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
              }else if(input$x_axis_as_label == TRUE ){
                plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]], type = 'histogram',text =~paste(input$x_succeeding), hoverinfo= 'y+text', color=~grouped_val, colors=~colourss, nbinsx=input$bin_size)%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
              }else if(input$categorise_as_label == TRUE ){
                plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]], type = 'histogram',text =~paste(input$cat_preceding, grouped_val, input$cat_succeeding), hoverinfo= 'text', color=~grouped_val, colors=~colourss, nbinsx=input$bin_size)%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
              }else{
                plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]], type = 'histogram',text =~'', hoverinfo= 'text', color=~grouped_val, colors=~colourss, nbinsx=input$bin_size)%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
              }
              
            }else if(catornum[input$categorise] == 'categorical'){
              
              grouped_data<-data%>%mutate(grouped_val = data[[coldc_type[input$categorise]]])
              
              if(input$x_axis_as_label == TRUE & input$categorise_as_label == TRUE){
                plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]], type = 'histogram',text =~paste(input$x_succeeding,'\n', '\n',input$cat_preceding, grouped_val, input$cat_succeeding), hoverinfo= 'y+text', color=~grouped_val, colors=~colourss, nbinsx=input$bin_size)%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
              }else if(input$x_axis_as_label == TRUE ){
                plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]], type = 'histogram',text =~paste(input$x_succeeding), hoverinfo= 'y+text', color=~grouped_val, colors=~colourss, nbinsx=input$bin_size)%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
              }else if(input$categorise_as_label == TRUE ){
                plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]], type = 'histogram',text =~paste(input$cat_preceding, grouped_val, input$cat_succeeding), hoverinfo= 'text', color=~grouped_val, colors=~colourss, nbinsx=input$bin_size)%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
              }else{
                plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]], type = 'histogram',text =~'', hoverinfo= 'text', color=~grouped_val, colors=~colourss, nbinsx=input$bin_size)%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
              }
              
              
            }
          }
          
        }else if(input$graph_type == 'Pie chart'){
          columns<-vector()
          col_type<- vector()
          options<- paste(names(data), '(', collumn_dc, ')')
          col_type[options]<- names(data)
          for(col in names(data)){
            if(col==col_type[input$x_axis]|col==col_type[input$y_axis]){
              
            }else{
              columns[length(columns)+1]<- col
            }
          }
          
          if(length(columns)!=0&input$animate){
            if(class(data[[col_type[input$x_axis]]]) != 'Date'){
              data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
            }else{
              if(input$date_format == 'Month Year'){
                data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }else if(input$date_format == 'Day Month Year'){
                data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }else if(input$date_format == 'Day Month'){
                data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
              }else if(input$date_format == 'Month'){
                data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
              }else if(input$date_format == 'Year'){
                data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
              }
              
            }
            data<-data%>%mutate(label1 = data[[col_type[input$y_axis]]])
            data<-data%>%mutate(animate = data[[input$animate_column]])
            nums<-vector()
            for(collumn in names(data)){
              if(mean(is.na(as.numeric(data[[collumn]]))==TRUE)<=0.07&class(data[[collumn]])!='Date'){
                nums[length(nums)+1]<- collumn
              }
            }
            if(class(data[[input$animate_column]])=='Date'|input$animate_column%in%nums){
              if(input$animate_column%in%nums){
                data<- data%>%arrange(as.numeric(animate))
              }
              if(class(data[[input$animate_column]])=='Date'){
                data<- data%>%arrange(animate)
              }
              data<- data%>%mutate(rank = 1:length(animate))
              
              if(class(data[[input$animate_column]])=='Date'){
                if(input$date_format_animate == 'Month Year'){
                  data<-data%>%mutate(date = paste(month(data[['animate']], label=TRUE), year(data[['animate']])))
                }else if(input$date_format_animate == 'Day Month Year'){
                  data<-data%>%mutate(date = paste(day(data[['animate']]), month(data[['animate']], label=TRUE), year(data[['animate']])))
                }else if(input$date_format_animate == 'Day Month'){
                  data<-data%>%mutate(date = paste(day(data[['animate']]), month(data[['animate']], label=TRUE)))
                }else if(input$date_format_animate == 'Month'){
                  data<-data%>%mutate(date = paste(month(data[['animate']], label=TRUE)))
                }else if(input$date_format_animate == 'Year'){
                  data<-data%>%mutate(date = paste(year(data[['animate']])))
                }
              }else if(input$animate_column%in%nums){
                data<-data%>%mutate(date = data[[input$animate_column]])
              }
              grouped_data<-data%>%group_by(x_axis,date)
              if(input$aggregate == 'Sum'){
                vized_data<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE), rank = max(rank, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                vized_data<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE), rank = max(rank, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                vized_data<- summarise(grouped_data, sum = length(label1), rank = max(rank, na.rm = TRUE))
              }else{
                vized_data<- summarise(grouped_data, sum = median(label1, na.rm = TRUE), rank= max(rank, na.rm = TRUE))
              }
              if(input$slider_visibility==TRUE){
                plt<-vized_data%>%plot_ly(labels=~x_axis, values=~sum, type = 'pie', text =~paste(labels,'\n', labelx), hoverinfo = 'text', textinfo = 'none', hole = input$hole,frame=~reorder(date,rank))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(hide=F,currentvalue = list(prefix = "", font = list(color="red")))%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
              }else{
                plt<-vized_data%>%plot_ly(labels=~x_axis, values=~sum, type = 'pie', text =~paste(labels,'\n', labelx), hoverinfo = 'text', textinfo = 'none', hole = input$hole,frame=~reorder(date,rank))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")%>%animation_slider(hide=T)
              }
            }else{
              data<-data%>%mutate(date = data[[input$animate_column]])
              grouped_data<-data%>%group_by(x_axis,date)
              if(input$aggregate == 'Sum'){
                vized_data<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                vized_data<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                vized_data<- summarise(grouped_data, sum = length(label1))
              }else{
                vized_data<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              if(input$slider_visibility==TRUE){
                plt<-vized_data%>%plot_ly(labels=~x_axis, values=~sum, type = 'pie', text =~'', hoverinfo = 'text', textinfo = 'none', hole = input$hole,frame=~date)%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(hide=F,currentvalue = list(prefix = "", font = list(color="red")))%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
              }else{
                plt<-vized_data%>%plot_ly(labels=~x_axis, values=~sum, type = 'pie', text =~'', hoverinfo = 'text', textinfo = 'none', hole = input$hole,frame=~date)%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")%>%animation_slider(hide=T)
              }
              
            }
          }else{
            
            data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
            data<-data%>%mutate(label1 = data[[col_type[input$y_axis]]])
            grouped_data<-data%>%group_by(x_axis)
            if(input$aggregate == 'Sum'){
              vized_data<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
            }else if(input$aggregate == 'Average'){
              vized_data<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
            }else if(input$aggregate == 'Count'){
              vized_data<- summarise(grouped_data, sum = length(label1))
            }else{
              vized_data<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
            }
            if(input$x_axis_as_label == TRUE){
              labelx<- paste(input$x_preceding, vized_data$x_axis, input$x_succeeding)
            }
            
            if(no_of_clicks == 1){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l1<- summarise(grouped_data, sum = length(label1))
              }else{
                l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              
              print(class(l1$sum))
              labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2) ,input$succeeding1)
            }else if(no_of_clicks == 2){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l1<- summarise(grouped_data, sum = length(label1))
              }else{
                l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              
              if(input$aggregate == 'Sum'){
                l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l2<- summarise(grouped_data, sum = length(label2))
              }else{
                l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
              }
              labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2) ,input$succeeding1, '\n',
                             input$preceding2,round(l2$sum/as.numeric(input$unit2),2) ,input$succeeding2)
              
            }else if(no_of_clicks == 3){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l1<- summarise(grouped_data, sum = length(label1))
              }else{
                l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l2<- summarise(grouped_data, sum = length(label2))
              }else{
                l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label3 = data[[input$column3]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l3<- summarise(grouped_data, sum = sum(label3, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l3<- summarise(grouped_data, sum = mean(label3, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l3<- summarise(grouped_data, sum = length(label3))
              }else{
                l3<- summarise(grouped_data, sum = median(label3, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2) ,input$succeeding1, '\n',
                             input$preceding2,round(l2$sum/as.numeric(input$unit2),2),input$succeeding2,'\n',
                             input$preceding3,round(l3$sum/as.numeric(input$unit3),2) ,input$succeeding3)
              
            } else if(no_of_clicks ==4){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l1<- summarise(grouped_data, sum = length(label1))
              }else{
                l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              
              if(input$aggregate == 'Sum'){
                l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l2<- summarise(grouped_data, sum = length(label2))
              }else{
                l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label3 = data[[input$column3]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              
              if(input$aggregate == 'Sum'){
                l3<- summarise(grouped_data, sum = sum(label3, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l3<- summarise(grouped_data, sum = mean(label3, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l3<- summarise(grouped_data, sum = length(label3))
              }else{
                l3<- summarise(grouped_data, sum = median(label3, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label4 = data[[input$column4]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              
              if(input$aggregate == 'Sum'){
                l4<- summarise(grouped_data, sum = sum(label4, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l4<- summarise(grouped_data, sum = mean(label4, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l4<- summarise(grouped_data, sum = length(label4))
              }else{
                l4<- summarise(grouped_data, sum = median(label4, na.rm = TRUE))
              }
              labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2),input$succeeding1, '\n',
                             input$preceding2,round(l2$sum/as.numeric(input$unit2),2) ,input$succeeding2,'\n',
                             input$preceding3,round(l3$sum/as.numeric(input$unit3),2) ,input$succeeding3,'\n',
                             input$preceding4,round(l4$sum/as.numeric(input$unit1),4) ,input$succeeding4)
            }
            if(input$label_text == TRUE){
              plt<-vized_data%>%plot_ly(labels=~x_axis, values=~sum, type = 'pie', text =~paste(labels,'\n', labelx), hoverinfo = 'text',texttemplate = '%{text}',textposition = 'inside',insidetextfont = list(color = input$colour_text), textinfo = 'none', hole = input$hole)%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')
            }else{
              plt<-vized_data%>%plot_ly(labels=~x_axis, values=~sum, type = 'pie', text =~paste(labels,'\n', labelx), hoverinfo = 'text', textinfo = 'none', hole = input$hole)%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, barmode = 'stack')
            }
          }
        }else if(input$graph_type == 'Time series plot'){
          
          if(0==1){
          }else{
            if(class(data[[col_type[input$x_axis]]]) != 'Date'){
              data<-data%>%mutate(x_axis = as.numeric(data[[col_type[input$x_axis]]]))
              data<- data%>%arrange(x_axis)
              data<- data%>%mutate(rank = 1:length(x_axis))
              grouped_data<-data
            }else{
              data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              data<- data%>%arrange(x_axis)
              data<- data%>%mutate(rank = 1:length(x_axis))
              if(input$date_format == 'Month Year'){
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }else if(input$date_format == 'Day Month Year'){
                grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }else if(input$date_format == 'Day Month'){
                grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
              }else if(input$date_format == 'Month'){
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
              }else if(input$date_format == 'Year'){
                grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
              }
              
            }
            grouped_data<-grouped_data%>%mutate(label1 = data[[col_type[input$y_axis]]])
            grouped_data<-grouped_data%>%group_by(x_axis)
            if(input$aggregate == 'Sum'){
              vized_data<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE), first_lab = max(rank, na.rm=TRUE))
            }else if(input$aggregate == 'Average'){
              vized_data<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE), first_lab = max(rank, na.rm=TRUE))
            }else if(input$aggregate == 'Count'){
              vized_data<- summarise(grouped_data, sum = length(label1), first_lab = max(rank, na.rm=TRUE))
            }else{
              vized_data<- summarise(grouped_data, sum = median(label1, na.rm = TRUE), first_lab = max(rank, na.rm=TRUE))
            }
            if(input$x_axis_as_label == TRUE){
              labelx<- paste(input$x_preceding, vized_data$x_axis, input$x_succeeding)
            }
            
            if(no_of_clicks == 1){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                data<-data%>%mutate(x_axis = as.numeric(data[[col_type[input$x_axis]]]))
                data<- data%>%arrange(x_axis)
                data<- data%>%mutate(rank = 1:length(x_axis))
                grouped_data<-data
              }else{
                if(input$date_format == 'Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                }
                
              }
              
              grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l1<- summarise(grouped_data, sum = length(label1))
              }else{
                l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              print(class(l1$sum))
              labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2) ,input$succeeding1)
            }else if(no_of_clicks == 2){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                data<-data%>%mutate(x_axis = as.numeric(data[[col_type[input$x_axis]]]))
                data<- data%>%arrange(x_axis)
                data<- data%>%mutate(rank = 1:length(x_axis))
                grouped_data<-data
              }else{
                if(input$date_format == 'Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                }
                
              }
              grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l1<- summarise(grouped_data, sum = length(label1))
              }else{
                l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                data<-data%>%mutate(x_axis = as.numeric(data[[col_type[input$x_axis]]]))
                data<- data%>%arrange(x_axis)
                data<- data%>%mutate(rank = 1:length(x_axis))
                grouped_data<-data
              }else{
                if(input$date_format == 'Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                }
                
              }
              grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              
              if(input$aggregate == 'Sum'){
                l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l2<- summarise(grouped_data, sum = length(label2))
              }else{
                l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
              }
              labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2) ,input$succeeding1, '\n',
                             input$preceding2,round(l2$sum/as.numeric(input$unit2),2) ,input$succeeding2)
              
            }else if(no_of_clicks == 3){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                data<-data%>%mutate(x_axis = as.numeric(data[[col_type[input$x_axis]]]))
                data<- data%>%arrange(x_axis)
                data<- data%>%mutate(rank = 1:length(x_axis))
                grouped_data<-data
              }else{
                if(input$date_format == 'Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                }
                
              }
              grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l1<- summarise(grouped_data, sum = length(label1))
              }else{
                l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                if(input$date_format == 'Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                }
                
              }
              grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l2<- summarise(grouped_data, sum = length(label2))
              }else{
                l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                data<-data%>%mutate(x_axis = as.numeric(data[[col_type[input$x_axis]]]))
                data<- data%>%arrange(x_axis)
                data<- data%>%mutate(rank = 1:length(x_axis))
                grouped_data<-data
              }else{
                if(input$date_format == 'Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                }
                
              }
              grouped_data<-grouped_data%>%mutate(label3 = data[[input$column3]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l3<- summarise(grouped_data, sum = sum(label3, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l3<- summarise(grouped_data, sum = mean(label3, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l3<- summarise(grouped_data, sum = length(label3))
              }else{
                l3<- summarise(grouped_data, sum = median(label3, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                data<-data%>%mutate(x_axis = as.numeric(data[[col_type[input$x_axis]]]))
                data<- data%>%arrange(x_axis)
                data<- data%>%mutate(rank = 1:length(x_axis))
                grouped_data<-data
              }else{
                if(input$date_format == 'Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                }
              }
              labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2) ,input$succeeding1, '\n',
                             input$preceding2,round(l2$sum/as.numeric(input$unit2),2),input$succeeding2,'\n',
                             input$preceding3,round(l3$sum/as.numeric(input$unit3),2) ,input$succeeding3)
              
            } else if(no_of_clicks ==4){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                data<-data%>%mutate(x_axis = as.numeric(data[[col_type[input$x_axis]]]))
                data<- data%>%arrange(x_axis)
                data<- data%>%mutate(rank = 1:length(x_axis))
                grouped_data<-data
              }else{
                if(input$date_format == 'Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                }
                
              }
              grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l1<- summarise(grouped_data, sum = length(label1))
              }else{
                l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                data<-data%>%mutate(x_axis = as.numeric(data[[col_type[input$x_axis]]]))
                data<- data%>%arrange(x_axis)
                data<- data%>%mutate(rank = 1:length(x_axis))
                grouped_data<-data
              }else{
                if(input$date_format == 'Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                }
                
              }
              grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              
              if(input$aggregate == 'Sum'){
                l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l2<- summarise(grouped_data, sum = length(label2))
              }else{
                l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                data<-data%>%mutate(x_axis = as.numeric(data[[col_type[input$x_axis]]]))
                data<- data%>%arrange(x_axis)
                data<- data%>%mutate(rank = 1:length(x_axis))
                grouped_data<-data
              }else{
                if(input$date_format == 'Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                }
                
              }
              grouped_data<-grouped_data%>%mutate(label3 = data[[input$column3]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              
              if(input$aggregate == 'Sum'){
                l3<- summarise(grouped_data, sum = sum(label3, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l3<- summarise(grouped_data, sum = mean(label3, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l3<- summarise(grouped_data, sum = length(label3))
              }else{
                l3<- summarise(grouped_data, sum = median(label3, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                data<-data%>%mutate(x_axis = as.numeric(data[[col_type[input$x_axis]]]))
                data<- data%>%arrange(x_axis)
                data<- data%>%mutate(rank = 1:length(x_axis))
                grouped_data<-data
              }else{
                if(input$date_format == 'Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                }
                
              }
              grouped_data<-grouped_data%>%mutate(label4 = data[[input$column4]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              
              if(input$aggregate == 'Sum'){
                l4<- summarise(grouped_data, sum = sum(label4, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l4<- summarise(grouped_data, sum = mean(label4, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l4<- summarise(grouped_data, sum = length(label4))
              }else{
                l4<- summarise(grouped_data, sum = median(label4, na.rm = TRUE))
              }
              labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2),input$succeeding1, '\n',
                             input$preceding2,round(l2$sum/as.numeric(input$unit2),2) ,input$succeeding2,'\n',
                             input$preceding3,round(l3$sum/as.numeric(input$unit3),2) ,input$succeeding3,'\n',
                             input$preceding4,round(l4$sum/as.numeric(input$unit1),4) ,input$succeeding4)
            }
            label_line<- ''
            if(input$trend_lines =='Average'){
              if(input$trend_line_as_label ==TRUE){
                label_line<- paste(input$trend_preceding, round(mean(vized_data$sum, na.rm=TRUE)/as.numeric(input$trend_unit),1),input$trend_succeeding )
              }
            }
            if(input$trend_lines =='Median'){
              if(input$trend_line_as_label ==TRUE){
                label_line<- paste(input$trend_preceding, round(median(vized_data$sum, na.rm=TRUE)/as.numeric(input$trend_unit),1),input$trend_succeeding )
              }
            }
            if(input$trend_lines =='Customise'){
              if(input$trend_line_as_label ==TRUE){
                label_line<- paste(input$trend_preceding, input$customised_val/as.numeric(input$trend_unit),input$trend_succeeding )
              }
            }
            if(input$trend_lines == 'Average'){
              plt<-vized_data%>%plot_ly(x=~reorder(x_axis, first_lab), y=~sum, text =~paste(labels,'\n', labelx), hoverinfo = 'text',texttemplate = '%{text}',textposition = 'outside', outsidetextfont = list(color = input$colour_text),line=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, marker=list(color=input$chart_colour))%>%add_lines()%>%add_lines(y=~mean(sum, na.rm= TRUE), line =~list(color = input$line_colour),text=~label_line, hoverinfo = 'text',showlegend = FALSE, marker=list(color=input$line_colour))
            }else if(input$trend_lines == 'Median'){
              plt<-vized_data%>%plot_ly(x=~reorder(x_axis, first_lab), y=~sum, text =~paste(labels,'\n', labelx), hoverinfo = 'text',texttemplate = '%{text}',textposition = 'outside', outsidetextfont = list(color = input$colour_text),line=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, marker=list(color=input$chart_colour))%>%add_lines()%>%add_lines(y=~median(sum, na.rm= TRUE), line =~list(color = input$line_colour),text=~label_line, hoverinfo = 'text',showlegend = FALSE, marker=list(color=input$line_colour))
            }else if(input$trend_lines == 'Customise'){
              plt<-vized_data%>%plot_ly(x=~reorder(x_axis, first_lab), y=~sum, text =~paste(labels,'\n', labelx), hoverinfo = 'text',texttemplate = '%{text}',textposition = 'outside', outsidetextfont = list(color = input$colour_text),line=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, marker=list(color=input$chart_colour))%>%add_lines()%>%add_lines(y=~as.numeric(input$customised_val), line =~list(color = input$line_colour),text=~label_line, hoverinfo = 'text',showlegend = FALSE, marker=list(color=input$line_colour))
            }else{
              plt<-vized_data%>%plot_ly(x=~reorder(x_axis, first_lab), y=~sum, text =~paste(labels,'\n', labelx), hoverinfo = 'text',texttemplate = '%{text}',textposition = 'outside', outsidetextfont = list(color = input$colour_text),line=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title, marker=list(color=input$chart_colour))%>%add_lines()
            }
            
          }
        }else if(input$graph_type == 'Box plot'){
          units<- c('Original' = 1, 'Thousands' = 1000, 'Millions' = 1000000, 'Billions' = 1000000000)
          labels<- ''
          options<- paste(names(data), '(', collumn_dc,')')
          col_type<- vector()
          col_type[options]<- names(data)
          options<- paste(names(data), '(',collumn_dc,')')
          catornum<- vector()
          catornum[options]<- collumn_dc
          coldc_type<- vector()
          coldc_type[options]<- names(data)
          colourss<- vector()
          columns<-vector()
          col_type<- vector()
          options<- paste(names(data), '(', collumn_dc, ')')
          col_type[options]<- names(data)
          for(col in names(data)){
            if(col==col_type[input$x_axis]|col==col_type[input$y_axis]){
              
            }else{
              columns[length(columns)+1]<- col
            }
          }
          
          if(length(columns)!=0&input$animate){
            data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
            data<-data%>%mutate(label1 = data[[col_type[input$y_axis]]])
            data<-data%>%mutate(animate = data[[input$animate_column]])
            nums<-vector()
            for(collumn in names(data)){
              if(mean(is.na(as.numeric(data[[collumn]]))==TRUE)<=0.07&class(data[[collumn]])!='Date'){
                nums[length(nums)+1]<- collumn
              }
            }
            if(class(data[[input$animate_column]])=='Date'|input$animate_column%in%nums){
              if(input$animate_column%in%nums){
                data<- data%>%arrange(as.numeric(animate))
              }
              if(class(data[[input$animate_column]])=='Date'){
                data<- data%>%arrange(animate)
              }
              data<- data%>%mutate(rank = 1:length(animate))
              
              if(class(data[[input$animate_column]])=='Date'){
                if(input$date_format_animate == 'Month Year'){
                  data<-data%>%mutate(date = paste(month(data[['animate']], label=TRUE), year(data[['animate']])))
                }else if(input$date_format_animate == 'Day Month Year'){
                  data<-data%>%mutate(date = paste(day(data[['animate']]), month(data[['animate']], label=TRUE), year(data[['animate']])))
                }else if(input$date_format_animate == 'Day Month'){
                  data<-data%>%mutate(date = paste(day(data[['animate']]), month(data[['animate']], label=TRUE)))
                }else if(input$date_format_animate == 'Month'){
                  data<-data%>%mutate(date = paste(month(data[['animate']], label=TRUE)))
                }else if(input$date_format_animate == 'Year'){
                  data<-data%>%mutate(date = paste(year(data[['animate']])))
                }
              }else if(input$animate_column%in%nums){
                data<-data%>%mutate(date = data[[input$animate_column]])
              }
              grouped_data<-data%>%group_by(date)
              vized_data<- summarise(grouped_data,x_axis=x_axis, sum = label1, rank = max(rank, na.rm = TRUE))
              
              if(input$slider_visibility==TRUE){
                if(input$a_d == 'Ascending'){
                  if(input$x_axis_as_label == TRUE){
                    plt<-vized_data%>% plot_ly(x=~reorder(x_axis,sum, FUN = median),y=~sum, type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour),frame=~reorder(date,rank))%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(currentvalue = list(prefix = "", font = list(color="red")))%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
                  }else{
                    plt<-vized_data%>% plot_ly(x=~reorder(x_axis,sum, FUN = median),y=~sum,text=~'', hoverinfo='text', type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour),frame=~reorder(date,rank))%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(currentvalue = list(prefix = "", font = list(color="red")))%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
                  }
                }else{
                  if(input$x_axis_as_label == TRUE){
                    plt<-vized_data%>% plot_ly(x=~reorder(x_axis,-sum, FUN = median),y=~sum, type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour),frame=~reorder(date,rank))%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(currentvalue = list(prefix = "", font = list(color="red")))%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
                  }else{
                    plt<-vized_data%>% plot_ly(x=~reorder(x_axis,-sum, FUN = median),y=~sum,text=~'', hoverinfo='text', type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour),frame=~reorder(date,rank))%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(currentvalue = list(prefix = "", font = list(color="red")))%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
                  }
                }
              }else{
                if(input$a_d == 'Ascending'){
                  if(input$x_axis_as_label == TRUE){
                    plt<-vized_data%>% plot_ly(x=~reorder(x_axis,sum, FUN = median),y=~sum, type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour),frame=~reorder(date,rank))%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(hide = TRUE)%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
                  }else{
                    plt<-vized_data%>% plot_ly(x=~reorder(x_axis,sum, FUN = median),y=~sum,text=~'', hoverinfo='text', type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour),frame=~reorder(date,rank))%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(hide = TRUE)%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
                  }
                }else{
                  if(input$x_axis_as_label == TRUE){
                    plt<-vized_data%>% plot_ly(x=~reorder(x_axis,-sum, FUN = median),y=~sum, type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour),frame=~reorder(date,rank))%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(hide = TRUE)%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
                  }else{
                    plt<-vized_data%>% plot_ly(x=~reorder(x_axis,-sum, FUN = median),y=~sum,text=~'', hoverinfo='text', type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour),frame=~reorder(date,rank))%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(hide = TRUE)%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
                  }
                }
              }
              
            }else{
              data<-data%>%mutate(date = data[[input$animate_column]])
              grouped_data<-data%>%group_by(date)
              if(input$aggregate == 'Sum'){
                vized_data<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                vized_data<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                vized_data<- summarise(grouped_data, sum = length(label1))
              }else{
                vized_data<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              plot_geo<- read_excel("usa_code.xlsx")
              vized_data<- vized_data%>%mutate(labels=paste(labels,'\n', labelx))
              real_data<- merge(vized_data, plot_geo, by.x='x_axis', by.y='State')
              
              if(input$slider_visibility==TRUE){
                if(input$a_d == 'Ascending'){
                  if(input$x_axis_as_label == TRUE){
                    plt<-vized_data%>% plot_ly(x=~reorder(x_axis,sum, FUN = median),y=~sum, type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour),frame=~date)%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title,)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(currentvalue = list(prefix = "", font = list(color="red")))%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
                  }else{
                    plt<-vized_data%>% plot_ly(x=~reorder(x_axis,sum, FUN = median),y=~sum,text=~'', hoverinfo='text', type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour),frame=~date)%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(currentvalue = list(prefix = "", font = list(color="red")))%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
                  }
                }else{
                  if(input$x_axis_as_label == TRUE){
                    plt<-vized_data%>% plot_ly(x=~reorder(x_axis,-sum, FUN = median),y=~sum, type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour),frame=~date)%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(currentvalue = list(prefix = "", font = list(color="red")))%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
                  }else{
                    plt<-vized_data%>% plot_ly(x=~reorder(x_axis,-sum, FUN = median),y=~sum,text=~'', hoverinfo='text', type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour),frame=~date)%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(currentvalue = list(prefix = "", font = list(color="red")))%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
                  }
                }
              }else{
                if(input$a_d == 'Ascending'){
                  if(input$x_axis_as_label == TRUE){
                    plt<-vized_data%>% plot_ly(x=~reorder(x_axis,sum, FUN = median),y=~sum, type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour),frame=~date)%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(hide = TRUE)%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
                  }else{
                    plt<-vized_data%>% plot_ly(x=~reorder(x_axis,sum, FUN = median),y=~sum,text=~'', hoverinfo='text', type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour),frame=~date)%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(hide = TRUE)%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
                  }
                }else{
                  if(input$x_axis_as_label == TRUE){
                    plt<-vized_data%>% plot_ly(x=~reorder(x_axis,-sum, FUN = median),y=~sum, type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour),frame=~date)%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(hide = TRUE)%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
                  }else{
                    plt<-vized_data%>% plot_ly(x=~reorder(x_axis,-sum, FUN = median),y=~sum,text=~'', hoverinfo='text', type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour),frame=~date)%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(hide = TRUE)%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
                  }
                }
              }
              
            }
            
          }else{
            if(input$categorise != 'None'){
              for(i in 1:length(unique(data[[coldc_type[input$categorise]]]))){
                ran_col<-randomColor()
                while(ran_col %in% colourss){
                  ran_col<- randomColor()
                }
                colourss[i]<- randomColor()
              }
            }
            if (input$categorise == 'None'){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                if(input$date_format == 'Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                }
                
              }
              
              if(input$a_d == 'Ascending'){
                if(input$x_axis_as_label == TRUE){
                  plt<-grouped_data%>% plot_ly(x=~reorder(x_axis,data[[coldc_type[input$y_axis]]], FUN = median),y=~data[[coldc_type[input$y_axis]]], type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour))%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else{
                  plt<-grouped_data%>% plot_ly(x=~reorder(x_axis,data[[coldc_type[input$y_axis]]], FUN = median),y=~data[[coldc_type[input$y_axis]]],text=~'', hoverinfo='text', type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour))%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }
              }else{
                if(input$x_axis_as_label == TRUE){
                  plt<-grouped_data%>% plot_ly(x=~reorder(x_axis,-data[[coldc_type[input$y_axis]]], FUN = median),y=~data[[coldc_type[input$y_axis]]], type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour))%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else{
                  plt<-grouped_data%>% plot_ly(x=~reorder(x_axis,-data[[coldc_type[input$y_axis]]], FUN = median),y=~data[[coldc_type[input$y_axis]]],text=~'', hoverinfo='text', type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour))%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }
              }
              
            }else if(catornum[input$categorise] == 'Date'){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                if(input$date_format == 'Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                }
                
              }
              if(input$date_format_cat == 'Month Year'){
                grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(data[[col_type[input$categorise]]], label=TRUE), year(data[[col_type[input$categorise]]])))
              }else if(input$date_format_cat == 'Day Month Year'){
                grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(data[[col_type[input$categorise]]]), month(data[[col_type[input$categorise]]], label=TRUE), year(data[[col_type[input$categorise]]])))
              }else if(input$date_format_cat == 'Day Month'){
                grouped_data<-grouped_data%>%mutate(grouped_val = paste(day(data[[col_type[input$categorise]]]), month(data[[col_type[input$categorise]]], label=TRUE)))
              }else if(input$date_format_cat == 'Month'){
                grouped_data<-grouped_data%>%mutate(grouped_val = paste(month(data[[col_type[input$categorise]]], label=TRUE)))
              }else if(input$date_format_cat == 'Year'){
                grouped_data<-grouped_data%>%mutate(grouped_val = paste(year(data[[col_type[input$categorise]]])))
              }
              
              if(input$a_d == 'Ascending'){
                if(input$x_axis_as_label == TRUE){
                  plt<-grouped_data%>% plot_ly(x=~reorder(x_axis,data[[coldc_type[input$y_axis]]], FUN = median),y=~data[[coldc_type[input$y_axis]]],color=~grouped_val, colors = colourss, type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour))%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else{
                  plt<-grouped_data%>% plot_ly(x=~reorder(x_axis,data[[coldc_type[input$y_axis]]], FUN = median),y=~data[[coldc_type[input$y_axis]]],color=~grouped_val, colors = colourss,text=~'', hoverinfo='text', type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour))%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }
              }else{
                if(input$x_axis_as_label == TRUE){
                  plt<-grouped_data%>% plot_ly(x=~reorder(x_axis,-data[[coldc_type[input$y_axis]]], FUN = median),y=~data[[coldc_type[input$y_axis]]],color=~grouped_val, colors = colourss, type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour))%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else{
                  plt<-grouped_data%>% plot_ly(x=~reorder(x_axis,-data[[coldc_type[input$y_axis]]], FUN = median),y=~data[[coldc_type[input$y_axis]]],color=~grouped_val, colors = colourss,text=~'', hoverinfo='text', type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour))%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }
              }
              
            }else if(catornum[input$categorise] == 'categorical'){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                if(input$date_format == 'Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
                }else if(input$date_format == 'Day Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Month'){
                  grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
                }else if(input$date_format == 'Year'){
                  grouped_data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
                }
                
              }
              grouped_data<-grouped_data%>%mutate(grouped_val = data[[coldc_type[input$categorise]]])
              
              if(input$a_d == 'Ascending'){
                if(input$x_axis_as_label == TRUE){
                  plt<-grouped_data%>% plot_ly(x=~reorder(x_axis,data[[coldc_type[input$y_axis]]], FUN = median),y=~data[[coldc_type[input$y_axis]]],color=~grouped_val, colors = colourss, type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour))%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else{
                  plt<-grouped_data%>% plot_ly(x=~reorder(x_axis,data[[coldc_type[input$y_axis]]], FUN = median),y=~data[[coldc_type[input$y_axis]]],color=~grouped_val, colors = colourss,text=~'', hoverinfo='text', type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour))%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }
              }else{
                if(input$x_axis_as_label == TRUE){
                  plt<-grouped_data%>% plot_ly(x=~reorder(x_axis,-data[[coldc_type[input$y_axis]]], FUN = median),y=~data[[coldc_type[input$y_axis]]],color=~grouped_val, colors = colourss, type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour))%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else{
                  plt<-grouped_data%>% plot_ly(x=~reorder(x_axis,-data[[coldc_type[input$y_axis]]], FUN = median),y=~data[[coldc_type[input$y_axis]]],color=~grouped_val, colors = colourss,text=~'', hoverinfo='text', type = 'box', marker=list(color=input$chart_colour),line=list(color=input$chart_colour))%>%layout(barmode ='group' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }
              }
              
              
            }
          }
        }else if(input$graph_type == 'Scatter plot'){
          units<- c('Original' = 1, 'Thousands' = 1000, 'Millions' = 1000000, 'Billions' = 1000000000)
          labels<- ''
          options<- paste(names(data), '(', collumn_dc,')')
          col_type<- vector()
          col_type[options]<- names(data)
          options<- paste(names(data), '(',collumn_dc,')')
          catornum<- vector()
          catornum[options]<- collumn_dc
          coldc_type<- vector()
          coldc_type[options]<- names(data)
          colourss<- vector()
          columns<-vector()
          col_type<- vector()
          options<- paste(names(data), '(', collumn_dc, ')')
          col_type[options]<- names(data)
          for(col in names(data)){
            if(col==col_type[input$x_axis]|col==col_type[input$y_axis]){
              
            }else{
              columns[length(columns)+1]<- col
            }
          }
          if(input$categorise=='None'&length(columns)!=0&input$animate){
            
            if(class(data[[col_type[input$x_axis]]]) != 'Date'){
              data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
            }else{
              if(input$date_format == 'Month Year'){
                data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }else if(input$date_format == 'Day Month Year'){
                data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }else if(input$date_format == 'Day Month'){
                data<-data%>%mutate(x_axis = paste(day(data[[col_type[input$x_axis]]]), month(data[[col_type[input$x_axis]]], label=TRUE)))
              }else if(input$date_format == 'Month'){
                data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE)))
              }else if(input$date_format == 'Year'){
                data<-data%>%mutate(x_axis = paste(year(data[[col_type[input$x_axis]]])))
              }
              
            }
            data<-data%>%mutate(label1 = data[[col_type[input$y_axis]]])
            data<-data%>%mutate(animate = data[[input$animate_column]])
            nums<-vector()
            for(collumn in names(data)){
              if(mean(is.na(as.numeric(data[[collumn]]))==TRUE)<=0.07&class(data[[collumn]])!='Date'){
                nums[length(nums)+1]<- collumn
              }
            }
            if(class(data[[input$animate_column]])=='Date'|input$animate_column%in%nums){
              if(input$animate_column%in%nums){
                data<- data%>%arrange(as.numeric(animate))
              }
              if(class(data[[input$animate_column]])=='Date'){
                data<- data%>%arrange(animate)
              }
              data<- data%>%mutate(rank = 1:length(animate))
              
              if(class(data[[input$animate_column]])=='Date'){
                if(input$date_format_animate == 'Month Year'){
                  data<-data%>%mutate(date = paste(month(data[['animate']], label=TRUE), year(data[['animate']])))
                }else if(input$date_format_animate == 'Day Month Year'){
                  data<-data%>%mutate(date = paste(day(data[['animate']]), month(data[['animate']], label=TRUE), year(data[['animate']])))
                }else if(input$date_format_animate == 'Day Month'){
                  data<-data%>%mutate(date = paste(day(data[['animate']]), month(data[['animate']], label=TRUE)))
                }else if(input$date_format_animate == 'Month'){
                  data<-data%>%mutate(date = paste(month(data[['animate']], label=TRUE)))
                }else if(input$date_format_animate == 'Year'){
                  data<-data%>%mutate(date = paste(year(data[['animate']])))
                }
              }else if(input$animate_column%in%nums){
                data<-data%>%mutate(date = data[[input$animate_column]])
              }
              grouped_data<-data%>%group_by(date)
              vized_data<- summarise(grouped_data, rank = max(rank, na.rm = TRUE),x_axis=x_axis,y_axis=label1)
              
              
              if(input$slider_visibility==TRUE){
                plt<-vized_data%>% plot_ly(x=~x_axis,y=~y_axis, type = 'scatter',text =~'', hoverinfo= 'text',frame=~reorder(date,rank), marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title,marker=list(color=input$chart_colour))%>%animation_slider(currentvalue = list(prefix = "", font = list(color="red")))%>%animation_button(x = 4, xanchor = "right", y = 0, yanchor = "bottom")
              }else{
                plt<- vized_data%>%plot_ly(x=~x_axis, y=~y_axis, text = '', hoverinfo = 'text',type='scatter',frame=~reorder(date,rank), marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title,marker=list(color=input$chart_colour))%>%animation_slider(hide = TRUE)%>%animation_button(x = 4, xanchor = "right", y = 0, yanchor = "bottom")
                
              }
            }else{
              data<-data%>%mutate(date = data[[input$animate_column]])
              grouped_data<-data%>%group_by(date)
              vized_data<- summarise(grouped_data,x_axis=x_axis,y_axis=label1)
              
              if(input$slider_visibility==TRUE){
                plt<-vized_data%>% plot_ly(x=~x_axis,y=~y_axis, type = 'scatter',text =~'', hoverinfo= 'text',frame=~date,marker=list(color=input$chart_colour), marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>%animation_slider(currentvalue = list(prefix = "", font = list(color="red")))%>%animation_button(x = 4, xanchor = "right", y = 0, yanchor = "bottom")
              }else{
                plt<- vized_data%>%plot_ly(x=~x_axis, y=~y_axis, text = '', hoverinfo = 'text',type='scatter',frame=~date,marker=list(color=input$chart_colour), marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)%>%animation_slider(hide = TRUE)%>%animation_button(x = 4, xanchor = "right", y = 0, yanchor = "bottom")
                
              }
            }
          }else{
            if(input$categorise != 'None'){
              for(i in 1:length(unique(data[[coldc_type[input$categorise]]]))){
                ran_col<-randomColor()
                while(ran_col %in% colourss){
                  ran_col<- randomColor()
                }
                colourss[i]<- randomColor()
              }
            }
            if (input$categorise == 'None'){
              if(input$size =='None'){
                if(input$x_axis_as_label == TRUE){
                  plt<- data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~paste(input$x_succeeding), hoverinfo= 'text', marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else{
                  plt<- data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~'', hoverinfo= 'text', marker=list(color=input$chart_colour))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }
              }else{
                
                data<- data%>%mutate(size= data[[input$size]])
                if(input$x_axis_as_label == TRUE){
                  plt<- data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~paste(input$x_succeeding), hoverinfo= 'text', marker=list(color=input$chart_colour,size= size))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else{
                  plt<- data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~'', hoverinfo= 'text', marker=list(color=input$chart_colour,size=~size))%>%layout(xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }
              }
              
            }else if(catornum[input$categorise] == 'Date'){
              if(input$date_format_cat == 'Month Year'){
                grouped_data<-data%>%mutate(grouped_val = paste(month(data[[col_type[input$categorise]]], label=TRUE), year(data[[col_type[input$categorise]]])))
              }else if(input$date_format_cat == 'Day Month Year'){
                grouped_data<-data%>%mutate(grouped_val = paste(day(data[[col_type[input$categorise]]]), month(data[[col_type[input$categorise]]], label=TRUE), year(data[[col_type[input$categorise]]])))
              }else if(input$date_format_cat == 'Day Month'){
                grouped_data<-data%>%mutate(grouped_val = paste(day(data[[col_type[input$categorise]]]), month(data[[col_type[input$categorise]]], label=TRUE)))
              }else if(input$date_format_cat == 'Month'){
                grouped_data<-data%>%mutate(grouped_val = paste(month(data[[col_type[input$categorise]]], label=TRUE)))
              }else if(input$date_format_cat == 'Year'){
                grouped_data<-data%>%mutate(grouped_val = paste(year(data[[col_type[input$categorise]]])))
              }
              if(input$size =='None'){
                if(input$x_axis_as_label == TRUE & input$categorise_as_label == TRUE){
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~paste(input$x_succeeding,'\n', '\n',input$cat_preceding, grouped_val, input$cat_succeeding), hoverinfo= 'text+y', color=~grouped_val, colors=~colourss)%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else if(input$x_axis_as_label == TRUE ){
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~paste(input$x_succeeding), hoverinfo= 'text+y', color=~grouped_val, colors=~colourss)%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else if(input$categorise_as_label == TRUE ){
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~paste(input$cat_preceding, grouped_val, input$cat_succeeding), hoverinfo= 'text', color=~grouped_val, colors=~colourss)%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else{
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~'', hoverinfo= 'text', color=~grouped_val, colors=~colourss)%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }
              }else{
                if(input$x_axis_as_label == TRUE & input$categorise_as_label == TRUE){
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~paste(input$x_succeeding,'\n', '\n',input$cat_preceding, grouped_val, input$cat_succeeding), hoverinfo= 'text+y', color=~grouped_val, colors=~colourss,marker=list(size= size))%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else if(input$x_axis_as_label == TRUE ){
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~paste(input$x_succeeding), hoverinfo= 'text+y', color=~grouped_val, colors=~colourss,marker=list(size= size))%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else if(input$categorise_as_label == TRUE ){
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~paste(input$cat_preceding, grouped_val, input$cat_succeeding), hoverinfo= 'text', color=~grouped_val, colors=~colourss,marker=list(size= size))%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else{
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~'', hoverinfo= 'text', color=~grouped_val, colors=~colourss,marker=list(size= size))%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }
              }
              
            }else if(catornum[input$categorise] == 'categorical'){
              
              grouped_data<-data%>%mutate(grouped_val = data[[coldc_type[input$categorise]]])
              if(input$size =='None'){
                if(input$x_axis_as_label == TRUE & input$categorise_as_label == TRUE){
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~paste(input$x_succeeding,'\n', '\n',input$cat_preceding, grouped_val, input$cat_succeeding), hoverinfo= 'text+y', color=~grouped_val, colors=~colourss)%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else if(input$x_axis_as_label == TRUE ){
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~paste(input$x_succeeding), hoverinfo= 'text+y', color=~grouped_val, colors=~colourss)%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else if(input$categorise_as_label == TRUE ){
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~paste(input$cat_preceding, grouped_val, input$cat_succeeding), hoverinfo= 'text', color=~grouped_val, colors=~colourss)%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else{
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~'', hoverinfo= 'text', color=~grouped_val, colors=~colourss)%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }
              }else{
                if(input$x_axis_as_label == TRUE & input$categorise_as_label == TRUE){
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~paste(input$x_succeeding,'\n', '\n',input$cat_preceding, grouped_val, input$cat_succeeding), hoverinfo= 'text+y', color=~grouped_val, colors=~colourss,marker=list(size= size))%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else if(input$x_axis_as_label == TRUE ){
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~paste(input$x_succeeding), hoverinfo= 'text+y', color=~grouped_val, colors=~colourss,marker=list(size= size))%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else if(input$categorise_as_label == TRUE ){
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~paste(input$cat_preceding, grouped_val, input$cat_succeeding), hoverinfo= 'text', color=~grouped_val, colors=~colourss,marker=list(size= size))%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else{
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~'', hoverinfo= 'text', color=~grouped_val, colors=~colourss,marker=list(size= size))%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }
              }
              
              
            }else if(catornum[input$categorise] == 'numeric'){
              
              grouped_data<-data%>%mutate(grouped_val = data[[coldc_type[input$categorise]]])
              
              if(input$size =='None'){
                if(input$x_axis_as_label == TRUE){
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~paste(input$x_succeeding,'\n', '\n',input$cat_preceding, grouped_val, input$cat_succeeding), hoverinfo= 'text+y', color=~grouped_val, colors=~input$colour_scale)%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else if(input$x_axis_as_label == TRUE ){
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~paste(input$x_succeeding), hoverinfo= 'text+y', color=~grouped_val, colors=~input$colour_scale)%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else{
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~'', hoverinfo= 'text', color=~grouped_val, colors=~input$colour_scale)%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }
              }else{
                if(input$x_axis_as_label == TRUE){
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~paste(input$x_succeeding,'\n', '\n',input$cat_preceding, grouped_val, input$cat_succeeding), hoverinfo= 'text+y', color=~grouped_val, colors=~input$colour_scale,marker=list(size= size))%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else if(input$x_axis_as_label == TRUE ){
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~paste(input$x_succeeding), hoverinfo= 'text+y', color=~grouped_val, colors=~input$colour_scale,marker=list(size= size))%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }else{
                  plt<-grouped_data%>% plot_ly(x=~data[[coldc_type[input$x_axis]]],y=~data[[coldc_type[input$y_axis]]], type = 'scatter',mode='markers',text =~'', hoverinfo= 'text', color=~grouped_val,  colors=~input$colour_scale,marker=list(size= size))%>%layout(barmode ='stack' ,xaxis = list(title = input$xlab ,showgrid = input$gridline), yaxis = list(title = input$ylab,showgrid = input$gridline),title = input$title)
                }
              }
            }
          }
        }else if(input$graph_type == 'World Map'){
          columns<-vector()
          col_type<- vector()
          options<- paste(names(data), '(', collumn_dc, ')')
          col_type[options]<- names(data)
          for(col in names(data)){
            if(col==col_type[input$x_axis]|col==col_type[input$y_axis]){
              
            }else{
              columns[length(columns)+1]<- col
            }
          }
          
          if(length(columns)!=0&input$animate){
            data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
            data<-data%>%mutate(label1 = data[[col_type[input$y_axis]]])
            data<-data%>%mutate(animate = data[[input$animate_column]])
            nums<-vector()
            for(collumn in names(data)){
              if(mean(is.na(as.numeric(data[[collumn]]))==TRUE)<=0.07&class(data[[collumn]])!='Date'){
                nums[length(nums)+1]<- collumn
              }
            }
            if(class(data[[input$animate_column]])=='Date'|input$animate_column%in%nums){
              if(input$animate_column%in%nums){
                data<- data%>%arrange(as.numeric(animate))
              }
              if(class(data[[input$animate_column]])=='Date'){
                data<- data%>%arrange(animate)
              }
              data<- data%>%mutate(rank = 1:length(animate))
              
              if(class(data[[input$animate_column]])=='Date'){
                if(input$date_format_animate == 'Month Year'){
                  data<-data%>%mutate(date = paste(month(data[['animate']], label=TRUE), year(data[['animate']])))
                }else if(input$date_format_animate == 'Day Month Year'){
                  data<-data%>%mutate(date = paste(day(data[['animate']]), month(data[['animate']], label=TRUE), year(data[['animate']])))
                }else if(input$date_format_animate == 'Day Month'){
                  data<-data%>%mutate(date = paste(day(data[['animate']]), month(data[['animate']], label=TRUE)))
                }else if(input$date_format_animate == 'Month'){
                  data<-data%>%mutate(date = paste(month(data[['animate']], label=TRUE)))
                }else if(input$date_format_animate == 'Year'){
                  data<-data%>%mutate(date = paste(year(data[['animate']])))
                }
              }else if(input$animate_column%in%nums){
                data<-data%>%mutate(date = data[[input$animate_column]])
              }
              grouped_data<-data%>%group_by(x_axis,date)
              if(input$aggregate == 'Sum'){
                vized_data<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE), rank = max(rank, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                vized_data<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE), rank = max(rank, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                vized_data<- summarise(grouped_data, sum = length(label1), rank = max(rank, na.rm = TRUE))
              }else{
                vized_data<- summarise(grouped_data, sum = median(label1, na.rm = TRUE), rank= max(rank, na.rm = TRUE))
              }
              plot_geo<- read.csv("country_code.csv")
              real_data<- merge(vized_data, plot_geo, by.x='x_axis', by.y='Country')
              if(input$slider_visibility==TRUE){
                plt<- plot_geo(real_data)%>%add_trace(locations=~Code, z=~sum, color=~sum, text =~'', hoverinfo='text',colors=input$colour_scale,frame=~reorder(date,rank))%>%layout(geo=list(scope = input$scope),title = input$title) %>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(currentvalue = list(prefix = "", font = list(color="red")))%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
              }else{
                plt<- plot_geo(real_data)%>%add_trace(locations=~Code, z=~sum, color=~sum, text =~'', hoverinfo='text',colors=input$colour_scale,frame=~reorder(date,rank))%>%layout(geo=list(scope = input$scope),title = input$title) %>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(hide = TRUE)%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
              }
            }else{
              data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              data<-data%>%mutate(label1 = data[[col_type[input$y_axis]]])
              data<-data%>%mutate(date = data[[input$animate_column]])
              grouped_data<-data%>%group_by(x_axis,date)
              if(input$aggregate == 'Sum'){
                vized_data<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                vized_data<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                vized_data<- summarise(grouped_data, sum = length(label1))
              }else{
                vized_data<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              plot_geo<- read.csv("country_code.csv")
              real_data<- merge(vized_data, plot_geo, by.x='x_axis', by.y='Country')
              if(input$slider_visibility==TRUE){
                plt<- plot_geo(real_data)%>%add_trace(locations=~Code, z=~sum, color=~sum, text =~'', hoverinfo='text',colors=input$colour_scale,frame=~date)%>%layout(geo=list(scope = input$scope),title = input$title) %>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(currentvalue = list(prefix = "", font = list(color="red")))%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
              }else{
                plt<- plot_geo(real_data)%>%add_trace(locations=~Code, z=~sum, color=~sum, text =~'', hoverinfo='text',colors=input$colour_scale,frame=~date)%>%layout(geo=list(scope = input$scope),title = input$title) %>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(hide = TRUE)%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
              }
            }
          }else{
            
            data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
            data<-data%>%mutate(label1 = data[[col_type[input$y_axis]]])
            grouped_data<-data%>%group_by(x_axis)
            if(input$aggregate == 'Sum'){
              vized_data<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
            }else if(input$aggregate == 'Average'){
              vized_data<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
            }else if(input$aggregate == 'Count'){
              vized_data<- summarise(grouped_data, sum = length(label1))
            }else{
              vized_data<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
            }
            if(input$x_axis_as_label == TRUE){
              labelx<- paste(input$x_preceding, vized_data$x_axis, input$x_succeeding)
            }
            
            if(no_of_clicks == 1){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l1<- summarise(grouped_data, sum = length(label1))
              }else{
                l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              
              print(class(l1$sum))
              labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2) ,input$succeeding1)
            }else if(no_of_clicks == 2){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l1<- summarise(grouped_data, sum = length(label1))
              }else{
                l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              
              if(input$aggregate == 'Sum'){
                l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l2<- summarise(grouped_data, sum = length(label2))
              }else{
                l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
              }
              labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2) ,input$succeeding1, '\n',
                             input$preceding2,round(l2$sum/as.numeric(input$unit2),2) ,input$succeeding2)
              
            }else if(no_of_clicks == 3){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l1<- summarise(grouped_data, sum = length(label1))
              }else{
                l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l2<- summarise(grouped_data, sum = length(label2))
              }else{
                l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label3 = data[[input$column3]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l3<- summarise(grouped_data, sum = sum(label3, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l3<- summarise(grouped_data, sum = mean(label3, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l3<- summarise(grouped_data, sum = length(label3))
              }else{
                l3<- summarise(grouped_data, sum = median(label3, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2) ,input$succeeding1, '\n',
                             input$preceding2,round(l2$sum/as.numeric(input$unit2),2),input$succeeding2,'\n',
                             input$preceding3,round(l3$sum/as.numeric(input$unit3),2) ,input$succeeding3)
              
            } else if(no_of_clicks ==4){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l1<- summarise(grouped_data, sum = length(label1))
              }else{
                l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              
              if(input$aggregate == 'Sum'){
                l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l2<- summarise(grouped_data, sum = length(label2))
              }else{
                l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label3 = data[[input$column3]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              
              if(input$aggregate == 'Sum'){
                l3<- summarise(grouped_data, sum = sum(label3, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l3<- summarise(grouped_data, sum = mean(label3, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l3<- summarise(grouped_data, sum = length(label3))
              }else{
                l3<- summarise(grouped_data, sum = median(label3, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label4 = data[[input$column4]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              
              if(input$aggregate == 'Sum'){
                l4<- summarise(grouped_data, sum = sum(label4, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l4<- summarise(grouped_data, sum = mean(label4, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l4<- summarise(grouped_data, sum = length(label4))
              }else{
                l4<- summarise(grouped_data, sum = median(label4, na.rm = TRUE))
              }
              labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2),input$succeeding1, '\n',
                             input$preceding2,round(l2$sum/as.numeric(input$unit2),2) ,input$succeeding2,'\n',
                             input$preceding3,round(l3$sum/as.numeric(input$unit3),2) ,input$succeeding3,'\n',
                             input$preceding4,round(l4$sum/as.numeric(input$unit1),4) ,input$succeeding4)
            }
            plot_geo<- read.csv("country_code.csv")
            vized_data<- vized_data%>%mutate(labels=paste(labels,'\n', labelx))
            real_data<- merge(vized_data, plot_geo, by.x='x_axis', by.y='Country')
            View(real_data)
            plt<- plot_geo(real_data)%>%add_trace(locations=~Code, z=~sum, color=~sum, text =~labels, hoverinfo='text',colors=input$colour_scale)%>%layout(geo=list(scope = input$scope),title = input$title)
          }
        }else if(input$graph_type == 'USA Map'){
          columns<-vector()
          col_type<- vector()
          options<- paste(names(data), '(', collumn_dc, ')')
          col_type[options]<- names(data)
          for(col in names(data)){
            if(col==col_type[input$x_axis]|col==col_type[input$y_axis]){
              
            }else{
              columns[length(columns)+1]<- col
            }
          }
          
          if(length(columns)!=0&input$animate){
            data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
            data<-data%>%mutate(label1 = data[[col_type[input$y_axis]]])
            data<-data%>%mutate(animate = data[[input$animate_column]])
            nums<-vector()
            for(collumn in names(data)){
              if(mean(is.na(as.numeric(data[[collumn]]))==TRUE)<=0.07&class(data[[collumn]])!='Date'){
                nums[length(nums)+1]<- collumn
              }
            }
            if(class(data[[input$animate_column]])=='Date'|input$animate_column%in%nums){
              if(input$animate_column%in%nums){
                data<- data%>%arrange(as.numeric(animate))
              }
              if(class(data[[input$animate_column]])=='Date'){
                data<- data%>%arrange(animate)
              }
              data<- data%>%mutate(rank = 1:length(animate))
              
              if(class(data[[input$animate_column]])=='Date'){
                if(input$date_format_animate == 'Month Year'){
                  data<-data%>%mutate(date = paste(month(data[['animate']], label=TRUE), year(data[['animate']])))
                }else if(input$date_format_animate == 'Day Month Year'){
                  data<-data%>%mutate(date = paste(day(data[['animate']]), month(data[['animate']], label=TRUE), year(data[['animate']])))
                }else if(input$date_format_animate == 'Day Month'){
                  data<-data%>%mutate(date = paste(day(data[['animate']]), month(data[['animate']], label=TRUE)))
                }else if(input$date_format_animate == 'Month'){
                  data<-data%>%mutate(date = paste(month(data[['animate']], label=TRUE)))
                }else if(input$date_format_animate == 'Year'){
                  data<-data%>%mutate(date = paste(year(data[['animate']])))
                }
              }else if(input$animate_column%in%nums){
                data<-data%>%mutate(date = data[[input$animate_column]])
              }
              grouped_data<-data%>%group_by(x_axis,date)
              if(input$aggregate == 'Sum'){
                vized_data<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE), rank = max(rank, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                vized_data<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE), rank = max(rank, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                vized_data<- summarise(grouped_data, sum = length(label1), rank = max(rank, na.rm = TRUE))
              }else{
                vized_data<- summarise(grouped_data, sum = median(label1, na.rm = TRUE), rank= max(rank, na.rm = TRUE))
              }
              plot_geo<- read_excel("usa_code.xlsx")
              vized_data<- vized_data%>%mutate(labels=paste(labels,'\n', labelx))
              real_data<- merge(vized_data, plot_geo, by.x='x_axis', by.y='State')
              
              if(input$slider_visibility==TRUE){
                plt<- plot_geo(real_data,locationmode = 'USA-states')%>%add_trace(locations=~Code, z=~sum, color=~sum, text =~'', hoverinfo='text',colors=input$colour_scale,frame=~reorder(date,rank))%>%layout(geo=list(scope = 'usa'),title = input$title)%>%animation_slider(currentvalue = list(prefix = "", font = list(color="red")))%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
              }else{
                plt<- plot_geo(real_data,locationmode = 'USA-states')%>%add_trace(locations=~Code, z=~sum, color=~sum, text =~'', hoverinfo='text',colors=input$colour_scale,frame=~reorder(date,rank))%>%layout(geo=list(scope = 'usa'),title = input$title)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(hide = TRUE)%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
              }
            }else{
              data<-data%>%mutate(date = data[[input$animate_column]])
              grouped_data<-data%>%group_by(x_axis,date)
              if(input$aggregate == 'Sum'){
                vized_data<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                vized_data<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                vized_data<- summarise(grouped_data, sum = length(label1))
              }else{
                vized_data<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              plot_geo<- read_excel("usa_code.xlsx")
              vized_data<- vized_data%>%mutate(labels=paste(labels,'\n', labelx))
              real_data<- merge(vized_data, plot_geo, by.x='x_axis', by.y='State')
              
              if(input$slider_visibility==TRUE){
                plt<- plot_geo(real_data,locationmode = 'USA-states')%>%add_trace(locations=~Code, z=~sum, color=~sum, text =~labels, hoverinfo='text',colors=input$colour_scale,frame=~date)%>%layout(geo=list(scope = 'usa'),title = input$title)%>%animation_slider(currentvalue = list(prefix = "", font = list(color="red")))%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
              }else{
                plt<- plot_geo(real_data,locationmode = 'USA-states')%>%add_trace(locations=~Code, z=~sum, color=~sum, text =~labels, hoverinfo='text',colors=input$colour_scale,frame=~date)%>%layout(geo=list(scope = 'usa'),title = input$title)%>% animation_opts(frame = input$transition_time, transition = 100, redraw = TRUE,mode='afterall')%>%animation_slider(hide = TRUE)%>%animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")
              }
            }
          }else{
            aggregation<- c('Sum', 'Average', 'Count', 'Median')
            if(class(data[[col_type[input$x_axis]]]) != 'Date'){
              grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
            }else{
              grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
            }
            grouped_data<-grouped_data%>%mutate(label1 = data[[col_type[input$y_axis]]])
            grouped_data<-grouped_data%>%group_by(x_axis)
            if(input$aggregate == 'Sum'){
              vized_data<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
            }else if(input$aggregate == 'Average'){
              vized_data<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
            }else if(input$aggregate == 'Count'){
              vized_data<- summarise(grouped_data, sum = length(label1))
            }else{
              vized_data<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
            }
            
            if(input$x_axis_as_label == TRUE){
              labelx<- paste(input$x_preceding, vized_data$x_axis, input$x_succeeding)
            }
            
            if(no_of_clicks == 1){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l1<- summarise(grouped_data, sum = length(label1))
              }else{
                l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              
              print(class(l1$sum))
              labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2) ,input$succeeding1)
            }else if(no_of_clicks == 2){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l1<- summarise(grouped_data, sum = length(label1))
              }else{
                l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              
              if(input$aggregate == 'Sum'){
                l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l2<- summarise(grouped_data, sum = length(label2))
              }else{
                l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
              }
              labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2) ,input$succeeding1, '\n',
                             input$preceding2,round(l2$sum/as.numeric(input$unit2),2) ,input$succeeding2)
              
            }else if(no_of_clicks == 3){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l1<- summarise(grouped_data, sum = length(label1))
              }else{
                l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l2<- summarise(grouped_data, sum = length(label2))
              }else{
                l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label3 = data[[input$column3]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l3<- summarise(grouped_data, sum = sum(label3, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l3<- summarise(grouped_data, sum = mean(label3, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l3<- summarise(grouped_data, sum = length(label3))
              }else{
                l3<- summarise(grouped_data, sum = median(label3, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2) ,input$succeeding1, '\n',
                             input$preceding2,round(l2$sum/as.numeric(input$unit2),2),input$succeeding2,'\n',
                             input$preceding3,round(l3$sum/as.numeric(input$unit3),2) ,input$succeeding3)
              
            } else if(no_of_clicks ==4){
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label1 = data[[input$column1]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              if(input$aggregate == 'Sum'){
                l1<- summarise(grouped_data, sum = sum(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l1<- summarise(grouped_data, sum = mean(label1, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l1<- summarise(grouped_data, sum = length(label1))
              }else{
                l1<- summarise(grouped_data, sum = median(label1, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label2 = data[[input$column2]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              
              if(input$aggregate == 'Sum'){
                l2<- summarise(grouped_data, sum = sum(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l2<- summarise(grouped_data, sum = mean(label2, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l2<- summarise(grouped_data, sum = length(label2))
              }else{
                l2<- summarise(grouped_data, sum = median(label2, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label3 = data[[input$column3]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              
              if(input$aggregate == 'Sum'){
                l3<- summarise(grouped_data, sum = sum(label3, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l3<- summarise(grouped_data, sum = mean(label3, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l3<- summarise(grouped_data, sum = length(label3))
              }else{
                l3<- summarise(grouped_data, sum = median(label3, na.rm = TRUE))
              }
              if(class(data[[col_type[input$x_axis]]]) != 'Date'){
                grouped_data<-data%>%mutate(x_axis = data[[col_type[input$x_axis]]])
              }else{
                grouped_data<-data%>%mutate(x_axis = paste(month(data[[col_type[input$x_axis]]], label=TRUE), year(data[[col_type[input$x_axis]]])))
              }
              grouped_data<-grouped_data%>%mutate(label4 = data[[input$column4]])
              grouped_data<-grouped_data%>%group_by(x_axis)
              
              if(input$aggregate == 'Sum'){
                l4<- summarise(grouped_data, sum = sum(label4, na.rm = TRUE))
              }else if(input$aggregate == 'Average'){
                l4<- summarise(grouped_data, sum = mean(label4, na.rm = TRUE))
              }else if(input$aggregate == 'Count'){
                l4<- summarise(grouped_data, sum = length(label4))
              }else{
                l4<- summarise(grouped_data, sum = median(label4, na.rm = TRUE))
              }
              labels<- paste(input$preceding1,round(l1$sum/as.numeric(input$unit1),2),input$succeeding1, '\n',
                             input$preceding2,round(l2$sum/as.numeric(input$unit2),2) ,input$succeeding2,'\n',
                             input$preceding3,round(l3$sum/as.numeric(input$unit3),2) ,input$succeeding3,'\n',
                             input$preceding4,round(l4$sum/as.numeric(input$unit1),4) ,input$succeeding4)
            }
            plot_geo<- read_excel("usa_code.xlsx")
            vized_data<- vized_data%>%mutate(labels=paste(labels,'\n', labelx))
            real_data<- merge(vized_data, plot_geo, by.x='x_axis', by.y='State')
            plt<- plot_geo(real_data,locationmode = 'USA-states')%>%add_trace(locations=~Code, z=~sum, color=~sum, text =~labels, hoverinfo='text',colors=input$colour_scale)%>%layout(geo=list(scope = 'usa'),title = input$title)
          }
        }
        output$plotput<- renderPlotly({
          plt%>%config(modeBarButtons=list(list('toImage')),displaylogo = FALSE,toImageButtonOptions=list(format=input$export,icon=icon('file-image'),click='download'))
          
        })
        
        output$download<-downloadHandler(
          filename = function() {
            paste('datum_html', 'html',sep='.')
          },
          content = function(file) {
            htmlwidgets::saveWidget(as_widget(plt%>%config(displayModeBar = FALSE)), file)
          }
        )
        
      })
    }
    
  })
}

shinyApp(ui,server)