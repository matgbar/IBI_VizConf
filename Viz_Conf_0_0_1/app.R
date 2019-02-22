if(!require('pacman')) install.packages('pacman')
pacman::p_unload(pacman::p_loaded(), character.only=TRUE)
pacman::p_load(shiny, 
               ggplot2, 
               shinythemes,
               shinyFiles,
               signal,
               zoo,
               forecast,
               psych,
               rtf, 
               shinyBS, 
               tseries, 
               rstan,
               rstanarm,
               bayesplot,
               MCMCvis, 
               astsa, 
               parallel,
               benchmarkme,
               doParallel,
               imputeTS)

# Define UI for application that draws a histogram
ui <- shinyUI(
  fluidPage(theme = shinytheme('united'),
            title = "IBI Visual Conference v0.0.1", 
            titlePanel("IBI Visual Conference v0.0.1"), 
            tabsetPanel(
              tabPanel("File Selection", 
                       sidebarLayout(
                         sidebarPanel(shinyDirButton(id='dir',
                                                     label='Select Directory',
                                                     title = 'Selecting a Working Directory',
                                                     style = "background-color: #E03A3E; border-color: #FFD520"
                                                     ), 
                                      tags$br(),
                                      tags$br(),
                                      ##
                                      shinyFilesButton(id='origFile', 
                                                       label = 'Select Original File', 
                                                       multiple = F, 
                                                       title = 'Choosing the Original File', 
                                                       style = "background-color: #E03A3E; border-color: #FFD520"
                                                       ),
                                      tags$br(),
                                      tags$br(),
                                      ##
                                      shinyFilesButton(id='ppgFile', 
                                                       label = 'Select PPG File', 
                                                       multiple = F, 
                                                       title = 'Choosing PPG file', 
                                                       style = "background-color: #E03A3E; border-color: #FFD520"
                                      ),
                                      tags$br(),
                                      tags$br(), 
                                      shinyFilesButton(id='ibiFiles', 
                                                       label = 'Select IBI Files', 
                                                       multiple = T, 
                                                       title = 'Choosing Edited IBI files', 
                                                       style = "background-color: #E03A3E; border-color: #FFD520"
                                      ),
                                      tags$br(),
                                      tags$br(),
                                      actionButton(inputId = "start", 
                                                   label = 'Process Selections', 
                                                   style = "background-color: #48f442; border-color: #FFD520")
                         ), 
                         mainPanel(tableOutput('viewFiles'))
                       )
              ),
              tabPanel("Edited IBIs",
                       sidebarLayout(
                         sidebarPanel(actionButton(inputId = "refresh", 
                                                   label = 'Update Plot', 
                                                   style = "background-color: #48f442; border-color: #FFD520"), 
                                      sliderInput(inputId = 'yLim1',
                                                  label = 'Select Target IBI range',
                                                  min = .25, 
                                                  max = 1.5,
                                                  value = c(.25, .75),
                                                  post = 'IBI'),
                                      checkboxGroupInput('editorSelect1', 
                                                         label = 'Select Editors to Display')
                                      ),
                         mainPanel(plotOutput(outputId = "multiIBI",
                                              height = '600px'), 
                                   plotOutput(outputId = 'IBIselect', 
                                              height = '150px', 
                                              brush = brushOpts(id="select_range1", delay=1000))
                                   )
                       )
              ),
              tabPanel("Filtered & Processed IBI",
                       sidebarLayout(
                         sidebarPanel(), 
                         mainPanel()
                       )
              )
            )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #browser()
  rv<-reactiveValues(
    DF.multi = NULL
  )
  options(shiny.maxRequestSize=150*1024^2)
  #browser()
  if(Sys.getenv('USERPROFILE')=="")
    user.folder<-"~"
  else if(Sys.getenv("HOMEPATH")!="")
    user.folder<-Sys.getenv('USERPROFILE')
  else
    user.folder<-'C:/'
  
  #############################################################################
  #File Entry and Working Directory Specification
  #############################################################################
  
  #This should work in order - important that the user starts here then...
  shinyDirChoose(input, 'dir', roots=c(User=user.folder))
  shinyFileChoose(input, 'ppgFile', roots=c(User=user.folder))
  shinyFileChoose(input, 'origFile', roots=c(User=user.folder))
  shinyFileChoose(input, 'ibiFiles', roots=c(User=user.folder))
  
  observeEvent(input$dir, {
    #browser()
    rv$wd<-parseDirPath(roots=c(User=user.folder), input$dir)
    rv$wd<-as.character(rv$wd)
    if(length(rv$wd)==1){
      shinyFileChoose(input, 'ppgFile', roots=c(wd=rv$wd, User=user.folder))
      shinyFileChoose(input, 'origFile', roots=c(wd=rv$wd, User=user.folder))
      shinyFileChoose(input, 'ibiFiles', roots=c(wd=rv$wd, User=user.folder))
    }
  })
  
  observeEvent(input$origFile, {
    if(!is.null(input$origFile)){
      file_selected<-parseFilePaths(roots=c(wd=rv$wd, User=user.folder), input$origFile)
      rv$origFile<-as.character(file_selected$datapath) 
    }
  })
  
  observeEvent(input$ppgFile, {
    if(!is.null(input$ppgFile)){
      file_selected<-parseFilePaths(roots=c(wd=rv$wd, User=user.folder), input$ppgFile)
      rv$ppgFile<-as.character(file_selected$datapath) 
    }
  })
  
  observeEvent(input$ibiFiles, {
    if(!is.null(input$ibiFiles)){
      #browser()
      file_selected<-parseFilePaths(roots=c(wd=rv$wd, User=user.folder), input$ibiFiles)
      rv$ibiFiles<-as.character(file_selected$datapath) 
    }
  })
  
  viewFiles<-eventReactive(input$start, {
    #browser()
    if(length(rv$wd)>0 & 
       length(rv$origFile)>0 & 
       length(rv$ppgFile)>0 & 
       length(rv$ibiFiles) > 0){
      Paths<-rv$ibiFiles
      temp_str<-strsplit(Paths, split = '/')
      
      Files<-vector()
      for(i in 1:length(Paths)){
        Files[i]<-temp_str[[i]][length(temp_str[[i]])]
      }
      
      temp_str<-strsplit(Files, split = '_')
      
      Editors<-vector()
      for(i in 1:length(Files)){
        Editors[i]<-temp_str[[i]][4]
      }
  
      RMSSD<-vector()
      HP<-vector()
      IBIs<-vector()
      File.list<-list()
      for(i in 1:length(Files)){
        dat.temp<-read.table(Paths[i], header = TRUE, sep ='\t')
        File.list[[i]]<-dat.temp
        RMSSD[i]<-round(psych::rmssd(dat.temp$IBI[dat.temp$Vals!="Uneditable"]), digits = 4)
        HP[i]<-round(mean(dat.temp$IBI[dat.temp$Vals!="Uneditable"]), digits = 4)
        IBIs[i]<-nrow(dat.temp[dat.temp$Vals!="Uneditable",])
      }
      updateCheckboxGroupInput(session, 
                               inputId = "editorSelect1", 
                               selected = Editors, 
                               choices = Editors)
      tab<-data.frame(Editors, 
                      RMSSD, 
                      HP, 
                      IBIs, 
                      Files)
    }
    else{
      tab<-matrix('No data provided', nrow=1)
    }
    rv$tab<-tab
    tab
  })
  
  Editors<-reactive({input$editorSelect1})
  
  output$viewFiles<-renderTable({
    viewFiles()
  })
  
  observeEvent(input$editorSelect1,{
    rv$editorSelect1<-input$editorSelect1
  })
  
  multiIBI<-eventReactive(input$refresh,{
    #browser()
    g1<-ggplot()
    rv$ymin1<-as.numeric(input$yLim1[1])
    rv$ymax1<-as.numeric(input$yLim1[2])
    if(length(rv$wd)>0 & 
       length(rv$origFile)>0 & 
       length(rv$ppgFile)>0 & 
       length(rv$ibiFiles) > 0 & 
       nrow(rv$tab)>0){
      DF.multi<-data.frame()
      for(i in 1:nrow(rv$tab)){
        dat.temp<-read.table(paste0(rv$wd, '/', rv$tab$Files[i]), 
                             header = TRUE, 
                             stringsAsFactors = FALSE)
        dat.temp$Editor<-rep(rv$tab$Editors[i])
        DF.multi<-rbind(DF.multi, dat.temp)
      }
      
      eds<-Editors()

      DF.multi.temp<-data.frame()
      for(i in 1:length(eds)){
        DF.multi.temp<-rbind(DF.multi.temp, DF.multi[DF.multi$Editor == eds[i],])
      }
      
      g1<-g1+
        geom_line(data = DF.multi.temp, 
                       aes(x = Time, 
                           y = IBI, 
                           group = Editor, 
                           lty = Editor), 
                  size = 2)+
        geom_point(data = DF.multi.temp, 
                   aes(x = Time, 
                       y = IBI, 
                       color = Vals, 
                       group = Editor), 
                   size = 4)+
        scale_y_continuous(limits = c(rv$ymin1, rv$ymax1))
      if(!is.null(input$select_range1)){
        g1<-g1+scale_x_continuous(limits = c(input$select_range1$xmin, input$select_range1$xmax))
      } 
    }
    g1
  })
  
  output$multiIBI<-renderPlot({
    multiIBI()
  })
  
  output$IBIselect<-renderPlot({
    #browser()
    rv$origDF<-read.table(rv$origFile, header = TRUE, sep = '\t')
    DF<-rv$origDF
    g1<-ggplot()+
      geom_line(data = DF, 
                aes(x = Time, 
                    y = IBI))+
      geom_point(color = 'red')

    g1
  })
  #############################################################################
  #Text display for File Name and Working Directory
  #############################################################################
}

# Run the application 
shinyApp(ui = ui, server = server)

