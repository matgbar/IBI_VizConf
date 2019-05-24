#===================================================================================================
# This is the Shiny application IBI Editor Conferencing Interface - Matthew G. Barstead (c) 2019. 
# You can run the application by clicking the 'Run App' button above.
#===================================================================================================
# By running this application you agree to the terms outlined below:
# 
# MIT License
#
# Copyright (c) 2019 Matthew G. Barstead
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#===================================================================================================
# Details about the processing steps are detailed at the link below: 
# https://github.com/matgbar/IBI_VizConf
#
# General questions? Contact the developer Matthew G. Barstead 
# Contact: mbarstead@gmail.com
#===================================================================================================

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
               shinyBS, 
               tseries, 
               astsa, 
               parallel,
               benchmarkme,
               doParallel,
               imputeTS, 
               seewave, 
               psd, 
               cowplot, 
               gridExtra)

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
                                                  value = c(.25, 1.25),
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
              tabPanel("Filtered & Spectral IBI",
                       sidebarLayout(
                         sidebarPanel(actionButton(inputId = "refresh2", 
                                                   label = 'Update Plot', 
                                                   style = "background-color: #48f442; border-color: #FFD520"), 
                                      sliderInput(inputId = 'yLim2',
                                                  label = 'Adjust y-axis',
                                                  min = -.25, 
                                                  max = .25,
                                                  value = c(-1, 1),
                                                  post = 'IBI'),
                                      checkboxGroupInput('editorSelect2', 
                                                         label = 'Select Editors to Display'),
                                      radioButtons(inputId = 'plotSelect',
                                                   label = 'Choose Information to Display', 
                                                   choices = c('BW Bandpass', 
                                                               'PSD & ln(HF-HRV)')), 
                                      selectInput(inputId = 'pop', 
                                                  label = 'Select Age Range:', 
                                                  choices = c('Infant (0-2)', 
                                                              'Child (3-6)', 
                                                              'Adolescent (7-17)',
                                                              'Adult (18+)'), 
                                                  selected = "Child (3-6)")), 
                         mainPanel(plotOutput(outputId = "HFHRV",
                                              height = '600px'), 
                                   plotOutput(outputId = 'IBIselect2', 
                                              height = '150px', 
                                              brush = brushOpts(id="select_range2", delay=1000))
                         )
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
      updateCheckboxGroupInput(session, 
                               inputId = "editorSelect2", 
                               selected = Editors, 
                               choices = Editors)
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
                           color = Editor), 
                  size = 2, 
                  alpha = .5)+
        geom_point(data = DF.multi.temp, 
                   aes(x = Time, 
                       y = IBI, 
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
    DF_ppg<-read.table(rv$ppgFile, header = TRUE, sep = '\t')
    DF_ppg$PPG<-DF_ppg$PPG-mean(DF_ppg$PPG)+mean(DF$IBI)
    g1<-ggplot()+
      geom_line(data = DF, 
                aes(x = Time, 
                    y = IBI))+
      geom_point(data = DF, 
                 aes(x = Time, 
                     y = IBI),
                 color = 'red')+
      geom_line(data = DF_ppg, 
                aes(x=Time, 
                    y=PPG))

    g1
  })
  
  #----------------------------------------------------------------------------
  Editors2<-reactive({input$editorSelect2})
  
  observeEvent(input$editorSelect2,{
    rv$editorSelect2<-input$editorSelect2
  })
  
  HFHRV<-eventReactive(input$refresh2,{
    #browser()
    if(input$plotSelect=='BW Bandpass'){
      g1<-ggplot()
      rv$ymin2<-as.numeric(input$yLim2[1])
      rv$ymax2<-as.numeric(input$yLim2[2])
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
        
        eds<-Editors2()
        
        DF.multi.temp<-data.frame()
        for(i in 1:length(eds)){
          DF.multi.temp<-rbind(DF.multi.temp, DF.multi[DF.multi$Editor == eds[i],])
        }
        
        if(input$pop == 'Infant (0-2)'){
          Hz_lb<-.3
          Hz_ub<-1.3
        }
        else if(input$pop == 'Child (3-6)'){
          Hz_lb<-.24
          Hz_ub<-1.04
        }
        else if(input$pop == 'Adolescent (7-17)'){
          Hz_lb<-.12
          Hz_ub<-1
        }
        else{
          Hz_lb<-.12
          Hz_ub<-.4
        }
        
        DF.filter<-data.frame()
        for(i in 1:length(eds)){
          dat.temp<-DF.multi.temp[DF.multi.temp$Editor == eds[i],]
          DF.time<-data.frame(Time = seq(min(dat.temp$Time), max(dat.temp$Time), by=.001))
          
          DF.time<-merge(DF.time, dat.temp, by='Time', all=TRUE)
          IBI_ts<-na.interpolation(DF.time$IBI, option='spline')
          IBI_ts<-ts(IBI_ts, frequency = 1000)
          IBI_filter<-bwfilter(IBI_ts, from = Hz_lb, to = Hz_ub, bandpass = TRUE, f=1000)
          
          DF.time<-data.frame(DF.time, 
                              IBI_filter = IBI_filter[,1])
          keep_rows<-seq(1, nrow(DF.time), by = 8)  #downsamples to 125 Hz (probably overkill)
          DF.time<-DF.time[keep_rows,]
          
          DF.time<-DF.time[,-3:-4]
          DF.time$Editor<-rep(eds[i])
          
          DF.filter<-rbind(DF.filter, DF.time)
        }
        
        g1<-g1+
          geom_line(data = DF.filter, 
                    aes(x = Time, 
                        y = IBI_filter, 
                        group = Editor, 
                        color = Editor), 
                    size = 2)+
          geom_point(data = DF.filter, 
                     aes(x = Time, 
                         y = IBI_filter, 
                         color = Editor, 
                         group = Editor), 
                     size = 4)+
          scale_y_continuous(limits = c(rv$ymin2, rv$ymax2))
        if(!is.null(input$select_range2)){
          g1<-g1+scale_x_continuous(limits = c(input$select_range2$xmin, input$select_range2$xmax))
        } 
      }
    }
    
    ##
    #Adding ability to view natural log of HF-HRV and psd by editor in respiration range
    if(input$plotSelect=='PSD & ln(HF-HRV)'){
      g1<-ggplot()
      rv$ymin2<-as.numeric(input$yLim2[1])
      rv$ymax2<-as.numeric(input$yLim2[2])
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
        
        eds<-Editors2()
        
        DF.multi.temp<-data.frame()
        for(i in 1:length(eds)){
          DF.multi.temp<-rbind(DF.multi.temp, DF.multi[DF.multi$Editor == eds[i],])
        }
        
        if(input$pop == 'Infant (0-2)'){
          Hz_lb<-.3
          Hz_ub<-1.3
        }
        else if(input$pop == 'Child (3-6)'){
          Hz_lb<-.24
          Hz_ub<-1.04
        }
        else if(input$pop == 'Adolescent (7-17)'){
          Hz_lb<-.12
          Hz_ub<-1
        }
        else{
          Hz_lb<-.12
          Hz_ub<-.4
        }
        
        DF.psd<-data.frame()
        DF.pow<-data.frame()
        for(i in 1:length(eds)){
          dat.temp<-DF.multi.temp[DF.multi.temp$Editor == eds[i],]
          DF.time<-data.frame(Time = seq(min(dat.temp$Time), max(dat.temp$Time), by=.001))
          
          DF.time<-merge(DF.time, dat.temp, by='Time', all=TRUE)
          IBI_ts<-na.interpolation(DF.time$IBI, option='spline')
          IBI_ts<-ts(IBI_ts, frequency = 1000)
          IBI_filter<-bwfilter(IBI_ts, from = Hz_lb, to = Hz_ub, bandpass = TRUE, f=1000)
          
          temp<-mvspec(x=IBI_filter,
                       spans = c(7,7),
                       taper = .01)
          
          DF.temp<-data.frame(Spec = temp$spec[temp$freq>=Hz_lb/1000 & temp$freq<=Hz_ub/1000], 
                              Freq = temp$freq[temp$freq>=Hz_lb/1000 & temp$freq<=Hz_ub/1000]*1000)
          DF.temp$Editor<-rep(eds[i])
          
          DF.temp2<-data.frame(Editor = eds[i], 
                               HF_HRV = log(sum(temp$spec[temp$freq>=Hz_lb/1000 & temp$freq<=Hz_ub/1000])))
          DF.psd<-rbind(DF.psd, DF.temp)
          DF.pow<-rbind(DF.pow, 
                        DF.temp2)
        }
        
        g1<-g1+
          geom_line(data = DF.psd, 
                    aes(x = Freq, 
                        y = Spec, 
                        group = Editor, 
                        color = Editor), 
                    size = 2)
        tbl<-tableGrob(DF.pow)
        
        g1<-plot_grid(g1, 
                      tbl, 
                      ncol =2)
      }
    }
    g1
  })
  
  output$HFHRV<-renderPlot({
    HFHRV()
  })
  
  output$IBIselect2<-renderPlot({
    #browser()
    rv$origDF<-read.table(rv$origFile, header = TRUE, sep = '\t')
    DF<-rv$origDF
    g1<-ggplot()+
      geom_line(data = DF, 
                aes(x = Time, 
                    y = IBI))+
      geom_point(data = DF, 
                 aes(x = Time, 
                     y = IBI),
                 color = 'red')
    
    g1
  })
  
  #############################################################################
  #Text display for File Name and Working Directory
  #############################################################################
}

# Run the application 
shinyApp(ui = ui, server = server)

