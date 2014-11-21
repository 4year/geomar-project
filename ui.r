library(shiny)
tabPanelAbout <- source("about.r")$value
headerPanel_2 <- function(title, h, windowTitle=title) {    
  tagList(
    tags$head(tags$title(windowTitle)),
    h(title)
  )
}

shinyUI(fluidPage(
  headerPanel_2(
    HTML(
      '<div id="stats_header" align ="center">
<font color="grey">
<body style="background-color:black;">
<h2 style="font-family:georgia;">
GEOMAR: MARINE BIOCHEMICAL MODELLING
</body>
</font>
			<a href="http://www.geomar.de/en/" target="_blank">
			<img id="stats_logo" align="right" style="margin-left: 15px;" alt="GEOMAR Logo" src="./img/tmp_geomar_logo.PNG" />
			</a>
      <a href="http://sopran.pangaea.de/" target="_blank">
  		<img id="stats_logo" align="right" alt="Marine Biochemistry Logo" src="./img/maine_biochemistry.PNG" />
			</a>
			</div>'
    ), h3, "GEOMAR: MARINE BIOCHEMICAL MODELLING"
  ),
  fluidRow(column(3,
                uiOutput("showMapPlot"),
                wellPanel(
                  h4("Data Upload"),
                  fileInput('file1', h5('Choose Your Model Data'), accept=c('text/csv','text/comma-separated-values,text/plain','.OUT')),
                  fileInput('file2', h5('Choose Your Observation Data'), accept=c('text/csv','text/comma-separated-values,text/plain','.xlsx'))    
                ),  
                wellPanel(
                  div(class="row-fluid",
                      div(class="span6", uiOutput("select"))
                  )),
                conditionalPanel(condition = "input.radio == '1' | input.radio == '2' | input.radio == '4'",
                                 wellPanel(uiOutput("check"))),
                wellPanel(
                   sliderInput("slider", label = h4("Time (Days)"), min = 0, max = 552/24, value = c(0,20),animate = FALSE, width="100%", format = "#")
                    ),
                wellPanel(
                  radioButtons("radio", label = h4("Plot Options:"),
                               choices = list("Choice 1 [Single var, all Exp and all Mesc]" = 1, 
                                              "Choice 2 [Single var, compare Exp for all Mesc]" = 2, 
                                             # "Choise 3 [Two var, all Exp and all Mesc, line plot comparison]" = 3,
                                              "Choice 4 [Six variable comparison for all exp and all Mesc]"= 4),selected = 1)),
                
                conditionalPanel(condition = "input.radio == '4'",
                       wellPanel(uiOutput("experiment"))),
               ## variable panel is moved up
                wellPanel(div(class="row-fluid", div(class="span6", downloadButton("Plotoutput", "Download"))))
                  ),
           #Conditional Plots tab creation
            column(9,
                  tabsetPanel(
                  tabPanel("Conditional Plots",plotOutput("plot",height="auto"),value="barplots"),
                  tabPanelAbout(),
                  id="tsp")),
           #Helps to suppress the unwanted red Error messages on the Browser
           tags$style(type="text/css",
                      ".shiny-output-error { visibility: hidden; }",
                      ".shiny-output-error:before { visibility: hidden; }"
           )
           
           
)))
