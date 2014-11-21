library(shiny)
pkgs <- c("reshape2","raster","maps","maptools", "ggplot2", "scales", "grid", "gdata","xlsx","data.table","tcltk")
pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(pkgs)) install.packages(pkgs,repos="http://cran.cs.wwu.edu/")
library(reshape2); library(raster); library(maptools) ; library(ggplot2) ; library(scales) ; library(grid); library(gdata); library(xlsx); library(data.table); library(tcltk)

file1 <- 0
file2 <- 0

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 10*1024^2)

#Main Shinyserver function
shinyServer(function(input, output) {
   
  
  #Plot
  output$showMapPlot <- renderUI({
        { list(plotOutput("plot",height="100%"), br()) }
  })
  

  #Change the selectInput based on the radioButton option  
  output$select <- renderUI({
    if((input$radio == 1 | input$radio == 2))
    {
      if(!is.null(a()) == TRUE & !is.null(b()) == FALSE)
      {
      selectInput("select", h4("Variables:"), choices=vars()$model, selected=vars()$model[1], multiple=F, width="100%")
      }
      else if(!is.null(b()) == TRUE & !is.null(a()) == FALSE)
      {
      selectInput("select", h4("Variables:"), choices=vars()$observation, selected=vars()$observation[1], multiple=F, width="100%")
      }
      else if(!is.null(a()) == TRUE & !is.null(b()) == TRUE)
      { 
      selectInput("select", h4("Variables:"), choices=var.both1(), selected=var.both1()[1], multiple=F, width="100%")
      }
    }
    else if(input$radio == 4)
    {
      if(!is.null(a()) == TRUE & !is.null(b()) == FALSE)
      {
        selectInput("select", h4("Variables:"), choices=vars()$model, selected=vars()$model[1], multiple=T, width="100%")
      }
      else if(!is.null(b()) == TRUE & !is.null(a()) == FALSE)
      {
      selectInput("select", h4("Variables:"), choices=vars()$observation, selected=vars()$observation[1], multiple=T, width="100%")
      }
      else if(!is.null(a()) == TRUE & !is.null(b()) == TRUE)
      {
      selectInput("select", h4("Variables:"), choices=var.both2(), selected=var.both2()[1], multiple=T, width="100%")
      }
    }
    else if(input$radio == 3)
    {
      selectInput("select", h4("Variables:"), choices=var.both1(), selected=var.both1()[1], multiple=T, width="100%")
    }
  })                      
  
  ######### variables = compare between the names of model and observation and eleminate the missings and show
  
  
  output$experiment <- renderUI({
    if(input$radio == 4 & input$check == 'Model') 
      {selectInput("experiment", h4("Experiment:"), choices=unique(a()$Experiment), selected=NULL)}
    else if(input$radio == 4 & input$check =='Observation') 
      {selectInput("experiment", h4("Experiment:"), choices=unique(b()$Experiment), selected=NULL)}
    else if(input$radio == 4 & input$check == 'Both') 
      {selectInput("experiment", h4("Experiment:"), choices=unique(b()$Experiment), selected=NULL)}                                                   
  })


  #Upload Model Data
 a <- reactive({
   fileinput1 <- input$file1
   if (is.null(fileinput1))
   return(NULL)
   #read.table(fileinput1$datapath, header = TRUE, col.names = c("Ei","Mi","hours","Nphy","Cphy","CHLphy","Nhet","Chet","Ndet","Cdet","DON","DOC","DIN","DIC","AT","dCCHO","TEPC","Ncocco","Ccocco","CHLcocco","PICcocco","par","Temp","Sal","co2atm","u10","dicfl","co2ppm","co2mol","pH"))
   read.table(fileinput1$datapath, header = TRUE, col.names =  c("Experiment","Mesocosm","Hour","Nphy","Cphy","CHLphy","Nhet","Chet","Ndet","Cdet","DON","DOC","DIN","DIC","AT","dCCHO","TEPC","Ncocco","Ccocco","CHLcocco","PICcocco","PAR","Temperature","Salinity","CO2atm","u10","DICflux","CO2ppm","CO2mol","pH"))
   #a$Chla <- a$CHLphy + a$CHLcocco  #Add new columns as per observation data
   #a$PON <- a$Nphy + a$Nhet + a$Ndet + a$Ncocco 
 })
 
 #Upload Observation Data 
b <- reactive({
  #xlfile <- list.files(pattern = "*.xlsx")
  fileinput2 <- input$file2
      if (is.null(fileinput2))
        return(NULL)
  withProgress(message = 'Downloading file', value = 0, {
      xlfile <- fileinput2$datapath
  wb <- loadWorkbook(xlfile)
  sheet_ct <- wb$getNumberOfSheets()
  for( i in 1:sheet_ct) {    #read the sheets into 3 separate dataframes (mydf_1, mydf_2, mydf3)
    print(i)
    variable_name <- sprintf('mydf_%s',i)
    assign(variable_name, read.xlsx(xlfile, sheetIndex=i))
    incProgress(1/sheet_ct, detail = paste("Sheet:", i,"Dowloaded"))
  }
  colnames(mydf_1) <- names(mydf_3)
  colnames(mydf_2) <- names(mydf_3)
  full_data <- rbind(mydf_1,mydf_2,mydf_3) #making one dataframe here
  b <- lapply(full_data,function(x) as.numeric(x))
})
})

namesb <- reactive ({
  b()[-c(1,2,3,4)]
})
namesa <- reactive ({
  a()[-c(1,2,3)]
})
vars <- reactive({  
  list(model = names(namesa()), observation = names(namesb()))
})
var.both1 <- reactive({
  c(vars()$model, vars()$observation)
})
var.both2 <- reactive({
  Reduce(intersect,list(vars()$model, vars()$observation))
})

############################################################

 #Change the Dataset selection based on file input.
 #Choose the variable --> if data for that varibale is present then plot--> Model present then give option for(dataset selection) model if observatjion present then give option for observation...if both present then give option for both
getmodel <- reactive({ 
  
    if(!is.null(a()) & is.element(input$select,names(a())) == TRUE)
    {
      if(!is.null(b()) == FALSE)
      {
      c("Model")
      }
      else if(!is.null(b()) & is.element(input$select,names(b())) == TRUE)
      {
      c("Model","Observation","Both")
      }
      else if(!is.null(b()) & is.element(input$select,names(b())) == FALSE)
      {
      c("Model")
      }
    }
    else if(!is.null(a()) & is.element(input$select,names(a())) == FALSE)
    {
      if(is.null(b()))
      {
        validate(
          need(!is.null(b()), "Please select a data set")
        )
        #tk_messageBox(type="ok",message="Variable not found in the dataset")
      }
      else if(!is.null(b()) & is.element(input$select,names(b())) == TRUE)
      {
        c("Observation")
      }
      else if(!is.null(b()) & is.element(input$select,names(b())) == FALSE)
      {
        validate(
          need(is.null(b()) & is.element(input$select,names(b())) == TRUE, "Please select a data set")
        )
        #tk_messageBox(type="ok",message="Variable not found in the dataset")
      }
    }
    else if(!is.null(b()) & is.element(input$select,names(b())) == TRUE)
    {
      if(is.null(a()))
      {
      c("Observation")
      }
      else if(!is.null(a()) & is.element(input$select,names(a())) == TRUE)
      {
      c("Model","Observation","Both")
      }
      else if(!is.null(a()) & is.element(input$select,names(a())) == FALSE)
      {
      c("Observation")
      }
    }
    else if(!is.null(b()) & is.element(input$select,names(b())) == FALSE)
    {
      if(is.null(a()))
      {
        validate(
          need(!is.null(a()), "Please select a data set")
        )
        #tk_messageBox(type="ok",message="Variable not found in the dataset")
      }
      else if(!is.null(a()) & is.element(input$select,names(a())) == TRUE)
      {
        c("Model")
      }
      else if(!is.null(a()) & is.element(input$select,names(a())) == FALSE)
      {
        validate(
          need(is.null(a()) & is.element(input$select,names(a())) == TRUE, "Please select a data set")
        )
        #tk_messageBox(type="ok",message="Variable not found in the dataset")
      }
    }
    else if(!is.null(b()) & !is.null(a()))
    {
      if(is.element(input$select,names(a())) == TRUE & is.element(input$select,names(b())) == FALSE)
      {
        c("Model")
      }
      else if(is.element(input$select,names(a())) == FALSE & is.element(input$select,names(b())) == TRUE)
      {
        c("Observation")
      }
      else if(is.element(input$select,names(a())) == TRUE & is.element(input$select,names(b())) == TRUE)
      {
      c("Model","Observation","Both")
      }
    }
})


#Update the Dataselection selectinput
  output$check <- renderUI({
  selectInput("check", label = h4("Dataset Selection"), choices = as.list(getmodel()), multiple = F )
  })
   
#Update the units to the variable names
variableunit <- reactive ({
  if(input$select == "TEPC") {"degC"}
  else if(input$select == "AT"){"µmol/kg"}
  else if(input$select == "DIC" | input$select == "DIN" | input$select == "PIC" | input$select == "POC" | input$select == "PON" | input$select == "POP" | input$select == "DOC" | input$select == "DON" | input$select == "DOP" | input$select == "TEP"){c("µmol/L")}
  else if(input$select == "Chla"){"µg/L"}
  else ("Meters")  
})

variableunitupdated <- reactive ({
  paste(input$select,"(", variableunit(),")")
})

  # Based on the selected radio button (Option) and the input variables (SelectInput) Do the plotting.
  # Option --> radioButton --> Function to apply
  # Variable --> SelectInput --> Variables to pass into functions
    #########################################Plot Code############################################
  
  #Plot conditions and Downlaod graph implementation
  output$plot <- renderPlot({
    if(input$radio == "1")
    {
      plot_1()
      
      output$Plotoutput <- downloadHandler(
        filename = 'curPlot.pdf',
        content = function(file){
          pdf(file = file, width=11, height=8.5, pointsize=8)
          plot_1()
          dev.off()
        }
      )
    }
    if(input$radio == "2")
    {
      plot_2()
      
      output$Plotoutput <- downloadHandler(
        filename = 'curPlot.pdf',
        content = function(file){
          pdf(file = file, width=11, height=8.5, pointsize=8)
          plot_2()
          dev.off()
        }
      )
    }
    if(input$radio == "3")
    {
      if(length(input$select) >= 2)
      {
      plot_3()
      
      output$Plotoutput <- downloadHandler(
        filename = 'curPlot.pdf',
        content = function(file){
          pdf(file = file, width=11, height=8.5, pointsize=8)
          plot_3()
          dev.off()
        }
      )
    }
    }
    if(input$radio == "4")
    {
#      withProgress(message = 'You have to select six variables for me to process the plots...', value = 0, {
      if(length(input$select) >= 6)
      { 
#         count <- length(input$select)
#          for (i in 1:count){
#            incProgress(1/count, detail = paste("Select", i,"variable"))
#          }
        
      plot_4()
      output$Plotoutput <- downloadHandler(
        filename = 'curPlot.pdf',
        content = function(file){
          pdf(file = file, width=11, height=8.5, pointsize=8)
          plot_4()
          dev.off()
        }
      )
    }
#      })
    }
  },
  height=700, width=1100
  )
                      ################# Option-1 ###################
  
plot_1 <-  function(var = input$select) {
        withProgress(message = 'Processing please wait...', value = 0, {
        if(input$check == 'Model'){
        gg1 <- aggregate(cbind(get(var)) ~ Experiment + Hour , a(), FUN=mean)
        names(gg1)[2] <- "Day"
        names(gg1)[3] <- var
        gg1$Day <- gg1$Day /24
        dataset <- gg1
        }
        
        else if(input$check == 'Observation'){
        ggl <- aggregate(cbind(get(var)) ~ Experiment + Hour , b(), FUN=mean)
        names(ggl)[2] <- "Day"
        names(ggl)[3] <- var
        ggl$Day <- ggl$Day /24
        dataset <- ggl
        }
        
        else if(input$check == 'Both'){
          gg1 <- aggregate(cbind(get(var)) ~ Experiment + Hour , a(), FUN=mean)
          names(gg1)[2] <- "Day"
          names(gg1)[3] <- var
          gg1$Day <- gg1$Day /24
          ggl <- aggregate(cbind(get(var)) ~ Experiment + Hour , b(), FUN=mean)
          names(ggl)[2] <- "Day"
          names(ggl)[3] <- var
          ggl$Day <- ggl$Day /24
          dataset <- gg1
        }
        
        g <- ggplot(subset(dataset,!is.na(var)), aes_string(x = "Day", y = var, group = "Experiment")) +
                #geom_point(aes(color = factor(Mesocosm)),size = 3, alpha = 0.7) + 
                #geom_line(data = (ggl), size = 1) +
                scale_color_manual("Experiment", values = c('#ff00ff', '#3399ff', '#808080')) +
                scale_shape_manual(values = c("Experiment 1" = '#ff00ff',"Experiment 2" = '#3399ff', "Experiment 3" = '#808080')) +
                scale_y_continuous(breaks=pretty_breaks(n=6)) +
                guides(colour = guide_legend(override.aes = list(size=6))) +
                scale_x_continuous(breaks=pretty_breaks(n=10), limits=c(input$slider[1],input$slider[2])) +
                theme_bw() +
                ylab(variableunitupdated()) +
                theme (legend.position = "right", legend.title=element_text(size=14),legend.key.width = unit(5, "line"),
                       legend.key.size =  unit(0.2, "in"), legend.text = element_text(size=14),
                       panel.border = element_rect(colour = "black"),strip.background = element_rect(fill="#CCCCFF"), 
                       strip.text.x = element_text(size=14, face="bold"),axis.text.y = element_text(colour="grey20",size=13,face="bold"),
                       axis.text.x = element_text(colour="grey20",size=13,face="bold"),
                       axis.title.x = element_text(colour="grey20",size=20,face="bold"),
                       axis.title.y = element_text(colour="grey20",size=20,face="bold")) 
       if(input$check == 'Model'){
        print(g + geom_point(aes(color = factor(Experiment)),size = 3, alpha = 0.7, shape = 16))
        }
        else if(input$check == 'Observation'){
        print(g + geom_point(aes(color = factor(Experiment)),size = 6, alpha = 0.7, shape = 17) )
        }
       else if(input$check == 'Both'){
          #print (g + geom_point(size = 3, alpha = 0.7, shape = b) + geom_point(data = ggl,aes(x = "Day", y = var, group = "Experiment"),size = 3, alpha = 0.7, shape = a)) 
          #print (g + geom_point(shape = 16, size = 6) + geom_point(data = ggl, shape = 17, size = 6) ) 
         print (g + geom_point(data = (ggl),aes(color = factor(Experiment)), size = 6, shape = 17) + geom_point(aes(color = factor(Experiment)),size = 3, shape = 16))
        }
    })
}

                      ################ END of Option-1 ###############
  
                      ################ Option-2 ######################
  plot_2 <- function(var = input$select) {
    withProgress(message = 'Processing please wait...', value = 0, {
    if(input$check == 'Model'){
      #EXP <- unique(a()$Experiment)
      gg2 <- aggregate(cbind(get(var)) ~ Experiment + Mesocosm + Hour, a(), FUN=mean)
      names(gg2)[3] <- "Day"
      names(gg2)[4] <- var
      gg2$Day <- gg2$Day /24
      dataset <- gg2
    }
    
    else if(input$check == 'Observation'){
      #gg2 <- subset(aggr,aggr$Experiment == c(EXP))
      ggl <- aggregate(cbind(get(var)) ~ Experiment + Mesocosm + Hour, b(), FUN=mean)
      names(ggl)[3] <- "Day"
      names(ggl)[4] <- var
      ggl$Day <- ggl$Day /24
      dataset <- ggl
    }
    
    else if(input$check == 'Both'){
      gg2 <- aggregate(cbind(get(var)) ~ Experiment + Mesocosm + Hour, a(), FUN=mean)
      names(gg2)[3] <- "Day"
      names(gg2)[4] <- var
      gg2$Day <- gg2$Day /24
      ggl <- aggregate(cbind(get(var)) ~ Experiment + Mesocosm + Hour, b(), FUN=mean)
      names(ggl)[3] <- "Day"
      names(ggl)[4] <- var
      ggl$Day <- ggl$Day /24
      dataset <- gg2
    }
       
    g <- ggplot(subset(dataset,!is.na(var)),aes_string(x="Day", y= var)) + 
             #geom_point(aes(color = factor(Mesocosm)),size = 3) +
             #geom_line(data = (ggl), size = 1) +
             #geom_smooth(color="blue",stat= "smooth" , alpha = I(0.01), method="loess") +
             #coord_fixed(ratio=1) +
             facet_grid(Experiment~., labeller=function(x,y) (paste0("EXP",y)))  +
             scale_color_manual("Mesocosm", values = c('#FF0000', '#00FF00', '#0000FF', '#FFFF00', '#FF00FF', '#808080', '#800000' , '#008000', '#008080'), breaks=pretty_breaks(n=8) ) +
             scale_y_continuous(breaks=pretty_breaks(n=10)) +
             guides(colour = guide_legend(override.aes = list(size=8))) +
             scale_x_continuous(breaks=pretty_breaks(n=10), limits=c(input$slider[1],input$slider[2])) +
             theme_bw() +
             ylab(variableunitupdated()) +
             theme (legend.position = "right", legend.title=element_text(size=14),legend.key.width = unit(5, "line"),
                    legend.key.size =  unit(0.2, "in"), legend.text = element_text(size=14),
                    panel.border = element_rect(colour = "black"),strip.background = element_rect(fill="#CCCCFF"), 
                    strip.text.x = element_text(size=14, face="bold"),
                    strip.text.y = element_text(size=14, face="bold"),
                    axis.text.y = element_text(colour="grey20",size=13,face="bold"),
                    axis.text.x = element_text(colour="grey20",size=13,face="bold"),
                    axis.title.x = element_text(colour="grey20",size=20,face="bold"),
                    axis.title.y = element_text(colour="grey20",size=20,face="bold")) 
    if(input$check == 'Model'){
      print (g + geom_line(aes(color = factor(Mesocosm)),size = 3))
    }
    else if(input$check == 'Observation'){
      print (g + geom_point(aes(color = factor(Mesocosm)), size = 5) + guides(colour = guide_legend(override.aes = list(size=5))))
    }
    else if(input$check == 'Both'){
      print (g + geom_point(data = (ggl),aes(color = factor(Mesocosm)), size = 5) + geom_line(aes(color = factor(Mesocosm)),size = 3))
    }
  })
  }
  
                      ################ END of Option-2 ###############
  
                      ############## Option-3 ########################
  plot_3 <- function(var1 = input$select[1],var2 = input$select[2]) {
    
    gg3 <- aggregate(cbind(get(var1),get(var2))~Mesocosm+Hour,a(), FUN=mean)
    names(gg3)[2] <- "Day"
    names(gg3)[3] <- var1
    names(gg3)[4] <- var2
    gg3$Day <- gg3$Day/24
    plot(x = gg3[,"Day"], y = gg3[,var1], type="p", col="red", xlab="", ylab="", xlim=c(input$slider[1],input$slider[2]))
    par(new=TRUE)
    plot(x = gg3[,"Day"], y = gg3[,var2], type="p", col="blue", xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(input$slider[1],input$slider[2]))
    axis(4)
    mtext(var1,side=2,line=2)
    mtext(var2,side=4,line=-2)
    mtext("Day",side=1,line=2)
    legend("topleft",col=c("red","blue"),lty=1,legend=c(var1,var2), text.width = 3)
  }
  
                      ################ END of Option-3 ################
  
                      #################### Option-4 #####################
  
  plot_4 <- function(var1 = input$select[1], var2 = input$select[2], var3 = input$select[3], var4 = input$select[4], var5 = input$select[5], var6 = input$select[6]) {
    myvars <- c(var1,var2,var3,var4,var5,var6)
    
    withProgress(message = 'Processing please wait...', value = 0, { 
    
    if(input$check == 'Model'){
      gg4 <- subset((aggregate(cbind(get(var1),get(var2),get(var3),get(var4),get(var5),get(var6))~Experiment+Mesocosm+Hour,a(), FUN=mean)),Experiment == input$experiment)  
      names(gg4)[4] <- var1
      names(gg4)[5] <- var2
      names(gg4)[6] <- var3
      names(gg4)[7] <- var4
      names(gg4)[8] <- var5
      names(gg4)[9] <- var6
      names(gg4)[3] <- "Day"
      gg4$Day <- gg4$Day /24
      ddl <- melt(gg4,id.vars=c("Experiment","Mesocosm","Day"), measure.vars=myvars, na.rm = TRUE)
      dataset <- ddl
    }
    else if(input$check == 'Observation'){
      ggl <- subset((aggregate(cbind(get(var1),get(var2),get(var3),get(var4),get(var5),get(var6))~Experiment+Mesocosm+Hour,b(), FUN=mean)),Experiment == input$experiment)
      names(ggl)[4] <- var1
      names(ggl)[5] <- var2
      names(ggl)[6] <- var3
      names(ggl)[7] <- var4
      names(ggl)[8] <- var5
      names(ggl)[9] <- var6
      names(ggl)[3] <- "Day"
      ggl$Day <- ggl$Day /24
      ddp <- melt(ggl,id.vars=c("Experiment","Mesocosm","Day"), measure.vars=myvars, na.rm = TRUE)
      dataset <- ddp
    }
    else if(input$check == 'Both'){
      gg4 <- subset((aggregate(cbind(get(var1),get(var2),get(var3),get(var4),get(var5),get(var6))~Experiment+Mesocosm+Hour,a(), FUN=mean)),Experiment == input$experiment)  
      names(gg4)[4] <- var1
      names(gg4)[5] <- var2
      names(gg4)[6] <- var3
      names(gg4)[7] <- var4
      names(gg4)[8] <- var5
      names(gg4)[9] <- var6
      names(gg4)[3] <- "Day"
      gg4$Day <- gg4$Day /24
      ddl <- melt(gg4,id.vars=c("Experiment","Mesocosm","Day"), measure.vars=myvars, na.rm = TRUE)
      ggl <- subset((aggregate(cbind(get(var1),get(var2),get(var3),get(var4),get(var5),get(var6))~Experiment+Mesocosm+Hour,b(), FUN=mean)),Experiment == input$experiment)
      names(ggl)[4] <- var1
      names(ggl)[5] <- var2
      names(ggl)[6] <- var3
      names(ggl)[7] <- var4
      names(ggl)[8] <- var5
      names(ggl)[9] <- var6
      names(ggl)[3] <- "Day"
      ggl$Day <- ggl$Day /24
      ddp <- melt(ggl,id.vars=c("Experiment","Mesocosm","Day"), measure.vars=myvars, na.rm = TRUE)
      dataset <- ddl
    }
      dataset$label <- paste(as.character(dataset$variable), "(", variableunit(), ")", sep="")
    g <- ggplot(subset(dataset,!is.na(myvars)),aes(x=Day, y=value)) + 
            #facet_wrap(~variable, nrow=3, ncol=2,scales = "free_y") +
            facet_wrap(~label, nrow=3, ncol=2,scales = "free_y") +
            scale_color_manual("Mesocosm", values = c('#FF0000', '#00FF00', '#0000FF', '#FFFF00', '#FF00FF', '#808080', '#800000' , '#008000', '#008080')) +
            scale_y_continuous(breaks=pretty_breaks(n=6)) +
            guides(colour = guide_legend(override.aes = list(size=8))) +
            scale_x_continuous(breaks=pretty_breaks(n=6), limits=c(input$slider[1],input$slider[2])) +
            theme_bw() +
            theme (legend.position = "right", legend.title=element_text(size=14),legend.key.width = unit(5, "line"),
                   legend.key.size =  unit(0.2, "in"), legend.text = element_text(size=14),
                   panel.border = element_rect(colour = "black"),strip.background = element_rect(fill="#CCCCFF"), 
                   strip.text.x = element_text(size=14, face="bold"),
                   axis.text.y = element_text(colour="grey20",size=13,face="bold"),
                   axis.text.x = element_text(colour="grey20",size=13,face="bold"),
                   axis.title.x = element_text(colour="grey20",size=20,face="bold"),
                   axis.title.y = element_text(colour="grey20",size=20,face="bold")) 
    if(input$check == 'Model'){
      print (g + geom_line(aes(color = factor(Mesocosm)),size = 3))
    }
    else if(input$check == 'Observation'){
      print (g + geom_point(data = ddp,aes(color = factor(Mesocosm)), size = 5) + guides(colour = guide_legend(override.aes = list(size=5))))
    }
    else if(input$check == 'Both'){
      print (g + geom_point(data = (ddp),aes(color = factor(Mesocosm)), size = 5) + geom_line(aes(color = factor(Mesocosm)),size = 3))
    }
    })
   }                    ################ END of Option-4 ################
})
            ###################################### END of Program##########################################