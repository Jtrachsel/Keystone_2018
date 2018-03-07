#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#setwd('~/FS2/Everything/shiny/')

library(ggplot2)
library(shiny)
library(plotly)
library(shinythemes)
library(ggrepel)

newplot <- read.table('shiny_net.txt', header = TRUE, sep = '\t')
int_data <- read.table('int_data.txt', header = TRUE,  sep = '\t')
enrigglies <- read.table('enrich.txt', header = TRUE, sep = '\t')

# these dataframes are used in the plot call, they are subsets of newplot
labbes <- unique(newplot[,c(1,2,4,5,8,30)])

CD25 <- newplot[grep('CD25\\+', newplot$from),]
FoxP3 <- newplot[grep('FoxP3\\+', newplot$from),]
CD4 <- newplot[grep('CD4\\+', newplot$from),]
CD8 <- newplot[grep('CD8a\\+', newplot$from),]


#colnames(enrigglies)[1] <- 'from'
#enrigglies <- merge(newplot, enrigglies, by='from') %>% select(from, color, x, y, enriched_in) %>% unique()

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Set theme
  theme = shinytheme("spacelab"),
  
  # Some help text
  h2("Interactive exploration of the RPS cecal tissue correlation network"),
  h4("Click on a node to view how that feature compares between the treatment groups"),
  h6("Hover over a node to view that node's identity"),
  # Vertical space
  tags$hr(),
  
  # First row
  fixedRow(
    column(7, plotlyOutput("Plot1", height = "800px")),
    column(5, plotOutput("Plot2", height = "400px"))))


# Define server logic required to draw a histogram
server <- function(input, output){
  
  
  # Note the use of 'source' argument
  output$Plot1 <- renderPlotly({
    pl <- ggplot(newplot) +
      geom_point(data=enrigglies, aes(x=x+.0001, y=y+.0001, color=color), size=46, alpha=.1, show.legend = FALSE, inherit.aes = FALSE)+
      geom_point(data=enrigglies, aes(x=x+.0002, y=y+.0002, color=color), size=44, alpha=.1, show.legend = FALSE, inherit.aes = FALSE)+
      #geom_point(data=enrigglies, aes(x=x, y=y, color=color), size=32, alpha=.1, show.legend = FALSE, inherit.aes = FALSE)+
     # geom_point(data=enrigglies, aes(x=x-.0001, y=y-0001, color=color), size=42, alpha=.1, show.legend = FALSE, inherit.aes = FALSE)+
      geom_point(data=enrigglies, aes(x=x+.00001, y=y+.00001, color=color), size=40, alpha=.1, show.legend = FALSE, inherit.aes = FALSE)+
      geom_point(data=enrigglies, aes(x=x-.00001, y=y-.00001, color=color), size=38, alpha=.1, show.legend = FALSE, inherit.aes = FALSE)+
      geom_point(data=enrigglies, aes(x=x+.00002, y=y+.00002, color=color), size=36, alpha=.1, show.legend = FALSE, inherit.aes = FALSE)+
      #geom_point(data=enrigglies, aes(x=x+.00001, y=y+.001, color=color), size=34, alpha=.1, show.legend = FALSE, inherit.aes = FALSE)+
      geom_point(data=enrigglies, aes(x=x+.000001, y=y-.000001, color=color), size=32, alpha=.1, show.legend = FALSE, inherit.aes = FALSE)+
      #geom_point(data=enrigglies, aes(x=x+.001, y=y-.00001, color=color), size=30, alpha=.1, show.legend = FALSE, inherit.aes = FALSE)+
      geom_point(data=enrigglies, aes(x=x+.000003, y=y+.00003, color=color), size=28, alpha=.1, show.legend = FALSE, inherit.aes = FALSE)+
      geom_point(data=enrigglies, aes(x=x+.000004, y=y+.000004, color=color), size=26, alpha=.1, show.legend = FALSE, inherit.aes = FALSE)+
      geom_point(data=enrigglies, aes(x=x+.0000011, y=y+.000011, color=color), size=24, alpha=.1, show.legend = FALSE, inherit.aes = FALSE)+
      geom_point(data=enrigglies, aes(x=x-.0000101, y=y-.000101, color=color), size=22, alpha=.1, show.legend = FALSE, inherit.aes = FALSE)+
      
      geom_segment(data=newplot, aes(x=x, y=y, xend=xend, yend=yend), color='black') +
      geom_point(data=CD25, aes(x=x, y=y), color='red', size=12, show.legend = FALSE)+
      geom_point(data=newplot, aes(x=x, y=y, fill=colour, group = label), shape=21, size=8, show.legend = FALSE, inherit.aes = FALSE) +
      geom_rect(data=CD8, aes(xmin=x-.0013,ymin=y-.03, xmax=x+.0013, ymax=y+.03), fill='black', color='black', show.legend = FALSE)+
      geom_rect(data=CD4, aes(xmin=x-.03,ymin=y-.0013, xmax=x+.03, ymax=y+.0013), fill='gold', color='gold', show.legend = FALSE)+
      geom_point(data=FoxP3, aes(x=x, y=y), color='blue', size=3.5)+
      #geom_label_repel(data=labbes,aes(x=x, y=y, label=label, fill=colour),point.padding = unit(.75, 'lines'), fontface="bold", color='black',
      #                 alpha=.75, show.legend=FALSE, size=3, parse = FALSE) + 
      scale_color_identity()+
      scale_fill_identity()+
      
      
      # this is all for the legend
      annotate('point', x=1.027, y=1.08, color="#7B6B4D", size=8) + annotate('text', x=1.072, y=1.08 , label='VFA', size=3, color='black') +
      annotate('point', x=1.027, y=1.04, color="#BB8714", size=8) + annotate('text', x=1.072, y=1.04 , label='16S', size=3, color='black') +
      annotate('point', x=1.027, y=1.00, color="#BC5266", size=8) + annotate('text', x=1.072, y=1.00, label='barrier', size=3, color='black') +
      annotate('point', x=1.027, y=.96, color="#66A61E", size=8) + annotate('text', x=1.072, y=.96 , label='CD3+', size=3, color='black') +
      annotate('point', x=1.027, y=.92, color="#966A78", size=8) + annotate('text', x=1.072, y=.92 , label='CD3-', size=3, color='black') +
      annotate('point', x=1.027, y=.88, color="grey", size=8) + annotate('text', x=1.072, y=.88 , label='CD4+', size=3, color='black') +
      annotate('point', x=1.027, y=.84, color="grey", size=8) + annotate('text', x=1.072, y=.84 , label='CD8+', size=3, color='black') +
      annotate('point', x=1.0270001, y=.80, color="red", size=11) +
      annotate('point', x=1.027, y=.80, color="grey", size=8) + annotate('text', x=1.072, y=.80 , label='CD25+', size=3, color='black') +
      annotate('point', x=1.027, y=.76, color="grey", size=8) + annotate('text', x=1.072, y=.76 , label='FoxP3+', size=3, color='black') +
      annotate('rect', xmin=1.004, xmax=1.05, ymin=.878, ymax=.882, color="gold", fill = 'gold') +
      annotate('rect', xmin=1.024, xmax=1.030, ymin=.818, ymax=.862, color="black", fill = 'black') +
      annotate('point', x=1.0270001, y=.76, color="blue", size=4) +
      
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            aspect.ratio = 1)
    
    
    
    
    ggply <- ggplotly(pl, source = "subset" , tooltip=c("group"))
    
    # ggply$x$data[[2]]$hoverinfo <- "none"
    # ggply$x$data[[1]]$hoverinfo <- "none"
    # ggply$x$data[[8]]$hoverinfo <- "none"
    # ggply$x$data[[9]]$hoverinfo <- "none"
    # ggply$x$data[[10]]$hoverinfo <- "none"
    # 
    # ggply$elementId <- NULL
    ggply %>% layout(showlegend = FALSE)
  })
  
  
  filtered <- reactive({
    
    # Get subset based on selection
    
    event.data <- event_data("plotly_click", source = "subset")
    
    # If NULL dont do anything
    if(is.null(event.data) == T) return(NULL)
    
    va <- event.data$x
    vb <- event.data$y
    
    xx <- which(abs(newplot[,4]-va)==min(abs(newplot[,4]-va)))
    yy <- which(abs(newplot[,5]-vb)==min(abs(newplot[,5]-vb)))
    p <- intersect(xx, yy)[1]
    type <- newplot[p, 2]
    filt <- int_data %>% filter(node == type)
    filt
    
  })
  
  # Coupled event 1
  output$Plot2 <- renderPlot({
    if(is.null(filtered()) == T) return(NULL)
    
    
    p3 <- ggplot(filtered()) + 
      geom_boxplot(aes(x= treatment, y=value, group=treatment, fill=treatment),outlier.shape = NA) + 
      geom_jitter(aes(x=treatment, y=value, group=treatment, fill=treatment),width = .2, shape = 21, size=4, stroke=1) +
      scale_fill_brewer(palette="Dark2") + 
      labs(y=filtered()$scale_lab[1]) + 
      ggtitle(filtered()$node)
    
    p3
  })
  
  
}



shinyApp(ui = ui, server = server) 

