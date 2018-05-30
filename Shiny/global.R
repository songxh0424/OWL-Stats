library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(plotly)
library(stringr)
library(DT)
library(lubridate)
library(shinyjs)
## library(markdown)
## library(formattable)

## load('../Data/allMatchStats.RData')
load('../Data/detailedStats.RData')
load('../Data/savedObjects.RData')
load('../Data/heroStats.RData')

## plotting and theming functions
theme_Publication <- function(base_size=10, legend.pos = 'bottom') {
  t = (theme_foundation(base_size=base_size)
       + theme(plot.title = element_text(face = "bold",
                                         size = rel(1.2), hjust = 0.5),
               text = element_text(),
               panel.background = element_rect(colour = NA),
               plot.background = element_rect(colour = NA),
               panel.border = element_rect(colour = NA),
               axis.title = element_text(face = "bold",size = rel(1)),
               axis.title.y = element_text(angle=90,vjust =2),
               axis.title.x = element_text(vjust = -0.2),
               axis.text = element_text(), 
               axis.line = element_line(colour="black"),
               axis.ticks = element_line(),
               panel.grid.major = element_line(colour="#f0f0f0"),
               panel.grid.minor = element_blank(),
               legend.key = element_rect(colour = NA),
               legend.position = legend.pos,
               legend.title = element_text(face="italic"),
               plot.margin=unit(c(10,5,5,5),"mm")
      ))
  return(t)
}

scale_fill_Publication <- function(...){
      library(scales)
      discrete_scale("fill","Publication",manual_pal(values = rep(c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33", "#FFA500", "#3cb371", "#1E90FF"), 2)), ...)
}

scale_colour_Publication <- function(...){
      library(scales)
      discrete_scale("colour","Publication",manual_pal(values = rep(c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33", "#FFA500", "#3cb371", "#1E90FF"), 2)), ...)
}

plot_custom <- function(p, saveTo = NULL, palette = 'tableau20', base_size=10, legend.pos = "right", color = TRUE, fill = FALSE) {
  out = p + theme_Publication(base_size, legend.pos)
  if(color) out = out + scale_colour_tableau(palette = palette)
  if(fill) out = out + scale_fill_tableau(palette = palette)
  if(is.null(saveTo)) return(out)
  ggsave(saveTo, out)
  return(out)
}

################################################################################
## other global objects
################################################################################

b = tags$b
br = tags$br
bq = tags$blockquote

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}

## source all components
source('header.R')
source('sidebar.R')
source('body.R')

