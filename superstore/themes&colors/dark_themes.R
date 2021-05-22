
# * Dark Grey Theme ---- 

theme_dark_grey <- function(base_size=14, base_family="sans") {
   library(grid)
   library(ggthemes)
   (theme_foundation(base_size=base_size, base_family=base_family)
      + theme(plot.title = element_text(face = "bold", colour = '#ffffb3',
                                        size = rel(1.1), hjust = 0.0, margin = margin(0,0,5,0)),
              text = element_text(),
              panel.background = element_rect(colour = NA, fill = 'grey20'),
              plot.background = element_rect(colour = NA, fill = '#262626'),
              panel.border = element_rect(colour = NA),
              axis.title = element_text(face = "bold",size = rel(1), colour = 'white'),
              axis.title.y = element_text(angle=90,vjust =2),
              axis.title.x = element_text(vjust = -0.2),
              axis.text = element_text(colour = 'grey70'), 
              axis.line.x = element_line(colour="grey70"),
              axis.line.y = element_line(colour="grey70"),
              axis.ticks = element_line(colour="grey70"),
              panel.grid.major = element_line(colour="#262626"),
              panel.grid.minor = element_blank(),
              legend.background = element_rect(fill ='#262626'),
              legend.text = element_text(color = 'white'),
              legend.key = element_rect(colour = NA, fill = '#262626'),
              legend.position = "bottom",
              legend.direction = "horizontal",
              legend.box = "vetical",
              legend.key.size= unit(0.5, "cm"),
              #legend.margin = unit(0, "cm"),
              legend.title = element_text(face="italic", colour = 'white'),
              plot.margin=unit(c(5,5,5,5),"mm"),
              strip.background=element_rect(colour="#2D3A4C",fill="#2D3A4C"),
              strip.text = element_text(face="bold", colour = 'white')
      ))
}


scale_fill_todd_dark_subtle <- function(...){
   library(scales)
   discrete_scale("fill","todd",manual_pal(values = c("#fbb4ae","#b3cde3","#ccebc5","#decbe4","#fed9a6",
                                                      "#ffffcc","#e5d8bd","#fddaec","#f2f2f2")), ...)
}
scale_fill_todd_dark_bright <- function(...){
   library(scales)
   discrete_scale("fill","todd",manual_pal(values = c("#fbb4ae","#d708fb","#ffffcc","#75e6da","#94c973",
                                                      "#d4f1f4","#d3ae7c","#307ca1","#ffdf00","#f2f2f2")), ...)
}


scale_color_todd_dark_subtle <- function(...){
   library(scales)
   discrete_scale("color","todd",manual_pal(values = c("#decbe4","#fed9a6","#b3cde3","#ccebc5","#fbb4ae",
                                                       "#ffffcc","#e5d8bd","#fddaec","#f2f2f2")), ...)
}
scale_color_todd_dark_bright <- function(...){
   library(scales)
   discrete_scale("color","todd",manual_pal(values = c("#fbb4ae","#d708fb","#ffffcc","#75e6da","#94c973",
                                                       "#d4f1f4","#d3ae7c","#307ca1","#ffdf00","#f2f2f2")), ...)
}


clrs <- c("#fbb4ae", "#59bec4","#94c973","#ffffcc","#ff8f46", "#da4511","#307ca1", "#ffdf00","#f2f2f2")
show_col(clrs, labels = T, borders = NA)

clrs2 <- c("#fbb4ae","#d708fb","#ffffcc","#75e6da","#94c973",
           "#d4f1f4","#d3ae7c","#307ca1","#ffdf00","#f2f2f2")
show_col(clrs2, labels = T, borders = NA)

colors_todd <- c("#922B21", "#EE865D", "#DDCD5E", "#59BEC4", "#048B9F", "#114676", 
                 "#5C374C", "#985277", "#CE6A85", "#FFCF6A", "#FFB742", "#E9692C", 
                 "#DA4511", "#FFBD00", "#6A953F", "#9A6233", "#D3AE7C", "#307CA1", 
                 "#FF8EC8", "#FFDF51", "#46DBDF", "#FF8F46", "#42BAB7", "#DB0000",
                 "#FA6E4F", "#F2CF59", "#FB8E7E", "#C5D7C0", "#8EC9BB", "#F8CA9D",
                 "#fbb4ae","#b3cde3","#ccebc5","#decbe4","#fed9a6","#ffffcc","#e5d8bd",
                 "#fddaec","#f2f2f2", "#81B622", '#94C973', '#2F5233', '#B1D8B7', '#76B947',
                 '#D4F1F4' , '#75E6DA', "#189AB4", "#05445E", "#a86cc1",
                 "#06a0b0","#2371bb","#9de25f","#d708fb","#9c278f")
show_col(colors_todd, labels = T, borders = NA)

color4DarkTheme = c("#fbb4ae","#b3cde3","#ccebc5","#decbe4","#fed9a6",
                    "#ffffcc","#e5d8bd","#fddaec","#f2f2f2")
show_col(color4DarkTheme, labels = T, borders = NA)

scale_color_scico(palette = "hawaii", guide = F, direction = -1)

library(ragg)
scico_palette_names()
scico_palette_show()
?scale_size

fonts()
??ggtext
install.packages('here')
library(pdftools)
??colorspace
??ragg
??pdftools
library(scico)
??here
??scico
here()
install.packages('scico')
