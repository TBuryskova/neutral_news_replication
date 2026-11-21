library(tidyverse)
library(plotly)
library(readr)
library(ggplot2)
library(ggforce)
set.seed(1997)
p=1

########### one source - belief evolution #################
util<- function(q) {
  2^(p-1)*abs(q-0.5)^p+0.5
#  max(q,1-q)
}
e <- 0.4
e_nu<- 0.1
q <- 0.8


  data <- cbind(q=rep(q,6), 
                source=c(rep("α",2), 
                         rep("ν",2),
                         rep("β",2)
                ), 
                signal=c("A", "B", "A", "B", "A", "B"),
                P=(c(q+e*(1-q), (1-q)*(1-e),((1-e_nu)*q+(e_nu)*(1-q)), ((e_nu)*q+(1-e_nu)*(1-q)),(1-e)*q, (e*q+(1-q)))),
                belief=c(q/(q+e*(1-q)), 0,(1-e_nu)*q/((1-e_nu)*q+(e_nu)*(1-q)), (e_nu)*q/((e_nu)*q+(1-e_nu)*(1-q)), 1, e*q/(e*q+(1-q)) )
                )%>%
    data.frame()  %>%
    mutate(P=as.numeric(P), belief=as.numeric(belief), q=as.numeric(q),
           util_b=util(belief),
           source=factor(source, levels=c("α","ν","β")))
  
envir <-data.frame(
  x=seq(0,1, by=0.01)
) %>% mutate(
  util=util(x)
)

  
  ggplot() +

    geom_point(data=data, aes(x=q,y =q), size=5) +
    geom_line(data=envir, aes(x=x, y=util)) +
    geom_point(data=data, aes(x=belief, y=util_b, group=source, color=source), size=5) +
     geom_line(data=data, aes(x=belief, y=util_b, group=source, color=source), linewidth=2) +
    geom_vline(xintercept=q, linetype="dashed") +
    theme(text = element_text(size = 20)) +
    labs(title = "",
         x = "belief",
         y = "utility", color="source") +
    theme_minimal() +

    geom_label(data=data,aes(x=q, y=1),label="exp.posterior=prior", nudge_y=0.05, size=5) +
    theme(text = element_text(size = 20), legend.position = "bottom") 



########### two sources - belief evolution #################
e = 0.8
e_nu = 0.3
q=0.55 #betanu
p=2
  data <- cbind(q=rep(q,16), 
                source=c(rep("αα",2), 
                         rep("αβ",3),
                         rep("ββ",2),
                         rep("νν",3),
                         rep("αν",3),
                         rep("βν",3)
                ), 
                signal=c("AαAα","AαBα/BαAα/BαBα", 
                         "AαAβ","AαBβ","BαBβ",
                         "AβAβ/AβBβ/BβAβ", "BβBβ",
                         "AνAν","AνBν/BνAν", "BνBν", 
                         "AαAν","AαBν","BαAν/BαBν", 
                         "AβAν/AβBν","BβAν","BβBν"
                ), 
                P=c(q+e^2*(1-q),1-q-e^2*(1-q),
                    (1-e)*q, e, (1-e)*(1-q),
                    1-e^2*q-(1-q), e^2*q+(1-q),
                    (1-e_nu)^2*q+e_nu^2*(1-q),2*(1-e_nu)*e_nu, e_nu^2*q+(1-e_nu)^2*(1-q),
                    (1-e_nu)*q+e*e_nu*(1-q), e_nu*q+e*(1-e_nu)*(1-q), (1-e)*e_nu*(1-q)+(1-e_nu)*(1-e)*(1-q),
                    (1-e)*(1-e_nu)*q+(1-e)*e_nu*q, (1-e_nu)*e*q+(1-q)*e_nu, e*e_nu*q+(1-q)*(1-e_nu)
                ),
                belief=c(q/(q+e^2*(1-q)),0,
                         1, q, 0,
                         1, e^2*q/(e^2*q+(1-q)),
                         (1-e_nu)^2*q/((1-e_nu)^2*q+e_nu^2*(1-q)),q, e_nu^2*q/(e_nu^2*q+(1-e_nu)^2*(1-q)),
                         (1-e_nu)*q/((1-e_nu)*q+e*e_nu*(1-q)), e_nu*q/(e_nu*q+e*(1-e_nu)*(1-q)), 0,
                         1, (1-e_nu)*e*q/((1-e_nu)*e*q+(1-q)*e_nu), e*e_nu*q/(e*e_nu*q+(1-q)*(1-e_nu))
                )
                
                
  )%>%
    data.frame()  %>%
    mutate(P=as.numeric(P), belief=as.numeric(belief), q=as.numeric(q), util_b=util(belief),source=factor(source, levels= c("αα","αν", "αβ", "νν",  "βν","ββ")))

  envir <-data.frame(
    x=seq(0,1, by=0.01)
  ) %>% mutate(
    util=util(x)
  )
  
  
  line_eq <- function(x1, y1, x2, y2, x) {
    m <- (y2 - y1) / (x2 - x1)
    b <- y1 - m * x1
    return(m * x + b)
  }
  

  
  # Handle both two-point and three-point cases
  result <- data %>%
    mutate(util_bp=P*util_b) %>%
    group_by(source) %>%
    summarise(util_s=sum(util_bp) )
  
  main_plot <- ggplot() +
    geom_line(data = envir, aes(x = x, y = util)) +
    geom_point(data = data, aes(x = belief, y = util_b, group = source, color = source), 
               position = position_dodge(width = 0.05), size = 6) +
    geom_vline(xintercept = q, linetype = "dashed") +
    geom_segment(data = merge(data, result, by = "source"), 
                 aes(x = belief, y = util_b, xend = q, yend = util_s, color = source), 
                 linewidth = 1) +
    geom_point(data = data, aes(x = q, y = util(q)), size=6) +
    scale_color_viridis_d(option = "plasma", direction = -1) +
    
    labs(title = "",
         x = "belief",
         y = "utility", 
         color = "Chosen Source Portfolio", 
         fill = NULL) +
    theme_minimal() +
    theme(text = element_text(size = 20), legend.position = "bottom", panel.grid=element_blank())
  
  # Zoomed-in plot
  zoom_plot <- ggplot() +
    geom_vline(xintercept = q, linetype = "dashed") +
    geom_point(data = data, aes(x = belief, y = util_b, group = source, color = source), size = 4) +
    geom_segment(data = merge(data, result, by = "source"), 
                 aes(x = belief, y = util_b, xend = q, yend = util_s, color = source), 
                 linewidth = 1) +
    coord_cartesian(xlim = c(0.54, 0.56), ylim = c(0.57, 0.63)) +  # Zoomed-in region
    theme_minimal() +
    scale_color_viridis_d(option = "plasma", direction = -1) +
    
    theme(legend.position = "none",  # Hide legend in inset
          axis.title = element_blank(),
      
          plot.background = element_rect(fill = "white", color = "black"), panel.grid=element_blank() )
  
  # Combine main plot and inset
  main_plot +
    annotation_custom(ggplotGrob(zoom_plot), xmin = 0.8, xmax =1.05, ymin = 0.5, ymax = 0.75)

  e = 0.8
  e_nu = 0.3
  q=0.6 # alphaalpha
  
  
  data <- cbind(q=rep(q,16), 
                source=c(rep("αα",2), 
                         rep("αβ",3),
                         rep("ββ",2),
                         rep("νν",3),
                         rep("αν",3),
                         rep("βν",3)
                ), 
                signal=c("AαAα","AαBα/BαAα/BαBα", 
                         "AαAβ","AαBβ","BαBβ",
                         "AβAβ/AβBβ/BβAβ", "BβBβ",
                         "AνAν","AνBν/BνAν", "BνBν", 
                         "AαAν","AαBν","BαAν/BαBν", 
                         "AβAν/AβBν","BβAν","BβBν"
                ), 
                P=c(q+e^2*(1-q),1-q-e^2*(1-q),
                    (1-e)*q, e, (1-e)*(1-q),
                    1-e^2*q-(1-q), e^2*q+(1-q),
                    (1-e_nu)^2*q+e_nu^2*(1-q),2*(1-e_nu)*e_nu, e_nu^2*q+(1-e_nu)^2*(1-q),
                    (1-e_nu)*q+e*e_nu*(1-q), e_nu*q+e*(1-e_nu)*(1-q), (1-e)*e_nu*(1-q)+(1-e_nu)*(1-e)*(1-q),
                    (1-e)*(1-e_nu)*q+(1-e)*e_nu*q, (1-e_nu)*e*q+(1-q)*e_nu, e*e_nu*q+(1-q)*(1-e_nu)
                ),
                belief=c(q/(q+e^2*(1-q)),0,
                         1, q, 0,
                         1, e^2*q/(e^2*q+(1-q)),
                         (1-e_nu)^2*q/((1-e_nu)^2*q+e_nu^2*(1-q)),q, e_nu^2*q/(e_nu^2*q+(1-e_nu)^2*(1-q)),
                         (1-e_nu)*q/((1-e_nu)*q+e*e_nu*(1-q)), e_nu*q/(e_nu*q+e*(1-e_nu)*(1-q)), 0,
                         1, (1-e_nu)*e*q/((1-e_nu)*e*q+(1-q)*e_nu), e*e_nu*q/(e*e_nu*q+(1-q)*(1-e_nu))
                )
                
                
  )%>%
    data.frame()  %>%
    mutate(P=as.numeric(P), belief=as.numeric(belief), q=as.numeric(q), util_b=util(belief),source=factor(source, levels= c("αα","αν", "αβ", "νν",  "βν","ββ")))
  
  envir <-data.frame(
    x=seq(0,1, by=0.01)
  ) %>% mutate(
    util=util(x)
  )
  
  
  line_eq <- function(x1, y1, x2, y2, x) {
    m <- (y2 - y1) / (x2 - x1)
    b <- y1 - m * x1
    return(m * x + b)
  }
  
  result <- data %>%
    mutate(util_bp=P*util_b) %>%
    group_by(source) %>%
    summarise(util_s=sum(util_bp) )
  
  main_plot <- ggplot() +
    geom_line(data = envir, aes(x = x, y = util)) +
    geom_point(data = data, aes(x = belief, y = util_b, group = source, color = source), 
               position = position_dodge(width = 0.05), size = 6) +
    geom_vline(xintercept = q, linetype = "dashed") +
    geom_segment(data = merge(data, result, by = "source"), 
                 aes(x = belief, y = util_b, xend = q, yend = util_s, color = source), 
                 linewidth = 1) +
    scale_color_viridis_d(option = "plasma", direction = -1) +
    
    labs(title = "",
         x = "belief",
         y = "utility", 
         color = "Chosen Source Portfolio", 
         fill = NULL) +
    theme_minimal() +
    geom_point(data = data, aes(x = q, y = q), size=6) +
    theme(text = element_text(size = 20), legend.position = "bottom", panel.grid=element_blank())
  
  # Zoomed-in plot
  zoom_plot <- ggplot() +
    geom_vline(xintercept = 0.87, linetype = "dashed") +
    geom_point(data = data, aes(x = belief, y = util_b, group = source, color = source), size = 4) +
    geom_segment(data = merge(data, result, by = "source"), 
                 aes(x = belief, y = util_b, xend = q, yend = util_s, color = source), 
                 linewidth = 1) +
    coord_cartesian(xlim = c(0.78, 0.82), ylim = c(0.81, 0.88)) +  # Zoomed-in region
    theme_minimal() +
    scale_color_viridis_d( option = "plasma",direction = -1) +
    
    theme(legend.position = "none",  # Hide legend in inset
          axis.title = element_blank(),
          axis.text = element_blank(),
          plot.background = element_rect(fill = "white", color = "black"), panel.grid=element_blank() )
  
  # Combine main plot and inset
  main_plot +
    annotation_custom(ggplotGrob(zoom_plot), xmin = 0.85, xmax =1.05, ymin = 0.5, ymax = 0.75)
  
