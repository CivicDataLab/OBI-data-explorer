prepare_dumbell <- function(graph_type){
  if(graph_type == 'top'){
    data_for_graph <- data_for_analysis %>% arrange(`Budget Difference`) %>% tail(10)  
  } else {
    data_for_graph <- data_for_analysis %>% arrange(`Budget Difference`) %>% head(10)   
  }
  
  graph_for_analysis <- data_for_graph
  graph_for_analysis$Particulars <- factor(graph_for_analysis$Particulars, levels = as.character(graph_for_analysis$Particulars))
  
  grapher <- ggplot(graph_for_analysis, aes(x=`Actual 2018-2019 Total`, xend=`Budget 2020-2021 Total`, y=Particulars, group=Particulars)) + 
    geom_dumbbell(color="#0e668b", 
                  size=0.75, 
                  point.colour.l="#0e668b") + 
    scale_x_continuous() + 
    labs(x=NULL, 
         y=NULL, 
         title="Budget Allocation (In Crores): 2018/19 vs 2020/21", 
         subtitle=stringr::str_to_title(glue::glue("{graph_type} 20 departments in terms of increase in budget allocation", 
         caption="Source: OpenBudgetsIndia"))) +
    theme(plot.title = element_text(hjust=0.5, face="bold"),
          plot.background=element_rect(fill="#f7f7f7"),
          panel.background=element_rect(fill="#f7f7f7"),
          panel.grid.minor=element_blank(),
          panel.grid.major.y=element_blank(),
          panel.grid.major.x=element_line(),
          axis.ticks=element_blank(),
          legend.position="top",
          panel.border=element_blank())
 return(grapher) 
}