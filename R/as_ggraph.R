as.ggraph <- function(object){
  # To graph object using tidygraph:
  df <- as.data.frame(object$Edgelist)
  
  if (any(df$directed)){
    stop("Not yet supported for directed graphs")
  }

  tidyG <- tidygraph::tbl_graph(edges = df[,1:2], directed = all(df$directed))
  
  edges <- NULL
  nodes <- NULL
  order <- NULL
  n <- NULL
  
  # Add ID factor:
  tidyG <- tidyG %>% tidygraph::activate(edges) %>% dplyr::mutate(id = as.factor(seq_len(dplyr::n())))
  tidyG <- tidyG %>% tidygraph::activate(nodes) %>% dplyr::mutate(id = as.factor(seq_len(dplyr::n())), label = object$graphAttributes$Nodes$labels)
  
  # Put the edges in the right order:
  tidyG <- tidyG %>% tidygraph::activate(edges) %>% dplyr::mutate(order = order(object$graphAttributes$Graph$edgesort)) %>% dplyr::arrange(order)
  
  # Create plot:
  ggraph::ggraph(tidyG, layout = object$layout) + 
    
    # Edges:
    ggraph::geom_edge_fan(aes(
      color = id,
      edge_width = id,
      edge_linetype = id
    ), show.legend = FALSE) + 
    ggraph::scale_edge_color_manual(values = object$graphAttributes$Edges$color) +
    ggraph::scale_edge_width_manual(values = object$graphAttributes$Edges$width/2) +
    ggraph::scale_edge_linetype_manual(values = sapply(object$graphAttributes$Edges$lty,switch,
                                                       '1' = "solid", '2' = "dashed", '3'= "dotted", '4' = "dotdash", '5' = "longdash", '6' = "twodash"
    )) + 
    
    # Nodes:
    ggraph::geom_node_point(
      aes_string(
        size = "id",
        colour = "id",
        fill = "id"
      ), shape = 21, show.legend = FALSE
    ) + 
    ggplot2::scale_size_manual(values = object$graphAttributes$Nodes$width*2)  + 
    ggplot2::scale_colour_manual(values = object$graphAttributes$Nodes$border.color) +
    ggplot2::scale_fill_manual(values = object$graphAttributes$Nodes$color) +
    
    # Labels:
    ggraph::geom_node_text(
      aes_string(
        label = "label"
      )
    ) +
    
    # Limits:
    ggplot2::xlim(-1-object$plotOptions$mar[2],1+object$plotOptions$mar[3]) + 
    ggplot2::ylim(-1-object$plotOptions$mar[1],1+object$plotOptions$mar[3]) +
    
    # Theme
    ggraph::theme_graph(plot_margin=margin(0,0,0,0))
}