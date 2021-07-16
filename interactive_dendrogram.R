edges <- readxl::read_excel("edges_vertices.xlsx", sheet = "edges")
vertices <- readxl::read_excel("edges_vertices.xlsx", sheet = "vertices")
vertices$group2 <- factor(vertices$group2)
#Libraries
library(ggraph)
library(igraph)
library(ggiraph)
library(tidyverse)
library(RColorBrewer) 
#Create a graph object
mygraph <- graph_from_data_frame(edges, vertices = vertices)
#Make the plot
sizeplot <- 
  ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  ggraph::geom_edge_diagonal(colour="grey") +
  ggraph::geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=str_wrap(sub('.*-', '', label2), width = 300), 
                             angle = -((-node_angle(x, y)+90)%%180)+90, 
                             hjust='outward', colour=group), size=2.7, alpha=1) +
  ggiraph::geom_point_interactive(aes(x = x*1.07, y=y*1.07, data_id = name, color = group2, size=value, tooltip = str_wrap(paste0(label, ' (n=', value, ')'), width = 50))) +
  ggplot2::scale_colour_manual(values = c("#4c956c", "#d1495b", "#5e548e", "#edae49", "#30638e", "black"), guide = FALSE) +
  ggplot2::scale_size_continuous(range = c(0.1,10)) +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position='bottom', 
    legend.direction='vertical',
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  ggplot2::expand_limits(x = c(-2, 2), y = c(-2, 2)) +
  ggplot2::labs(size = "Number of articles")
sizelegend <- cowplot::get_legend(sizeplot)
colourplot <- 
  ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  ggraph::geom_edge_diagonal(colour="grey") +
  ggraph::geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=str_wrap(sub('.*-', '', label2), width = 300), 
                             angle = -((-node_angle(x, y)+90)%%180)+90, 
                             hjust='outward', colour=group), size=2.7, alpha=1) +
  ggiraph::geom_point_interactive(aes(x = x*1.07, y=y*1.07, data_id = name, color = group2, size=value, tooltip = str_wrap(paste0(label, ' (n=', value, ')'), width = 50))) +
  ggplot2::scale_colour_manual(values = c("#4c956c", "#d1495b", "#5e548e", "#edae49", "#30638e", "black"), guide = 'legend', labels = c("Soil/Geology", "Water", "Air", "Biodiversity", "Societies")) +
  ggplot2::scale_size_continuous(range = c(0.1,10), guide = FALSE) +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position='bottom', 
    legend.direction='vertical',
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  ggplot2::expand_limits(x = c(-2, 2), y = c(-2, 2)) +
  ggplot2::labs(color = 'System') +
  ggplot2::guides(color = guide_legend(override.aes = list(size=5)))
colourlegend <- cowplot::get_legend(colourplot)

blank_p <- patchwork::plot_spacer() + ggplot2::theme_void()
legend <- cowplot::plot_grid(sizelegend, colourlegend,
                             blank_p,
                             ncol = 1,
                             align = "none", 
                             axis = 'l', 
                             rel_heights = c(2.5, 0.2))

affected_factor <- 
  ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  ggraph::geom_edge_diagonal(colour="grey") +
  ggraph::geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=str_wrap(sub('.*-', '', label2), width = 300), 
                             angle = -((-node_angle(x, y)+90)%%180)+90, 
                             hjust='outward', colour=group), size=2.7, alpha=1) +
  ggiraph::geom_point_interactive(aes(x = x*1.07, y=y*1.07, 
                                      data_id = name, 
                                      color = group2, 
                                      size=value, 
                                      tooltip = str_wrap(paste0(label, ' (n=', value, ')'), width = 50),
                                      onclick=paste0('window.open("', link , '")'))) +
  ggplot2::scale_colour_manual(values = c("#4c956c", "#d1495b", "#5e548e", "#edae49", "#30638e", "black"), guide = FALSE) +
  ggplot2::scale_size_continuous(range = c(0.1,10), guide = FALSE) +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position='bottom', 
    legend.direction='vertical',
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  ggplot2::expand_limits(x = c(-2, 2), y = c(-2, 2)) +
  ggplot2::labs(color = 'System')
plot <- cowplot::plot_grid(affected_factor,
                           legend,
                           ncol = 2,
                           align = "h",
                           axis = "t",
                           rel_widths = c(0.7, 0.2))

girafe(ggobj = plot, width_svg = 12, height_svg = 9,
       options = list(opts_tooltip(use_fill = TRUE),
                      opts_sizing(rescale = FALSE),
                      opts_hover_inv(css = "opacity:0.1;"),
                      opts_hover(css = "fill:gray;"),
                      opts_toolbar(saveaspng = FALSE)))

