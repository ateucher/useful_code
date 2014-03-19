multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, title=NULL, colwidths=NULL) {
  # Multiple plot function
  #
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  # - colwidths: a vector of relative column widths eg. c(3,2)
  #
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  # 
  # Credit: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
  
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (!is.null(title)) { # Add a narrow row at the top for the title
    layout <- rbind(rep(0,ncol(layout)),layout)
    rowheights <- c(0.5, rep(5,nrow(layout)-1))
  } else {
    rowheights <- rep(5,nrow(layout))
  }
  
  if (is.null(colwidths)) {
    colwidths=unit(rep_len(1, cols), "null")
  } else {
    colwidths = colwidths / sum(colwidths)
  }
  
  if (numPlots==1) {
    
    print(plots[[1]] + labs(title=title))
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout), 
                                               widths=colwidths, 
                                               heights=unit(rowheights, "null"))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
    
    if (!is.null(title)) {
      grid.text(title, vp = viewport(layout.pos.row = 1
                                     , layout.pos.col = 1:ncol(layout)))
    }
    
  }
}
