library(rgdal)
library(ggplot2)
library(maptools)
library(gpclib)
library(igraph)


    #source("C:\\Users\\Andrew\\R-Booklist\\booklist.r")
    
    
multiplot <- function(..., plotlist=NULL, cols) {
    require(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # Make the panel
    plotCols = cols                          # Number of columns of plots
    plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols

    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
    vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
        curRow = ceiling(i/plotCols)
        curCol = (i-1) %% plotCols + 1
        print(plots[[i]], vp = vplayout(curRow, curCol ))
    }

}

world_map <- function(){
    gpclibPermit()

    world.map <- readOGR(dsn="C:\\Users\\Andrew\\Downloads\\TM_WORLD_BORDERS_SIMPL-0.3", layer="TM_WORLD_BORDERS_SIMPL-0.3")
    world.ggmap <- fortify(world.map, region = "NAME")

    books <- read.csv("C:\\Users\\Andrew\\Downloads\\Book List Revised - Book List.csv", header = TRUE, sep=",", encoding="UTF-8")
    books <- books[-101,]
    nat <- data.frame(table(id=books$Country))

    n <- length(unique(world.ggmap$id))

    df <- data.frame(id = unique(world.ggmap$id))

    all <- merge(nat, df, all=TRUE)

    #jpeg('C:\\Users\\Andrew\\psychic-octo-hipster\\app\\assets\\images\\worldmap.jpg')
    
    ggplot(all, aes(map_id = id)) + geom_map(aes(fill = Freq), map =world.ggmap) + expand_limits(x = world.ggmap$long, y = world.ggmap$lat) + scale_fill_gradient(high = '#333366', low = '#CCCCFF', guide = "colorbar") + scale_colour_hue(h = c(120, 240))
    #dev.off()
}

plot_read <- function(){
    books <- read.csv("C:\\Users\\Andrew\\Downloads\\Book List Revised - Book List.csv", header = TRUE, sep=",", encoding="UTF-8")
    books <- books[-101,]
    a_read <- books[books$Andrew == "Yes",]
    d_read <- books[books$Dan == "Yes",]
    
    jpeg('C:\\Users\\Andrew\\psychic-octo-hipster\\app\\assets\\images\\boxplot.jpg')
    
    a_plot <- ggplot(a_read, aes(x=Andrew, y=Year)) + geom_boxplot(outlier.colour = "green") + coord_flip() + xlab("") + opts(title="Andrew")
    d_plot <- ggplot(d_read, aes(x=Dan, y=Year)) + geom_boxplot(outlier.colour = "green") + coord_flip() + xlab("") + opts(title="Dan")
    
    sidebysideplot <- multiplot(a_plot, d_plot, cols=1)
    dev.off()
}

plot_read_andrew <- function(){
    books <- read.csv("C:\\Users\\Andrew\\Downloads\\Book List Revised - Book List.csv", header = TRUE, sep=",", encoding="UTF-8")
    books <- books[-101,]
    a_read <- books[books$Andrew == "Yes",]

    ggplot(a_read, aes(x=Andrew, y=Year)) + geom_boxplot(outlier.colour = "green") + coord_flip() + xlab("") + opts(title="Novels read by Year Published")
}

plot_influencers_new <- function(){
    infl <- read.csv("C:\\Users\\Andrew\\Downloads\\Book List Revised - Influencers.csv", header = TRUE, sep=",", encoding="UTF-8")
    g <- graph.data.frame(infl)
    adj <- get.adjacency(g)
    authors <- adj@Dimnames[[1]]
    #pdf(file='C:\\Users\\Andrew\\influencers.pdf')
    #plot.igraph(g, vertex.label=authors, vertex.label.cex=0.5, vertex.size=8, vertex.label.family="sans-serif", vertex.frame.color="NA", edge.curved=TRUE, layout=layout.fruchterman.reingold, edge.arrow.size=0.2)
    plot.igraph(g, vertex.label=authors, vertex.label.cex=0.5, vertex.size=15, vertex.frame.color="NA", layout=layout.fruchterman.reingold, edge.arrow.size=0.2, main="Influencing Authors")
    #dev.off()
}