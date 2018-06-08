########## PLOTLY BASICS
library(plotly)
library(ggplot2)

#### ggplot2 mit plotly

## Labeling von Punkten
mtcars$name <- rownames(mtcars)
ggplot(mtcars,aes(hp,disp,
                  text = paste("Car:", name,"\nCyl:",cyl))) + geom_point()
ggplotly()
# alternativ p <- ggplot(...); ggplotly(p)



#### Natives Plotly
### Boxplot
plot_ly(mtcars, x = ~hp, color = ~cyl, type = "box")
# Punkte mit Jitter hinzu
plot_ly(mtcars, x = ~hp, color = ~cyl, type = "box", 
        boxpoints = "all", jitter = 0.3, pointpos = -1.8)
# Gruppiert
plot_ly(mtcars, x = ~hp, y=~am, color = ~cyl, type = "box")  %>%
  layout(boxmode = "group")


p <- plot_ly(ggplot2::diamonds, x = ~cut, y = ~price, color = ~clarity, type = "box") %>%
  layout(boxmode = "group")

### Histogram
# Normalisiert: histnorm = "probability"
plot_ly (mtcars, x=~hp, type ='histogram')

## Überlagert
plot_ly(alpha = 0.6) %>%
  add_histogram(x = ~rnorm(500)) %>%
  add_histogram(x = ~rnorm(500) + 1) %>%
  layout(barmode = "overlay")

### Scatterplot 
plot_ly(mtcars, x = ~hp, y = ~disp) 

## Farben
plot_ly(mtcars, x = ~hp, y = ~disp, color=~cyl) 
plot_ly(mtcars, x = ~hp, y = ~disp, color=~cyl, colors = "Set1") # brewer 
# Vordefinierte Farben
pal <- c("steelblue", "sandybrown", "forestgreen")
plot_ly(mtcars, x = ~hp, y = ~disp, color=~cyl, colors = pal)

## Größe
plot_ly(mtcars, x = ~hp, y = ~disp, size=~as.numeric(cyl))

## Alpha
cor(select_if(mtcars,is.numeric))
subplot(
  plot_ly(mtcars, x = ~hp, y = ~disp, name = "default"),
  plot_ly(mtcars, x = ~hp, y = ~disp) %>% 
    add_markers(alpha = 0.2, name = "alpha")
)

## Datenpunkt Labels
mtcars$name <- rownames(mtcars)
plot_ly(mtcars, x = ~hp, y = ~disp, text = ~paste("Car: ", name, '\nCyl:',cyl))



