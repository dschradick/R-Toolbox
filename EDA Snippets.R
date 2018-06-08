########## EDA SNIPPETS

## Mapping von Values
# => um daten lesbarer zu machen
mtcars$am <- plyr::mapvalues(mtcars$am, from=c(0,1), to=c('manual','automatic'))

## Outlier IQR basierend entfernen 
remove.outliers <- function(df,x) {
  x.without.outliers <- df[!df[x,] %in% boxplot.stats(df[,x])$out]
  return(x.without.outliers)
}

remove.outliers.q <- function(df,field,low=0,high=1) {
  df.without.outliers <- subset(df, df[,field] > quantile(df[,field],low) &
                                    df[,field] < quantile(df[,field],high))
  return(df.without.outliers)
}

# anstatt outlier auf df entfernen => xlim(0,max)

#x <-rnorm(1000) ; range(x)
##x <- remove.outliers(x) ; range(x)

# Einfache Korrelationsmatrix
library(corrplot)
library(RColorBrewer)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(Boston), method="color", col=brewer.pal(n=8, name="RdYlBu"),  
         type='full', order="hclust", 
         addCoef.col = "black", number.cex = .7, 
         tl.col="black", tl.srt=45, tl.cex = .9)


# Korrelationsmatrix mit p-werten
library(Hmisc)
res2<-rcorr(as.matrix(Boston))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(res2$r, method="color", col=col(100),  
         type='full', order="hclust", 
         addCoef.col = "black", number.cex = .7, 
         tl.col="black", tl.srt=45, tl.cex = .9,
         p.mat = res2$p, sig.level = 0.0001, insig = "blank", 
         addrect = 3
)

# Alle Korrelationen mit einer Variable anzeigen
corr.with.var <- function(df,var){
  cor_mat = rcorr(as.matrix(df[sapply(df,is.numeric)]))$r
  r <- cor_mat[,v]
  n <- names(r)
  r_df <- as.data.frame(r)
  r_df$variable <- n
  r_df
  r_df <- r_df[order(r_df$r,decreasing = F),]
  r_df$variable <- factor(r_df$variable, levels = r_df$variable) 
  r_df$r_type <- ifelse(r_df$r < 0, "negative", "positive")
  r_df$r_type <- factor(r_df$r_type, levels = c('positive','negative')) 
  no_target <- r_df$variable == v
  lim_min <- min(r_df$r)
  lim_max <- max(r_df$r[1:nrow(r_df)-1]) # alle bis auf target
  ggplot(r_df[!no_target,], aes(x=variable, y=r, label=round(r,2))) + 
    geom_bar(stat='identity', aes(fill=r_type), width=.5)  +
    geom_point(stat='identity',fill='black', size=6) +
    geom_text(color="white", size=2, check_overlap = T) +
    scale_fill_manual(name="r",
                      labels = c("r < 0", "r > 0"), 
                      values = c("positive"="#00ba38", "negative"="#f8766d")) + 
    coord_flip() + ylab(paste('Correlation with', var)) + ylim(lim_min,lim_max)
}
#corr.with.var(Boston,'medv')

## Stärkste Korrelationen anzeigen
highest.corr <- function(df,n=10){
  cor_mat = rcorr(as.matrix(df[sapply(df,is.numeric)]))$r
  cor_mat[lower.tri(cor_mat,diag=TRUE)] <- NA  
  cor_mat <- as.data.frame(as.table(cor_mat))  
  cor_mat <- na.omit(cor_mat)                  
  cor_mat <- cor_mat[order(-abs(cor_mat$Freq)),]  
  colnames(cor_mat) <- c("Var1","Var2","r")
  cat('Strongest correlations:\n')
  print(cor_mat[1:n,],row.names=F)
}
#highest.corr(Boston,10)


## Horizontaler gelabelter Barplot
mtcars$name <- rownames(mtcars)
ggplot(data = sample_n(mtcars,5), aes(x = reorder(name,hp), y = hp)) +  
  geom_bar(stat="identity", fill="steelblue", colour="black") +
  geom_text(aes(label=hp), hjust = 1.6, color = "white", size=3.5) +
  coord_flip() + theme_bw(base_size = 8)  +
  labs(title="", x ="car", y = "hp") + theme_minimal()
  


## Line Plot
#mtcars %>% group_by(cyl) %>% summarise(mean_hp = mean(hp)) %>% ungroup -> out

ggplot(mtcars, aes(x = hp, y = qsec, colour = as.factor(cyl))) +       
  geom_line() + geom_point() + theme_bw() 


# Side-by-side ggplots
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
