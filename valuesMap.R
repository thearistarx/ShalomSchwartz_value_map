library("readxl")
library("tidyverse")
library("vegan")
# grid на всякий случай
library("grid")
library("RColorBrewer")
library("ggrepel")
library("ggalt")

dataSchwartz <- read_excel("cultvalueorientationscores0.xls")
dataS1 <- dataSchwartz


NMDS.log <- log(dataS1[,c(5:11)]+1)
sol <- metaMDS(NMDS.log)

# плотим текст в плоте, чтобы потестить
ord <- metaMDS(dataS1[,5:11])
plot(ord, type = "t")

#что-то посчиталось
vec.sp<-envfit(sol$points, NMDS.log, perm=1000)
vec.sp.df<-as.data.frame(vec.sp$vectors$arrows*sqrt(vec.sp$vectors$r))
vec.sp.df$species<-rownames(vec.sp.df)

#рисуем человеческим ggplot
NMDS <- data.frame(MDS1 = sol$points[,1], MDS2 = sol$points[,2], row.names = dataS1$label)
NMDS$profile <- dataS1$profile

newColors <- brewer.pal(n = 7, name = "Set3")

ggplot(data = NMDS, aes(MDS1*10, MDS2*10)) + 
  #stat_ellipse(geom = "polygon", alpha = 0.5, aes(fill = as.factor(profile))) +
  geom_encircle(alpha = 0.5, aes(fill = as.factor(profile))) +
  geom_segment(data=vec.sp.df,aes(x=0,xend=MDS1,y=0,yend=MDS2), arrow = arrow(length = unit(0.1, "cm")), colour=newColors) + 
 geom_label_repel(aes(label = dataS1$label)) + 
  geom_point(aes(color = as.factor(profile))) +
  #geom_polygon(aes(color = as.factor(profile)), alpha = 0.1) +
  geom_text_repel(data=vec.sp.df,aes(x=MDS1,y=MDS2,label=species),size=5) +
  theme_void() +
  theme(aspect.ratio = 0.5) 

ggsave(
  "plotSchwartz.png",
  device = "png",
  scale = 2,
  dpi = 500
)



