library("readxl")
library("tidyverse")
library("vegan")
library("RColorBrewer")
library("ggrepel")
library("ggalt")
library("ggforce")
library("concaveman")
set.seed(42)

## reading
dataValues <- read_excel("cultvalueorientationscores.xls")

# subset and log transformation of values
NMDS.log <- log(dataValues[,c(5:11)])
sol <- metaMDS(NMDS.log)

# simple plot to check that everything is fine
ord <- metaMDS(dataValues[,5:11])
plot(ord, type = "t")

# fitting
vec.sp<-envfit(sol$points, NMDS.log, perm=1000)
vec.sp.df<-as.data.frame(vec.sp$vectors$arrows*sqrt(vec.sp$vectors$r))
vec.sp.df$species<-c("Гармония",
                     "Принадлежность",
                     "Иерархия",
                     "Мастерство",
                     "Автономия (Афф.)",
                     "Автономия (Инт.)",
                     "Равноправие")

# dataset for plotting
NMDS <- data.frame(MDS1 = sol$points[,1], MDS2 = sol$points[,2], row.names = dataValues$label)
NMDS$profile <- dataValues$profile

# choose seven colors for seven values
#newColors <- brewer.pal(n = 7, name = "Set1")

# plot
ggplot(data = NMDS, aes(MDS1*10, MDS2*10)) + 
  #stat_ellipse(geom = "polygon", alpha = 0.5, aes(fill = as.factor(profile))) +
  geom_mark_hull(aes(fill = as.factor(profile)), alpha = 0.08, colour = NA,
                 expand = unit(3, "mm"), 
<<<<<<< HEAD
                 concavity = 1) +
  #geom_encircle(alpha = 0.5, aes(fill = as.factor(profile))) +
  geom_point(aes(color = as.factor(profile)), size = 3) +
  geom_segment(data=vec.sp.df,
               aes(x = 0,xend = MDS1*1.5,y = 0, yend= MDS2*1.5),
               lineend = "butt",
               linejoin = "mitre",
               size = 0.5, 
               arrow = arrow(length = unit(0.2, "cm")),
               colour="grey"
  ) + 
=======
                  concavity = 1) +
  #geom_encircle(alpha = 0.5, aes(fill = as.factor(profile))) +
  geom_point(aes(color = as.factor(profile)), size = 3) +
  geom_segment(data=vec.sp.df,
    aes(x = 0,xend = MDS1*1.5,y = 0, yend= MDS2*1.5),
    lineend = "butt",
    linejoin = "mitre",
    size = 0.5, 
    arrow = arrow(length = unit(0.2, "cm")),
    colour="grey"
    ) + 
>>>>>>> 1c7ad456b8f16d86ab265b39c3ff263daf5abbf3
  geom_label_repel(aes(label = dataValues$label), label.size = NA, fill = NA) + 
  #geom_polygon(aes(color = as.factor(profile)), alpha = 0.1) +
  geom_text_repel(data = vec.sp.df,aes(x = MDS1*1.6, y = MDS2*1.6, label = species),size = 5) +
  theme_void() +
  theme(aspect.ratio = 1, legend.position = "none") 

## save as png
ggsave(
  "plotSchwartzMap.png",
  device = "png",
  scale = 2,
  # because I like when I can zoom my plot a lot)
  dpi = 500
)



