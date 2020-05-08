library("plotly")
library("readxl")
library("vegan")
set.seed(42)

# reading
dataValues <- read_excel("cultvalueorientationscores.xls")

# subset and log transformation of values
NMDS.log <- log(dataValues[,c(5:11)])
sol <- metaMDS(NMDS.log, k = 3)

# fitting
vec.sp<-envfit(sol$points, NMDS.log, perm=1000, choices = c(1:3))
vec.sp.df<-as.data.frame(vec.sp$vectors$arrows*sqrt(vec.sp$vectors$r))
vec.sp.df$species<-c("Гармония",
                     "Принадлежность",
                     "Иерархия",
                     "Мастерство",
                     "Автономия (Афф.)",
                     "Автономия (Инт.)",
                     "Равноправие")

# dataset for plotting
NMDS <- data.frame(MDS1 = sol$points[,1], MDS2 = sol$points[,2], MDS3 = sol$points[,3], row.names = dataValues$label)
NMDS$profile <- dataValues$profile


plot_ly(data = NMDS, x = ~MDS1*10, y = ~MDS2*10, z = ~MDS3*10, color = as.factor(NMDS$profile), 
        text = row.names(NMDS), mode = "lines+merkers+text") %>%
  layout(title = "Values Map",
    scene = list(
    xaxis = list(title = "X"), 
    yaxis = list(title = "Y"), 
    zaxis = list(title = "Z")
    )) %>%
  add_markers() %>%
  add_text(textposition = "top center") %>%
  add_trace(data = vec.sp.df, 
            x = c(0, ~MDS1[1]), 
            y = c(0, ~MDS2[1]), 
            z = c(0, ~MDS3[1]),
            type = "scatter3d", mode = "lines+text", name = "Гармония", line = list(color = "black"), 
            text = c(NA, "Гармония"), showlegend = FALSE, inherit = FALSE) %>%
  add_trace(data = vec.sp.df, 
            x = c(0, ~MDS1[2]), 
            y = c(0, ~MDS2[2]), 
            z = c(0, ~MDS3[2]),
            type = "scatter3d", mode = "lines+text", name = "Принадлежность", line = list(color = "black"), 
            text = c(NA, "Принадлежность"), showlegend = FALSE, inherit = FALSE) %>%
  add_trace(data = vec.sp.df, 
            x = c(0, ~MDS1[3]), 
            y = c(0, ~MDS2[3]), 
            z = c(0, ~MDS3[3]),
            type = "scatter3d", mode = "lines+text", name = "Иерархия", line = list(color = "black"), 
            text = c(NA, "Иерархия"), showlegend = FALSE, inherit = FALSE) %>%
  add_trace(data = vec.sp.df, 
            x = c(0, ~MDS1[4]), 
            y = c(0, ~MDS2[4]), 
            z = c(0, ~MDS3[4]),
            type = "scatter3d", mode = "lines+text", name = "Мастерство", line = list(color = "black"), 
            text = c(NA, "Мастерство"), showlegend = FALSE, inherit = FALSE) %>%
  add_trace(data = vec.sp.df, 
            x = c(0, ~MDS1[5]), 
            y = c(0, ~MDS2[5]), 
            z = c(0, ~MDS3[5]),
            type = "scatter3d", mode = "lines+text", name = "Автономия (Афф.)", line = list(color = "black"), 
            text = c(NA, "Автономия (Афф.)"), showlegend = FALSE, inherit = FALSE) %>%
  add_trace(data = vec.sp.df, 
            x = c(0, ~MDS1[6]), 
            y = c(0, ~MDS2[6]), 
            z = c(0, ~MDS3[6]),
            type = "scatter3d", mode = "lines+text", name = "Автономия (Инт.)", line = list(color = "black"), 
            text = c(NA, "Автономия (Инт.)"), showlegend = FALSE, inherit = FALSE) %>%
  add_trace(data = vec.sp.df, 
            x = c(0, ~MDS1[7]), 
            y = c(0, ~MDS2[7]), 
            z = c(0, ~MDS3[7]),
            type = "scatter3d", mode = "lines+text", name = "Равноправие", line = list(color = "black"), 
            text = c(NA, "Равноправие"), showlegend = FALSE, inherit = FALSE)




