library("plotly")
library("readxl")

# reading
dataValues <- read_excel("cultvalueorientationscores.xls")

# 3d plot
plot_ly(data = dataValues, x = ~har_mas, y = ~ega_hie, z = ~aut_emb, color = as.factor(dataValues$profile), 
        text = ~label, mode = "markers+text",
        textposition = "top center", 
        hovertemplate = paste(
          "<b>%{text}</b><br><br>",
          "Равн/Иерарх: %{y:.2f}<br>",
          "Гарм/Мастер: %{x:.2f}<br>",
          "Авто/Принад: %{z:.2f}<br>",
          "<extra></extra>")) %>%
  layout(title = "Values Map",
    scene = list(
    xaxis = list(title = "Гармония - Мастерство"), 
    yaxis = list(title = "Равноправие - Иерархия"), 
    zaxis = list(title = "Автономия - Принадлежность")
    )) %>%
  add_markers() %>%
  add_text()




