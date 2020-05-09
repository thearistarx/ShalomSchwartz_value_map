library("plotly")
library("readxl")

# reading
dataValues <- read_excel("cultvalueorientationscores.xls")

# 3d plot
plot_ly(data = dataValues, x = ~har_mas, y = ~ega_hie, z = ~aut_emb, color = as.factor(dataValues$profile), 
        text = ~region, mode = "markers+text",
        textposition = "top center", 
        hovertemplate = paste(
          "<b>%{text}</b><br><br>",
          "Harmon/Mastery: %{y:.2f}<br>",
          "Egalit/Hierarchy: %{x:.2f}<br>",
          "Autonom/Embed: %{z:.2f}<br>",
          "<extra></extra>")) %>%
  layout(title = "Values Map",
    scene = list(
    xaxis = list(title = "Harmony - Mastery"), 
    yaxis = list(title = "Egalitar - Hierarchy"), 
    zaxis = list(title = "Autonomy - Embedded")
    )) %>%
  add_markers() %>%
  add_text()




