### tempo bar chart

years_to_keep <- c(2014,2017,2019, 2021, 2022, 2023)
filtered_df <- subset(df, album_release_year %in% years_to_keep)

# Aggregate tempo values by artist_name and album_release_year
agg_df <- aggregate(tempo ~ artist_name + album_release_year, data = filtered_df, FUN = mean)

# Create the animated bar plot
fig <- plot_ly(agg_df,
               x = ~artist_name,
               y = ~tempo, 
               color = ~artist_name, 
               type = 'bar',
               frame = ~album_release_year, 
               text = ~artist_name, 
               hoverinfo = "text")

# Add animation button and slider
fig <- fig %>% animation_button(
  x = 1, xanchor = "right", y = 0, yanchor = "bottom"
)
fig <- fig %>% animation_slider(
  currentvalue = list(prefix = "YEAR ", font = list(color="red"))
)

# Add title, x-axis label, and y-axis label
fig <- fig %>% layout(
  title = "Tempo of Ed Sheeran's and Taylor Swift’s songs over the Years",
  xaxis = list(title = "Album Release Year"),
  yaxis = list(title = "Tempo (BPM)")
)

fig
