#### Bubble Chart

df<-read.csv("Artists_TS_B.csv") 
df$artist_name<-as.factor(df$artist_name)


# Aggregate data to get average values for each year and artist
agg_df <- aggregate(cbind(danceability, speechiness, acousticness, liveness) ~ album_release_year + artist_name, data = df, FUN = mean)

# Define custom colors for Ed Sheeran and Taylor Swift
custom_colors <- c("Ed Sheeran" = "blue", "Taylor Swift" = "red")

# Plot danceability bubble chart
fig1 <- plot_ly(agg_df, x = ~album_release_year, y = ~danceability, 
                size = ~danceability,
                color = ~artist_name, 
                type = 'scatter', mode = 'markers', 
                marker = list(sizemode = 'diameter', sizeref = 1.6),
                colors = custom_colors,
                legendgroup = ~artist_name ,
                showlegend = FALSE
) |>
  layout(xaxis = list(showgrid = FALSE),yaxis = list(showgrid = FALSE))



# Plot speechiness bubble chart
fig2 <- plot_ly(agg_df, x = ~album_release_year, y = ~speechiness, 
                size = ~speechiness, color = ~artist_name, 
                type = 'scatter', mode = 'markers', 
                marker = list(sizemode = 'diameter', sizeref = 1.6, line = list(width = 2)), 
                colors = custom_colors, legendgroup = ~artist_name,
                showlegend = FALSE) |>
  layout(xaxis = list(showgrid = FALSE),yaxis = list(showgrid = FALSE)) 

# Plot acousticness bubble chart
fig3 <- plot_ly(agg_df, x = ~album_release_year, y =  ~acousticness, 
                size = ~acousticness, color = ~artist_name, 
                type = 'scatter', mode = 'markers', 
                marker = list(sizemode = 'diameter', sizeref = 1.6, line = list(width = 2)), 
                colors = custom_colors,
                legendgroup = ~artist_name,
                showlegend = FALSE
) |>
  layout(xaxis = list(showgrid = FALSE, zeroline=FALSE),yaxis = list(showgrid = FALSE, zeroline=FALSE)) 

# Plot liveness bubble chart
fig4 <- plot_ly(agg_df, x = ~album_release_year, y = ~liveness, 
                size = ~liveness, color = ~artist_name, 
                type = 'scatter', mode = 'markers', 
                marker = list(sizemode = 'diameter', sizeref = 1.6, line = list(width = 2)), 
                colors = custom_colors,
                legendgroup = ~artist_name,
                showlegend = TRUE
) |>
  layout(xaxis = list(showgrid = FALSE),yaxis = list(showgrid = FALSE)) 

# Combine subplots
#subplot(fig1, fig2, fig3, fig4, nrows = 2, shareX = TRUE)

# fig <- make_subplots(rows=2,
#                     cols=2,
#                     #print_grid=True,
#                     vertical_spacing=0.1,
#                     horizontal_spacing=0.085,
#                     x_title='Your master x-title',
#                     y_title='Your master y-title')

fig <- subplot(fig1, fig2, fig3, fig4, nrows = 2, shareX=TRUE, titleX=FALSE)

#fig <- subplot(fig1, fig2, nrows=2, shareX = TRUE, showlegend=TRUE)
#%>% layout(title = "Average Music Properties of Beyoncé and Taylor Swift's Songs")

annotations <- list(
  list(
    x = 0.2,
    y = 1.0,
    text = "Danceability",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.8,
    y = 1,
    text = "Speechiness",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.2,
    y = 0.45,
    text = "Acousticness",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x = 0.8,
    y = 0.45,
    text = "Liveness",
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  ),
  list(
    x=0.5,
    y=-0.035,
    text="Release Year",
    xref = "paper",
    yref = "paper",
    xanchor = "bottom",
    yanchor = "top",
    showarrow = FALSE,
    font=list(size=16)
  )
)

fig <- fig |> layout(annotations = annotations)
fig