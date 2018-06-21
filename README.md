# Cyence
## Tips
1. When publishing, all files in the directory in which ui.R and server.R are stored will be exported together, so we don't need to connect to database every time. We can pull the data into the directory first, then read them when running the app.

2. **`guides(colour = FALSE)`** option can suppress the legend that is automatically shown if you specify `colour = some variable` before in `aes()`.
