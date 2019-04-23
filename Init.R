library(plumber)

server <- plumb("Services.R") 
server$run(port=7777)