#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)

con_dt = read.csv("/Applications/STAT\ 5293/Final\ Project\ Data/782ec845-2135-4698-8881-b38823e533bf\ 2/data/1_Spatial_dataset.csv")

con = con_dt %>% 
    slice(-c(1:19)) %>%
    select(c("Code", "Latitude", "Longitude", "Date_of_activity_measurement", "X137Cs", "X134Cs"))

mo2Num = function(x) match(tolower(x), tolower(month.abb))
con$`date` = paste0(substr(con$Date_of_activity_measurement, 1, 2), "/", mo2Num(substr(con$Date_of_activity_measurement, 4, 6)), "/", substr(con$Date_of_activity_measurement, 8, 9))
con$`date` = as_date(con$`date`, format = '%d/%m/%y')

final = con %>%
    select(!Date_of_activity_measurement) %>% 
    pivot_longer(cols=!c("Code", "Latitude", "Longitude", "date"), names_to="radionuclides", values_to="contamination") %>% 
    drop_na()

selectDate = function(month, type){
    final[(final$date == month & final$radionuclides == type), ]
}

dates = seq(as.Date("1995-07-28"), as.Date("2002-04-20"), by = "day")
mydates = unique(final$date)
diff = dates[!dates %in%  mydates]

# Define UI for application that draws a histogram
ui <- fluidPage(
    leafletOutput("map", width="100%", height=620),
     absolutePanel(
        # call helper functions for more details about the parameters
        id = "choices", class = "panel panel-default",
        top = 50, left = 25, width = 250, fixed=FALSE,
        draggable = TRUE, height = "auto",
        # add text 
        tags$h1("Please Select",
                align = "left", 
                style = "font-size:30px"),
        dateInput("date",
                  label = "Date",
                  value = "1995-07-28",
                  min = "1995-07-28",
                  max = "2002-04-20",
                  datesdisabled = diff),
        tags$h2("Type of Radionuclide",
                align = "left",style = "font-size:15px"),
        # add checkbox
        checkboxInput("Cs134",
                      label = "Caesium-134 (Cs-134)",
                      value=FALSE),
        checkboxInput("Cs137",
                      label = "Caesium-137 (Cs-137)", 
                      value = FALSE),
        style = "opacity: 0.80")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$map <- renderLeaflet({
        leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
            # since we will have different markers for different crimes, we change to a map that contains fewer colors for a better visualization
            addProviderTiles("CartoDB.Voyager") %>%
            setView(lng = 30.04194, lat = 51.37611, zoom = 10)
    })
    
    df_react_34 <- reactive({
        selectDate(input$date,"X134Cs")
    })
    
    df_react_37 <- reactive({
        selectDate(input$date,"X137Cs")
    })
    
    # set colors of the markers for each type
    ic_1 = awesomeIcons(icon = 'ios-close',
                        library = 'ion', markerColor = "lightgray")
    ic_2 = awesomeIcons(icon = 'ios-close',
                        library = 'ion', markerColor = "beige")
    
    observe({
        leafletProxy("map", data = final) %>%
            clearShapes() %>%
            clearMarkers() %>%
            addProviderTiles("CartoDB.Voyager") %>%
            setView(lng = 30.04194, lat = 51.37611, zoom = 10)
        
        # if a date is selected
        if (input$date){
            if (input$Cs134){
                if (nrow(df_react_34()) != 0) {
                    leafletProxy("map", data = df_react_34()) %>%
                        # add markers that are self-designed
                        addCircleMarkers(
                            lng=~Longitude,
                            lat=~Latitude,
                            color = ~ifelse(contamination > 100, "red", "green"),
                            stroke = FALSE,
                            fillOpacity = 0.5,
                            radius = 10
                        )   %>%
                        addAwesomeMarkers(
                            lng=~Longitude,
                            lat=~Latitude,
                            icon=ic_1,
                            label=~as.character((contamination))         
                        )
                }
            }
            
            if (input$Cs137){
                if (nrow(df_react_37()) != 0) {
                    leafletProxy("map", data = df_react_37()) %>%
                        addCircleMarkers(
                            lng=~Longitude,
                            lat=~Latitude,
                            color = ~ifelse(contamination > 1119.508, "red", "green"),
                            stroke = FALSE,
                            fillOpacity = 0.5,
                            radius = 30
                        )  %>%
                        addAwesomeMarkers(
                            lng=~Longitude,
                            lat=~Latitude,
                            icon=ic_2,
                            label=~as.character((contamination))         
                        )
                }

            } 
        }
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
