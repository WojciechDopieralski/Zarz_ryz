
library(shiny)
library(openxlsx)
library(PerformanceAnalytics)
library(e1071)
# library(httr)
# library(rio)

#Wczytanie zbioru danych z pliku excel


# zarz_ryz <- read.xlsx(xlsxFile = "01_nstacj_waluty_STACJONARNE.xlsx", 
#                       sheet = "ceny walut", 
#                        startRow = 1)


#dostosowanie zbioru danych do dalszej analiy 

zarz_ryz <- zarz_ryz[-c(1:2),]      #usuwam wiersze 1 i 2
zarz_ryz <- as.data.frame(sapply(zarz_ryz, as.numeric))   #zmiana typu danych na numeric (ze wzgledu na pierwsze wiesze, cale kolumny ustawily sie jako zmienna tekstowa)
zarz_ryz$`data/waluta` <- as.Date(zarz_ryz$`data/waluta`,origin = "1900-01-01") #Ustawienie pierwszej kolumny jako typ danych "data"

#Wyciagniecie wszystkich walut i lat

waluty <- colnames(zarz_ryz[2:length(zarz_ryz)])

daty <- unique(substring(zarz_ryz$`data/waluta`,1,4))
daty <- as.numeric(daty)



ui <- fluidPage(
   
   # Application title
  
   titlePanel("Kursy walut - analiza"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "year",
                     label = "Pick year:",
                     choices = daty,
                     selected = 2017),
      
        selectInput(inputId = "curr",
                    label = "Pick currency:",
                    choices = waluty,
                    selected = "USD")
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         textOutput("variation")
      )
   )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
   
   output$variation <- renderText ({
     
     x <- input$curr
     y <- input$year
     variation <- var(zarz_ryz$USD, na.rm = TRUE)
     paste("Wartosc wariancji jest rowna: ",variation, " w roku: ", y)

     
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

