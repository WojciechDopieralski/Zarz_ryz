
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

min_date <- min(zarz_ryz$`data/waluta`)
max_date <- max(zarz_ryz$`data/waluta`)

#Do skracania

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))



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
                    selected = "USD"),
        
        dateRangeInput(inputId = "date",
                  label = "Select dates:",
                  start = "2013",
                  end = "2014",
                  format = "yyyy",
                  min = min_date, 
                  max = max_date,
                  startview = "year")
      ),
      
      
      
      
      # Show a plot of the generated distribution
      mainPanel(
         textOutput("variation"),
         textOutput("variation_int"),
         textOutput("stan_dev"),
         textOutput("stan_dev_int"),
         textOutput("coeff_of_var"),
         textOutput("coeff_of_var_int"),
         textOutput("avg_dev"),
         textOutput("avg_dev_int"),
         textOutput("downside_dev"),
         textOutput("downside_dev_int"),
         textOutput("skewness"),
         textOutput("skewness_int"),
         textOutput("kurtosis_dev"),
         textOutput("kurtosis_int"),
         textOutput("percentile"),
         textOutput("percentile_int"),
         textOutput("five_alfa_quartile"),
         textOutput("five_alfa_quartile_int"),
         textOutput("basic_measures"),
         textOutput("basic_measures_int")
         
         
      )
   )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$variation <- renderText ({
     
     variation <- var(zarz_ryz[input$curr], na.rm = TRUE)
     paste("Wartość wariancji jest równa: ",specify_decimal(variation,4), " w: ", input$year,"roku.")
    

   })
   
   output$variation_int <- renderText ({
     

     paste("Wariancji co do zasady nie interpretujemy!")
     
     
   })
   
   
   output$stan_dev <- renderText ({
     
     standard_dev <- sd(as.numeric(unlist(zarz_ryz[input$curr])), na.rm = TRUE)
     paste("Wartość odchylenia standardowego jest równa: ", specify_decimal(standard_dev,4), " w: ",input$year,"roku.")
     
   })
   
   output$stan_dev_int <- renderText ({
     
     standard_dev <- sd(as.numeric(unlist(zarz_ryz[input$curr])), na.rm = TRUE)
     paste("Cena", input$curr , "w stosunku do złotego odchylała się przeciętnie od średniej o:",specify_decimal(standard_dev,4),"w",input$year,"roku.")
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

