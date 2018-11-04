
library(shiny)
library(openxlsx)
library(PerformanceAnalytics)
library(e1071)
library(dplyr)
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
        width = 3,
        #Zostawione w razie gdyby wymagane byly warunki (np. wybor dokladny, a filtrowanie tylko po roku)
         selectInput(inputId = "year",
                     label = "Pick year:",
                     choices = daty,
                     selected = 2017),
        #Pole odpowiada za wybor waluty 
        selectInput(inputId = "curr",
                    label = "Pick currency:",
                    choices = waluty,
                    selected = "USD"),
        #Pole odpowiada za wybor dokladniej daty wzietej pod uwage przy badaniu 
        # Co zwrocic, gdy nie ma obserwacji w danym okresie 
        dateRangeInput(inputId = "date",
                  label = "Select dates:",
                  start = "2007-01-04",
                  end = "2007-12-31",
                  format = "yyyy-mm-dd",
                  min = min_date, 
                  max = max_date,
                  startview = "year")
      ),
      
      
      
      
      # Show a plot of the generated distribution
      mainPanel(
         htmlOutput("variation"),
         htmlOutput("stan_dev"),
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
  
   output$variation <- renderUI ({
     
     #Filtrowanie po latach, zgodnie z zal zadania (korzysta z dokladnego okresu)
     zarz_ryz_fil <- zarz_ryz %>% 
       filter(zarz_ryz$`data/waluta` >= input$date[1] & zarz_ryz$`data/waluta` <= input$date[2])
     
     #Wyliczenie wartosci wariancji dla zbioru PO filtorwaniu. Uwzglednienie zbioru zlozonego z samych brakow danych
     variation <- var(zarz_ryz_fil[input$curr], na.rm = TRUE)
     if (is.na(variation)) {
       variation <- "Brak danych w podanym okresie."
     }
      
       #Wyswietlenie interpretacji, zakladajac 2 opcje: Wariacja wyliczona i niewyliczona.
       if(is.character(variation)) { HTML(
         paste(variation),
         "<br/>",
         paste("Wariancji co do zasady nie interpretujemy!")
       )}
     
       else {HTML (
         paste("Wartość wariancji jest równa: ",
         specify_decimal(variation,4),
         " w przedziale czasowym od: ",
         input$date[1],
         "do:",
         input$date[2],
         "." ),
         "<br/>",
         paste("Wariancji co do zasady nie interpretujemy!"))}
 
   })
   
   output$stan_dev <- renderUI ({
     
     #Filtrowanie po latach, zgodnie z zal zadania (korzysta z dokladnego okresu)
     zarz_ryz_fil <- zarz_ryz %>% 
       filter(zarz_ryz$`data/waluta` >= input$date[1] & zarz_ryz$`data/waluta` <= input$date[2])
     
     #Wyliczenie wartosci odch stand. dla zbioru PO filtorwaniu. Uwzglednienie zbioru zlozonego z samych brakow danych
     
     standard_dev <- sd(as.numeric(unlist(zarz_ryz_fil[input$curr])), na.rm = TRUE)
     
     if (is.na(standard_dev)) {
       standard_dev <- "Brak danych w podanym okresie."
     }
     
     #Wyswietlenie interpretacji, zakladajac 2 opcje: odch stand. wyliczone i niewyliczone.
     if(is.character(standard_dev)) { HTML(
       paste(standard_dev),
       "<br/>",
       paste("Brak wartosci do interpretacji!")
     )}
     
     else {HTML (
       paste("Wartość odchylenia standardowego jest równa: ",
             specify_decimal(standard_dev,4),
             " w przedziale czasowym od: ",
             input$date[1],
             "do:",
             input$date[2],
             "." ),
       "<br/>",
       paste("Cena",
             input$curr,
             "w stosunku do złotego odchylała się przeciętnie od średniej o:",
             specify_decimal(standard_dev,4),
             " w przedziale czasowym od: ",
             input$date[1],
             "do:",
             input$date[2],
             "." ))}
   })
   
   # output$stan_dev_int <- renderText ({
   #   
   #   standard_dev <- sd(as.numeric(unlist(zarz_ryz[input$curr])), na.rm = TRUE)
   #   paste("Cena", input$curr , "w stosunku do złotego odchylała się przeciętnie od średniej o:",specify_decimal(standard_dev,4),"w",input$year,"roku.")
   #   
   # })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

