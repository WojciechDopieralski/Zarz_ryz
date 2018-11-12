library(shiny)
library(openxlsx)
library(PerformanceAnalytics)
library(e1071)
library(dplyr)
library(quantmod)

# library(httr)
# library(rio)

#Wczytanie zbioru danych z pliku excel

 
  zarz_ryz <- read.xlsx(xlsxFile = "01_nstacj_waluty_STACJONARNE.xlsx",
                        sheet = "ceny walut",
                        startRow = 1)


# zarz_ryz <- load("data.RData")
# zarz_ryz1 <- janitor::clean_names(zarz_ryz)
  

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

# Stopy zwrotu 

zarz_ryz_nd <- zarz_ryz[,-1]
zarz_ryz_sto_zw <- lapply(zarz_ryz_nd, Delt, k=1) 
zarz_ryz_sto_zw <- as.data.frame(zarz_ryz_sto_zw)
zarz_ryz_sto_zw <- cbind (zarz_ryz[,1],zarz_ryz_sto_zw)
zarz_ryz_coln <- colnames(zarz_ryz)
names(zarz_ryz_sto_zw) <- zarz_ryz_coln 


ui <- fluidPage(
   
   # Application title
  
   titlePanel("Kursy walut - analiza"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        width = 3,
        #Zostawione w razie gdyby wymagane byly warunki (np. wybor dokladny, a filtrowanie tylko po roku)
         # selectInput(inputId = "year",
         #             label = "Pick year:",
         #             choices = daty,
         #             selected = 2017),
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
      
      
      
      
      # 
      mainPanel(
         htmlOutput("variation"),
         htmlOutput("stan_dev"),
         htmlOutput("coeff_of_var"),
         htmlOutput("avg_dev"),
         htmlOutput("downside_dev"),
         
         htmlOutput("skewness"),
         htmlOutput("kurtosis_dev"),

         
         htmlOutput("percentile"),
         htmlOutput("five_alfa_quartile"),
         htmlOutput("basic_measures")

         
         
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
   
   output$coeff_of_var <- renderUI ({
     
     #Filtrowanie po latach, zgodnie z zal zadania (korzysta z dokladnego okresu)
     zarz_ryz_fil <- zarz_ryz %>% 
       filter(zarz_ryz$`data/waluta` >= input$date[1] & zarz_ryz$`data/waluta` <= input$date[2])
     
     #Wyliczenie wartosci wspoczynnika zmiennosci dla zbioru PO filtorwaniu. Uwzglednienie zbioru zlozonego z samych brakow danych
     
     standard_dev <- sd(as.numeric(unlist(zarz_ryz_fil[input$curr])), na.rm = TRUE)
     avg <- mean(as.numeric(unlist(zarz_ryz_fil[input$curr])), na.rm = TRUE)  
     coef <- (standard_dev/avg)*100 
     
     if (is.na(standard_dev)) {
       standard_dev <- "Brak danych w podanym okresie."
     }
     
     #Wyswietlenie interpretacji, zakladajac 2 opcje: wspolczynnik zmiennosci wyliczony i niewyliczony.
     
     if(is.character(standard_dev) == T) { HTML(
       paste(standard_dev),
       "<br/>",
       paste("Brak wartosci do interpretacji!")
     )} else {HTML (
       paste("Wartosc wspolczynnika zmiennosci jest rowna: ",
             specify_decimal(coef,2),
             "%",
             " w przedziale czasowym od: ",
             input$date[1],
             "do:",
             input$date[2],
             "." ),
       "<br/>")}
   
     if (is.na(coef)) {
       HTML( paste(  "Współczynnik zmienności dla",
                     input$curr,
                     "nie istnieje, ze wzgledu na brak danych"
       ))} else if (coef <= 35) {
      HTML( paste("Współczynnik zmienności dla",
             input$curr,
             "jest niski, a więc zmienność w badanym okresie była niska. Okres, od:",
             input$date[1],
             "do:",
             input$date[2],
             "." ))

       } else if (coef > 35 & coef < 65) {
     HTML( paste(  "Współczynnik zmienności dla",
         input$curr,
         "jest umiarkowany, a więc zmienność w badanym okresie była umiarowana. Okres, od:",
         input$date[1],
         "do:",
         input$date[2],
         "."))
       } else if ( coef >= 65){
         HTML( paste(  "Współczynnik zmienności dla",
                       input$curr,
                       "jest wysoki lub bardzo wysoki, a więc zmienność w badanym okresie jest zbyt wysoka! Okres, od:",
                       input$date[1],
                       "do:",
                       input$date[2],
                       "."))
   
             } 
   })
   
   
   output$avg_dev <- renderUI ({
     
     #Filtrowanie po latach, zgodnie z zal zadania (korzysta z dokladnego okresu)
     zarz_ryz_fil <- zarz_ryz %>% 
       filter(zarz_ryz$`data/waluta` >= input$date[1] & zarz_ryz$`data/waluta` <= input$date[2])
     
     #Wyliczenie wartosci odch śred dla zbioru PO filtorwaniu. Uwzglednienie zbioru zlozonego z samych brakow danych
     
     standard_mad <- mad(as.numeric(unlist(zarz_ryz_fil[input$curr])), na.rm = TRUE)
     
     if (is.na(standard_mad)) {
       standard_mad <- "Brak danych w podanym okresie."
     }
     
     #Wyswietlenie interpretacji, zakladajac 2 opcje: odch śred. wyliczone i niewyliczone.
     if(is.character(standard_mad)) { HTML(
       paste(standard_mad),
       "<br/>",
       paste("Brak wartosci do interpretacji!")
     )}
     
     else {HTML (
       paste("Wartość odchylenia średniego jest równa: ",
             specify_decimal(standard_mad,4),
             " w przedziale czasowym od: ",
             input$date[1],
             "do:",
             input$date[2],
             "." ),
       "<br/>",
       paste("Cena",
             input$curr,
             "w stosunku do złotego odchylała się przeciętnie od średniej o:",
             specify_decimal(standard_mad,4),
             " w przedziale czasowym od: ",
             input$date[1],
             "do:",
             input$date[2],
             "." ))}
     
   })
   
   output$downside_dev <- renderUI ({
     
     #Filtrowanie po latach, zgodnie z zal zadania (korzysta z dokladnego okresu)
     zarz_ryz_fil <- zarz_ryz %>% 
       filter(zarz_ryz$`data/waluta` >= input$date[1] & zarz_ryz$`data/waluta` <= input$date[2])
     
     #Wyliczenie wartosci semi odch ujemnego dla zbioru PO filtorwaniu. Uwzglednienie zbioru zlozonego z samych brakow danych
     
     down_mad <- DownsideDeviation(as.numeric(unlist(zarz_ryz_fil[input$curr])), MAR = mean(as.numeric(unlist(zarz_ryz_fil[input$curr]))))
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
       paste("Wartość semi ochylenia ujemnego jest równa: ",
             specify_decimal(down_mad,4),
             " w przedziale czasowym od: ",
             input$date[1],
             "do:",
             input$date[2],
             "." ),
       "<br/>",
       paste("Biorąc pod uwage wartosci ponizej sredniej cena",
             input$curr,
             "w stosunku do złotego odchylała się przeciętnie od średniej o:",
             specify_decimal(down_mad,4),
             " w przedziale czasowym od: ",
             input$date[1],
             "do:",
             input$date[2],
             "." ))}
     
   })
   
   
   output$skewness <- renderUI ({
     
     #Filtrowanie po latach, zgodnie z zal zadania (korzysta z dokladnego okresu)
     zarz_ryz_fil <- zarz_ryz %>% 
       filter(zarz_ryz$`data/waluta` >= input$date[1] & zarz_ryz$`data/waluta` <= input$date[2])
   
     #Wyliczenie wartosci skośności dla zbioru PO filtorwaniu. Uwzglednienie zbioru zlozonego z samych brakow danych
     
     skew <- skewness(as.numeric(unlist(zarz_ryz_fil[input$curr])), na.rm = TRUE, type = 1)
     
     if (is.na(skew)) {
       skew <- "Brak danych w podanym okresie."
     }
     
     #Wyswietlenie interpretacji, zakladajac 2 opcje: odch stand. wyliczone i niewyliczone.
     if(is.character(skew)) { HTML(
       paste(skew),
       "<br/>",
       paste("Brak wartosci do interpretacji!")
     )} else if (skew > 0) {
       HTML (
         paste("Wartość współczynnika asymetrii jest równa: ",
               specify_decimal(skew,4),
               " w przedziale czasowym od: ",
               input$date[1],
               "do:",
               input$date[2],
               "." ),
         "<br/>",
       paste("Wystąpiła asymetria prawostronna, w przedziale czasowym od",
             input$date[1],
             "do:",
             input$date[2],
             ". Jest ona tym większa im większa wartość współczynnika." ),
       "<br/>")} else if (skew <0) {
         HTML (
           paste("Wartość współczynnika asymetrii jest równa: ",
                 specify_decimal(skew,4),
                 " w przedziale czasowym od: ",
                 input$date[1],
                 "do:",
                 input$date[2],
                 "." ),
           "<br/>",
         paste("Wystąpiła asymetria lewostronna, w przedziale czasowym od ",
               specify_decimal(skew,4),
               " w przedziale czasowym od: ",
               input$date[1],
               "do:",
               input$date[2],
               ". Jest ona tym większa im większa wartość współczynnika." )
         )} else {
           HTML (
           paste("W wybranym przedziale czasowym nie wysąpiła asymetria ")
           )
         }
      
     
   })
   
   output$kurtosis_dev<- renderUI ({
     
     #Filtrowanie po latach, zgodnie z zal zadania (korzysta z dokladnego okresu)
     zarz_ryz_fil <- zarz_ryz %>% 
       filter(zarz_ryz$`data/waluta` >= input$date[1] & zarz_ryz$`data/waluta` <= input$date[2])
     
     kurto <- kurtosis(as.numeric(unlist(zarz_ryz_fil[input$curr])), na.rm = TRUE, type = 1)
     
     if (is.na(kurto)) {
       kurto <- "Brak danych w podanym okresie."
     }
     
     #Wyswietlenie interpretacji, zakladajac 2 opcje: odch stand. wyliczone i niewyliczone.
     if(is.character(kurto)) { HTML(
       paste(kurto),
       "<br/>",
       paste("Brak wartosci do interpretacji!")
     )} else if (kurto > 0) {
       HTML (
         paste("Wartość współczynnika kurtozy jest równa: ",
               specify_decimal(kurto,4),
               " w przedziale czasowym od: ",
               input$date[1],
               "do:",
               input$date[2],
               "." ),
         "<br/>",
         paste("Wystąpiła znaczna koncentrcja wyników w okół średniej, w przedziale czasowym od",
               input$date[1],
               "do:",
               input$date[2],
               ". Jest ona tym większa im większa wartość współczynnika." ),
         "<br/>")} else if (kurto <0) {
           HTML (
             paste("Wartość współczynnika kurtozy jest równa: ",
                   specify_decimal(kurto,4),
                   " w przedziale czasowym od: ",
                   input$date[1],
                   "do:",
                   input$date[2],
                   "." ),
             "<br/>",
             paste("Wystąpiła słaba koncentrcja wyników w okół średniej,, w przedziale czasowym od ",
                   specify_decimal(kurto,4),
                   " w przedziale czasowym od: ",
                   input$date[1],
                   "do:",
                   input$date[2],
                   ". Jest ona tym słabsza im większa wartość współczynnika." )
           )} else {
             HTML (
               paste("W wybranym przedziale czasowym nie wysąpiła asymetria ")
             )
           }
     
   })
   
   output$percentile <- renderUI ({
     
     #Filtrowanie po latach, zgodnie z zal zadania (korzysta z dokladnego okresu)
     zarz_ryz_fil <- zarz_ryz %>% 
       filter(zarz_ryz$`data/waluta` >= input$date[1] & zarz_ryz$`data/waluta` <= input$date[2])
     
   })
   
   output$basic_measures <- renderUI ({
     
     #Filtrowanie po latach, zgodnie z zal zadania (korzysta z dokladnego okresu)
     zarz_ryz_fil <- zarz_ryz %>% 
       filter(zarz_ryz$`data/waluta` >= input$date[1] & zarz_ryz$`data/waluta` <= input$date[2])
     
     summ <- summary(as.numeric(unlist(zarz_ryz_fil[input$curr])))
     
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

