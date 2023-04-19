# Set Working Directory
#setwd("")

# Library
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(summarytools)
library(shinydashboardPlus)
library(lubridate)
library(dplyr)
library(tidyr)
library(readxl)
library(plotly)
library(DT)
library(factoextra)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(raster)
library(sf)
library(sp)
library(scales)
library(ggsn)
library(dbscan)
library(rgdal)
library(spatialreg)
library(spatial)
library(mapview)
library(tidyselect)
library(rgeos)
library(tmap)
library(dashboardthemes)
library(tableHTML)

# Datasets
profil <- read_xlsx("Profil Kabupaten Kota.xlsx") %>% mutate_at(c("UMR", "Luas Wilayah", "Penduduk Laki-laki",
                                                                  "Penduduk Perempuan", "Banyak Penduduk"), as.numeric)
kamar <- read_xlsx("Kamar.xlsx") %>% mutate_at(c("Tahun", "Berbintang", "Nonbintang"), as.numeric)
kasur <- read_xlsx("Tempat Tidur.xlsx") %>% mutate_at(c("Tahun", "Berbintang", "Nonbintang"), as.numeric)
akomodasi <- read_xlsx("Akomodasi.xlsx") %>% mutate_at(c("Tahun", "Berbintang", "Nonbintang"), as.numeric)
PDRB <- read_xlsx("PDRB ADHK 2010.xlsx") %>% mutate_at(c("2010","2011","2012","2013","2014","2015","2016",
                                                         "2017","2018","2019","2020"), as.numeric)
persentase <- read_xlsx("Persentase Penghuni Kamar.xlsx") %>% 
  mutate(Date = dmy(paste("01", Date)))
cluster <- read_xlsx("Cluster.xlsx") %>% 
  mutate_at(c("UMR", "Luas Wilayah", "Banyak Penduduk", "Akomodasi Kamar",
              "Akomodasi Hotel", "Akomodasi Tempat Tidur"), as.numeric)
agen <- read_xlsx("Agen dan Biro Perjalanan.xlsx")
RLMT <- read_xlsx("Rata-Rata Lama Menginap Tamu.xlsx") %>% 
  mutate(Date = dmy(paste("01", Date)))
SHP_Indonesia <- st_read('SHP/gadm40_IDN_2.shp')
df_peta <- read_xlsx("Data Peta.xlsx") %>% mutate_at("Banyak Penduduk", as.numeric)
jatim <- SHP_Indonesia %>%
  subset(SHP_Indonesia$NAME_1 == "Jawa Timur")

# Function
my_comma1 <- scales::label_comma(accuracy = .1, big.mark = ".", decimal.mark = ",")
my_comma2 <- scales::label_comma(big.mark = ".", decimal.mark = ",")

# Header Item
headerItem <- dashboardHeader(title = span(tagList(icon("house-user"))), titleWidth = 250,
                              dropdownMenu(type = "notifications",
                                           notificationItem(text = "Data Referensi",
                                                            href = "https://jatim.bps.go.id/subject/16/pariwisata.html"),
                                           notificationItem(text = "Data Dummy")),
                              dropdownMenu(type = "tasks",
                                           taskItem(
                                             text = "Tingkat Kesukaan Terhadap Pariwisata Jawa Timur",
                                             value = 97
                                           )),
                              dropdownMenu(type = "message",messageItem(from="Penulis",
                                                                        message = "Yuk ke Jawa Timur"))
)

# Sidebar Item
sidebarItem <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    menuItem("Pendahuluan", tabName = "Pendahuluan",icon = icon("book-open")),
    menuItem("Dataset", tabName = "Dataset", icon = icon("table")),
    menuItem("Profil Kabupaten/Kota", tabName = "Profil", icon = icon("city")),
    menuItem("Akomodasi", tabName = "Akomodasi", icon = icon("landmark")),
    menuItem("Travel", tabName = "Travel", icon = icon("bus")),
    menuItem("Klaster", tabName = "Klaster", icon = icon("layer-group")),
    menuItem("Penulis", tabName = "Penulis", icon = icon("marker"))
  )
)

# Body Item
bodyItem <- dashboardBody(
  shinyDashboardThemes(
    theme = "purple_gradient"
  ),
  tabItems(
    tabItem(tabName = "Pendahuluan",
            tabsetPanel(
              tabPanel(span(tagList(icon("book-open"), "Latar Belakang")),
                       fluidRow(
                         column(width = 12,
                                div(h1("LATAR BELAKANG"), style = "text-align:center;"),
                                div(h3("DASHBOARD PARIWISATA JAWA TIMUR"), style = "text-align:center;"),
                                br(),
                                box(
                                  width = 12,
                                  status = NULL,
                                  div(imageOutput("jatim"), style = "text-align:center;",
                                      style = "margin-bottom:-180px;"),
                                  div(h4("Jawa Timur merupakan salah satu provinsi terbesar di Indonesia, destinasi wisata dan pariwisata merupakan salah satu penyumbang pendapatan terbesar. Provinsi dalam mempertahankan pamornya tentu harus mengembangkan bagian-bagian/perangkat yang ada didalamnya. Terutama dalam sector pariwisata. Provinsi Jawa Timur memiliki kewajiban mengembangkan daerahnya sendiri baik melalui kabupatan dan kota penyusunnya. Di Jawa Timur, Dinas Pariwisata merupakan Lembaga yang mengelola data yang berkaitan dengan alur keluar masuk pariwisata mulai dari pengunjung, usaha akomodasi hingga produk domestic regional bruto. Keterbatasan ruang dan penyimpanan dokumen pencatatan kegiatan yang berdampak pada kebutuhan sarana visualisasi dan pengelolaan dokumen yang ada."), 
                                      style = "text-align: justify;"),
                                  div(h4("Seiring dengan perkembangan terknologi informasi dan komunikasi pada zaman sekarang yang semakin maju khususnya dalam bidang internet service memberikan dampak positif bagi pengelolaan kegiatan yang berhubungan dengan data. Beberapa usaha telah dilakukan dalam penelitian terkait pemanfaatan teknologi. Adapun penelitian yang pernah dilakukan adalah melakukan pengembangan sistem informasi database pada penelitian dan pengabdian masyarakat yang telah dilakukan oleh dosen-dosen Departemen Statistika Institut Teknologi Sepuluh Nopember dalam menghitung kinerja dosen dalam kegiatan penelitian dan pengabdian kepada masyarakat dengan membuat dashboard dengan menggunakan Shiny R."), 
                                      style = "text-align: justify;"),
                                  div(h4("Berdasarkan latar belakang permasalahan diatas dan mengacu pada beberapa hasil penelitian yang telah dilakukan maka penelitian ini bertujuan untuk membuat dashboard mneggunakan R Shiny guna mengembangkan sistem informasi manajemen dari  Pariwisata Jawa Timur meliputi pengunjung, usaha akomodasi maupun pendapatan daerah yang ada di Provinsi Jawa Timur sendiri yang menghasilkan database. Tidak hanya digunakan pada internal provinsi, melainkkan juga untuk digitaliasai hasil penelitian berupa dokumen elektronik. Selain itu, dashboard pada penelitian dapat menunjukkan data-data penelitian dan pengamatan unsur unsur yang ada di pariwisata mulai dari Wisatawan, Pelancong , usaha akomodasi, hotel berbintang-non bintang, Penginapan, rata rata tamu perhari , 
                                  Produk Domestik Regional Bruto. Dashboard ini juga diharapkan mempermudah orang lain untuk mengetahui perkembangan daerahnya sendiri. Selain untuk orang lain, juga diharapkan mampu mengoptimalkan pengelolaan sumber daya pariwisata dan mempersingkat waktu proses pengelolaan data pada Dinas Provinsi Jawa Timur. Hasil laporan ini juga diharapkan bisa menjadi Langkah pengambilan keputusan terbaik oleh Provinsi Jawa Timur beserta kabupatan dan kota yang ada didalamnya untuk memantau apakah sudah optimal atau tidak perkembangan pariwisata yang ada dan menjadi lebih baik."), 
                                      style = "text-align: justify;")
                                ))
                       )
              ),
              tabPanel(span(tagList(icon("compass"), "Tujuan")),
                       fluidRow(
                         column(width = 12,
                                div(h1("TUJUAN"), style = "text-align:center;"),
                                div(h3("DASHBOARD PARIWISATA JAWA TIMUR"), style = "text-align:center;"),
                                br(),
                                box(
                                  width = 12,
                                  div(h4(p("Adapun yang menjadi tujuan dari penelitian ini adalah :"))),
                                  div(h4(p("1. Mengetahui Profil identitas dari kabupaten atau kota"))),
                                  div(h4(p("2. Merancang dashboard untuk mengetahui kondisi dan perkembangan pariwisata khususnya di Jawa Timur"))),
                                  div(h4(p("3. Mengetahui Statistik Kunjungan Wisatawan baik dari Luar Negeri maupun Indonesia dari waktu ke waktu"))),
                                  div(h4(p("4. Mengetahui perkembangan akomodasi serta biro dan agen perjalanan"))),
                                  div(h4(p("5. Mengetahui cluster terdekat dari tiap kabupaten kota berdasarkan banyak klaster yang diinginkan "))),
                                  div(h4(p("6. Mengetahui persebaran wilayah dari UMR dan Jumlah penduduk Jawa Timur")))
                                ))
                       )),
              tabPanel(span(tagList(icon("bullseye"), "Manfaat")),
                       fluidRow(
                         column(width = 12,
                                div(h1("MANFAAT"), style = "text-align:center;"),
                                div(h3("DASHBOARD PARIWISATA JAWA TIMUR"), style = "text-align:center;"),
                                br(),
                                box(
                                  width = 12,
                                  div(h4(p("Adapun yang menjadi tujuan dari penelitian ini adalah :"))),
                                  div(h4(p("1. Mempermudah user untuk mengetahui identitas dari kabupaten kota terkait"))),
                                  div(h4(p("2. Sebagai dasar pengambilan keputusan bagi pemerintah kabupaten kota terkait"))),
                                  div(h4(p("3. Mempermudah pemerintah kabupaten kota mengambil kebijakan terkait kunjungan wisatawan yang berkunjung ke Jawa Timur"))),
                                  div(h4(p("4. Mempermudah user melihat akomodasi, biro dan agen perjalanan mana yang mudah dijangkau"))),
                                  div(h4(p("5. Sebagai dasar untuk pembaharuan informasi terkini perkembangan Pariwisata Jawa Timur dengan melihat maps chart")))
                                ))
                       ))
            )
    ),
    tabItem(tabName = "Dataset",
            tabsetPanel(
              tabPanel(span(tagList(icon("database"), "DataFrame")),
                       fluidPage(pickerInput("dataframe",label = "Dataset", 
                                             choices = c("Akomodasi Hotel", "Akomodasi Kamar Hotel","Akomodasi Tempat Tidur", 
                                                         "PDRB Kabupaten/Kota","Persentase Penghuni kamar","cluster","Agen Biro dan Perjalanan","Rata-rata Lama Menginap Tamu"),
                                             choicesOpt = list(style = "color=white")),
                                 DTOutput("table_data"))
              ),
              tabPanel(span(tagList(icon("pen"), "Summary")),
                       verbatimTextOutput("summary_stat"))
            )
    ),
    tabItem(tabName = "Profil",
            fluidRow(
              box(
                title = "Profil Kabupaten / Kota", 
                closable = FALSE, 
                enable_label = TRUE,
                label_status = "primary",
                solidHeader = TRUE, 
                collapsible = TRUE,
                width = 12,
                
                pickerInput(inputId = "Kab1",label = "Kabupaten/Kota :",
                            choices = unique(profil$`Kabupaten/Kota`),
                            selected = unique(profil$`Kabupaten/Kota`)[1],
                            choicesOpt = list(style = "color=white"))
              )
            ),
            fluidRow(
              width = 12,
              box(width = 4,
                  fluidRow(height = 3,
                           infoBoxOutput(
                             outputId = "UMR",
                             width = 12
                           )),
                  fluidRow(height = 3,
                           infoBoxOutput(
                             outputId = "Luas",
                             width = 12
                           ))
              ),
              box(width = 4,
                  div(imageOutput("FotoKab"),style="text-align: center;",
                      style = "margin-bottom:-165px;")),
              box(width = 4,
                  fluidRow(height = 3,
                           infoBoxOutput(
                             outputId = "PendudukP",
                             width = 12
                           )),
                  fluidRow(height = 3,
                           infoBoxOutput(
                             outputId = "PendudukW",
                             width = 12
                           ))
              )
            ),
            fluidRow(
              box(
                title = "Produk Domestik Regional Bruto", 
                closable = FALSE, 
                enable_label = TRUE,
                label_status = "primary",
                solidHeader = TRUE, 
                collapsible = TRUE,
                width = 12,
                plotlyOutput(outputId = "PDRB"))
            )
    ),
    tabItem(tabName = "Akomodasi",
            tabsetPanel(
              tabPanel(span(tagList(icon("chart-line"), "Visualisasi 1")),
                       fluidRow(
                         box(
                           title = "Informasi Hotel", 
                           closable = FALSE, 
                           enable_label = TRUE,
                           label_status = "primary",
                           solidHeader = TRUE, 
                           collapsible = TRUE,
                           width = 12,
                           pickerInput(inputId = "Kab2",label = "Kabupaten/Kota :",
                                       choices = unique(kamar$`Kabupaten/Kota`),
                                       selected = unique(kamar$`Kabupaten/Kota`)[length(unique(kamar$`Kabupaten/Kota`))],
                                       choicesOpt = list(style = "color=white")),
                           dropdown(
                             tags$h3("Jenis Akomodasi :"),
                             prettyRadioButtons(inputId = "datahotel", label = " ",
                                                choices = c("Hotel", "Kamar Hotel", "Tempat Tidur"),
                                                selected = "Hotel",
                                                status = "primary",
                                                fill=T,
                                                icon = icon("check"),
                                                animation = "jelly"),
                             style = "unite", icon = icon("gear"),
                             status = "primary", width = "300px",
                             animate = animateOptions(
                               enter = animations$fading_entrances$fadeInLeftBig,
                               exit = animations$fading_exits$fadeOutRightBig
                             )
                           )
                         )),
                       fluidRow(
                         box(
                           title = "Perkembangan Akomodasi", 
                           closable = FALSE, 
                           enable_label = TRUE,
                           label_status = "primary",
                           solidHeader = TRUE, 
                           collapsible = TRUE,
                           width = 6,
                           plotlyOutput(outputId = "ako1")),
                         box(
                           title = " Total Akomodasi", 
                           closable = FALSE, 
                           enable_label = TRUE,
                           label_status = "primary",
                           solidHeader = TRUE, 
                           collapsible = TRUE,
                           width = 6,
                           plotlyOutput(outputId = "ako2"))
                       )),
              tabPanel(span(tagList(icon("chart-bar"), "Visualisasi 2")),
                       fluidRow(
                         box(
                           title = "Perbandingan Akomodasi", 
                           closable = FALSE, 
                           enable_label = TRUE,
                           label_status = "primary", 
                           solidHeader = TRUE, 
                           collapsible = TRUE,
                           width = 12,
                           height = 12,
                           prettyRadioButtons(inputId = "jenisako3", label = "Jenis Akomodasi :",
                                              choices = c("Hotel", "Kamar Hotel", "Tempat Tidur"),
                                              selected = "Hotel",
                                              status = "primary",
                                              fill=T,
                                              inline = T,
                                              icon = icon("check"),
                                              animation = "jelly"),
                           dropdown(
                             tags$h3("Sorting"),
                             style = "unite", icon = icon("gear"),
                             status = "primary", width = "300px",
                             checkboxGroupButtons(
                               inputId = "tahunako3",
                               label = "Tahun Akomodasi :",
                               choices = unique(akomodasi$Tahun),
                               selected = "2020",
                               status = "primary"
                             ),
                             prettyRadioButtons(inputId = "sortako3", label = "Jenis Akomodasi :",
                                                choices = c("Tertinggi","Terendah"),
                                                selected = "Tertinggi",
                                                status = "primary",
                                                fill=T,
                                                icon = icon("check"),
                                                animation = "jelly"),
                             animate = animateOptions(
                               enter = animations$fading_entrances$fadeInLeftBig,
                               exit = animations$fading_exits$fadeOutRightBig
                             )
                           ),
                           plotlyOutput(outputId = "ako3")
                         )
                       ),
                       fluidRow(
                         box(
                           title = "Korelasi", 
                           closable = FALSE, 
                           enable_label = TRUE,
                           label_status = "primary",
                           solidHeader = TRUE, 
                           collapsible = TRUE,
                           width = 12,
                           dropdown(
                             tags$h3("Jenis Akomodasi :"),
                             prettyRadioButtons(inputId = "ako4x", label = "Variabel X",
                                                choices = names(cluster)[2:4],
                                                selected = "UMR",
                                                status = "primary",
                                                fill=T,
                                                icon = icon("check"),
                                                animation = "jelly"),
                             prettyRadioButtons(inputId = "ako4y", label = "Variabel Y",
                                                choices = names(cluster)[5:7],
                                                selected = "Akomodasi Kamar",
                                                status = "primary",
                                                fill=T,
                                                icon = icon("check"),
                                                animation = "jelly"),
                             style = "unite", icon = icon("gear"),
                             status = "primary", width = "300px",
                             animate = animateOptions(
                               enter = animations$fading_entrances$fadeInLeftBig,
                               exit = animations$fading_exits$fadeOutRightBig
                             )
                           ),
                           plotlyOutput(outputId = "ako4")
                         )
                       ))
            )
    ),
    tabItem(tabName = "Travel",
            tabsetPanel(
              tabPanel(span(tagList(icon("store"), "Biro dan Agen")),
                       fluidRow(
                         box(
                           title = "Informasi Biro dan Agen Perjalanan", 
                           closable = FALSE, 
                           enable_label = TRUE,
                           label_status = "primary",
                           solidHeader = TRUE, 
                           collapsible = TRUE,
                           width = 12,
                           
                           pickerInput(inputId = "Kab3",label = "Kabupaten/Kota :",
                                       choices = unique(agen$`Kabupaten/Kota`),
                                       selected = unique(agen$`Kabupaten/Kota`)[length(unique(agen$`Kabupaten/Kota`))],
                                       choicesOpt = list(style = "color=white")),
                           plotlyOutput(outputId = "agen")
                         )
                       )
              ),
              tabPanel(span(tagList(icon("car"), "Wisatawan")),
                       fluidRow(
                         box(
                           title = "Persentase Penggunaan Kamar Hotel", 
                           closable = FALSE, 
                           enable_label = TRUE,
                           label_status = "primary",
                           solidHeader = TRUE, 
                           collapsible = TRUE,
                           width = 12,
                           dropdown(
                             tags$h3("Opsi :"),
                             dateRangeInput(inputId = "date1",
                                            label = "Rentang Waktu :",
                                            start = min(persentase$Date),
                                            end = max(persentase$Date),
                                            min = min(persentase$Date),
                                            max = max(persentase$Date),
                                            format = "yyyy/mm/dd",
                                            separator = "-"),
                             style = "unite", icon = icon("gear"),
                             status = "primary", width = "300px",
                             animate = animateOptions(
                               enter = animations$fading_entrances$fadeInLeftBig,
                               exit = animations$fading_exits$fadeOutRightBig
                             )
                           ),
                           plotlyOutput(outputId = "persenkamar")),
                         box(
                           title = "Rata-Rata Lama Menginap", 
                           closable = FALSE, 
                           enable_label = TRUE,
                           label_status = "primary",
                           solidHeader = TRUE, 
                           collapsible = TRUE,
                           width = 12,
                           dropdown(
                             tags$h3("Opsi :"),
                             dateRangeInput(inputId = "date2",
                                            label = "Rentang Waktu :",
                                            start = min(RLMT$Date),
                                            end = max(RLMT$Date),
                                            min = min(RLMT$Date),
                                            max = max(RLMT$Date),
                                            format = "yyyy/mm/dd",
                                            separator = "-"),
                             style = "unite", icon = icon("gear"),
                             status = "primary", width = "300px",
                             animate = animateOptions(
                               enter = animations$fading_entrances$fadeInLeftBig,
                               exit = animations$fading_exits$fadeOutRightBig
                             )
                           ),
                           plotlyOutput(outputId = "lamainap"))
                       )),
            )
    ),
    tabItem(tabName = "Klaster",
            fluidRow(
              box(
                title = "Maps Jawa Timur",
                closable = FALSE, 
                enable_label = TRUE,
                label_status = "primary",
                solidHeader = TRUE, 
                collapsible = TRUE,
                width=12,
                
                numericInput(inputId = "k_klaster",
                             label = "Banyak Klaster :",
                             value = 1,
                             min = 1,
                             max = 10),
                leafletOutput(outputId = "peta", height=500, width = 1080)
              ))
    ),
    tabItem(tabName = "Penulis",
            div(h1("DASHBOARD"), style = "text-align: center;"),
            div(h2("PARIWISATA JAWA TIMUR"), style = "text-align: center;"),
            br(),
            br(),
            div(h2("Penyusun"), style = "text-align: center;"),
            br(),
            fluidRow(
              box(
                width = 6,
                status = NULL,
                div(imageOutput("foto1"), style = "text-align:center;",
                    style = "margin-bottom:-180px;"),
                div(strong("Benedictus Kenny Tjahjono"), style = "text-align:center;"),
                div(strong("5003201159"), style = "text-align: center;")
              ),
              box(
                width = 6,
                status = NULL,
                div(imageOutput("foto2"), style = "text-align:center;",
                    style = "margin-bottom:-180px;"),
                div(strong("Yusron Ardiyanto"), style = "text-align:center;"),
                div(strong("5003201007"), style = "text-align: center;")
              ),
              div(h2("Sistem Informasi Manajemen"), style = "text-align: center;"),
              div(h3("Departemen Statistika"), style = "text-align: center;"),
              div(h3("Fakultas Sains dan Analitika Data"), style = "text-align: center;"),
              div(h3("Institut Teknologi Sepuluh Nopember"), style = "text-align: center;"),
              div(h3("2022"), style = "text-align: center;")
            ))
  )
)

# Server
server <- function(input, output,session){
  # Tab Pendahuluan
  output[["jatim"]]<-renderImage({
    list(src = "www/jatim.png", height = 225, width = 225)
  }, deleteFile = FALSE)
  
  # Tab Profil Kabupaten/Kota
  output$FotoKab<-renderImage({
    list(src = as.character(profil[profil$`Kabupaten/Kota` == input$Kab1, "Link Logo"]), height = 200, width = 160)
  }, deleteFile = FALSE) 
  
  output$UMR <- renderValueBox({
    data <- profil %>% 
      filter(`Kabupaten/Kota`==input$Kab1)
    data[,c(2:6)] <- lapply(data[,c(2:6)], my_comma1)
    
    valueBox(tags$p("UMR (IDR)",style="font-weight:bold;"),
             value = data %>% pull(UMR),
             color = "purple",
             icon = icon("money-bill"))
  })
  output$Luas <- renderValueBox({
    data <- profil %>% 
      filter(`Kabupaten/Kota`==input$Kab1)
    data[,c(2:6)] <- lapply(data[,c(2:6)], my_comma1)
    
    valueBox(tags$p("Luas Wilayah (km²)",style="font-weight:bold;"),
             value = data %>% pull(`Luas Wilayah`),
             color = "purple",
             icon = icon("map-pin"))
  })
  output$PendudukP <- renderValueBox({
    data <- profil %>% 
      filter(`Kabupaten/Kota`==input$Kab1)
    data[,c(2:6)] <- lapply(data[,c(2:6)], my_comma2)
    
    valueBox(tags$p("Penduduk Laki-laki",style="font-weight:bold;"),
             value = data %>% pull(`Penduduk Laki-laki`),
             color = "purple",
             icon = icon("mars"))
  })
  output$PendudukW <- renderValueBox({
    data <- profil %>% 
      filter(`Kabupaten/Kota`==input$Kab1)
    data[,c(2:6)] <- lapply(data[,c(2:6)], my_comma2)
    
    valueBox(tags$p("Penduduk Perempuan",style="font-weight:bold;"),
             value = data %>% pull(`Penduduk Perempuan`),
             color = "purple",
             icon = icon("venus"))
  })
  output$PDRB <- renderPlotly({
    New_PDRB <- PDRB %>% gather(key = Year, value = Value, -`Kabupaten/Kota`) %>%
      mutate_at(c("Year", "Value"), as.numeric) 
    
    New_PDRB %>% 
      filter(`Kabupaten/Kota`==input$Kab1) %>%
      ggplot(aes(x = Year, y = Value)) + labs(title=paste("PDRB", input$Kab1), 
                                              subtitle="Tahun 2010-2020", 
                                              x="Year", y="PDRB (Miliyar Rupiah)") +
      geom_line(col = "#560bad") + geom_point(col = "#560bad") + scale_x_continuous(breaks=seq(2010, 2020, 1)) + 
      theme_minimal()
  })
  
  # Tab Dataset
  pick_df <- reactive({
    if(input$dataframe == "Akomodasi Hotel"){
      dataset <- akomodasi
    }else if(input$dataframe == "Akomodasi Kamar Hotel"){
      dataset <- kamar
    }else if(input$dataframe == "Akomodasi Tempat Tidur"){
      dataset <- kasur
    }else if(input$dataframe == "PDRB Kabupaten/Kota"){
      dataset <- PDRB
    }else if(input$dataframe == "Persentase Penghuni kamar"){
      dataset <- persentase
    } else if(input$dataframe == "cluster"){
      dataset <- cluster
    } else if(input$dataframe == "Agen Biro dan Perjalanan"){
      dataset <- agen
    } else if(input$dataframe == "Rata-rata Lama Menginap Tamu"){
      dataset <- RLMT
    }
    return(dataset)
  })
  output$table_data <- renderDT({pick_df()})
  output$summary_stat <- renderPrint(dfSummary({pick_df()}))
  
  # Tab Akomodasi
  data_hotel <- reactive({
    if(input$datahotel == "Hotel"){
      data_hotel <- akomodasi
    }else if(input$datahotel == "Kamar Hotel"){
      data_hotel <- kamar
    }else(
      data_hotel <- kasur
    )
    return(data_hotel)
  })
  
  output$ako1 <- renderPlotly({
    data_hotel() %>% filter(`Kabupaten/Kota` == input$Kab2) %>%
      group_by(`Tahun`) %>%
      summarise(Bintang = sum(`Berbintang`),
                Nonbintang = sum(`Nonbintang`)) %>%
      gather(key = Status, value = Value, -Tahun) %>%
      ggplot(aes(x=Tahun, y=Value, col = Status)) + geom_line() + geom_point() +
      labs(x="Tahun", y=input$datahotel) + scale_color_manual(values = c("#560bad", "#4361ee")) + theme_minimal()
  })
  
  output$ako2 <- renderPlotly({
    data_hotel() %>% filter(`Kabupaten/Kota` == input$Kab2) %>%
      summarise(Bintang = sum(`Berbintang`),
                Nonbintang = sum(`Nonbintang`)) %>%
      gather(key = Status, value = Value) %>%
      ggplot(aes(Status, Value, fill=Status)) + geom_col() + 
      labs(x="Status", y=input$datahotel) + scale_fill_manual(values = c("#0077b6", "#48cae4")) + theme_minimal()
  })
  
  data_ako3 <- reactive({
    if(input$jenisako3 == "Hotel"){
      data_ako3 <- akomodasi
    }else if(input$jenisako3 == "Kamar Hotel"){
      data_ako3 <- kamar
    }else(
      data_ako3 <- kasur
    )
    return(data_ako3)
  })
  
  output$ako3 <- renderPlotly({
    if(input$sortako3 == "Tertinggi"){
      judul <- paste("7 Kabupaten/Kota Dengan Akomodasi", input$jenisako3 ,"Tertinggi")
      xlab <- paste("Akomodasi", input$jenisako3)
      data_ako3()[!data_ako3()$`Kabupaten/Kota`=="Jawa Timur",] %>% group_by(`Kabupaten/Kota`) %>% filter(`Tahun` == input$tahunako3) %>% 
        summarise(Akomodasi = sum(c(`Berbintang`, `Nonbintang`))) %>% arrange(desc(`Akomodasi`)) %>% head(7) %>% 
        ggplot(aes(x = `Akomodasi`, y = reorder(`Kabupaten/Kota`, `Akomodasi`), fill = `Kabupaten/Kota`)) + 
        geom_bar(stat = "Identity", fill = "#4361ee") + labs(title = judul, subtitle = paste("Tahun", "2020"), x = xlab, y = "Kabupaten/Kota") + 
        theme_minimal() + theme(legend.position = "none", 
                                plot.title = element_text(size = 14, face = 'bold', hjust = 1),
                                plot.subtitle = element_text(size = 12, face = 'bold', hjust = 1))
    }else{
      judul <- paste("7 Kabupaten/Kota Dengan Akomodasi", input$jenisako3 ,"Terendah")
      xlab <- paste("Akomodasi", input$jenisako3)
      data_ako3()[!data_ako3()$`Kabupaten/Kota`=="Jawa Timur",] %>% group_by(`Kabupaten/Kota`) %>% filter(`Tahun` == input$tahunako3) %>% 
        summarise(Akomodasi = sum(c(`Berbintang`, `Nonbintang`))) %>% arrange(desc(`Akomodasi`)) %>% tail(7) %>% 
        ggplot(aes(x = `Akomodasi`, y = reorder(`Kabupaten/Kota`, -`Akomodasi`), fill = `Kabupaten/Kota`)) + 
        geom_bar(stat = "Identity", fill = "#4361ee") + labs(title = judul, subtitle = paste("Tahun", "2020"), x = xlab, y = "Kabupaten/Kota") + 
        theme_minimal() + theme(legend.position = "none", 
                                plot.title = element_text(size = 14, face = 'bold', hjust = 1),
                                plot.subtitle = element_text(size = 12, face = 'bold', hjust = 1))
    }
  })
  
  output$ako4 <- renderPlotly({
    if(input$ako4x == "UMR"){
      if(input$ako4y == "Akomodasi Kamar"){
        ggplot(cluster, aes(x=`UMR`, y=`Akomodasi Kamar`)) + 
          geom_point(color = "#560bad", alpha = 0.5) + geom_smooth(method = "lm") + 
          labs(title = paste("Korelasi :",round(cor(cluster$UMR, cluster$`Akomodasi Kamar`), 3))) + 
          theme_minimal() + theme(plot.title = element_text(size = 12, face = 'bold', hjust=1))
      }else if(input$ako4y == "Akomodasi Hotel"){
        ggplot(cluster, aes(x=`UMR`, y=`Akomodasi Hotel`)) + 
          geom_point(color = "#560bad", alpha = 0.5) + geom_smooth(method = "lm") + 
          labs(title = paste("Korelasi :",round(cor(cluster$UMR, cluster$`Akomodasi Hotel`), 3))) + 
          theme_minimal() + theme(plot.title = element_text(size = 12, face = 'bold', hjust=1))
      }else if(input$ako4y == "Akomodasi Tempat Tidur"){
        ggplot(cluster, aes(x=`UMR`, y=`Akomodasi Tempat Tidur`)) + 
          geom_point(color = "#560bad", alpha = 0.5) + geom_smooth(method = "lm") + 
          labs(title = paste("Korelasi :",round(cor(cluster$UMR, cluster$`Akomodasi Tempat Tidur`), 3))) + 
          theme_minimal() + theme(plot.title = element_text(size = 12, face = 'bold', hjust=1))
      }
    }else if(input$ako4x == "Luas Wilayah"){
      if(input$ako4y == "Akomodasi Kamar"){
        ggplot(cluster, aes(x=`Luas Wilayah`, y=`Akomodasi Kamar`)) + 
          geom_point(color = "#560bad", alpha = 0.5) + geom_smooth(method = "lm") + 
          labs(title = paste("Korelasi :",round(cor(cluster$`Luas Wilayah`, cluster$`Akomodasi Kamar`), 3))) + 
          theme_minimal() + theme(plot.title = element_text(size = 12, face = 'bold', hjust=1))
      }else if(input$ako4y == "Akomodasi Hotel"){
        ggplot(cluster, aes(x=`Luas Wilayah`, y=`Akomodasi Hotel`)) + 
          geom_point(color = "#560bad", alpha = 0.5) + geom_smooth(method = "lm") + 
          labs(title = paste("Korelasi :",round(cor(cluster$`Luas Wilayah`, cluster$`Akomodasi Hotel`), 3))) + 
          theme_minimal() + theme(plot.title = element_text(size = 12, face = 'bold', hjust=1))
      }else if(input$ako4y == "Akomodasi Tempat Tidur"){
        ggplot(cluster, aes(x=`Luas Wilayah`, y=`Akomodasi Tempat Tidur`)) + 
          geom_point(color = "#560bad", alpha = 0.5) + geom_smooth(method = "lm") + 
          labs(title = paste("Korelasi :",round(cor(cluster$`Luas Wilayah`, cluster$`Akomodasi Tempat Tidur`), 3))) + 
          theme_minimal() + theme(plot.title = element_text(size = 12, face = 'bold', hjust=1))
      }
    }else if(input$ako4x == "Banyak Penduduk"){
      if(input$ako4y == "Akomodasi Kamar"){
        ggplot(cluster, aes(x=`Banyak Penduduk`, y=`Akomodasi Kamar`)) + 
          geom_point(color = "#560bad", alpha = 0.5) + geom_smooth(method = "lm") + 
          labs(title = paste("Korelasi :",round(cor(cluster$`Banyak Penduduk`, cluster$`Akomodasi Kamar`), 3))) + 
          theme_minimal() + theme(plot.title = element_text(size = 12, face = 'bold', hjust=1))
      }else if(input$ako4y == "Akomodasi Hotel"){
        ggplot(cluster, aes(x=`Banyak Penduduk`, y=`Akomodasi Hotel`)) + 
          geom_point(color = "#560bad", alpha = 0.5) + geom_smooth(method = "lm") + 
          labs(title = paste("Korelasi :",round(cor(cluster$`Banyak Penduduk`, cluster$`Akomodasi Hotel`), 3))) + 
          theme_minimal() + theme(plot.title = element_text(size = 12, face = 'bold', hjust=1))
      }else if(input$ako4y == "Akomodasi Tempat Tidur"){
        ggplot(cluster, aes(x=`Banyak Penduduk`, y=`Akomodasi Tempat Tidur`)) + 
          geom_point(color = "#560bad", alpha = 0.5) + geom_smooth(method = "lm") + 
          labs(title = paste("Korelasi :",round(cor(cluster$`Banyak Penduduk`, cluster$`Akomodasi Tempat Tidur`), 3))) + 
          theme_minimal() + theme(plot.title = element_text(size = 12, face = 'bold', hjust=1))
      }
    }
  })
  
  #Tab Travel
  output$agen <- renderPlotly({
    agen %>% filter(`Kabupaten/Kota` == input$Kab3) %>%
      group_by(`tahun`) %>%
      summarise(Biro = sum(`Biro`),
                Agen = sum(`Agen`)) %>%
      gather(key = Status, value = Jenis, -tahun) %>%
      ggplot(aes(x=tahun, y=Jenis, col = Status)) + geom_line() + geom_point() +
      labs(title="Tahun 2016-2021", 
           x="Tahun", y="Biro") + scale_color_manual(values = c("#0077b6", "#48cae4")) + 
      geom_vline(xintercept = as.numeric("2020"), linetype = "dashed", color = "blue") + theme_minimal()
  })
  
  output$persenkamar <- renderPlotly({
    persentase %>% 
      filter(Date >= input$date1[1] & Date <= input$date1[2]) %>% group_by(Date) %>%
      summarize(Berbintang = sum(Berbintang), Nonbintang = sum(Nonbintang)) %>%
      gather(key = Status, value = Value, -Date) %>% ggplot(aes(Date, Value, col=Status)) +
      geom_line() + geom_point() + geom_point(aes(group = seq_along(Date))) + 
      scale_color_manual(values = c("#560bad", "#4361ee")) + 
      geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed") + theme_minimal()
  })
  
  output$lamainap <- renderPlotly({
    RLMT %>%
      filter(Date >= input$date2[1] & Date <= input$date2[2]) %>% group_by(Date) %>%
      gather(key = Asal, value = Value, -Date) %>% ggplot(aes(Date, Value, col=Asal)) +
      geom_line() + geom_point() + scale_color_manual(values = c("#560bad", "#4361ee")) +
      geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed") + theme_minimal()
  })
  
  #Tab Cluster
  output$peta <- renderLeaflet({
    data_cluster <- cluster[,-1]
    row.names(data_cluster) <- cluster$`Kabupaten/Kota`; data_cluster
    result <- kmeans(data_cluster, input$k_klaster)
    dataframe_cluster <- cbind(data_cluster, Klaster = result$cluster)
    dataframe_cluster <- dataframe_cluster %>% relocate(Klaster, .before="UMR")
    dataframe_cluster <- dataframe_cluster[order(dataframe_cluster$Klaster),]
    data <- dataframe_cluster$Klaster
    data <- as.factor(data)
    
    colorset <- c("#e76f51", "#2a9d8f", "#023e8a", "#e63946", "#6b705c", 
                  "#b5179e", "#7209b7", "#d4a373", "#495057", "#5f0f40")
    col <- sample(colorset, size = input$k_klaster, replace=F)
    
    kab <- jatim %>%
      dplyr::select(NAME_2)
    
    cent <- st_centroid(kab)
    coords <-  cent[2]
    datakoordinat = st_coordinates(coords)
    xcord <- datakoordinat[,1]
    ycord <- datakoordinat[,2]
    
    gambarpeta <- leaflet(jatim) %>%
      addTiles() %>% 
      addPolygons(
        color = col,
        weight = 2,
        opacity = 1,
        dashArray = "3",
        fillOpacity = 0.7,
        
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        
        label = paste0(jatim$NAME_2, " ", jatim$data),
        
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      addLegend(
        position = "bottomright",
        title = "Klaster",
        colors = col,
        labels = seq(1, input$k_klaster, 1),
        opacity = 1
      ) %>%
      addMarkers(xcord, ycord, 
                 label = paste0(jatim$NAME_2, " ", jatim$data) ,
                 labelOptions = labelOptions(noHide = T, textsize = "1px", direction = "bottom",
                                             style = list(
                                               "color" = "red",
                                               "font-family" = "serif",
                                               "font-style" = "italic",
                                               "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)"
                                             ))
      )
    
    gambarpeta
  })
  #Tab Penulis
  output[["foto1"]]<-renderImage({
    list(src = "www/kenny.jpeg", height = 200, width = 180)
  }, deleteFile = FALSE)
  
  output[["foto2"]]<-renderImage({
    list(src = "www/yusron.jpg", height = 200, width = 180)
  }, deleteFile = FALSE)
  
}

# User Interface
ui <- dashboardPage(header=headerItem,
                    sidebar = sidebarItem,
                    body = bodyItem)
shinyApp(ui,server)
