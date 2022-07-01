library(shiny)
library(shinydashboard)
library(highcharter)
library(C50)

ui <- dashboardPage(
  dashboardHeader(title = "Prediksi Penyakit Jantung"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visualisasi", tabName = "visual", icon = icon("dashboard")),
      menuItem("Prediksi", tabName = "model", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "visual",
              box(
                title = "ScatterPlot",
                highchartOutput("sp")
              ),
              box(
                title = "Histogram",
                highchartOutput("hist")
              )
      ),
      
      # Second tab content
      tabItem(tabName = "model",
              box(width = 5,
                  textInput("nama", "Masukkan nama:"),
                  numericInput("age", "Masukkan Umur", 0, 120, 50),
                  selectInput("sex", "Pilih Jenis Kelamin", c("Pria" = factor(0),
                                                              "Perempuan" = factor(1))),
                  selectInput("cp", "Pilih Jenis CP", c("0" = factor(0),
                                                        "1" = factor(1),
                                                        "2" = factor(2),
                                                        "3" = factor(3))),
                  numericInput("trtbps", "Masukkan trtbps", 0),
                  numericInput("chol", "Masukkan chol", 0)
              ),
              box(width = 5,
                  selectInput("fbs", "Pilih fbs", c("0" = factor(0),
                                                    "1" = factor(1))),
                  selectInput("restecg", "Pilih restecg", c("0" = factor(0),
                                                            "1" = factor(1),
                                                            "2" = factor(2))),
                  numericInput("thalach", "Masukkan thalach", 0),
                  selectInput("exng", "Pilih exng", c("0" = factor(0),
                                                      "1" = factor(1))),
                  numericInput("oldpeak", "Masukkan Oldpeak", 0)
              ),
              box(
                width = 2,
                textOutput("hasil")
              ),
                box(
                  width = 10,
                  textOutput("legenda"),
                  print("||Cp :  jenis nyeri dada jenis nyeri dada.
                Nilai 1: angina tipikal.
                Nilai 2: angina atipikal.
                Nilai 3: nyeri non-angina.
                Nilai 4: tanpa gejala.||"),
                  print("||trtbps : tekanan darah (dalam mm Hg)||"),
                  print("||chol : kolestoral dalam mg/dl diambil melalui sensor BMI||"),
                  print("||fbs : (gula darah diatas 120 mg/dl) (1 = benar; 0 = salah||"),
                  print("||restecg : hasil elektrokardiografi istirahat
                        Nilai 0: biasa
                        Nilai 1: memiliki kelainan gelombang ST-T (pembalikan gelombang T dan/atau elevasi atau depresi ST > 0,05 mV)
                        Nilai 2: menunjukkan kemungkinan atau pasti hipertrofi ventrikel kiri menurut kriteria Estes||"),
                  print("||thalach : detak jantung maksimum tercapai||"),
                  print("||exang: angina yang diinduksi olahraga (1 = ya; 0 = tidak)||")
                )
      )
    )
  )
)

server <- function(input, output) {
  data_ha <- read.csv("data/heart.csv")
  
  output$sp <- renderHighchart({
    #data vis 1
    hchart(data_ha, "scatter", hcaes(x = trtbps, y = chol, group = sex))
  })
  
  output$hist <- renderHighchart({
    hchart(data_ha$age, name = "data") 
  })
  
  
  output$hasil <- renderText({
    
    model_c50 <- readRDS("data/model_c50.RDS")
    
    
    data_baru <- data.frame(
      nama = input$nama,
      age = input$age,
      sex = input$sex,
      cp  = input$cp,
      trtbps =  input$trtbps,
      chol = input$chol,
      fbs = input$fbs,
      restecg = input$restecg,
      thalachh = input$thalach,
      exng = input$exng,
      oldpeak = input$oldpeak
    )
    if(predict(model_c50, data_baru) == 0){
      print(paste(data_baru[1,1], "beresiko rendah menderita penyakit jantung"))
    } else{
      print(paste(data_baru[1,1], "beresiko tinggi menderita penyakit jantung"))  
    }
  })
  output$legenda <- renderText(
    print("Keterangan")
  )
}

shinyApp(ui, server)