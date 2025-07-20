library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)

dashboardPage(
  skin = "blue", 
  dashboardHeader(title = "INUL Dashboard", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen", icon = icon("sliders-h")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-bar")),
      menuItem("Uji Asumsi", icon = icon("flask"),
               menuSubItem("Uji Normalitas", tabName = "normalitas"),
               menuSubItem("Uji Homogenitas", tabName = "homogenitas")),
      menuItem("Statistik Inferensia", icon = icon("calculator"),
               menuSubItem("Uji Beda Rata-rata", tabName = "ratarata"),
               menuSubItem("Uji Proporsi & Varians", tabName = "propvar"),
               menuSubItem("ANOVA", tabName = "anova")),
      menuItem("Regresi Linear Berganda", tabName = "regresi", icon = icon("project-diagram")),
      menuItem("Unduh Laporan Gabungan", tabName = "unduhan", icon = icon("download"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "myStyle.css")),
    tabItems(
      # BERANDA
      tabItem("beranda",
              h2("Selamat Datang di Dashboard Analisis Kerentanan Sosial Indonesia"),
              p("Dashboard ini menyediakan serangkaian alat untuk analisis statistik, mulai dari manajemen data hingga pemodelan regresi, berdasarkan data kerentanan sosial di Indonesia."),
              hr(),
              box(
                title = "Tentang Data", width = 12, collapsible = TRUE, 
                uiOutput("info_summary"), class = "softpink-box"
              ),
              box(
                title = "Ringkasan Rata-rata Indikator", width = 12, collapsible = TRUE, 
                uiOutput("avg_boxes"), class = "softpink-box"
              )
      ),
      # MANAJEMEN DATA
      tabItem("manajemen",
              h2("Manajemen Data: Kategorisasi Variabel"),
              fluidRow(
                box(width = 4, title = "Pilih Variabel", 
                    selectInput("kategori_var", "Pilih Variabel Numerik:", choices = NULL), class = "softpink-box"
                ),
                box(width = 4, title = "Tentukan Jumlah Kategori", 
                    numericInput("kategori_n", "Jumlah Kategori:", value = 3, min = 2, max = 10), class = "softpink-box"
                ),
                box(width = 4, title = "Terapkan", 
                    p("Klik tombol untuk memproses."),
                    actionButton("btn_kategorisasi", "Terapkan Kategorisasi", icon = icon("cogs"), class = "btn-softpink btn-block")
                )
              ),
              fluidRow(
                box(width = 12, title = "Hasil Kategorisasi", collapsible = TRUE, 
                    uiOutput("interpretasi_kategori"),
                    hr(),
                    h4("Tabel Data dengan Variabel Baru"),
                    DTOutput("tabel_kategori"), class = "softpink-box"
                )
              ),
              fluidRow(
                box(width = 12, title = "Unduh Hasil", 
                    p("Unduh data lengkap dengan kolom kategori baru dalam format Excel, atau unduh ringkasan laporan dalam format PDF/Word."),
                    downloadButton("unduh_kategori_csv", "Unduh Data (Excel)", class = "btn-softpink"),
                    hr(),
                    radioButtons("format_kategori_report", "Pilih Format Laporan:", choices = c("PDF", "Word"), inline = TRUE),
                    downloadButton("unduh_kategori_report", "Unduh Laporan", class = "btn-softpink")
                )
              )
      ),
      # EKSPLORASI DATA
      tabItem("eksplorasi",
              h2("Eksplorasi Data"),
              fluidRow(
                box(width = 4, title = "Pengaturan Plot", 
                    selectInput("explore_var", "Pilih Variabel Y:", choices = NULL),
                    selectInput("jenis_plot", "Pilih Jenis Plot:",
                                choices = c("Histogram", "Boxplot", "Density", "Scatter Plot")),
                    conditionalPanel(
                      condition = "input.jenis_plot == 'Scatter Plot'",
                      selectInput("x_var", "Pilih Variabel X:", choices = NULL)
                    ),
                    actionButton("run_eksplorasi", "Tampilkan", icon = icon("play"), class = "btn-softpink btn-block")
                ),
                box(width = 8, title = "Visualisasi Data", 
                    plotOutput("plot_eksplorasi"), class = "softpink-box"
                )
              ),
              fluidRow(
                box(width = 12, title = "Statistik Deskriptif & Interpretasi", collapsible = TRUE, 
                    verbatimTextOutput("desc_table"),
                    uiOutput("explore_interpretation"), class = "softpink-box"
                )
              ),
              fluidRow(
                box(width = 12, title = "Unduh Hasil", 
                    downloadButton("unduh_eksplorasi_plot", "Unduh Grafik (JPG)", class = "btn-softpink"),
                    hr(),
                    radioButtons("format_eksplorasi_report", "Pilih Format Laporan:", choices = c("PDF", "Word"), inline = TRUE),
                    downloadButton("unduh_eksplorasi_report", "Unduh Laporan", class = "btn-softpink")
                )
              )
      ),
      # UJI ASUMSI: UJI NORMALITAS
      tabItem("normalitas",
              h2("Uji Asumsi: Normalitas"),
              fluidRow(
                box(width = 4, title = "Pengaturan Uji", status = "primary", 
                    selectInput("normal_var", "Pilih Variabel:", choices = NULL),
                    selectInput("uji_normalitas", "Pilih Metode Uji:",
                                choices = c("Shapiro-Wilk", "Kolmogorov-Smirnov", "Jarque-Bera", "Chi-Square Goodness of Fit")),
                    actionButton("run_normalitas", "Jalankan Uji", icon = icon("play"), class = "btn-softpink btn-block")
                ),
                box(width = 8, title = "Hasil Uji", status = "primary", 
                    verbatimTextOutput("hasil_normalitas"),
                    uiOutput("normal_interpretasi")
                )
              ),
              fluidRow(
                box(width = 12, title = "Visualisasi Data", status = "info", 
                    plotOutput("hist_normal")
                )
              ),
              fluidRow(
                box(width = 12, title = "Unduh Laporan", status = "warning", 
                    radioButtons("format_normalitas_report", "Pilih Format Laporan:", choices = c("PDF", "Word"), inline = TRUE),
                    downloadButton("unduh_normalitas_report", "Unduh Laporan")
                )
              )
      ),
      # UJI ASUMSI: UJI HOMOGENITAS
      tabItem("homogenitas",
              h2("Uji Asumsi: Homogenitas Varians"),
              fluidRow(
                box(width = 4, title = "Pengaturan Uji", status = "primary",
                    selectInput("dep_var_homo", "Pilih Variabel Dependen (Numerik):", choices = NULL),
                    selectInput("group_var_homo", "Pilih Variabel Grup (Kategorik):", choices = NULL),
                    selectInput("metode_homogenitas", "Pilih Metode Uji:",
                                choices = c("Levene", "Bartlett", "F-test (hanya 2 grup)")),
                    radioButtons("homo_plot_type", "Pilih Jenis Plot:", 
                                 choices = c("Boxplot", "Scatter Plot"), inline = TRUE),
                    actionButton("run_homogenitas", "Jalankan Uji", icon = icon("play"), class = "btn-softpink btn-block")
                ),
                box(width = 8, title = "Hasil Uji", status = "primary",
                    verbatimTextOutput("hasil_homogenitas"),
                    uiOutput("interpretasi_homogenitas")
                )
              ),
              fluidRow(
                box(width = 12, title = "Visualisasi Data", status = "info", 
                    # Nama output plot tetap sama, server akan menyesuaikan isinya
                    plotOutput("plot_scatter_homo")
                )
              ),
              fluidRow(
                box(width = 12, title = "Unduh Laporan", status = "warning",
                    radioButtons("format_homogenitas_report", "Pilih Format Laporan:", choices = c("PDF", "Word"), inline = TRUE),
                    downloadButton("unduh_homogenitas_report", "Unduh Laporan")
                )
              )
      ),
      # STATISTIK INFERENSIA: UJI BEDA RATA-RATA
      tabItem("ratarata",
              h2("Statistik Inferensia: Uji Beda Rata-rata"),
              tabsetPanel(
                type = "tabs",
                tabPanel("Uji t 1 Sampel",
                         br(),
                         fluidRow(
                           box(width = 12, title = "Pengaturan Uji", class = "softpink-box",
                               fluidRow(
                                 column(4, selectInput("rata1_var", "Pilih Variabel Numerik:", choices = NULL)),
                                 column(4, numericInput("rata1_mu", "Nilai Hipotesis (µ₀):", value = 0)),
                                 column(4, div(actionButton("run_rata1", "Jalankan Uji", icon = icon("play"), class = "btn-softpink btn-block"), style = "margin-top: 25px;"))
                               )
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Hasil Uji", class = "softpink-box",
                               verbatimTextOutput("hasil_uji_rata1"),
                               uiOutput("interpret_rata1")
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Visualisasi", class = "softpink-box",
                               plotOutput("plot_rata1")
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Unduh Laporan", class = "softpink-box",
                               radioButtons("format_rata1_report", "Pilih Format:", choices = c("PDF", "Word"), inline = TRUE),
                               downloadButton("unduh_rata1_report", "Unduh Laporan", class = "btn-softpink")
                           )
                         )
                ),
                tabPanel("Uji t 2 Sampel Independen",
                         br(),
                         fluidRow(
                           box(width = 12, title = "Pengaturan Uji", class = "softpink-box",
                               fluidRow(
                                 column(4, selectInput("rata2_var", "Variabel Numerik:", choices = NULL)),
                                 column(4, selectInput("rata2_group", "Variabel Grup (2 Kategori):", choices = NULL)),
                                 column(4, div(actionButton("run_rata2", "Jalankan Uji", icon = icon("play"), class = "btn-softpink btn-block"), style = "margin-top: 25px;"))
                               )
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Hasil Uji", class = "softpink-box",
                               verbatimTextOutput("hasil_uji_rata2"),
                               uiOutput("interpret_rata2")
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Visualisasi", class = "softpink-box",
                               plotOutput("plot_rata2")
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Unduh Laporan", class = "softpink-box",
                               radioButtons("format_rata2_report", "Pilih Format:", choices = c("PDF", "Word"), inline = TRUE),
                               downloadButton("unduh_rata2_report", "Unduh Laporan", class = "btn-softpink")
                           )
                         )
                ),
                tabPanel("Uji t 2 Sampel Berpasangan",
                         br(),
                         fluidRow(
                           box(width = 12, title = "Pengaturan Uji", class = "softpink-box",
                               fluidRow(
                                 column(4, selectInput("rata3_var1", "Variabel 1 (Numerik):", choices = NULL)),
                                 column(4, selectInput("rata3_var2", "Variabel 2 (Numerik):", choices = NULL)),
                                 column(4, div(actionButton("run_rata3", "Jalankan Uji", icon = icon("play"), class = "btn-softpink btn-block"), style = "margin-top: 25px;"))
                               )
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Hasil Uji", class = "softpink-box",
                               verbatimTextOutput("hasil_uji_rata3"),
                               uiOutput("interpret_rata3")
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Visualisasi", class = "softpink-box",
                               plotOutput("plot_rata3")
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Unduh Laporan", class = "softpink-box",
                               radioButtons("format_rata3_report", "Pilih Format:", choices = c("PDF", "Word"), inline = TRUE),
                               downloadButton("unduh_rata3_report", "Unduh Laporan", class = "btn-softpink")
                           )
                         )
                )
              )
      ),
      # STATISTIK INFERENSIA: UJI PROPORSI DAN VARIANS
      tabItem("propvar",
              h2("Statistik Inferensia: Uji Proporsi & Varians"),
              tabsetPanel(
                type = "tabs",
                tabPanel("Uji Proporsi 1 Sampel",
                         br(),
                         fluidRow(
                           box(width = 12, title = "Pengaturan Uji", class = "softpink-box",
                               fluidRow(
                                 column(3, selectInput("prop1_var", "Variabel Kategorik:", choices = NULL)),
                                 column(3, uiOutput("prop1_level_ui")),
                                 column(3, numericInput("prop1_p", "Nilai Hipotesis (p₀):", value = 0.5, min = 0, max = 1, step = 0.01)),
                                 column(3, div(actionButton("run_prop1", "Jalankan", icon("play"), class="btn-softpink btn-block"), style = "margin-top: 25px;"))
                               )
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Hasil Uji", class = "softpink-box",
                               verbatimTextOutput("hasil_prop1"),
                               uiOutput("interpret_prop1")
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Visualisasi", class = "softpink-box",
                               plotOutput("plot_prop1")
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Unduh Laporan", class = "softpink-box",
                               radioButtons("format_propvar_report1", "Pilih Format:", choices = c("PDF", "Word"), inline = TRUE),
                               downloadButton("unduh_propvar_report1", "Unduh Laporan", class = "btn-softpink")
                           )
                         )
                ),
                tabPanel("Uji Proporsi 2 Sampel",
                         br(),
                         fluidRow(
                           box(width = 12, title = "Pengaturan Uji", class = "softpink-box",
                               fluidRow(
                                 column(6, selectInput("prop2_var_cat", "Pilih Variabel Grup:", choices = NULL)),
                                 column(6, uiOutput("prop2_success_ui"))
                               ),
                               fluidRow(
                                 column(6, uiOutput("prop2_level1_ui")),
                                 column(6, uiOutput("prop2_level2_ui"))
                               ),
                               fluidRow(
                                 column(12, div(actionButton("run_prop2", "Jalankan Uji", class = "btn-softpink btn-block"), style = "margin-top: 15px;"))
                               )
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Hasil Uji", class = "softpink-box",
                               verbatimTextOutput("hasil_prop2"),
                               uiOutput("interpret_prop2")
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Visualisasi", class = "softpink-box",
                               plotOutput("plot_prop2")
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Unduh Laporan", class = "softpink-box",
                               radioButtons("format_propvar_report2", "Pilih Format:", choices = c("PDF", "Word"), inline = TRUE),
                               downloadButton("unduh_propvar_report2", "Unduh Laporan", class = "btn-softpink")
                           )
                         )
                ),
                tabPanel("Uji Varians 1 Sampel",
                         br(),
                         fluidRow(
                           box(width = 12, title = "Pengaturan Uji", class = "softpink-box",
                               fluidRow(
                                 column(4, selectInput("var1_var", "Variabel Numerik:", choices = NULL)),
                                 column(4, numericInput("var1_val", "Nilai Hipotesis (σ²):", value = 1, min = 0)),
                                 column(4, div(actionButton("run_var1", "Jalankan", icon("play"), class="btn-softpink btn-block"), style = "margin-top: 25px;"))
                               )
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Hasil Uji", class = "softpink-box",
                               verbatimTextOutput("hasil_var1"),
                               uiOutput("interpret_var1")
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Visualisasi", class = "softpink-box",
                               plotOutput("plot_var1")
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Unduh Laporan", class = "softpink-box",
                               radioButtons("format_propvar_report3", "Pilih Format:", choices = c("PDF", "Word"), inline = TRUE),
                               downloadButton("unduh_propvar_report3", "Unduh Laporan", class = "btn-softpink")
                           )
                         )
                ),
                tabPanel("Uji Varians 2 Sampel (F-test)",
                         br(),
                         fluidRow(
                           box(width = 12, title = "Pengaturan Uji", class = "softpink-box",
                               fluidRow(
                                 column(6, selectInput("var2_var_num", "Pilih Variabel Numerik:", choices = NULL)),
                                 column(6, selectInput("var2_var_cat", "Pilih Variabel Grup:", choices = NULL))
                               ),
                               fluidRow(
                                 column(6, uiOutput("var2_level1_ui")),
                                 column(6, uiOutput("var2_level2_ui"))
                               ),
                               fluidRow(
                                 column(12, div(actionButton("run_var2", "Jalankan Uji", class = "btn-softpink btn-block"), style = "margin-top: 15px;"))
                               )
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Hasil Uji", class = "softpink-box",
                               verbatimTextOutput("hasil_var2"),
                               uiOutput("interpret_var2")
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Visualisasi", class = "softpink-box",
                               plotOutput("plot_var2")
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Unduh Laporan", class = "softpink-box",
                               radioButtons("format_propvar_report4", "Pilih Format:", choices = c("PDF", "Word"), inline = TRUE),
                               downloadButton("unduh_propvar_report4", "Unduh Laporan", class = "btn-softpink")
                           )
                         )
                )
              )
      ),
      # STATISTIK INFERENSIA: ANOVA
      tabItem("anova",
              h2("Statistik Inferensia: Analysis of Variance (ANOVA)"),
              fluidRow(
                box(width = 4, title = "Pengaturan Uji ANOVA", status = "primary", 
                    radioButtons("anova_type", "Pilih Tipe ANOVA:", choices = c("Satu Arah (One-Way)", "Dua Arah (Two-Way)"), inline = TRUE),
                    selectInput("anova_y", "Variabel Dependen (Y):", choices = NULL),
                    selectInput("anova_group1", "Variabel Grup 1 (X1):", choices = NULL),
                    conditionalPanel(
                      condition = "input.anova_type == 'Dua Arah (Two-Way)'",
                      selectInput("anova_group2", "Variabel Grup 2 (X2):", choices = NULL)
                    ),
                    actionButton("run_anova", "Jalankan Uji", icon = icon("play"), class = "btn-softpink btn-block")
                ),
                box(width = 8, title = "Hasil Uji ANOVA", status = "info", 
                    plotOutput("plot_anova"),
                    hr(),
                    verbatimTextOutput("hasil_anova"),
                    uiOutput("interpret_anova")
                )
              ),
              fluidRow(
                box(width = 12, title = "Unduh Laporan", status = "warning", 
                    radioButtons("format_anova_report", "Pilih Format Laporan:", choices = c("PDF", "Word"), inline = TRUE),
                    downloadButton("unduh_anova_report", "Unduh Laporan")
                )
              )
      ),
      # REGRESI LINEAR BERGANDA
      tabItem("regresi",
              h2("Regresi Linear Berganda"),
              fluidRow(
                box(width = 12, title = "Pengaturan Model", class = "softpink-box",
                    fluidRow(
                      column(6, selectInput("regresi_y", "Pilih Variabel Dependen (Y):", choices = NULL)),
                      column(6, selectInput("regresi_x", "Pilih Variabel Independen (X):", choices = NULL, multiple = TRUE))
                    ),
                    fluidRow(
                      column(12, div(actionButton("run_regresi", "Jalankan Model", icon("play"), class = "btn-softpink btn-block"), style = "margin-top: 15px;"))
                    )
                )
              ),
              fluidRow(
                box(width = 12, title = "Hasil Analisis", class = "softpink-box",
                    tabBox(width = 12,
                           tabPanel("Hasil Model",
                                    verbatimTextOutput("hasil_regresi"),
                                    uiOutput("interpret_regresi")),
                           tabPanel("Uji Asumsi",
                                    h4("Hasil Pemeriksaan Asumsi Klasik"),
                                    uiOutput("asumsi_regresi_results")),
                           tabPanel("Plot Diagnostik",
                                    plotOutput("plot_asumsi_regresi", height = "600px"))
                    )
                )
              ),
              fluidRow(
                box(width = 12, title = "Unduh Laporan", class = "softpink-box",
                    radioButtons("format_regresi_report", "Pilih Format Laporan:", choices = c("PDF", "Word"), inline = TRUE),
                    downloadButton("unduh_regresi_report", "Unduh Laporan", class = "btn-softpink")
                )
              )
      ),
      # UNDUH GABUNGAN
      tabItem("unduhan",
              h3("Unduhan Gabungan"),
              p("Fitur ini merekam semua analisis yang Anda lakukan selama sesi ini. Anda dapat mengunduh rekaman ini sebagai laporan aktivitas."),
              box(width = 12, status = "primary",
                  title = "Riwayat Aktivitas Sesi Ini",
                  verbatimTextOutput("tampilan_histori"),
                  hr(),
                  p("Pilih format dan klik tombol di bawah untuk mengunduh riwayat aktivitas."),
                  fluidRow(
                    column(6, radioButtons("format_gabungan_report", "Pilih Format:", 
                                           choices = c("PDF", "Word"), inline = TRUE, selected = "PDF")),
                    column(6, downloadButton("unduh_gabungan", "Unduh Histori Sesi", class = "btn-softpink btn-lg"))
                  )
              )
      )
    )
  )
)