library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(shinyjs)
library(readr)
library(car)
library(nortest)
library(broom)
library(openxlsx)
library(rmarkdown)
library(knitr)
library(tseries)
library(gridExtra)

sovi_data <- read_csv("sovi_data.csv")
distance <- read_csv("distance.csv")

# Metadata variabel
metadata <- data.frame(
  Label = c("DISTRICTCODE", "CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", "NOELECTRIC", "LOWEDU",
            "GROWTH", "POVERTY", "ILLITERATE", "NOTRAINING", "DPRONE", "RENTED", "NOSEWER", "TAPWATER", "POPULATION"),
  Variable = c("District Code", "Children", "Female", "Elderly", "Female household", "Household members", "Non-electric household", "Low education",
               "Population growth", "Poverty", "Illiteracy", "Training", "Disaster prone", "Homeownership", "Drainage", "Water source", "Population"),
  Description = c("Kode unik untuk setiap kabupaten/kota", "Persentase populasi di bawah lima tahun", "Persentase populasi perempuan",
                  "Persentase populasi berusia 65 tahun ke atas", "Persentase rumah tangga dengan kepala keluarga perempuan",
                  "Rata-rata jumlah anggota rumah tangga", "Persentase rumah tangga tanpa akses listrik",
                  "Persentase populasi 15 tahun ke atas dengan pendidikan rendah", "Persentase perubahan populasi", "Persentase penduduk miskin",
                  "Persentase populasi yang buta huruf", "Persentase rumah tangga yang tidak pernah mendapat pelatihan bencana",
                  "Persentase rumah tangga yang tinggal di area rawan bencana", "Persentase rumah tangga yang menyewa rumah",
                  "Persentase rumah tangga tanpa sistem drainase", "Persentase rumah tangga yang menggunakan air ledeng",
                  "Jumlah populasi")
)

# --- FUNGSI BANTUAN ---
interpret_pval <- function(pval, alpha = 0.05) {
  if (is.na(pval)) return("Hasil tidak tersedia.")
  if (pval <= alpha) {
    paste0("Karena p-value (", format.pval(pval, digits = 3, eps = 0.001), ") \u2264 ", alpha, ", maka H\u2080 ditolak.")
  } else {
    paste0("Karena p-value (", format.pval(pval, digits = 3, eps = 0.001), ") > ", alpha, ", maka H\u2080 gagal ditolak.")
  }
}

# --- SERVER LOGIC ---
shinyServer(function(input, output, session) {
  
  histori_sesi <- reactiveVal(character(0))
  catat_histori <- function(pesan) {
    catatan_baru <- paste(format(Sys.time(), "%H:%M:%S"), "-", pesan)
    histori_lama <- histori_sesi()
    histori_sesi(c(histori_lama, catatan_baru))
  }
  
  values <- reactiveValues(data = sovi_data, kategorisasi_info = NULL, hasil_unduhan = list(), saved_plots = list())
  
  # Dapatkan nama-nama variabel numerik dan semua variabel dari data awal
  numeric_vars_initial <- names(sovi_data)[sapply(sovi_data, is.numeric)]
  categorical_vars_initial <- names(sovi_data)[sapply(sovi_data, function(x) is.character(x) || is.factor(x) || length(unique(x)) < 15)]
  all_vars_initial <- names(sovi_data)
  
  # Inisialisasi semua input select (dijalankan sekali saat sesi dimulai)
  # Inisialisasi semua input select (dijalankan sekali saat sesi dimulai)
  observe({
    updateSelectInput(session, "kategori_var", choices = numeric_vars_initial)
    updateSelectInput(session, "explore_var", choices = numeric_vars_initial)
    updateSelectInput(session, "x_var", choices = categorical_vars_initial)
    updateSelectInput(session, "normal_var", choices = numeric_vars_initial)
    updateSelectInput(session, "dep_var_homo", choices = numeric_vars_initial)
    updateSelectInput(session, "group_var_homo", choices = categorical_vars_initial) # Ditambahkan untuk homogenitas
    updateSelectInput(session, "rata1_var", choices = numeric_vars_initial)
    
    # Untuk Uji-t 2 sampel, proporsi 2 sampel, dan varians 2 sampel,
    # kita hanya perlu memilih variabel numerik dan grupnya di sini.
    # Pilihan level grup akan di-update secara dinamis.
    updateSelectInput(session, "rata2_var", choices = numeric_vars_initial)
    updateSelectInput(session, "rata2_group", choices = categorical_vars_initial)
    updateSelectInput(session, "prop2_var_cat", choices = categorical_vars_initial)
    updateSelectInput(session, "var2_var_num", choices = numeric_vars_initial)
    updateSelectInput(session, "var2_var_cat", choices = categorical_vars_initial)
    
    updateSelectInput(session, "rata3_var1", choices = numeric_vars_initial)
    updateSelectInput(session, "rata3_var2", choices = numeric_vars_initial)
    updateSelectInput(session, "prop1_var", choices = categorical_vars_initial)
    updateSelectInput(session, "var1_var", choices = numeric_vars_initial)
    updateSelectInput(session, "anova_y", choices = numeric_vars_initial)
    updateSelectInput(session, "anova_group1", choices = categorical_vars_initial)
    updateSelectInput(session, "anova_group2", choices = categorical_vars_initial)
    updateSelectInput(session, "regresi_y", choices = numeric_vars_initial)
    updateSelectInput(session, "regresi_x", choices = numeric_vars_initial, selected = numeric_vars_initial[numeric_vars_initial != "DISTRICTCODE" & numeric_vars_initial != "POPULATION"][1:min(4, length(numeric_vars_initial)-2)])
  })
  
  # FIX: Observer khusus untuk memperbarui pilihan variabel X secara reaktif
  observe({
    req(input$explore_var)
    current_y_var <- input$explore_var
    # Pilihan untuk variabel X adalah semua variabel numerik KECUALI yang sedang dipakai untuk Y
    x_var_choices <- setdiff(names(values$data)[sapply(values$data, is.numeric)], current_y_var)
    updateSelectInput(session, "x_var", choices = x_var_choices)
  })
  
  # Observer untuk memperbarui pilihan level pada Uji Proporsi 1 Sampel
  observeEvent(input$prop1_var, {
    req(input$prop1_var)
    if (input$prop1_var %in% names(values$data)) {
      levels_prop1 <- unique(values$data[[input$prop1_var]])
      output$prop1_level_ui <- renderUI({
        selectInput("prop1_level", "Pilih Level:", choices = levels_prop1)
      })
    }
  })
  
  render_report <- function(input_file, output_file, params, output_format_type) {
    tempReport <- file.path(tempdir(), basename(input_file))
    file.copy(input_file, tempReport, overwrite = TRUE)
    tryCatch({
      rmarkdown::render(
        tempReport,
        output_file = output_file,
        params = params,
        output_format = output_format_type,
        envir = new.env(parent = globalenv())
      )
      TRUE
    }, error = function(e) {
      showNotification(
        paste0("Gagal membuat laporan PDF/Word. Pastikan Pandoc dan LaTeX (untuk PDF) sudah terinstall. Pesan error: ", e$message),
        type = "error"
      )
      FALSE
    })
  }
  
  # Helper untuk format output robust
  get_report_format <- function(input_val) {
    val <- toupper(as.character(input_val))
    if (val == "PDF") return(rmarkdown::pdf_document())
    if (val == "WORD") return(rmarkdown::word_document())
    rmarkdown::pdf_document() # default
  }
  
  # Helper untuk ambil plain text dari renderUI
  get_interpretasi_text <- function(ui_output) {
    if (inherits(ui_output, "shiny.tag.list") || inherits(ui_output, "shiny.tag")) {
      paste0(as.character(ui_output))
    } else if (is.character(ui_output)) {
      ui_output
    } else {
      ""
    }
  }
  
  # Beranda
  output$info_summary <- renderUI({ 
    tagList(
      p("Data ini berasal dari Survei Sosial Ekonomi Nasional (SUSENAS) 2017 dan Proyeksi Penduduk 2017 oleh BPS."),
      tags$ul(
        tags$li("Terdapat 511 kabupaten/kota dengan 17 indikator kerentanan sosial."),
        tags$li("Indikator mencakup aspek: demografi (anak-anak, lansia, perempuan), ekonomi (kemiskinan, pendidikan), dan ketahanan terhadap bencana."),
        tags$li("Disediakan pula matriks jarak antar wilayah untuk keperluan analisis spasial."),
        tags$li("Data dikalibrasi terhadap peta administratif 2013.")
      )
    ) 
  })
  summary_cards <- reactive({
    sovi_data %>%
      select(where(is.numeric)) %>%
      summarise(across(everything(), list(mean = mean), na.rm = TRUE)) %>%
      pivot_longer(cols = everything(), names_to = "Label", values_to = "Mean") %>%
      mutate(Label = gsub("_mean", "", Label)) %>%
      left_join(metadata, by = "Label") %>%
      filter(!(Label %in% c("DISTRICTCODE", "POPULATION")))
  })
  output$avg_boxes <- renderUI({
    df <- summary_cards()
    req(nrow(df) > 0)
    
    box_ui <- lapply(1:nrow(df), function(i) {
      div(class = "overview-box",
          HTML(paste0(
            "<b>", df$Label[i], "</b>: ", sprintf("%.2f", df$Mean[i]), "%<br/>",
            "<b>", df$Variable[i], "</b><br/>",
            df$Description[i]
          ))
      )
    })
    
    div(class = "box-grid", box_ui)
  })
  
  
  # Manajemen Data
  kategori_results <- eventReactive(input$btn_kategorisasi, {
    req(input$kategori_var, input$kategori_n)
    catat_histori(paste("Kategorisasi variabel:", input$kategori_var, "dengan", input$kategori_n, "kategori"))
    var_to_cat <- input$kategori_var
    num_cat <- as.integer(input$kategori_n)
    
    df <- values$data # Gunakan data dari reactive value
    if (!var_to_cat %in% names(df) || !is.numeric(df[[var_to_cat]])) {
      showNotification("Variabel yang dipilih tidak numerik atau tidak ditemukan.", type = "error")
      return(NULL)
    }
    
    cutpoints <- unique(quantile(df[[var_to_cat]], probs = seq(0, 1, length.out = num_cat + 1), na.rm = TRUE))
    if (length(cutpoints) < 2) {
      showNotification("Tidak cukup variasi dalam data untuk membuat kategori. Coba variabel lain atau jumlah kategori yang berbeda.", type = "error")
      return(NULL)
    }
    labels <- paste0("K", 1:num_cat)
    # Sesuaikan labels jika jumlah cutpoints tidak sesuai dengan num_cat + 1 (misal ada nilai duplikat di quantile)
    if (length(cutpoints) - 1 < num_cat) {
      labels <- paste0("K", 1:(length(cutpoints)-1))
    }
    
    new_col_name <- paste0(var_to_cat, "_", num_cat, "cat")
    df[[new_col_name]] <- cut(df[[var_to_cat]], breaks = cutpoints, include.lowest = TRUE, labels = labels)
    
    values$data <- df # Update reactive value
    
    info <- list(var = var_to_cat, n = num_cat, new_col = new_col_name, labels = labels, cutpoints = cutpoints)
    values$kategorisasi_info <- info
    
    # Update inputs in other menus
    # Perbarui pilihan variabel setelah data berubah
    updated_numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
    updated_factor_vars <- names(values$data)[sapply(values$data, is.factor)]
    updated_categorical_vars <- names(values$data)[sapply(values$data, function(x) is.character(x) || is.factor(x) || length(unique(x)) < 15)]
    
    updateSelectInput(session, "kategori_var", choices = updated_numeric_vars)
    updateSelectInput(session, "explore_var", choices = updated_numeric_vars)
    updateSelectInput(session, "x_var", choices = updated_categorical_vars)
    updateSelectInput(session, "normal_var", choices = updated_numeric_vars)
    updateSelectInput(session, "dep_var_homo", choices = updated_numeric_vars)
    updateSelectInput(session, "rata1_var", choices = updated_numeric_vars)
    updateSelectInput(session, "rata2_var", choices = updated_numeric_vars)
    updateSelectInput(session, "rata2_group", choices = updated_categorical_vars)
    updateSelectInput(session, "rata3_var1", choices = updated_numeric_vars)
    updateSelectInput(session, "rata3_var2", choices = updated_numeric_vars)
    updateSelectInput(session, "prop1_var", choices = updated_categorical_vars)
    updateSelectInput(session, "prop2_var_cat", choices = updated_categorical_vars)
    updateSelectInput(session, "var1_var", choices = updated_numeric_vars)
    updateSelectInput(session, "var2_var_num", choices = updated_numeric_vars)
    updateSelectInput(session, "var2_var_cat", choices = updated_categorical_vars)
    updateSelectInput(session, "anova_y", choices = updated_numeric_vars)
    updateSelectInput(session, "anova_group1", choices = updated_categorical_vars)
    updateSelectInput(session, "anova_group2", choices = updated_categorical_vars)
    updateSelectInput(session, "regresi_y", choices = updated_numeric_vars)
    updateSelectInput(session, "regresi_x", choices = updated_numeric_vars, selected = setdiff(updated_numeric_vars, input$regresi_y)[1:min(4, length(setdiff(updated_numeric_vars, input$regresi_y)))])
    
    
    # Only update group variables if they are factors
    updateSelectInput(session, "group_var_homo", choices = updated_factor_vars, selected = new_col_name)
    updateSelectInput(session, "rata2_group", choices = updated_factor_vars[sapply(df[,updated_factor_vars, drop=FALSE], function(x) nlevels(x) == 2)])
    
    showNotification(paste("Variabel", var_to_cat, "berhasil dikategorikan."), type = "message")
    return(info)
  })
  
  output$interpretasi_kategori <- renderUI({
    info <- kategori_results()
    req(info)
    # Format cutpoints untuk tampilan yang lebih baik, terutama jika ada banyak desimal
    formatted_cutpoints <- sprintf("%.2f", info$cutpoints)
    # Membuat rentang label dengan penanganan jika ada cutpoint yang berulang (misal karena data sangat sedikit variasi)
    rentang_list <- lapply(1:(length(info$cutpoints) - 1), function(i) {
      paste0("<li><b>", info$labels[i], "</b>: ", formatted_cutpoints[i], " s.d. ", formatted_cutpoints[i + 1], "</li>")
    })
    HTML(paste0("<p>Variabel <b>'", info$var, "'</b> telah dikategorikan menjadi <b>", info$n, "</b> grup dalam kolom <b>'", info$new_col, "'</b>.</p><b>Rentang nilai:</b><ul>", paste(rentang_list, collapse = ""), "</ul>"))
  })
  
  output$tabel_kategori <- renderDT({ datatable(values$data, options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE) })
  
  # --- Unduhan Manajemen Data ---
  output$unduh_kategori_csv <- downloadHandler(
    filename = function() { paste0("data_kategorisasi_", Sys.Date(), ".xlsx") }, # Changed to xlsx
    content = function(file) {
      write.xlsx(values$data, file, rowNames = FALSE) # Using write.xlsx from openxlsx
    }
  )
  output$unduh_kategori_report <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_kategori_report), 
                    "PDF" = ".pdf", 
                    "WORD" = ".docx",
                    ".pdf")  # default
      paste0('laporan_kategorisasi_', Sys.Date(), ext)
    },
    content = function(file) {
      req(values$kategorisasi_info)
      info <- kategori_results()
      req(info)
      
      # Buat interpretasi dalam format text
      formatted_cutpoints <- sprintf("%.2f", info$cutpoints)
      rentang_list <- lapply(1:(length(info$cutpoints) - 1), function(i) {
        paste0("- ", info$labels[i], ": ", formatted_cutpoints[i], " s.d. ", formatted_cutpoints[i + 1])
      })
      interpretasi_text <- paste0("Variabel '", info$var, "' telah dikategorikan menjadi ", info$n, " grup dalam kolom '", info$new_col, "'.\n\nRentang nilai:\n", paste(rentang_list, collapse = "\n"))
      
      params <- list(
        var_name = values$kategorisasi_info$var,
        num_cat = values$kategorisasi_info$n,
        new_col_name = values$kategorisasi_info$new_col,
        interpretation_text = interpretasi_text,
        head_data = head(values$data)
      )
      
      # Render ke temporary file dulu
      temp_file <- tempfile(fileext = switch(toupper(input$format_kategori_report), 
                                             "PDF" = ".pdf", 
                                             "WORD" = ".docx",
                                             ".pdf"))
      
      ok <- render_report("kategori_report.Rmd", temp_file, params,
                          switch(toupper(input$format_kategori_report), 
                                 "PDF" = rmarkdown::pdf_document(), 
                                 "WORD" = rmarkdown::word_document(),
                                 rmarkdown::pdf_document()))
      
      if (!ok || !file.exists(temp_file) || file.size(temp_file) == 0) {
        showNotification("Gagal membuat file PDF/Word. Pastikan Pandoc dan LaTeX (untuk PDF) sudah terinstall.", type = "error")
        return()
      }
      
      # Copy ke file download
      file.copy(temp_file, file, overwrite = TRUE)
    },
    contentType = function() {
      switch(
        toupper(input$format_kategori_report),
        "PDF" = "application/pdf",
        "WORD" = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        "application/pdf"
      )
    }
  )
  
  # Eksplorasi Data
  explore_results <- eventReactive(input$run_eksplorasi, {
    req(input$explore_var, input$jenis_plot)
    catat_histori(paste("Eksplorasi Data:", input$jenis_plot, "pada variabel Y", input$explore_var, if (!is.null(input$x_var)) paste("dan X", input$x_var) else ""))
    plot_data <- values$data
    
    p <- NULL
    plot_title <- ""
    desc_summary <- NULL
    correlation_text <- NULL
    
    if (input$explore_var %in% names(plot_data)) {
      if (input$jenis_plot == "Histogram") {
        p <- ggplot(plot_data, aes_string(x = input$explore_var)) +
          geom_histogram(bins = 30, fill = "#f78fb3", color = "#fff0f5") +
          labs(title = paste("Histogram", input$explore_var)) +
          theme_minimal()
        plot_title <- paste("Histogram untuk", input$explore_var)
      } else if (input$jenis_plot == "Boxplot") {
        p <- ggplot(plot_data, aes_string(y = input$explore_var)) +
          geom_boxplot(fill = "#fbd6e3", color = "#d65076") +
          labs(title = paste("Boxplot", input$explore_var)) +
          theme_minimal()
        plot_title <- paste("Boxplot untuk", input$explore_var)
      } else if (input$jenis_plot == "Density") {
        p <- ggplot(plot_data, aes_string(x = input$explore_var)) +
          geom_density(fill = "#fbd6e3", color = "#d65076") +
          labs(title = paste("Density Plot", input$explore_var)) +
          theme_minimal()
        plot_title <- paste("Density Plot untuk", input$explore_var)
      } else if (input$jenis_plot == "Scatter Plot") {
        req(input$x_var)
        if (input$x_var %in% names(plot_data) && is.numeric(plot_data[[input$x_var]])) {
          p <- ggplot(plot_data, aes_string(x = input$x_var, y = input$explore_var)) +
            geom_point(alpha = 0.7, color = "#d65076") +
            labs(title = paste("Scatter Plot", input$explore_var, "vs", input$x_var)) +
            theme_minimal()
          plot_title <- paste("Scatter Plot untuk", input$explore_var, "dan", input$x_var)
          correlation_value <- cor(plot_data[[input$explore_var]], plot_data[[input$x_var]], use="complete.obs")
          correlation_text <- paste("Korelasi:", sprintf("%.3f", correlation_value))
        } else {
          showNotification("Variabel X untuk Scatter Plot tidak valid atau tidak numerik.", type = "error")
        }
      }
      
      desc_summary_y <- capture.output(print(summary(plot_data[[input$explore_var]])))
      desc_summary <- paste0("Ringkasan Statistik untuk Variabel Y (", input$explore_var, "):\n",
                             paste(desc_summary_y, collapse = "\n"))
      
      if (input$jenis_plot == "Scatter Plot" && !is.null(input$x_var)) {
        desc_summary_x <- capture.output(print(summary(plot_data[[input$x_var]])))
        desc_summary <- paste0(desc_summary, "\n\nRingkasan Statistik untuk Variabel X (", input$x_var, "):\n",
                               paste(desc_summary_x, collapse = "\n"), "\n\n", correlation_text)
      }
    } else {
      showNotification("Variabel yang dipilih tidak ditemukan.", type = "error")
    }
    
    list(plot = p, explore_var = input$explore_var, x_var = input$x_var,
         jenis_plot = input$jenis_plot, desc_summary = desc_summary,
         plot_title = plot_title)
  })
  
  output$plot_eksplorasi <- renderPlot({ explore_results()$plot })
  output$desc_table <- renderPrint({ cat(explore_results()$desc_summary) })
  output$explore_interpretation <- renderUI({
    res <- explore_results()
    if (is.null(res$desc_summary)) {
      HTML("<p>Tidak ada hasil untuk ditampilkan.</p>")
    } else {
      HTML(paste0("<p>Analisis eksplorasi untuk variabel <b>", res$explore_var, "</b> menunjukkan hal berikut:</p>",
                  "<p>Data visualisasikan menggunakan <b>", res$jenis_plot, "</b>. Interpretasi ringkasan statistik dan bentuk distribusi dapat dilihat dari grafik dan ringkasan di atas.</p>"))
    }
  })
  
  
  # --- Unduhan Eksplorasi Data ---
  output$unduh_eksplorasi_plot <- downloadHandler(
    filename = function() { paste0("plot_eksplorasi_", input$explore_var, "_", Sys.Date(), ".jpg") },
    content = function(file) {
      res <- explore_results()
      if (!is.null(res$plot)) {
        ggsave(file, plot = res$plot, device = "jpeg", width = 8, height = 6, units = "in")
      } else {
        stop("Tidak ada plot untuk diunduh.")
      }
    }
  )
  
  output$unduh_eksplorasi_report <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_eksplorasi_report), 
                    "PDF" = ".pdf", 
                    "WORD" = ".docx",
                    ".pdf")  # default
      paste0('laporan_eksplorasi_', Sys.Date(), ext)
    },
    content = function(file) {
      res <- explore_results()
      req(res)
      
      # Buat plot temporary
      plot_path <- tempfile(fileext = ".png")
      if (!is.null(res$plot)) {
        ggsave(plot_path, plot = res$plot, device = "png", width = 8, height = 6, units = "in")
      } else {
        plot_path <- ""
      }
      
      # Buat interpretasi dalam format text
      interpretasi_text <- paste0("Analisis eksplorasi untuk variabel ", res$explore_var, " menunjukkan hal berikut:\n",
                                  "Data divisualisasikan menggunakan ", res$jenis_plot, ". Interpretasi ringkasan statistik dan bentuk distribusi dapat dilihat dari grafik dan ringkasan di atas.")
      
      params <- list(
        var_y = res$explore_var,
        var_x = if(res$jenis_plot == "Scatter Plot") res$x_var else NULL,
        plot_type = res$jenis_plot,
        summary_text = res$desc_summary,
        interpretation_text = interpretasi_text,
        plot_path = plot_path
      )
      
      # Render ke temporary file dulu
      temp_file <- tempfile(fileext = switch(toupper(input$format_eksplorasi_report), 
                                             "PDF" = ".pdf", 
                                             "WORD" = ".docx",
                                             ".pdf"))
      
      ok <- render_report("eksplorasi_report.Rmd", temp_file, params, 
                          switch(toupper(input$format_eksplorasi_report), 
                                 "PDF" = rmarkdown::pdf_document(), 
                                 "WORD" = rmarkdown::word_document(),
                                 rmarkdown::pdf_document()))
      
      if (!ok || !file.exists(temp_file) || file.size(temp_file) == 0) {
        showNotification("Gagal membuat file PDF/Word. Pastikan Pandoc dan LaTeX (untuk PDF) sudah terinstall.", type = "error")
        return()
      }
      
      # Copy ke file download
      file.copy(temp_file, file, overwrite = TRUE)
    },
    contentType = function() {
      switch(
        toupper(input$format_eksplorasi_report),
        "PDF" = "application/pdf",
        "WORD" = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        "application/pdf"
      )
    }
  )
  
  
  # --- Uji Normalitas ---
  normal_test_results <- eventReactive(input$run_normalitas, {
    req(input$normal_var)
    catat_histori(paste("Uji Normalitas:", input$uji_normalitas, "pada variabel", input$normal_var))
    x <- na.omit(values$data[[input$normal_var]])
    if (length(x) < 5) { # Shapiro-Wilk requires at least 3, some other tests might need more
      showNotification("Jumlah data terlalu sedikit untuk uji normalitas.", type = "warning")
      return(NULL)
    }
    
    test <- NULL
    method_name <- input$uji_normalitas
    p_hist <- NULL
    
    tryCatch({
      if (method_name == "Shapiro-Wilk") {
        if (length(x) > 5000) { # Shapiro-Wilk has a limit of 5000 samples
          showNotification("Shapiro-Wilk tidak dapat digunakan untuk lebih dari 5000 data. Menggunakan Kolmogorov-Smirnov.", type = "warning")
          method_name <- "Kolmogorov-Smirnov"
        }
      }
      
      if (method_name == "Shapiro-Wilk") {
        test <- shapiro.test(x)
      } else if (method_name == "Kolmogorov-Smirnov") {
        test <- ks.test(x, "pnorm", mean = mean(x), sd = sd(x)) # Use estimated parameters
      } else if (method_name == "Jarque-Bera") {
        test <- jarque.bera.test(x)
      } else if (method_name == "Chi-Square Goodness of Fit") {
        test <- suppressWarnings(chisq.test(x = observed_counts, p = expected_probs, rescale.p = TRUE))
        test$method <- "Chi-Square Goodness-of-Fit Test for Normality"
      }
      
      p_hist <- ggplot(data.frame(x=x), aes(x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30, fill="#f78fb3", color="#fff0f5") +
        stat_function(fun=dnorm, args=list(mean=mean(x), sd=sd(x)), color="#d65076", linetype="dashed", size=1) +
        labs(title = paste("Histogram dengan Kurva Normal untuk", input$normal_var)) +
        theme_minimal()
    }, error = function(e) {
      showNotification(paste("Error saat menjalankan uji normalitas:", e$message), type = "error")
      return(NULL)
    })
    
    list(test = test, var = input$normal_var, plot = p_hist, method = method_name)
  })
  
  output$hasil_normalitas <- renderPrint({
    res <- normal_test_results()
    if (!is.null(res)) {
      print(res$test)
    }
  })
  output$hist_normal <- renderPlot({
    res <- normal_test_results()
    if (!is.null(res)) {
      res$plot
    }
  })
  output$normal_interpretasi <- renderUI({
    res <- normal_test_results()
    if (!is.null(res) && !is.null(res$test)) {
      HTML(paste0("<b>Hipotesis Nol (H\u2080):</b> Data berdistribusi normal.<br>",
                  "<b>Hipotesis Alternatif (H\u2081):</b> Data tidak berdistribusi normal.<br>",
                  interpret_pval(res$test$p.value)))
    } else {
      HTML("<p>Tidak ada hasil interpretasi.</p>")
    }
  })
  output$unduh_normalitas_report <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_normalitas_report), 
                    "PDF" = ".pdf", 
                    "WORD" = ".docx",
                    ".pdf")  # default
      paste0('laporan_normalitas_', Sys.Date(), ext)
    },
    content = function(file) {
      res <- normal_test_results()
      if (is.null(res)) {
        showNotification("Tidak ada hasil uji normalitas untuk diunduh.", type = "warning")
        return()
      }
      
      # Buat plot temporary
      plot_path <- tempfile(fileext = ".png")
      if (!is.null(res$plot)) {
        ggsave(plot_path, plot = res$plot, device = "png", width = 8, height = 6, units = "in")
      } else {
        plot_path <- ""
      }
      
      # Buat interpretasi dalam format text
      interpretasi_text <- paste0("Hipotesis Nol (H0): Data berdistribusi normal.\n",
                                  "Hipotesis Alternatif (H1): Data tidak berdistribusi normal.\n",
                                  interpret_pval(res$test$p.value))
      
      params <- list(
        var = res$var,
        method = res$method,
        test_output = capture.output(print(res$test)),
        interpretation_text = interpretasi_text,
        plot_path = plot_path
      )
      
      # Render ke temporary file dulu
      temp_file <- tempfile(fileext = switch(toupper(input$format_normalitas_report), 
                                             "PDF" = ".pdf", 
                                             "WORD" = ".docx",
                                             ".pdf"))
      
      ok <- render_report("normalitas_report.Rmd", temp_file, params,
                          switch(toupper(input$format_normalitas_report), 
                                 "PDF" = rmarkdown::pdf_document(), 
                                 "WORD" = rmarkdown::word_document(),
                                 rmarkdown::pdf_document()))
      
      if (!ok || !file.exists(temp_file) || file.size(temp_file) == 0) {
        showNotification("Gagal membuat file PDF/Word. Pastikan Pandoc dan LaTeX (untuk PDF) sudah terinstall.", type = "error")
        return()
      }
      
      # Copy ke file download
      file.copy(temp_file, file, overwrite = TRUE)
    },
    contentType = function() {
      switch(
        toupper(input$format_normalitas_report),
        "PDF" = "application/pdf",
        "WORD" = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        "application/pdf"
      )
    }
  )
  
  # --- Uji Homogenitas ---
  homogen_test_results <- eventReactive(input$run_homogenitas, {
    req(input$dep_var_homo, input$group_var_homo, input$homo_plot_type)
    catat_histori(paste("Uji Homogenitas:", input$metode_homogenitas, "pada", input$dep_var_homo, "oleh", input$group_var_homo))
    df <- values$data
    y <- df[[input$dep_var_homo]]
    grup <- as.factor(df[[input$group_var_homo]])
    
    if (length(levels(grup)) < 2) {
      showNotification("Variabel grup harus memiliki setidaknya dua kategori.", type = "warning")
      return(NULL)
    }
    if (!is.numeric(y)) {
      showNotification("Variabel dependen harus numerik.", type = "warning")
      return(NULL)
    }
    
    test <- NULL
    p_value <- NA
    method_name <- input$metode_homogenitas
    
    tryCatch({
      df <- values$data
      y <- df[[input$dep_var_homo]]
      grup <- as.factor(df[[input$group_var_homo]])
      
      if (method_name == "Levene") {
        test <- leveneTest(y ~ grup, data = df)
        p_value <- test$`Pr(>F)`[1]
      } else if (method_name == "Bartlett") {
        test <- bartlett.test(y ~ grup, data = df)
        p_value <- test$p.value
      } else if (method_name == "F-test (hanya 2 grup)") {
        if (nlevels(grup) != 2) {
          showNotification("F-test hanya berlaku untuk dua grup. Pilih metode lain atau variabel grup dengan 2 kategori.", type = "warning")
          return(NULL)
        }
        test <- var.test(y ~ grup, data = df)
        p_value <- test$p.value
      }
      
      plot_scatter <- NULL
      if (input$homo_plot_type == "Boxplot") {
        plot_scatter <- ggplot(df, aes_string(x=input$group_var_homo, y=input$dep_var_homo, fill=input$group_var_homo)) +
          geom_boxplot(alpha = 0.7, fill = "#fbd6e3", color = "#d65076") +
          labs(title = paste("Boxplot", input$dep_var_homo, "berdasarkan", input$group_var_homo)) +
          theme_minimal() + theme(legend.position = "none")
      } else { # Scatter Plot
        plot_scatter <- ggplot(df, aes_string(x=input$group_var_homo, y=input$dep_var_homo, color=input$group_var_homo)) +
          geom_jitter(width = 0.2, alpha = 0.6, color = "#d65076") +
          labs(title = paste("Scatter Plot", input$dep_var_homo, "berdasarkan", input$group_var_homo)) +
          theme_minimal() + theme(legend.position = "none")
      }
      
    }, error = function(e) {
      showNotification(paste("Error saat menjalankan uji homogenitas:", e$message), type = "error")
      return(NULL)
    })
    
    list(test = test, method = method_name, dep = input$dep_var_homo, grp = input$group_var_homo, plot = plot_scatter, p_value = p_value)
  })
  
  output$hasil_homogenitas <- renderPrint({
    res <- homogen_test_results()
    if (!is.null(res)) {
      print(res$test)
    }
  })
  output$plot_scatter_homo <- renderPlot({
    res <- homogen_test_results()
    if (!is.null(res)) {
      res$plot
    }
  })
  output$interpretasi_homogenitas <- renderUI({
    res <- homogen_test_results()
    if (!is.null(res) && !is.null(res$test)) {
      HTML(paste0("<b>Hipotesis Nol (H\u2080):</b> Varians antar grup adalah homogen.<br>",
                  "<b>Hipotesis Alternatif (H\u2081):</b> Varians antar grup tidak homogen.<br>",
                  interpret_pval(res$p_value)))
    } else {
      HTML("<p>Tidak ada hasil interpretasi.</p>")
    }
  })
  output$unduh_homogenitas_report <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_homogenitas_report), 
                    "PDF" = ".pdf", 
                    "WORD" = ".docx",
                    ".pdf")  # default
      paste0('laporan_homogenitas_', Sys.Date(), ext)
    },
    content = function(file) {
      res <- homogen_test_results()
      if (is.null(res)) {
        showNotification("Tidak ada hasil uji homogenitas untuk diunduh.", type = "warning")
        return()
      }
      
      # Buat plot temporary
      plot_path <- tempfile(fileext = ".png")
      if (!is.null(res$plot)) {
        ggsave(plot_path, plot = res$plot, device = "png", width = 8, height = 6, units = "in")
      } else {
        plot_path <- ""
      }
      
      # Buat interpretasi dalam format text
      interpretasi_text <- paste0("Hipotesis Nol (H0): Varians antar grup adalah homogen.\n",
                                  "Hipotesis Alternatif (H1): Varians antar grup tidak homogen.\n",
                                  interpret_pval(res$p_value))
      
      params <- list(
        dep_var = res$dep,
        group_var = res$grp,
        method = res$method,
        test_output = capture.output(print(res$test)),
        interpretation_text = interpretasi_text,
        plot_path = plot_path
      )
      
      # Render ke temporary file dulu
      temp_file <- tempfile(fileext = switch(toupper(input$format_homogenitas_report), 
                                             "PDF" = ".pdf", 
                                             "WORD" = ".docx",
                                             ".pdf"))
      
      ok <- render_report("homogenitas_report.Rmd", temp_file, params,
                          switch(toupper(input$format_homogenitas_report), 
                                 "PDF" = rmarkdown::pdf_document(), 
                                 "WORD" = rmarkdown::word_document(),
                                 rmarkdown::pdf_document()))
      
      if (!ok || !file.exists(temp_file) || file.size(temp_file) == 0) {
        showNotification("Gagal membuat file PDF/Word. Pastikan Pandoc dan LaTeX (untuk PDF) sudah terinstall.", type = "error")
        return()
      }
      
      # Copy ke file download
      file.copy(temp_file, file, overwrite = TRUE)
    },
    contentType = function() {
      switch(
        toupper(input$format_homogenitas_report),
        "PDF" = "application/pdf",
        "WORD" = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        "application/pdf"
      )
    }
  )
  
  
  # --- Uji t 1 Sampel ---
  ttest1_res <- eventReactive(input$run_rata1, {
    req(input$rata1_var, input$rata1_mu)
    catat_histori(paste("Uji t 1 Sampel pada variabel:", input$rata1_var, "dengan mu0 =", input$rata1_mu))
    x <- na.omit(values$data[[input$rata1_var]])
    if (length(x) == 0) {
      showNotification("Variabel yang dipilih kosong setelah menghilangkan NA.", type = "warning")
      return(NULL)
    }
    t.test(x, mu = input$rata1_mu)
  })
  output$hasil_uji_rata1 <- renderPrint({
    res <- ttest1_res()
    if (!is.null(res)) print(res)
  })
  output$interpret_rata1 <- renderUI({
    res <- ttest1_res()
    if (!is.null(res)) {
      HTML(paste0("<b>H\u2080: \u03bc = ", input$rata1_mu, "</b> vs <b>H\u2081: \u03bc \u2260 ", input$rata1_mu, "</b><br>", interpret_pval(res$p.value)))
    } else {
      HTML("<p>Tidak ada hasil interpretasi.</p>")
    }
  })
  output$unduh_rata1_report <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_rata1_report), 
                    "PDF" = ".pdf", 
                    "WORD" = ".docx",
                    ".pdf")  # default
      paste0('laporan_t1sampel_', Sys.Date(), ext)
    },
    content = function(file) {
      res <- ttest1_res()
      if (is.null(res)) {
        showNotification("Tidak ada hasil uji t 1 sampel untuk diunduh.", type = "warning")
        return()
      }
      
      # Buat plot temporary
      plot_path <- tempfile(fileext = ".png")
      tryCatch({
        x <- na.omit(values$data[[input$rata1_var]])
        if (length(x) > 0) {
          df_plot <- data.frame(Value = x)
          plot_data <- ggplot(df_plot, aes(x = Value)) +
            geom_histogram(bins = 20, fill = "#f78fb3", alpha = 0.7, color = "#fff0f5") +
            geom_vline(xintercept = mean(x), color = "#d65076", linetype = "dashed", size = 1) +
            geom_vline(xintercept = input$rata1_mu, color = "#5c2a41", linetype = "solid", size = 1) +
            labs(title = paste("Distribusi", input$rata1_var),
                 x = input$rata1_var, y = "Frekuensi",
                 subtitle = paste("Garis merah: rata-rata sampel (", round(mean(x), 3), "), Garis ungu: hipotesis μ₀ (", input$rata1_mu, ")")) +
            theme_minimal()
          ggsave(plot_path, plot = plot_data, device = "png", width = 8, height = 6, units = "in")
        } else {
          plot_path <- ""
        }
      }, error = function(e) {
        plot_path <- ""
      })
      
      # Buat interpretasi dalam format text
      interpretasi_text <- get_interpretasi_text(output$interpret_rata1())
      
      params <- list(
        var = input$rata1_var,
        mu = input$rata1_mu,
        test_output = capture.output(print(res)),
        interpretation_text = interpretasi_text,
        plot_path = plot_path
      )
      
      # Render ke temporary file dulu
      temp_file <- tempfile(fileext = switch(toupper(input$format_rata1_report), 
                                             "PDF" = ".pdf", 
                                             "WORD" = ".docx",
                                             ".pdf"))
      
      ok <- render_report("ratarata_1sampel_report.Rmd", temp_file, params,
                          switch(toupper(input$format_rata1_report), 
                                 "PDF" = rmarkdown::pdf_document(), 
                                 "WORD" = rmarkdown::word_document(),
                                 rmarkdown::pdf_document()))
      
      if (!ok || !file.exists(temp_file) || file.size(temp_file) == 0) {
        showNotification("Gagal membuat file PDF/Word. Pastikan Pandoc dan LaTeX (untuk PDF) sudah terinstall.", type = "error")
        return()
      }
      
      # Copy ke file download
      file.copy(temp_file, file, overwrite = TRUE)
    },
    contentType = function() {
      switch(
        toupper(input$format_rata1_report),
        "PDF" = "application/pdf",
        "WORD" = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        "application/pdf"
      )
    }
  )
  
  # --- Uji t 1 Sampel: Plot ---
  output$plot_rata1 <- renderPlot({
    res <- ttest1_res()
    if (!is.null(res)) {
      x <- na.omit(values$data[[input$rata1_var]])
      if (length(x) > 0) {
        df_plot <- data.frame(Value = x)
        ggplot(df_plot, aes(x = Value)) +
          geom_histogram(bins = 20, fill = "#f78fb3", alpha = 0.7, color = "#fff0f5") +
          geom_vline(xintercept = mean(x), color = "#d65076", linetype = "dashed", size = 1) +
          geom_vline(xintercept = input$rata1_mu, color = "#5c2a41", linetype = "solid", size = 1) +
          labs(title = paste("Distribusi", input$rata1_var),
               x = input$rata1_var, y = "Frekuensi",
               subtitle = paste("Garis merah: rata-rata sampel (", round(mean(x), 3), "), Garis ungu: hipotesis μ₀ (", input$rata1_mu, ")")) +
          theme_minimal()
      }
    }
  })
  
  # --- Uji t 2 Sampel Bebas ---
  ttest2_res <- eventReactive(input$run_rata2, {
    req(input$rata2_var, input$rata2_group)
    catat_histori(paste("Uji t 2 Sampel Independen:", input$rata2_var, "berdasarkan grup", input$rata2_group))
    df_clean <- values$data %>% select(all_of(c(input$rata2_var, input$rata2_group))) %>% na.omit()
    if (nrow(df_clean) == 0) {
      showNotification("Data kosong setelah menghilangkan NA.", type = "warning")
      return(NULL)
    }
    # Ensure group variable is a factor with exactly 2 levels
    group_var <- as.factor(df_clean[[input$rata2_group]])
    if (nlevels(group_var) != 2) {
      showNotification("Variabel grup harus memiliki tepat dua kategori untuk Uji t 2 Sampel Independen.", type = "warning")
      return(NULL)
    }
    formula_str <- paste(input$rata2_var, "~", input$rata2_group)
    t.test(as.formula(formula_str), data = df_clean)
  })
  output$hasil_uji_rata2 <- renderPrint({
    res <- ttest2_res()
    if (!is.null(res)) print(res)
  })
  output$interpret_rata2 <- renderUI({
    res <- ttest2_res()
    if (!is.null(res)) {
      HTML(paste0("<b>H\u2080: \u03bc\u2081 = \u03bc\u2082</b> vs <b>H\u2081: \u03bc\u2081 \u2260 \u03bc\u2082</b><br>", interpret_pval(res$p.value)))
    } else {
      HTML("<p>Tidak ada hasil interpretasi.</p>")
    }
  })
  output$unduh_rata2_report <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_rata2_report), 
                    "PDF" = ".pdf", 
                    "WORD" = ".docx",
                    ".pdf")  # default
      paste0('laporan_t2bebas_', Sys.Date(), ext)
    },
    content = function(file) {
      res <- ttest2_res()
      if (is.null(res)) {
        showNotification("Tidak ada hasil uji t 2 sampel bebas untuk diunduh.", type = "warning")
        return()
      }
      
      # Buat plot temporary
      plot_path <- tempfile(fileext = ".png")
      tryCatch({
        df_clean <- values$data %>% select(all_of(c(input$rata2_var, input$rata2_group))) %>% na.omit()
        if (nrow(df_clean) > 0) {
          plot_data <- ggplot(df_clean, aes_string(x = input$rata2_group, y = input$rata2_var, fill = input$rata2_group)) +
            geom_boxplot(alpha = 0.7, fill = "#fbd6e3", color = "#d65076") +
            geom_jitter(width = 0.2, alpha = 0.5, color = "#d65076") +
            labs(title = paste("Perbandingan", input$rata2_var, "berdasarkan", input$rata2_group),
                 x = input$rata2_group, y = input$rata2_var) +
            theme_minimal() +
            theme(legend.position = "none")
          ggsave(plot_path, plot = plot_data, device = "png", width = 8, height = 6, units = "in")
        } else {
          plot_path <- ""
        }
      }, error = function(e) {
        plot_path <- ""
      })
      
      # Buat interpretasi dalam format text
      interpretasi_text <- get_interpretasi_text(output$interpret_rata2())
      
      params <- list(
        var = input$rata2_var,
        group_var = input$rata2_group,
        test_output = capture.output(print(res)),
        interpretation_text = interpretasi_text,
        plot_path = plot_path
      )
      
      # Render ke temporary file dulu
      temp_file <- tempfile(fileext = switch(toupper(input$format_rata2_report), 
                                             "PDF" = ".pdf", 
                                             "WORD" = ".docx",
                                             ".pdf"))
      
      ok <- render_report("ratarata_2bebas_report.Rmd", temp_file, params,
                          switch(toupper(input$format_rata2_report), 
                                 "PDF" = rmarkdown::pdf_document(), 
                                 "WORD" = rmarkdown::word_document(),
                                 rmarkdown::pdf_document()))
      
      if (!ok || !file.exists(temp_file) || file.size(temp_file) == 0) {
        showNotification("Gagal membuat file PDF/Word. Pastikan Pandoc dan LaTeX (untuk PDF) sudah terinstall.", type = "error")
        return()
      }
      
      # Copy ke file download
      file.copy(temp_file, file, overwrite = TRUE)
    },
    contentType = function() {
      switch(
        toupper(input$format_rata2_report),
        "PDF" = "application/pdf",
        "WORD" = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        "application/pdf"
      )
    }
  )
  
  # --- Uji t 2 Sampel Independen: Plot ---
  output$plot_rata2 <- renderPlot({
    res <- ttest2_res()
    if (!is.null(res)) {
      df_clean <- values$data %>% select(all_of(c(input$rata2_var, input$rata2_group))) %>% na.omit()
      if (nrow(df_clean) > 0) {
        ggplot(df_clean, aes_string(x = input$rata2_group, y = input$rata2_var)) +
          geom_boxplot(fill = "#fbd6e3", color = "#d65076", alpha = 0.7) +
          geom_jitter(width = 0.2, alpha = 0.5, color = "#d65076") +
          labs(title = paste("Perbandingan", input$rata2_var, "berdasarkan", input$rata2_group),
               x = input$rata2_group, y = input$rata2_var) +
          theme_minimal() +
          theme(legend.position = "none")
      }
    }
  })
  
  # --- Uji t 2 Sampel Berpasangan ---
  ttest3_res <- eventReactive(input$run_rata3, {
    req(input$rata3_var1, input$rata3_var2)
    catat_histori(paste("Uji t 2 Sampel Berpasangan:", input$rata3_var1, "vs", input$rata3_var2))
    # Ensure both variables are numeric and have the same length after NA omission
    df_paired <- values$data %>% select(all_of(c(input$rata3_var1, input$rata3_var2))) %>% na.omit()
    if (nrow(df_paired) == 0) {
      showNotification("Data kosong setelah menghilangkan NA atau variabel tidak dapat dipasangkan.", type = "warning")
      return(NULL)
    }
    t.test(df_paired[[input$rata3_var1]], df_paired[[input$rata3_var2]], paired = TRUE)
  })
  output$hasil_uji_rata3 <- renderPrint({
    res <- ttest3_res()
    if (!is.null(res)) print(res)
  })
  output$interpret_rata3 <- renderUI({
    res <- ttest3_res()
    if (!is.null(res)) {
      HTML(paste0("<b>H\u2080: \u03bc_diff = 0</b> vs <b>H\u2081: \u03bc_diff \u2260 0</b><br>", interpret_pval(res$p.value)))
    } else {
      HTML("<p>Tidak ada hasil interpretasi.</p>")
    }
  })
  output$unduh_rata3_report <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_rata3_report), 
                    "PDF" = ".pdf", 
                    "WORD" = ".docx",
                    ".pdf")  # default
      paste0('laporan_t2pasangan_', Sys.Date(), ext)
    },
    content = function(file) {
      res <- ttest3_res()
      if (is.null(res)) {
        showNotification("Tidak ada hasil uji t 2 sampel berpasangan untuk diunduh.", type = "warning")
        return()
      }
      
      # Buat interpretasi dalam format text
      interpretasi_text <- get_interpretasi_text(output$interpret_rata3())
      
      params <- list(
        var1 = input$rata3_var1,
        var2 = input$rata3_var2,
        test_output = capture.output(print(res)),
        interpretation_text = interpretasi_text
      )
      
      # Render ke temporary file dulu
      temp_file <- tempfile(fileext = switch(toupper(input$format_rata3_report), 
                                             "PDF" = ".pdf", 
                                             "WORD" = ".docx",
                                             ".pdf"))
      
      ok <- render_report("ratarata_2pasangan_report.Rmd", temp_file, params,
                          switch(toupper(input$format_rata3_report), 
                                 "PDF" = rmarkdown::pdf_document(), 
                                 "WORD" = rmarkdown::word_document(),
                                 rmarkdown::pdf_document()))
      
      if (!ok || !file.exists(temp_file) || file.size(temp_file) == 0) {
        showNotification("Gagal membuat file PDF/Word. Pastikan Pandoc dan LaTeX (untuk PDF) sudah terinstall.", type = "error")
        return()
      }
      
      # Copy ke file download
      file.copy(temp_file, file, overwrite = TRUE)
    },
    contentType = function() {
      switch(
        toupper(input$format_rata3_report),
        "PDF" = "application/pdf",
        "WORD" = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        "application/pdf"
      )
    }
  )
  
  # --- Uji t 2 Sampel Berpasangan: Plot ---
  output$plot_rata3 <- renderPlot({
    res <- ttest3_res()
    if (!is.null(res)) {
      df_paired <- values$data %>% select(all_of(c(input$rata3_var1, input$rata3_var2))) %>% na.omit()
      if (nrow(df_paired) > 0) {
        df_long <- tidyr::pivot_longer(df_paired, cols = everything(), names_to = "Variabel", values_to = "Nilai")
        ggplot(df_long, aes(x = Variabel, y = Nilai, fill = Variabel)) +
          geom_boxplot(fill = "#fbd6e3", color = "#d65076", alpha = 0.7) +
          geom_jitter(width = 0.2, alpha = 0.5, color = "#d65076") +
          labs(title = paste("Perbandingan Berpasangan", input$rata3_var1, "vs", input$rata3_var2),
               x = "Variabel", y = "Nilai") +
          theme_minimal() +
          theme(legend.position = "none")
      }
    }
  })
  
  # --- Uji Proporsi & Varians ---
  download_propvar_report_handler <- function(report_name_base, report_title, test_result_func, interpretation_func, format_input_id, plot_func = NULL, var_info_func = NULL) {
    downloadHandler(
      filename = function() {
        ext <- switch(toupper(input[[format_input_id]]), 
                      "PDF" = ".pdf", 
                      "WORD" = ".docx",
                      ".pdf")  # default
        paste0(report_name_base, '_', Sys.Date(), ext)
      },
      content = function(file) {
        res <- test_result_func()
        if (is.null(res)) {
          showNotification(paste("Tidak ada hasil untuk", report_title, "yang akan diunduh."), type = "warning")
          return()
        }
        
        # Create plot if plot function is provided
        plot_path <- tempfile(fileext = ".png")
        if (!is.null(plot_func)) {
          tryCatch({
            plot_data <- plot_func()
            if (!is.null(plot_data)) {
              ggsave(plot_path, plot = plot_data, device = "png", width = 8, height = 6, units = "in")
            } else {
              plot_path <- ""
            }
          }, error = function(e) {
            plot_path <- ""
          })
        } else {
          plot_path <- ""
        }
        
        # Get variable info if function is provided
        var_info <- NULL
        if (!is.null(var_info_func)) {
          tryCatch({
            var_info <- var_info_func()
          }, error = function(e) {
            var_info <- NULL
          })
        }
        
        # Buat interpretasi dalam format text
        interpretasi_text <- get_interpretasi_text(interpretation_func())
        
        params <- list(
          title = report_title,
          test_output = capture.output(print(res)),
          interpretation_text = interpretasi_text,
          plot_path = plot_path,
          var_info = var_info
        )
        
        # Render ke temporary file dulu
        temp_file <- tempfile(fileext = switch(toupper(input[[format_input_id]]), 
                                               "PDF" = ".pdf", 
                                               "WORD" = ".docx",
                                               ".pdf"))
        
        ok <- render_report("propvar_report.Rmd", temp_file, params,
                            switch(toupper(input[[format_input_id]]), 
                                   "PDF" = rmarkdown::pdf_document(), 
                                   "WORD" = rmarkdown::word_document(),
                                   rmarkdown::pdf_document()))
        
        if (!ok || !file.exists(temp_file) || file.size(temp_file) == 0) {
          showNotification("Gagal membuat file PDF/Word. Pastikan Pandoc dan LaTeX (untuk PDF) sudah terinstall.", type = "error")
          return()
        }
        
        # Copy ke file download
        file.copy(temp_file, file, overwrite = TRUE)
      },
      contentType = function() {
        switch(
          toupper(input[[format_input_id]]),
          "PDF" = "application/pdf",
          "WORD" = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
          "application/pdf"
        )
      }
    )
  }
  
  
  # Uji Proporsi 1 Sampel
  prop1_res <- eventReactive(input$run_prop1, {
    req(input$prop1_var, input$prop1_level, input$prop1_p)
    catat_histori(paste("Uji Proporsi 1 Sampel:", input$prop1_var, "level", input$prop1_level, "p0 =", input$prop1_p))
    df_clean <- values$data %>% select(all_of(input$prop1_var)) %>% na.omit()
    if (nrow(df_clean) == 0) {
      showNotification("Data kosong setelah menghilangkan NA.", type = "warning")
      return(NULL)
    }
    x_n <- sum(df_clean[[input$prop1_var]] == input$prop1_level, na.rm=TRUE)
    n <- length(df_clean[[input$prop1_var]])
    if (n == 0) {
      showNotification("Ukuran sampel nol.", type = "warning")
      return(NULL)
    }
    prop.test(x=x_n, n=n, p=input$prop1_p)
  })
  output$hasil_prop1 <- renderPrint({ res <- prop1_res(); if(!is.null(res)) print(res) })
  output$interpret_prop1 <- renderUI({
    res <- prop1_res()
    if (!is.null(res)) {
      HTML(paste0("<b>H\u2080: p = ", input$prop1_p, "</b> vs <b>H\u2081: p \u2260 ", input$prop1_p, "</b><br>", interpret_pval(res$p.value)))
    } else {
      HTML("<p>Tidak ada hasil interpretasi.</p>")
    }
  })
  
  prop1_plot <- reactive({
    req(input$prop1_var, input$prop1_level, input$prop1_p)
    
    df_clean <- values$data %>% select(all_of(input$prop1_var)) %>% na.omit()
    x_n <- sum(df_clean[[input$prop1_var]] == input$prop1_level, na.rm=TRUE)
    n <- length(df_clean[[input$prop1_var]])
    
    # Create data for plot
    plot_data <- data.frame(
      Category = c(input$prop1_level, paste0("Bukan ", input$prop1_level)),
      Count = c(x_n, n - x_n),
      Proportion = c(x_n/n, (n-x_n)/n)
    )
    
    ggplot(plot_data, aes(x = Category, y = Proportion, fill = Category)) +
      geom_bar(stat = "identity", alpha = 0.7) +
      geom_hline(yintercept = input$prop1_p, color = "red", linetype = "dashed", size = 1) +
      geom_text(aes(label = paste0(Count, "/", n, "\n(", round(Proportion*100, 1), "%)")), 
                vjust = -0.5) +
      labs(title = paste("Distribusi Proporsi", input$prop1_var),
           subtitle = paste("Garis merah: proporsi hipotesis (", input$prop1_p, ")"),
           x = "Kategori", y = "Proporsi") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Info variabel untuk proporsi 1 sampel
  prop1_var_info <- reactive({
    req(input$prop1_var, input$prop1_level, input$prop1_p)
    paste0("**Variabel yang diuji:** ", input$prop1_var, "\n\n",
           "**Level yang dihitung proporsinya:** ", input$prop1_level, "\n\n",
           "**Proporsi hipotesis (p₀):** ", input$prop1_p)
  })
  
  output$unduh_propvar_report1 <- download_propvar_report_handler(
    "laporan_prop1sampel", "Uji Proporsi 1 Sampel", prop1_res, interpret_prop1, "format_propvar_report1",
    plot_func = prop1_plot, var_info_func = prop1_var_info
  )
  
  # Uji Proporsi 2 Sampel
  observeEvent(input$prop2_var_cat, {
    req(input$prop2_var_cat)
    levels <- unique(na.omit(values$data[[input$prop2_var_cat]]))
    
    output$prop2_level1_ui <- renderUI({
      selectInput("prop2_level1", "Pilih Grup 1:", choices = levels, selected = levels[1])
    })
    output$prop2_level2_ui <- renderUI({
      selectInput("prop2_level2", "Pilih Grup 2:", choices = levels, selected = levels[2])
    })
    output$prop2_success_ui <- renderUI({
      selectInput("prop2_success", "Pilih Level Keberhasilan (Success):", choices = levels, selected = levels[1])
    })
  })
  
  prop2_res <- eventReactive(input$run_prop2, {
    req(input$prop2_var_cat, input$prop2_level1, input$prop2_level2, input$prop2_success)
    catat_histori(paste("Uji Proporsi 2 Sampel:", input$prop2_var_cat, "antara", input$prop2_level1, "vs", input$prop2_level2))
    
    if (input$prop2_level1 == input$prop2_level2) {
      showNotification("Grup 1 dan Grup 2 tidak boleh sama.", type = "error"); return(NULL)
    }
    
    df <- values$data[[input$prop2_var_cat]]
    grup1_data <- df[df == input$prop2_level1]
    grup2_data <- df[df == input$prop2_level2]
    
    x1 <- sum(grup1_data == input$prop2_success, na.rm = TRUE)
    n1 <- length(na.omit(grup1_data))
    x2 <- sum(grup2_data == input$prop2_success, na.rm = TRUE)
    n2 <- length(na.omit(grup2_data))
    
    prop.test(x = c(x1, x2), n = c(n1, n2))
  })
  
  prop2_plot <- reactive({
    req(input$prop2_var_cat, input$prop2_level1, input$prop2_level2, input$prop2_success)
    
    df <- values$data[[input$prop2_var_cat]]
    grup1_data <- df[df == input$prop2_level1]
    grup2_data <- df[df == input$prop2_level2]
    
    x1 <- sum(grup1_data == input$prop2_success, na.rm = TRUE)
    n1 <- length(na.omit(grup1_data))
    x2 <- sum(grup2_data == input$prop2_success, na.rm = TRUE)
    n2 <- length(na.omit(grup2_data))
    
    # Create plot data
    plot_data <- data.frame(
      Grup = c(input$prop2_level1, input$prop2_level2),
      Sukses = c(x1, x2),
      Total = c(n1, n2),
      Proporsi = c(x1/n1, x2/n2)
    )
    
    ggplot(plot_data, aes(x = Grup, y = Proporsi, fill = Grup)) +
      geom_bar(stat = "identity", alpha = 0.7) +
      geom_text(aes(label = paste0(Sukses, "/", Total, "\n(", round(Proporsi*100, 1), "%)")), 
                vjust = -0.5) +
      labs(title = paste("Perbandingan Proporsi", input$prop2_success, "antara", input$prop2_level1, "dan", input$prop2_level2),
           x = "Grup", y = "Proporsi") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Info variabel untuk proporsi 2 sampel
  prop2_var_info <- reactive({
    req(input$prop2_var_cat, input$prop2_level1, input$prop2_level2, input$prop2_success)
    paste0("**Variabel Grup:** ", input$prop2_var_cat, "\n\n",
           "**Grup yang dibandingkan:** ", input$prop2_level1, " vs ", input$prop2_level2, "\n\n",
           "**Level keberhasilan:** ", input$prop2_success)
  })
  
  output$hasil_prop2 <- renderPrint({ res <- prop2_res(); if(!is.null(res)) print(res) })
  output$interpret_prop2 <- renderUI({
    res <- prop2_res()
    if (!is.null(res)) {
      HTML(paste0("<b>H\u2080: p\u2081 = p\u2082</b> vs <b>H\u2081: p\u2081 \u2260 p\u2082</b><br>", interpret_pval(res$p.value)))
    } else {
      HTML("<p>Tidak ada hasil interpretasi.</p>")
    }
  })
  output$unduh_propvar_report2 <- download_propvar_report_handler(
    "laporan_prop2sampel", "Uji Proporsi 2 Sampel", prop2_res, interpret_prop2, "format_propvar_report2"
  )
  
  # Uji Varians 1 Sampel (Chi-Squared test for variance)
  var1_res <- eventReactive(input$run_var1, {
    req(input$var1_var, input$var1_val)
    catat_histori(paste("Uji Varians 1 Sampel:", input$var1_var, "dengan varians hipotesis", input$var1_val))
    x <- na.omit(values$data[[input$var1_var]])
    if (length(x) < 2) {
      showNotification("Jumlah data terlalu sedikit untuk uji varians.", type = "warning")
      return(NULL)
    }
    if (input$var1_val <= 0) {
      showNotification("Nilai hipotesis varians harus lebih besar dari 0.", type = "warning")
      return(NULL)
    }
    
    n <- length(x)
    sample_var <- var(x)
    chi2_stat <- (n - 1) * sample_var / input$var1_val
    
    # Calculate p-value for a two-sided test
    # This involves finding the probability of getting a chi2_stat more extreme in both tails
    p_val <- 2 * min(pchisq(chi2_stat, n - 1), 1 - pchisq(chi2_stat, n - 1))
    
    # Mimic htest structure for consistency in reporting
    structure(list(
      statistic = c("X-squared" = chi2_stat),
      parameter = c(df = n - 1),
      p.value = p_val,
      method = "Chi-squared Test for One Sample Variance",
      data.name = input$var1_var,
      alternative = "two.sided",
      estimate = c("sample variance" = sample_var),
      null.value = c("variance" = input$var1_val)
    ), class = "htest")
  })
  output$hasil_var1 <- renderPrint({ res <- var1_res(); if(!is.null(res)) print(res) })
  output$interpret_var1 <- renderUI({
    res <- var1_res()
    if (!is.null(res)) {
      HTML(paste0("<b>H\u2080: \u03c3\u00b2 = ", input$var1_val, "</b> vs <b>H\u2081: \u03c3\u00b2 \u2260 ", input$var1_val, "</b><br>", interpret_pval(res$p.value)))
    } else {
      HTML("<p>Tidak ada hasil interpretasi.</p>")
    }
  })
  
  var1_plot <- reactive({
    req(input$var1_var, input$var1_val)
    
    x <- na.omit(values$data[[input$var1_var]])
    if (length(x) < 2) return(NULL)
    
    df_plot <- data.frame(Value = x)
    sample_var <- var(x)
    
    ggplot(df_plot, aes(x = Value)) +
      geom_histogram(bins = 20, fill = "lightgreen", alpha = 0.7, color = "black") +
      labs(title = paste("Distribusi", input$var1_var),
           subtitle = paste("Varians sampel:", round(sample_var, 3), "| Varians hipotesis:", input$var1_val),
           x = input$var1_var, y = "Frekuensi") +
      theme_minimal()
  })
  
  var1_var_info <- reactive({
    req(input$var1_var, input$var1_val)
    x <- na.omit(values$data[[input$var1_var]])
    sample_var <- var(x)
    paste0("**Variabel yang diuji:** ", input$var1_var, "\n\n",
           "**Varians hipotesis (σ²₀):** ", input$var1_val, "\n\n",
           "**Varians sampel:** ", round(sample_var, 4), "\n\n",
           "**Ukuran sampel:** ", length(x))
  })
  
  output$unduh_propvar_report3 <- download_propvar_report_handler(
    "laporan_var1sampel", "Uji Varians 1 Sampel", var1_res, interpret_var1, "format_propvar_report3",
    plot_func = var1_plot, var_info_func = var1_var_info
  )
  
  # Uji Varians 2 Sampel (F-test)
  observeEvent(input$var2_var_cat, {
    req(input$var2_var_cat)
    levels <- unique(na.omit(values$data[[input$var2_var_cat]]))
    
    output$var2_level1_ui <- renderUI({
      selectInput("var2_level1", "Pilih Grup 1:", choices = levels, selected = levels[1])
    })
    output$var2_level2_ui <- renderUI({
      selectInput("var2_level2", "Pilih Grup 2:", choices = levels, selected = levels[2])
    })
  })
  
  var2_res <- eventReactive(input$run_var2, {
    req(input$var2_var_num, input$var2_var_cat, input$var2_level1, input$var2_level2)
    catat_histori(paste("Uji Varians 2 Sampel:", input$var2_var_num, "antara", input$var2_level1, "vs", input$var2_level2))
    
    if (input$var2_level1 == input$var2_level2) {
      showNotification("Grup 1 dan Grup 2 tidak boleh sama.", type = "error"); return(NULL)
    }
    
    df <- values$data
    var_num <- df[[input$var2_var_num]]
    var_cat <- df[[input$var2_var_cat]]
    
    data1 <- var_num[var_cat == input$var2_level1]
    data2 <- var_num[var_cat == input$var2_level2]
    
    var.test(data1, data2)
  })
  
  var2_plot <- reactive({
    req(input$var2_var_num, input$var2_var_cat, input$var2_level1, input$var2_level2)
    
    df <- values$data
    var_num <- df[[input$var2_var_num]]
    var_cat <- df[[input$var2_var_cat]]
    
    data1 <- var_num[var_cat == input$var2_level1]
    data2 <- var_num[var_cat == input$var2_level2]
    
    # Remove NA values
    data1 <- data1[!is.na(data1)]
    data2 <- data2[!is.na(data2)]
    
    # Create combined data for boxplot
    combined_data <- data.frame(
      Value = c(data1, data2),
      Group = c(rep(input$var2_level1, length(data1)), 
                rep(input$var2_level2, length(data2)))
    )
    
    stats_data <- data.frame(
      Group = c(input$var2_level1, input$var2_level2),
      Variance = c(var(data1), var(data2)),
      SD = c(sd(data1), sd(data2)),
      N = c(length(data1), length(data2))
    )
    
    # Create boxplot
    p1 <- ggplot(combined_data, aes(x = Group, y = Value, fill = Group)) +
      geom_boxplot(alpha = 0.7) +
      labs(title = paste("Distribusi", input$var2_var_num, "berdasarkan", input$var2_var_cat),
           x = input$var2_var_cat, y = input$var2_var_num) +
      theme_minimal() +
      theme(legend.position = "none")
    
    # Create variance comparison plot
    p2 <- ggplot(stats_data, aes(x = Group, y = Variance, fill = Group)) +
      geom_bar(stat = "identity", alpha = 0.7) +
      geom_text(aes(label = paste0("Var: ", round(Variance, 3), "\nSD: ", round(SD, 3), "\nN: ", N)), 
                vjust = -0.5) +
      labs(title = "Perbandingan Varians antar Grup",
           x = "Grup", y = "Varians") +
      theme_minimal() +
      theme(legend.position = "none")
    
    gridExtra::grid.arrange(p1, p2, ncol = 1)
  })
  
  # Info variabel untuk varians 2 sampel
  var2_var_info <- reactive({
    req(input$var2_var_num, input$var2_var_cat, input$var2_level1, input$var2_level2)
    paste0("**Variabel Numerik:** ", input$var2_var_num, "\n\n",
           "**Variabel Grup:** ", input$var2_var_cat, "\n\n",
           "**Grup yang dibandingkan:** ", input$var2_level1, " vs ", input$var2_level2)
  })
  
  output$hasil_var2 <- renderPrint({ res <- var2_res(); if(!is.null(res)) print(res) })
  output$interpret_var2 <- renderUI({
    res <- var2_res()
    if (!is.null(res)) {
      HTML(paste0("<b>H\u2080: \u03c3\u2081\u00b2 = \u03c3\u2082\u00b2</b> vs <b>H\u2081: \u03c3\u2081\u00b2 \u2260 \u03c3\u2082\u00b2</b><br>", interpret_pval(res$p.value)))
    } else {
      HTML("<p>Tidak ada hasil interpretasi.</p>")
    }
  })
  output$unduh_propvar_report4 <- download_propvar_report_handler(
    "laporan_var2sampel", "Uji Varians 2 Sampel", var2_res, interpret_var2, "format_propvar_report4",
    plot_func = var2_plot, var_info_func = var2_var_info
  )
  
  # --- ANOVA ---
  anova_results <- eventReactive(input$run_anova, {
    req(input$anova_y, input$anova_group1, input$anova_type)
    catat_histori(paste("Uji ANOVA", input$anova_type, ": Y =", input$anova_y, ", Grup 1 =", input$anova_group1, if (!is.null(input$anova_group2)) paste(", Grup 2 =", input$anova_group2) else ""))
    df_anova <- values$data
    
    formula_str <- NULL
    plot_anova <- NULL
    model <- NULL
    p_value <- NA
    
    tryCatch({
      if (input$anova_type == "Satu Arah (One-Way)") {
        req(is.numeric(df_anova[[input$anova_y]]), is.factor(df_anova[[input$anova_group1]]) || is.character(df_anova[[input$anova_group1]]))
        formula_str <- paste(input$anova_y, "~", input$anova_group1)
        model <- aov(as.formula(formula_str), data = df_anova)
        plot_anova <- ggplot(df_anova, aes_string(x=input$anova_group1, y=input$anova_y, fill=input$anova_group1)) +
          geom_boxplot(alpha = 0.7) +
          labs(title = paste("Boxplot", input$anova_y, "berdasarkan", input$anova_group1)) +
          theme_minimal()
        p_value <- summary(model)[[1]]$`Pr(>F)`[1]
        
      } else if (input$anova_type == "Dua Arah (Two-Way)") {
        req(input$anova_group2)
        req(is.numeric(df_anova[[input$anova_y]]),
            is.factor(df_anova[[input$anova_group1]]) || is.character(df_anova[[input$anova_group1]]),
            is.factor(df_anova[[input$anova_group2]]) || is.character(df_anova[[input$anova_group2]]))
        
        # Convert to factor if they are character
        df_anova[[input$anova_group1]] <- as.factor(df_anova[[input$anova_group1]])
        df_anova[[input$anova_group2]] <- as.factor(df_anova[[input$anova_group2]])
        
        if (nlevels(df_anova[[input$anova_group1]]) < 2 || nlevels(df_anova[[input$anova_group2]]) < 2) {
          showNotification("Variabel grup untuk ANOVA Dua Arah harus memiliki setidaknya dua kategori.", type = "warning")
          return(NULL)
        }
        formula_str <- paste(input$anova_y, "~", input$anova_group1, "*", input$anova_group2)
        model <- aov(as.formula(formula_str), data = df_anova)
        plot_anova <- ggplot(df_anova, aes_string(x=input$anova_group1, y=input$anova_y, fill=input$anova_group2)) +
          geom_boxplot(alpha = 0.7) +
          facet_wrap(as.formula(paste("~", input$anova_group2))) +
          labs(title = paste("Boxplot", input$anova_y, "berdasarkan", input$anova_group1, "dan", input$anova_group2)) +
          theme_minimal()
        
        # For two-way ANOVA, we might interpret main effects and interaction
        # For simplicity, let's take the p-value of the interaction term first, or the first main effect if no interaction
        anova_summary <- summary(model)[[1]]
        if (nrow(anova_summary) > 2 && !is.na(anova_summary$`Pr(>F)`[3])) { # Check for interaction term
          p_value <- anova_summary$`Pr(>F)`[3] # Interaction p-value
        } else if (nrow(anova_summary) > 1) {
          p_value <- anova_summary$`Pr(>F)`[1] # First main effect p-value
        }
      }
    }, error = function(e) {
      showNotification(paste("Error saat menjalankan ANOVA:", e$message), type = "error")
      return(NULL)
    })
    
    list(model=model, plot=plot_anova, formula=formula_str, p_value = p_value, type = input$anova_type,
         y_var = input$anova_y, group1_var = input$anova_group1, group2_var = input$anova_group2)
  })
  output$hasil_anova <- renderPrint({
    res <- anova_results()
    if (!is.null(res) && !is.null(res$model)) summary(res$model)
  })
  output$plot_anova <- renderPlot({
    res <- anova_results()
    if (!is.null(res) && !is.null(res$plot)) res$plot
  })
  output$interpret_anova <- renderUI({
    res <- anova_results()
    if (!is.null(res) && !is.null(res$model)) {
      if (res$type == "Satu Arah (One-Way)") {
        HTML(paste0("<b>Hipotesis Nol (H\u2080):</b> Tidak ada perbedaan rata-rata antara grup.<br>",
                    "<b>Hipotesis Alternatif (H\u2081):</b> Ada perbedaan rata-rata antara setidaknya satu pasang grup.<br>",
                    interpret_pval(res$p_value)))
      } else { # Two-Way ANOVA
        anova_summary <- summary(res$model)[[1]]
        interpretation_text <- ""
        
        # Interpretasi Interaksi
        if (nrow(anova_summary) > 2 && !is.na(anova_summary$`Pr(>F)`[3])) {
          p_interaksi <- anova_summary$`Pr(>F)`[3]
          interpretation_text <- paste0("<b>Interaksi (", res$group1_var, " * ", res$group2_var, "):</b> ", interpret_pval(p_interaksi))
          if (p_interaksi <= 0.05) {
            interpretation_text <- paste0(interpretation_text, "<br>Pengaruh satu faktor tergantung pada level faktor lainnya.")
          } else {
            interpretation_text <- paste0(interpretation_text, "<br>Tidak ada interaksi signifikan antar faktor.")
          }
        }
        
        # Interpretasi Efek Utama (Faktor 1)
        if (nrow(anova_summary) > 0 && !is.na(anova_summary$`Pr(>F)`[1])) {
          p_group1 <- anova_summary$`Pr(>F)`[1]
          interpretation_text <- paste0(interpretation_text, "<br><b>Efek Utama (", res$group1_var, "):</b> ", interpret_pval(p_group1))
          if (p_group1 <= 0.05) {
            interpretation_text <- paste0(interpretation_text, "<br>Ada efek utama signifikan dari ", res$group1_var, ".")
          } else {
            interpretation_text <- paste0(interpretation_text, "<br>Tidak ada efek utama signifikan dari ", res$group1_var, ".")
          }
        }
        
        # Interpretasi Efek Utama (Faktor 2)
        if (nrow(anova_summary) > 1 && !is.na(anova_summary$`Pr(>F)`[2])) {
          p_group2 <- anova_summary$`Pr(>F)`[2]
          interpretation_text <- paste0(interpretation_text, "<br><b>Efek Utama (", res$group2_var, "):</b> ", interpret_pval(p_group2))
          if (p_group2 <= 0.05) {
            interpretation_text <- paste0(interpretation_text, "<br>Ada efek utama signifikan dari ", res$group2_var, ".")
          } else {
            interpretation_text <- paste0(interpretation_text, "<br>Tidak ada efek utama signifikan dari ", res$group2_var, ".")
          }
        }
        HTML(interpretation_text)
      }
    } else {
      HTML("<p>Tidak ada hasil interpretasi.</p>")
    }
  })
  output$unduh_anova_report <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_anova_report), 
                    "PDF" = ".pdf", 
                    "WORD" = ".docx",
                    ".pdf")  # default
      paste0('laporan_anova_', Sys.Date(), ext)
    },
    content = function(file) {
      res <- anova_results()
      if (is.null(res) || is.null(res$model)) {
        showNotification("Tidak ada hasil ANOVA untuk diunduh.", type = "warning")
        return()
      }
      
      # Buat plot temporary
      plot_path <- tempfile(fileext = ".png")
      if (!is.null(res$plot)) {
        ggsave(plot_path, plot = res$plot, device = "png", width = 10, height = 7, units = "in")
      } else {
        plot_path <- ""
      }
      
      # Buat interpretasi dalam format text
      interpretasi_text <- get_interpretasi_text(output$interpret_anova())
      
      params <- list(
        formula = res$formula,
        test_output = capture.output(summary(res$model)),
        interpretation_text = interpretasi_text,
        plot_path = plot_path,
        anova_type = res$type,
        y_var = res$y_var,
        group1_var = res$group1_var,
        group2_var = res$group2_var # Will be NULL for one-way
      )
      
      # Render ke temporary file dulu
      temp_file <- tempfile(fileext = switch(toupper(input$format_anova_report), 
                                             "PDF" = ".pdf", 
                                             "WORD" = ".docx",
                                             ".pdf"))
      
      ok <- render_report("anova_report.Rmd", temp_file, params,
                          switch(toupper(input$format_anova_report), 
                                 "PDF" = rmarkdown::pdf_document(), 
                                 "WORD" = rmarkdown::word_document(),
                                 rmarkdown::pdf_document()))
      
      if (!ok || !file.exists(temp_file) || file.size(temp_file) == 0) {
        showNotification("Gagal membuat file PDF/Word. Pastikan Pandoc dan LaTeX (untuk PDF) sudah terinstall.", type = "error")
        return()
      }
      
      # Copy ke file download
      file.copy(temp_file, file, overwrite = TRUE)
    },
    contentType = function() {
      switch(
        toupper(input$format_anova_report),
        "PDF" = "application/pdf",
        "WORD" = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        "application/pdf"
      )
    }
  )
  
  
  # --- Regresi Linear Berganda ---
  reg_model <- eventReactive(input$run_regresi, {
    req(input$regresi_y, input$regresi_x)
    catat_histori(paste("Regresi Linear Berganda: Y =", input$regresi_y, ", X =", paste(input$regresi_x, collapse = ", ")))
    if (length(input$regresi_x) == 0) {
      showNotification("Pilih setidaknya satu variabel independen.", type = "warning")
      return(NULL)
    }
    
    # Prepare data, remove NAs from selected columns
    selected_cols <- unique(c(input$regresi_y, input$regresi_x))
    df_reg <- values$data %>% select(all_of(selected_cols)) %>% na.omit()
    
    if (nrow(df_reg) == 0) {
      showNotification("Data kosong setelah menghilangkan baris dengan nilai hilang (NA) pada variabel yang dipilih.", type = "warning")
      return(NULL)
    }
    
    formula_str <- paste(input$regresi_y, "~", paste(input$regresi_x, collapse = " + "))
    tryCatch({
      lm(as.formula(formula_str), data = df_reg)
    }, error = function(e) {
      showNotification(paste("Error saat menjalankan model regresi:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$hasil_regresi <- renderPrint({
    model <- reg_model()
    if (!is.null(model)) summary(model)
  })
  
  output$interpret_regresi <- renderUI({
    model_summary <- summary(reg_model())
    if (is.null(model_summary)) return(HTML("<p>Tidak ada hasil interpretasi.</p>"))
    
    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r.squared
    f_statistic <- model_summary$fstatistic
    f_pvalue <- pf(f_statistic[1], f_statistic[2], f_statistic[3], lower.tail = FALSE)
    
    # Interpretasi model keseluruhan
    overall_interpret <- paste0(
      "<p><b>R-squared:</b> ", sprintf("%.3f", r_squared), " (Adjusted R-squared: ", sprintf("%.3f", adj_r_squared), "). Ini menunjukkan proporsi variabilitas variabel dependen (", input$regresi_y, ") yang dapat dijelaskan oleh model.</p>",
      "<p><b>Uji F Model Keseluruhan:</b> ", interpret_pval(f_pvalue),
      " Ini menunjukkan apakah setidaknya satu variabel independen secara signifikan memprediksi variabel dependen.</p>"
    )
    
    # Interpretasi koefisien
    coeff_interpret <- "<h4>Interpretasi Koefisien:</h4><ul>"
    coefs <- as.data.frame(model_summary$coefficients)
    names(coefs) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
    
    for (i in 1:nrow(coefs)) {
      var_name <- rownames(coefs)[i]
      estimate <- sprintf("%.3f", coefs$Estimate[i])
      p_val_coeff <- coefs$`Pr(>|t|)`[i]
      
      if (var_name == "(Intercept)") {
        coeff_interpret <- paste0(coeff_interpret, "<li><b>Intercept:</b> ", estimate, ". Ini adalah nilai rata-rata dari ", input$regresi_y, " ketika semua variabel independen bernilai nol.</li>")
      } else {
        coeff_interpret <- paste0(coeff_interpret, "<li><b>", var_name, ":</b> ", estimate, ". Ini menunjukkan perubahan rata-rata pada ", input$regresi_y, " untuk setiap peningkatan satu unit pada ", var_name, ", dengan variabel lain konstan. ", interpret_pval(p_val_coeff), "</li>")
      }
    }
    coeff_interpret <- paste0(coeff_interpret, "</ul>")
    
    HTML(paste0("<h3>Interpretasi Model Regresi:</h3>", overall_interpret, coeff_interpret))
  })
  
  output$plot_asumsi_regresi <- renderPlot({
    model <- reg_model()
    if (!is.null(model)) {
      par(mfrow = c(2, 2), oma = c(0, 0, 3, 0), mar = c(4, 4, 3, 2) + 0.1) # Set up 2x2 plot layout
      plot(model)
      mtext("Plot Diagnostik Regresi", side = 3, line = -1.5, outer = TRUE, cex = 1.5, font = 2)
      par(mfrow = c(1, 1)) # Reset plot layout
    }
  })
  
  output$asumsi_regresi_results <- renderUI({
    model <- reg_model()
    if (is.null(model)) return(HTML("<p>Tidak ada hasil uji asumsi.</p>"))
    
    shapiro_res <- NULL
    bp_res <- NULL
    dw_res <- NULL
    
    tryCatch({
      # Normalitas Residual (Shapiro-Wilk)
      residuals_model <- residuals(model)
      if (length(residuals_model) > 5000) { # Shapiro-Wilk limit
        shapiro_res <- list(p.value = ks.test(scale(residuals_model), "pnorm")$p.value, method = "Kolmogorov-Smirnov")
        shapiro_text <- paste0("<b>Normalitas Residual (Kolmogorov-Smirnov):</b> ", interpret_pval(shapiro_res$p.value))
      } else if (length(residuals_model) >= 3) {
        shapiro_res <- shapiro.test(residuals_model)
        shapiro_text <- paste0("<b>Normalitas Residual (Shapiro-Wilk):</b> ", interpret_pval(shapiro_res$p.value))
      } else {
        shapiro_text <- "<b>Normalitas Residual:</b> Tidak cukup data untuk uji."
      }
      
      # Homoskedastisitas (Breusch-Pagan Test / ncvTest from car)
      bp_res <- ncvTest(model)
      bp_text <- paste0("<b>Homoskedastisitas (Breusch-Pagan):</b> ", interpret_pval(bp_res$p))
      
      # Non-Autokorelasi (Durbin-Watson Test)
      dw_res <- durbinWatsonTest(model)
      dw_text <- paste0("<b>Non-Autokorelasi (Durbin-Watson):</b> ", interpret_pval(dw_res$p))
      
    }, error = function(e) {
      showNotification(paste("Error saat menjalankan uji asumsi:", e$message), type = "error")
      return(NULL)
    })
    
    HTML(paste(shapiro_text, bp_text, dw_text, sep = "<br>"))
  })
  
  
  output$unduh_regresi_report <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_regresi_report), 
                    "PDF" = ".pdf", 
                    "WORD" = ".docx",
                    ".pdf")  # default
      paste0('laporan_regresi_', Sys.Date(), ext)
    },
    content = function(file) {
      model <- reg_model()
      if (is.null(model)) {
        showNotification("Tidak ada hasil regresi untuk diunduh.", type = "warning")
        return()
      }
      
      # Save diagnostic plots
      plot_path <- tempfile(fileext = ".png")
      png(plot_path, width = 800, height = 800) # Increased resolution
      par(mfrow = c(2, 2))
      plot(model)
      dev.off()
      par(mfrow = c(1, 1)) # Reset par
      
      # Buat interpretasi dalam format text
      interpretasi_text <- get_interpretasi_text(output$interpret_regresi())
      asumsi_text <- get_interpretasi_text(output$asumsi_regresi_results())
      
      params <- list(
        formula = formula(model),
        summary_output = capture.output(summary(model)),
        assumption_text = asumsi_text,
        interpretation_text = interpretasi_text,
        plot_path = plot_path,
        y_var = input$regresi_y,
        x_vars = input$regresi_x
      )
      
      # Render ke temporary file dulu
      temp_file <- tempfile(fileext = switch(toupper(input$format_regresi_report), 
                                             "PDF" = ".pdf", 
                                             "WORD" = ".docx",
                                             ".pdf"))
      
      ok <- render_report("rlb_report.Rmd", temp_file, params,
                          switch(toupper(input$format_regresi_report), 
                                 "PDF" = rmarkdown::pdf_document(), 
                                 "WORD" = rmarkdown::word_document(),
                                 rmarkdown::pdf_document()))
      
      if (!ok || !file.exists(temp_file) || file.size(temp_file) == 0) {
        showNotification("Gagal membuat file PDF/Word. Pastikan Pandoc dan LaTeX (untuk PDF) sudah terinstall.", type = "error")
        return()
      }
      
      # Copy ke file download
      file.copy(temp_file, file, overwrite = TRUE)
    },
    contentType = function() {
      switch(
        toupper(input$format_regresi_report),
        "PDF" = "application/pdf",
        "WORD" = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        "application/pdf"
      )
    }
  )
  
  # --- Unduh Gabungan ---
  output$tampilan_histori <- renderPrint({
    log_data <- histori_sesi()
    if(length(log_data) == 0) {
      "Belum ada aktivitas analisis yang tercatat dalam sesi ini."
    } else {
      cat(paste(log_data, collapse = "\n"))
    }
  })
  
  output$unduh_gabungan <- downloadHandler(
    filename = function() {
      ext <- switch(toupper(input$format_gabungan_report), 
                    "PDF" = ".pdf", 
                    "WORD" = ".docx",
                    ".pdf")  # default
      paste0("Histori_Sesi_Analisis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ext)
    },
    content = function(file) {
      log_data <- histori_sesi()
      
      # Create summary info
      summary_info <- paste0(
        "**Tanggal Sesi:** ", format(Sys.Date(), "%d %B %Y"), "\n\n",
        "**Waktu Pembuatan Laporan:** ", format(Sys.time(), "%H:%M:%S"), "\n\n",
        "**Total Aktivitas:** ", length(log_data), " aktivitas\n\n",
        "**Dataset:** SOVI (Social Vulnerability Index) Data"
      )
      
      # Prepare plots list (collect saved plots if any)
      plots_list <- values$saved_plots
      
      params <- list(
        log_data = log_data,
        plots_list = plots_list,
        summary_info = summary_info
      )
      
      # Render ke temporary file dulu
      temp_file <- tempfile(fileext = switch(toupper(input$format_gabungan_report), 
                                             "PDF" = ".pdf", 
                                             "WORD" = ".docx",
                                             ".pdf"))
      
      ok <- render_report("gabungan_report.Rmd", temp_file, params,
                          switch(toupper(input$format_gabungan_report), 
                                 "PDF" = rmarkdown::pdf_document(), 
                                 "WORD" = rmarkdown::word_document(),
                                 rmarkdown::pdf_document()))
      
      if (!ok || !file.exists(temp_file) || file.size(temp_file) == 0) {
        showNotification("Gagal membuat file PDF/Word. Pastikan Pandoc dan LaTeX (untuk PDF) sudah terinstall.", type = "error")
        return()
      }
      
      # Copy ke file download
      file.copy(temp_file, file, overwrite = TRUE)
    },
    contentType = function() {
      switch(
        toupper(input$format_gabungan_report),
        "PDF" = "application/pdf",
        "WORD" = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        "application/pdf"
      )
    }
  )
  
  # --- Uji Proporsi 1 Sampel: Plot (Contoh Barplot) ---
  output$plot_prop1 <- renderPlot({
    req(input$prop1_var, input$prop1_level)
    df <- values$data
    if (input$prop1_var %in% names(df)) {
      tab <- table(df[[input$prop1_var]])
      bar_df <- data.frame(Level = names(tab), Count = as.numeric(tab))
      ggplot(bar_df, aes(x = Level, y = Count, fill = Level)) +
        geom_bar(stat = "identity", fill = "#f78fb3", color = "#d65076") +
        labs(title = paste("Distribusi", input$prop1_var), x = input$prop1_var, y = "Frekuensi") +
        theme_minimal()
    }
  })
  
  # --- Uji Proporsi 2 Sampel: Plot (Contoh Barplot) ---
  output$plot_prop2 <- renderPlot({
    req(input$prop2_var_cat)
    df <- values$data
    if (input$prop2_var_cat %in% names(df)) {
      tab <- table(df[[input$prop2_var_cat]])
      bar_df <- data.frame(Level = names(tab), Count = as.numeric(tab))
      ggplot(bar_df, aes(x = Level, y = Count, fill = Level)) +
        geom_bar(stat = "identity", fill = "#f78fb3", color = "#d65076") +
        labs(title = paste("Distribusi", input$prop2_var_cat), x = input$prop2_var_cat, y = "Frekuensi") +
        theme_minimal()
    }
  })
  
  # --- Uji Varians 1 Sampel: Plot (Histogram) ---
  output$plot_var1 <- renderPlot({
    req(input$var1_var)
    x <- na.omit(values$data[[input$var1_var]])
    if (length(x) > 0) {
      df_plot <- data.frame(Value = x)
      ggplot(df_plot, aes(x = Value)) +
        geom_histogram(bins = 20, fill = "#f78fb3", alpha = 0.7, color = "#fff0f5") +
        labs(title = paste("Distribusi", input$var1_var), x = input$var1_var, y = "Frekuensi") +
        theme_minimal()
    }
  })
  
  # --- Uji Varians 2 Sampel: Plot (Boxplot) ---
  output$plot_var2 <- renderPlot({
    req(input$var2_var_num, input$var2_var_cat)
    df <- values$data
    if (input$var2_var_num %in% names(df) && input$var2_var_cat %in% names(df)) {
      ggplot(df, aes_string(x = input$var2_var_cat, y = input$var2_var_num)) +
        geom_boxplot(fill = "#fbd6e3", color = "#d65076", alpha = 0.7) +
        geom_jitter(width = 0.2, alpha = 0.5, color = "#d65076") +
        labs(title = paste("Perbandingan Varians", input$var2_var_num, "berdasarkan", input$var2_var_cat),
             x = input$var2_var_cat, y = input$var2_var_num) +
        theme_minimal() +
        theme(legend.position = "none")
    }
  })
  
  # --- ANOVA: Plot (Boxplot Soft Pink) ---
  output$plot_anova <- renderPlot({
    req(input$anova_y, input$anova_group1)
    df <- values$data
    if (input$anova_y %in% names(df) && input$anova_group1 %in% names(df)) {
      ggplot(df, aes_string(x = input$anova_group1, y = input$anova_y)) +
        geom_boxplot(fill = "#fbd6e3", color = "#d65076", alpha = 0.7) +
        geom_jitter(width = 0.2, alpha = 0.5, color = "#d65076") +
        labs(title = paste("Boxplot ANOVA", input$anova_y, "berdasarkan", input$anova_group1),
             x = input$anova_group1, y = input$anova_y) +
        theme_minimal() +
        theme(legend.position = "none")
    }
  })
  
})