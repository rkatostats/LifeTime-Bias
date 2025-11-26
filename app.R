# Simulação de Análise de Sobrevivência - Shiny App
# Rodrigo Reginato Kakuta Kato

# Carregar pacotes necessários
library(shiny)
library(tidyverse)
library(survminer)
library(survival)
library(DT)
library(glue)

# Definir intervalos e rótulos globais
interval_breaks <- c(0, 1, 2, 3, 4, 5)
interval_labels <- c("(0,1]", "(1,2]", "(2,3]", "(3,4]", "(4,5]")

# ===== UI =====
ui <- fluidPage(
    # CSS personalizado
    tags$head(
        tags$style(HTML(
            "
      .central-title {
        text-align: center;
        margin-top: 10px;
        margin-bottom: 30px;
        font-size: 2em;
        font-weight: bold;
      }
      .fixed-sidebar {
        position: fixed;
        top: 0;
        left: 0;
        height: 100vh;
        width: 320px;
        overflow-y: auto;
        background: #f8f9fa;
        border-right: 1px solid #ddd;
        z-index: 1000;
        padding: 20px 10px 15px 10px;
        box-shadow: 2px 0 8px rgba(0,0,0,0.06);
      }
      .main-panel-move {
        margin-left: 330px;
      }
      @media (max-width: 600px) {
        .fixed-sidebar {
          width: 100vw;
          height: auto;
          position: static;
          border-right: none;
          box-shadow: none;
        }
        .main-panel-move {
          margin-left: 0;
        }
      }
    "
        ))
    ),

    # Título central
    div(
        class = "central-title",
        tags$small("Simulação de Análise de Sobrevivência"),
        tags$br(),
        tags$small("Rodrigo Reginato Kakuta Kato")
    ),

    # Sidebar fixa
    div(
        class = "fixed-sidebar",
        h4("Grupo A"),
        tags$b("Censura por intervalo"),
        lapply(seq_along(interval_labels), function(i) {
            sliderInput(
                inputId = paste0("censA_", i),
                label = paste0(interval_labels[i], " | % censura"),
                min = 0,
                max = 100,
                value = 0,
                step = 5
            )
        }),
        tags$b("Remoção de amostras por intervalo"),
        lapply(seq_along(interval_labels), function(i) {
            sliderInput(
                inputId = paste0("remA_", i),
                label = paste0(interval_labels[i], " | % remoção"),
                min = 0,
                max = 100,
                value = 0,
                step = 5
            )
        }),
        tags$hr(),
        h4("Grupo B"),
        tags$b("Censura por intervalo"),
        lapply(seq_along(interval_labels), function(i) {
            sliderInput(
                inputId = paste0("censB_", i),
                label = paste0(interval_labels[i], " | % censura"),
                min = 0,
                max = 100,
                value = 0,
                step = 5
            )
        }),
        tags$b("Remoção de amostras por intervalo"),
        lapply(seq_along(interval_labels), function(i) {
            sliderInput(
                inputId = paste0("remB_", i),
                label = paste0(interval_labels[i], " | % remoção"),
                min = 0,
                max = 100,
                value = 0,
                step = 5
            )
        })
    ),

    # Painel principal
    div(
        class = "main-panel-move",
        tags$hr(),
        plotOutput("curva_surv"),
        tags$hr(),
        fluidRow(
            column(
                6,
                textInput(
                    inputId = "script_filename",
                    label = "Nome do arquivo do script (.R):",
                    value = "script_gerado"
                ),
                downloadButton(
                    outputId = "download_script",
                    label = "Baixar Script"
                )
            ),
            column(
                6,
                textInput(
                    inputId = "grafico_filename",
                    label = "Nome do arquivo do gráfico (.jpeg):",
                    value = "curva_sobrevivencia"
                ),
                downloadButton(
                    outputId = "download_grafico",
                    label = "Baixar Gráfico"
                )
            )
        ),
        tags$hr(),
        h2("Estatísticas de Sobrevivência"),
        verbatimTextOutput(outputId = "Statistics_simple"),
        tags$hr(),
        h2("Tabela dos dados"),
        DT::dataTableOutput("table")
    )
)

# ===== SERVER =====
server <- function(input, output, session) {
    # Reactive: processar dados e criar gráfico
    surv_data_and_plot <- reactive({
        base_data <- read.csv("data.csv", header = TRUE, sep = ",")
        base_data$Time <- as.numeric(base_data$Time)

        dados <- base_data
        if ("A" %in% unique(dados$Group) && "B" %in% unique(dados$Group)) {
            temposA <- dados |> dplyr::filter(Group == "A") |> dplyr::pull(Time)
            idxB <- which(dados$Group == "B")
            nB <- length(idxB)
            nA <- length(temposA)
            if (nA > 0 && nB > 0) {
                temposB <- sort(rep(temposA, length.out = nB))
                dados$Time[idxB] <- temposB
            }
        }
        dados$Group <- as.factor(dados$Group)

        dados <- dados |>
            dplyr::mutate(
                Interval = cut(
                    Time,
                    breaks = interval_breaks,
                    labels = interval_labels,
                    right = TRUE,
                    include.lowest = FALSE
                ),
                Event = 1,
                Censoring = 0
            )

        # REMOÇÃO de amostras por intervalo
        for (grupo in c("A", "B")) {
            for (i in seq_along(interval_labels)) {
                percent <- if (grupo == "A") {
                    input[[paste0("remA_", i)]]
                } else {
                    input[[paste0("remB_", i)]]
                }
                interval <- interval_labels[i]
                if (percent > 0) {
                    idx <- which(
                        dados$Group == grupo & dados$Interval == interval
                    )
                    n_idx <- length(idx)
                    n_rem <- round(n_idx * percent / 100)
                    if (n_rem > 0 && n_idx > 0) {
                        set.seed(123)
                        rem_idx <- sample(idx, n_rem)
                        dados <- dados[-rem_idx, ]
                    }
                }
            }
        }

        # CENSURA manual por intervalo
        for (grupo in c("A", "B")) {
            for (i in seq_along(interval_labels)) {
                percent <- if (grupo == "A") {
                    input[[paste0("censA_", i)]]
                } else {
                    input[[paste0("censB_", i)]]
                }
                interval <- interval_labels[i]
                if (percent > 0) {
                    idx <- which(
                        dados$Group == grupo &
                            dados$Interval == interval &
                            dados$Censoring == 0
                    )
                    n_idx <- length(idx)
                    n_cens <- round(n_idx * percent / 100)
                    if (n_cens > 0 && n_idx > 0) {
                        set.seed(123)
                        cens_idx <- sample(idx, n_cens)
                        dados$Event[cens_idx] <- 0
                        dados$Censoring[cens_idx] <- 1
                    }
                }
            }
        }

        # Criar gráfico de sobrevivência
        legenda_grupos <- sort(unique(dados$Group))
        legend_labs <- if (all(legenda_grupos %in% c("A", "B"))) {
            c("Grupo A", "Grupo B")
        } else {
            as.character(legenda_grupos)
        }
        fit <- survfit(Surv(Time, Event) ~ Group, data = dados)
        g <- ggsurvplot(
            fit,
            data = dados,
            legend.title = "Grupo",
            legend.labs = legend_labs,
            ggtheme = ggplot2::theme_minimal(),
            xlab = "Tempo",
            ylab = "Probabilidade de Sobrevivência",
            surv.median.line = "none"
        )
        g$plot <- g$plot +
            ggplot2::scale_color_manual(
                values = c("#E41A1C", "#377EB8")[seq_along(legend_labs)],
                labels = legend_labs,
                name = "Grupo"
            ) +
            ggplot2::scale_fill_manual(
                values = c("#E41A1C", "#377EB8")[seq_along(legend_labs)],
                labels = legend_labs,
                name = "Grupo"
            ) +
            ggplot2::theme(
                legend.text = ggplot2::element_text(size = 12),
                legend.title = ggplot2::element_blank()
            )
        list(
            df = dados,
            plot = g$plot
        )
    })

    # Output: Tabela de dados
    output$table <- DT::renderDataTable(
        surv_data_and_plot()$df,
        options = list(pageLength = 10, autoWidth = TRUE)
    )

    # Output: Curva de sobrevivência
    output$curva_surv <- renderPlot({
        print(surv_data_and_plot()$plot)
    })

    # Output: Estatísticas simples
    output$Statistics_simple <- renderPrint({
        df <- surv_data_and_plot()$df

        cat(
            "Logrank p-value: ",
            survdiff(Surv(Time, Event) ~ Group, data = df)$pvalue
        )
        cat(
            "\n-------------------------------------------------------------------\n"
        )

        kmi <- survfit(Surv(Time, Censoring) ~ Group, data = df)
        kmi
    })

    # Download handler: Script R
    output$download_script <- downloadHandler(
        filename = function() {
            fname <- input$script_filename
            if (is.null(fname) || fname == "") {
                fname <- "script_gerado"
            }
            fname <- sub("\\.R$", "", fname, ignore.case = TRUE)
            paste0(fname, ".R")
        },
        content = function(file) {
            censura_percentual_A <- sapply(
                seq_along(interval_labels),
                function(i) input[[paste0("censA_", i)]]
            )
            censura_percentual_B <- sapply(
                seq_along(interval_labels),
                function(i) input[[paste0("censB_", i)]]
            )
            remocao_percentual_A <- sapply(
                seq_along(interval_labels),
                function(i) input[[paste0("remA_", i)]]
            )
            remocao_percentual_B <- sapply(
                seq_along(interval_labels),
                function(i) input[[paste0("remB_", i)]]
            )

            script <- glue::glue(
                '
options(encoding = "UTF-8")
try(Sys.setlocale("LC_ALL", "Portuguese_Brazil.utf8"), silent = TRUE)
try(Sys.setlocale("LC_ALL", "pt_BR.UTF-8"), silent = TRUE)
try(Sys.setlocale("LC_CTYPE", "UTF-8"), silent = TRUE)
# Carregar pacotes necessários
library(tidyverse)
library(survminer)
library(survival)

# Definir intervalos e rótulos
quebras_intervalos <- c(0, 1, 2, 3, 4, 5)
rotulos_intervalos <- c("(0,1]", "(1,2]", "(2,3]", "(3,4]", "(4,5]")

# Parâmetros de censura e remoção definidos na interface
percentual_censura_A <- c({paste(censura_percentual_A, collapse = ", ")})
percentual_censura_B <- c({paste(censura_percentual_B, collapse = ", ")})
percentual_remocao_A <- c({paste(remocao_percentual_A, collapse = ", ")})
percentual_remocao_B <- c({paste(remocao_percentual_B, collapse = ", ")})

# Carregamento dos dados originais
dados_originais <- read.csv("data.csv", header = TRUE, sep = ",")
dados_originais$Time <- as.numeric(dados_originais$Time)

# Fazer Grupo B igual ao Grupo A (mesmo tempo, ORDENADO, sem duplicar linhas)
dados_simulados <- dados_originais
if ("A" %in% unique(dados_simulados$Group) 
&& "B" %in% unique(dados_simulados$Group)) {{
  tempos_grupoA <- dados_simulados |> filter(Group == "A") |> pull(Time)
  indices_grupoB <- which(dados_simulados$Group == "B")
  n_grupoB <- length(indices_grupoB)
  n_grupoA <- length(tempos_grupoA)
  if (n_grupoA > 0 && n_grupoB > 0) {{
    tempos_grupoB <- sort(rep(tempos_grupoA, length.out = n_grupoB))
    dados_simulados$Time[indices_grupoB] <- tempos_grupoB
  }}
}}
dados_simulados$Group <- as.factor(dados_simulados$Group)

# Categorizar intervalos
dados_simulados <- dados_simulados |>
  mutate(
    Intervalo = cut(
      Time,
      breaks = quebras_intervalos,
      labels = rotulos_intervalos,
      right = TRUE,
      include.lowest = FALSE
    ),
    Evento = 1,
    Censurado = 0
  )

# REMOÇÃO de amostras por intervalo
for (grupo_nome in c("A", "B")) {{
  for (i in seq_along(rotulos_intervalos)) {{
    percentual_remover <- 
    if (grupo_nome == "A") percentual_remocao_A[i] else percentual_remocao_B[i]
    intervalo_atual <- rotulos_intervalos[i]
    if (percentual_remover > 0) {{
      indices_para_verificar <- which(
      dados_simulados$Group == grupo_nome 
      & dados_simulados$Intervalo == intervalo_atual
      )
      total_indices <- length(indices_para_verificar)
      total_remover <- round(total_indices * percentual_remover / 100)
      if (total_remover > 0 && total_indices > 0) {{
        set.seed(123)
        indices_remover <- sample(indices_para_verificar, total_remover)
        dados_simulados <- dados_simulados[-indices_remover, ]
      }}
    }}
  }}
}}

# CENSURA manual por intervalo
for (grupo_nome in c("A", "B")) {{
  for (i in seq_along(rotulos_intervalos)) {{
    percentual_censurar <- 
    if (grupo_nome == "A") percentual_censura_A[i] else percentual_censura_B[i]
    intervalo_atual <- rotulos_intervalos[i]
    if (percentual_censurar > 0) {{
      indices_para_censurar <- which(
      dados_simulados$Group == grupo_nome 
      & dados_simulados$Intervalo == intervalo_atual 
      & dados_simulados$Censurado == 0
      )
      total_para_censurar <- length(indices_para_censurar)
      total_censurar <- round(total_para_censurar * percentual_censurar / 100)
      if (total_censurar > 0 && total_para_censurar > 0) {{
        set.seed(123)
        indices_censurados <- sample(indices_para_censurar, total_censurar)
        dados_simulados$Evento[indices_censurados] <- 0
        dados_simulados$Censurado[indices_censurados] <- 1
      }}
    }}
  }}
}}

# Ajuste para nomes padronizados no Surv()
dados_simulados$Tempo <- dados_simulados$Time
dados_simulados$Evento <- dados_simulados$Evento

# Curva de sobrevivência de Kaplan-Meier
ajuste_km <- survfit(Surv(Tempo, Evento) ~ Group, data = dados_simulados)
grafico_km <- ggsurvplot(
  ajuste_km,
  data = dados_simulados,
  legend.title = "Grupo",
  legend.labs = c("Grupo A", "Grupo B"),
  ggtheme = theme_minimal(),
  xlab = "Tempo",
  ylab = "Probabilidade de Sobrevivência",
  surv.median.line = "none"
)
grafico_km$plot <- grafico_km$plot + 
  scale_color_manual(
  values = c("#E41A1C", "#377EB8"), 
  labels = c("Grupo A", "Grupo B"), 
  name = "Grupo"
  ) +
  scale_fill_manual(
  values = c("#E41A1C", "#377EB8"), 
  labels = c("Grupo A", "Grupo B"),
  name = "Grupo"
  ) +
  theme(
  legend.text = element_text(size = 14),
  legend.title = element_blank(),
  axis.title = element_text(size = 16),
  axis.text = element_text(size = 14),
  plot.title = element_text(size = 18, hjust = 0.5)
  )
grafico_km$plot

cat("\\nResumo do ajuste considerando censura:\\n")
print(survfit(Surv(Tempo, Censurado) ~ Group, data = dados_simulados))
cat("\\nResumo do ajuste considerando eventos:\\n")
print(survfit(Surv(Tempo, Evento) ~ Group, data = dados_simulados))
cat("\\nP-value do teste Logrank\\n")
cat("Logrank p-value: ", survdiff(Surv(Tempo, Evento) ~ Group, 
data = dados_simulados)$pvalue)

'
            )
            writeLines(script, file)
        }
    )

    # Download handler: Gráfico JPEG
    output$download_grafico <- downloadHandler(
        filename = function() {
            nome_grafico <- input$grafico_filename
            if (is.null(nome_grafico) || nome_grafico == "") {
                nome_grafico <- "curva_sobrevivencia"
            }
            nome_grafico <- sub(
                "\\.jpeg$|\\.jpg$",
                "",
                nome_grafico,
                ignore.case = TRUE
            )
            paste0(nome_grafico, ".jpeg")
        },
        content = function(file) {
            plot_obj <- surv_data_and_plot()$plot
            jpeg(file, width = 1200, height = 900, res = 120)
            print(plot_obj)
            dev.off()
        },
        contentType = "image/jpeg"
    )
}

# ===== RUN APP =====
shiny::shinyApp(ui = ui, server = server)
