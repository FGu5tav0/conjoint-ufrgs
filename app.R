# app.R
library(shiny)
library(sortable)
library(dplyr)
library(googlesheets4)
library(gargle)

# ---- Configuração Google Sheets ----
SHEET_ID <- "1alRcGwPLKAMJ7b5K5z7aV0a6AfBbpez1s9xsnwjNBs4"

# Autenticação
options(gargle_oauth_cache = ".secrets")
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = "negaofrosi@gmail.com"
)

# ---- Dados dos estímulos ----
orientacao <- c("Semanal", "Mensal", "Sob demanda")
# formato <- c("Presencial", "Híbrido")
producao <- c("Até 2 artigos", "De 3 a 5 artigos", "Mais de 5 artigos")

stimuli <- expand.grid(
  Orientacao = orientacao,
  # Formato = formato,
  Producao = producao,
  stringsAsFactors = FALSE
) %>%
  mutate(Estimulo = row_number())

numero_ordinal <- function(n) {
  paste0(n, "º")
}

cores_orientacao <- c(
  "Semanal" = "#FFCDD2",
  "Mensal" = "#F8BBD0",
  "Sob demanda" = "#E1BEE7"
)

cores_formato <- c(
  "Presencial" = "#BBDEFB",
  "Híbrido" = "#B2EBF2"
)

cores_producao <- c(
  "Até 2 artigos" = "#C8E6C9",
  "De 3 a 5 artigos" = "#DCEDC8",
  "Mais de 5 artigos" = "#FFF9C4"
)

criar_label <- function(i) {
  cor_orient <- cores_orientacao[stimuli$Orientacao[i]]
  # cor_formato <- cores_formato[stimuli$Formato[i]]
  cor_producao <- cores_producao[stimuli$Producao[i]]

  tags$div(
    style = "padding: 16px; border-radius: 12px; margin: 10px 0; border: 2px solid #ddd;",

    # Título + número
    tags$div(
      style = "display: flex; align-items: center; gap: 12px;",
      tags$div(
        style = "background: #764ba2; 
                 color: white; border-radius: 50%; width: 32px; height: 32px; 
                 display: flex; align-items: center; justify-content: center; 
                 font-weight: bold; font-size: 14px;",
        i
      ),

      tags$div(
        style = "flex: 1;",
        tags$div(
          style = "font-weight: 600; font-size: 15px; color: #2c3e50;",
          paste0("Opção ", i)
        )
      )
    ),

    # Blocos coloridos
    tags$div(
      style = "margin-top: 10px; display: flex; gap: 8px; flex-wrap: wrap;",

      tags$span(
        style = sprintf(
          "background:%s; padding:6px 10px; border-radius:8px; font-size:13px; font-weight:600;",
          cor_orient
        ),
        icon("calendar"),
        " ",
        stimuli$Orientacao[i]
      ),

      # tags$span(
      #   style = sprintf(
      #     "background:%s; padding:6px 10px; border-radius:8px; font-size:13px; font-weight:600;",
      #     cor_formato
      #   ),
      #   icon("laptop"),
      #   " ",
      #   stimuli$Formato[i]
      # ),

      tags$span(
        style = sprintf(
          "background:%s; padding:6px 10px; border-radius:8px; font-size:13px; font-weight:600;",
          cor_producao
        ),
        icon("file-alt"),
        " ",
        stimuli$Producao[i]
      )
    )
  )
}


labels_cards <- lapply(1:nrow(stimuli), criar_label)

# ---- UI ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
      
      body {
        font-family: 'Inter', sans-serif;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        min-height: 100vh;
        padding: 20px 0;
      }
      
      .container-fluid {
        max-width: 1400px;
        margin: 0 auto;
      }
      
      .main-card {
        background: white;
        border-radius: 20px;
        padding: 40px;
        box-shadow: 0 20px 60px rgba(0,0,0,0.3);
        margin-bottom: 30px;
      }
      
      .header-title {
        text-align: center;
        margin-bottom: 40px;
      }
      
      .header-title h1 {
        color: #2c3e50;
        font-weight: 700;
        font-size: 32px;
        margin-bottom: 10px;
      }
      
      .header-title p {
        color: #7f8c8d;
        font-size: 16px;
      }
      
      .form-section {
        background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
        padding: 30px;
        border-radius: 15px;
        margin-bottom: 30px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
      }
      
      .form-section h4 {
        color: #2c3e50;
        font-weight: 600;
        margin-bottom: 25px;
        display: flex;
        align-items: center;
        gap: 10px;
      }
      
      .form-control {
        border-radius: 10px;
        border: 2px solid #e0e6ed;
        padding: 12px 16px;
        font-size: 14px;
        transition: all 0.3s;
      }
      
      .form-control:focus {
        border-color: #667eea;
        box-shadow: 0 0 0 0.2rem rgba(102, 126, 234, 0.25);
      }
      
      .form-group label {
        font-weight: 600;
        color: #2c3e50;
        margin-bottom: 8px;
        font-size: 14px;
      }
      
      .nivel-buttons {
        display: flex;
        gap: 10px;
        width: 100%;
      }
      
      .nivel-btn {
        flex: 1;
        padding: 12px 20px;
        border: 2px solid #e0e6ed;
        background: white;
        border-radius: 10px;
        cursor: pointer;
        transition: all 0.3s;
        font-weight: 600;
        text-align: center;
        font-size: 14px;
        color: #2c3e50;
      }
      
      .nivel-btn:hover {
        border-color: #667eea;
        background: #f8f9ff;
        transform: translateY(-2px);
      }
      
      .nivel-btn.active {
        border-color: #667eea;
        background: #667eea;
        color: white;
        box-shadow: 0 4px 12px rgba(102, 126, 234, 0.4);
      }
      
      .year-buttons {
        display: flex;
        gap: 10px;
        flex-wrap: wrap;
      }
      
      .year-btn {
        flex: 1;
        min-width: 80px;
        padding: 12px 20px;
        border: 2px solid #e0e6ed;
        background: white;
        border-radius: 10px;
        cursor: pointer;
        transition: all 0.3s;
        font-weight: 600;
        text-align: center;
        font-size: 14px;
        color: #2c3e50;
      }
      
      .year-btn:hover {
        border-color: #667eea;
        background: #f8f9ff;
        transform: translateY(-2px);
      }
      
      .year-btn.active {
        border-color: #667eea;
        background: #667eea;
        color: white;
        box-shadow: 0 4px 12px rgba(102, 126, 234, 0.4);
      }
      
      .instruction-box {
        background: #e3f2fd;
        border-left: 4px solid #2196F3;
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 25px;
      }
      
      .instruction-box h4 {
        color: #1976D2;
        font-weight: 600;
        margin-bottom: 12px;
        font-size: 18px;
      }
      
      .instruction-box p {
        color: #0d47a1;
        margin-bottom: 8px;
        line-height: 1.6;
      }
      
      .rank-list-container {
        background: #ffffff;
        border: 3px dashed #b0bec5;
        border-radius: 15px;
        padding: 20px;
        min-height: 500px;
        transition: all 0.3s;
      }
      
      .rank-list-container:hover {
        border-color: #667eea;
        background: #fafbff;
      }
      
      .rank-list-item {
        cursor: grab;
        transition: all 0.3s ease;
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #e0e6ed;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      }
      
      .rank-list-item:hover {
        transform: translateX(8px) scale(1.02);
        box-shadow: 0 8px 25px rgba(102, 126, 234, 0.3);
        border-color: #667eea;
      }
      
      .rank-list-item:active {
        cursor: grabbing;
        background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%);
        border-color: #2196F3;
        transform: scale(1.05);
      }
      
      .rank-list-item.sortable-chosen {
        background: linear-gradient(135deg, #fff9c4 0%, #fff59d 100%);
        border-color: #ffc107;
        box-shadow: 0 10px 30px rgba(255, 193, 7, 0.5);
        transform: scale(1.08);
        opacity: 0.95;
      }
      
      .rank-list-item.sortable-ghost {
        background: linear-gradient(135deg, #e1f5fe 0%, #b3e5fc 100%);
        border: 3px dashed #0dcaf0;
        opacity: 0.6;
      }
      
      .btn-save {
        width: 100%;
        padding: 16px;
        font-size: 18px;
        font-weight: 600;
        border-radius: 12px;
        border: none;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        cursor: pointer;
        transition: all 0.3s;
        box-shadow: 0 4px 15px rgba(102, 126, 234, 0.4);
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 10px;
      }
      
      .btn-save:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 25px rgba(102, 126, 234, 0.6);
      }
      
      .btn-save:active {
        transform: translateY(-1px);
      }
      
      .alert {
        border-radius: 10px;
        padding: 16px;
        margin-top: 20px;
        border: none;
        font-size: 15px;
        animation: slideIn 0.3s ease;
      }
      
      @keyframes slideIn {
        from {
          opacity: 0;
          transform: translateY(-10px);
        }
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }
      
      .alert-success {
        background: linear-gradient(135deg, #d4edda 0%, #c3e6cb 100%);
        color: #155724;
        border-left: 4px solid #28a745;
      }
      
      .alert-danger {
        background: linear-gradient(135deg, #f8d7da 0%, #f5c6cb 100%);
        color: #721c24;
        border-left: 4px solid #dc3545;
      }
      
      .section-title {
        color: #2c3e50;
        font-weight: 600;
        margin-bottom: 20px;
        font-size: 20px;
        display: flex;
        align-items: center;
        gap: 10px;
      }
      
      .required-mark {
        color: #e74c3c;
        font-weight: bold;
      }
    "
    ))
  ),

  div(
    class = "main-card",
    div(
      class = "header-title",
      h1(icon("star"), " Ranking de Preferências"),
      p("Pesquisa Acadêmica")
    ),

    div(
      class = "form-section",
      h4(icon("user"), " Dados do Participante"),
      fluidRow(
        column(
          width = 4,
          div(
            class = "form-group",
            tags$label("Nome (opcional)"),
            textInput(
              "nome_participante",
              NULL,
              placeholder = "Digite seu nome completo",
              width = "100%"
            )
          )
        ),
        column(
          width = 4,
          div(
            class = "form-group",
            tags$label(HTML(
              "Área de pesquisa <span class='required-mark'>*</span>"
            )),
            textInput(
              "area_pesquisa",
              NULL,
              placeholder = "Ex: Física do Solo",
              width = "100%"
            )
          )
        ),
        column(
          width = 4,
          div(
            class = "form-group",
            tags$label(HTML(
              "Nível acadêmico <span class='required-mark'>*</span>"
            )),
            div(
              class = "radio-inline",
              radioButtons(
                "nivel_academico",
                NULL,
                choices = c(
                  "Mestrando" = "Mestrando",
                  "Doutorando" = "Doutorando"
                ),
                selected = character(0),
                inline = TRUE
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          div(
            class = "form-group",
            tags$label(HTML(
              "Ano do curso <span class='required-mark'>*</span>"
            )),
            div(
              id = "ano_curso_buttons",
              class = "year-buttons",
              lapply(1:4, function(i) {
                tags$div(
                  class = "year-btn",
                  `data-value` = i,
                  onclick = sprintf("selectYear(%d)", i),
                  paste("Ano", i)
                )
              })
            ),
            tags$input(
              type = "hidden",
              id = "ano_curso",
              name = "ano_curso"
            )
          )
        )
      ),
      tags$small(
        class = "text-muted",
        style = "display: block; margin-top: 15px;",
        icon("info-circle"),
        " Campos marcados com * são obrigatórios"
      )
    ),

    div(
      class = "instruction-box",
      h4(icon("lightbulb"), " Como funciona?"),
      p(
        icon("hand-pointer"),
        strong(" Clique e arraste"),
        " cada opção para reordená-la na lista abaixo."
      ),
      p(
        icon("arrow-up"),
        strong(" Posição superior"),
        " = maior preferência | ",
        icon("arrow-down"),
        strong(" Posição inferior"),
        " = menor preferência"
      )
    ),

    fluidRow(
      h2("Explicação"),

      div(
        style = "font-size: 16px; line-height: 1.5;",

        p(
          "A seguir, você verá uma lista com diferentes combinações relacionadas ao processo de orientação na pós-graduação.
     Sua tarefa é ordenar essas combinações da melhor para a pior, conforme suas preferências."
        ),

        tags$ul(
          tags$li(
            strong("Frequência da orientação:"),
            " semanal, mensal ou sob demanda."
          ),
          # tags$li(strong("Formato das reuniões:"), " presencial ou híbrido."),
          tags$li(
            strong("Produção científica esperada ao fim do curso:"),
            " até 2 artigos, de 3 a 5 artigos ou mais de 5 artigos."
          )
        ),

        p(
          "Arraste os itens e construa seu ranking pessoal. Não existe resposta certa ou errada — queremos apenas entender suas preferências."
        )
      ),

      column(
        width = 8,
        div(
          class = "section-title",
          icon("list-ol"),
          "Ordene suas preferências:"
        ),
        rank_list(
          text = "",
          labels = labels_cards,
          input_id = "ranking_ordem",
          class = "rank-list-container"
        )
      ),
      column(
        width = 4,
        div(
          class = "section-title",
          icon("save"),
          "Finalizar"
        ),
        tags$button(
          id = "save",
          class = "btn-save action-button",
          icon("check-circle"),
          " Salvar Ranking"
        ),
        uiOutput("mensagem")
      )
    )
  ),

  tags$script(HTML(
    "
    function selectYear(year) {
      // Remove active class from all buttons
      document.querySelectorAll('.year-btn').forEach(btn => {
        btn.classList.remove('active');
      });
      
      // Add active class to clicked button
      event.target.classList.add('active');
      
      // Update hidden input
      document.getElementById('ano_curso').value = year;
      
      // Trigger Shiny input change
      Shiny.setInputValue('ano_curso', year);
    }
  "
  ))
)

# ---- SERVER ----
server <- function(input, output, session) {
  ranking_atual <- reactiveVal(NULL)

  salvar_google_sheets <- function(dados) {
    tryCatch(
      {
        # Tenta ler dados existentes (opcional - apenas para verificar conexão)
        existing_data <- tryCatch(
          {
            read_sheet(
              gs4_get(SHEET_ID),
              sheet = "Rankings"
            )
          },
          error = function(e) NULL
        )

        # Adiciona os novos dados
        sheet_append(
          ss = SHEET_ID,
          data = dados,
          sheet = "Rankings"
        )

        return(TRUE)
      },
      error = function(e) {
        message("Erro ao salvar no Google Sheets: ", e$message)
        return(FALSE)
      }
    )
  }

  observeEvent(input$save, {
    # Validações
    if (is.null(input$area_pesquisa) || trimws(input$area_pesquisa) == "") {
      output$mensagem <- renderUI({
        tags$div(
          class = "alert alert-danger",
          icon("exclamation-circle"),
          " Por favor, preencha sua área de pesquisa."
        )
      })
      return(NULL)
    }

    if (is.null(input$nivel_academico) || length(input$nivel_academico) == 0) {
      output$mensagem <- renderUI({
        tags$div(
          class = "alert alert-danger",
          icon("exclamation-circle"),
          " Por favor, selecione seu nível acadêmico."
        )
      })
      return(NULL)
    }

    if (is.null(input$ano_curso) || input$ano_curso == "") {
      output$mensagem <- renderUI({
        tags$div(
          class = "alert alert-danger",
          icon("exclamation-circle"),
          " Por favor, selecione o ano do seu curso."
        )
      })
      return(NULL)
    }

    # ---- 1) Captura e validação da ordem ----

    ordem_ids <- input$ranking_ordem

    validate(
      need(
        !is.null(ordem_ids) && length(ordem_ids) == nrow(stimuli),
        paste0("Por favor, ranqueie todos os ", nrow(stimuli), " itens.")
      )
    )

    # Extrai apenas números dos IDs (item3 → 3)
    ordem_nums <- readr::parse_number(ordem_ids)

    # ---- 2) Define o nome do participante ----

    nome_participante <- if (
      !is.null(input$nome_participante) &&
        trimws(input$nome_participante) != ""
    ) {
      input$nome_participante
    } else {
      paste0("Anônimo_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    }

    # ---- 3) Monta o resultado na ordem escolhida ----

    resultado <- stimuli %>%
      arrange(match(Estimulo, ordem_nums)) %>%
      mutate(
        Participante = nome_participante,
        Area_Pesquisa = input$area_pesquisa,
        Nivel_Academico = input$nivel_academico,
        Ano_Curso = as.integer(input$ano_curso),
        Posicao = row_number(),
        Posicao_Ordinal = numero_ordinal(row_number()),
        Data_Hora = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        .before = 1
      )

    # ---- 4) Salva para reactive + Google Sheets ----

    ranking_atual(resultado)

    sucesso_gsheets <- salvar_google_sheets(resultado)

    if (sucesso_gsheets) {
      output$mensagem <- renderUI({
        tags$div(
          class = "alert alert-success",
          icon("check-circle"),
          " Ranking salvo com sucesso! ",
          tags$br(),
          tags$strong(nrow(resultado), " opções ranqueadas"),
          tags$br(),
          tags$small(
            "Participante: ",
            nome_participante,
            " | ",
            "Área: ",
            input$area_pesquisa,
            " | ",
            "Ano: ",
            input$ano_curso
          )
        )
      })
    } else {
      output$mensagem <- renderUI({
        tags$div(
          class = "alert alert-danger",
          icon("times-circle"),
          " Erro ao salvar. Verifique as permissões do Google Sheets e tente novamente."
        )
      })
    }
  })
}

shinyApp(ui, server, options = list(port = 3838))
