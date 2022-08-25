# Aplicativo Shiny para o IDCREAS - 2019 

## Pacotes
library(shiny)
library(tidyverse)
library(bs4Dash)
library(plotly)
library(DT)

# Base de dados
base_idcreas <- readr::read_csv2("IDCREAS.csv") %>%  
  filter(!is.na(IDCREAS)) %>% 
  pivot_longer(cols = notaEF:IDCREAS, names_to = "componentes", values_to = "valores" ) %>% 
  mutate(componentes=case_when(componentes=="notaEF" ~ "Estrutura física",
                               componentes=="notaRH" ~ "Recursos humanos",
                               componentes=="notaServ" ~ "Serviços",
                               TRUE ~ "IDCREAS"))

# Opções dos filtros

ufs <- base_idcreas %>% pull(UF) %>% unique() %>% sort()
ufs <- c("Todas", ufs)
componentes <- base_idcreas %>% pull(componentes) %>% unique()
portes <- base_idcreas %>% pull(Porte_pop2010) %>% unique() %>% sort()
portes <- c("Todos",portes)

shinyApp(
  ui = dashboardPage(
    title = "IDCREAS - 2019",
    fullscreen = TRUE,
    header = dashboardHeader(
      title = dashboardBrand(
        title = "IDCREAS",
        color = "primary",
        href = "https://setades.es.gov.br/",
        image = "https://static.pciconcursos.com.br/i/9038a23bdd3d2d2f2242c7276499f5ab.jpg",
      ),
      skin = "light",
      status = "white",
      border = TRUE,
      sidebarIcon = icon("bars"),
      controlbarIcon = icon("th"),
      fixed = FALSE
    ),
    sidebar = dashboardSidebar(
      skin = "light",
      status = "primary",
      elevation = 3,
      sidebarUserPanel(image = "https://image.flaticon.com/icons/svg/1149/1149168.svg",
                       name = "SETADES"),
      sidebarMenu(sidebarHeader("Seções"),
                  menuItem("Sobre",
                           tabName = "sobre",
                           icon = icon("question-circle")),
                  menuItem("Resultados gerais",
                           tabName = "geral",
                           icon = icon("chart-bar")),
                  menuItem("Ranking UF",
                           tabName = "rankinguf",
                           icon = icon("sort-amount-up-alt")),
                  menuItem("Ranking Municípios",
                           tabName = "rankingmunic",
                           icon = icon("sort-amount-down-alt")),
                  menuItem("Quadro de Notas",
                           tabName = "quadronotas",
                           icon = icon("clipboard-check"))
      )
    ),
    controlbar = dashboardControlbar(
      skin = "light",
      pinned = TRUE,
      collapsed = FALSE,
      overlay = FALSE,
      controlbarMenu(
        id = "controlbarmenu",
        controlbarItem(
          title = "Filtros",
          radioButtons(inputId = "comp",
                       label = "Componente do índice",
                       choices = componentes,
                       selected = componentes[4]),
          column(
            width = 12,
            align = "center",
            selectInput(inputId = "ufs",
                        label = "Unidade da Federação",
                        choices = ufs,
                        selected = ufs[1]),
            selectInput(inputId = "porte",
                        label = "Porte do município",
                        choices = portes,
                        selected = portes[1])
          )
        )
      )
    ),
    footer = dashboardFooter(
      left = a(
        href = "https://github.com/victorijsn",
        target = "_blank", "@victorijsn"
      ),
      right = "2021"
    ),
    ###### Corpo  ######
    body = dashboardBody(
      tabItems(
        ##### Sobre o índice #####
        tabItem(tabName = "sobre",
                h3("Sobre o painel e sobre o indicador"),
                fluidRow(
                  box(
                    title = "Contexto e objetivo do painel",
                    width = 12,
                    p("A proposta de indicador de desenvolvimento do 
                       CREAS (IDCREAS) atende a necessidade daqueles que atuam 
                       no campo da Proteção Social Especial. 
                       O indicador oferece aos técnicos, gestores e conselheiros 
                       da assistência social uma ferramenta simples e fácil 
                       de compreender, e que permite a todos identificar a direção 
                       sugerida para o processo de aprimoramento da qualidade dos CREAS, 
                       bem como a situação de cada unidade, e de cada município, 
                       nesta trajetória de contínua busca da melhoria dos serviços
                       socioassistenciais."),
                    p("Os indicador busca capturar, de forma aproximada e 
                       comparativa, a qualidade dos serviços prestados à 
                         população por meio do CREAS."),
                    p("Nesse sentido, o painel busca apresentar as informações
                      de maneira interativa para uma análise ampla dos resultados 
                      obtidos para o ano de 2019, comparando o desempenho com as demais 
                      Unidades da Federação e Municípios. Ao final desta página, você 
                      também poderá fazer o download da base de dados completa que 
                      gerou este painel.")),
                  box(
                    title = "O que é o CREAS?",
                    width = 12,
                    p("O Centro de Referência Especializado de 
                      Assistência Social (CREAS) é uma unidade pública da 
                      Assistência Social que atende pessoas que vivenciam 
                      situações de violações de direitos ou de 
                violências. Uma pessoa será atendida no CREAS, entre outras 
                situações, por sofrer algum tipo de assédio, de discriminação, 
                de abuso, de violência ou por demandar cuidados em razão da 
                idade ou deficiência.")),
                  box(
                    title = "Metodologia resumida",
                    width = 12,
                    tags$p(
                      "A metodologia completa, desenvolvida pelo Ministério da Cidadania, ", 
                      "pode ser encontrada no seguinte link ",
                      tags$a(href = "https://bityli.com/8ECBRS", "Nota técnica Nº27 /2015/ DGSUAS/SNAS/MDS", target = "_blank"),
                      ". O script utilizado para para gerar o indicador para o ",
                      "ano de 2019 pode ser encontrado no seguinte repositório:",
                      tags$a(href = "https://github.com/victorijsn/IDCREAS2019", "IDCREAS_validado", target = "_blank"),
                      "."
                    ),
                    h6("Estrutura física"),
                    p("Esta dimensão pretende mensurar as condições de infraestrutura 
                das unidades CREAS, a partir do número de salas para atendimento, 
                número de banheiros, condições de acessibilidade, entre outros. 
                Nesta dimensão, em seu nível 5, é também considerado um conjunto 
                de equipamentos (telefone, impressora, computadores com acesso à 
                internet, veículo próprio ou compartilhado) tido como importantes 
                para o desenvolvimento de serviços com qualidade."),
                    h6("Recursos Humanos"),
                    p("A dimensão de Recursos Humanos objetiva aferir sobre o 
                dimensionamento das equipes de referência, tendo em conta, 
                o porte do município e o tipo de CREAS (municipal ou regional). 
                Conforme estabelecido na NOB-RH, as unidades devem possuir um 
                quantitativo mínimo de trabalhadores, parte dos quais de nível 
                superior, nomeadamente aqueles com formações acadêmicas em 
                Serviço Social, Psicologia e Direito. No nível 5, o tipo de 
                vínculo é também considerado. Os CREAS devem ter em suas equipes, 
                no mínimo, 1 trabalhador de nível superior (no caso de unidades 
                de até porte médio) ou 2 (no caso de unidades de porte grande, 
                metrópoles ou CREAS regional) com vínculo estatutário ou 
                empregado público celetista."),
                    h6("Serviços"),
                    p("A dimensão Serviços avalia a oferta de serviços socioassistenciais 
                nas unidades CREAS, nomeadamente as atividades desenvolvidas no 
                âmbito do PAEFI, serviço de acompanhamento de Medidas socioeducativas 
                (MSE), se oferta diretamente ou referencia o serviço de abordagem 
                social, e se mantem articulação com outros equipamentos que compõem 
                a rede de proteção social, tais como CRAS, unidades de Acolhimento 
                e Conselhos Tutelares. Esta dimensão relaciona também dados referentes 
                ao volume de acompanhamentos do PAEFI com o número de profissionais 
                (assistentes sociais e psicólogos) da unidade.")),
                  "Faça o download da base de dados completa:",
                  downloadButton("downloadData", "Download"))
        ),
        tabItem(tabName = "geral",
                box(title = "Intruções gerais",
                    width = 12,
                    p("Utilize o painel do lado direito da tela para selecionar os",
                      "componentes, Unidades da Federação e Portes dos Municípios",
                      "para fazer sua análise."),
                    p("Todas as seleções aparecerão no texto abaixo e alterarão os gráficos:"),
                    p(textOutput(outputId = "compselecionado")),
                    p(textOutput(outputId = "ufselecionada")),
                    p(textOutput(outputId = "porteselecionado")),
                    p("Você poderá comparar as notas de cada componente do indicador entre as Unidades da Federação e os portes dos municípios e a média geral do indicador no Brasil.")),
                fluidRow(infoBoxOutput(outputId = "total_creas_filtro", width = 6),
                         infoBoxOutput(outputId = "nota_media_filtro", width = 6)),
                box(title="Gráfico 1 - Frequência absoluta de CREAS segundo as notas do índice",
                    width = 12,
                    plotlyOutput(outputId = "grafico1")),
                box(title = "Gráfico 2 - Frequência relativa (%) de CREAS segundo as notas do índice",
                    width = 12,
                    plotlyOutput(outputId = "grafico2"))),
        
        tabItem(tabName = "rankinguf",
                fluidRow(column(width = 12, h3(textOutput(outputId = "titulorankinguf")))),
                box(title = "Tabela com os valores",
                    width = 12,
                    DT::dataTableOutput("rankingUF")),
                box(title = "Gráfico com o ordenamento",
                    width = 12,
                    plotOutput(outputId = "grafico3",height = 600))
        ),
        
        tabItem(tabName = "rankingmunic",
                fluidRow(column(width = 12, h3(textOutput(outputId = "titulorankingmunic")))),
                box(title = "Tabela com os valores",
                    width = 12,
                    DT::dataTableOutput("rankingMunic"))),
        tabItem(tabName = "quadronotas",
                box(title= "Quadro geral de notas das Unidades da Federação",
                    width = 12,
                    tableOutput(outputId = "quadronotas")),
                p("Os filtros não afetam esta tabela.")
        )
      )
    )
  ),
  
  ###### Servidor #####
  server = function(input, output) {
    
    output$compselecionado <- renderText({glue::glue("- Componente selecionado: {input$comp}.")})
    
    output$ufselecionada <- renderText({glue::glue("- UF selecionada: {input$ufs}.")})
    
    output$porteselecionado <- renderText({glue::glue("- Porte de município selecionado: {input$porte}.")})
    
    output$total_creas_filtro <- renderInfoBox({
      total_creas_filtro <- base_idcreas
      if (input$ufs!="Todas") {
        total_creas_filtro <- total_creas_filtro %>% filter(UF==input$ufs)
      }
      if (input$porte!="Todos") {
        total_creas_filtro <- total_creas_filtro %>% filter(Porte_pop2010==input$porte)
      }
      total_creas_filtro <- total_creas_filtro %>%
        pull(NU_IDENTIFICADOR) %>% n_distinct() %>% 
        scales::number(big.mark = ".", decimal.mark = ",")
      infoBox(
        title = paste0("Total de CREAS - ",input$ufs),
        value = total_creas_filtro,
        color = "gray",
        icon = icon("home"),
        fill = TRUE
      )
    })
    
    output$nota_media_filtro <- renderInfoBox({
      nota_media_filtro <- base_idcreas 
      if (input$ufs!="Todas") {
        nota_media_filtro <- nota_media_filtro %>% filter(UF == input$ufs)
      }
      if (input$porte!="Todos") {
        nota_media_filtro <- nota_media_filtro %>% filter(Porte_pop2010== input$porte)
      }
      nota_media_filtro <- nota_media_filtro %>% 
        filter(componentes==input$comp) %>%
        pull(valores) %>% mean() %>% round(2) %>% 
        scales::number(big.mark = ".", decimal.mark = ",", accuracy = 0.01)
      infoBox(
        title = paste0("Média ",input$comp," - ", input$ufs),
        value = nota_media_filtro,
        color = "orange",
        icon = icon("pen"),
        fill = TRUE
      )
    })
    
    
    output$grafico1 <- renderPlotly({
      g1tab <- base_idcreas
      if (input$ufs!="Todas") {
        g1tab <- g1tab %>% filter(UF==input$ufs)
      }
      if (input$porte!="Todos") {
        g1tab <- g1tab %>% filter(Porte_pop2010==input$porte)
      }
      grafico1 <- g1tab %>% 
        filter(componentes == input$comp) %>% 
        group_by(valores) %>% 
        summarise(n=n_distinct(NU_IDENTIFICADOR)) %>% 
        ggplot(aes(x = as.factor(valores), y = n)) + 
        geom_col() +
        labs(
          x = "Notas",
          y = "Frequência absoluta"
        ) +
        ggthemes::theme_hc(base_size = 14)+
        ggthemes::scale_fill_tableau("Tableau 10")
      
      plotly::ggplotly(grafico1)
    })
    
    output$grafico2 <- renderPlotly({
      
      total_creas <- base_idcreas %>% pull(NU_IDENTIFICADOR) %>% n_distinct()
      
      total_creas_filtro <- base_idcreas 
      if (input$ufs!="Todas") {
        total_creas_filtro <- total_creas_filtro %>% filter(UF==input$ufs)
      }
      if (input$porte!="Todos") {
        total_creas_filtro <- total_creas_filtro %>% filter(Porte_pop2010==input$porte)
      }
      total_creas_filtro <- total_creas_filtro %>% pull(NU_IDENTIFICADOR) %>% n_distinct()
      
      part1 <- base_idcreas
      if (input$ufs!="Todas") {
        part1 <- part1 %>% filter(UF==input$ufs)
      }
      if (input$porte!="Todos") {
        part1 <- part1 %>% filter(Porte_pop2010==input$porte)
      }
      part1 <- part1 %>%
        filter(componentes==input$comp) %>% 
        group_by(valores) %>% 
        summarise(fr=round(n_distinct(NU_IDENTIFICADOR)/total_creas_filtro*100,2)) %>% 
        mutate(localidade=paste0("UF: ",input$ufs," - Porte: ",input$porte))
      
      part2 <- base_idcreas %>% 
        filter(componentes==input$comp) %>% 
        group_by(valores) %>% 
        summarise(fr=round(n_distinct(NU_IDENTIFICADOR)/total_creas*100,2)) %>% 
        mutate(localidade="Brasil - todos os portes")
      
      grafico2 <- bind_rows(part1,part2) %>%  
        ggplot(aes(x = as.factor(valores), y = fr, fill=localidade)) + 
        geom_col(position = "dodge") +
        labs(
          x = "Notas",
          y = "Frequência relativa (%)",
          fill = "Localidade"
        ) +
        ggthemes::theme_hc(base_size = 14)+
        ggthemes::scale_fill_tableau("Tableau 10")
      
      plotly::ggplotly(grafico2)
    })
    
    
    output$titulorankinguf <- renderText({glue::glue("Ranking de UFs do indicador: {input$comp}")})
    
    output$rankingUF <- DT::renderDataTable(DT::datatable({
      if (input$porte!="Todos") {
        tab1 <- base_idcreas %>% filter(Porte_pop2010==input$porte)
      } else {tab1 <- base_idcreas}
      tab1 <- tab1 %>%
        filter(componentes==input$comp) %>% 
        group_by(UF) %>% 
        summarise(valores_media = round(mean(valores, na.rm = T),2)) %>%
        arrange(-valores_media) %>% 
        mutate(valores_media=scales::number(valores_media, big.mark = ".", 
                                            decimal.mark = ",", 
                                            accuracy = 0.01))
      names(tab1) <- c("UFs", input$comp)
      tab1
    }))
    
    output$grafico3 <- renderPlot({
      if (input$porte!="Todos") {
        tabg3 <- base_idcreas %>% filter(Porte_pop2010==input$porte)
      } else {tabg3 <- base_idcreas}
      
      grafico3 <- tabg3 %>%
        filter(componentes==input$comp) %>%
        group_by(UF) %>% 
        summarise(valores_media = round(mean(valores, na.rm = T),2)) %>%
        arrange(-valores_media) %>% 
        ggplot(aes(x = reorder(UF,valores_media), y = valores_media))+
        geom_col()+
        scale_y_continuous(breaks = seq(from = 0,to = 5,by = 1), limits = c(0,5))+
        geom_text(aes(label = round(valores_media, 2)), hjust = -.5) +
        labs(x = "Unidades da Federação",
             y = "Notas")+
        coord_flip()+
        theme_minimal(base_size = 14)
      
      if (input$ufs!="Todas") {
        grafico3+gghighlight::gghighlight(UF==input$ufs)
      } else grafico3
      
    })
    
    output$titulorankingmunic <- renderText({glue::glue("Seleção para construção do 
                                                        ranking: indicador: {input$comp}, UF: {input$ufs}, Porte do município: {input$porte}")})
    output$rankingMunic <- DT::renderDataTable(DT::datatable({
      tab2 <- base_idcreas
      
      if (input$ufs!="Todas") {
        tab2 <- tab2 %>% filter(UF==input$ufs)
      }
      if (input$porte!="Todos") {
        tab2 <- tab2 %>% filter(Porte_pop2010==input$porte)
      }
      tab2 <- tab2 %>% filter(componentes==input$comp) %>% 
        group_by(Municipio, UF, Porte_pop2010) %>%
        summarise(valores_media = round(mean(valores, na.rm = T),2),
                  .groups = "keep") %>% 
        arrange(-valores_media) %>% 
        mutate(valores_media=scales::number(valores_media, big.mark = ".", 
                                            decimal.mark = ",", 
                                            accuracy = 0.01))
      names(tab2) <- c("Município", "UF", "Porte", input$comp)
      
      tab2
    }))
    
    output$quadronotas <- renderTable({
      total_creas <- base_idcreas %>% pull(NU_IDENTIFICADOR) %>% n_distinct()
      tab3_corpo <- base_idcreas %>%
        group_by(UF,componentes) %>%
        summarise(N = n(), 
                  Part_percent = round((n()/total_creas*100), 2),
                  valores_media = round(mean(valores, na.rm = T),2),
                  .groups = "keep") %>%
        pivot_wider(names_from = componentes, values_from = valores_media)
      
      tab3_total <- base_idcreas %>% 
        mutate(UF="Brasil") %>% 
        group_by(UF, componentes) %>% 
        summarise(N = n(), 
                  Part_percent = round((n()/total_creas*100), 2),
                  valores_media = round(mean(valores, na.rm = T),2),
                  .groups = "keep") %>%
        pivot_wider(names_from = componentes, values_from = valores_media) 
      
      tab3 <- bind_rows(tab3_corpo, tab3_total)
      rm(tab3_corpo, tab3_total)
      tab3 <- tab3 %>% 
        select(UF, IDCREAS,`Estrutura física`,`Recursos humanos`,
               `Serviços`,N,Part_percent) %>% 
        rename (Estado = UF, 
                'Participação % do nº de CREAS' = Part_percent, 
                'Número de CREAS'= N)
      tab3
    })
    
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("IDCREAS-2019-", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv2(base_idcreas, file)
      }
    )
    
    
  }
)
