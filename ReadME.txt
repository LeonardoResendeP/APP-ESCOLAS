📄 README — Pipeline Censo 2022 → Projeção 2025 → Dashboard Analítico
1. Objetivo
Construir um pipeline unificado para:

Baixar, limpar e integrar dados do Censo Demográfico 2022 por setor censitário (IBGE FTP).

Calcular indicadores demográficos e socioeconômicos relevantes para análise de mercado escolar.

Projetar dados de 2022 para 2025, utilizando fatores estaduais (projeções populacionais IBGE) e avanço de coortes etárias.

Preparar arquivos georreferenciados para integração no dashboard analítico.

2. Fontes de Dados Utilizadas
IBGE — Censo 2022 (setor censitário, arquivos CSV + shapefile)Diagrama contexto dados…

Alfabetização

Demografia

Básico (estrutura e domicílios)

IBGE — Projeções Populacionais (2024) por UF, idade simples (2022 e 2025)

Shapefile da Malha Setor Censitário 2022 (SIRGAS 2000)

Outras previstas no dashboard: Censo Escolar (INEP), ENEM, RAIS, OpenStreetMap_Diagrama Geral do Proj…

3. Estrutura Geral do Processo
3.1. Download e Pré-processamento
Download via FTP IBGE
Arquivos .zip de alfabetização, demografia, básico e dicionário.

Extração e leitura robusta (read_agregado_safe)

Normalização de cabeçalhos.

Detecção automática da coluna cd_setor (por nome e conteúdo).

Conversão segura de colunas Vxxxxx para numérico.

Sanitização e checagens

Garantia de chaves consistentes.

Verificação de ausência de NA/zeros inesperados.

3.2. Construção das Features 2022
Seleção das variáveis-chave:

pop_0_4 (v01009)

pop_5_9 (v01010)

pop_10_14 (v01011)

pop_15_17 (v01012)

alf_15mais (detectada automaticamente por correlação com total 15+)

dpp_total (v0001)

Cálculo de tam_total_0_17 e extração de cod_municipio_ibge.

3.3. Projeção 2025 (Nowcast)
Leitura da planilha projecoes_2024_tab1_idade_simples.xlsx (a partir da linha 6).

Conversão de idades simples em grupos etários (0-4, 5-9, 10-14, 15-17).

Cálculo de fatores UF 2022→2025 para cada grupo etário.

Aplicação de avanço de coortes (advance_cohorts_3y) e ajuste por fatores.

Geração de setor_2025 com as mesmas métricas de 2022, porém projetadas.

3.4. Integração Geoespacial
Leitura e normalização do shapefile dos setores (BR_Malha_Preliminar_2022.shp).

Join espacial (left_join) com features_min e setor_2025.

Exportação em formatos:

.gpkg para uso direto em GIS/dashboards.

.rds e .csv para análises no R/Python.

4. Saídas do Pipeline
bash
Copy
Edit
data/processed/
├── censo2022_setor_features_min.csv   # Indicadores 2022 por setor
├── censo2022_setor_features_min.rds
├── censo2022_universo_setor.gpkg       # Setor + geometria (GIS)
├── censo2022_universo_setor.rds
├── censo2022_universo_municipio.rds    # Agregado por município
└── setor_2025.rds                      # Projeção 2025 por setor
5. Aplicações no Dashboard
Esses dados alimentam módulos do Dashboard Analítico para Escolas Privadas_Diagrama Geral do Proj…:

Benchmark Regional: comparação de mercado com concorrentes próximos.

Perfil Socioeconômico Local: densidade de população escolar por faixa etária.

Análise de Mercado: oportunidades de expansão em áreas com alta demanda potencial.

Filtros Geográficos Dinâmicos: seleção por município, bairro ou setor.

6. Próximos Passos
Integrar Censo Escolar (matrículas, infraestrutura) ao nível de escola.

Adicionar camadas de mobilidade urbana (tempo de deslocamento).

Construir API para servir dados agregados no dashboard.

Criar visualizações georreferenciadas interativas com filtros dinâmicos.