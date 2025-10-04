# LOTE PARA ENVIAR

lote_para_enviar <- 6

# Send email with passwords
library(gmailr)
# Ler arquivo com as informações para enviar
googlesheets4::gs4_auth(email = "milz.bea@gmail.com")

caminho_drive <- "https://docs.google.com/spreadsheets/d/1uOPb8bx3pCoeU0MjisHLhAEcLVAjssCvngQf4sL234s/edit?usp=sharing"

df_info <- googlesheets4::read_sheet(caminho_drive) |>
  janitor::clean_names()

df_info_prep <- df_info |>
  dplyr::filter(dados_respondido != TRUE, lote == lote_para_enviar) |>
  dplyr::mutate(
    first_name = stringr::str_split(
      autor_correspondente,
      " ",
      simplify = TRUE
    )[, 1] |>
      stringr::str_to_title(),
    data_maxima = format(Sys.Date() + 14, "%d/%m/%Y")
  )


# Authenticate with gmail- ------

gm_auth_configure(
  path = "client_secret_734517618910-dtdbrsvc6n7g5ruosc8l6h0snhiv35kc.apps.googleusercontent.com.json"
)

gm_auth('milz.bea@gmail.com')

# Function to send email

send_email_gmail <- function(df_row) {
  browser()

  gm_message <- glue::glue(paste(
    readLines("template-email.html"),
    collapse = "\n"
  ))

  emails_enviar <- df_row$e_mails |>
    stringr::str_split(pattern = ",") |>
    purrr::pluck(1) |>
    stringr::str_trim()

  # Para testes
  # emails_enviar <- c("milz.bea@gmail.com", "beatriz.milz@icloud.com")

  # TODO Futuro: função de validar email?

  # Prepara a mensagem
  gm_rascunho <- gm_mime() |>
    gm_to(emails_enviar) |>
    gm_from("milz.bea@gmail.com") |>
    #gm_cc("revistaambienteesociedade@gmail.com") |>
    gm_subject("Envio da Declaração de Disponibilidade de Dados SciELO") |>
    gm_html_body(gm_message) |>
    gm_attach_file(
      filename = "formulario_declaracao_disponibilidade_dados_PT.docx"
    )

  draft <- gm_create_draft(gm_rascunho)

  gm_send_draft(draft)

  usethis::ui_done("Email enviado para: {df_person$name}")
}


# Testing if works:
df_person <- df_info_prep[1, ]
send_email_gmail(df_person)

# Send email to all persons in the list
list_password <- df_password |>
  tibble::rowid_to_column() |>
  dplyr::group_split(rowid)

purrr::map(list_password, send_email_gmail)
