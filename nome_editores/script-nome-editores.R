# Ler arquivo com as informações para enviar
googlesheets4::gs4_auth(email = "milz.bea@gmail.com")

caminho_drive <- "https://docs.google.com/spreadsheets/d/1uOPb8bx3pCoeU0MjisHLhAEcLVAjssCvngQf4sL234s/edit?usp=sharing"

df_info <- googlesheets4::read_sheet(caminho_drive) |>
  janitor::clean_names()

df_info_prep_aut <- df_info |>
  #  dplyr::filter(is.na(editor_associado) | is.na(eic_editor_chefe)) |>
  dplyr::mutate(id_base = stringr::str_extract(id, "ASOC-[0-9]{4}-[0-9]{4}"))

df_info_prep_aut$id_base

# ler manuscripts decided

manuscripts_decided <- readxl::read_xlsx(
  "nome_editores/Manuscripts Decided.xlsx",
  sheet = 3,
  skip = 12
) |>
  janitor::clean_names()

manuscripts_decided_authors <- manuscripts_decided |>
  dplyr::mutate(
    id_base = stringr::str_extract(manuscript_id, "ASOC-[0-9]{4}-[0-9]{4}")
  ) |>
  dplyr::filter(id_base %in% df_info_prep_aut$id_base) |>
  dplyr::arrange(id_base) |>
  dplyr::select(
    manuscript_id,
    id_base,
    contact_author,
    editor_full_name,
    eic_full_name
  ) |>
  dplyr::group_by(id_base, contact_author) |>
  dplyr::summarise(
    manuscript_id = paste0(manuscript_id, collapse = "; "),
    associate_editors = editor_full_name |> unique() |> paste0(collapse = "; "),
    eic = eic_full_name |> unique() |> paste0(collapse = "; "),
  )
