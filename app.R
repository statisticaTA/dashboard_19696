###############################################################################
# DASHBOARD SETTIMANALE PER SERVIZIO - V1
# Fondazione S.O.S. Il Telefono Azzurro ETS
#
# Un'unica codebase per due app separate:
#   - 114 Emergenza Infanzia
#   - 1.96.96
#
# La app legge esclusivamente gli snapshot verticali prodotti da
# genera_snapshot_servizi_v1.R. Non ricalcola KPI o analisi CRM.
#
# Configurazione servizio, in ordine di priorita':
#   1. variabile d'ambiente SERVIZIO_APP
#   2. opzione R telefono_azzurro.servizio_app
#   3. default: 114
#
# Esempi locali:
#   options(telefono_azzurro.servizio_app = "114")
#   shiny::runApp()
#
#   options(telefono_azzurro.servizio_app = "19696")
#   shiny::runApp()
#
# Struttura attesa:
#   APP/
#     app.R
#     snapshot_servizio/
#       114/
#         S21_20260921_20260927/
#           dashboard_114_S21_20260921_20260927.rds
#       19696/
#         S21_20260921_20260927/
#           dashboard_19696_S21_20260921_20260927.rds
###############################################################################

# =============================================================================
# 1. PACCHETTI E CONFIGURAZIONE
# =============================================================================

required_packages <- c(
  "shiny", "DT", "dplyr", "ggplot2", "scales"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Pacchetti mancanti: ", paste(missing_packages, collapse = ", "),
    ". Installarli con install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    ")).",
    call. = FALSE
  )
}

library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(scales)

options(
  scipen = 999,
  stringsAsFactors = FALSE,
  dplyr.summarise.inform = FALSE
)

SERVIZIO_APP <- Sys.getenv("SERVIZIO_APP", unset = "")
if (!nzchar(SERVIZIO_APP)) {
  SERVIZIO_APP <- getOption("telefono_azzurro.servizio_app", "114")
}
SERVIZIO_APP <- as.character(SERVIZIO_APP)[1L]

if (!SERVIZIO_APP %in% c("114", "19696")) {
  stop(
    "SERVIZIO_APP deve essere '114' oppure '19696'. Valore trovato: ",
    SERVIZIO_APP,
    call. = FALSE
  )
}

SERVIZIO_LABEL <- if (SERVIZIO_APP == "114") {
  "114 Emergenza Infanzia"
} else {
  "1.96.96"
}

SERVIZIO_LABEL_BREVE <- if (SERVIZIO_APP == "114") "114" else "1.96.96"
SERVIZIO_KPI_CODES <- if (SERVIZIO_APP == "114") c("114", "114_ML") else "196"

SNAPSHOT_ROOT <- Sys.getenv(
  "APP_SNAPSHOT_DIR",
  unset = file.path(getwd(), "snapshot_servizio")
)
SNAPSHOT_DIR <- file.path(SNAPSHOT_ROOT, SERVIZIO_APP)
SCHEMA_ATTESO <- "telefono_azzurro_servizio_dashboard_v1"
VERSIONE_SCHEMA_ATTESA <- 1L

SOGLIA_GIALLA <- 10
SOGLIA_ROSSA <- 25
COLORE_GIALLO <- "#FFF3CD"
COLORE_ROSSO <- "#F8D7DA"
COLORE_TESTO_ROSSO <- "#842029"

TA_BLUE_DARK <- "#003B5C"
TA_BLUE <- "#0072BC"
TA_SKY <- "#00A3E0"
TA_GREEN <- "#2E8B57"
TA_ORANGE <- "#E67E22"
TA_RED <- "#B03A2E"
TA_GREY <- "#667085"
TA_LIGHT <- "#F5F7FA"
TA_BORDER <- "#D8E2EA"
ACCENT <- if (SERVIZIO_APP == "114") TA_BLUE else TA_GREEN
ACCENT_LIGHT <- if (SERVIZIO_APP == "114") "#EAF6FC" else "#EAF6EF"

# =============================================================================
# 2. FUNZIONI DI SUPPORTO GENERALI
# =============================================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

safe_num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- as.character(x)
  x <- gsub("%", "", x, fixed = TRUE)
  has_comma <- grepl(",", x, fixed = TRUE)
  x[has_comma] <- gsub(".", "", x[has_comma], fixed = TRUE)
  x[has_comma] <- gsub(",", ".", x[has_comma], fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

fmt_int <- function(x) {
  x <- safe_num(x)
  if (!length(x) || is.na(x[1L])) return("–")
  format(
    round(x[1L]),
    big.mark = ".",
    decimal.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

fmt_num <- function(x, digits = 2L) {
  x <- safe_num(x)
  if (!length(x) || is.na(x[1L])) return("–")
  format(
    round(x[1L], digits),
    big.mark = ".",
    decimal.mark = ",",
    nsmall = digits,
    scientific = FALSE,
    trim = TRUE
  )
}

fmt_hours <- function(x) {
  x <- safe_num(x)
  if (!length(x) || is.na(x[1L])) return("–")
  fmt_num(x[1L], 2L)
}

fmt_pct_ratio <- function(x, digits = 1L) {
  x <- safe_num(x)
  if (!length(x) || is.na(x[1L])) return("–")
  paste0(fmt_num(100 * x[1L], digits), "%")
}

fmt_pct_100 <- function(x, digits = 1L) {
  x <- safe_num(x)
  if (!length(x) || is.na(x[1L])) return("–")
  paste0(fmt_num(x[1L], digits), "%")
}

fmt_duration <- function(seconds) {
  seconds <- safe_num(seconds)
  if (!length(seconds) || is.na(seconds[1L]) || seconds[1L] < 0) return("–")
  s <- round(seconds[1L])
  h <- s %/% 3600
  m <- (s %% 3600) %/% 60
  sec <- s %% 60
  sprintf("%02d:%02d:%02d", h, m, sec)
}

mesi_it <- c(
  "gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno",
  "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre"
)

fmt_date_it <- function(x, include_year = TRUE) {
  x <- as.Date(x)
  if (!length(x) || is.na(x[1L])) return("–")
  out <- paste(
    as.integer(format(x[1L], "%d")),
    mesi_it[as.integer(format(x[1L], "%m"))]
  )
  if (isTRUE(include_year)) out <- paste(out, format(x[1L], "%Y"))
  out
}

fmt_week_it <- function(start, end) {
  start <- as.Date(start)
  end <- as.Date(end)
  if (is.na(start) || is.na(end)) return("Settimana non disponibile")

  same_month <- format(start, "%Y-%m") == format(end, "%Y-%m")
  same_year <- format(start, "%Y") == format(end, "%Y")

  if (same_month) {
    paste0(
      as.integer(format(start, "%d")), "–", as.integer(format(end, "%d")), " ",
      mesi_it[as.integer(format(end, "%m"))], " ", format(end, "%Y")
    )
  } else if (same_year) {
    paste0(
      as.integer(format(start, "%d")), " ", mesi_it[as.integer(format(start, "%m"))],
      " – ", as.integer(format(end, "%d")), " ", mesi_it[as.integer(format(end, "%m"))],
      " ", format(end, "%Y")
    )
  } else {
    paste0(fmt_date_it(start), " – ", fmt_date_it(end))
  }
}

service_label <- function(x) {
  dplyr::recode(
    as.character(x),
    "114" = "114",
    "114_ML" = "114 Multilingua",
    "196" = "1.96.96",
    "19696" = "1.96.96",
    .default = as.character(x)
  )
}

operator_key <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[is.na(x)] <- ""
  x
}

clean_operator_label <- function(x) {
  x <- as.character(x)
  x[x == "[SCHEDA_CASO_ASSENTE]"] <- "Scheda Caso assente"
  x[x == "[OPERATORE_NON_DISPONIBILE]"] <- "Operatore non disponibile"
  x[x == "[ATTRIBUZIONE_DA_VERIFICARE]"] <- "Attribuzione da verificare"
  x[x == "[TOTALE_SERVIZIO]"] <- "Totale servizio"
  x
}

pretty_names <- function(x) {
  x <- gsub("_", " ", x, fixed = TRUE)
  x <- gsub("pct", "%", x, fixed = TRUE)
  x <- gsub("^n ", "N. ", x)
  # I suffissi tecnici del formato durata non sono utili nell'interfaccia.
  x <- sub("[[:space:]]+hhmmss$", "", x, ignore.case = TRUE)
  x <- gsub("  +", " ", x)
  x <- trimws(x)
  ifelse(
    nzchar(x),
    paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))),
    x
  )
}

# Rimuove soltanto le colonne testuali di appoggio che duplicano una misura
# numerica gia' presente (es. pct + pct_label). La colonna numerica resta
# disponibile e viene formattata dalla dashboard.
drop_redundant_label_columns <- function(df) {
  if (is.null(df) || !ncol(df)) return(df)
  nm <- names(df)
  label_cols <- nm[grepl("_label$", nm, ignore.case = TRUE)]
  to_drop <- character()

  for (lab in label_cols) {
    base <- sub("_label$", "", lab, ignore.case = TRUE)
    if (base %in% nm) to_drop <- c(to_drop, lab)
  }

  if (length(to_drop)) {
    df <- df[, setdiff(names(df), unique(to_drop)), drop = FALSE]
  }
  df
}

format_display_df <- function(df) {
  if (is.null(df)) return(data.frame())
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!ncol(df)) return(df)

  # Le colonne *_label sono versioni gia' formattate delle percentuali numeriche
  # e produrrebbero due colonne identiche nell'interfaccia.
  df <- drop_redundant_label_columns(df)
  original_names <- names(df)

  for (nm in original_names) {
    x <- df[[nm]]

    if (inherits(x, "Date")) {
      df[[nm]] <- ifelse(is.na(x), "", format(x, "%d/%m/%Y"))
      next
    }

    if (inherits(x, "POSIXt")) {
      df[[nm]] <- ifelse(is.na(x), "", format(x, "%d/%m/%Y %H:%M"))
      next
    }

    if (is.logical(x)) {
      df[[nm]] <- ifelse(is.na(x), "", ifelse(x, "Sì", "No"))
      next
    }

    if (is.numeric(x)) {
      is_ratio <- grepl(
        "(^pct$|^pct_|_pct$|quota|livello_servizio|tasso_|rapporto_log_su_kpi)",
        nm,
        ignore.case = TRUE
      )
      is_percent_100 <- grepl("Percentuale_", nm, fixed = TRUE)
      is_integerish <- all(is.na(x) | abs(x - round(x)) < 1e-9)

      if (is_percent_100) {
        df[[nm]] <- ifelse(is.na(x), "", vapply(x, fmt_pct_100, character(1)))
      } else if (is_ratio) {
        df[[nm]] <- ifelse(is.na(x), "", vapply(x, fmt_pct_ratio, character(1)))
      } else if (is_integerish) {
        df[[nm]] <- ifelse(is.na(x), "", vapply(x, fmt_int, character(1)))
      } else {
        df[[nm]] <- ifelse(is.na(x), "", vapply(x, fmt_num, character(1), digits = 2L))
      }
    }
  }

  names(df) <- pretty_names(original_names)
  df
}

# =============================================================================
# 3. LETTURA E VALIDAZIONE SNAPSHOT
# =============================================================================

parse_snapshot_filename <- function(path) {
  f <- basename(path)
  rx <- paste0(
    "^dashboard_", SERVIZIO_APP,
    "_(S[0-9]{2})_([0-9]{8})_([0-9]{8})(?:\\([0-9]+\\))?\\.rds$"
  )
  m <- regexec(rx, f, perl = TRUE)
  z <- regmatches(f, m)[[1L]]
  if (!length(z)) return(NULL)

  data_inizio <- as.Date(z[3L], format = "%Y%m%d")
  data_fine <- as.Date(z[4L], format = "%Y%m%d")

  data.frame(
    settimana_id = z[2L],
    data_inizio = data_inizio,
    data_fine = data_fine,
    snapshot_id = paste0(z[2L], "_", z[3L], "_", z[4L]),
    settimana_label = paste0(z[2L], " | ", fmt_week_it(data_inizio, data_fine)),
    path = normalizePath(path, winslash = "/", mustWork = FALSE),
    stringsAsFactors = FALSE
  )
}

scan_snapshots <- function(root = SNAPSHOT_DIR) {
  empty <- data.frame(
    settimana_id = character(),
    data_inizio = as.Date(character()),
    data_fine = as.Date(character()),
    snapshot_id = character(),
    settimana_label = character(),
    path = character(),
    stringsAsFactors = FALSE
  )

  if (!dir.exists(root)) return(empty)

  files <- list.files(root, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
  parsed <- lapply(files, parse_snapshot_filename)
  parsed <- parsed[!vapply(parsed, is.null, logical(1))]
  if (!length(parsed)) return(empty)

  out <- do.call(rbind, parsed)
  out <- out[order(out$data_fine, decreasing = TRUE), , drop = FALSE]

  dup_key <- paste(out$settimana_id, out$data_inizio, out$data_fine)
  if (anyDuplicated(dup_key)) {
    mt <- file.info(out$path)$mtime
    ord <- order(out$data_fine, mt, decreasing = TRUE)
    out <- out[ord, , drop = FALSE]
    dup_key <- paste(out$settimana_id, out$data_inizio, out$data_fine)
    out <- out[!duplicated(dup_key), , drop = FALSE]
  }

  rownames(out) <- NULL
  out
}

validate_snapshot <- function(x) {
  if (!is.list(x)) {
    stop("Lo snapshot RDS non contiene una lista valida.", call. = FALSE)
  }

  if (!identical(x$schema_snapshot, SCHEMA_ATTESO)) {
    stop(
      "Schema snapshot non riconosciuto. Atteso: ", SCHEMA_ATTESO,
      "; trovato: ", x$schema_snapshot %||% "<assente>", ".",
      call. = FALSE
    )
  }

  if (!identical(as.integer(x$versione_schema %||% NA_integer_), VERSIONE_SCHEMA_ATTESA)) {
    stop(
      "Versione dello schema snapshot non riconosciuta. Attesa: ", VERSIONE_SCHEMA_ATTESA,
      "; trovata: ", x$versione_schema %||% "<assente>", ".",
      call. = FALSE
    )
  }

  required_top <- c("metadata", "metodo", "kpi", "crm", "operatori", "qualita")
  missing_top <- setdiff(required_top, names(x))
  if (length(missing_top)) {
    stop(
      "Oggetti mancanti nello snapshot: ", paste(missing_top, collapse = ", "), ".",
      call. = FALSE
    )
  }

  metadata_required <- c(
    "servizio", "settimana_id", "settimana_iso", "data_inizio", "data_fine",
    "regola_settimana", "creato_il"
  )
  metadata_missing <- setdiff(metadata_required, names(x$metadata))
  if (length(metadata_missing)) {
    stop(
      "Metadati mancanti nello snapshot: ", paste(metadata_missing, collapse = ", "), ".",
      call. = FALSE
    )
  }

  if (!identical(as.character(x$metadata$servizio), SERVIZIO_APP)) {
    stop(
      "Snapshot del servizio sbagliato. App configurata per ", SERVIZIO_APP,
      "; snapshot: ", x$metadata$servizio %||% "<assente>", ".",
      call. = FALSE
    )
  }

  data_inizio_md <- suppressWarnings(as.Date(x$metadata$data_inizio))
  data_fine_md <- suppressWarnings(as.Date(x$metadata$data_fine))
  if (is.na(data_inizio_md) || is.na(data_fine_md) ||
      as.integer(data_fine_md - data_inizio_md) != 6L ||
      as.POSIXlt(data_inizio_md)$wday != 1L ||
      as.POSIXlt(data_fine_md)$wday != 0L) {
    stop(
      "Periodo snapshot non valido: ogni settimana deve andare da lunedi' a domenica (7 giorni).",
      call. = FALSE
    )
  }

  if (!identical(as.character(x$metadata$regola_settimana), "lunedi-domenica")) {
    stop(
      "Regola calendario snapshot non valida: ",
      as.character(x$metadata$regola_settimana),
      call. = FALSE
    )
  }

  if (is.null(x$kpi$indicatori) || is.null(x$crm) || is.null(x$operatori$attivita) ||
      is.null(x$operatori$crm) || is.null(x$qualita$completezza)) {
    stop("Snapshot incompleto per la dashboard di servizio.", call. = FALSE)
  }

  service_values <- unique(unlist(lapply(
    x$kpi$indicatori,
    function(tab) {
      if (is.data.frame(tab) && "servizio" %in% names(tab)) {
        as.character(tab$servizio)
      } else {
        character()
      }
    }
  ), use.names = FALSE))
  service_values <- service_values[!is.na(service_values)]

  if (length(setdiff(service_values, SERVIZIO_KPI_CODES)) > 0L) {
    stop(
      "Lo snapshot contiene servizi KPI fuori perimetro: ",
      paste(setdiff(service_values, SERVIZIO_KPI_CODES), collapse = ", "),
      call. = FALSE
    )
  }

  x
}

# =============================================================================
# 4. FUNZIONI KPI
# =============================================================================

aggregate_kpi <- function(df, group_cols = character()) {
  if (is.null(df) || !nrow(df)) return(data.frame())

  required <- c("conversazioni_totali", "gestite", "abbandonate", "non_gestite")
  if (!all(required %in% names(df))) return(data.frame())

  summarise_body <- function(z) {
    z %>%
      summarise(
        conversazioni_totali = sum(conversazioni_totali, na.rm = TRUE),
        gestite = sum(gestite, na.rm = TRUE),
        abbandonate = sum(abbandonate, na.rm = TRUE),
        non_gestite = sum(non_gestite, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        livello_servizio = if_else(
          conversazioni_totali > 0,
          gestite / conversazioni_totali,
          NA_real_
        ),
        tasso_abbandono = if_else(
          conversazioni_totali > 0,
          abbandonate / conversazioni_totali,
          NA_real_
        ),
        tasso_non_gestite = if_else(
          conversazioni_totali > 0,
          non_gestite / conversazioni_totali,
          NA_real_
        )
      )
  }

  if (!length(group_cols)) {
    summarise_body(df)
  } else {
    summarise_body(df %>% group_by(across(all_of(group_cols))))
  }
}

kpi_detail_codes <- function(choice = "group") {
  if (SERVIZIO_APP == "19696") return("196")
  switch(
    choice %||% "group",
    "group" = c("114", "114_ML"),
    "114" = "114",
    "114_ml" = "114_ML",
    c("114", "114_ML")
  )
}

filter_kpi_rows <- function(df, detail_choice = "group", channel_choice = "all") {
  if (is.null(df) || !nrow(df)) return(data.frame())
  out <- df

  if ("servizio" %in% names(out)) {
    out <- out %>% filter(servizio %in% kpi_detail_codes(detail_choice))
  }

  if (!identical(channel_choice, "all") && "canale" %in% names(out)) {
    out <- out %>% filter(canale == channel_choice)
  }

  out
}

service_weekly_kpi <- function(snapshot) {
  x <- snapshot$kpi$indicatori$kpi_settimanali_gruppo
  if (is.null(x) || !nrow(x)) {
    x <- snapshot$kpi$indicatori$kpi_settimanali_canale_servizio
    x <- filter_kpi_rows(x, "group", "all")
    return(aggregate_kpi(x))
  }
  aggregate_kpi(x)
}

service_daily_kpi <- function(snapshot) {
  x <- snapshot$kpi$indicatori$kpi_giornalieri_gruppo
  if (is.null(x) || !nrow(x)) {
    x <- snapshot$kpi$indicatori$kpi_giornalieri_canale_servizio
    x <- filter_kpi_rows(x, "group", "all")
  }
  aggregate_kpi(x, group_cols = "giorno") %>% arrange(giorno)
}

selected_weekly_kpi <- function(snapshot, detail_choice = "group", channel_choice = "all") {
  x <- snapshot$kpi$indicatori$kpi_settimanali_canale_servizio
  x <- filter_kpi_rows(x, detail_choice, channel_choice)
  aggregate_kpi(x)
}

selected_daily_kpi <- function(snapshot, detail_choice = "group", channel_choice = "all") {
  x <- snapshot$kpi$indicatori$kpi_giornalieri_canale_servizio
  x <- filter_kpi_rows(x, detail_choice, channel_choice)
  aggregate_kpi(x, group_cols = "giorno") %>% arrange(giorno)
}

prepare_heatmap_data <- function(snapshot, detail_choice = "group", channel_choice = "all") {
  x <- snapshot$kpi$indicatori$contatti_fascia_settimana_canale_servizio
  x <- filter_kpi_rows(x, detail_choice, channel_choice)
  if (is.null(x) || !nrow(x)) return(data.frame())

  x <- x %>%
    group_by(canale, fascia_oraria) %>%
    summarise(contatti_ricevuti = sum(contatti_ricevuti, na.rm = TRUE), .groups = "drop") %>%
    mutate(riga = as.character(canale))

  fascia_levels <- c(
    "00:00-05:59", "06:00-08:59", "09:00-11:59", "12:00-14:59",
    "15:00-17:59", "18:00-20:59", "21:00-23:59"
  )

  x %>%
    mutate(fascia_oraria = factor(as.character(fascia_oraria), levels = fascia_levels)) %>%
    arrange(riga, fascia_oraria)
}

# =============================================================================
# 5. FUNZIONI CRM, OPERATORI E QUALITA
# =============================================================================

crm_metric <- function(snapshot, metric, partial = FALSE) {
  tab <- snapshot$crm$tabelle$tab_overview
  if (is.null(tab) || !nrow(tab) || !all(c("metrica", "valore") %in% names(tab))) {
    return(NA_real_)
  }

  key <- tolower(trimws(as.character(tab$metrica)))
  target <- tolower(trimws(metric))
  idx <- if (isTRUE(partial)) grep(target, key, fixed = TRUE) else which(key == target)
  if (!length(idx)) return(NA_real_)
  safe_num(tab$valore[idx[1L]])
}

quality_indicator <- function(snapshot, name, numeric = FALSE) {
  tab <- snapshot$qualita$completezza$riepilogo
  if (is.null(tab) || !all(c("Indicatore", "Valore") %in% names(tab))) {
    return(if (isTRUE(numeric)) NA_real_ else NA_character_)
  }
  pos <- match(name, as.character(tab$Indicatore))
  if (is.na(pos)) return(if (isTRUE(numeric)) NA_real_ else NA_character_)
  val <- tab$Valore[pos]
  if (isTRUE(numeric)) safe_num(val) else as.character(val)
}

operator_totals <- function(snapshot) {
  tab <- snapshot$operatori$attivita$settimanali
  if (is.null(tab) || !nrow(tab)) {
    return(list(
      ore = NA_real_, contatti = NA_real_, attivazioni = NA_real_,
      gestite_ora = NA_real_, attivazioni_ora = NA_real_,
      tempo_chiamata = NA_real_, tempo_chat = NA_real_
    ))
  }

  ore <- sum(safe_num(tab$ore_login), na.rm = TRUE)
  contatti <- sum(safe_num(tab$contatti_gestiti), na.rm = TRUE)
  attivazioni <- sum(safe_num(tab$attivazioni), na.rm = TRUE)
  chiamate_n <- sum(safe_num(tab$chiamate_gestite_tempo), na.rm = TRUE)
  chat_n <- sum(safe_num(tab$chat_gestite_tempo), na.rm = TRUE)
  chiamate_sec <- sum(safe_num(tab$tempo_totale_chiamate_secondi), na.rm = TRUE)
  chat_sec <- sum(safe_num(tab$tempo_totale_chat_secondi), na.rm = TRUE)

  list(
    ore = ore,
    contatti = contatti,
    attivazioni = attivazioni,
    gestite_ora = if (ore > 0) contatti / ore else NA_real_,
    attivazioni_ora = if (ore > 0) attivazioni / ore else NA_real_,
    tempo_chiamata = if (chiamate_n > 0) chiamate_sec / chiamate_n else NA_real_,
    tempo_chat = if (chat_n > 0) chat_sec / chat_n else NA_real_
  )
}

service_overview_metrics <- function(snapshot) {
  kpi <- service_weekly_kpi(snapshot)
  ops <- operator_totals(snapshot)
  schede <- quality_indicator(snapshot, "Schede Caso presenti nel perimetro", TRUE)
  missing <- quality_indicator(
    snapshot,
    "Schede valutate con almeno una variabile di raccolta/attribuzione mancante",
    TRUE
  )
  progressivi_verifica <- nrow(snapshot$qualita$completezza$progressivi_mancanti %||% data.frame())

  list(
    conversazioni = if (nrow(kpi)) kpi$conversazioni_totali[[1L]] else NA_real_,
    livello_servizio = if (nrow(kpi)) kpi$livello_servizio[[1L]] else NA_real_,
    casi_crm = crm_metric(snapshot, "Casi distinti"),
    ore_login = ops$ore,
    schede_mancanti = missing,
    quota_schede_mancanti = if (!is.na(schede) && schede > 0 && !is.na(missing)) missing / schede else NA_real_,
    progressivi_verifica = progressivi_verifica
  )
}

delta_note <- function(current, previous, mode = c("relative", "pp")) {
  mode <- match.arg(mode)
  current <- safe_num(current)
  previous <- safe_num(previous)

  if (!length(current) || !length(previous) || is.na(current[1L]) || is.na(previous[1L])) {
    return("Confronto non ancora disponibile")
  }

  current <- current[1L]
  previous <- previous[1L]

  if (mode == "relative") {
    if (previous == 0) return("Confronto non calcolabile")
    d <- 100 * (current - previous) / previous
    prefix <- if (d > 0) "+" else ""
    paste0(prefix, fmt_num(d, 1L), "% vs settimana precedente")
  } else {
    d <- 100 * (current - previous)
    prefix <- if (d > 0) "+" else ""
    paste0(prefix, fmt_num(d, 1L), " p.p. vs settimana precedente")
  }
}

prepare_progressivi_table <- function(snapshot) {
  x <- snapshot$qualita$completezza$progressivi_mancanti
  if (is.null(x) || !nrow(x)) return(data.frame())

  keep <- c(
    "Progressivo", "Operatore_creazione_caso",
    "N_variabili_raccolta_attribuzione_con_mancanti",
    "Variabili_raccolta_attribuzione_con_mancanti",
    "N_contatti_nel_periodo", "Stato_segnalazione",
    "Scheda_Caso_disponibile", "Sezioni_senza_record"
  )
  keep <- intersect(keep, names(x))
  x <- x[, keep, drop = FALSE]

  if ("Operatore_creazione_caso" %in% names(x)) {
    op <- as.character(x$Operatore_creazione_caso)
    op[is.na(op) | !nzchar(trimws(op))] <- "Scheda Caso assente / operatore non disponibile"
    x$Operatore_creazione_caso <- clean_operator_label(op)
  }

  if ("Scheda_Caso_disponibile" %in% names(x)) {
    x$Scheda_Caso_disponibile <- ifelse(as.logical(x$Scheda_Caso_disponibile), "Sì", "No")
  }

  ncol_missing <- "N_variabili_raccolta_attribuzione_con_mancanti"
  if (ncol_missing %in% names(x)) {
    x <- x[
      order(safe_num(x[[ncol_missing]]), decreasing = TRUE, na.last = TRUE),
      ,
      drop = FALSE
    ]
  }

  rename_map <- c(
    Progressivo = "Progressivo",
    Operatore_creazione_caso = "Operatore (Creato da)",
    N_variabili_raccolta_attribuzione_con_mancanti = "N. variabili mancanti",
    Variabili_raccolta_attribuzione_con_mancanti = "Dove mancano dati",
    N_contatti_nel_periodo = "Contatti nella settimana",
    Stato_segnalazione = "Stato segnalazione",
    Scheda_Caso_disponibile = "Scheda Caso disponibile",
    Sezioni_senza_record = "Sezioni senza record"
  )
  names(x) <- unname(rename_map[names(x)])
  rownames(x) <- NULL
  x
}

operator_quality_summary <- function(snapshot, include_special = FALSE) {
  x <- snapshot$qualita$completezza$operatori$copertura
  if (is.null(x) || !nrow(x)) return(data.frame())

  if (!isTRUE(include_special) && "Tipo_riga" %in% names(x)) {
    x <- x[x$Tipo_riga == "OPERATORE", , drop = FALSE]
  }

  if (!nrow(x)) return(x)

  den <- safe_num(x$Progressivi_con_almeno_un_dato_valutabile)
  num <- safe_num(x$Progressivi_con_almeno_un_mancante)
  x$Percentuale_progressivi_con_mancanti <- ifelse(
    den > 0,
    round(100 * num / den, 1L),
    NA_real_
  )
  x$Operatore_creazione_caso <- clean_operator_label(x$Operatore_creazione_caso)
  x
}

operator_overview_data <- function(snapshot) {
  a <- snapshot$operatori$attivita$settimanali
  if (is.null(a)) a <- data.frame()
  a <- as.data.frame(a, stringsAsFactors = FALSE)
  if (nrow(a)) {
    a$operatore_key <- operator_key(a$operatore)
    a <- a %>%
      transmute(
        operatore_key,
        Operatore = as.character(operatore),
        Ore_login = safe_num(ore_login),
        Eventi_log = safe_num(contatti_gestiti),
        Gestite_ora = safe_num(gestite_ora),
        Attivazioni = safe_num(attivazioni),
        Attivazioni_ora = safe_num(attivazioni_ora),
        Tempo_medio_chiamata = as.character(tempo_medio_chiamata_hhmmss),
        Tempo_medio_chat = as.character(tempo_medio_chat_hhmmss)
      )
  } else {
    a <- data.frame(operatore_key = character(), Operatore = character())
  }

  cvol <- snapshot$operatori$crm$tabelle$tab_volumi_operatore
  if (is.null(cvol)) cvol <- data.frame()
  cvol <- as.data.frame(cvol, stringsAsFactors = FALSE)
  if (nrow(cvol)) {
    cvol$operatore_key <- operator_key(cvol$operatore)
    cvol <- cvol %>%
      transmute(
        operatore_key,
        Operatore_crm = as.character(operatore),
        Casi_CRM = safe_num(n_casi_distinti),
        Contatti_CRM = safe_num(n_contatti_periodo),
        Contatti_per_caso = safe_num(contatti_medi_per_caso)
      )
  } else {
    cvol <- data.frame(operatore_key = character(), Operatore_crm = character())
  }

  crec <- snapshot$operatori$crm$tabelle$tab_ricontatto_operatore
  if (is.null(crec)) crec <- data.frame()
  crec <- as.data.frame(crec, stringsAsFactors = FALSE)
  if (nrow(crec)) {
    crec$operatore_key <- operator_key(crec$operatore)
    crec <- crec %>%
      transmute(
        operatore_key,
        Ricontatto = safe_num(pct_ricontatto_su_casi_con_contatto)
      )
  } else {
    crec <- data.frame(operatore_key = character(), Ricontatto = numeric())
  }

  q <- operator_quality_summary(snapshot, include_special = FALSE)
  q <- as.data.frame(q, stringsAsFactors = FALSE)
  if (nrow(q)) {
    q$operatore_key <- operator_key(q$Operatore_creazione_caso)
    q <- q %>%
      transmute(
        operatore_key,
        Operatore_qualita = as.character(Operatore_creazione_caso),
        Progressivi_qualita = safe_num(Progressivi_nel_perimetro),
        Progressivi_con_mancanti = safe_num(Progressivi_con_almeno_un_mancante),
        Percentuale_con_mancanti = safe_num(Percentuale_progressivi_con_mancanti)
      )
  } else {
    q <- data.frame(operatore_key = character(), Operatore_qualita = character())
  }

  out <- full_join(a, cvol, by = "operatore_key") %>%
    full_join(crec, by = "operatore_key") %>%
    full_join(q, by = "operatore_key")

  if (!nrow(out)) return(out)

  out <- out %>%
    mutate(
      Operatore = dplyr::coalesce(
        na_if(Operatore, ""),
        na_if(Operatore_crm, ""),
        na_if(Operatore_qualita, ""),
        operatore_key
      )
    ) %>%
    select(
      Operatore,
      Ore_login,
      Eventi_log,
      Gestite_ora,
      Attivazioni,
      Attivazioni_ora,
      Casi_CRM,
      Contatti_CRM,
      Contatti_per_caso,
      Ricontatto,
      Progressivi_qualita,
      Progressivi_con_mancanti,
      Percentuale_con_mancanti
    ) %>%
    arrange(desc(Eventi_log), desc(Casi_CRM), Operatore)

  out
}

operator_coverage_summary <- function(snapshot) {
  x <- snapshot$operatori$attivita$settimanali
  x <- as.data.frame(x %||% data.frame(), stringsAsFactors = FALSE)

  data.frame(
    Indicatore = c(
      "Operatori mappati nel servizio",
      "Operatori presenti nella settimana",
      "Con ore di login positive",
      "Con almeno un evento gestito",
      "Con almeno un'attivazione",
      "Con tempi chiamata disponibili",
      "Con tempi chat disponibili",
      "Operatori-giorno con attività ma senza ore"
    ),
    Valore = c(
      length(snapshot$metadata$operatori_mappati %||% character()),
      if (nrow(x)) length(unique(operator_key(x$operatore))) else 0,
      if (nrow(x)) sum(safe_num(x$ore_login) > 0, na.rm = TRUE) else 0,
      if (nrow(x)) sum(safe_num(x$contatti_gestiti) > 0, na.rm = TRUE) else 0,
      if (nrow(x)) sum(safe_num(x$attivazioni) > 0, na.rm = TRUE) else 0,
      if (nrow(x)) sum(safe_num(x$chiamate_gestite_tempo) > 0, na.rm = TRUE) else 0,
      if (nrow(x)) sum(safe_num(x$chat_gestite_tempo) > 0, na.rm = TRUE) else 0,
      safe_num(snapshot$kpi$controlli$n_operatori_giorno_senza_ore %||% 0)
    ),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# 6. FUNZIONI GRAFICHE E UI
# =============================================================================

theme_ta <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", color = TA_BLUE_DARK, size = 14),
      plot.subtitle = element_text(color = TA_GREY, size = 10),
      axis.title = element_text(color = TA_BLUE_DARK),
      axis.text = element_text(color = "#344054"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.margin = margin(8, 12, 8, 8)
    )
}

plot_daily_metric <- function(df, metric, title, percent = FALSE) {
  if (is.null(df) || !nrow(df) || !metric %in% names(df)) return(NULL)

  p <- ggplot(df, aes(x = giorno, y = .data[[metric]], group = 1)) +
    geom_line(linewidth = 1.05, color = ACCENT, na.rm = TRUE) +
    geom_point(size = 2.8, color = ACCENT, na.rm = TRUE) +
    scale_x_date(
      breaks = sort(unique(as.Date(df$giorno))),
      labels = function(x) format(x, "%d/%m")
    ) +
    labs(title = title, x = NULL, y = NULL) +
    theme_ta() +
    theme(legend.position = "none")

  if (isTRUE(percent)) {
    p <- p + scale_y_continuous(
      labels = scales::percent_format(accuracy = 1, decimal.mark = ","),
      limits = c(0, 1),
      expand = expansion(mult = c(0.02, 0.08))
    )
  } else {
    p <- p + scale_y_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ","),
      expand = expansion(mult = c(0.02, 0.12))
    )
  }

  p
}

plot_distribution <- function(tab, title, top_n = 12L) {
  if (is.null(tab) || !nrow(tab)) return(NULL)
  tab <- as.data.frame(tab, stringsAsFactors = FALSE)

  count_candidates <- c("n_casi", "n_contatti", "n", "valore")
  count_col <- count_candidates[count_candidates %in% names(tab)][1]
  if (is.na(count_col) || !length(count_col)) return(NULL)

  excluded <- c(
    count_col, "totale", "pct", "pct_label", "totale_periodo", "pct_periodo",
    "totale_giorno", "pct_giorno", "n_casi_operatore", "n_contatti_operatore",
    "pct_operatore"
  )
  cat_candidates <- setdiff(names(tab), excluded)
  if (!length(cat_candidates)) return(NULL)

  char_candidates <- cat_candidates[vapply(
    tab[cat_candidates],
    function(z) is.character(z) || is.factor(z),
    logical(1)
  )]
  cat_col <- if (length(char_candidates)) char_candidates[1L] else cat_candidates[1L]

  z <- data.frame(
    categoria = as.character(tab[[cat_col]]),
    valore = safe_num(tab[[count_col]]),
    stringsAsFactors = FALSE
  )
  z$categoria[is.na(z$categoria) | !nzchar(trimws(z$categoria))] <- "Non indicato"
  z <- z[!is.na(z$valore) & z$valore > 0, , drop = FALSE]
  if (!nrow(z)) return(NULL)

  z <- z %>%
    group_by(categoria) %>%
    summarise(valore = sum(valore, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(valore)) %>%
    slice_head(n = top_n) %>%
    mutate(categoria = reorder(categoria, valore))

  ggplot(z, aes(x = categoria, y = valore)) +
    geom_col(fill = ACCENT, width = 0.72) +
    geom_text(
      aes(label = vapply(valore, fmt_int, character(1))),
      hjust = -0.12,
      size = 3.5,
      color = TA_BLUE_DARK
    ) +
    coord_flip() +
    scale_y_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ","),
      expand = expansion(mult = c(0, 0.18))
    ) +
    labs(title = title, x = NULL, y = NULL) +
    theme_ta() +
    theme(legend.position = "none")
}

plot_crm_daily <- function(crm) {
  casi <- crm$tabelle$tab_casi_giorno
  contatti <- crm$tabelle$tab_contatti_giorno
  if (is.null(casi) || is.null(contatti)) return(NULL)

  a <- casi %>%
    transmute(giorno = as.Date(giorno_caso), indicatore = "Casi", valore = n_casi)
  b <- contatti %>%
    transmute(giorno = as.Date(giorno_contatto), indicatore = "Contatti", valore = n_contatti)

  z <- bind_rows(a, b)
  if (!nrow(z)) return(NULL)

  ggplot(z, aes(x = giorno, y = valore, color = indicatore, group = indicatore)) +
    geom_line(linewidth = 1.05) +
    geom_point(size = 2.8) +
    scale_color_manual(values = c("Casi" = ACCENT, "Contatti" = TA_GREEN)) +
    scale_x_date(
      breaks = sort(unique(z$giorno)),
      labels = function(x) format(x, "%d/%m")
    ) +
    scale_y_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ","),
      expand = expansion(mult = c(0.02, 0.12))
    ) +
    labs(title = "Casi e contatti agganciati per giorno", x = NULL, y = NULL) +
    theme_ta()
}

kpi_card <- function(label, value, note = NULL, emphasis = FALSE) {
  div(
    class = paste("kpi-card", if (isTRUE(emphasis)) "kpi-card-emphasis" else ""),
    div(class = "kpi-label", label),
    div(class = "kpi-value", value),
    if (!is.null(note)) div(class = "kpi-note", note)
  )
}

panel_box <- function(title, note = NULL, ...) {
  div(
    class = "panel-card",
    div(class = "panel-title", title),
    if (!is.null(note)) div(class = "panel-note", note),
    ...
  )
}

attention_legend <- function() {
  div(
    class = "attention-legend",
    span(class = "legend-chip legend-yellow", "Giallo: attenzione (10–24,9%)"),
    span(class = "legend-chip legend-red", "Rosso: attenzione elevata (≥25%)"),
    span(class = "legend-chip legend-grey", "ND: non valutabile")
  )
}

crm_ui <- function() {
  tabsetPanel(
    id = "crm_subtab",
    type = "pills",
    tabPanel(
      "Sintesi",
      uiOutput("crm_cards"),
      fluidRow(
        column(7, panel_box("Andamento giornaliero", plotOutput("crm_daily", height = "380px"))),
        column(5, panel_box("Quadro CRM", DTOutput("crm_overview")))
      )
    ),
    tabPanel(
      "Canali ed esiti",
      fluidRow(
        column(6, panel_box("Direzione dei contatti", plotOutput("crm_direction", height = "360px"))),
        column(6, panel_box("Stato di chiusura dei casi", plotOutput("crm_closure", height = "360px")))
      ),
      panel_box(
        "Dettaglio canali ed esiti",
        fluidRow(
          column(
            5,
            selectInput(
              "crm_contact_detail_select",
              "Tabella",
              choices = c(
                "Tipologia contatto" = "tab_contatti_tipo",
                "Direzione" = "tab_contatti_direzione",
                "Esito" = "tab_contatti_esito",
                "Stato chiusura" = "tab_stato_chiusura"
              ),
              width = "100%"
            )
          )
        ),
        DTOutput("crm_contact_detail")
      )
    ),
    tabPanel(
      "Aree e motivazioni",
      fluidRow(
        column(6, panel_box("Aree primarie", plotOutput("crm_area", height = "430px"))),
        column(6, panel_box("Categorie primarie", plotOutput("crm_category", height = "430px")))
      ),
      panel_box(
        "Altri driver del caso",
        fluidRow(
          column(
            5,
            selectInput(
              "crm_area_detail_select",
              "Approfondimento",
              choices = c(
                "Elemento primario" = "tab_elemento_primario",
                "Frequenza" = "tab_frequenza",
                "Luogo prevalente" = "tab_luogo_prevalente",
                "Da quanto persiste" = "tab_da_quanto_persiste",
                "Intervento immediato" = "tab_intervento_immediato"
              ),
              width = "100%"
            )
          )
        ),
        DTOutput("crm_area_detail")
      )
    ),
    tabPanel(
      "Profilo",
      fluidRow(
        column(6, panel_box("Età dei minori", plotOutput("crm_minor_age", height = "380px"))),
        column(6, panel_box("Rapporto del chiamante", plotOutput("crm_caller_relation", height = "380px")))
      ),
      panel_box(
        "Dettaglio profilo",
        fluidRow(
          column(
            5,
            selectInput(
              "crm_profile_select",
              "Indicatore",
              choices = c(
                "Età chiamante" = "tab_chiamante_eta",
                "Genere chiamante" = "tab_chiamante_genere",
                "Rapporto chiamante" = "tab_chiamante_rapporto",
                "Chiamante anonimo" = "tab_chiamante_anonimo",
                "Chiamante interessato diretto" = "tab_chiamante_interessato",
                "Numero minori per caso" = "tab_n_minori_per_caso",
                "Età minori" = "tab_eta_minori",
                "Genere minori" = "tab_genere_minori",
                "Con chi vive il minore" = "tab_con_chi_vive_minori",
                "Cittadinanza minori" = "tab_cittadinanza_minori",
                "Genere responsabile" = "tab_responsabile_genere",
                "Rapporto responsabile" = "tab_responsabile_rapporto",
                "Numero responsabili per caso" = "tab_n_responsabili_per_caso"
              ),
              width = "100%"
            )
          )
        ),
        DTOutput("crm_profile_table")
      )
    ),
    tabPanel(
      "Attivazioni",
      fluidRow(
        column(6, panel_box("Casi con servizi attivati", plotOutput("crm_services_yesno", height = "350px"))),
        column(6, panel_box("Tipologia di servizio", plotOutput("crm_service_type", height = "350px")))
      ),
      panel_box("Strutture attivate", DTOutput("crm_service_structures"))
    ),
    tabPanel(
      "Geografia",
      fluidRow(
        column(7, panel_box("Prime regioni", plotOutput("crm_regions", height = "440px"))),
        column(5, panel_box("Distribuzione nazionale", DTOutput("crm_countries")))
      ),
      panel_box("Province", DTOutput("crm_provinces"))
    )
  )
}

operatori_ui <- function() {
  tabsetPanel(
    id = "operator_subtab",
    type = "pills",
    tabPanel(
      "Quadro generale",
      uiOutput("operator_cards"),
      panel_box(
        "Vista integrata per operatore",
        paste0(
          "La tabella accosta attività da log, casi CRM e completezza. ",
          "Le fonti hanno significati diversi e non costituiscono una graduatoria individuale."
        ),
        DTOutput("operator_overview_table")
      )
    ),
    tabPanel(
      "Attività",
      fluidRow(
        column(
          8,
          panel_box(
            "Indicatori operativi settimanali",
            "Ore di LOGIN ed eventi TALKING/INTERACTION attribuiti al gruppo di servizio.",
            DTOutput("operator_activity_table")
          )
        ),
        column(
          4,
          panel_box(
            "Copertura delle fonti",
            DTOutput("operator_coverage_table")
          )
        )
      ),
      panel_box("Tempi di gestione giornalieri", DTOutput("operator_time_table"))
    ),
    tabPanel(
      "CRM",
      panel_box(
        "Analisi CRM per operatore",
        "L'operatore corrisponde al campo 'Creato da' della scheda Caso.",
        fluidRow(
          column(
            5,
            selectInput(
              "operator_crm_table_select",
              "Tabella",
              choices = c(
                "Volumi settimanali" = "tab_volumi_operatore",
                "Ricontatto settimanale" = "tab_ricontatto_operatore",
                "Stato chiusura - casi" = "tab_stato_operatore_casi",
                "Stato chiusura - contatti" = "tab_stato_operatore_contatti",
                "Aree - casi" = "tab_area_operatore_casi",
                "Aree - contatti" = "tab_area_operatore_contatti"
              ),
              width = "100%"
            )
          )
        ),
        DTOutput("operator_crm_table")
      )
    ),
    tabPanel(
      "Completezza",
      panel_box(
        "Sintesi per creatore della scheda Caso",
        paste0(
          "Il creatore della scheda non identifica necessariamente chi ha compilato o omesso il singolo campo. ",
          "La tabella è uno strumento di verifica operativa, non una graduatoria."
        ),
        checkboxInput("op_quality_special", "Mostra anche gruppi non attribuibili e totale servizio", FALSE),
        DTOutput("operator_quality_summary")
      ),
      panel_box(
        "Matrice operatore × variabile",
        fluidRow(
          column(
            5,
            selectInput(
              "op_quality_metric",
              "Metrica",
              choices = c(
                "Tutti i mancanti (VUOTO + NON_NOTO)" = "mancanti",
                "Solo campi vuoti" = "vuoti",
                "Solo valori NON_NOTO" = "non_noti",
                "Celle/record mancanti" = "record_mancanti",
                "Progressivi valutabili (denominatore)" = "denominatori"
              ),
              selected = "mancanti",
              width = "100%"
            )
          )
        ),
        attention_legend(),
        DTOutput("operator_quality_matrix")
      ),
      panel_box(
        "Dettaglio per operatore",
        fluidRow(
          column(6, selectInput("op_quality_detail", "Operatore / gruppo", choices = NULL, width = "100%")),
          column(6, selectInput("op_quality_section", "Sezione CRM", choices = "Tutte", width = "100%"))
        ),
        attention_legend(),
        DTOutput("operator_quality_detail_table")
      )
    )
  )
}

qualita_ui <- function() {
  tabsetPanel(
    id = "quality_subtab",
    type = "pills",
    tabPanel(
      "Sintesi",
      uiOutput("quality_cards"),
      fluidRow(
        column(7, panel_box("Quadro di sintesi", DTOutput("quality_summary"))),
        column(5, panel_box("Copertura delle sezioni CRM", DTOutput("quality_sections")))
      )
    ),
    tabPanel(
      "Variabili",
      panel_box(
        "Variabili con dati mancanti",
        fluidRow(
          column(4, selectInput("quality_section_filter", "Sezione CRM", choices = "Tutte", width = "100%")),
          column(4, checkboxInput("quality_only_missing", "Solo variabili con almeno un mancante", TRUE)),
          column(4, checkboxInput("quality_show_records", "Mostra indicatori a livello record", FALSE))
        ),
        div(
          class = "panel-note",
          "La percentuale principale usa come denominatore i progressivi con almeno un record valutabile per la variabile."
        ),
        attention_legend(),
        DTOutput("quality_variables")
      )
    ),
    tabPanel(
      "Progressivi",
      panel_box(
        "Progressivi con dati da verificare",
        paste0(
          "La tabella contiene identificativo, creatore e descrizione delle lacune, ma non i valori originali dei campi CRM. ",
          "Uso interno riservato."
        ),
        DTOutput("quality_progressivi")
      )
    ),
    tabPanel(
      "Controlli",
      fluidRow(
        column(6, panel_box("Controlli completezza CRM", DTOutput("quality_controls"))),
        column(6, panel_box("Coerenza KPI di servizio", DTOutput("quality_kpi_controls")))
      ),
      panel_box("Confronto log operatori / KPI", DTOutput("quality_log_kpi"))
    )
  )
}

# =============================================================================
# 7. UI
# =============================================================================

css_text <- sprintf("\n  :root {\n    --ta-blue: %s;\n    --ta-blue-dark: %s;\n    --ta-accent: %s;\n    --ta-accent-light: %s;\n    --ta-border: %s;\n    --ta-text: #24313B;\n    --ta-muted: %s;\n    --ta-bg: %s;\n  }\n\n  html { scroll-behavior: smooth; }\n  body { background: var(--ta-bg); color: var(--ta-text); }\n  .container-fluid { max-width: 1700px; padding-left: 18px; padding-right: 18px; }\n\n  .app-header {\n    background: linear-gradient(135deg, var(--ta-blue-dark), var(--ta-accent));\n    color: white; border-radius: 16px; padding: 22px 26px; margin: 18px 0 14px 0;\n    box-shadow: 0 6px 18px rgba(0,0,0,.09);\n  }\n  .app-kicker { font-size: 12px; letter-spacing: .08em; text-transform: uppercase; opacity: .86; font-weight: 700; }\n  .app-title { font-size: 29px; font-weight: 750; line-height: 1.1; margin-top: 4px; }\n  .app-subtitle { opacity: .92; margin-top: 7px; font-size: 14px; }\n\n  .filter-panel {\n    background: white; border: 1px solid var(--ta-border); border-radius: 13px;\n    padding: 14px 16px 6px 16px; margin-bottom: 14px; box-shadow: 0 2px 8px rgba(0,0,0,.04);\n  }\n  .filter-panel .form-group { margin-bottom: 8px; }\n  .snapshot-strip {\n    background: white; border-left: 4px solid var(--ta-accent); border-radius: 9px;\n    padding: 11px 14px; margin-bottom: 14px; color: #344054; font-size: 13px;\n  }\n  .privacy-strip {\n    background: #FFF8E6; border-left: 4px solid #D99B00; border-radius: 9px;\n    padding: 10px 14px; margin-bottom: 14px; color: #614700; font-size: 12px;\n  }\n\n  .kpi-grid {\n    display: grid; grid-template-columns: repeat(6, minmax(145px, 1fr));\n    gap: 12px; margin: 14px 0 16px 0;\n  }\n  .kpi-card {\n    background: white; border: 1px solid var(--ta-border); border-radius: 13px;\n    padding: 15px 16px; min-height: 112px; box-shadow: 0 2px 8px rgba(0,0,0,.04);\n  }\n  .kpi-card-emphasis { border-top: 4px solid var(--ta-accent); padding-top: 12px; }\n  .kpi-label { color: var(--ta-muted); font-size: 12px; line-height: 1.25; min-height: 30px; }\n  .kpi-value { color: var(--ta-blue-dark); font-size: 29px; font-weight: 750; margin-top: 5px; }\n  .kpi-note { color: var(--ta-muted); font-size: 11px; margin-top: 5px; line-height: 1.25; }\n\n  .panel-card {\n    background: white; border: 1px solid var(--ta-border); border-radius: 13px;\n    padding: 16px; margin-bottom: 16px; box-shadow: 0 2px 8px rgba(0,0,0,.04);\n  }\n  .panel-title { font-size: 18px; font-weight: 700; color: #25364A; margin-bottom: 9px; }\n  .panel-note { font-size: 12px; color: var(--ta-muted); margin-bottom: 11px; line-height: 1.45; }\n  .small-muted { color: var(--ta-muted); font-size: 12px; }\n\n  .attention-legend { display: flex; flex-wrap: wrap; gap: 8px; margin: 8px 0 12px 0; }\n  .legend-chip { border-radius: 999px; padding: 5px 9px; font-size: 11px; border: 1px solid rgba(0,0,0,.08); }\n  .legend-yellow { background: #FFF3CD; color: #664D03; }\n  .legend-red { background: #F8D7DA; color: #842029; }\n  .legend-grey { background: #E9ECEF; color: #495057; }\n\n  .nav-tabs { border-bottom: 1px solid var(--ta-border); margin-bottom: 14px; }\n  .nav-tabs > li > a { color: var(--ta-blue-dark); font-weight: 650; }\n  .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {\n    color: var(--ta-blue-dark); border-top: 3px solid var(--ta-accent);\n  }\n  .nav-pills > li > a { color: var(--ta-blue-dark); font-weight: 600; }\n  .nav-pills > li.active > a, .nav-pills > li.active > a:focus, .nav-pills > li.active > a:hover {\n    background: var(--ta-accent);\n  }\n\n  table.dataTable thead th { white-space: nowrap; }\n  .dataTables_wrapper { font-size: 12px; }\n  .dataTables_scrollBody { border-bottom: 1px solid #ddd !important; }\n  .method-list { padding-left: 20px; }\n  .method-list li { margin-bottom: 8px; line-height: 1.42; }\n\n  @media (max-width: 1250px) { .kpi-grid { grid-template-columns: repeat(3, minmax(150px, 1fr)); } }\n  @media (max-width: 900px) { .kpi-grid { grid-template-columns: repeat(2, minmax(140px, 1fr)); } }\n  @media (max-width: 760px) {\n    .container-fluid { padding-left: 10px; padding-right: 10px; }\n    .app-header { padding: 18px; margin-top: 10px; border-radius: 12px; }\n    .app-title { font-size: 23px; }\n    .kpi-card { min-height: 100px; padding: 12px; }\n    .kpi-value { font-size: 25px; }\n    .panel-card { padding: 12px; }\n    .nav-tabs, .nav-pills {\n      display: flex; flex-wrap: nowrap; overflow-x: auto; overflow-y: hidden;\n      -webkit-overflow-scrolling: touch; white-space: nowrap;\n    }\n    .nav-tabs > li, .nav-pills > li { float: none; flex: 0 0 auto; }\n  }\n  @media (max-width: 430px) { .kpi-grid { grid-template-columns: 1fr; } }\n", TA_BLUE, TA_BLUE_DARK, ACCENT, ACCENT_LIGHT, TA_BORDER, TA_GREY, TA_LIGHT)

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML(css_text))
  ),

  div(
    class = "app-header",
    div(class = "app-kicker", paste("Telefono Azzurro ·", SERVIZIO_LABEL_BREVE)),
    div(class = "app-title", paste("Dashboard settimanale ·", SERVIZIO_LABEL)),
    div(class = "app-subtitle", uiOutput("header_subtitle"))
  ),

  div(
    class = "filter-panel",
    fluidRow(
      column(6, selectInput("settimana", "Settimana", choices = NULL, width = "100%")),
      column(3, br(), actionButton("refresh_data", "Aggiorna elenco", class = "btn-primary", width = "100%")),
      column(3, br(), div(class = "small-muted", textOutput("snapshot_count", inline = TRUE)))
    )
  ),

  uiOutput("snapshot_info"),
  div(
    class = "privacy-strip",
    "Uso interno riservato: la sezione Qualità dati contiene progressivi CRM con informazioni di completezza."
  ),

  tabsetPanel(
    id = "main_tab",

    tabPanel(
      "Panoramica",
      uiOutput("overview_cards"),
      fluidRow(
        column(6, panel_box("Conversazioni per giorno", plotOutput("overview_volume_plot", height = "390px"))),
        column(6, panel_box("Livello di servizio per giorno", plotOutput("overview_ls_plot", height = "390px")))
      ),
      fluidRow(
        column(6, panel_box("Sintesi CRM", DTOutput("overview_crm_table"))),
        column(6, panel_box("Sintesi qualità dati", DTOutput("overview_quality_table")))
      )
    ),

    tabPanel(
      "KPI",
      panel_box(
        "Filtri KPI",
        fluidRow(
          if (SERVIZIO_APP == "114") {
            column(
              6,
              selectInput(
                "kpi_detail",
                "Perimetro 114",
                choices = c(
                  "114 incl. Multilingua" = "group",
                  "114" = "114",
                  "114 Multilingua" = "114_ml"
                ),
                selected = "group",
                width = "100%"
              )
            )
          } else {
            column(6, div(class = "small-muted", style = "padding-top:30px;", "Perimetro: 1.96.96"))
          },
          column(
            6,
            selectInput(
              "kpi_channel",
              "Canale",
              choices = c(
                "Tutti i canali" = "all",
                "Telefono" = "Telefono",
                "Chat" = "Chat",
                "WhatsApp" = "WhatsApp"
              ),
              selected = "all",
              width = "100%"
            )
          )
        )
      ),
      uiOutput("kpi_cards"),
      fluidRow(
        column(6, panel_box("Conversazioni per giorno", plotOutput("kpi_daily_volume", height = "390px"))),
        column(6, panel_box("Livello di servizio per giorno", plotOutput("kpi_daily_ls", height = "390px")))
      ),
      panel_box(
        "Distribuzione per fascia oraria",
        "La mappa mostra i contatti ricevuti a prescindere dall'esito.",
        plotOutput("kpi_heatmap", height = "430px")
      ),
      panel_box("Dettaglio settimanale per canale", DTOutput("kpi_detail_table"))
    ),

    tabPanel("CRM", crm_ui()),
    tabPanel("Operatori", operatori_ui()),
    tabPanel("Qualità dati", qualita_ui()),

    tabPanel(
      "Metodo",
      fluidRow(
        column(7, panel_box("Regole di lettura", uiOutput("method_notes"))),
        column(5, panel_box("Metadati snapshot", DTOutput("metadata_table")))
      ),
      panel_box(
        "Perimetro e interpretazione",
        tags$ul(
          class = "method-list",
          tags$li("L'app legge uno snapshot già calcolato e non ricalcola i KPI a ogni accesso."),
          tags$li("Lo snapshot contiene un solo servizio; il 116000 è escluso."),
          if (SERVIZIO_APP == "114") tags$li("Il 114 Multilingua è incluso nel gruppo 114 e resta distinguibile nella sezione KPI."),
          tags$li("I dati CRM case-level e contact-level non sono inclusi nello snapshot."),
          tags$li("I progressivi mostrati in Qualità dati servono esclusivamente alla verifica operativa interna."),
          tags$li("Le analisi per operatore accostano fonti diverse e non costituiscono una graduatoria individuale."),
          tags$li("Le settimane ordinarie vanno da lunedì a domenica.")
        )
      )
    )
  ),

  div(
    class = "small-muted",
    style = "text-align:center; margin: 10px 0 24px 0;",
    "Fondazione S.O.S. Il Telefono Azzurro ETS · Uso interno"
  )
)

# =============================================================================
# 8. SERVER
# =============================================================================

server <- function(input, output, session) {

  snapshot_index <- reactiveVal(scan_snapshots())

  refresh_week_selector <- function(prefer = isolate(input$settimana)) {
    idx <- snapshot_index()
    if (!nrow(idx)) {
      updateSelectInput(session, "settimana", choices = character())
      return(invisible(NULL))
    }

    choices <- stats::setNames(idx$snapshot_id, idx$settimana_label)
    selected <- if (!is.null(prefer) && prefer %in% idx$snapshot_id) prefer else idx$snapshot_id[1L]
    updateSelectInput(session, "settimana", choices = choices, selected = selected)
  }

  observeEvent(TRUE, {
    refresh_week_selector()
  }, once = TRUE)

  observeEvent(input$refresh_data, {
    current <- isolate(input$settimana)
    snapshot_index(scan_snapshots())
    refresh_week_selector(current)
    showNotification("Elenco snapshot aggiornato.", type = "message", duration = 3)
  })

  selected_row <- reactive({
    req(input$settimana)
    idx <- snapshot_index()
    z <- idx[idx$snapshot_id == input$settimana, , drop = FALSE]
    validate(need(nrow(z) >= 1L, "Snapshot non trovato per la settimana selezionata."))
    z[1L, , drop = FALSE]
  })

  active_snapshot <- reactive({
    row <- selected_row()
    validate(need(file.exists(row$path), paste("File RDS non trovato:", row$path)))

    tryCatch({
      snap <- validate_snapshot(readRDS(row$path))
      md <- snap$metadata

      if (!identical(as.character(md$settimana_id), as.character(row$settimana_id)) ||
          !identical(as.Date(md$data_inizio), as.Date(row$data_inizio)) ||
          !identical(as.Date(md$data_fine), as.Date(row$data_fine))) {
        stop(
          "Metadati dello snapshot non coerenti con il nome del file selezionato.",
          call. = FALSE
        )
      }

      snap
    }, error = function(e) {
      validate(need(FALSE, paste("Impossibile leggere lo snapshot:", conditionMessage(e))))
    })
  })

  previous_snapshot <- reactive({
    row <- selected_row()
    idx <- snapshot_index()
    expected_start <- as.Date(row$data_inizio) - 7
    expected_end <- as.Date(row$data_inizio) - 1
    z <- idx[idx$data_inizio == expected_start & idx$data_fine == expected_end, , drop = FALSE]
    if (!nrow(z) || !file.exists(z$path[1L])) return(NULL)
    tryCatch(validate_snapshot(readRDS(z$path[1L])), error = function(e) NULL)
  })

  output$snapshot_count <- renderText({
    n <- nrow(snapshot_index())
    if (!n) {
      "Nessuno snapshot disponibile"
    } else {
      paste0(n, if (n == 1L) " settimana disponibile" else " settimane disponibili")
    }
  })

  output$header_subtitle <- renderUI({
    x <- active_snapshot()
    md <- x$metadata
    span(paste0(
      md$settimana_id, " · ISO ", md$settimana_iso, " · ",
      fmt_week_it(md$data_inizio, md$data_fine),
      " · KPI, CRM, operatori e qualità dati"
    ))
  })

  output$snapshot_info <- renderUI({
    x <- active_snapshot()
    md <- x$metadata
    generated <- as.POSIXct(md$creato_il)

    extra <- if (SERVIZIO_APP == "114") {
      " · 114 Multilingua incluso nel gruppo 114"
    } else {
      ""
    }

    div(
      class = "snapshot-strip",
      strong(paste0(
        SERVIZIO_LABEL, " · ", md$settimana_id, " · ISO ", md$settimana_iso,
        " · ", fmt_week_it(md$data_inizio, md$data_fine)
      )),
      " · snapshot generato il ", format(generated, "%d/%m/%Y %H:%M"),
      extra
    )
  })

  # ---------------------------------------------------------------------------
  # PANORAMICA
  # ---------------------------------------------------------------------------

  output$overview_cards <- renderUI({
    x <- active_snapshot()
    p <- previous_snapshot()
    cur <- service_overview_metrics(x)
    prev <- if (!is.null(p)) service_overview_metrics(p) else list()

    div(
      class = "kpi-grid",
      kpi_card(
        "Conversazioni ricevute",
        fmt_int(cur$conversazioni),
        delta_note(cur$conversazioni, prev$conversazioni %||% NA_real_, "relative"),
        emphasis = TRUE
      ),
      kpi_card(
        "Livello di servizio",
        fmt_pct_ratio(cur$livello_servizio),
        delta_note(cur$livello_servizio, prev$livello_servizio %||% NA_real_, "pp"),
        emphasis = TRUE
      ),
      kpi_card(
        "Casi CRM aperti",
        fmt_int(cur$casi_crm),
        delta_note(cur$casi_crm, prev$casi_crm %||% NA_real_, "relative")
      ),
      kpi_card(
        "Ore di login",
        fmt_hours(cur$ore_login),
        delta_note(cur$ore_login, prev$ore_login %||% NA_real_, "relative")
      ),
      kpi_card(
        "Schede con almeno un mancante",
        fmt_int(cur$schede_mancanti),
        paste0(fmt_pct_ratio(cur$quota_schede_mancanti), " delle schede valutate")
      ),
      kpi_card(
        "Progressivi da verificare",
        fmt_int(cur$progressivi_verifica),
        "Dettaglio disponibile in Qualità dati"
      )
    )
  })

  output$overview_volume_plot <- renderPlot({
    z <- service_daily_kpi(active_snapshot())
    validate(need(nrow(z) > 0, "Dati giornalieri non disponibili."))
    plot_daily_metric(z, "conversazioni_totali", "Conversazioni ricevute")
  }, res = 110)

  output$overview_ls_plot <- renderPlot({
    z <- service_daily_kpi(active_snapshot())
    validate(need(nrow(z) > 0, "Dati giornalieri non disponibili."))
    plot_daily_metric(z, "livello_servizio", "Livello di servizio", percent = TRUE)
  }, res = 110)

  output$overview_crm_table <- renderDT({
    x <- active_snapshot()$crm$tabelle$tab_overview
    validate(need(!is.null(x) && nrow(x) > 0, "Sintesi CRM non disponibile."))
    DT::datatable(
      format_display_df(x),
      rownames = FALSE,
      selection = "none",
      options = list(dom = "t", pageLength = 20, scrollX = TRUE)
    )
  }, server = FALSE)

  output$overview_quality_table <- renderDT({
    x <- active_snapshot()$qualita$completezza$riepilogo
    validate(need(!is.null(x) && nrow(x) > 0, "Sintesi qualità non disponibile."))

    wanted <- c(
      "Progressivi unici nella tabella completa",
      "Schede Caso presenti nel perimetro",
      "Contatti CRM nel periodo (con e senza progressivo)",
      "Contatti senza progressivo nel periodo",
      "Schede valutate con almeno una variabile di raccolta/attribuzione mancante",
      "Schede valutate senza record Chiamante",
      "Schede valutate senza record Minori",
      "Schede valutate senza record Motivazioni",
      "Schede valutate senza record Responsabili",
      "Schede valutate senza record Servizi"
    )
    x <- x[x$Indicatore %in% wanted, , drop = FALSE]

    DT::datatable(
      x,
      rownames = FALSE,
      selection = "none",
      options = list(dom = "t", pageLength = 20, scrollX = TRUE)
    )
  }, server = FALSE)

  # ---------------------------------------------------------------------------
  # KPI
  # ---------------------------------------------------------------------------

  selected_kpi_week <- reactive({
    selected_weekly_kpi(
      active_snapshot(),
      input$kpi_detail %||% "group",
      input$kpi_channel %||% "all"
    )
  })

  selected_kpi_day <- reactive({
    selected_daily_kpi(
      active_snapshot(),
      input$kpi_detail %||% "group",
      input$kpi_channel %||% "all"
    )
  })

  output$kpi_cards <- renderUI({
    z <- selected_kpi_week()
    validate(need(nrow(z) > 0, "Nessun KPI disponibile con i filtri selezionati."))
    r <- z[1L, , drop = FALSE]

    div(
      class = "kpi-grid",
      kpi_card("Conversazioni", fmt_int(r$conversazioni_totali), emphasis = TRUE),
      kpi_card("Gestite", fmt_int(r$gestite)),
      kpi_card("Abbandonate", fmt_int(r$abbandonate)),
      kpi_card("Non gestite", fmt_int(r$non_gestite)),
      kpi_card("Livello servizio", fmt_pct_ratio(r$livello_servizio), emphasis = TRUE),
      kpi_card("Tasso abbandono", fmt_pct_ratio(r$tasso_abbandono)),
      kpi_card("Tasso non gestite", fmt_pct_ratio(r$tasso_non_gestite))
    )
  })

  output$kpi_daily_volume <- renderPlot({
    z <- selected_kpi_day()
    validate(need(nrow(z) > 0, "Dati giornalieri non disponibili."))
    plot_daily_metric(z, "conversazioni_totali", "Conversazioni ricevute")
  }, res = 110)

  output$kpi_daily_ls <- renderPlot({
    z <- selected_kpi_day()
    validate(need(nrow(z) > 0, "Dati giornalieri non disponibili."))
    plot_daily_metric(z, "livello_servizio", "Livello di servizio", percent = TRUE)
  }, res = 110)

  output$kpi_heatmap <- renderPlot({
    z <- prepare_heatmap_data(
      active_snapshot(),
      input$kpi_detail %||% "group",
      input$kpi_channel %||% "all"
    )
    validate(need(nrow(z) > 0, "Dati per fascia oraria non disponibili."))

    ggplot(z, aes(x = fascia_oraria, y = riga, fill = contatti_ricevuti)) +
      geom_tile(color = "white", linewidth = 0.8) +
      geom_text(aes(label = vapply(contatti_ricevuti, fmt_int, character(1))), size = 3.6, color = "#152534") +
      scale_fill_gradient(
        low = ACCENT_LIGHT,
        high = ACCENT,
        labels = scales::label_number(big.mark = ".", decimal.mark = ",")
      ) +
      labs(x = NULL, y = NULL, fill = "Contatti") +
      theme_ta() +
      theme(
        axis.text.x = element_text(angle = 35, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "bottom"
      )
  }, res = 110)

  output$kpi_detail_table <- renderDT({
    x <- active_snapshot()$kpi$indicatori$kpi_settimanali_canale_servizio
    x <- filter_kpi_rows(
      x,
      input$kpi_detail %||% "group",
      input$kpi_channel %||% "all"
    )
    validate(need(nrow(x) > 0, "Nessun dettaglio disponibile."))
    if ("servizio" %in% names(x)) x$servizio <- service_label(x$servizio)

    DT::datatable(
      format_display_df(x),
      rownames = FALSE,
      filter = "top",
      selection = "none",
      options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE)
    )
  }, server = FALSE)

  # ---------------------------------------------------------------------------
  # CRM
  # ---------------------------------------------------------------------------

  crm_obj <- reactive({
    x <- active_snapshot()$crm
    validate(need(!is.null(x), "Analisi CRM non disponibile nello snapshot."))
    x
  })

  output$crm_cards <- renderUI({
    x <- active_snapshot()
    cases <- crm_metric(x, "Casi distinti")
    contacts <- crm_metric(x, "Contatti agganciati ai casi del periodo")
    mean_contacts <- crm_metric(x, "Contatti medi per caso")
    rec_pct <- crm_metric(x, "Quota casi con ricontatto")
    minors_pct <- crm_metric(x, "Quota casi con minori coinvolti")
    external_pct <- crm_metric(x, "Quota casi con servizi esterni attivati")

    div(
      class = "kpi-grid",
      kpi_card("Casi distinti", fmt_int(cases), emphasis = TRUE),
      kpi_card("Contatti agganciati", fmt_int(contacts)),
      kpi_card("Contatti medi per caso", fmt_num(mean_contacts, 2L)),
      kpi_card("Casi con ricontatto", fmt_pct_ratio(rec_pct)),
      kpi_card("Casi con minori coinvolti", fmt_pct_ratio(minors_pct)),
      kpi_card("Casi con servizi esterni", fmt_pct_ratio(external_pct))
    )
  })

  output$crm_daily <- renderPlot({
    p <- plot_crm_daily(crm_obj())
    validate(need(!is.null(p), "Dati CRM giornalieri non disponibili."))
    p
  }, res = 110)

  output$crm_overview <- renderDT({
    x <- crm_obj()$tabelle$tab_overview
    validate(need(!is.null(x) && nrow(x) > 0, "Quadro CRM non disponibile."))
    DT::datatable(
      format_display_df(x),
      rownames = FALSE,
      selection = "none",
      options = list(dom = "t", pageLength = 20, scrollX = TRUE)
    )
  }, server = FALSE)

  output$crm_direction <- renderPlot({
    p <- plot_distribution(crm_obj()$tabelle$tab_contatti_direzione, "Direzione dei contatti")
    validate(need(!is.null(p), "Dati non disponibili."))
    p
  }, res = 110)

  output$crm_closure <- renderPlot({
    p <- plot_distribution(crm_obj()$tabelle$tab_stato_chiusura, "Stato di chiusura")
    validate(need(!is.null(p), "Dati non disponibili."))
    p
  }, res = 110)

  output$crm_contact_detail <- renderDT({
    nm <- input$crm_contact_detail_select %||% "tab_contatti_tipo"
    x <- crm_obj()$tabelle[[nm]]
    validate(need(!is.null(x) && nrow(x) > 0, "Tabella non disponibile."))
    DT::datatable(
      format_display_df(x), rownames = FALSE, filter = "top", selection = "none",
      options = list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE)
    )
  }, server = FALSE)

  output$crm_area <- renderPlot({
    p <- plot_distribution(crm_obj()$tabelle$tab_area_primaria, "Area primaria", top_n = 14L)
    validate(need(!is.null(p), "Dati non disponibili."))
    p
  }, res = 110)

  output$crm_category <- renderPlot({
    p <- plot_distribution(crm_obj()$tabelle$tab_categoria_primaria, "Categoria primaria", top_n = 14L)
    validate(need(!is.null(p), "Dati non disponibili."))
    p
  }, res = 110)

  output$crm_area_detail <- renderDT({
    nm <- input$crm_area_detail_select %||% "tab_elemento_primario"
    x <- crm_obj()$tabelle[[nm]]
    validate(need(!is.null(x) && nrow(x) > 0, "Tabella non disponibile."))
    DT::datatable(
      format_display_df(x), rownames = FALSE, filter = "top", selection = "none",
      options = list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE)
    )
  }, server = FALSE)

  output$crm_minor_age <- renderPlot({
    p <- plot_distribution(crm_obj()$tabelle$tab_eta_minori, "Età dei minori", top_n = 12L)
    validate(need(!is.null(p), "Dati non disponibili."))
    p
  }, res = 110)

  output$crm_caller_relation <- renderPlot({
    p <- plot_distribution(crm_obj()$tabelle$tab_chiamante_rapporto, "Rapporto del chiamante", top_n = 12L)
    validate(need(!is.null(p), "Dati non disponibili."))
    p
  }, res = 110)

  output$crm_profile_table <- renderDT({
    nm <- input$crm_profile_select %||% "tab_chiamante_eta"
    x <- crm_obj()$tabelle[[nm]]
    validate(need(!is.null(x) && nrow(x) > 0, "Tabella non disponibile."))
    DT::datatable(
      format_display_df(x), rownames = FALSE, filter = "top", selection = "none",
      options = list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE)
    )
  }, server = FALSE)

  output$crm_services_yesno <- renderPlot({
    p <- plot_distribution(crm_obj()$tabelle$tab_servizi_attivati, "Presenza di servizi esterni attivati")
    validate(need(!is.null(p), "Dati non disponibili."))
    p
  }, res = 110)

  output$crm_service_type <- renderPlot({
    p <- plot_distribution(crm_obj()$tabelle$tab_tipologia_servizi, "Tipologia di servizio", top_n = 12L)
    validate(need(!is.null(p), "Dati non disponibili."))
    p
  }, res = 110)

  output$crm_service_structures <- renderDT({
    x <- crm_obj()$tabelle$tab_strutture_servizi
    validate(need(!is.null(x) && nrow(x) > 0, "Tabella non disponibile."))
    DT::datatable(
      format_display_df(x), rownames = FALSE, filter = "top", selection = "none",
      options = list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE)
    )
  }, server = FALSE)

  output$crm_regions <- renderPlot({
    p <- plot_distribution(crm_obj()$tabelle$tab_regione_caso_top, "Prime regioni", top_n = 13L)
    validate(need(!is.null(p), "Dati non disponibili."))
    p
  }, res = 110)

  output$crm_countries <- renderDT({
    x <- crm_obj()$tabelle$tab_nazione_caso
    validate(need(!is.null(x) && nrow(x) > 0, "Tabella non disponibile."))
    DT::datatable(
      format_display_df(x), rownames = FALSE, filter = "top", selection = "none",
      options = list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE)
    )
  }, server = FALSE)

  output$crm_provinces <- renderDT({
    x <- crm_obj()$tabelle$tab_provincia_caso
    validate(need(!is.null(x) && nrow(x) > 0, "Tabella non disponibile."))
    DT::datatable(
      format_display_df(x), rownames = FALSE, filter = "top", selection = "none",
      options = list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE)
    )
  }, server = FALSE)

  # ---------------------------------------------------------------------------
  # OPERATORI - QUADRO GENERALE E ATTIVITA
  # ---------------------------------------------------------------------------

  output$operator_cards <- renderUI({
    ops <- operator_totals(active_snapshot())
    div(
      class = "kpi-grid",
      kpi_card("Ore di login", fmt_hours(ops$ore), emphasis = TRUE),
      kpi_card("Eventi gestiti", fmt_int(ops$contatti)),
      kpi_card("Gestite / ora", fmt_num(ops$gestite_ora, 2L)),
      kpi_card("Attivazioni", fmt_int(ops$attivazioni)),
      kpi_card("Tempo medio chiamata", fmt_duration(ops$tempo_chiamata)),
      kpi_card("Tempo medio chat", fmt_duration(ops$tempo_chat))
    )
  })

  output$operator_overview_table <- renderDT({
    x <- operator_overview_data(active_snapshot())
    validate(need(nrow(x) > 0, "Dati operatori non disponibili."))

    display <- x
    names(display) <- c(
      "Operatore", "Ore login", "Eventi log", "Gestite / ora", "Attivazioni",
      "Attivazioni / ora", "Casi CRM", "Contatti CRM", "Contatti / caso",
      "Ricontatto", "Progressivi qualità", "Con mancanti", "% con mancanti"
    )

    dt <- DT::datatable(
      display,
      rownames = FALSE,
      filter = "top",
      selection = "none",
      options = list(pageLength = 25, scrollX = TRUE, autoWidth = TRUE)
    )

    for (cc in intersect(c("Ore login", "Gestite / ora", "Attivazioni / ora", "Contatti / caso"), names(display))) {
      dt <- DT::formatRound(dt, columns = cc, digits = 2, dec.mark = ",")
    }
    if ("Ricontatto" %in% names(display)) {
      dt <- DT::formatPercentage(dt, columns = "Ricontatto", digits = 1)
    }
    if ("% con mancanti" %in% names(display)) {
      dt <- DT::formatRound(dt, columns = "% con mancanti", digits = 1, dec.mark = ",")
      dt <- DT::formatStyle(
        dt,
        columns = c("Operatore", "% con mancanti"),
        valueColumns = "% con mancanti",
        backgroundColor = DT::styleInterval(
          c(SOGLIA_GIALLA - 1e-8, SOGLIA_ROSSA - 1e-8),
          c("transparent", COLORE_GIALLO, COLORE_ROSSO)
        ),
        fontWeight = DT::styleInterval(
          c(SOGLIA_GIALLA - 1e-8, SOGLIA_ROSSA - 1e-8),
          c("normal", "600", "700")
        )
      )
    }
    dt
  }, server = FALSE)

  output$operator_activity_table <- renderDT({
    x <- active_snapshot()$operatori$attivita$settimanali
    validate(need(!is.null(x) && nrow(x) > 0, "Indicatori operatori non disponibili."))

    keep <- c(
      "operatore", "ore_login", "contatti_gestiti", "gestite_telefono",
      "gestite_chat", "gestite_whatsapp", "gestite_altro", "gestite_ora",
      "attivazioni", "attivazioni_ora", "tempo_medio_chiamata_hhmmss",
      "tempo_medio_chat_hhmmss", "bassa_esposizione_ore"
    )
    keep <- intersect(keep, names(x))
    x <- x[, keep, drop = FALSE]

    DT::datatable(
      format_display_df(x),
      rownames = FALSE,
      filter = "top",
      selection = "none",
      options = list(pageLength = 25, scrollX = TRUE, autoWidth = TRUE)
    )
  }, server = FALSE)

  output$operator_coverage_table <- renderDT({
    x <- operator_coverage_summary(active_snapshot())
    DT::datatable(
      x,
      rownames = FALSE,
      selection = "none",
      options = list(dom = "t", pageLength = 20, scrollX = TRUE)
    )
  }, server = FALSE)

  output$operator_time_table <- renderDT({
    x <- active_snapshot()$operatori$attivita$tempi_gestione_giornalieri
    validate(need(!is.null(x) && nrow(x) > 0, "Tempi di gestione non disponibili."))

    keep <- c(
      "giorno", "operatore", "chiamate_gestite", "tempo_medio_chiamata_hhmmss",
      "chat_gestite", "tempo_medio_chat_hhmmss"
    )
    keep <- intersect(keep, names(x))
    x <- x[, keep, drop = FALSE]

    DT::datatable(
      format_display_df(x),
      rownames = FALSE,
      filter = "top",
      selection = "none",
      options = list(pageLength = 25, scrollX = TRUE, autoWidth = TRUE)
    )
  }, server = FALSE)

  # ---------------------------------------------------------------------------
  # OPERATORI - CRM
  # ---------------------------------------------------------------------------

  output$operator_crm_table <- renderDT({
    nm <- input$operator_crm_table_select %||% "tab_volumi_operatore"
    x <- active_snapshot()$operatori$crm$tabelle[[nm]]
    validate(need(!is.null(x) && nrow(x) > 0, "Tabella CRM operatori non disponibile."))

    # Nei volumi settimanali queste due colonne misurano lo stesso insieme di
    # casi; ne mostriamo una sola per evitare ridondanze.
    if (identical(nm, "tab_volumi_operatore") &&
        all(c("n_casi_con_almeno_un_contatto", "n_casi_distinti_con_contatto") %in% names(x))) {
      x$n_casi_distinti_con_contatto <- NULL
    }

    # Le tabelle di ricontatto mantengono le due percentuali numeriche perché
    # hanno denominatori diversi (casi con contatto vs tutti i casi) e possono
    # divergere in settimane con casi privi di contatti. Le rispettive colonne
    # *_label vengono eliminate automaticamente da format_display_df().
    DT::datatable(
      format_display_df(x),
      rownames = FALSE,
      filter = "top",
      selection = "none",
      options = list(pageLength = 25, scrollX = TRUE, autoWidth = TRUE)
    )
  }, server = FALSE)

  # ---------------------------------------------------------------------------
  # OPERATORI - COMPLETEZZA
  # ---------------------------------------------------------------------------

  observeEvent(active_snapshot(), {
    snap <- active_snapshot()
    d <- snap$qualita$completezza$operatori$dettaglio

    if (!is.null(d) && nrow(d)) {
      ops <- unique(as.character(d$Operatore_creazione_caso))
      ops <- ops[!is.na(ops) & nzchar(trimws(ops))]
      labels <- clean_operator_label(ops)
      updateSelectInput(
        session,
        "op_quality_detail",
        choices = stats::setNames(ops, labels),
        selected = if (length(ops)) ops[1L] else character()
      )

      tabs <- sort(unique(as.character(d$Tabella)))
      tabs <- tabs[!is.na(tabs) & nzchar(trimws(tabs))]
      updateSelectInput(
        session,
        "op_quality_section",
        choices = c("Tutte", tabs),
        selected = "Tutte"
      )
    } else {
      updateSelectInput(session, "op_quality_detail", choices = character(), selected = character())
      updateSelectInput(session, "op_quality_section", choices = "Tutte", selected = "Tutte")
    }

    v <- snap$qualita$completezza$variabili
    if (!is.null(v) && nrow(v)) {
      tabs <- sort(unique(as.character(v$Tabella)))
      tabs <- tabs[!is.na(tabs) & nzchar(trimws(tabs))]
      updateSelectInput(
        session,
        "quality_section_filter",
        choices = c("Tutte", tabs),
        selected = "Tutte"
      )
    } else {
      updateSelectInput(session, "quality_section_filter", choices = "Tutte", selected = "Tutte")
    }
  }, ignoreInit = FALSE)

  output$operator_quality_summary <- renderDT({
    x <- operator_quality_summary(
      active_snapshot(),
      include_special = isTRUE(input$op_quality_special)
    )
    validate(need(nrow(x) > 0, "Nessun dato di completezza per operatore disponibile."))

    keep <- c(
      "Operatore_creazione_caso", "Progressivi_nel_perimetro", "Schede_Caso_disponibili",
      "Contatti_nel_periodo", "Progressivi_con_almeno_un_dato_valutabile",
      "Progressivi_con_almeno_un_mancante", "Percentuale_progressivi_con_mancanti",
      "Coppie_progressivo_variabile_con_mancanti"
    )
    keep <- intersect(keep, names(x))
    x <- x[, keep, drop = FALSE]

    rename <- c(
      Operatore_creazione_caso = "Operatore",
      Progressivi_nel_perimetro = "Progressivi",
      Schede_Caso_disponibili = "Schede Caso",
      Contatti_nel_periodo = "Contatti",
      Progressivi_con_almeno_un_dato_valutabile = "Progressivi valutabili",
      Progressivi_con_almeno_un_mancante = "Progressivi con almeno un mancante",
      Percentuale_progressivi_con_mancanti = "% con almeno un mancante",
      Coppie_progressivo_variabile_con_mancanti = "Coppie progressivo-variabile con mancanti"
    )
    names(x) <- unname(rename[names(x)])

    dt <- DT::datatable(
      x,
      rownames = FALSE,
      filter = "top",
      selection = "none",
      options = list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE)
    )
    dt <- DT::formatRound(dt, columns = "% con almeno un mancante", digits = 1, dec.mark = ",")
    dt <- DT::formatStyle(
      dt,
      columns = c("Operatore", "% con almeno un mancante"),
      valueColumns = "% con almeno un mancante",
      backgroundColor = DT::styleInterval(
        c(SOGLIA_GIALLA - 1e-8, SOGLIA_ROSSA - 1e-8),
        c("transparent", COLORE_GIALLO, COLORE_ROSSO)
      ),
      fontWeight = DT::styleInterval(
        c(SOGLIA_GIALLA - 1e-8, SOGLIA_ROSSA - 1e-8),
        c("normal", "600", "700")
      )
    )
    dt
  }, server = FALSE)

  selected_operator_matrix <- reactive({
    x <- active_snapshot()$qualita$completezza$operatori[[input$op_quality_metric %||% "mancanti"]]
    validate(need(!is.null(x) && nrow(x) > 0, "Matrice non disponibile."))
    if (!isTRUE(input$op_quality_special) && "Tipo_riga" %in% names(x)) {
      x <- x[x$Tipo_riga == "OPERATORE", , drop = FALSE]
    }
    x$Operatore_creazione_caso <- clean_operator_label(x$Operatore_creazione_caso)
    x
  })

  output$operator_quality_matrix <- renderDT({
    x <- selected_operator_matrix()
    metrica <- input$op_quality_metric %||% "mancanti"

    metadata_originali <- c(
      "Servizio", "Tipo_riga", "Operatore_creazione_caso",
      "Progressivi_nel_perimetro", "Schede_Caso_disponibili", "Contatti_nel_periodo"
    )
    variabili_matrix <- setdiff(names(x), metadata_originali)
    pct_map <- character()

    if (metrica %in% c("mancanti", "vuoti", "non_noti")) {
      den <- active_snapshot()$qualita$completezza$operatori$denominatori
      if (!isTRUE(input$op_quality_special) && "Tipo_riga" %in% names(den)) {
        den <- den[den$Tipo_riga == "OPERATORE", , drop = FALSE]
      }
      den$Operatore_creazione_caso <- clean_operator_label(den$Operatore_creazione_caso)
      j <- match(as.character(x$Operatore_creazione_caso), as.character(den$Operatore_creazione_caso))

      for (ii in seq_along(variabili_matrix)) {
        v <- variabili_matrix[ii]
        if (!v %in% names(den)) next
        pct_name <- paste0(".__pct_", ii)
        num <- safe_num(x[[v]])
        dd <- safe_num(den[[v]][j])
        x[[pct_name]] <- ifelse(!is.na(dd) & dd > 0, 100 * num / dd, NA_real_)
        pct_map[v] <- pct_name
      }
    }

    names(x)[names(x) == "Operatore_creazione_caso"] <- "Operatore"
    names(x)[names(x) == "Progressivi_nel_perimetro"] <- "Progressivi"
    names(x)[names(x) == "Schede_Caso_disponibili"] <- "Schede Caso"
    names(x)[names(x) == "Contatti_nel_periodo"] <- "Contatti"

    hidden_cols <- grep("^\\.__pct_", names(x))
    col_defs <- list()
    if (length(hidden_cols)) {
      col_defs <- list(list(visible = FALSE, targets = hidden_cols - 1L))
    }

    dt <- DT::datatable(
      x,
      rownames = FALSE,
      filter = "top",
      selection = "none",
      extensions = "FixedColumns",
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        scrollY = "520px",
        scrollCollapse = TRUE,
        fixedColumns = list(leftColumns = min(6L, ncol(x) - length(hidden_cols))),
        autoWidth = TRUE,
        columnDefs = col_defs
      )
    )

    if (length(pct_map)) {
      cuts <- c(SOGLIA_GIALLA - 1e-8, SOGLIA_ROSSA - 1e-8)
      for (v in names(pct_map)) {
        dt <- DT::formatStyle(
          dt,
          columns = v,
          valueColumns = pct_map[[v]],
          backgroundColor = DT::styleInterval(cuts, c("transparent", COLORE_GIALLO, COLORE_ROSSO)),
          fontWeight = DT::styleInterval(cuts, c("normal", "600", "700")),
          color = DT::styleInterval(cuts, c("inherit", "#664D03", COLORE_TESTO_ROSSO))
        )
      }
    }
    dt
  }, server = FALSE)

  output$operator_quality_detail_table <- renderDT({
    req(input$op_quality_detail)
    x <- active_snapshot()$qualita$completezza$operatori$dettaglio
    validate(need(!is.null(x) && nrow(x) > 0, "Dettaglio completezza per operatore non disponibile."))
    validate(need("Operatore_creazione_caso" %in% names(x), "Colonna operatore non disponibile nel dettaglio completezza."))
    x <- x[x$Operatore_creazione_caso == input$op_quality_detail, , drop = FALSE]
    validate(need(nrow(x) > 0, "Nessun dettaglio disponibile per l'operatore selezionato."))
    if (!is.null(input$op_quality_section) && input$op_quality_section != "Tutte") {
      x <- x[x$Tabella == input$op_quality_section, , drop = FALSE]
    }

    keep <- c(
      "Tabella", "Variabile", "Progressivi_valutabili", "Progressivi_con_mancanti",
      "Percentuale_progressivi_con_mancanti", "Progressivi_con_vuoti",
      "Progressivi_con_non_noti", "Mancanza_totale_tra_record_valutabili",
      "Mancanza_parziale", "Record_valutabili", "Record_mancanti",
      "Percentuale_record_mancanti"
    )
    keep <- intersect(keep, names(x))
    x <- x[, keep, drop = FALSE]

    if ("Percentuale_progressivi_con_mancanti" %in% names(x)) {
      x <- x[
        order(safe_num(x$Percentuale_progressivi_con_mancanti), decreasing = TRUE, na.last = TRUE),
        ,
        drop = FALSE
      ]
    }

    rename_map <- c(
      Tabella = "Sezione",
      Variabile = "Variabile",
      Progressivi_valutabili = "Progressivi valutabili",
      Progressivi_con_mancanti = "Progressivi con mancanti",
      Percentuale_progressivi_con_mancanti = "% progressivi con mancanti",
      Progressivi_con_vuoti = "Con campi vuoti",
      Progressivi_con_non_noti = "Con NON_NOTO",
      Mancanza_totale_tra_record_valutabili = "Mancanza totale",
      Mancanza_parziale = "Mancanza parziale",
      Record_valutabili = "Record valutabili",
      Record_mancanti = "Record mancanti",
      Percentuale_record_mancanti = "% record mancanti"
    )
    names(x) <- unname(rename_map[names(x)])

    dt <- DT::datatable(
      x,
      rownames = FALSE,
      filter = "top",
      selection = "none",
      options = list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE)
    )

    pct_cols <- grep("^%", names(x), value = TRUE)
    for (cc in pct_cols) {
      dt <- DT::formatRound(dt, columns = cc, digits = 1, dec.mark = ",")
    }

    pct_main <- "% progressivi con mancanti"
    if (pct_main %in% names(x)) {
      cuts <- c(SOGLIA_GIALLA - 1e-8, SOGLIA_ROSSA - 1e-8)
      cols_attention <- intersect(c("Variabile", "Progressivi con mancanti", pct_main), names(x))
      if (length(cols_attention)) {
        dt <- DT::formatStyle(
          dt,
          columns = cols_attention,
          valueColumns = pct_main,
          backgroundColor = DT::styleInterval(cuts, c("transparent", COLORE_GIALLO, COLORE_ROSSO)),
          fontWeight = DT::styleInterval(cuts, c("normal", "600", "700")),
          color = DT::styleInterval(cuts, c("inherit", "#664D03", COLORE_TESTO_ROSSO))
        )
      }
    }
    dt
  }, server = FALSE)

  # ---------------------------------------------------------------------------
  # QUALITA DATI - SENZA DUPLICARE LA VISTA OPERATORI
  # ---------------------------------------------------------------------------

  quality_obj <- reactive({
    x <- active_snapshot()$qualita$completezza
    validate(need(!is.null(x), "Analisi completezza non disponibile."))
    validate(need(is.null(x$errore), paste("Errore completezza:", x$errore %||% "")))
    x
  })

  output$quality_cards <- renderUI({
    x <- active_snapshot()
    q <- quality_obj()
    progressivi <- quality_indicator(x, "Progressivi unici nella tabella completa", TRUE)
    schede <- quality_indicator(x, "Schede Caso presenti nel perimetro", TRUE)
    missing <- quality_indicator(
      x,
      "Schede valutate con almeno una variabile di raccolta/attribuzione mancante",
      TRUE
    )
    no_prog <- quality_indicator(x, "Contatti senza progressivo nel periodo", TRUE)
    missing_pct <- if (!is.na(schede) && schede > 0 && !is.na(missing)) 100 * missing / schede else NA_real_
    n_vars <- if (!is.null(q$variabili) && "Progressivi_con_mancanti" %in% names(q$variabili)) {
      sum(safe_num(q$variabili$Progressivi_con_mancanti) > 0, na.rm = TRUE)
    } else {
      NA_real_
    }
    progressivi_verifica <- nrow(q$progressivi_mancanti %||% data.frame())

    div(
      class = "kpi-grid",
      kpi_card("Progressivi nel perimetro", fmt_int(progressivi), emphasis = TRUE),
      kpi_card("Schede Caso disponibili", fmt_int(schede)),
      kpi_card("Schede con almeno un mancante", fmt_int(missing), paste0(fmt_pct_100(missing_pct), " delle schede")),
      kpi_card("Progressivi da verificare", fmt_int(progressivi_verifica)),
      kpi_card("Variabili con almeno un mancante", fmt_int(n_vars)),
      kpi_card("Contatti senza progressivo", fmt_int(no_prog))
    )
  })

  output$quality_summary <- renderDT({
    x <- quality_obj()$riepilogo
    validate(need(!is.null(x) && nrow(x) > 0, "Sintesi non disponibile."))

    drop_rows <- c(
      "Periodo richiesto da", "Periodo richiesto a",
      "Data minima Caso nell'export", "Data massima Caso nell'export",
      "Data minima contatti nell'export", "Data massima contatti nell'export"
    )
    x <- x[!x$Indicatore %in% drop_rows, , drop = FALSE]

    DT::datatable(
      x,
      rownames = FALSE,
      selection = "none",
      options = list(dom = "t", pageLength = 30, scrollX = TRUE)
    )
  }, server = FALSE)

  output$quality_sections <- renderDT({
    x <- quality_obj()$sezioni
    validate(need(!is.null(x) && nrow(x) > 0, "Copertura sezioni non disponibile."))
    DT::datatable(
      format_display_df(x),
      rownames = FALSE,
      selection = "none",
      options = list(dom = "t", pageLength = 20, scrollX = TRUE)
    )
  }, server = FALSE)

  output$quality_variables <- renderDT({
    x <- quality_obj()$variabili
    validate(need(!is.null(x) && nrow(x) > 0, "Nessuna variabile disponibile."))

    if (!is.null(input$quality_section_filter) && input$quality_section_filter != "Tutte") {
      x <- x[x$Tabella == input$quality_section_filter, , drop = FALSE]
    }
    if (isTRUE(input$quality_only_missing) && "Progressivi_con_mancanti" %in% names(x)) {
      x <- x[safe_num(x$Progressivi_con_mancanti) > 0, , drop = FALSE]
    }

    core_cols <- c(
      "Tabella", "Variabile", "Progressivi_valutabili", "Progressivi_con_mancanti",
      "Percentuale_progressivi_con_mancanti", "Mancanza_totale_tra_record_valutabili",
      "Mancanza_parziale", "Progressivi_senza_record"
    )
    record_cols <- c(
      "Record_valutabili", "Record_vuoti", "Record_non_noti", "Record_mancanti",
      "Percentuale_record_mancanti"
    )
    keep <- core_cols
    if (isTRUE(input$quality_show_records)) keep <- c(keep, record_cols)
    keep <- intersect(keep, names(x))
    x <- x[, keep, drop = FALSE]

    if ("Percentuale_progressivi_con_mancanti" %in% names(x)) {
      x <- x[
        order(safe_num(x$Percentuale_progressivi_con_mancanti), decreasing = TRUE, na.last = TRUE),
        ,
        drop = FALSE
      ]
    }

    rename_map <- c(
      Tabella = "Sezione",
      Variabile = "Variabile",
      Progressivi_valutabili = "Progressivi valutabili",
      Progressivi_con_mancanti = "Progressivi con mancanti",
      Percentuale_progressivi_con_mancanti = "% progressivi con mancanti",
      Mancanza_totale_tra_record_valutabili = "Mancanza totale",
      Mancanza_parziale = "Mancanza parziale",
      Progressivi_senza_record = "Progressivi senza record",
      Record_valutabili = "Record valutabili",
      Record_vuoti = "Record vuoti",
      Record_non_noti = "Record NON_NOTO",
      Record_mancanti = "Record mancanti",
      Percentuale_record_mancanti = "% record mancanti"
    )
    names(x) <- unname(rename_map[names(x)])

    dt <- DT::datatable(
      x,
      rownames = FALSE,
      filter = "top",
      selection = "none",
      options = list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE)
    )

    pct_cols <- grep("^%", names(x), value = TRUE)
    for (cc in pct_cols) {
      dt <- DT::formatRound(dt, columns = cc, digits = 1, dec.mark = ",")
    }

    pct_main <- "% progressivi con mancanti"
    if (pct_main %in% names(x)) {
      cuts <- c(SOGLIA_GIALLA - 1e-8, SOGLIA_ROSSA - 1e-8)
      cols_attention <- intersect(c("Variabile", "Progressivi con mancanti", pct_main), names(x))
      if (length(cols_attention)) {
        dt <- DT::formatStyle(
          dt,
          columns = cols_attention,
          valueColumns = pct_main,
          backgroundColor = DT::styleInterval(cuts, c("transparent", COLORE_GIALLO, COLORE_ROSSO)),
          fontWeight = DT::styleInterval(cuts, c("normal", "600", "700")),
          color = DT::styleInterval(cuts, c("inherit", "#664D03", COLORE_TESTO_ROSSO))
        )
      }
    }
    dt
  }, server = FALSE)

  output$quality_progressivi <- renderDT({
    x <- prepare_progressivi_table(active_snapshot())
    validate(need(nrow(x) > 0, "Nessun progressivo con dati mancanti nello snapshot selezionato."))

    dt <- DT::datatable(
      x,
      rownames = FALSE,
      filter = "top",
      selection = "none",
      options = list(
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )

    if ("N. variabili mancanti" %in% names(x)) {
      dt <- DT::formatStyle(dt, columns = "N. variabili mancanti", fontWeight = "700", color = "#25364A")
    }
    if ("Dove mancano dati" %in% names(x)) {
      dt <- DT::formatStyle(dt, columns = "Dove mancano dati", whiteSpace = "normal", minWidth = "360px")
    }
    if ("Sezioni senza record" %in% names(x)) {
      dt <- DT::formatStyle(dt, columns = "Sezioni senza record", whiteSpace = "normal", minWidth = "260px")
    }
    dt
  }, server = FALSE)

  output$quality_controls <- renderDT({
    x <- quality_obj()$controlli_qualita
    validate(need(!is.null(x) && nrow(x) > 0, "Controlli completezza non disponibili."))
    DT::datatable(
      format_display_df(x),
      rownames = FALSE,
      filter = "top",
      selection = "none",
      options = list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE)
    )
  }, server = FALSE)

  output$quality_kpi_controls <- renderDT({
    x <- active_snapshot()$qualita$controlli_kpi$controllo_totali
    validate(need(!is.null(x) && nrow(x) > 0, "Controlli KPI non disponibili."))
    if ("servizio" %in% names(x)) x$servizio <- service_label(x$servizio)
    DT::datatable(
      format_display_df(x),
      rownames = FALSE,
      selection = "none",
      options = list(dom = "t", pageLength = 20, scrollX = TRUE)
    )
  }, server = FALSE)

  output$quality_log_kpi <- renderDT({
    x <- active_snapshot()$qualita$controlli_kpi$controllo_contatti_log_vs_kpi
    validate(need(!is.null(x) && nrow(x) > 0, "Confronto log/KPI non disponibile."))
    DT::datatable(
      format_display_df(x),
      rownames = FALSE,
      filter = "top",
      selection = "none",
      options = list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE)
    )
  }, server = FALSE)

  # ---------------------------------------------------------------------------
  # METODO
  # ---------------------------------------------------------------------------

  output$method_notes <- renderUI({
    x <- active_snapshot()$metodo
    if (is.null(x) || !length(x)) return(div("Note metodologiche non disponibili."))

    labels <- c(
      kpi_coda = "KPI di coda",
      whatsapp = "WhatsApp",
      crm = "CRM",
      operatori_log = "Operatori - log operativo",
      operatori_casi = "Operatori - casi CRM",
      completezza = "Completezza CRM"
    )

    tags$ul(
      class = "method-list",
      lapply(names(x), function(nm) {
        tags$li(tags$strong(labels[[nm]] %||% nm), ": ", as.character(x[[nm]]))
      })
    )
  })

  output$metadata_table <- renderDT({
    x <- active_snapshot()
    md <- x$metadata

    z <- data.frame(
      Voce = c(
        "Schema snapshot", "Versione schema", "Servizio", "Settimana", "Settimana ISO",
        "Periodo", "Regola calendario", "Creato il", "Mappatura operatori"
      ),
      Valore = c(
        x$schema_snapshot,
        as.character(x$versione_schema),
        SERVIZIO_LABEL,
        as.character(md$settimana_id),
        as.character(md$settimana_iso),
        fmt_week_it(md$data_inizio, md$data_fine),
        as.character(md$regola_settimana),
        format(as.POSIXct(md$creato_il), "%d/%m/%Y %H:%M:%S"),
        as.character(md$mappatura_operatori %||% "–")
      ),
      stringsAsFactors = FALSE
    )

    if (SERVIZIO_APP == "114" && !is.null(md$multilingua_114)) {
      z <- rbind(
        z,
        data.frame(
          Voce = "114 Multilingua",
          Valore = as.character(md$multilingua_114),
          stringsAsFactors = FALSE
        )
      )
    }

    DT::datatable(
      z,
      rownames = FALSE,
      selection = "none",
      options = list(dom = "t", pageLength = 20, scrollX = TRUE)
    )
  }, server = FALSE)
}

shinyApp(ui, server)
