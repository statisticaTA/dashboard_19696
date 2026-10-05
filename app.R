###############################################################################
# DASHBOARD SETTIMANALE PER SERVIZIO - V1.1 (TREND E CONFRONTI)
# Fondazione S.O.S. Il Telefono Azzurro ETS
#
# Un'unica codebase per due app separate:
#   - 114 Emergenza Infanzia
#   - 1.96.96
#
# Aggiunta V1.1: storico aggregato, trend e confronti esplorativi.
# Le sezioni settimanali e il formato degli snapshot restano invariati.
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

# =============================================================================
# TREND E CONFRONTI - V1.1
# Modulo autonomo: non modifica gli snapshot ne' le viste settimanali esistenti.
# stats::cor(method = "spearman") e' usato solo come descrizione esplorativa.
# Riferimenti: R stats/cor; Shiny moduleServer/testServer (documentazione ufficiale).
# =============================================================================

TREND_MIN_PAIRS <- 8L
TREND_CAUTION_PAIRS <- 12L

trend_catalog <- local({
  out <- list()
  add <- function(id, family, label, source, modes, default = modes[1L],
                  table = NULL, column = NULL, numerator = NULL,
                  denominator = NULL, dimension = NULL, note = "",
                  additive = TRUE) {
    out[[id]] <<- list(id = id, family = family, label = label, source = source,
      modes = modes, default = default, table = table, column = column,
      numerator = numerator, denominator = denominator, dimension = dimension,
      note = note, additive = additive)
  }
  kn <- c(kpi_received = "Conversazioni ricevute", kpi_handled = "Conversazioni gestite",
          kpi_abandoned = "Conversazioni abbandonate", kpi_unhandled = "Conversazioni non gestite",
          kpi_service = "Livello di servizio", kpi_abandon_rate = "Tasso di abbandono",
          kpi_unhandled_rate = "Tasso di non gestite")
  kc <- c("conversazioni_totali", "gestite", "abbandonate", "non_gestite",
          "gestite", "abbandonate", "non_gestite")
  for (i in seq_along(kn)) {
    add(names(kn)[i], "KPI di servizio", unname(kn[i]), "kpi",
        if (i > 4L) "pct" else "n", column = kc[i],
        denominator = if (i > 4L) "conversazioni_totali" else NULL,
        note = paste("KPI di coda: conversazioni, non persone uniche.",
          "Per WhatsApp abbandono non applicabile; timeout = non gestite.",
          "Nel totale multicanale il denominatore resta quello della dashboard settimanale."))
  }
  add("crm_cases", "Volumi CRM", "Casi CRM aperti", "crm", "n",
      numerator = "Casi distinti")
  add("crm_contacts", "Volumi CRM", "Contatti agganciati ai casi aperti nella settimana", "crm", "n",
      numerator = "Contatti agganciati ai casi del periodo")
  add("crm_contacts_case", "Volumi CRM", "Contatti per caso", "crm", "ratio",
      numerator = "Contatti agganciati ai casi del periodo", denominator = "Casi distinti")
  cr <- list(
    crm_recontact = c("Casi con ricontatto", "Casi con ricontatto"),
    crm_minors = c("Casi con minori coinvolti", "Casi con minori coinvolti"),
    crm_immediate = c("Casi con intervento immediato", "Casi con intervento immediato"),
    crm_external = c("Casi con servizi esterni attivati", "Casi con servizi esterni attivati")
  )
  for (id in names(cr)) add(id, "Volumi CRM", cr[[id]][1], "crm", c("n", "pct"),
    default = "pct", numerator = cr[[id]][2], denominator = "Casi distinti",
    note = "Numeratore e denominatore riguardano i casi aperti nella settimana, non tutti i casi in gestione.")
  ds <- list(
    mix_contact_type = c("Tipo di contatto", "tab_contatti_tipo", "tipo", "n_contatti"),
    mix_direction = c("Direzione del contatto", "tab_contatti_direzione", "direzione", "n_contatti"),
    mix_outcome = c("Esito del contatto", "tab_contatti_esito", "esito", "n_contatti"),
    mix_closure = c("Stato di chiusura del caso", "tab_stato_chiusura", "stato_chiusura", "n_casi"),
    mix_frequency = c("Frequenza", "tab_frequenza", "frequenza", "n_casi"),
    mix_place = c("Luogo prevalente", "tab_luogo_prevalente", "luogo_prevalente", "n_casi"),
    mix_persistence = c("Da quanto persiste", "tab_da_quanto_persiste", "da_quanto_persiste", "n_casi"),
    mix_caller_age = c("Eta' prevalente del chiamante", "tab_chiamante_eta", "eta_chiamante_prevalente", "n_casi"),
    mix_caller_gender = c("Genere prevalente del chiamante", "tab_chiamante_genere", "genere_chiamante_prevalente", "n_casi"),
    mix_caller_relation = c("Rapporto prevalente del chiamante", "tab_chiamante_rapporto", "rapporto_chiamante_prevalente", "n_casi"),
    mix_anonymous = c("Chiamante anonimo", "tab_chiamante_anonimo", "anonimo_label", "n_casi"),
    mix_direct = c("Chiamante interessato diretto", "tab_chiamante_interessato", "interessato_label", "n_casi"),
    mix_minors_n = c("Numero di minori per caso", "tab_n_minori_per_caso", "classe_n_minori", "n_casi"),
    mix_minors_age = c("Fascia di eta' prevalente dei minori", "tab_eta_minori", "eta_classe_prevalente", "n_casi"),
    mix_minors_gender = c("Genere prevalente dei minori", "tab_genere_minori", "genere_minore_prevalente", "n_casi"),
    mix_living = c("Con chi vive il minore (prevalente)", "tab_con_chi_vive_minori", "con_chi_vive_prevalente", "n_casi"),
    mix_citizenship = c("Cittadinanza prevalente", "tab_cittadinanza_minori", "cittadinanza_prevalente", "n_casi"),
    mix_area = c("Area primaria", "tab_area_primaria", "area_contatto", "n_casi"),
    mix_category = c("Categoria primaria", "tab_categoria_primaria", "categoria_contatto", "n_casi"),
    mix_element = c("Elemento primario", "tab_elemento_primario", "elemento_contatto", "n_casi"),
    mix_resp_gender = c("Genere prevalente del responsabile", "tab_responsabile_genere", "responsabile_genere_prevalente", "n_casi"),
    mix_resp_relation = c("Rapporto prevalente del responsabile", "tab_responsabile_rapporto", "rapporto_responsabile_prevalente", "n_casi"),
    mix_resp_n = c("Numero di responsabili per caso", "tab_n_responsabili_per_caso", "classe_n_responsabili", "n_casi"),
    mix_service_type = c("Tipologia prevalente del servizio attivato", "tab_tipologia_servizi", "tipologia_servizio_prevalente", "n_casi"),
    mix_region = c("Regione del caso", "tab_regione_caso", "regione_caso", "n_casi")
  )
  for (id in names(ds)) {
    v <- ds[[id]]
    note <- if (v[4] == "n_contatti") {
      "Distribuzione dei contatti agganciati ai casi aperti nella settimana: non dei contatti CRM totali."
    } else {
      paste("Distribuzione a livello caso; le caratteristiche prevalenti non sono conteggi di persone.",
            "La percentuale usa tutti i casi della tabella, incluse le categorie non note.")
    }
    if (id %in% c("mix_area", "mix_category", "mix_element")) note <- paste(note,
      "Si considera solo la motivazione primaria, non tutte le motivazioni o le loro co-occorrenze.")
    if (id == "mix_closure") note <- paste(note,
      "Stato osservato nell'export usato per generare lo snapshot; non necessariamente alla chiusura della settimana.")
    if (id == "mix_service_type") note <- paste(note,
      "Non rappresenta il conteggio di tutti gli interventi; include le voci non indicate.")
    add(id, "Composizione CRM", v[1], "distribution", c("n", "pct"), "pct",
        table = v[2], dimension = v[3], column = v[4], note = note)
  }
  os <- list(
    ops_hours = c("Ore di login", "ore_login", "hours", ""),
    ops_events = c("Eventi di gestione da log", "contatti_gestiti", "n", ""),
    ops_events_hour = c("Eventi gestiti per ora", "contatti_gestiti", "ratio", "ore_login"),
    ops_activations = c("Attivazioni attribuite agli operatori del gruppo", "attivazioni", "n", ""),
    ops_activations_hour = c("Attivazioni per ora", "attivazioni", "ratio", "ore_login"),
    ops_call_time = c("Tempo medio chiamata", "tempo_totale_chiamate_secondi", "seconds", "chiamate_gestite_tempo"),
    ops_chat_time = c("Tempo medio chat", "tempo_totale_chat_secondi", "seconds", "chat_gestite_tempo"),
    ops_active = c("Operatori con ore, eventi o attivazioni", "operatori_attivi", "n", "")
  )
  for (id in names(os)) {
    v <- os[[id]]
    add(id, "Attivita' operativa", v[1], "ops", v[3], column = v[2],
        denominator = if (nzchar(v[4])) v[4] else NULL,
        additive = id != "ops_active",
        note = paste("Dati del gruppo operativo mappato: non ore per singola coda o canale.",
          "Eventi TALKING/INTERACTION, non contatti unici. Rapporti e tempi medi sulle somme, non sulle medie individuali."))
  }
  qrows <- list(
    q_progressivi = c("Progressivi nel perimetro di qualita'", "Progressivi unici nella tabella completa", ""),
    q_schede = c("Schede Caso presenti nel perimetro", "Schede Caso presenti nel perimetro", ""),
    q_cases = c("Casi aperti (perimetro audit)", "Casi aperti nel periodo", ""),
    q_contacts = c("Contatti CRM totali nella settimana", "Contatti CRM nel periodo (con e senza progressivo)", ""),
    q_no_progressivo = c("Contatti senza progressivo", "Contatti senza progressivo nel periodo", "Contatti CRM nel periodo (con e senza progressivo)"),
    q_cases_no_contacts = c("Casi aperti senza contatti nella settimana", "Casi aperti nel periodo senza contatti nel periodo", "Casi aperti nel periodo"),
    q_missing = c("Schede con almeno un dato mancante", "Schede valutate con almeno una variabile di raccolta/attribuzione mancante", "Schede Caso presenti nel perimetro")
  )
  for (sec in c("Chiamante", "Minori", "Motivazioni", "Responsabili", "Servizi")) {
    qrows[[paste0("q_no_", tolower(sec))]] <- c(paste("Schede senza record", sec),
      paste("Schede valutate senza record", sec), "Schede Caso presenti nel perimetro")
  }
  for (id in names(qrows)) {
    v <- qrows[[id]]; israte <- nzchar(v[3])
    add(id, "Qualita' dati", v[1], "quality", if (israte) c("n", "pct") else "n",
        default = if (israte) "pct" else "n", numerator = v[2],
        denominator = if (israte) v[3] else NULL,
        additive = id %in% c("q_cases", "q_contacts", "q_no_progressivo", "q_cases_no_contacts"),
        note = paste("Audit sui progressivi aperti o contattati, non solo sui nuovi casi CRM.",
          "Le schede possono ricomparire in settimane diverse: le somme non sono persone/casi unici del periodo.",
          "Assenza di una sezione e campo mancante sono concetti distinti; non ogni sezione e' obbligatoria."))
  }
  add("q_variable", "Qualita' dati", "Dati mancanti di una variabile", "quality_variable",
      c("pct_progressivi", "pct_record"), default = "pct_progressivi", additive = FALSE,
      note = paste("VUOTO + NON_NOTO, al netto delle regole di applicabilita'.",
        "Denominatore: progressivi o record valutabili per la variabile selezionata.",
        "L'assenza di una sezione non e' convertita in una percentuale di campi mancanti."))
  out
})

trend_mode_labels <- c(n = "Numero", pct = "Percentuale", ratio = "Rapporto",
  hours = "Ore", seconds = "Durata media", pct_progressivi = "% progressivi con mancanti",
  pct_record = "% record mancanti")

trend_metric <- function(id) {
  if (!is.character(id) || length(id) != 1L || !id %in% names(trend_catalog)) return(NULL)
  trend_catalog[[id]]
}
trend_cat_text <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | !nzchar(x)] <- "Non indicato"
  x
}
trend_quality_key <- function(x) paste(as.character(x$Tabella), as.character(x$Variabile), sep = " :: ")
trend_sum <- function(x) {
  x <- safe_num(x)
  if (!length(x) || any(!is.finite(x))) return(NA_real_)
  sum(x)
}
trend_single <- function(x) {
  x <- safe_num(x)
  if (length(x) != 1L || !is.finite(x)) NA_real_ else x
}
trend_col_sum <- function(tab, column) {
  if (!is.data.frame(tab) || !column %in% names(tab)) return(NA_real_)
  if (!nrow(tab)) return(0)
  trend_sum(tab[[column]])
}
trend_unit <- function(spec) {
  mode <- spec$mode
  if (mode %in% c("pct", "pct_progressivi", "pct_record")) "pct" else mode
}
trend_unit_label <- function(unit) switch(unit, n = "n.", hours = "ore", seconds = "secondi",
  pct = "%", ratio = "rapporto", "valore")
trend_format <- function(x, unit, signed = FALSE) {
  if (!length(x) || !is.finite(x[1L])) return("ND")
  x <- x[1L]
  sign <- if (signed && x > 0) "+" else ""
  if (unit == "seconds") {
    neg <- if (x < 0) "-" else sign
    return(paste0(neg, fmt_duration(abs(x))))
  }
  paste0(sign, switch(unit, n = fmt_int(x), pct = fmt_pct_ratio(x),
    hours = paste0(fmt_num(x, 2), " h"), ratio = fmt_num(x, 2), fmt_num(x, 2)))
}
trend_spec <- function(id, mode = NULL, category = "", detail = "group", channel = "all") {
  m <- trend_metric(id)
  if (is.null(m)) return(NULL)
  if (is.null(mode) || !mode %in% m$modes) mode <- m$default
  if (SERVIZIO_APP != "114") detail <- "group"
  if (!detail %in% c("group", "114", "114_ml")) detail <- "group"
  if (!channel %in% c("all", "Telefono", "Chat", "WhatsApp")) channel <- "all"
  if (m$source != "kpi") { detail <- "group"; channel <- "all" }
  if (!m$source %in% c("distribution", "quality_variable")) category <- ""
  list(id = id, mode = mode, category = as.character(category)[1L], detail = detail, channel = channel)
}
trend_spec_label <- function(spec) {
  if (is.null(spec)) return("Indicatore non selezionato")
  m <- trend_metric(spec$id)
  out <- m$label
  if (nzchar(spec$category)) out <- paste(out, spec$category, sep = " - ")
  if (m$source == "kpi") {
    if (SERVIZIO_APP == "114") out <- paste(out, switch(spec$detail,
      group = "114 incl. Multilingua", `114` = "114", `114_ml` = "114 Multilingua"), sep = " | ")
    out <- paste(out, if (spec$channel == "all") "Tutti i canali" else spec$channel, sep = " | ")
  }
  paste0(out, " (", trend_mode_labels[[spec$mode]], ")")
}

# Proiezione in memoria: lo storico conserva solo aggregati necessari ai trend.
# Non vengono copiati progressivi, nomi operatori o il dettaglio delle celle.
trend_project <- function(x) {
  allowed <- unique(c("tab_overview", vapply(Filter(function(m) m$source == "distribution", trend_catalog),
    function(m) m$table, character(1))))
  ct <- x$crm$tabelle
  ct <- ct[intersect(allowed, names(ct))]
  a <- x$operatori$attivita$settimanali
  cols <- c("ore_login", "contatti_gestiti", "attivazioni", "chiamate_gestite_tempo",
            "chat_gestite_tempo", "tempo_totale_chiamate_secondi", "tempo_totale_chat_secondi")
  ops <- setNames(lapply(cols, function(col) trend_col_sum(a, col)), cols)
  ops$operatori_attivi <- NA_real_
  if (is.data.frame(a) && all(c("agent_key", "ore_login", "contatti_gestiti", "attivazioni") %in% names(a))) {
    nums <- lapply(a[c("ore_login", "contatti_gestiti", "attivazioni")], safe_num)
    if (all(vapply(nums, function(v) all(is.finite(v)), logical(1)))) {
      active <- nums[[1]] > 0 | nums[[2]] > 0 | nums[[3]] > 0
      keys <- operator_key(a$agent_key)
      ops$operatori_attivi <- length(unique(keys[active & nzchar(keys)]))
    }
  }
  q <- x$qualita$completezza
  if (!is.null(q$errore)) q <- list(errore = q$errore)
  list(metadata = x$metadata[intersect(names(x$metadata), c("servizio", "settimana_id", "settimana_iso",
       "anno_iso", "data_inizio", "data_fine", "creato_il", "regola_settimana"))],
       kpi = x$kpi$indicatori$kpi_settimanali_canale_servizio,
       crm = ct, ops = ops,
       quality = list(riepilogo = q$riepilogo, variabili = q$variabili, errore = q$errore))
}
trend_lookup <- function(tab, key_column, value_column, key) {
  if (!is.data.frame(tab) || !all(c(key_column, value_column) %in% names(tab))) return(NA_real_)
  j <- which(as.character(tab[[key_column]]) == key)
  if (length(j) != 1L) return(NA_real_)
  trend_single(tab[[value_column]][j])
}
trend_extract <- function(x, spec) {
  fail <- function(msg) list(value = NA_real_, numerator = NA_real_, denominator = NA_real_, status = msg)
  if (is.null(spec)) return(fail("Selezione non disponibile"))
  m <- trend_metric(spec$id)
  if (is.null(x)) return(fail("Snapshot non disponibile"))
  num <- den <- NA_real_
  status <- "OK"
  if (m$source == "kpi") {
    tab <- x$kpi
    required <- c("servizio", "canale", "conversazioni_totali", "gestite", "abbandonate", "non_gestite")
    if (!is.data.frame(tab) || !all(required %in% names(tab))) return(fail("Tabella KPI assente o incompleta"))
    codes <- if (SERVIZIO_APP == "19696") "196" else switch(spec$detail,
      `114` = "114", `114_ml` = "114_ML", c("114", "114_ML"))
    tab <- tab[as.character(tab$servizio) %in% codes, , drop = FALSE]
    if (spec$channel != "all") tab <- tab[as.character(tab$canale) == spec$channel, , drop = FALSE]
    if (!nrow(tab)) return(fail("Combinazione servizio/canale non presente"))
    if (spec$id %in% c("kpi_abandoned", "kpi_abandon_rate") &&
        all(as.character(tab$canale) == "WhatsApp")) return(fail("Abbandono non applicabile a WhatsApp"))
    num <- trend_col_sum(tab, m$column)
    if (!is.null(m$denominator)) den <- trend_col_sum(tab, m$denominator)
  } else if (m$source == "crm") {
    tab <- x$crm$tab_overview
    num <- trend_lookup(tab, "metrica", "valore", m$numerator)
    if (!is.null(m$denominator)) den <- trend_lookup(tab, "metrica", "valore", m$denominator)
  } else if (m$source == "distribution") {
    tab <- x$crm[[m$table]]
    if (!is.data.frame(tab) || !all(c(m$column, m$dimension) %in% names(tab))) return(fail("Distribuzione non disponibile"))
    if (!nzchar(spec$category)) return(fail("Selezionare una categoria"))
    den <- trend_col_sum(tab, m$column)
    if (!is.finite(den)) return(fail("Conteggi della distribuzione non valutabili"))
    # Un'intera tabella vuota non e' automaticamente un universo pari a zero.
    if (!nrow(tab)) {
      metric <- if (m$column == "n_contatti") "Contatti agganciati ai casi del periodo" else "Casi distinti"
      universe <- trend_lookup(x$crm$tab_overview, "metrica", "valore", metric)
      if (!is.finite(universe) || universe != 0) return(fail("Distribuzione vuota con universo non verificabile"))
    }
    values <- trend_cat_text(tab[[m$dimension]])
    hit <- which(values == spec$category)
    num <- if (length(hit)) trend_sum(tab[[m$column]][hit]) else 0
    if (!length(hit)) status <- "Categoria assente nella distribuzione disponibile: 0"
  } else if (m$source == "ops") {
    num <- trend_single(x$ops[[m$column]])
    if (!is.null(m$denominator)) den <- trend_single(x$ops[[m$denominator]])
  } else if (m$source == "quality") {
    if (!is.null(x$quality$errore)) return(fail("Completezza non disponibile per errore nello snapshot"))
    tab <- x$quality$riepilogo
    num <- trend_lookup(tab, "Indicatore", "Valore", m$numerator)
    if (!is.null(m$denominator)) den <- trend_lookup(tab, "Indicatore", "Valore", m$denominator)
  } else if (m$source == "quality_variable") {
    tab <- x$quality$variabili
    if (!is.data.frame(tab) || !all(c("Tabella", "Variabile") %in% names(tab))) return(fail("Dettaglio variabili non disponibile"))
    pos <- which(trend_quality_key(tab) == spec$category)
    if (length(pos) != 1L) return(fail("Variabile assente o non univoca nella settimana"))
    row <- tab[pos, , drop = FALSE]
    if ("Colonna_esportata" %in% names(row) && !isTRUE(as.logical(row$Colonna_esportata[1]))) {
      return(fail("Colonna non esportata: completezza non valutabile"))
    }
    record_mode <- identical(spec$mode, "pct_record")
    num <- trend_single(row[[if (record_mode) "Record_mancanti" else "Progressivi_con_mancanti"]])
    den <- trend_single(row[[if (record_mode) "Record_valutabili" else "Progressivi_valutabili"]])
  }
  if (!is.finite(num) || num < 0) return(fail("Numeratore assente o non valido"))
  unit <- trend_unit(spec)
  ratio <- unit %in% c("pct", "ratio") || (unit == "seconds" && !is.null(m$denominator))
  if (ratio) {
    if (!is.finite(den) || den <= 0) return(list(value = NA_real_, numerator = num,
      denominator = den, status = "Denominatore nullo o non valutabile"))
    if (unit == "pct" && num > den + 1e-7) return(fail("Numeratore superiore al denominatore: verificare fonte"))
    value <- num / den
  } else value <- num
  list(value = value, numerator = num, denominator = den, status = status)
}

# Calendario completo: i buchi restano NA e interrompono linee, medie mobili e delta.
trend_build_series <- function(history, start, end, spec) {
  dates <- seq(as.Date(start), as.Date(end), by = "7 days")
  ans <- vector("list", length(dates))
  for (i in seq_along(dates)) {
    day <- dates[i]; j <- match(as.character(day), names(history)); entry <- if (is.na(j)) NULL else history[[j]]
    item <- if (is.null(entry)) NULL else entry$data
    result <- trend_extract(item, spec)
    if (!is.null(entry$error)) result$status <- paste("Snapshot non utilizzabile:", entry$error)
    sid <- if (!is.null(entry$week)) entry$week else paste0("ISO ", format(day, "%V"), " (assente)")
    stamp <- if (!is.null(item$metadata$creato_il)) {
      format(as.POSIXct(item$metadata$creato_il), "%d/%m/%Y %H:%M", tz = "Europe/Rome")
    } else ""
    ans[[i]] <- data.frame(start = day, end = day + 6, week = sid,
      value = result$value, numerator = result$numerator, denominator = result$denominator,
      status = result$status, generated = stamp, stringsAsFactors = FALSE)
  }
  z <- do.call(rbind, ans)
  z$delta <- c(NA_real_, diff(z$value))
  previous <- c(NA_real_, head(z$value, -1L))
  z$delta_relative <- ifelse(is.finite(previous) & previous != 0, z$delta / previous, NA_real_)
  z$segment <- cumsum(!is.finite(z$value))
  rownames(z) <- NULL
  z
}
trend_summary <- function(z, spec) {
  valid <- is.finite(z$value)
  vals <- z$value[valid]
  m <- trend_metric(spec$id); unit <- trend_unit(spec)
  has_ratio <- unit %in% c("pct", "ratio") || (unit == "seconds" && !is.null(m$denominator))
  summary_value <- NA_real_; label <- "Totale nel periodo"
  if (length(vals)) {
    if (has_ratio) {
      good <- valid & is.finite(z$numerator) & is.finite(z$denominator) & z$denominator > 0
      summary_value <- sum(z$numerator[good]) / sum(z$denominator[good])
      label <- if (m$family == "Qualita' dati") "Quota sulle valutazioni-settimana" else "Rapporto sulle somme del periodo"
      if (unit == "seconds") label <- "Tempo medio ponderato del periodo"
    } else if (isTRUE(m$additive)) {
      summary_value <- sum(vals)
    } else {
      summary_value <- stats::median(vals)
      label <- "Mediana settimanale (non unici)"
    }
  }
  if (any(!valid) && is.finite(summary_value)) label <- paste0(label, " [parziale]")
  list(n = sum(valid), missing = sum(!valid), mean = if (length(vals)) mean(vals) else NA_real_,
    min = if (length(vals)) min(vals) else NA_real_, max = if (length(vals)) max(vals) else NA_real_,
    last = tail(z$value, 1L), total = summary_value, total_label = label,
    delta_last = tail(z$delta, 1L), relative_last = tail(z$delta_relative, 1L),
    delta_period = if (nrow(z) > 1L) tail(z$value, 1L) - z$value[1L] else NA_real_,
    relative_period = if (nrow(z) > 1L && is.finite(z$value[1L]) && z$value[1L] != 0) {
      (tail(z$value, 1L) - z$value[1L]) / z$value[1L]
    } else NA_real_)
}
trend_rolling <- function(z, spec, width = 4L) {
  out <- rep(NA_real_, nrow(z))
  m <- trend_metric(spec$id)
  ratio <- trend_unit(spec) %in% c("pct", "ratio") ||
    (trend_unit(spec) == "seconds" && !is.null(m$denominator))
  if (nrow(z) >= width) for (i in seq.int(width, nrow(z))) {
    part <- z[seq.int(i - width + 1L, i), , drop = FALSE]
    if (!all(is.finite(part$value)) || any(diff(as.integer(part$start)) != 7L)) next
    out[i] <- if (ratio) sum(part$numerator) / sum(part$denominator) else mean(part$value)
  }
  out
}

# Lista curata e simmetrica delle coppie proposte nel secondo selettore.
trend_compatible_ids <- function(id) {
  ids <- names(trend_catalog)
  k <- ids[vapply(trend_catalog, function(m) m$source == "kpi", logical(1))]
  cmain <- c("crm_cases", "crm_contacts", "crm_contacts_case")
  cflags <- c("crm_recontact", "crm_minors", "crm_immediate", "crm_external")
  themes <- c("mix_area", "mix_category", "mix_element")
  ops <- ids[vapply(trend_catalog, function(m) m$source == "ops", logical(1))]
  quality <- ids[vapply(trend_catalog, function(m) m$family == "Qualita' dati", logical(1))]
  wanted <- function(a) {
    m <- trend_metric(a)
    if (is.null(m)) return(character())
    if (m$source == "kpi") return(c(k, cmain[1:2], ops, "q_contacts", "q_no_progressivo", "q_missing"))
    if (a %in% cmain) return(c("kpi_received", "kpi_handled", cmain, cflags, "ops_hours", "ops_events", quality))
    if (a %in% cflags) return(c(cmain[1:2], cflags, themes, "q_missing"))
    if (m$source == "distribution") return(c(a, "crm_cases", "crm_contacts", cflags, "q_missing"))
    if (m$source == "ops") return(c(k, cmain[1:2], ops, "q_missing", "q_variable"))
    if (m$family == "Qualita' dati") return(c(cmain[1:2], "kpi_received", "ops_hours", "q_contacts", "q_missing", "q_variable", a))
    character()
  }
  selected <- ids[vapply(ids, function(other) other %in% wanted(id) || id %in% wanted(other), logical(1))]
  # Stesso indicatore ammesso solo quando il dettaglio puo' cambiare.
  m <- trend_metric(id)
  if (!is.null(m) && !m$source %in% c("kpi", "distribution", "quality_variable") && length(m$modes) == 1L) {
    selected <- setdiff(selected, id)
  }
  selected
}
trend_components <- function(spec) {
  m <- trend_metric(spec$id); ratio <- trend_unit(spec) %in% c("pct", "ratio", "seconds")
  num <- den <- NULL
  if (m$source == "crm") {
    num <- paste0("crm:", m$numerator)
    if (ratio && !is.null(m$denominator)) den <- paste0("crm:", m$denominator)
  } else if (m$source == "distribution") {
    num <- paste0("distribution:", m$table, ":", spec$category)
    if (ratio) den <- paste0("crm:", if (m$column == "n_casi") "Casi distinti" else "Contatti agganciati ai casi del periodo")
  } else if (m$source == "ops") {
    num <- paste0("ops:", m$column)
    if (ratio && !is.null(m$denominator)) den <- paste0("ops:", m$denominator)
  } else if (m$source == "quality") {
    num <- paste0("quality:", m$numerator)
    if (ratio && !is.null(m$denominator)) den <- paste0("quality:", m$denominator)
  } else if (m$source == "quality_variable") {
    num <- paste0("qvar:", spec$category, ":", spec$mode)
    den <- paste0("qden:", spec$category, ":", spec$mode)
  }
  list(num = num, den = den)
}
trend_pair_policy <- function(a, b) {
  result <- list(graph = TRUE, correlate = TRUE, reason = "Confronto esplorativo consentito.", caution = "")
  block <- function(reason, graph = TRUE) list(graph = graph, correlate = FALSE, reason = reason, caution = "")
  if (is.null(a) || is.null(b)) return(block("Selezionare entrambi gli indicatori.", FALSE))
  if (!b$id %in% trend_compatible_ids(a$id)) return(block("Coppia non prevista dal catalogo dei confronti.", FALSE))
  if (identical(a, b)) return(block("Le due selezioni coincidono: scegliere un indicatore o un dettaglio diverso."))
  ma <- trend_metric(a$id); mb <- trend_metric(b$id)
  if (ma$source == "kpi" && mb$source == "kpi") {
    ca <- kpi_detail_codes(a$detail); cb <- kpi_detail_codes(b$detail)
    channel_overlap <- a$channel == "all" || b$channel == "all" || a$channel == b$channel
    if (length(intersect(ca, cb)) && channel_overlap) return(block(paste(
      "Solo confronto grafico: indicatori di coda su perimetri sovrapposti, con relazioni parte/totale",
      "o percentuali della stessa composizione. Non viene calcolata la correlazione.")))
  }
  if (a$id == b$id && a$category == b$category && ma$source != "kpi") {
    return(block("Solo confronto grafico: versioni o denominatori dello stesso indicatore."))
  }
  if (ma$source == "distribution" && mb$source == "distribution" && identical(ma$table, mb$table)) {
    return(block("Solo confronto grafico: categorie della stessa distribuzione, vincolate al medesimo totale."))
  }
  c1 <- trend_components(a); c2 <- trend_components(b)
  common <- intersect(c(c1$num, c1$den), c(c2$num, c2$den))
  common_den_only <- length(common) && identical(common, intersect(c1$den, c2$den))
  if (length(common) && !common_den_only) return(block(
    "Solo confronto grafico: una misura condivide un numeratore, oppure compare nel denominatore dell'altra."))
  if (common_den_only) result$caution <- paste(
    "Le due quote condividono il denominatore: l'associazione puo' risentire della variazione del totale.",
    "Non descrive una relazione tra le caratteristiche dei singoli casi.")
  quality_parts <- list(
    q_schede = c("q_missing", "q_no_chiamante", "q_no_minori", "q_no_motivazioni", "q_no_responsabili", "q_no_servizi"),
    q_contacts = c("q_no_progressivo", "crm_contacts"),
    q_cases = "q_cases_no_contacts"
  )
  for (whole in names(quality_parts)) {
    if ((a$id == whole && b$id %in% quality_parts[[whole]]) ||
        (b$id == whole && a$id %in% quality_parts[[whole]])) {
      return(block("Solo confronto grafico: relazione tra un totale e una sua parte, oppure la quota derivata."))
    }
  }
  base_crm <- c("crm_cases", "crm_contacts")
  if (a$id %in% base_crm || b$id %in% base_crm) {
    base <- if (a$id %in% base_crm) a else b
    other <- if (a$id %in% base_crm) b else a
    om <- trend_metric(other$id)
    if ((base$id == "crm_cases" && (other$id %in% c("crm_recontact", "crm_minors", "crm_immediate", "crm_external") ||
        (om$source == "distribution" && om$column == "n_casi"))) ||
        (base$id == "crm_contacts" && om$source == "distribution" && om$column == "n_contatti")) {
      return(block("Solo confronto grafico: confronto tra il totale CRM e una sua parte o quota."))
    }
  }
  if (all(c(a$id, b$id) %in% c("crm_cases", "q_cases"))) return(block(
    "Solo confronto grafico: casi aperti da due perimetri di analisi, da confrontare come controllo e non come associazione."))
  result
}
trend_paired <- function(a, b, basis = "levels") {
  stopifnot(identical(as.Date(a$start), as.Date(b$start)))
  data.frame(start = a$start, week = a$week,
    x = if (basis == "changes") a$delta else a$value,
    y = if (basis == "changes") b$delta else b$value, stringsAsFactors = FALSE)
}
trend_association <- function(a, b, spec_a, spec_b, basis = "levels") {
  p <- trend_pair_policy(spec_a, spec_b)
  paired <- trend_paired(a, b, basis)
  ok <- is.finite(paired$x) & is.finite(paired$y)
  paired <- paired[ok, , drop = FALSE]
  n <- nrow(paired)
  ans <- list(rho = NA_real_, n = n, pairs = paired, status = "", policy = p)
  if (!p$correlate) { ans$status <- p$reason; return(ans) }
  if (n < TREND_MIN_PAIRS) {
    ans$status <- paste0("Associazione non calcolata: ", n, " coppie valide; minimo operativo ",
      TREND_MIN_PAIRS, ". Non e' una soglia di affidabilita' statistica.")
    return(ans)
  }
  if (length(unique(paired$x)) < 2L || length(unique(paired$y)) < 2L) {
    ans$status <- "Associazione non calcolabile: almeno una serie e' costante."
    return(ans)
  }
  ans$rho <- suppressWarnings(stats::cor(paired$x, paired$y, method = "spearman"))
  ans$status <- if (n < TREND_CAUTION_PAIRS) {
    "Associazione esplorativa su poche settimane: interpretare con particolare cautela."
  } else "Associazione descrittiva ed esplorativa; non e' una prova di causalita'."
  if (anyDuplicated(paired$x) || anyDuplicated(paired$y)) {
    ans$status <- paste(ans$status, "Sono presenti valori a pari merito.")
  }
  ans
}
trend_base100 <- function(a, b) {
  # Stessa base temporale per entrambe le serie; non si sposta la base per nascondere uno zero/NA.
  if (!nrow(a) || !nrow(b) || !is.finite(a$value[1]) || !is.finite(b$value[1]) ||
      a$value[1] <= 0 || b$value[1] <= 0) return(NULL)
  list(a = 100 * a$value / a$value[1], b = 100 * b$value / b$value[1], base = a$start[1])
}
trend_ticks <- function(z) {
  n <- nrow(z)
  z$start[unique(c(seq.int(1L, n, by = max(1L, ceiling(n / 10L))), n))]
}
trend_axis_labels <- function(unit) {
  if (unit == "pct") return(function(v) paste0(scales::number(100 * v, accuracy = 0.1, decimal.mark = ","), "%"))
  if (unit == "seconds") return(function(v) vapply(v, function(t) {
    if (!is.finite(t)) return("")
    if (t < 0) paste0("-", fmt_duration(-t)) else fmt_duration(t)
  }, character(1)))
  scales::label_number(big.mark = ".", decimal.mark = ",")
}
trend_line_data <- function(z, column = "value") {
  z <- z[is.finite(z[[column]]), , drop = FALSE]
  if (!nrow(z)) return(z)
  sizes <- table(z$segment)
  z[as.character(z$segment) %in% names(sizes)[sizes >= 2L], , drop = FALSE]
}
trend_plot <- function(z, spec, rolling = FALSE, labels = TRUE, color = ACCENT) {
  unit <- trend_unit(spec)
  lines <- trend_line_data(z)
  points <- z[is.finite(z$value), , drop = FALSE]
  p <- ggplot(z, aes(x = start, y = value)) +
    geom_line(data = lines, aes(group = segment), linewidth = 1.0, color = color, na.rm = TRUE) +
    geom_point(data = points, color = color, size = 2.8, na.rm = TRUE)
  if (isTRUE(rolling)) {
    roll <- z; roll$value <- trend_rolling(z, spec); roll$segment <- cumsum(!is.finite(roll$value))
    p <- p + geom_line(data = trend_line_data(roll), aes(group = segment),
      linewidth = 0.95, linetype = "dashed", color = TA_GREY, na.rm = TRUE)
  }
  if (isTRUE(labels) && nrow(points) <= 26L) {
    points$label <- vapply(points$value, trend_format, character(1), unit = unit)
    p <- p + geom_text(data = points, aes(label = label), vjust = -0.9,
      color = color, size = 3.1, check_overlap = TRUE, na.rm = TRUE)
  }
  p + scale_x_date(breaks = trend_ticks(z), labels = function(d) format(d, "%d/%m/%y"),
        limits = range(z$start), expand = expansion(mult = c(0.04, 0.06))) +
    scale_y_continuous(labels = trend_axis_labels(unit), expand = expansion(mult = c(0.08, 0.18))) +
    labs(x = "Settimana (data del lunedi')", y = trend_unit_label(unit),
      caption = if (isTRUE(rolling)) "Tratteggio: media mobile su 4 settimane consecutive; rapporti e durate ponderati sui denominatori." else NULL) +
    theme_ta() + theme(axis.text.x = element_text(angle = 35, hjust = 1))
}
trend_index_plot <- function(a, b) {
  ix <- trend_base100(a, b)
  if (is.null(ix)) return(NULL)
  pa <- data.frame(start = a$start, value = ix$a, serie = "A", segment = paste0("A", cumsum(!is.finite(ix$a))))
  pb <- data.frame(start = b$start, value = ix$b, serie = "B", segment = paste0("B", cumsum(!is.finite(ix$b))))
  z <- rbind(pa, pb)
  colors <- c(A = ACCENT, B = if (SERVIZIO_APP == "114") TA_ORANGE else TA_BLUE)
  ggplot(z, aes(x = start, y = value, color = serie)) +
    geom_hline(yintercept = 100, color = TA_GREY, linetype = "dashed") +
    geom_line(data = trend_line_data(z), aes(group = segment), linewidth = 1.0, na.rm = TRUE) +
    geom_point(size = 2.7, na.rm = TRUE) + scale_color_manual(values = colors) +
    scale_x_date(breaks = trend_ticks(a), labels = function(d) format(d, "%d/%m/%y"), limits = range(a$start)) +
    labs(x = "Settimana", y = "Indice base 100", caption = paste("Base comune:", format(ix$base, "%d/%m/%Y"))) +
    theme_ta() + theme(axis.text.x = element_text(angle = 35, hjust = 1))
}
trend_scatter <- function(assoc, sa, sb, basis, labels = TRUE) {
  z <- assoc$pairs
  if (!nrow(z)) return(NULL)
  u1 <- trend_unit(sa); u2 <- trend_unit(sb)
  p <- ggplot(z, aes(x = x, y = y)) + geom_point(color = ACCENT, size = 3, alpha = 0.85)
  if (isTRUE(labels) && nrow(z) <= 26L) p <- p + geom_text(aes(label = week),
    vjust = -0.8, size = 3.2, color = TA_BLUE_DARK, check_overlap = TRUE)
  p + scale_x_continuous(labels = trend_axis_labels(u1), expand = expansion(mult = c(0.1, 0.15))) +
    scale_y_continuous(labels = trend_axis_labels(u2), expand = expansion(mult = c(0.1, 0.18))) +
    labs(x = paste0(if (basis == "changes") "Variazione " else "", "A (", trend_unit_label(u1), ")"),
         y = paste0(if (basis == "changes") "Variazione " else "", "B (", trend_unit_label(u2), ")"),
         caption = if (basis == "changes") "Ogni punto e' una coppia di variazioni tra settimane consecutive disponibili." else "Ogni punto rappresenta una settimana con entrambi i valori disponibili.") +
    theme_ta()
}


trend_read_history <- function(idx, cache) {
  history <- list()
  if (!nrow(idx)) return(history)
  duplicate_dates <- unique(as.character(idx$data_inizio[duplicated(idx$data_inizio) |
    duplicated(idx$data_inizio, fromLast = TRUE)]))
  for (i in seq_len(nrow(idx))) {
    row <- idx[i, , drop = FALSE]
    day <- as.character(as.Date(row$data_inizio))
    if (is.na(day) || !nzchar(day)) next
    entry <- list(data = NULL, error = NULL, week = as.character(row$settimana_id), file = basename(row$path))
    if (day %in% duplicate_dates) {
      entry$error <- "Piu' file/settimane diversi hanno la stessa data di inizio: verificare lo storico."
      history[[day]] <- entry
      next
    }
    signature <- if (file.exists(row$path)) {
      fi <- file.info(row$path)
      paste(fi$size, as.numeric(fi$mtime), sep = "|")
    } else "absent"
    cached <- cache[[row$path]]
    if (!is.null(cached) && identical(cached$signature, signature)) {
      history[[day]] <- cached$entry
      next
    }
    entry <- tryCatch({
      snap <- validate_snapshot(readRDS(row$path))
      if (!identical(as.character(snap$metadata$settimana_id), as.character(row$settimana_id)) ||
          !identical(as.Date(snap$metadata$data_inizio), as.Date(row$data_inizio)) ||
          !identical(as.Date(snap$metadata$data_fine), as.Date(row$data_fine))) {
        stop("Metadati non coerenti con il nome del file.", call. = FALSE)
      }
      entry$data <- trend_project(snap)
      entry
    }, error = function(e) {
      entry$error <- gsub(as.character(row$path), basename(row$path), conditionMessage(e), fixed = TRUE)
      entry
    })
    cache[[row$path]] <- list(signature = signature, entry = entry)
    history[[day]] <- entry
  }
  history[order(names(history))]
}
trend_categories <- function(history, id) {
  m <- trend_metric(id)
  values <- character()
  if (is.null(m)) return(values)
  for (entry in history) {
    x <- entry$data
    if (is.null(x)) next
    if (m$source == "distribution") {
      tab <- x$crm[[m$table]]
      if (is.data.frame(tab) && m$dimension %in% names(tab)) values <- c(values, trend_cat_text(tab[[m$dimension]]))
    } else if (m$source == "quality_variable") {
      tab <- x$quality$variabili
      if (is.data.frame(tab) && all(c("Tabella", "Variabile") %in% names(tab))) values <- c(values, trend_quality_key(tab))
    }
  }
  values <- sort(unique(values[!is.na(values) & nzchar(values)]))
  # Non noto rimane selezionabile ma non e' il primo dettaglio suggerito.
  unknown <- grepl("^(non |nd$|n/d$)", tolower(values))
  c(values[!unknown], values[unknown])
}

trend_is_distribution_overview <- function(spec) {
  if (is.null(spec)) return(FALSE)
  m <- trend_metric(spec$id)
  !is.null(m) && identical(m$source, "distribution") && !nzchar(spec$category %||% "")
}

trend_distribution_ranking <- function(history, start, end, spec, top_n = 10L) {
  m <- trend_metric(spec$id)
  if (is.null(m) || m$source != "distribution") return(data.frame())
  dates <- as.Date(names(history))
  use <- !is.na(dates) & dates >= as.Date(start) & dates <= as.Date(end)
  rows <- list()
  k <- 0L
  for (entry in history[use]) {
    x <- entry$data
    if (is.null(x)) next
    tab <- x$crm[[m$table]]
    if (!is.data.frame(tab) || !nrow(tab) || !all(c(m$dimension, m$column) %in% names(tab))) next
    category <- trend_cat_text(tab[[m$dimension]])
    value <- safe_num(tab[[m$column]])
    ok <- !is.na(category) & nzchar(category) & is.finite(value) & value >= 0
    if (!any(ok)) next
    k <- k + 1L
    rows[[k]] <- data.frame(category = category[ok], weight = value[ok], stringsAsFactors = FALSE)
  }
  if (!length(rows)) return(data.frame())
  z <- bind_rows(rows) %>%
    group_by(category) %>%
    summarise(weight = sum(weight, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(weight), category)
  z$rank <- seq_len(nrow(z))
  total_categories <- nrow(z)
  z <- z %>% slice_head(n = min(as.integer(top_n), nrow(z)))
  z <- as.data.frame(z, stringsAsFactors = FALSE)
  attr(z, "total_categories") <- total_categories
  z
}

trend_build_top_distribution <- function(history, start, end, spec, top_n = 10L) {
  ranking <- trend_distribution_ranking(history, start, end, spec, top_n)
  if (!nrow(ranking)) {
    return(list(data = data.frame(), ranking = ranking, total_categories = 0L))
  }
  total_categories <- attr(ranking, "total_categories") %||% nrow(ranking)
  series <- vector("list", nrow(ranking))
  for (i in seq_len(nrow(ranking))) {
    sp <- spec
    sp$category <- as.character(ranking$category[i])
    z <- trend_build_series(history, start, end, sp)
    z$category <- sp$category
    z$rank <- ranking$rank[i]
    series[[i]] <- z
  }
  out <- bind_rows(series)
  out$category <- factor(out$category, levels = as.character(ranking$category))
  list(data = out, ranking = ranking, total_categories = as.integer(total_categories))
}

trend_top_plot <- function(top, spec) {
  z <- top$data
  if (!is.data.frame(z) || !nrow(z)) return(NULL)
  unit <- trend_unit(spec)
  lines <- z[is.finite(z$value), , drop = FALSE]
  if (!nrow(lines)) return(NULL)
  p <- ggplot(lines, aes(x = start, y = value, color = category,
                         group = interaction(category, segment, drop = TRUE))) +
    geom_line(linewidth = 0.95, na.rm = TRUE) +
    geom_point(size = 2.25, na.rm = TRUE) +
    scale_x_date(
      breaks = trend_ticks(lines[!duplicated(lines$start), , drop = FALSE]),
      labels = function(d) format(d, "%d/%m/%y"),
      expand = expansion(mult = c(0.04, 0.06))
    ) +
    scale_y_continuous(
      labels = trend_axis_labels(unit),
      expand = expansion(mult = c(0.06, 0.14))
    ) +
    labs(
      x = "Settimana (data del lunedi')",
      y = trend_unit_label(unit),
      color = NULL,
      caption = paste0(
        "Prime ", nrow(top$ranking), " categorie del periodo, ordinate per volume cumulato. ",
        "Il ranking usa i conteggi; il grafico mostra la misura selezionata."
      )
    ) +
    theme_ta() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "bottom")
  p
}

trend_top_cards_ui <- function(top, spec) {
  z <- top$data
  ranking <- top$ranking
  if (!is.data.frame(z) || !nrow(z) || !nrow(ranking)) return(NULL)
  valid <- z[is.finite(z$value), , drop = FALSE]
  unit <- trend_unit(spec)
  if (nrow(valid)) {
    latest_date <- max(valid$start)
    last <- valid[valid$start == latest_date, , drop = FALSE]
    last <- last[order(last$value, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
    latest_category <- if (nrow(last)) as.character(last$category[1L]) else "ND"
    latest_value <- if (nrow(last)) trend_format(last$value[1L], unit) else "ND"
    latest_week <- if (nrow(last)) as.character(last$week[1L]) else "ND"
    n_weeks <- length(unique(valid$start))
  } else {
    latest_category <- latest_value <- latest_week <- "ND"
    n_weeks <- 0L
  }
  div(class = "kpi-grid",
    kpi_card("Categorie mostrate", fmt_int(nrow(ranking)),
      paste0("su ", fmt_int(top$total_categories), " categorie osservate nel periodo"), TRUE),
    kpi_card("Prima tra le Top 10 nell'ultima settimana", latest_category, latest_week),
    kpi_card("Valore nell'ultima settimana", latest_value, trend_mode_labels[[spec$mode]]),
    kpi_card("Settimane valutabili", fmt_int(n_weeks), "Almeno una categoria Top 10 con valore disponibile"),
    kpi_card("Prima categoria nel periodo", as.character(ranking$category[1L]),
      paste0("Volume cumulato: ", fmt_int(ranking$weight[1L]))),
    kpi_card("Criterio Top 10", "Volume cumulato", "Somma dei conteggi settimanali nel periodo selezionato")
  )
}

trend_top_table_data <- function(top, spec, audit = FALSE) {
  z <- top$data
  if (!is.data.frame(z) || !nrow(z)) return(data.frame())
  unit <- trend_unit(spec)
  mult <- if (unit == "pct") 100 else 1
  out <- data.frame(
    Settimana = z$week,
    Dal = format(z$start, "%d/%m/%Y"),
    Al = format(z$end, "%d/%m/%Y"),
    Categoria = as.character(z$category),
    Rank_periodo = z$rank,
    Valore = z$value * mult,
    Delta = z$delta * mult,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (unit == "pct") names(out)[names(out) == "Valore"] <- "Valore (%)"
  if (unit == "pct") names(out)[names(out) == "Delta"] <- "Delta (p.p.)"
  if (unit != "pct") {
    out[["Delta (%)"]] <- z$delta_relative * 100
  }
  if (audit) {
    out[["Numeratore"]] <- z$numerator
    if (any(is.finite(z$denominator))) out[["Denominatore"]] <- z$denominator
    out[["Snapshot generato il"]] <- z$generated
  }
  out[["Stato"]] <- z$status
  out[order(as.Date(z$start), z$rank), , drop = FALSE]
}

trend_metric_choices <- function(ids) {
  cats <- trend_catalog[intersect(names(trend_catalog), ids)]
  if (!length(cats)) return(character())
  fam <- unique(vapply(cats, function(m) m$family, character(1)))
  setNames(lapply(fam, function(f) {
    rows <- cats[vapply(cats, function(m) m$family == f, logical(1))]
    setNames(names(rows), vapply(rows, function(m) m$label, character(1)))
  }), fam)
}

trend_selection_ui <- function(ns, prefix, primary = TRUE) {
  mid <- paste0(prefix, "_metric")
  condition_distribution <- paste0("(input['", ns(mid), "'] || '').indexOf('mix_') === 0")
  condition_qvar <- paste0("input['", ns(mid), "'] === 'q_variable'")
  condition_kpi <- paste0("(input['", ns(mid), "'] || '').indexOf('kpi_') === 0")
  detail_id <- paste0(prefix, "_detail_enabled")
  condition_category <- if (primary) {
    paste0("(", condition_qvar, ") || (", condition_distribution, " && input['", ns(detail_id), "'])")
  } else {
    paste0("(", condition_qvar, ") || (", condition_distribution, ")")
  }

  tagList(
    if (primary) selectInput(ns("a_family"), "Famiglia", choices = unname(unique(vapply(trend_catalog,
      function(m) m$family, character(1)))), selected = "KPI di servizio", width = "100%"),
    selectInput(ns(mid), if (primary) "Indicatore A" else "Indicatore B compatibile",
      choices = if (primary) c("Conversazioni ricevute" = "kpi_received") else c("Ore di login" = "ops_hours"),
      width = "100%"),
    if (primary) conditionalPanel(
      condition_distribution,
      checkboxInput(ns(detail_id), "Seleziona una categoria specifica", FALSE),
      div(class = "panel-note",
        "Se non selezioni un dettaglio, il grafico mostra automaticamente l'andamento delle prime 10 categorie del periodo.")
    ),
    conditionalPanel(
      condition_category,
      selectInput(ns(paste0(prefix, "_category")),
        if (primary) "Categoria / variabile" else "Categoria / variabile da confrontare",
        choices = character(), width = "100%")
    ),
    selectInput(ns(paste0(prefix, "_mode")), "Misura", choices = c("Numero" = "n"), width = "100%"),
    conditionalPanel(condition_kpi,
      if (SERVIZIO_APP == "114") selectInput(ns(paste0(prefix, "_detail")), "Perimetro KPI",
        choices = c("114 incl. Multilingua" = "group", "114" = "114", "114 Multilingua" = "114_ml"),
        selected = "group", width = "100%"),
      selectInput(ns(paste0(prefix, "_channel")), "Canale KPI",
        choices = c("Tutti i canali" = "all", "Telefono" = "Telefono", "Chat" = "Chat", "WhatsApp" = "WhatsApp"),
        selected = "all", width = "100%"))
  )
}

trend_ui <- function(id) {
  ns <- NS(id)
  tagList(
    panel_box("Trend e confronti",
      "Ogni punto rappresenta una settimana lunedi'-domenica. Il periodo di questa sezione e' indipendente dalla settimana delle altre schede.",
      fluidRow(
        column(4, selectInput(ns("period"), "Periodo", choices = c("Ultime 4 settimane" = "4",
          "Ultime 8 settimane" = "8", "Ultime 12 settimane" = "12", "Tutto lo storico" = "all",
          "Personalizzato" = "custom"), selected = "all", width = "100%")),
        column(5, conditionalPanel(paste0("input['", ns("period"), "'] === 'custom'"),
          fluidRow(column(6, selectInput(ns("from"), "Da", choices = NULL, width = "100%")),
                   column(6, selectInput(ns("to"), "A", choices = NULL, width = "100%"))))),
        column(3, br(), actionButton(ns("refresh"), "Aggiorna storico", class = "btn-primary", width = "100%"))
      ),
      uiOutput(ns("period_info"))
    ),
    fluidRow(
      column(6, panel_box("Indicatore principale", NULL, trend_selection_ui(ns, "a", TRUE))),
      column(6, panel_box("Confronto facoltativo", NULL,
        checkboxInput(ns("compare"), "Confronta con un secondo indicatore", FALSE),
        conditionalPanel(paste0("input['", ns("compare"), "']"), trend_selection_ui(ns, "b", FALSE)),
        div(class = "panel-note", "Il menu propone confronti operativi pertinenti. La presenza nel menu non garantisce che sia corretto calcolare una correlazione: la regola viene verificata sulla coppia selezionata.")
      ))
    ),
    panel_box("Opzioni di lettura", NULL,
      fluidRow(column(4, checkboxInput(ns("rolling"), "Media mobile di 4 settimane", FALSE)),
               column(4, checkboxInput(ns("labels"), "Etichette dei punti (fino a 26 settimane)", TRUE)),
               column(4, checkboxInput(ns("audit"), "Mostra denominatori e date snapshot", FALSE))),
      uiOutput(ns("indicator_notes"))
    ),
    uiOutput(ns("cards_a")),
    panel_box("Andamento dell'indicatore A", NULL, uiOutput(ns("title_a")), plotOutput(ns("plot_a"), height = "420px")),
    conditionalPanel(paste0("input['", ns("compare"), "']"),
      uiOutput(ns("cards_b")),
      panel_box("Andamento dell'indicatore B", NULL, uiOutput(ns("title_b")), plotOutput(ns("plot_b"), height = "420px")),
      panel_box("Confronto normalizzato - base 100", "Le due serie usano come base la stessa prima settimana del periodo. Nessun doppio asse Y.",
        uiOutput(ns("base_message")), plotOutput(ns("plot_base"), height = "390px")),
      panel_box("Associazione tra gli indicatori", NULL,
        selectInput(ns("basis"), "Valori da confrontare", choices = c("Valori settimanali" = "levels",
          "Variazioni assolute tra settimane consecutive" = "changes"), selected = "levels", width = "100%"),
        uiOutput(ns("association_info")), plotOutput(ns("scatter"), height = "420px"),
        div(class = "panel-note", paste(
          "Spearman descrive un'associazione monotona, non un effetto causale.",
          "Trend comuni, stagionalita', autocorrelazione e denominatori condivisi possono influenzare il coefficiente.",
          "Non si mostrano p-value o giudizi automatici di significativita'. Anche le variazioni restano esplorative."))
      )
    ),
    panel_box("Tabella settimanale", NULL,
      downloadButton(ns("download"), "Esporta la tabella CSV"), br(), br(), DTOutput(ns("table"))),
    panel_box("Qualita' dello storico e regole di lettura", NULL,
      uiOutput(ns("history_notes")),
      tags$details(tags$summary("Apri catalogo degli indicatori e delle fonti"), DTOutput(ns("catalog_table"))))
  )
}

trend_cards_ui <- function(z, spec, prefix) {
  s <- trend_summary(z, spec); unit <- trend_unit(spec)
  delta_text <- function(abs_value, relative) {
    if (unit == "pct") {
      if (!is.finite(abs_value)) return("ND")
      return(paste0(if (abs_value > 0) "+" else "", fmt_num(100 * abs_value, 1), " p.p."))
    }
    trend_format(abs_value, unit, signed = TRUE)
  }
  relative_text <- function(v) if (unit == "pct") "Differenza in punti percentuali" else {
    if (!is.finite(v)) "Variazione relativa non calcolabile (base nulla o mancante)" else
      paste0(if (v > 0) "+" else "", fmt_pct_ratio(v), " in termini relativi")
  }
  div(class = "kpi-grid",
    kpi_card(paste0(prefix, " - Ultima settimana del periodo"), trend_format(s$last, unit), tail(z$week, 1L), TRUE),
    kpi_card("Media settimanale", trend_format(s$mean, unit), paste(s$n, "settimane valide; media semplice dei valori settimanali")),
    kpi_card(s$total_label, trend_format(s$total, unit), if (trend_metric(spec$id)$additive) "Calcolato sulle settimane valide" else "Le schede/operatori possono ricomparire in piu' settimane"),
    kpi_card("Minimo - Massimo", paste(trend_format(s$min, unit), trend_format(s$max, unit), sep = " / ")),
    kpi_card("Variazione vs settimana precedente", delta_text(s$delta_last, s$relative_last), relative_text(s$relative_last)),
    kpi_card("Variazione inizio - fine periodo", delta_text(s$delta_period, s$relative_period), relative_text(s$relative_period))
  )
}
trend_table_data <- function(a, sa, b = NULL, sb = NULL, audit = FALSE) {
  out <- data.frame(Settimana = a$week, Dal = format(a$start, "%d/%m/%Y"),
    Al = format(a$end, "%d/%m/%Y"), stringsAsFactors = FALSE, check.names = FALSE)
  append_series <- function(out, z, spec, tag) {
    unit <- trend_unit(spec)
    mult <- if (unit == "pct") 100 else 1
    out[[paste0(tag, " (", trend_unit_label(unit), ")")]] <- z$value * mult
    out[[paste0("Delta ", tag, if (unit == "pct") " (p.p.)" else paste0(" (", trend_unit_label(unit), ")"))]] <- z$delta * mult
    if (unit != "pct") out[[paste0("Delta ", tag, " (%)")]] <- z$delta_relative * 100
    if (audit) {
      out[[paste0("Numeratore ", tag)]] <- z$numerator
      if (any(is.finite(z$denominator))) out[[paste0("Denominatore ", tag)]] <- z$denominator
    }
    out[[paste0("Stato ", tag)]] <- z$status
    out
  }
  out <- append_series(out, a, sa, "A")
  if (!is.null(b) && !is.null(sb)) out <- append_series(out, b, sb, "B")
  if (audit) out[["Snapshot generato il"]] <- a$generated
  out
}
trend_server <- function(id, index, active_tab, refresh_index) {
  moduleServer(id, function(input, output, session) {
    started <- reactiveVal(FALSE)
    revision <- reactiveVal(0L)
    cache <- new.env(parent = emptyenv())
    observeEvent(active_tab(), {
      if (identical(active_tab(), "Trend e confronti")) started(TRUE)
    }, ignoreNULL = FALSE)
    observeEvent(input$refresh, {
      keys <- ls(cache, all.names = TRUE)
      if (length(keys)) rm(list = keys, envir = cache)
      revision(isolate(revision()) + 1L)
      refresh_index()
      showNotification("Storico riletto dagli snapshot locali del servizio.", type = "message", duration = 3)
    })
    history <- reactive({
      req(started())
      revision()
      trend_read_history(index(), cache)
    })
    date_choices <- reactive({
      idx <- index()
      if (!nrow(idx)) return(character())
      idx <- idx[!is.na(idx$data_inizio) & !is.na(idx$data_fine), , drop = FALSE]
      idx <- idx[order(idx$data_inizio), , drop = FALSE]
      idx <- idx[!duplicated(idx$data_inizio), , drop = FALSE]
      setNames(as.character(idx$data_inizio), paste(idx$settimana_id, format(idx$data_inizio, "%d/%m/%Y"), sep = " | "))
    })
    observeEvent(date_choices(), {
      ch <- date_choices()
      if (!length(ch)) return()
      prev_from <- isolate(input$from); prev_to <- isolate(input$to)
      updateSelectInput(session, "from", choices = ch,
        selected = if (length(prev_from) && prev_from %in% ch) prev_from else unname(ch[1L]))
      updateSelectInput(session, "to", choices = ch,
        selected = if (length(prev_to) && prev_to %in% ch) prev_to else unname(tail(ch, 1L)))
    }, ignoreInit = FALSE)
    period <- reactive({
      ch <- date_choices()
      validate(need(length(ch) > 0L, "Nessuno snapshot disponibile per costruire lo storico."))
      dates <- as.Date(unname(ch)); start <- min(dates); end <- max(dates)
      choice <- input$period %||% "all"
      if (choice == "custom") {
        start <- suppressWarnings(as.Date(input$from %||% as.character(start)))
        end <- suppressWarnings(as.Date(input$to %||% as.character(end)))
      } else if (choice %in% c("4", "8", "12")) {
        start <- max(start, end - 7L * (as.integer(choice) - 1L))
      }
      validate(need(length(start) == 1L && length(end) == 1L && !is.na(start) && !is.na(end), "Selezionare un periodo valido."))
      validate(need(start <= end, "La settimana iniziale deve precedere o coincidere con quella finale."))
      list(start = start, end = end)
    })
    selected_history <- reactive({
      h <- history(); p <- period()
      dates <- as.Date(names(h))
      h[!is.na(dates) & dates >= p$start & dates <= p$end]
    })
    a_id <- reactive({
      family <- input$a_family %||% "KPI di servizio"
      ids <- names(trend_catalog)[vapply(trend_catalog, function(m) m$family == family, logical(1))]
      if (!length(ids)) ids <- "kpi_received"
      if (!is.null(input$a_metric) && input$a_metric %in% ids) input$a_metric else ids[1L]
    })
    observeEvent(input$a_family, {
      family <- input$a_family %||% "KPI di servizio"
      rows <- trend_catalog[vapply(trend_catalog, function(m) m$family == family, logical(1))]
      if (!length(rows)) return()
      old <- isolate(input$a_metric)
      updateSelectInput(session, "a_metric", choices = setNames(names(rows), vapply(rows, function(m) m$label, character(1))),
        selected = if (!is.null(old) && old %in% names(rows)) old else names(rows)[1L])
    }, ignoreInit = FALSE)
    b_choices <- reactive(trend_compatible_ids(a_id()))
    b_id <- reactive({
      ids <- b_choices()
      if (!length(ids)) return(NULL)
      if (!is.null(input$b_metric) && input$b_metric %in% ids) input$b_metric else {
        if ("ops_hours" %in% ids) "ops_hours" else ids[1L]
      }
    })
    observeEvent(b_choices(), {
      ids <- b_choices(); old <- isolate(input$b_metric)
      selected <- if (!is.null(old) && old %in% ids) old else if ("ops_hours" %in% ids) "ops_hours" else ids[1L]
      updateSelectInput(session, "b_metric", choices = trend_metric_choices(ids), selected = selected)
    }, ignoreInit = FALSE)
    bind_controls <- function(prefix, id_reactive) {
      previous_id <- NULL
      observeEvent(id_reactive(), {
        mid <- id_reactive(); m <- trend_metric(mid); req(!is.null(m))
        old <- isolate(input[[paste0(prefix, "_mode")]])
        selected <- if (identical(previous_id, mid) && !is.null(old) && old %in% m$modes) old else m$default
        previous_id <<- mid
        updateSelectInput(session, paste0(prefix, "_mode"), choices = setNames(m$modes, unname(trend_mode_labels[m$modes])), selected = selected)
      }, ignoreInit = FALSE)
      observeEvent(list(selected_history(), id_reactive()), {
        values <- trend_categories(selected_history(), id_reactive())
        old <- isolate(input[[paste0(prefix, "_category")]])
        selected <- if (!is.null(old) && old %in% values) old else if (length(values)) values[1L] else character()
        updateSelectInput(session, paste0(prefix, "_category"), choices = values, selected = selected)
      }, ignoreInit = FALSE)
    }
    bind_controls("a", a_id)
    bind_controls("b", b_id)
    make_spec <- function(prefix, id_reactive) {
      mid <- id_reactive(); m <- trend_metric(mid); req(!is.null(m))
      category <- input[[paste0(prefix, "_category")]] %||% ""
      if (m$source == "distribution" && identical(prefix, "a") && !isTRUE(input$a_detail_enabled)) {
        # Nessun dettaglio specifico: la vista principale diventa automaticamente Top 10.
        category <- ""
      } else if (m$source %in% c("distribution", "quality_variable")) {
        choices <- trend_categories(selected_history(), mid)
        if (!category %in% choices) category <- if (length(choices)) choices[1L] else ""
      }
      trend_spec(mid, input[[paste0(prefix, "_mode")]], category,
        input[[paste0(prefix, "_detail")]] %||% "group", input[[paste0(prefix, "_channel")]] %||% "all")
    }
    spec_a <- reactive(make_spec("a", a_id))
    spec_b <- reactive({ req(isTRUE(input$compare)); make_spec("b", b_id) })

    # Il confronto A/B richiede una sola serie per A. Se l'utente passa alla
    # panoramica Top 10 di una composizione, il confronto viene disattivato.
    observe({
      a <- spec_a()
      if (trend_is_distribution_overview(a) && isTRUE(input$compare)) {
        updateCheckboxInput(session, "compare", value = FALSE)
        showNotification(
          "Per confrontare due indicatori seleziona prima una categoria specifica dell'indicatore A.",
          type = "message", duration = 4
        )
      }
    })

    top_a <- reactive({
      a <- spec_a()
      if (!trend_is_distribution_overview(a)) return(NULL)
      p <- period()
      trend_build_top_distribution(history(), p$start, p$end, a, top_n = 10L)
    })
    series_a <- reactive({
      a <- spec_a()
      if (trend_is_distribution_overview(a)) return(data.frame())
      p <- period(); trend_build_series(history(), p$start, p$end, a)
    })
    series_b <- reactive({ req(isTRUE(input$compare)); p <- period(); trend_build_series(history(), p$start, p$end, spec_b()) })
    association <- reactive(trend_association(series_a(), series_b(), spec_a(), spec_b(), input$basis %||% "levels"))

    output$period_info <- renderUI({
      p <- period(); h <- selected_history()
      expected <- as.integer(p$end - p$start) %/% 7L + 1L
      usable <- sum(vapply(h, function(x) !is.null(x$data), logical(1)))
      div(class = "snapshot-strip", strong(SERVIZIO_LABEL), " | ", fmt_week_it(p$start, p$end + 6),
        " | ", usable, " snapshot leggibili / ", expected, " settimane di calendario",
        if (usable < expected) span(". Le settimane non disponibili restano ND; nessuna interpolazione."))
    })
    output$indicator_notes <- renderUI({
      a <- spec_a(); ma <- trend_metric(a$id)
      overview <- trend_is_distribution_overview(a)
      div(
        div(strong("A: "), if (overview) {
          paste0(ma$label, " - Top 10 nel periodo (", trend_mode_labels[[a$mode]], ")")
        } else trend_spec_label(a)),
        if (overview) div(
          class = "snapshot-strip",
          paste(
            "Vista panoramica: vengono mostrate insieme le prime 10 categorie del periodo selezionato,",
            "ordinate per volume cumulato. Il dettaglio di una singola categoria e' facoltativo."
          )
        ),
        div(class = "panel-note", ma$note),
        if (overview && isTRUE(input$rolling)) div(
          class = "panel-note",
          "La media mobile a 4 settimane non viene sovrapposta nella vista Top 10 per evitare di raddoppiare il numero di linee. Attiva un dettaglio specifico per usarla."
        ),
        if (overview) div(
          class = "panel-note",
          "Per il confronto con un secondo indicatore e per la correlazione e' necessario selezionare una categoria specifica."
        ),
        if (isTRUE(input$compare)) {
          b <- spec_b(); pol <- trend_pair_policy(a, b)
          tagList(div(strong("B: "), trend_spec_label(b)),
            div(class = "panel-note", trend_metric(b$id)$note),
            div(class = if (pol$correlate) "snapshot-strip" else "privacy-strip", pol$reason,
                if (nzchar(pol$caution)) paste0(" ", pol$caution)))
        }
      )
    })
    output$title_a <- renderUI({
      a <- spec_a(); m <- trend_metric(a$id)
      if (trend_is_distribution_overview(a)) {
        div(class = "panel-note", strong(paste0(m$label, " - Top 10 nel periodo (", trend_mode_labels[[a$mode]], ")")))
      } else div(class = "panel-note", strong(trend_spec_label(a)))
    })
    output$title_b <- renderUI({ req(isTRUE(input$compare)); div(class = "panel-note", strong(trend_spec_label(spec_b()))) })
    output$cards_a <- renderUI({
      a <- spec_a()
      if (trend_is_distribution_overview(a)) trend_top_cards_ui(top_a(), a)
      else trend_cards_ui(series_a(), a, "A")
    })
    output$cards_b <- renderUI({ req(isTRUE(input$compare)); trend_cards_ui(series_b(), spec_b(), "B") })
    output$plot_a <- renderPlot({
      a <- spec_a()
      if (trend_is_distribution_overview(a)) {
        p <- trend_top_plot(top_a(), a)
        validate(need(!is.null(p), "Nessuna categoria valutabile nel periodo selezionato."))
        p
      } else {
        z <- series_a(); validate(need(any(is.finite(z$value)), "Nessun valore valutabile per questa selezione: consultare gli stati nella tabella."))
        trend_plot(z, a, isTRUE(input$rolling), isTRUE(input$labels))
      }
    }, res = 144)
    output$plot_b <- renderPlot({
      req(isTRUE(input$compare)); z <- series_b()
      validate(need(any(is.finite(z$value)), "Nessun valore valutabile per il secondo indicatore."))
      trend_plot(z, spec_b(), isTRUE(input$rolling), isTRUE(input$labels),
        color = if (SERVIZIO_APP == "114") TA_ORANGE else TA_BLUE)
    }, res = 144)
    output$base_message <- renderUI({
      req(isTRUE(input$compare))
      if (is.null(trend_base100(series_a(), series_b()))) {
        return(div(class = "privacy-strip", "Indice base 100 non disponibile: nella prima settimana del periodo uno dei due valori e' nullo o mancante. Selezionare esplicitamente un'altra settimana iniziale."))
      }
      div(class = "panel-note", "A = indicatore principale; B = secondo indicatore. La normalizzazione confronta le variazioni relative, non i livelli assoluti.")
    })
    output$plot_base <- renderPlot({
      req(isTRUE(input$compare))
      p <- trend_index_plot(series_a(), series_b())
      validate(need(!is.null(p), "Base comune non disponibile."))
      p
    }, res = 144)
    output$association_info <- renderUI({
      req(isTRUE(input$compare)); a <- association()
      tagList(div(class = "kpi-grid trend-kpi-small",
        kpi_card("Spearman (rho)", if (is.finite(a$rho)) fmt_num(a$rho, 3) else "ND"),
        kpi_card(if ((input$basis %||% "levels") == "changes") "Coppie di variazioni valide" else "Settimane appaiate", fmt_int(a$n))),
        div(class = "privacy-strip", a$status),
        if (nzchar(a$policy$caution)) div(class = "panel-note", a$policy$caution))
    })
    output$scatter <- renderPlot({
      req(isTRUE(input$compare)); a <- association()
      validate(need(a$policy$graph, a$policy$reason))
      p <- trend_scatter(a, spec_a(), spec_b(), input$basis %||% "levels", isTRUE(input$labels))
      validate(need(!is.null(p), "Nessuna coppia di valori valutabili."))
      p
    }, res = 144)
    table_data <- reactive({
      a <- spec_a()
      if (trend_is_distribution_overview(a)) {
        trend_top_table_data(top_a(), a, isTRUE(input$audit))
      } else {
        trend_table_data(series_a(), a,
          if (isTRUE(input$compare)) series_b() else NULL,
          if (isTRUE(input$compare)) spec_b() else NULL, isTRUE(input$audit))
      }
    })
    output$table <- renderDT({
      x <- table_data()
      validate(need(nrow(x) > 0, "Nessun dato disponibile per la tabella."))
      overview <- trend_is_distribution_overview(spec_a())
      dt <- DT::datatable(x, rownames = FALSE, selection = "none", escape = TRUE,
        filter = if (overview) "top" else "none",
        options = list(pageLength = if (overview) 25 else 12,
          lengthMenu = c(12, 25, 50, 100, 250), scrollX = TRUE,
          order = list(), language = list(search = "Cerca:",
            lengthMenu = if (overview) "Mostra _MENU_ righe" else "Mostra _MENU_ settimane",
            info = if (overview) "Righe _START_-_END_ di _TOTAL_" else "Settimane _START_-_END_ di _TOTAL_",
            zeroRecords = "Nessun risultato",
            paginate = list(previous = "Precedente", `next` = "Successiva"))))
      numeric_cols <- names(x)[vapply(x, is.numeric, logical(1))]
      if (length(numeric_cols)) dt <- DT::formatRound(dt, columns = numeric_cols, digits = 2,
        dec.mark = ",", mark = ".")
      dt
    }, server = FALSE)
    output$download <- downloadHandler(
      filename = function() { p <- period(); paste0("trend_", SERVIZIO_APP, "_", format(p$start, "%Y%m%d"), "_", format(p$end + 6, "%Y%m%d"), ".csv") },
      content = function(file) {
        a <- spec_a()
        if (trend_is_distribution_overview(a)) {
          x <- trend_top_table_data(top_a(), a, TRUE)
          x[["Indicatore A"]] <- paste0(trend_metric(a$id)$label, " - Top 10 nel periodo")
        } else {
          x <- trend_table_data(series_a(), a, if (isTRUE(input$compare)) series_b() else NULL,
            if (isTRUE(input$compare)) spec_b() else NULL, TRUE)
          x[["Indicatore A"]] <- trend_spec_label(a)
          if (isTRUE(input$compare)) x[["Indicatore B"]] <- trend_spec_label(spec_b())
        }
        for (name in names(x)) if (is.character(x[[name]])) {
          risky <- !is.na(x[[name]]) & grepl("^[[:space:]]*[=+@-]", x[[name]])
          x[[name]][risky] <- paste0("'", x[[name]][risky])
        }
        utils::write.table(x, file, sep = ";", dec = ",", row.names = FALSE, na = "",
          fileEncoding = "UTF-8", qmethod = "double")
      }, contentType = "text/csv; charset=UTF-8")
    output$history_notes <- renderUI({
      h <- selected_history()
      bad <- h[vapply(h, function(e) !is.null(e$error), logical(1))]
      notes <- c(
        "I totali e i rapporti riguardano solo le settimane valutabili. Una media settimanale semplice e un rapporto sulle somme sono riportati come misure diverse.",
        "Le percentuali variano in punti percentuali. La media mobile richiede quattro settimane consecutive con valori validi: nessun riempimento dei buchi.",
        "Una categoria assente in una distribuzione completa vale zero. Tabella assente, colonna non esportata o denominatore nullo producono ND, non zero.",
        "Completezza e stato dei casi sono fotografie dell'export usato: uno snapshot ricostruito dopo mesi non descrive necessariamente lo stato alla fine di quella settimana.",
        "Verificare continuita' delle regole di analisi, copertura delle fonti e mappatura degli operatori, soprattutto nelle settimane storiche.",
        "Le quote CRM descrivono aggregati settimanali: un'associazione fra due quote non dimostra che le due condizioni riguardino gli stessi casi.",
        "Nelle composizioni CRM, senza dettaglio specifico vengono mostrate le prime 10 categorie del periodo, ordinate per somma dei conteggi settimanali; il grafico usa poi Numero o Percentuale secondo la misura selezionata.",
        "Il minimo di 8 coppie e l'avvertenza fino a 11 sono regole operative della dashboard, non garanzie statistiche. Tutte le associazioni restano esplorative."
      )
      tagList(tags$ul(class = "method-list", lapply(notes, tags$li)),
        if (length(bad)) div(class = "privacy-strip", strong("Snapshot non utilizzabili: "),
          tags$ul(lapply(bad, function(e) tags$li(e$file, ": ", e$error)))))
    })
    output$catalog_table <- renderDT({
      x <- do.call(rbind, lapply(trend_catalog, function(m) data.frame(Famiglia = m$family,
        Indicatore = m$label, Misure = paste(trend_mode_labels[m$modes], collapse = " / "),
        Fonte = switch(m$source, kpi = "KPI settimanali canale-servizio",
          crm = "CRM - tab_overview", distribution = paste("CRM -", m$table),
          ops = "Attivita' operatori - somme settimanali", quality = "Completezza - riepilogo",
          quality_variable = "Completezza - variabili"), Note = m$note, stringsAsFactors = FALSE)))
      DT::datatable(x, rownames = FALSE, selection = "none", escape = TRUE,
        options = list(pageLength = 10, scrollX = TRUE, order = list()))
    }, server = FALSE)
    list(period = period, series_a = series_a, series_b = series_b, association = association)
  })
}

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML(css_text)),
    tags$style(HTML(".trend-kpi-small { grid-template-columns: repeat(2, minmax(140px, 1fr)); } @media (max-width:430px) { .trend-kpi-small { grid-template-columns:1fr; } }"))
  ),

  div(
    class = "app-header",
    div(class = "app-kicker", paste("Telefono Azzurro ·", SERVIZIO_LABEL_BREVE)),
    div(class = "app-title", paste("Dashboard settimanale ·", SERVIZIO_LABEL)),
    div(class = "app-subtitle", uiOutput("header_subtitle"))
  ),

  conditionalPanel(
    condition = "input.main_tab !== 'Trend e confronti'",
  div(
    class = "filter-panel",
    fluidRow(
      column(6, selectInput("settimana", "Settimana", choices = NULL, width = "100%")),
      column(3, br(), actionButton("refresh_data", "Aggiorna elenco", class = "btn-primary", width = "100%")),
      column(3, br(), div(class = "small-muted", textOutput("snapshot_count", inline = TRUE)))
    )
  ),

  uiOutput("snapshot_info")
  ),
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
    tabPanel("Trend e confronti", trend_ui("trend")),

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

  trend_controller <- trend_server(
    "trend", index = snapshot_index, active_tab = reactive(input$main_tab),
    refresh_index = function() {
      current <- isolate(input$settimana)
      snapshot_index(scan_snapshots())
      refresh_week_selector(current)
    }
  )

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
    if (identical(input$main_tab, "Trend e confronti")) {
      return(span("Trend e confronti | ", SERVIZIO_LABEL, " | storico settimanale disponibile"))
    }
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
