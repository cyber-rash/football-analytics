# ============================================================================
#  FOOTBALL EVENTS DASHBOARD  —  app.R
#  R + Shiny | Tund tema, klub rengine gore dinamik aksent, hibrid naviqasiya
#  ---------------------------------------------------------------------------
#  ISTIFADE:  app.R + butun *.rds fayllari + club_logos.rds eyni qovluqda olsun.
#             (istege gore:  www/stadium.jpg  arxa fon ucun)
#  Paketler:  install.packages(c("shiny","ggplot2","dplyr","tidyr",
#                                "scales","stringr","httr"))
#  Ishe salmaq:  RStudio-da "Run App"  (ve ya  shiny::runApp())
# ============================================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(stringr)
library(httr)

# ---------------------------------------------------------------------------
# 1. DATA
# ---------------------------------------------------------------------------
rd <- function(f) readRDS(f)

club_summary   <- rd("club_summary_top5.rds")
club_colors    <- rd("club_colors.rds")
club_trend     <- rd("club_season_trend.rds")
club_style     <- rd("club_style_profile.rds")
club_home_adv  <- rd("club_home_advantage.rds")
club_logos     <- rd("club_logos.rds")
clean_sheets   <- rd("clean_sheets.rds")

player_summary <- rd("player_summary_top5.rds")
player_home    <- rd("player_home_away.rds")
player_radar   <- rd("player_radar_profile.rds")
player_matches <- rd("player_match_goals.rds")

league_summary <- rd("league_season_summary.rds")
red_card       <- rd("red_card_impact.rds")
comebacks      <- rd("biggest_comebacks.rds")
high_scoring   <- rd("highest_scoring_games.rds")

# insights.rds opsionaldir: prep_insights.R ile yaradilir. Yoxdursa Analitika
# tab-i telimat gosterir, app cokmur.
INSIGHTS <- tryCatch(readRDS("insights.rds"), error = function(e) NULL)
HAS_INSIGHTS <- !is.null(INSIGHTS)

# bar-chart-race VIDEO-lari (prep_bar_race.R ile www/ qovlugunda yaradilir).
# league_name -> fayl adi xeritesi. Fayl yoxdursa kart sadece gorunmur.
RACE_VIDEO_MAP <- c(
  "Bundesliga"     = "race_bundesliga.mp4",
  "Premier League" = "race_premier_league.mp4",
  "Ligue 1"        = "race_ligue_1.mp4",
  "Serie A"        = "race_serie_a.mp4",
  "La Liga"        = "race_la_liga.mp4"
)

# --- Global KPI-lar --------------------------------------------------------
KPI <- list(
  games   = sum(league_summary$games),
  goals   = sum(league_summary$total_goals),
  leagues = dplyr::n_distinct(league_summary$league),
  seasons = dplyr::n_distinct(league_summary$season),
  clubs   = nrow(club_summary),
  players = nrow(player_summary)
)

# --- Seciler ucun siyahilar ------------------------------------------------
leagues_vec <- sort(unique(club_summary$league_name))
# klublar liqaya gore qruplanmis
clubs_by_league <- split(club_summary$club[order(club_summary$club)],
                         club_summary$league_name[order(club_summary$club)])
players_choices <- setNames(player_summary$player_label,
                            paste0(player_summary$player_label,
                                   "  \u2014  ", player_summary$goals, " qol"))
players_choices <- players_choices[order(-player_summary$goals)]

DEFAULT_ACCENT <- "#22d38b"   # klub secilmeyende: emerald "cemen" aksenti
DEFAULT_ACCENT2 <- "#0f5132"

# ---------------------------------------------------------------------------
# 2. KOMEKCI FUNKSIYALAR
# ---------------------------------------------------------------------------

# --- klub rengi -----------------------------------------------------------
club_color <- function(club, which = "primary_color") {
  row <- club_colors[club_colors$club == club, ]
  if (nrow(row) == 0) return(if (which == "primary_color") DEFAULT_ACCENT else DEFAULT_ACCENT2)
  val <- row[[which]][1]
  if (is.na(val) || val == "") return(if (which == "primary_color") DEFAULT_ACCENT else DEFAULT_ACCENT2)
  val
}

# --- klub loqosu / fallback badge -----------------------------------------
club_logo_url <- function(club) {
  row <- club_logos[club_logos$club_name == club, ]
  if (nrow(row) == 0 || is.na(row$logo_url[1])) return(NA_character_)
  row$logo_url[1]
}

# rengli initial-badge (loqo tapilmayanda)
club_badge <- function(club, color, size = 96) {
  initials <- toupper(substr(gsub("[^A-Za-z ]", "", club), 1, 1))
  words <- strsplit(club, " ")[[1]]
  if (length(words) >= 2) initials <- toupper(paste0(substr(words[1],1,1), substr(words[2],1,1)))
  div(class = "club-badge",
      style = sprintf("width:%dpx;height:%dpx;background:%s;font-size:%dpx;",
                      size, size, color, round(size/2.5)),
      initials)
}

logo_or_badge <- function(club, color, size = 96) {
  url <- club_logo_url(club)
  if (is.na(url)) return(club_badge(club, color, size))
  div(class = "logo-wrap", style = sprintf("width:%dpx;height:%dpx;", size, size),
      img(src = url, style = "max-width:100%;max-height:100%;object-fit:contain;"))
}

# --- oyuncu sekli (Wikipedia REST, sessiya keshi ile) ----------------------
.photo_cache <- new.env(parent = emptyenv())
player_photo_url <- function(player_name) {
  key <- player_name
  if (!is.null(.photo_cache[[key]])) return(.photo_cache[[key]])
  page <- str_to_title(player_name)
  url <- paste0("https://en.wikipedia.org/api/rest_v1/page/summary/",
                URLencode(page, reserved = FALSE))
  res <- tryCatch(GET(url, timeout(6)), error = function(e) NULL)
  out <- NA_character_
  if (!is.null(res) && status_code(res) == 200) {
    data <- tryCatch(content(res, as = "parsed", simplifyVector = FALSE),
                     error = function(e) NULL)
    if (!is.null(data$thumbnail)) out <- data$thumbnail$source
  }
  .photo_cache[[key]] <- out
  out
}

# --- ggplot tund temasi ----------------------------------------------------
theme_dash <- function(base = 13) {
  theme_minimal(base_size = base) +
    theme(
      plot.background  = element_rect(fill = "#141821", color = NA),
      panel.background = element_rect(fill = "#141821", color = NA),
      panel.grid.major = element_line(color = "#232a36", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      text        = element_text(color = "#c7cdd8", family = ""),
      axis.text   = element_text(color = "#8b93a3"),
      axis.title  = element_text(color = "#9aa2b1"),
      plot.title  = element_text(color = "#e9ecf2", face = "bold", size = base + 2),
      plot.subtitle = element_text(color = "#8b93a3"),
      strip.text  = element_text(color = "#dfe3ea", face = "bold"),
      legend.background = element_rect(fill = "#141821", color = NA),
      legend.key  = element_rect(fill = "#141821", color = NA),
      legend.text = element_text(color = "#c7cdd8"),
      legend.title = element_text(color = "#9aa2b1"),
      plot.margin = margin(10, 14, 10, 10)
    )
}

# --- radar (coord_polar) ---------------------------------------------------
radar_plot <- function(scores, labels, accent) {
  df <- data.frame(label = factor(labels, levels = labels), score = as.numeric(scores))
  ggplot(df, aes(x = label, y = score, group = 1)) +
    geom_hline(yintercept = c(20, 40, 60, 80, 100), color = "#2a323f", linewidth = 0.35) +
    geom_polygon(fill = accent, alpha = 0.30, color = accent, linewidth = 1.1) +
    geom_point(color = accent, size = 3) +
    geom_text(aes(label = round(score)), color = "#e9ecf2", vjust = -1.1, size = 3.4) +
    coord_polar() +
    scale_y_continuous(limits = c(0, 108), expand = c(0, 0)) +
    theme_dash() +
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#232a36", linewidth = 0.3),
          axis.text.x = element_text(color = "#dfe3ea", size = 11, face = "bold"))
}

# --- insight komekci qrafikleri --------------------------------------------
# ufuqi bar (kateqoriya vs deyer) — deyere gore siralanir, etiketlenir
hbar <- function(df, cat, val, accent, suffix = "") {
  df <- df[order(df[[val]]), ]
  df[[cat]] <- factor(df[[cat]], levels = df[[cat]])
  df$.lbl <- paste0(format(df[[val]], big.mark = ","), suffix)
  ggplot(df, aes(x = .data[[val]], y = .data[[cat]])) +
    geom_col(fill = accent, width = 0.66) +
    geom_text(aes(label = .lbl), hjust = -0.12, color = "#e9ecf2", size = 3.6) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.24))) +
    labs(x = NULL, y = NULL) + theme_dash()
}
# suqun bar (deqiqe/kateqoriya vs deyer)
vbar <- function(df, cat, val, accent, suffix = "") {
  ggplot(df, aes(x = .data[[cat]], y = .data[[val]])) +
    geom_col(fill = accent, width = 0.66) +
    geom_text(aes(label = paste0(format(.data[[val]], big.mark = ","), suffix)),
              vjust = -0.5, color = "#e9ecf2", size = 3.6) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
    labs(x = NULL, y = NULL) + theme_dash()
}
# ---------------------------------------------------------------------------
# 3. CSS  (F1-uslublu tund dashboard + dinamik aksent)
# ---------------------------------------------------------------------------
APP_CSS <- HTML("
@import url('https://fonts.googleapis.com/css2?family=Oswald:wght@400;500;600;700&family=Inter:wght@400;500;600;700&display=swap');

:root{
  --accent:#22d38b; --accent2:#0f5132;
  --bg:#0b0e14; --panel:#141821; --panel2:#1a1f2b; --line:#232a36;
  --txt:#e9ecf2; --muted:#8b93a3;
  --display:'Oswald',sans-serif; --body:'Inter',system-ui,sans-serif;
}
*{box-sizing:border-box}
body,.container-fluid{
  background:
    radial-gradient(1100px 480px at 50% -8%, color-mix(in srgb,var(--accent) 9%,transparent), transparent 62%),
    var(--bg);
  color:var(--txt);font-family:var(--body);margin:0}
.shell{display:flex;min-height:100vh;position:relative}
/* istege gore: www/stadium.jpg qoyulsa, yuxarida solgun stadion fonu gorunur */
.shell::before{content:'';position:fixed;inset:0;z-index:-1;pointer-events:none;
  background:url('stadium.jpg') center top/cover no-repeat;opacity:.10;
  -webkit-mask-image:linear-gradient(180deg,rgba(0,0,0,.9),transparent 55%);
          mask-image:linear-gradient(180deg,rgba(0,0,0,.9),transparent 55%)}

/* ---- Sidebar ---- */
.sidebar{width:236px;flex-shrink:0;background:linear-gradient(180deg,#10141d,#0b0e14);
  border-right:1px solid var(--line);padding:22px 14px;position:sticky;top:0;height:100vh}
.brand{display:flex;align-items:center;gap:10px;padding:4px 8px 22px}
.brand .dot{width:12px;height:12px;border-radius:3px;background:var(--accent);
  box-shadow:0 0 12px var(--accent);transition:.4s}
.brand .name{font-family:var(--display);font-weight:700;font-size:20px;letter-spacing:.5px;
  text-transform:uppercase;line-height:1}
.brand .sub{color:var(--muted);font-size:10px;letter-spacing:2px;text-transform:uppercase}
.nav-item-link{display:flex;align-items:center;gap:12px;padding:11px 14px;margin:3px 0;
  border-radius:10px;color:var(--muted);text-decoration:none;font-weight:500;font-size:14px;
  border-left:3px solid transparent;cursor:pointer;transition:.18s}
.nav-item-link:hover{background:var(--panel);color:var(--txt)}
.nav-item-link.active{background:var(--panel);color:var(--txt);
  border-left:3px solid var(--accent);box-shadow:inset 0 0 0 1px var(--line)}
.nav-item-link.active i{color:var(--accent)}
.nav-item-link i{width:18px;text-align:center;font-size:15px}
.side-foot{position:absolute;bottom:18px;left:14px;right:14px;color:var(--muted);
  font-size:10px;letter-spacing:1px;text-transform:uppercase;border-top:1px solid var(--line);
  padding-top:12px}

/* ---- Main ---- */
.main{flex:1;min-width:0;padding:22px 26px 40px}
.topbar{display:flex;justify-content:space-between;align-items:flex-end;margin-bottom:18px;
  flex-wrap:wrap;gap:12px}
.page-title{font-family:var(--display);font-weight:600;font-size:30px;text-transform:uppercase;
  letter-spacing:1px;line-height:1;margin:0}
.page-sub{color:var(--muted);font-size:13px;margin-top:6px}

/* ---- KPI cards ---- */
.kpi-row{display:grid;grid-template-columns:repeat(5,1fr);gap:14px;margin-bottom:22px}
.kpi{background:linear-gradient(160deg,var(--panel2),var(--panel));border:1px solid var(--line);
  border-radius:14px;padding:16px 18px;position:relative;overflow:hidden;transition:.2s}
.kpi:hover{transform:translateY(-3px);border-color:var(--accent)}
.kpi::before{content:'';position:absolute;top:0;left:0;right:0;height:3px;
  background:linear-gradient(90deg,var(--accent),transparent)}
.kpi .num{font-family:var(--display);font-weight:700;font-size:34px;line-height:1;color:var(--txt)}
.kpi .lbl{color:var(--muted);font-size:11px;letter-spacing:1.5px;text-transform:uppercase;margin-top:6px}
.kpi .ic{position:absolute;right:14px;top:14px;color:var(--accent);opacity:.35;font-size:20px}

/* ---- Cards / panels ---- */
.card2{background:var(--panel);border:1px solid var(--line);border-radius:16px;padding:20px;
  margin-bottom:18px}
.card2 .card-h{font-family:var(--display);text-transform:uppercase;letter-spacing:1px;
  font-weight:600;font-size:15px;color:var(--txt);margin:0 0 14px;
  border-left:3px solid var(--accent);padding-left:10px}
.grid-2{display:grid;grid-template-columns:1fr 1fr;gap:18px}
.grid-3{display:grid;grid-template-columns:repeat(3,1fr);gap:18px}
@media(max-width:1100px){.grid-2,.grid-3{grid-template-columns:1fr}.kpi-row{grid-template-columns:repeat(2,1fr)}}

/* ---- stat mini-grid ---- */
.stat-grid{display:grid;grid-template-columns:repeat(4,1fr);gap:12px}
.stat{background:var(--panel2);border:1px solid var(--line);border-radius:11px;padding:12px 14px}
.stat .v{font-family:var(--display);font-weight:700;font-size:24px;color:var(--accent);line-height:1}
.stat .k{color:var(--muted);font-size:10.5px;letter-spacing:1px;text-transform:uppercase;margin-top:5px}
@media(max-width:900px){.stat-grid{grid-template-columns:repeat(2,1fr)}}

/* ---- club/player hero ---- */
.hero{display:flex;gap:22px;align-items:center;background:
  radial-gradient(120% 140% at 0% 0%,color-mix(in srgb,var(--accent) 22%,transparent),transparent 60%),
  var(--panel);border:1px solid var(--line);border-radius:16px;padding:22px;margin-bottom:18px}
.hero .htext .hname{font-family:var(--display);font-weight:700;font-size:30px;text-transform:uppercase;
  letter-spacing:.5px;line-height:1.05}
.hero .htext .hmeta{color:var(--muted);font-size:13px;margin-top:6px}
.club-badge{border-radius:16px;display:flex;align-items:center;justify-content:center;
  font-family:var(--display);font-weight:700;color:#fff;flex-shrink:0;
  box-shadow:0 6px 22px rgba(0,0,0,.4)}
.logo-wrap{display:flex;align-items:center;justify-content:center;background:#fff;border-radius:16px;
  padding:10px;flex-shrink:0;box-shadow:0 6px 22px rgba(0,0,0,.4)}
.photo-wrap{width:150px;height:150px;border-radius:16px;overflow:hidden;flex-shrink:0;
  border:2px solid var(--accent);box-shadow:0 6px 22px rgba(0,0,0,.45);background:var(--panel2)}
.photo-wrap img{width:100%;height:100%;object-fit:cover}

/* ---- badges / chips ---- */
.chip{display:inline-block;background:var(--panel2);border:1px solid var(--line);
  border-radius:20px;padding:4px 12px;font-size:12px;color:var(--muted);margin-right:6px}
.chip.accent{background:color-mix(in srgb,var(--accent) 18%,transparent);
  border-color:var(--accent);color:var(--txt)}

/* ---- stories ---- */
.story{background:linear-gradient(160deg,var(--panel2),var(--panel));border:1px solid var(--line);
  border-radius:16px;padding:22px;position:relative;overflow:hidden;height:100%}
.story::before{content:'';position:absolute;inset:0 auto 0 0;width:4px;background:var(--accent)}
.story .st-eyebrow{color:var(--accent);font-family:var(--display);text-transform:uppercase;
  letter-spacing:2px;font-size:12px;font-weight:600}
.story .st-big{font-family:var(--display);font-weight:700;font-size:40px;line-height:1;margin:8px 0}
.story .st-title{font-weight:600;font-size:16px;color:var(--txt);margin-bottom:6px}
.story .st-body{color:var(--muted);font-size:13.5px;line-height:1.5}

/* ---- form controls (dark) ---- */
.selectize-input,.selectize-dropdown,.form-control{background:var(--panel2)!important;
  color:var(--txt)!important;border:1px solid var(--line)!important;border-radius:10px!important}
.selectize-dropdown .active{background:var(--accent)!important;color:#08120c!important}
.control-label{color:var(--muted)!important;font-size:12px;letter-spacing:.5px;
  text-transform:uppercase;font-weight:600}
.irs-bar,.irs-handle>i:first-child{background:var(--accent)!important}
hr{border-color:var(--line)}
.small-muted{color:var(--muted);font-size:12px}
.insight-note{margin-top:12px;padding:10px 14px;border-radius:10px;font-size:13.5px;
  line-height:1.5;color:var(--txt);background:color-mix(in srgb,var(--accent) 12%,transparent);
  border-left:3px solid var(--accent)}
.insight-note b{color:var(--accent);font-family:var(--display);font-weight:700}
.race-controls{display:flex;gap:12px;justify-content:center;margin-top:16px}
.race-btn{display:flex;align-items:center;gap:8px;padding:10px 22px;border-radius:10px;
  border:1px solid var(--line);background:var(--panel2);color:var(--txt);font-weight:600;
  font-size:14px;cursor:pointer;transition:.15s;font-family:var(--body)}
.race-btn:hover{border-color:var(--accent);background:color-mix(in srgb,var(--accent) 14%,var(--panel2))}
.race-btn-play{background:var(--accent);color:#08120c;border-color:var(--accent)}
.race-btn-play:hover{filter:brightness(1.1)}
")

# JS: naviqasiya kliklerini birbasha idare edir (action-button bagliligindan asili deyil)
NAV_JS <- HTML("
(function(){
  function attach(){
    var links = document.querySelectorAll('.nav-item-link');
    links.forEach(function(el){
      el.addEventListener('click', function(e){
        e.preventDefault();
        var tab = el.getAttribute('data-tab');
        links.forEach(function(n){ n.classList.remove('active'); });
        el.classList.add('active');
        if (window.Shiny && Shiny.setInputValue){
          Shiny.setInputValue('nav_tab', tab, {priority:'event'});
        }
      });
    });
  }
  if (document.readyState === 'loading'){
    document.addEventListener('DOMContentLoaded', attach);
  } else { attach(); }
})();
")

# ---------------------------------------------------------------------------
# 4. UI komekcileri
# ---------------------------------------------------------------------------
nav_link <- function(tab, icon_name, label, active = FALSE) {
  a(class = paste("nav-item-link", if (active) "active" else ""),
    href = "#", `data-tab` = tab,
    icon(icon_name), span(label))
}
kpi_card <- function(num, lbl, icon_name) {
  div(class = "kpi",
      icon(icon_name, class = "ic"),
      div(class = "num", num),
      div(class = "lbl", lbl))
}
stat_box <- function(v, k) div(class = "stat", div(class = "v", v), div(class = "k", k))
card2 <- function(title, ...) div(class = "card2", if (!is.null(title)) div(class = "card-h", title), ...)

# kicik bolme basligi (tab daxilinde)
sect <- function(txt) div(style = "font-family:var(--display);text-transform:uppercase;letter-spacing:1.5px;color:var(--muted);font-size:13px;margin:8px 2px 14px;", txt)
note <- function(html) div(class = "insight-note", HTML(html))

# --- Analitika tab-inin qurulmasi ------------------------------------------
insights_tab <- function() {
  head_ui <- div(class = "topbar",
    div(h1(class = "page-title", "Analitika"),
        div(class = "page-sub", "Datadan cixan numuneler — data analitik baxisi")))

  if (!HAS_INSIGHTS) {
    return(tagList(head_ui,
      div(class = "card2",
        div(class = "card-h", "Bir addim qalib"),
        p(class = "small-muted", style = "font-size:14px;line-height:1.6",
          HTML(paste0(
            "Bu bolme <b style='color:var(--accent)'>insights.rds</b> faylini teleb edir ",
            "(qol deqiqeleri, situasiya, assist, beden hissesi, intizam ve s. aqreqasiyalari).<br><br>",
            "RStudio-da <b>full_data</b> movcud olan sessiyada bir defe bunu isle:<br>",
            "<code style='color:var(--accent)'>source(\"prep_insights.R\")</code><br><br>",
            "Bu, <b>insights.rds</b> faylini yaradacaq. Onu app.R ile eyni qovluqda saxla ",
            "ve app-i yeniden isle — Analitika bolmesi avtomatik dolacaq.")))))
    )
  }

  I <- INSIGHTS
  pick <- function(df, kc, k, vc) { v <- df[[vc]][df[[kc]] == k]; if (length(v)) v[1] else NA }

  gm <- I$goal_minutes; pk <- as.character(gm$time_bin[which.max(gm$n)])
  open_pct <- pick(I$situation, "situation_name", "Open play", "pct")
  ta <- I$assist[which.max(I$assist$n), ]
  bp <- I$bodypart[which.max(I$bodypart$conversion), ]
  kb_pct <- pick(I$fast_break, "label", "Kontratak", "pct")
  ot_pct <- pick(I$shot_outcome, "shot_outcome_name", "On target", "pct")
  dl <- I$discipline[which.max(I$discipline$yellow_pg), ]
  cc <- I$conversion_clubs[which.max(I$conversion_clubs$conversion), ]
  yp <- I$yellow_players[which.max(I$yellow_players$n), ]

  tagList(head_ui,
    sect("Qol anatomiyasi"),
    div(class = "grid-2",
      card2("Qollarin deqiqe paylanmasi",
        plotOutput("ins_goal_minutes", height = "260px"),
        note(sprintf("Qollarin en cox vuruldugu interval: <b>%s deqiqe</b> (%s qol). Oyunun sonuna dogru qol ehtimali artir.",
                     pk, format(max(gm$n), big.mark = ",")))),
      card2("Qollar situasiyaya gore",
        plotOutput("ins_situation", height = "260px"),
        note(sprintf("Qollarin <b>%.1f%%</b>-i acik oyunda vurulub — standart veziyyetlerden (kunc, cerime, azad zerbe) qat-qat cox.",
                     open_pct)))
    ),
    div(class = "grid-2",
      card2("Qollarda assist metodu",
        plotOutput("ins_assist", height = "260px"),
        note(sprintf("En cox qol <b>%s</b> ile gelir (%s qol, %.1f%%).",
                     ta$assist_method_name, format(ta$n, big.mark = ","), ta$pct))),
      card2("Beden hissesine gore effektivlik",
        plotOutput("ins_bodypart", height = "260px"),
        note(sprintf("En yuksek cevrilme <b>%s</b> ile: <b>%.1f%%</b>. Baxmayaraq ki, en cox zerbe ayaqla vurulur.",
                     bp$bodypart_name, bp$conversion)))
    ),
    div(class = "grid-2",
      card2("Zerbe neticesinin paylanmasi",
        plotOutput("ins_shot_outcome", height = "260px"),
        note(sprintf("Butun zerbelerin yalniz <b>%.1f%%</b>-i qapini tapir — futbolda deqiqlik nadirdir.",
                     ot_pct))),
      card2("Kontratak qollari",
        plotOutput("ins_fastbreak", height = "260px"),
        note(sprintf("Qollarin cemi <b>%.1f%%</b>-i kontratakdan gelir — nadirdir, amma cox tesirli.",
                     kb_pct)))
    ),
    sect("Komanda & oyuncu numuneleri"),
    div(class = "grid-2",
      card2("Liqa uzre intizam (oyun basina sari kart)",
        plotOutput("ins_discipline", height = "280px"),
        note(sprintf("<b>%s</b> oyun basina en cox sari kart gorur (<b>%.2f</b>) — en \"qizishmis\" liqa.",
                     dl$league_name, dl$yellow_pg))),
      card2("En yuksek zerbe-qol cevrilmesi (klublar)",
        plotOutput("ins_conversion", height = "280px"),
        note(sprintf("<b>%s</b> en effektiv klubdur: her 100 zerbeden <b>%.1f</b> qol.",
                     cc$label, cc$conversion)))
    ),
    div(class = "grid-3",
      card2("En cox sari kart (oyuncular)",
        plotOutput("ins_yellow", height = "300px"),
        note(sprintf("Lider: <b>%s</b> (%s sari kart).", yp$label, yp$n))),
      card2("En cox ofsayda dushen klublar",
        plotOutput("ins_offside", height = "300px")),
      card2("En cox penalti qazanan klublar",
        plotOutput("ins_penalty", height = "300px"))
    )
  )
}

# ---------------------------------------------------------------------------
# 5. UI
# ---------------------------------------------------------------------------
ui <- bootstrapPage(theme = NULL,
  tags$head(tags$style(APP_CSS), tags$script(NAV_JS),
            tags$title("Football Events Dashboard")),
  uiOutput("accent_css"),   # dinamik aksent injeksiyasi

  div(class = "shell",
    # ---- SIDEBAR ----
    div(class = "sidebar",
      div(class = "brand",
          div(class = "dot"),
          div(div(class = "name", "FUTBOL"),
              div(class = "sub", "Events Analytics"))),
      nav_link("home",       "house",          "Ana Sehife", active = TRUE),
      nav_link("club",       "shield-halved",  "Klub Analizi"),
      nav_link("club_cmp",   "scale-balanced", "Klub Muqayise"),
      nav_link("player",     "user",           "Oyuncu Analizi"),
      nav_link("player_cmp", "users",          "Oyuncu Muqayise"),
      nav_link("league",     "trophy",         "Liqa Analizi"),
      nav_link("race",       "bolt",           "Qol Yarishi"),
      nav_link("insights",   "chart-line",     "Analitika"),
      nav_link("stories",    "fire",           "Rekordlar"),
      div(class = "side-foot", "Kaggle Football Events", br(), "2012 \u2013 2017 \u00b7 5 liqa")
    ),

    # ---- MAIN ----
    div(class = "main",
      # global KPI-lar (hemishe gorunur)
      div(class = "kpi-row",
        kpi_card(format(KPI$games,   big.mark = ","), "Oyun",    "futbol"),
        kpi_card(format(KPI$goals,   big.mark = ","), "Qol",     "bullseye"),
        kpi_card(KPI$leagues,                          "Liqa",    "trophy"),
        kpi_card(KPI$clubs,                            "Klub",    "shield-halved"),
        kpi_card(KPI$players,                          "Oyuncu",  "user")
      ),

      tabsetPanel(id = "main_tabs", type = "hidden",

        # ============ HOME ============
        tabPanelBody("home",
          div(class = "topbar",
              div(h1(class = "page-title", "Icmal"),
                  div(class = "page-sub",
                      "Avropanin 5 boyuk liqasi uzre 941 min hadise \u00b7 kesfiyyat paneli"))),
          div(class = "grid-2",
            card2("Liqalar uzre qol paylanmasi", plotOutput("home_league_plot", height = "300px")),
            card2("Movsum uzre orta qol", plotOutput("home_season_plot", height = "300px"))
          ),
          div(class = "grid-2",
            card2("En cox qol vuran oyuncular", plotOutput("home_top_players", height = "320px")),
            card2("En cox qol vuran klublar",   plotOutput("home_top_clubs",   height = "320px"))
          )
        ),

        # ============ KLUB ANALIZI ============
        tabPanelBody("club",
          div(class = "topbar",
              div(h1(class = "page-title", "Klub Analizi"),
                  div(class = "page-sub", "Klub secin \u2014 panel onun brend rengini geyinir")),
              div(style = "min-width:260px",
                  selectInput("club_sel", NULL, choices = clubs_by_league,
                              selected = "Bayern Munich", width = "260px"))),
          uiOutput("club_hero"),
          uiOutput("club_stats"),
          div(class = "grid-2",
            card2("Movsum uzre qol trendi", plotOutput("club_trend_plot", height = "300px")),
            card2("Stil profili (0\u2013100)", plotOutput("club_radar_plot", height = "300px"))
          ),
          div(class = "grid-2",
            card2("Ev / Qonaq qelebe faizi", plotOutput("club_home_plot", height = "260px")),
            card2("Zerbe & effektivlik", plotOutput("club_eff_plot", height = "260px"))
          )
        ),

        # ============ KLUB MUQAYISE ============
        tabPanelBody("club_cmp",
          div(class = "topbar",
              div(h1(class = "page-title", "Klub Muqayise"),
                  div(class = "page-sub", "Iki klub \u2014 her biri oz brend renginde"))),
          div(class = "grid-2",
            selectInput("cmp_club1", "1-ci klub", choices = clubs_by_league, selected = "Bayern Munich"),
            selectInput("cmp_club2", "2-ci klub", choices = clubs_by_league, selected = "Barcelona")),
          uiOutput("club_cmp_heroes"),
          card2("Metriklerin muqayisesi", plotOutput("club_cmp_plot", height = "440px"))
        ),

        # ============ OYUNCU ANALIZI ============
        tabPanelBody("player",
          div(class = "topbar",
              div(h1(class = "page-title", "Oyuncu Analizi"),
                  div(class = "page-sub", "Sekil Wikipedia-dan avtomatik cekilir")),
              div(style = "min-width:280px",
                  selectInput("player_sel", NULL, choices = players_choices,
                              selected = "Lionel Messi", width = "280px"))),
          uiOutput("player_hero"),
          uiOutput("player_stats"),
          div(class = "grid-2",
            card2("Oyuncu profili (0\u2013100)", plotOutput("player_radar_plot", height = "300px")),
            card2("Ev / Qonaq qol bolgusu", plotOutput("player_home_plot", height = "300px"))
          )
        ),

        # ============ OYUNCU MUQAYISE ============
        tabPanelBody("player_cmp",
          div(class = "topbar",
              div(h1(class = "page-title", "Oyuncu Muqayise"),
                  div(class = "page-sub", "Iki oyuncunu yanasi qoy"))),
          div(class = "grid-2",
            selectInput("cmp_p1", "1-ci oyuncu", choices = players_choices, selected = "Lionel Messi"),
            selectInput("cmp_p2", "2-ci oyuncu", choices = players_choices, selected = "Cristiano Ronaldo")),
          uiOutput("player_cmp_heroes"),
          card2("Metriklerin muqayisesi", plotOutput("player_cmp_plot", height = "440px"))
        ),

        # ============ LIQA ANALIZI ============
        tabPanelBody("league",
          div(class = "topbar",
              div(h1(class = "page-title", "Liqa Analizi"),
                  div(class = "page-sub", "Movsum trendi ve ev sahibi ustunluyu")),
              div(style = "min-width:220px",
                  selectInput("league_sel", NULL, choices = leagues_vec,
                              selected = "Bundesliga", width = "220px"))),
          uiOutput("league_stats"),
          div(class = "grid-2",
            card2("Movsum uzre orta qol", plotOutput("league_goals_plot", height = "300px")),
            card2("Netice paylanmasi (ev / heci / qonaq)", plotOutput("league_result_plot", height = "300px"))
          )
        ),

        # ============ QOL YARISHI ============
        tabPanelBody("race",
          div(class = "topbar",
              div(h1(class = "page-title", "Qol Yarishi"),
                  div(class = "page-sub", "Movsum-movsum klub siralamasi \u2014 animasiyali")),
              div(style = "min-width:220px",
                  selectInput("race_league_sel", NULL, choices = leagues_vec,
                              selected = "Bundesliga", width = "220px"))),
          card2(NULL,
            uiOutput("race_img"),
            div(class = "race-controls",
                tags$button(class = "race-btn race-btn-play", type = "button",
                  onclick = "var v=document.getElementById('race_video_el'); if(v) v.play();",
                  icon("play"), "Bashlat"),
                tags$button(class = "race-btn", type = "button",
                  onclick = "var v=document.getElementById('race_video_el'); if(v) v.pause();",
                  icon("pause"), "Pauza"),
                tags$button(class = "race-btn", type = "button",
                  onclick = "var v=document.getElementById('race_video_el'); if(v){v.pause(); v.currentTime=0;}",
                  icon("stop"), "Dayandir"))
          )
        ),

        # ============ ANALITIKA ============
        tabPanelBody("insights", insights_tab()),

        # ============ REKORDLAR ============
        tabPanelBody("stories",
          div(class = "topbar",
              div(h1(class = "page-title", "Rekordlar & Hekayeler"),
                  div(class = "page-sub", "Datasetden cixan 5 guclu hekaye"))),
          div(class = "grid-3",
            uiOutput("story_redcard"),
            uiOutput("story_comeback"),
            uiOutput("story_lewandowski")),
          div(class = "grid-2",
            uiOutput("story_scoring"),
            uiOutput("story_defense")),
          card2("Qirmizi kartdan sonra netice", plotOutput("redcard_plot", height = "260px"))
        )
      )
    )
  )
)

# ---------------------------------------------------------------------------
# 6. SERVER
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- naviqasiya -----------------------------------------------------------
  # JS her klikde input$nav_tab-i tab adi ile doldurur; burada tab-i deyisirik
  observeEvent(input$nav_tab, {
    updateTabsetPanel(session, "main_tabs", selected = input$nav_tab)
  })

  # ---- dinamik aksent -------------------------------------------------------
  accent <- reactiveVal(DEFAULT_ACCENT)
  accent2 <- reactiveVal(DEFAULT_ACCENT2)
  observeEvent(input$club_sel, {
    accent(club_color(input$club_sel, "primary_color"))
    accent2(club_color(input$club_sel, "secondary_color"))
  }, ignoreInit = TRUE)
  # klub tab-larindan cixanda default-a qayit? sade saxlayiriq: son secilmis klub rengi qalir
  output$accent_css <- renderUI({
    tags$style(HTML(sprintf(":root{--accent:%s;--accent2:%s;}", accent(), accent2())))
  })
  ac <- reactive(accent())

  # =========================================================================
  #  HOME
  # =========================================================================
  output$home_league_plot <- renderPlot({
    d <- league_summary %>% group_by(league_name) %>%
      summarise(goals = sum(total_goals), .groups = "drop") %>% arrange(goals)
    d$league_name <- factor(d$league_name, levels = d$league_name)
    ggplot(d, aes(goals, league_name)) +
      geom_col(fill = ac(), width = .68) +
      geom_text(aes(label = format(goals, big.mark = ",")), hjust = -0.15,
                color = "#e9ecf2", size = 4) +
      scale_x_continuous(expand = expansion(mult = c(0, .16))) +
      labs(x = NULL, y = NULL) + theme_dash()
  })

  output$home_season_plot <- renderPlot({
    d <- league_summary %>% group_by(season) %>%
      summarise(avg = weighted.mean(avg_goals, games), .groups = "drop")
    ggplot(d, aes(season, avg)) +
      geom_area(fill = ac(), alpha = .18) +
      geom_line(color = ac(), linewidth = 1.4) +
      geom_point(color = ac(), size = 3) +
      geom_text(aes(label = sprintf("%.2f", avg)), vjust = -1.1, color = "#e9ecf2", size = 3.6) +
      scale_y_continuous(expand = expansion(mult = c(.06, .16))) +
      labs(x = "Movsum", y = "Oyun basina qol") + theme_dash()
  })

  output$home_top_players <- renderPlot({
    d <- player_summary %>% arrange(desc(goals)) %>% head(10)
    d$player_label <- factor(d$player_label, levels = rev(d$player_label))
    ggplot(d, aes(goals, player_label)) +
      geom_col(fill = ac(), width = .7) +
      geom_text(aes(label = goals), hjust = -0.2, color = "#e9ecf2", size = 3.8) +
      scale_x_continuous(expand = expansion(mult = c(0, .12))) +
      labs(x = NULL, y = NULL) + theme_dash()
  })

  output$home_top_clubs <- renderPlot({
    d <- club_summary %>% arrange(desc(goals)) %>% head(10)
    d$club <- factor(d$club, levels = rev(d$club))
    ggplot(d, aes(goals, club)) +
      geom_col(fill = ac(), width = .7) +
      geom_text(aes(label = goals), hjust = -0.2, color = "#e9ecf2", size = 3.8) +
      scale_x_continuous(expand = expansion(mult = c(0, .12))) +
      labs(x = NULL, y = NULL) + theme_dash()
  })

  # =========================================================================
  #  KLUB ANALIZI
  # =========================================================================
  club_row <- reactive(club_summary[club_summary$club == input$club_sel, ])

  output$club_hero <- renderUI({
    r <- club_row(); req(nrow(r) == 1)
    div(class = "hero",
        logo_or_badge(r$club, ac(), 110),
        div(class = "htext",
            div(class = "hname", r$club),
            div(class = "hmeta",
                span(class = "chip accent", r$league_name),
                span(class = "chip", paste0(r$goals, " qol")),
                span(class = "chip", paste0(r$shots, " zerbe")),
                span(class = "chip", paste0(r$conversion_rate, "% konversiya")))))
  })

  output$club_stats <- renderUI({
    r <- club_row(); req(nrow(r) == 1)
    div(class = "card2",
      div(class = "card-h", "Statistika"),
      div(class = "stat-grid",
        stat_box(format(r$goals, big.mark = ","),      "Qol"),
        stat_box(format(r$shots, big.mark = ","),      "Zerbe"),
        stat_box(format(r$shots_on_target, big.mark=","),"Hedefe zerbe"),
        stat_box(paste0(r$shot_accuracy, "%"),         "Zerbe deqiqliyi"),
        stat_box(paste0(r$conversion_rate, "%"),       "Konversiya"),
        stat_box(format(r$key_passes, big.mark=","),   "Acar otme"),
        stat_box(r$corners,                            "Kunc"),
        stat_box(r$offsides,                           "Ofsayd"),
        stat_box(r$penalties_won,                      "Penalti qazanib"),
        stat_box(format(r$fouls, big.mark=","),        "Qayda pozmasi"),
        stat_box(r$yellow_cards,                        "Sari kart"),
        stat_box(r$red_cards,                           "Qirmizi kart")
      ))
  })

  output$club_trend_plot <- renderPlot({
    d <- club_trend[club_trend$club == input$club_sel, ]
    req(nrow(d) > 0)
    ggplot(d, aes(season, goals)) +
      geom_area(fill = ac(), alpha = .18) +
      geom_line(color = ac(), linewidth = 1.4) +
      geom_point(color = ac(), size = 3) +
      geom_text(aes(label = goals), vjust = -1.1, color = "#e9ecf2", size = 3.6) +
      scale_y_continuous(expand = expansion(mult = c(.05, .18))) +
      labs(x = "Movsum", y = "Qol") + theme_dash()
  })

  output$club_radar_plot <- renderPlot({
    r <- club_style[club_style$club == input$club_sel, ]
    req(nrow(r) == 1)
    radar_plot(c(r$attack_score, r$efficiency_score, r$discipline_score,
                 r$corner_score, r$creativity_score),
               c("Hucum", "Effektivlik", "Intizam", "Kunc", "Yaradiciliq"), ac())
  }, bg = "#141821")

  output$club_home_plot <- renderPlot({
    d <- club_home_adv[club_home_adv$club == input$club_sel, ]
    req(nrow(d) > 0)
    d$location <- factor(d$location, levels = c("Ev", "Qonaq"))
    ggplot(d, aes(location, win_pct, fill = location)) +
      geom_col(width = .6) +
      geom_text(aes(label = sprintf("%.1f%%", win_pct)), vjust = -0.5,
                color = "#e9ecf2", size = 4.2) +
      scale_fill_manual(values = c(Ev = ac(), Qonaq = "#38414f"), guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, .16))) +
      labs(x = NULL, y = "Qelebe %") + theme_dash()
  })

  output$club_eff_plot <- renderPlot({
    r <- club_row(); req(nrow(r) == 1)
    d <- data.frame(
      k = factor(c("Zerbe", "Hedefe", "Qol"), levels = c("Zerbe","Hedefe","Qol")),
      v = c(r$shots, r$shots_on_target, r$goals))
    ggplot(d, aes(k, v)) +
      geom_col(fill = ac(), width = .6) +
      geom_text(aes(label = format(v, big.mark = ",")), vjust = -0.5,
                color = "#e9ecf2", size = 4) +
      scale_y_continuous(expand = expansion(mult = c(0, .16))) +
      labs(x = NULL, y = NULL) + theme_dash()
  })

  # =========================================================================
  #  KLUB MUQAYISE
  # =========================================================================
  output$club_cmp_heroes <- renderUI({
    c1 <- input$cmp_club1; c2 <- input$cmp_club2
    col1 <- club_color(c1); col2 <- club_color(c2)
    div(class = "grid-2",
      div(class = "hero", style = sprintf("--accent:%s", col1),
          logo_or_badge(c1, col1, 84),
          div(class = "htext", div(class = "hname", style="font-size:22px", c1))),
      div(class = "hero", style = sprintf("--accent:%s", col2),
          logo_or_badge(c2, col2, 84),
          div(class = "htext", div(class = "hname", style="font-size:22px", c2))))
  })

  output$club_cmp_plot <- renderPlot({
    c1 <- input$cmp_club1; c2 <- input$cmp_club2
    req(c1 != c2)
    metrics <- c(goals="Qol", shots="Zerbe", shots_on_target="Hedefe zerbe",
                 fouls="Qayda pozma", yellow_cards="Sari kart", corners="Kunc",
                 offsides="Ofsayd", penalties_won="Penalti")
    d <- club_summary %>% filter(club %in% c(c1, c2)) %>%
      select(club, names(metrics)) %>%
      pivot_longer(-club, names_to = "metric", values_to = "value") %>%
      mutate(metric = factor(metrics[metric], levels = metrics),
             club = factor(club, levels = c(c1, c2)))
    ggplot(d, aes(club, value, fill = club)) +
      geom_col(width = .68) +
      geom_text(aes(label = format(value, big.mark = ",")), vjust = -0.4,
                color = "#e9ecf2", size = 3.4) +
      facet_wrap(~metric, scales = "free_y", ncol = 4) +
      scale_fill_manual(values = setNames(c(club_color(c1), club_color(c2)), c(c1, c2))) +
      scale_y_continuous(expand = expansion(mult = c(0, .2))) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme_dash() + theme(legend.position = "top",
                           axis.text.x = element_text(size = 9))
  })

  # =========================================================================
  #  OYUNCU ANALIZI
  # =========================================================================
  player_row <- reactive(player_summary[player_summary$player_label == input$player_sel, ])

  output$player_hero <- renderUI({
    r <- player_row(); req(nrow(r) == 1)
    url <- player_photo_url(r$player_label)
    photo <- if (is.na(url))
      div(class = "photo-wrap", style = "display:flex;align-items:center;justify-content:center;",
          span(style = "font-family:var(--display);font-size:44px;color:var(--accent)",
               toupper(substr(r$player_label, 1, 1))))
    else div(class = "photo-wrap", img(src = url))
    div(class = "hero",
        photo,
        div(class = "htext",
            div(class = "hname", r$player_label),
            div(class = "hmeta",
                span(class = "chip accent", r$main_club),
                span(class = "chip", r$league_name),
                span(class = "chip", paste0(r$goals, " qol")),
                span(class = "chip", paste0(r$goals_per_game, " qol/oyun")))))
  })

  output$player_stats <- renderUI({
    r <- player_row(); req(nrow(r) == 1)
    div(class = "card2",
      div(class = "card-h", "Statistika"),
      div(class = "stat-grid",
        stat_box(r$goals,               "Qol"),
        stat_box(r$games_played,        "Oyun"),
        stat_box(r$goals_per_game,      "Qol / oyun"),
        stat_box(r$shots,               "Zerbe"),
        stat_box(r$shots_on_target,     "Hedefe zerbe"),
        stat_box(paste0(r$conversion_rate, "%"), "Konversiya"),
        stat_box(r$key_passes,          "Acar otme"),
        stat_box(r$key_passes_per_game, "Acar otme/oyun"),
        stat_box(r$yellow_cards,        "Sari kart"),
        stat_box(r$red_cards,           "Qirmizi kart"),
        stat_box(r$fouls,               "Qayda pozma"),
        stat_box(r$shots_per_game,      "Zerbe / oyun")
      ))
  })

  output$player_radar_plot <- renderPlot({
    r <- player_radar[player_radar$player_label == input$player_sel, ]
    req(nrow(r) == 1)
    radar_plot(c(r$attack_score, r$efficiency_score, r$creativity_score,
                 r$discipline_score, r$aggression_score),
               c("Hucum", "Effektivlik", "Yaradiciliq", "Intizam", "Aqressiya"), ac())
  }, bg = "#141821")

  output$player_home_plot <- renderPlot({
    r <- player_home[player_home$player_label == input$player_sel, ]
    req(nrow(r) == 1)
    d <- data.frame(loc = factor(c("Ev", "Qonaq"), levels = c("Ev", "Qonaq")),
                    goals = c(r$Ev, r$Qonaq))
    ggplot(d, aes(loc, goals, fill = loc)) +
      geom_col(width = .6) +
      geom_text(aes(label = goals), vjust = -0.5, color = "#e9ecf2", size = 4.4) +
      scale_fill_manual(values = c(Ev = ac(), Qonaq = "#38414f"), guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, .16))) +
      labs(x = NULL, y = "Qol") + theme_dash()
  })

  # =========================================================================
  #  OYUNCU MUQAYISE
  # =========================================================================
  output$player_cmp_heroes <- renderUI({
    p1 <- input$cmp_p1; p2 <- input$cmp_p2
    mk <- function(nm, col) {
      url <- player_photo_url(nm)
      ph <- if (is.na(url))
        div(class="photo-wrap", style="width:96px;height:96px;display:flex;align-items:center;justify-content:center;",
            span(style="font-family:var(--display);font-size:34px;color:var(--accent)", toupper(substr(nm,1,1))))
      else div(class="photo-wrap", style="width:96px;height:96px;", img(src=url))
      div(class="hero", style=sprintf("--accent:%s", col), ph,
          div(class="htext", div(class="hname", style="font-size:20px", nm)))
    }
    div(class = "grid-2", mk(p1, "#22d38b"), mk(p2, "#e0553b"))
  })

  output$player_cmp_plot <- renderPlot({
    p1 <- input$cmp_p1; p2 <- input$cmp_p2
    req(p1 != p2)
    metrics <- c(goals="Qol", shots="Zerbe", shots_on_target="Hedefe zerbe",
                 fouls="Qayda pozma", yellow_cards="Sari kart", key_passes="Acar otme")
    d <- player_summary %>% filter(player_label %in% c(p1, p2)) %>%
      select(player_label, names(metrics)) %>%
      pivot_longer(-player_label, names_to = "metric", values_to = "value") %>%
      mutate(metric = factor(metrics[metric], levels = metrics),
             player_label = factor(player_label, levels = c(p1, p2)))
    ggplot(d, aes(player_label, value, fill = player_label)) +
      geom_col(width = .68) +
      geom_text(aes(label = format(value, big.mark = ",")), vjust = -0.4,
                color = "#e9ecf2", size = 3.6) +
      facet_wrap(~metric, scales = "free_y", ncol = 3) +
      scale_fill_manual(values = setNames(c("#22d38b", "#e0553b"), c(p1, p2))) +
      scale_y_continuous(expand = expansion(mult = c(0, .2))) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme_dash() + theme(legend.position = "top",
                           axis.text.x = element_text(size = 9))
  })

  # =========================================================================
  #  LIQA ANALIZI
  # =========================================================================
  league_rows <- reactive(league_summary[league_summary$league_name == input$league_sel, ])

  output$league_stats <- renderUI({
    d <- league_rows(); req(nrow(d) > 0)
    tot_g <- sum(d$total_goals); tot_m <- sum(d$games)
    div(class = "card2",
      div(class = "card-h", "Liqa xulasesi"),
      div(class = "stat-grid",
        stat_box(format(tot_m, big.mark=","),           "Oyun"),
        stat_box(format(tot_g, big.mark=","),           "Qol"),
        stat_box(sprintf("%.2f", tot_g/tot_m),          "Oyun basina qol"),
        stat_box(sprintf("%.0f%%", weighted.mean(d$home_win_pct, d$games)), "Ev qelebesi")
      ))
  })

  output$league_goals_plot <- renderPlot({
    d <- league_rows(); req(nrow(d) > 0)
    ggplot(d, aes(season, avg_goals)) +
      geom_area(fill = ac(), alpha = .18) +
      geom_line(color = ac(), linewidth = 1.4) +
      geom_point(color = ac(), size = 3) +
      geom_text(aes(label = sprintf("%.2f", avg_goals)), vjust = -1.1,
                color = "#e9ecf2", size = 3.6) +
      scale_y_continuous(expand = expansion(mult = c(.05, .18))) +
      labs(x = "Movsum", y = "Oyun basina qol") + theme_dash()
  })

  output$league_result_plot <- renderPlot({
    d <- league_rows(); req(nrow(d) > 0)
    dd <- d %>% select(season, Ev = home_win_pct, Heci = draw_pct, Qonaq = away_win_pct) %>%
      pivot_longer(-season, names_to = "res", values_to = "pct") %>%
      mutate(res = factor(res, levels = c("Ev", "Heci", "Qonaq")))
    ggplot(dd, aes(factor(season), pct, fill = res)) +
      geom_col(width = .7) +
      scale_fill_manual(values = c(Ev = ac(), Heci = "#5b6472", Qonaq = "#38414f")) +
      scale_y_continuous(expand = expansion(mult = c(0, .04))) +
      labs(x = "Movsum", y = "%", fill = NULL) +
      theme_dash() + theme(legend.position = "top")
  })

  output$race_img <- renderUI({
    fname <- RACE_VIDEO_MAP[[input$race_league_sel]]
    path  <- if (!is.null(fname)) file.path("www", fname) else NA
    if (!is.null(fname) && file.exists(path)) {
      tags$video(id = "race_video_el", src = fname, type = "video/mp4",
                 style = "width:100%;max-width:640px;border-radius:10px;display:block;margin:0 auto;background:#141821;",
                 preload = "auto")
    } else {
      div(class = "small-muted", style = "padding:20px 4px;",
          HTML(paste0(
            "Bu liqa ucun video hele hazir deyil. RStudio-da (",
            "<code style='color:var(--accent)'>full_data</code>",
            ", ",
            "<code style='color:var(--accent)'>club_summary_top5.rds</code>",
            " ve ",
            "<code style='color:var(--accent)'>club_colors.rds</code>",
            " movcud olan sessiyada) bir defe islet:<br>",
            "<code style='color:var(--accent)'>source(\"prep_bar_race.R\")</code>")))
    }
  })

  # =========================================================================
  #  ANALITIKA  (yalniz insights.rds movcud olduqda)
  # =========================================================================
  if (HAS_INSIGHTS) {
    I <- INSIGHTS
    output$ins_goal_minutes <- renderPlot(
      vbar(I$goal_minutes, "time_bin", "n", ac()), bg = "#141821")
    output$ins_situation <- renderPlot(
      hbar(I$situation, "situation_name", "n", ac()), bg = "#141821")
    output$ins_assist <- renderPlot(
      hbar(I$assist, "assist_method_name", "n", ac()), bg = "#141821")
    output$ins_bodypart <- renderPlot(
      hbar(I$bodypart, "bodypart_name", "conversion", ac(), suffix = "%"), bg = "#141821")
    output$ins_shot_outcome <- renderPlot(
      hbar(I$shot_outcome, "shot_outcome_name", "n", ac()), bg = "#141821")
    output$ins_fastbreak <- renderPlot(
      hbar(I$fast_break, "label", "n", ac()), bg = "#141821")
    output$ins_discipline <- renderPlot(
      hbar(I$discipline, "league_name", "yellow_pg", ac()), bg = "#141821")
    output$ins_conversion <- renderPlot(
      hbar(I$conversion_clubs, "label", "conversion", ac(), suffix = "%"), bg = "#141821")
    output$ins_yellow <- renderPlot(
      hbar(I$yellow_players, "label", "n", ac()), bg = "#141821")
    output$ins_offside <- renderPlot(
      hbar(I$offside_clubs, "label", "n", ac()), bg = "#141821")
    output$ins_penalty <- renderPlot(
      hbar(I$penalty_clubs, "label", "n", ac()), bg = "#141821")
  }

  # =========================================================================
  #  REKORDLAR
  # =========================================================================
  output$story_redcard <- renderUI({
    n <- nrow(red_card); loss <- round(mean(red_card$result == "loss")*100)
    div(class = "story",
        div(class = "st-eyebrow", "Qirmizi kart"),
        div(class = "st-big", paste0(loss, "%")),
        div(class = "st-title", "Kart = meglubiyyet ehtimali"),
        div(class = "st-body",
            sprintf("%s qirmizi kart hadisesi arasinda kart goren komanda oyunlarin %d%%-ni uduzub, cemi %d%%-ni udub.",
                    format(n, big.mark=","), loss, round(mean(red_card$result=="win")*100))))
  })

  output$story_comeback <- renderUI({
    top <- comebacks %>% arrange(desc(deficit)) %>% head(1)
    div(class = "story",
        div(class = "st-eyebrow", "Geri donus"),
        div(class = "st-big", paste0(top$deficit, " qol")),
        div(class = "st-title", paste0(top$winner, " \u2014 en boyuk geri donus")),
        div(class = "st-body",
            sprintf("%s, %s oyununda (%s) %d qol geridan qalib %s hesabi ile qalib geldi.",
                    top$winner, paste(top$ht, "-", top$at), top$date, top$deficit, top$score)))
  })

  output$story_lewandowski <- renderUI({
    lw <- player_matches %>% filter(grepl("lewandowski", player, ignore.case = TRUE)) %>%
      arrange(desc(goals_in_match)) %>% head(1)
    if (nrow(lw) == 0) lw <- player_matches %>% arrange(desc(goals_in_match)) %>% head(1)
    div(class = "story",
        div(class = "st-eyebrow", "Dunya rekordu"),
        div(class = "st-big", paste0(lw$goals_in_match, " qol")),
        div(class = "st-title", paste0(lw$player_label, " \u2014 9 deqiqede")),
        div(class = "st-body",
            sprintf("%s oyununda (%s) evezedici kimi girib 9 deqiqede %d qol vurdu \u2014 dunya rekordu.",
                    paste(lw$ht, "-", lw$at), lw$date, lw$goals_in_match)))
  })

  output$story_scoring <- renderUI({
    g <- high_scoring %>% arrange(desc(total_goals)) %>% head(1)
    div(class = "story",
        div(class = "st-eyebrow", "En cox qollu oyun"),
        div(class = "st-big", paste0(g$total_goals, " qol")),
        div(class = "st-title", sprintf("%s %d\u2013%d %s", g$ht, g$fthg, g$ftag, g$at)),
        div(class = "st-body",
            sprintf("%s tarixinde bir oyunda cemi %d qol \u2014 dataset rekordu.", g$date, g$total_goals)))
  })

  output$story_defense <- renderUI({
    cs <- clean_sheets %>% arrange(desc(clean_sheet_pct)) %>% head(1)
    div(class = "story",
        div(class = "st-eyebrow", "En yaxsi mudafie"),
        div(class = "st-big", paste0(cs$clean_sheet_pct, "%")),
        div(class = "st-title", paste0(cs$club, " \u2014 clean sheet lideri")),
        div(class = "st-body",
            sprintf("%s oyunun %.0f%%-nde qapisini qorudu, oyun basina cemi %.2f qol buraxdi.",
                    cs$club, cs$clean_sheet_pct, cs$avg_conceded)))
  })

  output$redcard_plot <- renderPlot({
    d <- red_card %>% count(result) %>%
      mutate(result = recode(result, win = "Qelebe", draw = "Heci", loss = "Meglubiyyet"),
             result = factor(result, levels = c("Qelebe", "Heci", "Meglubiyyet")),
             pct = n / sum(n) * 100)
    ggplot(d, aes(result, pct, fill = result)) +
      geom_col(width = .6) +
      geom_text(aes(label = sprintf("%.0f%%", pct)), vjust = -0.5,
                color = "#e9ecf2", size = 5) +
      scale_fill_manual(values = c(Qelebe = "#22c55e", Heci = "#8b93a3",
                                   Meglubiyyet = "#e0553b"), guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, .16))) +
      labs(x = NULL, y = "%") + theme_dash()
  })
}

shinyApp(ui, server)
