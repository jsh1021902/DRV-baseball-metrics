###############################################################################
# 04_make_figures.R   (그림 생성 — 02·03 결과 CSV → 보고서용 PNG)
#
#   기존 04_compare_war_viz.R + 05_case_study_viz.R + 06_case_cards.R 를 하나로 통합.
#
#   (A) fig_war_correlation.png      : DRV+/DRWAR vs fWAR·bWAR·wRC+·ERA+ 순위상관
#   (B) fig_case_decomposition.png   : 가치 분해 — 결과 가치 + 과정 가치(선구안) = DRV
#   (C) fig_case_war_compare.png     : 대표 선수 DRWAR vs 기존 fWAR·bWAR
#   (D) figures/cards/*.png          : 선수 1명당 1장 비교 카드
#
#   입력 : output/war_correlation_summary.csv  (02)
#          output/case_study_summary.csv       (03)
#   출력 : output/figures/*.png  ·  output/figures/cards/*.png
#   글꼴 : Jalnan(있으면) — webapp/JalnanGothic.ttf 또는 ~/Library/Fonts, 없으면 sans
###############################################################################

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2); library(showtext) })

## -- 경로/공용설정: 저장소의 R/00_paths.R 로드 (실행 위치와 무관하게 동작) --
local({
  .a <- commandArgs(FALSE); .m <- grep("^--file=", .a, value = TRUE)
  .d <- if (length(.m)) dirname(sub("^--file=", "", .m[1])) else "R"
  source(file.path(.d, "00_paths.R"))
})
CARDIR <- file.path(FIGDIR, "cards"); dir.create(CARDIR, showWarnings = FALSE, recursive = TRUE)

# ── 글꼴 등록 (Jalnan 우선, 없으면 기본 sans) ───────────────────────────────
.cand <- c(file.path(WEBDIR, "JalnanGothic.ttf"),
           path.expand("~/Library/Fonts/JalnanGothicTTF.ttf"),
           path.expand("~/Library/Fonts/JalnanGothic.ttf"))
.font <- .cand[file.exists(.cand)]
if (length(.font)) { font_add("jalnan", regular = .font[1]); FF <- "jalnan" } else FF <- "sans"
showtext_auto(); showtext_opts(dpi = 300)

deacc <- function(x) if (requireNamespace("stringi", quietly = TRUE))
  stringi::stri_trans_general(x, "Latin-ASCII") else x       # Álvarez -> Alvarez
BLUE <- "#2f6fb3"; GREEN <- "#2e8b3d"; RED <- "#c0504d"; GREY <- "#9e9e9e"

# ===========================================================================
# (A) fig_war_correlation.png — 지표 타당성(순위상관)
# ===========================================================================
df <- read_bom(file.path(OUTDIR, "war_correlation_summary.csv")) %>%
  filter(grepl("정당", relation)) %>%
  mutate(pair   = ifelse(y %in% c("wRC+", "ERA+"), "DRV+ ↔ wRC+/ERA+", paste0("DRWAR ↔ ", y)),
         season_lab = ifelse(season == "2025_full", "2025 풀시즌", "2026 개막~5/23"))
df$pair <- factor(df$pair, levels = c("DRWAR ↔ bWAR", "DRWAR ↔ fWAR", "DRV+ ↔ wRC+/ERA+"))

pCorr <- ggplot(df, aes(pair, spearman, fill = side)) +
  geom_col(width = 0.72, color = "white") +
  geom_text(aes(label = sprintf("%.2f", spearman)), hjust = -0.18, family = FF, fontface = "bold", size = 4.6) +
  coord_flip() + facet_grid(side ~ season_lab) +
  scale_fill_manual(values = c("타자" = BLUE, "투수" = RED), name = NULL) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 0.75, 0.25), expand = expansion(mult = c(0, 0.02))) +
  labs(title = "지표 타당성 검증 — 같은 척도끼리(정당한 짝) 순위상관",
       subtitle = "효율은 효율끼리(DRV+↔wRC+/ERA+), 누적은 누적끼리(DRWAR↔fWAR/bWAR) · Spearman",
       x = NULL, y = "Spearman 순위상관 (1 = 완전 일치)") +
  theme_minimal(base_family = FF, base_size = 15) +
  theme(plot.title = element_text(face = "bold", size = 21),
        plot.subtitle = element_text(size = 14, color = "grey30"),
        strip.text = element_text(face = "bold", size = 15),
        legend.position = "top", panel.spacing.x = unit(1.4, "lines"),
        panel.grid.major.y = element_blank())
ggsave(file.path(FIGDIR, "fig_war_correlation.png"), pCorr, width = 14, height = 8.5, dpi = 300, units = "in", device = grDevices::png)
cat("saved fig_war_correlation.png\n")

# ===========================================================================
# 케이스 스터디 (B)(C)(D) — case_study_summary.csv
# ===========================================================================
cs <- read_bom(file.path(OUTDIR, "case_study_summary.csv")) %>%
  mutate(player = deacc(name), plabel = sprintf("%s (%s)", player, side),
         yr = sub(" .*", "", season),
         panel = factor(sprintf("%s · %s", side, yr),
                        levels = c("타자 · 2025", "타자 · 2026", "투수 · 2025", "투수 · 2026")))

theme_cs <- function(base = 15) theme_minimal(base_family = FF, base_size = base) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(face = "bold", size = 21),
        plot.subtitle = element_text(size = 13.5, color = "grey30"),
        strip.text = element_text(face = "bold", size = 14.5),
        legend.position = "top", panel.spacing.x = unit(1.3, "lines"))

# (B) 가치 분해 : 결과 + 과정 = DRV(런)
long <- cs %>% select(panel, plabel, DRV_total, DRWAR, result_runs, process_runs) %>%
  pivot_longer(c(result_runs, process_runs), names_to = "comp", values_to = "runs") %>%
  mutate(comp = factor(ifelse(comp == "result_runs", "결과 가치 (PA Score)", "과정 가치 (선구안)"),
                       levels = c("결과 가치 (PA Score)", "과정 가치 (선구안)")))
pDec <- ggplot(long, aes(runs, reorder(plabel, DRV_total), fill = comp)) +
  geom_col(width = 0.62, color = "white") +
  geom_text(data = cs, aes(DRV_total, reorder(plabel, DRV_total), label = sprintf("→ DRWAR %.1f승", DRWAR)),
            inherit.aes = FALSE, hjust = -0.04, family = FF, fontface = "bold", size = 3.7) +
  facet_wrap(~ panel, scales = "free", ncol = 2) +
  scale_fill_manual(values = c("결과 가치 (PA Score)" = BLUE, "과정 가치 (선구안)" = GREEN), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.42))) +
  labs(title = "대표 선수 케이스 스터디 ① 가치 분해 — 결과 + 과정 = DRV",
       subtitle = "버려지던 '과정 가치(선구안)'를 점수화한 게 우리 지표의 핵심 · 막대=런(평균 대비), 끝 라벨=DRWAR(승)",
       x = "런 (평균 대비)", y = NULL) + theme_cs()
ggsave(file.path(FIGDIR, "fig_case_decomposition.png"), pDec, width = 14, height = 8.5, dpi = 300, units = "in", device = grDevices::png)
cat("saved fig_case_decomposition.png\n")

# (C) 개별 검증 : DRWAR vs fWAR · bWAR (승)
warlong <- cs %>% select(panel, plabel, DRWAR, fWAR, bWAR) %>%
  pivot_longer(c(DRWAR, fWAR, bWAR), names_to = "metric", values_to = "wins") %>%
  mutate(metric = factor(metric, levels = c("DRWAR", "fWAR", "bWAR")))
pCmp <- ggplot(warlong, aes(plabel, wins, fill = metric)) +
  geom_col(width = 0.7, position = position_dodge(0.78), color = "white") +
  geom_text(aes(label = sprintf("%.1f", wins)), position = position_dodge(0.78),
            hjust = -0.15, family = FF, fontface = "bold", size = 3.6) +
  coord_flip() + facet_wrap(~ panel, scales = "free", ncol = 2) +
  scale_fill_manual(values = c("DRWAR" = RED, "fWAR" = BLUE, "bWAR" = GREY), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "대표 선수 케이스 스터디 ② 개별 검증 — DRWAR vs 기존 fWAR · bWAR",
       subtitle = "같은 스타를 우리 지표(DRWAR)와 공인 지표(fWAR·bWAR)가 비슷하게 평가하는가 · 단위=승",
       x = NULL, y = "WAR (승)") + theme_cs()
ggsave(file.path(FIGDIR, "fig_case_war_compare.png"), pCmp, width = 14, height = 8.5, dpi = 300, units = "in", device = grDevices::png)
cat("saved fig_case_war_compare.png\n")

# (D) 선수 1명당 1장 비교 카드
OURS_BAT <- BLUE; OURS_PIT <- RED; EXIST <- GREY
make_card <- function(r) {
  unit <- if (r$side == "타자") "PA" else "BF"
  ours_col <- if (r$side == "타자") OURS_BAT else OURS_PIT
  d <- bind_rows(
    data.frame(type = "누적 가치 (승)", metric = c("DRWAR", "fWAR", "bWAR"),
               value = c(r$DRWAR, r$fWAR, r$bWAR), ours = c(TRUE, FALSE, FALSE)),
    data.frame(type = "효율 지수 (100 = 리그 평균)", metric = c("DRV+", r$rate_bench_name),
               value = c(r$DRVplus, r$rate_bench), ours = c(TRUE, FALSE))
  ) %>% mutate(type = factor(type, levels = c("누적 가치 (승)", "효율 지수 (100 = 리그 평균)")),
               grp = ifelse(ours, "우리 지표", "기존 공인지표"),
               lbl = ifelse(type == "누적 가치 (승)", sprintf("%.1f", value), sprintf("%.0f", value))) %>%
    group_by(type) %>% mutate(ord = rank(value, ties.method = "first")) %>% ungroup()
  p <- ggplot(d, aes(reorder(metric, ord), value, fill = grp)) +
    geom_col(width = 0.66, color = "white") +
    geom_text(aes(label = lbl), hjust = -0.18, family = FF, fontface = "bold", size = 7) +
    coord_flip() + facet_wrap(~ type, scales = "free", ncol = 2) +
    scale_fill_manual(values = c("우리 지표" = ours_col, "기존 공인지표" = EXIST), name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
    labs(title = sprintf("%s   ·   %s · %s", r$player, r$side, r$season),
         subtitle = sprintf("%s %d · 가치분해: 결과 %+.1f런 + 과정(선구안) %+.1f런 = DRV %.0f런",
                            unit, r$opp, r$result_runs, r$process_runs, r$DRV_total),
         x = NULL, y = NULL,
         caption = "파랑 = 우리가 만든 지표(DRWAR·DRV+) · 회색 = 기존 공인지표(fWAR·bWAR·wRC+/ERA+)") +
    theme_minimal(base_family = FF, base_size = 17) +
    theme(plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold", size = 27),
          plot.subtitle = element_text(size = 15, color = "grey30"),
          plot.caption = element_text(size = 12.5, color = "grey40"),
          strip.text = element_text(face = "bold", size = 18),
          axis.text.y = element_text(face = "bold", size = 18),
          axis.text.x = element_blank(), panel.grid = element_blank(),
          legend.position = "top", panel.spacing.x = unit(2, "lines"))
  side_e <- if (r$side == "타자") "batter" else "pitcher"
  fn <- sprintf("fig_case_%s_%s_%s.png", sub(" .*", "", r$season), side_e,
                gsub("[^A-Za-z0-9]+", "_", r$player))
  ggsave(file.path(CARDIR, fn), p, width = 12, height = 8.364, dpi = 300, units = "in", device = grDevices::png)
  cat("saved cards/", fn, "\n", sep = "")
}
cs2 <- read_bom(file.path(OUTDIR, "case_study_summary.csv")) %>% mutate(player = deacc(name))
for (i in seq_len(nrow(cs2))) make_card(cs2[i, ])

cat(sprintf("\n[done] 그림 저장 폴더: %s\n", FIGDIR))
