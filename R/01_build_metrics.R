###############################################################################
# 01_build_metrics.R   (시각화 제외 — 데이터 → 최종 지표 산출 단일 파이프라인)
#
#   데이터(statcast)를 불러와 베이지안 야구 평가지표를 순서대로 산출:
#     Step1  A 점수  : 타구 질 격자 Â[EV,LA]            (EB 수축)
#     Step2  B 점수  : 상황 난이도 936칸 B̂[카운트×구종×위치] (EB 수축, 좌우 핸드 보정)
#     Step3  V 점수  : 타석이 만든 가치 (인플레이→A, 삼진/볼넷/사구→확정ΔRE)
#            PA Score: V − B̂            (상황 난이도로 보정한 타석 가치)
#     Step4  DRV     : PA Score + Σ_{중간투구}(ΔRE_p − Ê_league[상황])  (선구안 누적)
#                      Ê_league = 중간 투구 기대 ΔRE 격자(EB 수축)
#     Step5-1 DRV+   : 타석당(per-PA) 효율을 리그 평균 100 지수로 (선수레벨 EB 수축,
#                      수축 후 분포 기준 scale → 타자·투수 폭 통일)
#     Step5-2 DRWAR  : DRV를 승수(WAR)로 환산한 누적 지표 (대체수준 r, RPW)
#
#   입력 : data/<2025_data_csv|2026_data_csv>/statcast_*.csv,  data/Raw data/id_map.rds
#   출력 : Final Code/output/
#            metrics_{batter,pitcher}_<label>.csv   (선수별 최종 지표 — Script2·3 입력)
#            pa_detail_<label>.csv                  (타석별 분해 — Script3 케이스스터디)
#            score_tables_<label>.rds               (A·B·Ehat 격자 + 진단)
###############################################################################

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

## -- 경로/공용설정: 저장소의 R/00_paths.R 로드 (실행 위치와 무관하게 동작) --
local({
  .a <- commandArgs(FALSE); .m <- grep("^--file=", .a, value = TRUE)
  .d <- if (length(.m)) dirname(sub("^--file=", "", .m[1])) else "R"
  source(file.path(.d, "00_paths.R"))
})

write_excel_csv <- function(df, path) {            # 한글 Excel용 UTF-8 BOM
  con <- file(path, open = "wb", encoding = "UTF-8"); on.exit(close(con))
  writeBin(charToRaw("﻿"), con); write.csv(df, con, row.names = FALSE, fileEncoding = "")
  invisible(path)
}

# ── 공통 상수 (전 Step 동일) ────────────────────────────────────────────────
PITCH_GROUP <- c(FF="직구",FA="직구",SI="싱커",FC="커터",SL="슬라",ST="슬라",SV="슬라",
                 CU="커브",KC="커브",CS="커브",CH="체인지",FS="체인지",FO="체인지")
PG_LEVELS  <- c("직구","싱커","커터","슬라","커브","체인지")
FLIP_ZONE  <- c("1"=3,"2"=2,"3"=1,"4"=6,"5"=5,"6"=4,"7"=9,"8"=8,"9"=7,
                "11"=12,"12"=11,"13"=14,"14"=13)
VALID_ZONE <- c(1:9, 11:14)
BIN_EV <- 5; BIN_LA <- 5
MIN_PA <- c("2025_full"=300, "2026_partial"=100)   # 타자 규정
MIN_BF <- c("2025_full"=250, "2026_partial"=80)    # 투수 규정
# DRWAR 파라미터
R_REPL <- 0.0085          # 대체 수준 (런/투구) = 20런 / (600PA × 3.9투구)
RPW    <- 10              # 런→승
SCALE_PER_SD <- 10        # DRV+ : 1 SD ≈ 10점

# ── EB 분산성분 / 수축 K (random-effects ANOVA 모멘트; 전 Step 동일식) ───────
eb_components <- function(value, group) {
  d <- data.frame(value = value, group = group)
  g <- d %>% group_by(group) %>%
    summarise(n = dplyr::n(), m = mean(value), ss = sum((value - mean(value))^2),
              .groups = "drop")
  N <- sum(g$n); G <- nrow(g)
  if (G < 2 || N <= G) return(list(within = NA, between = NA, k = 0))
  grand   <- weighted.mean(g$m, g$n)
  within  <- sum(g$ss) / (N - G)
  SSB     <- sum(g$n * (g$m - grand)^2)
  n0      <- (N - sum(g$n^2) / N) / (G - 1)
  between <- max((SSB / (G - 1) - within) / n0, 1e-9)
  k <- within / between; if (!is.finite(k) || k < 0) k <- 0
  list(within = within, between = between, k = k)
}
eb_k <- function(value, group) eb_components(value, group)$k

# 이름 매핑
id_map <- readRDS(file.path(ROOT, "data", "Raw data", "id_map.rds"))
name_tbl <- id_map %>% transmute(key = as.character(key_mlbam), name) %>%
  filter(!is.na(key)) %>% distinct(key, .keep_all = TRUE)

# ===========================================================================
# 한 시즌 전체 산출
# ===========================================================================
run_label <- function(label) {
  yr  <- if (label == "2025_full") "2025" else "2026"
  cat(sprintf("\n================  [%s]  ================\n", label))
  sc  <- read.csv(file.path(ROOT, "data", sprintf("%s_data_csv", yr),
                            sprintf("statcast_%s.csv", yr)),
                  stringsAsFactors = FALSE, check.names = FALSE)
  sc <- sc %>% mutate(
    events  = ifelse(is.na(events), "", as.character(events)),
    type    = as.character(type),
    dRE     = suppressWarnings(as.numeric(delta_run_exp)),
    EV      = suppressWarnings(as.numeric(launch_speed)),
    LA      = suppressWarnings(as.numeric(launch_angle)),
    zone    = suppressWarnings(as.integer(zone)),
    balls   = suppressWarnings(as.integer(balls)),
    strikes = suppressWarnings(as.integer(strikes)),
    stand   = as.character(stand),
    descr   = as.character(description),
    pg      = unname(PITCH_GROUP[as.character(pitch_type)]),
    terminal = events != "",
    count = ifelse(balls %in% 0:3 & strikes %in% 0:2, sprintf("%dB-%dS", balls, strikes), NA),
    zone_norm = ifelse(stand == "L" & !is.na(zone) & as.character(zone) %in% names(FLIP_ZONE),
                       FLIP_ZONE[as.character(zone)], zone))

  # ── Step1 : A[EV,LA] (인플레이 타구, EB 수축) ──────────────────────────────
  bip <- sc %>% filter(type == "X", is.finite(EV), is.finite(LA), is.finite(dRE)) %>%
    mutate(ev_bin = floor(EV/BIN_EV)*BIN_EV, la_bin = floor(LA/BIN_LA)*BIN_LA,
           cell = paste(ev_bin, la_bin, sep="_"))
  muA <- mean(bip$dRE); kA <- eb_k(bip$dRE, bip$cell)
  A_grid <- bip %>% group_by(ev_bin, la_bin) %>%
    summarise(n = dplyr::n(), x = mean(dRE), .groups="drop") %>%
    mutate(w = n/(n+kA), A_shrunk = w*x + (1-w)*muA)
  cat(sprintf("Step1 A: 타구 %d · 칸 %d · muA=%.4f · K_A=%.1f\n",
              nrow(bip), nrow(A_grid), muA, kA))

  # ── Step2 : B[상황] 936칸 (모든 투구 ΔRE, EB 수축) ────────────────────────
  bp <- sc %>% filter(is.finite(dRE), !is.na(pg), !is.na(zone), zone %in% VALID_ZONE,
                      !is.na(count), stand %in% c("L","R")) %>%
    mutate(cellB = paste(count, pg, zone_norm, sep="|"))
  muB <- mean(bp$dRE); kB <- eb_k(bp$dRE, bp$cellB)
  B_grid <- bp %>% group_by(count, pg, zone = zone_norm) %>%
    summarise(n = dplyr::n(), x = mean(dRE), .groups="drop") %>%
    mutate(w = n/(n+kB), B_shrunk = w*x + (1-w)*muB)
  cat(sprintf("Step2 B: 투구 %d · 칸 %d/936 · muB=%.4f · K_B=%.1f\n",
              nrow(bp), nrow(B_grid), muB, kB))

  # ── Step3 : V, PA Score (종결 투구 = 타석당 1행) ──────────────────────────
  term <- sc %>% filter(terminal) %>%
    mutate(kind = case_when(
      events %in% c("strikeout","strikeout_double_play") ~ "K",
      events %in% c("walk","intent_walk")                ~ "BB",
      events == "hit_by_pitch"                           ~ "HBP",
      type == "X"                                        ~ "INPLAY",
      TRUE                                               ~ "OTHER"))
  fx <- term %>% filter(kind %in% c("K","BB","HBP")) %>%
    group_by(kind) %>% summarise(v = mean(dRE, na.rm=TRUE), .groups="drop")
  vK <- fx$v[fx$kind=="K"]; vBB <- fx$v[fx$kind=="BB"]; vHBP <- fx$v[fx$kind=="HBP"]
  term <- term %>%
    mutate(ev_bin = floor(EV/BIN_EV)*BIN_EV, la_bin = floor(LA/BIN_LA)*BIN_LA) %>%
    left_join(A_grid %>% select(ev_bin, la_bin, A_shrunk), by=c("ev_bin","la_bin")) %>%
    left_join(B_grid %>% select(count, pg, zone, B_shrunk),
              by=c("count","pg","zone_norm"="zone")) %>%
    mutate(V = case_when(kind=="INPLAY" ~ ifelse(is.na(A_shrunk), muA, A_shrunk),
                         kind=="K" ~ vK, kind=="BB" ~ vBB, kind=="HBP" ~ vHBP,
                         TRUE ~ ifelse(is.na(dRE), 0, dRE)),
           B_hat = ifelse(is.na(B_shrunk), muB, B_shrunk),
           PA_score = V - B_hat)
  cat(sprintf("Step3 V/PA: 타석 %d · 확정 K=%.3f BB=%.3f HBP=%.3f · mean PA_score=%.4f(≈0)\n",
              nrow(term), vK, vBB, vHBP, mean(term$PA_score)))

  # ── Step4a : 중간 투구 기대 ΔRE 격자 Ê_league (비종결, EB 수축) ───────────
  nt <- sc %>% filter(!terminal, is.finite(dRE), !is.na(pg), !is.na(zone),
                      zone %in% VALID_ZONE, !is.na(count), stand %in% c("L","R"))
  muE <- mean(nt$dRE); kE <- eb_k(nt$dRE, paste(nt$count, nt$pg, nt$zone_norm, sep="|"))
  E_grid <- nt %>% group_by(count, pg, zone = zone_norm) %>%
    summarise(n = dplyr::n(), x = mean(dRE), .groups="drop") %>%
    mutate(w = n/(n+kE), E_hat = w*x + (1-w)*muE)
  cat(sprintf("Step4 Ehat: 중간투구 %d · 칸 %d · muE=%.4f · K_E=%.1f\n",
              nrow(nt), nrow(E_grid), muE, kE))

  # ── Step4b : 선구안 pitch_value = ΔRE_p − Ê_league · 타석별 과정가치 ───────
  nt <- nt %>% left_join(E_grid %>% select(count, pg, zone, E_hat),
                         by=c("count","pg","zone_norm"="zone")) %>%
    mutate(E_hat = ifelse(is.na(E_hat), muE, E_hat), pitch_value = dRE - E_hat)
  proc <- nt %>% group_by(game_pk, at_bat_number) %>%
    summarise(process = sum(pitch_value), .groups="drop")

  # ── 타석별 DRV = PA Score + 과정가치 ──────────────────────────────────────
  pa <- term %>% left_join(proc, by=c("game_pk","at_bat_number")) %>%
    mutate(process = ifelse(is.na(process), 0, process), DRV = PA_score + process)
  write_excel_csv(
    pa %>% transmute(game_pk, at_bat_number, batter, pitcher,
                     game_date = as.Date(game_date), events, kind, count, pg, zone,
                     V = round(V,5), B_hat = round(B_hat,5), PA_score = round(PA_score,5),
                     process = round(process,5), DRV = round(DRV,5)),
    file.path(OUTDIR, sprintf("pa_detail_%s.csv", label)))

  # 투구 수 N_p (DRWAR용)
  np_bat <- sc %>% count(batter,  name="N_p")
  np_pit <- sc %>% count(pitcher, name="N_p")

  # ── Step5 : 선수별 DRV → DRV+ → DRWAR (포지션별) ──────────────────────────
  metrics <- list()
  for (side in c("batter","pitcher")) {
    sgn  <- if (side=="batter") 1 else -1
    ncol <- if (side=="batter") "PA" else "BF"
    cut  <- if (side=="batter") MIN_PA[[label]] else MIN_BF[[label]]
    np   <- if (side=="batter") np_bat else np_pit
    pa$pid <- pa[[side]]

    pl <- pa %>% group_by(pid) %>%
      summarise(n = dplyr::n(), DRV_total = sgn*sum(DRV),
                DRV_per_PA = sgn*mean(DRV), .groups="drop") %>%
      inner_join(np, by=c("pid"=side)) %>%
      mutate(key = as.character(pid)) %>%
      left_join(name_tbl, by="key") %>%
      mutate(name = ifelse(is.na(name), paste0("ID_", pid), name))

    # DRV+ : 선수레벨 K (per-PA DRV 분산비) · 수축 후 분포 기준 scale
    Kp   <- eb_k(pa$DRV, pa$pid)
    leag <- weighted.mean(pl$DRV_per_PA, pl$n)
    pl <- pl %>% mutate(w = n/(n+Kp), rate_shrunk = leag + (DRV_per_PA-leag)*w)
    sd_ref <- sd(pl$rate_shrunk[pl$n >= cut]); scale <- SCALE_PER_SD / sd_ref
    pl <- pl %>% mutate(DRVplus = 100 + (rate_shrunk - leag)*scale)

    # DRWAR : (rate − league + r)·N_p / RPW   (DRV_total 에 δ 이미 반영)
    pl <- pl %>% mutate(rate_pp = DRV_total/N_p) %>%
      mutate(leag_pp = weighted.mean(rate_pp, N_p),
             DRWAR = ((rate_pp - leag_pp) + R_REPL)*N_p / RPW)

    out <- pl %>% arrange(desc(DRV_total)) %>%
      transmute(mlbam_id = pid, name, side, season = label,
                !!ncol := n, N_p,
                DRV_total = round(DRV_total,3), DRV_per_PA = round(DRV_per_PA,5),
                DRVplus = round(DRVplus,1), DRWAR = round(DRWAR,3),
                qualified = n >= cut)
    write_excel_csv(out, file.path(OUTDIR, sprintf("metrics_%s_%s.csv", side, label)))
    metrics[[side]] <- out
    top <- out %>% arrange(desc(DRWAR)) %>% head(3)
    cat(sprintf("Step5 %s: 선수 %d (규정 %d) · K_player=%.1f · scale=%.0f\n",
                side, nrow(out), sum(out$qualified), Kp, scale))
    cat(sprintf("   DRWAR TOP3: %s\n",
                paste(sprintf("%s %.1f(DRV+%.0f)", top$name, top$DRWAR, top$DRVplus), collapse=" | ")))
  }

  saveRDS(list(A_grid=A_grid, B_grid=B_grid, E_grid=E_grid,
               muA=muA, muB=muB, muE=muE, kA=kA, kB=kB, kE=kE,
               vK=vK, vBB=vBB, vHBP=vHBP),
          file.path(OUTDIR, sprintf("score_tables_%s.rds", label)))
  invisible(metrics)
}

for (label in c("2025_full", "2026_partial")) run_label(label)
cat(sprintf("\n[done] 최종 지표 저장: %s\n", OUTDIR))
cat("  · metrics_{batter,pitcher}_<label>.csv  (DRV·DRV+·DRWAR — Script2·3 입력)\n")
cat("  · pa_detail_<label>.csv · score_tables_<label>.rds\n")
