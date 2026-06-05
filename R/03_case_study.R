###############################################################################
# 03_case_study.R   (시즌별 대표 2명씩 — 타자·투수 케이스 스터디)
#
#   각 시즌(2025·2026)마다 타자 2명 + 투수 2명(기본: DRWAR 상위 2명, 대표 스타)을
#   골라, 우리 지표가 그 선수를 '어떻게·왜' 그렇게 평가했는지 분해해서 보여준다.
#     · 가치 분해 : 결과 가치(ΣPA Score) + 과정 가치(Σ선구안) = DRV
#                   + 대체수준 기여(r·N_p) → DRWAR(승)
#     · 효율/누적 : DRV+ (타석당 효율, 100기준) · DRWAR (누적 승) · 각 순위
#     · 기존지표  : fWAR · bWAR · wRC+/ERA+ 와 나란히 (순위 일치 확인)
#     · 타석구성  : 인플레이/삼진/볼넷/사구 분포, 평균 PA Score
#
#   대표 선수 교체 : CONFIG 리스트에 이름을 직접 적으면 그 선수로 케이스 진행.
#   입력 : Final Code/output/{metrics_*, pa_detail_*}.csv, data/<yr>/{fwar,bwar}_*
#   출력 : Final Code/output/case_study_summary.csv  (+ 콘솔 내러티브)
###############################################################################

suppressPackageStartupMessages({ library(dplyr) })

## -- 경로/공용설정: 저장소의 R/00_paths.R 로드 (실행 위치와 무관하게 동작) --
local({
  .a <- commandArgs(FALSE); .m <- grep("^--file=", .a, value = TRUE)
  .d <- if (length(.m)) dirname(sub("^--file=", "", .m[1])) else "R"
  source(file.path(.d, "00_paths.R"))
})
write_excel_csv <- function(df, path) {
  con <- file(path, open = "wb", encoding = "UTF-8"); on.exit(close(con))
  writeBin(charToRaw("﻿"), con); write.csv(df, con, row.names = FALSE, fileEncoding = "")
  invisible(path)
}
read_bom <- function(p) read.csv(p, stringsAsFactors = FALSE,
                                 fileEncoding = "UTF-8-BOM", check.names = FALSE)
dedupe_max <- function(df, idc, byc) df %>% filter(!is.na(.data[[idc]])) %>%
  group_by(.data[[idc]]) %>% slice_max(.data[[byc]], n=1, with_ties=FALSE) %>% ungroup()
R_REPL <- 0.0085; RPW <- 10

# 대표 선수 직접 지정 가능 (NULL = DRWAR 상위 2명 자동) ----------------------
CONFIG <- list(
  "2025_full"    = list(batter = NULL, pitcher = NULL),
  "2026_partial" = list(batter = NULL, pitcher = NULL))

# 기존지표(벤치마크) 로더
bench <- function(label, side) {
  yr <- if (label=="2025_full") "2025" else "2026"
  ddir <- file.path(ROOT, "data", sprintf("%s_data_csv", yr))
  fw <- read.csv(file.path(ddir, sprintf("fwar_%s_%s.csv", yr, side)),
                 stringsAsFactors=FALSE, check.names=FALSE)
  bw <- read.csv(file.path(ddir, sprintf("bwar_%s_%s.csv", yr, side)),
                 stringsAsFactors=FALSE, check.names=FALSE)
  if (side=="batter") {
    fw <- dedupe_max(fw,"xMLBAMID","PA") %>%
      transmute(id=as.integer(xMLBAMID), fWAR=as.numeric(WAR), rate_bench=as.numeric(wRC_plus))
    bw <- dedupe_max(bw,"mlb_ID","PA") %>% transmute(id=as.integer(mlb_ID), bWAR=as.numeric(WAR))
    rl <- "wRC+"
  } else {
    fw <- dedupe_max(fw,"xMLBAMID","IP") %>% transmute(id=as.integer(xMLBAMID), fWAR=as.numeric(WAR))
    bw <- dedupe_max(bw,"mlb_ID","IPouts") %>%
      transmute(id=as.integer(mlb_ID), bWAR=as.numeric(WAR), rate_bench=as.numeric(ERA_plus))
    rl <- "ERA+"
  }
  list(tab = fw %>% full_join(bw, by="id"), rate_lab = rl)
}

rows <- list()
for (label in c("2025_full", "2026_partial")) {
  yr_disp <- if (label=="2025_full") "2025 풀시즌" else "2026 개막~5/23"
  pa_all <- read_bom(file.path(OUTDIR, sprintf("pa_detail_%s.csv", label)))
  cat(sprintf("\n##################  %s  ##################\n", yr_disp))
  for (side in c("batter","pitcher")) {
    who  <- if (side=="batter") "타자" else "투수"
    ncol <- if (side=="batter") "PA" else "BF"
    sgn  <- if (side=="batter") 1 else -1
    M <- read_bom(file.path(OUTDIR, sprintf("metrics_%s_%s.csv", side, label)))
    q <- M %>% filter(qualified %in% c(TRUE,"TRUE")) %>%
      mutate(DRWAR_rank = rank(-DRWAR, ties.method="min"),
             DRVplus_rank = rank(-DRVplus, ties.method="min"))
    bc <- bench(label, side); B <- bc$tab
    Bq <- B %>% filter(id %in% q$mlbam_id) %>%
      mutate(fWAR_rank = rank(-fWAR, ties.method="min", na.last="keep"))

    # 타석별 결과/과정 분해 (선수별)
    pid <- pa_all[[side]]
    dec <- pa_all %>% mutate(pid = pid) %>% group_by(pid) %>%
      summarise(n_INPLAY=sum(kind=="INPLAY"), n_K=sum(kind=="K"),
                n_BB=sum(kind=="BB"), n_HBP=sum(kind=="HBP"),
                result_runs = sgn*sum(PA_score), process_runs = sgn*sum(process),
                mean_PA_score = sgn*mean(PA_score), .groups="drop")

    # 대표 2명 선정
    sel <- CONFIG[[label]][[side]]
    reps <- if (is.null(sel)) q %>% arrange(desc(DRWAR)) %>% slice(1:2) %>% pull(name) else sel
    cat(sprintf("\n=====  [%s] 대표 2명: %s  =====\n", who, paste(reps, collapse=", ")))

    for (nm in reps) {
      r <- q %>% filter(name == nm) %>% slice(1)
      if (nrow(r)==0) { cat(sprintf("  (없음/규정미달: %s)\n", nm)); next }
      d <- dec %>% filter(pid == r$mlbam_id) %>% slice(1)
      bb <- Bq %>% filter(id == r$mlbam_id) %>% slice(1)
      repl_W  <- R_REPL * r$N_p / RPW
      above_W <- r$DRWAR - repl_W
      prof <- data.frame(
        season=yr_disp, side=who, name=nm,
        opp = r[[ncol]], N_p = r$N_p,
        result_runs = round(d$result_runs,1), process_runs = round(d$process_runs,1),
        DRV_total = r$DRV_total,
        above_avg_W = round(above_W,2), replacement_W = round(repl_W,2),
        DRVplus = r$DRVplus, DRVplus_rank = r$DRVplus_rank,
        DRWAR = r$DRWAR, DRWAR_rank = r$DRWAR_rank,
        fWAR = if(nrow(bb)) round(bb$fWAR,1) else NA,
        bWAR = if(nrow(bb)) round(bb$bWAR,1) else NA,
        rate_bench_name = bc$rate_lab,
        rate_bench = if(nrow(bb)) round(bb$rate_bench,0) else NA,
        fWAR_rank = if(nrow(bb)) bb$fWAR_rank else NA,
        n_INPLAY=d$n_INPLAY, n_K=d$n_K, n_BB=d$n_BB, n_HBP=d$n_HBP,
        mean_PA_score = round(d$mean_PA_score,3))
      rows[[length(rows)+1]] <- prof

      cat(sprintf("\n  ● %s  (%s %d · %d투구)\n", nm, ncol, prof$opp, prof$N_p))
      cat(sprintf("    가치분해 : 결과 %+.1f런 + 과정(선구안) %+.1f런 = DRV %+.1f런\n",
                  prof$result_runs, prof$process_runs, prof$DRV_total))
      cat(sprintf("    승수환산 : 평균대비 %+.2f승 + 대체수준 %+.2f승 = DRWAR %+.2f승 (리그 %d위)\n",
                  prof$above_avg_W, prof$replacement_W, prof$DRWAR, prof$DRWAR_rank))
      cat(sprintf("    효율지수 : DRV+ %.0f (리그 %d위)\n", prof$DRVplus, prof$DRVplus_rank))
      cat(sprintf("    기존지표 : fWAR %.1f · bWAR %.1f · %s %s  (fWAR %d위 ↔ DRWAR %d위)\n",
                  prof$fWAR, prof$bWAR, prof$rate_bench_name, prof$rate_bench,
                  ifelse(is.na(prof$fWAR_rank),0,prof$fWAR_rank), prof$DRWAR_rank))
      cat(sprintf("    타석구성 : 인플레이 %d · 삼진 %d · 볼넷 %d · 사구 %d · 평균 PA Score %+.3f\n",
                  prof$n_INPLAY, prof$n_K, prof$n_BB, prof$n_HBP, prof$mean_PA_score))
    }
  }
}
out <- bind_rows(rows)
write_excel_csv(out, file.path(OUTDIR, "case_study_summary.csv"))
cat(sprintf("\n[done] 케이스 스터디 저장: %s\n", file.path(OUTDIR, "case_study_summary.csv")))
