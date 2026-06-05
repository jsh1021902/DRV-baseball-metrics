###############################################################################
# 02_compare_war.R   (지표 타당성 검증 — fWAR · bWAR 와의 상관관계 비교)
#
#   가설: DRV+ / DRWAR 가 진짜 실력을 측정한다면, 같은 선수를 기존 공인 지표
#         (fWAR · bWAR · wRC+ · ERA+)와 매겼을 때 순위가 강하게 일치해야 함.
#
#   비교 짝(정당한 척도 매칭):
#     · DRWAR(누적 승) ↔ fWAR · bWAR (누적 승)          ← 같은 누적·승 단위
#     · DRV+(타석당 효율) ↔ wRC+(타자) · ERA+(투수)      ← 같은 비율·100기준
#     · (참고) DRV+ ↔ fWAR/bWAR : 효율 vs 누적이라 척도 불일치
#
#   입력 : Final Code/output/metrics_{batter,pitcher}_<label>.csv
#          data/<yr>/{fwar,bwar}_<yr>_{batter,pitcher}.csv
#   출력 : Final Code/output/war_correlation_summary.csv  (+ 콘솔)
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
# 한 선수 중복(트레이드 등) → 표본 큰 행만
dedupe_max <- function(df, idc, byc) df %>% filter(!is.na(.data[[idc]])) %>%
  group_by(.data[[idc]]) %>% slice_max(.data[[byc]], n=1, with_ties=FALSE) %>% ungroup()
# 상관 (결측 제거, 최소 5명)
corr <- function(x, y) { ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 5) return(c(n=sum(ok), pearson=NA, spearman=NA))
  c(n = sum(ok), pearson = round(cor(x[ok], y[ok], method="pearson"), 3),
    spearman = round(cor(x[ok], y[ok], method="spearman"), 3)) }

rows <- list()
for (label in c("2025_full", "2026_partial")) {
  yr <- if (label=="2025_full") "2025" else "2026"
  ddir <- file.path(ROOT, "data", sprintf("%s_data_csv", yr))
  cat(sprintf("\n================  [%s]  ================\n", label))
  for (side in c("batter","pitcher")) {
    m <- read_bom(file.path(OUTDIR, sprintf("metrics_%s_%s.csv", side, label))) %>%
      filter(qualified %in% c(TRUE, "TRUE")) %>%
      transmute(id = as.integer(mlbam_id), name, DRV_total, DRVplus, DRWAR)

    fw <- read.csv(file.path(ddir, sprintf("fwar_%s_%s.csv", yr, side)),
                   stringsAsFactors=FALSE, check.names=FALSE)
    bw <- read.csv(file.path(ddir, sprintf("bwar_%s_%s.csv", yr, side)),
                   stringsAsFactors=FALSE, check.names=FALSE)
    if (side == "batter") {
      fw <- dedupe_max(fw, "xMLBAMID", "PA") %>%
        transmute(id = as.integer(xMLBAMID), fWAR = as.numeric(WAR),
                  rate_bench = as.numeric(wRC_plus))
      bw <- dedupe_max(bw, "mlb_ID", "PA") %>%
        transmute(id = as.integer(mlb_ID), bWAR = as.numeric(WAR))
      rate_lab <- "wRC+"
    } else {
      fw <- dedupe_max(fw, "xMLBAMID", "IP") %>%
        transmute(id = as.integer(xMLBAMID), fWAR = as.numeric(WAR))
      bw <- dedupe_max(bw, "mlb_ID", "IPouts") %>%       # bWAR 투수 이닝 = IPouts/3
        transmute(id = as.integer(mlb_ID), bWAR = as.numeric(WAR),
                  rate_bench = as.numeric(ERA_plus))
      rate_lab <- "ERA+"
    }
    d <- m %>% left_join(fw, by="id") %>% left_join(bw, by="id")

    # 비교 짝 정의
    pairs <- list(
      # 정당한 짝 — 같은 척도(누적↔누적 / 비율↔비율)
      c("DRWAR",  "fWAR",       "누적↔누적 (정당)"),
      c("DRWAR",  "bWAR",       "누적↔누적 (정당)"),
      c("DRVplus","rate_bench", sprintf("효율↔%s (정당)", rate_lab)),
      # 참고 — 척도 불일치(비율↔누적): DRV+ 는 출전량을 안 곱하므로 누적WAR과 단위가 다름
      c("DRVplus","fWAR",       "효율↔누적 (참고·척도불일치)"),
      c("DRVplus","bWAR",       "효율↔누적 (참고·척도불일치)"),
      c("DRV_total","fWAR",     "누적런↔누적승 (참고)"))
    who <- if (side=="batter") "타자" else "투수"
    cat(sprintf("\n[%s] 규정 선수 %d명 (fWAR매칭 %d · bWAR매칭 %d)\n",
                who, nrow(d), sum(is.finite(d$fWAR)), sum(is.finite(d$bWAR))))
    for (p in pairs) {
      r <- corr(d[[p[1]]], d[[p[2]]])
      yb <- if (p[2]=="rate_bench") rate_lab else p[2]
      cat(sprintf("   %-9s vs %-6s  Pearson=%-6s Spearman=%-6s  (n=%d) %s\n",
                  p[1], yb, r["pearson"], r["spearman"], r["n"], p[3]))
      rows[[length(rows)+1]] <- data.frame(
        season=label, side=who, x=p[1], y=yb, relation=p[3],
        n=r["n"], pearson=r["pearson"], spearman=r["spearman"])
    }
  }
}
summary_tbl <- bind_rows(rows)
write_excel_csv(summary_tbl, file.path(OUTDIR, "war_correlation_summary.csv"))
cat(sprintf("\n[done] 상관관계 요약 저장: %s\n", file.path(OUTDIR, "war_correlation_summary.csv")))

# 핵심 결론 요약
core <- summary_tbl %>% filter(grepl("정당", relation))
cat("\n=== 핵심(정당한 짝) 평균 상관 ===\n")
core %>% group_by(x, y) %>%
  summarise(mean_spearman = round(mean(spearman, na.rm=TRUE), 3), .groups="drop") %>%
  arrange(desc(mean_spearman)) %>% as.data.frame() %>% print(row.names=FALSE)
