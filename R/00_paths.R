###############################################################################
# 00_paths.R  ── 저장소 공용 경로 + 헬퍼 (다른 스크립트가 source 로 불러 씀)
#
#   ROOT   : 저장소 루트 (이 파일의 부모 폴더 = R/ 의 부모). 자동 탐지.
#            환경변수 DRV_ROOT 로 강제 지정 가능.
#   DATA   : 원자료 폴더            (ROOT/data)
#   OUTDIR : 산출물(지표·요약) 폴더 (ROOT/output)
#   FIGDIR : 그림 폴더             (ROOT/output/figures)
#   WEBDIR : 웹앱 폴더             (ROOT/webapp)
#
#   공용 헬퍼 : write_excel_csv() · read_bom() · eb_k()
###############################################################################

# ── 저장소 루트 자동 탐지 ───────────────────────────────────────────────────
ROOT <- local({
  env <- Sys.getenv("DRV_ROOT", "")
  if (nzchar(env)) return(normalizePath(env, mustWork = FALSE))
  a <- commandArgs(FALSE); m <- grep("^--file=", a, value = TRUE)
  if (length(m)) {                                   # Rscript R/xx.R 로 실행한 경우
    return(normalizePath(file.path(dirname(sub("^--file=", "", m[1])), ".."), mustWork = FALSE))
  }
  of <- NULL                                          # source() / RStudio 인 경우
  for (i in rev(seq_len(sys.nframe()))) { o <- sys.frame(i)$ofile; if (!is.null(o)) { of <- o; break } }
  if (!is.null(of)) return(normalizePath(file.path(dirname(of), ".."), mustWork = FALSE))
  if (dir.exists("R")) return(normalizePath(".", mustWork = FALSE))   # 저장소 루트에서 실행
  normalizePath(getwd(), mustWork = FALSE)
})

DATA   <- file.path(ROOT, "data")
OUTDIR <- file.path(ROOT, "output");          dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)
FIGDIR <- file.path(OUTDIR, "figures");       dir.create(FIGDIR, showWarnings = FALSE, recursive = TRUE)
WEBDIR <- file.path(ROOT, "webapp")
message(sprintf("[00_paths] ROOT = %s", ROOT))

# ── 공용 헬퍼 ───────────────────────────────────────────────────────────────
# 한글 Excel 호환 CSV (UTF-8 BOM)
write_excel_csv <- function(df, path) {
  con <- file(path, open = "wb", encoding = "UTF-8"); on.exit(close(con))
  writeBin(charToRaw("﻿"), con)
  write.csv(df, con, row.names = FALSE, fileEncoding = "")
  invisible(path)
}
# BOM 포함 CSV 읽기
read_bom <- function(p) read.csv(p, stringsAsFactors = FALSE,
                                 fileEncoding = "UTF-8-BOM", check.names = FALSE)

# Empirical Bayes 수축 강도 K = sigma2_within / sigma2_between (random-effects 적률추정, base R)
eb_k <- function(value, group) {
  ok <- is.finite(value) & !is.na(group)
  value <- value[ok]; group <- as.character(group)[ok]
  if (!length(value)) return(0)
  ag <- tapply(value, group, function(v) c(n = length(v), m = mean(v), ss = sum((v - mean(v))^2)))
  M  <- do.call(rbind, ag); n <- M[, "n"]; m <- M[, "m"]; ss <- M[, "ss"]
  N <- sum(n); G <- length(n)
  if (G < 2 || N <= G) return(0)
  grand   <- weighted.mean(m, n)
  within  <- sum(ss) / (N - G)
  n0      <- (N - sum(n^2) / N) / (G - 1)
  between <- max((sum(n * (m - grand)^2) / (G - 1) - within) / n0, 1e-9)
  k <- within / between
  if (!is.finite(k) || k < 0) 0 else k
}
