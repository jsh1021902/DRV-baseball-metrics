###############################################################################
# 07_export_webapp_data.R
#   반응형 웹앱(타석 시뮬레이터)용 데이터 내보내기 — webapp/webapp_data.js
#   2026 개막~5/23 데이터 기준. 사용자가 존·구종·결과를 누르며 한 타석을
#   시뮬레이션하면, 그 선수의 시즌 DRV·DRV+·DRWAR이 실시간 갱신되도록
#   필요한 룩업표·기준값·리그 상수를 JSON(JS)로 출력한다.
#
#   입력 : Final Code/output/{score_tables, metrics_batter, pa_detail}_2026_partial.*
#          output/DRV/RE_count_2026_partial.csv
#   출력 : Final Code/webapp/webapp_data.js   (const APP_DATA = {...})
###############################################################################

suppressPackageStartupMessages({ library(dplyr); library(jsonlite) })

## -- 경로/공용설정: 저장소의 R/00_paths.R 로드 (실행 위치와 무관하게 동작) --
local({
  .a <- commandArgs(FALSE); .m <- grep("^--file=", .a, value = TRUE)
  .d <- if (length(.m)) dirname(sub("^--file=", "", .m[1])) else "R"
  source(file.path(.d, "00_paths.R"))
})
dir.create(WEBDIR, showWarnings = FALSE)
read_bom <- function(p) read.csv(p, stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM", check.names=FALSE)

LAB <- "2026_partial"
eb_k <- function(value, group) {                       # 선수레벨 수축 K (01과 동일식)
  g <- data.frame(value, group) %>% group_by(group) %>%
    summarise(n=dplyr::n(), m=mean(value), ss=sum((value-mean(value))^2), .groups="drop")
  N<-sum(g$n); G<-nrow(g); grand<-weighted.mean(g$m,g$n)
  within<-sum(g$ss)/(N-G); SSB<-sum(g$n*(g$m-grand)^2); n0<-(N-sum(g$n^2)/N)/(G-1)
  between<-max((SSB/(G-1)-within)/n0,1e-9); k<-within/between; if(!is.finite(k)||k<0)k<-0; k
}

st  <- readRDS(file.path(OUTDIR, sprintf("score_tables_%s.rds", LAB)))
Mb  <- read_bom(file.path(OUTDIR, sprintf("metrics_batter_%s.csv", LAB)))
Mp  <- read_bom(file.path(OUTDIR, sprintf("metrics_pitcher_%s.csv", LAB)))
pa  <- read_bom(file.path(OUTDIR, sprintf("pa_detail_%s.csv", LAB)))
re  <- read_bom(file.path(OUTDIR, sprintf("RE_count_%s.csv", LAB)))

# 룩업표 → 키 "count|pg|zone"
key <- function(df, val) setNames(as.list(round(df[[val]], 6)),
                                  paste(df$count, df$pg, df$zone, sep="|"))
E_map <- key(st$E_grid, "E_hat")          # 중간투구 기대 ΔRE
B_map <- key(st$B_grid, "B_shrunk")       # 상황 허들 B
RE_map <- setNames(as.list(round(re$RE, 6)), re$count)
# 타구질 격자 A[EV,LA] (키 "ev_la", 5단위 구간 하한)
A_map <- setNames(as.list(round(st$A_grid$A_shrunk, 6)),
                  paste(st$A_grid$ev_bin, st$A_grid$la_bin, sep="_"))

# 인플레이 확정 V (안타=단타 평균, 홈런=홈런 평균) — pa_detail 의 V
Vsingle <- mean(pa$V[pa$events=="single"], na.rm=TRUE)
VHR     <- mean(pa$V[pa$events=="home_run"], na.rm=TRUE)

# 포지션별 리그 상수 (DRV+ / DRWAR 재계산용)
Kb <- eb_k(pa$DRV, pa$batter); Kp <- eb_k(pa$DRV, pa$pitcher)
side_const <- function(Mdf, oppcol, K) {
  leag <- weighted.mean(Mdf$DRV_per_PA, Mdf[[oppcol]])
  rs   <- leag + (Mdf$DRV_per_PA - leag) * (Mdf[[oppcol]]/(Mdf[[oppcol]]+K))
  list(leag = round(leag,6),
       scale = round(10 / sd(rs[Mdf$qualified %in% c(TRUE,"TRUE")]), 4),
       Kplayer = round(K,3),
       league_pp = round(weighted.mean(Mdf$DRV_total/Mdf$N_p, Mdf$N_p), 6))
}
CB <- side_const(Mb, "PA", Kb)     # 타자
CP <- side_const(Mp, "BF", Kp)     # 투수

# 선수 목록 (타자 + 투수, side 구분)
players <- bind_rows(
  Mb %>% transmute(id=mlbam_id, name, side="타자", opp=PA, N_p,
                   DRV_total=round(DRV_total,4), DRV_per_PA=round(DRV_per_PA,6)),
  Mp %>% transmute(id=mlbam_id, name, side="투수", opp=BF, N_p,
                   DRV_total=round(DRV_total,4), DRV_per_PA=round(DRV_per_PA,6))
) %>% arrange(side, desc(DRV_total))

APP <- list(
  season = "2026 개막 ~ 5/23",
  pitches = c("직구","싱커","커터","슬라","커브","체인지"),
  zones_inner = c(1:9), zones_outer = c(11:14),
  RE = RE_map, E = E_map, B = B_map, A = A_map,
  muE = round(st$muE,6), muB = round(st$muB,6), muA = round(st$muA,6),
  vK = round(st$vK,6), vBB = round(st$vBB,6), vHBP = round(st$vHBP,6),
  Vsingle = round(Vsingle,6), VHR = round(VHR,6),
  r = 0.0085, RPW = 10,
  league = list("타자"=CB, "투수"=CP),     # 포지션별 상수
  players = players)

js <- paste0("// 자동 생성 (07_export_webapp_data.R) — 2026 타석 시뮬레이터 데이터\n",
             "const APP_DATA = ",
             toJSON(APP, auto_unbox=TRUE, dataframe="rows", digits=6, na="null"), ";\n")
writeLines(js, file.path(WEBDIR, "webapp_data.js"), useBytes=TRUE)
cat(sprintf("[done] %s\n  선수 %d명(타자 %d·투수 %d) · E격자 %d칸 · B격자 %d칸 · RE %d카운트\n",
            file.path(WEBDIR,"webapp_data.js"), nrow(players), nrow(Mb), nrow(Mp),
            length(E_map), length(B_map), length(RE_map)))
cat(sprintf("  Vsingle=%.3f VHR=%.3f\n  타자: leag=%.5f scale=%.0f K=%.0f lpp=%.5f\n  투수: leag=%.5f scale=%.0f K=%.0f lpp=%.5f\n",
            Vsingle, VHR, CB$leag, CB$scale, CB$Kplayer, CB$league_pp,
            CP$leag, CP$scale, CP$Kplayer, CP$league_pp))
