# DRV · DRV+ · DRWAR — 타구 질·상황 난이도 기반 베이지안 야구 평가지표

STAT 9703-01 *Analysis of Sports Big Data* (Yonsei University) · **Team 6** — 이승민 · 이연제 · 정달민 · 정서현

기존 야구 평가지표(wRC+·ERA+·WAR)는 대부분 **결과(outcome) 기반**이라 ① 수비·BABIP 운, ② 상황 난이도,
③ 타격 중간 과정(선구안·커맨드), ④ 소표본 불안정을 제대로 다루지 못한다. 본 프로젝트는 이 네 빈틈을
메우는 **환경 독립적(environment-independent)** 가치 지표를 제안한다.

| 지표 | 형식 | 한 줄 정의 |
|---|---|---|
| **DRV** (Deserved Run Value) | 런(run) | `결과 가치(PA Score) + 과정 가치(선구안/커맨드)` |
| **DRV+** | 효율 지수 (리그 평균 = 100) | 타석 당(per-PA) DRV를 100 기준으로 변환 (선수레벨 베이즈 수축) |
| **DRWAR** (Deserved WAR) | 누적 승수 | DRV를 대체수준·RPW로 환산한 WAR |

핵심 도구는 **Empirical Bayesian Shrinkage** — 표본이 적은 칸/선수의 추정치를 리그 평균으로 끌어당겨
우연(노이즈)을 제거하되 실력 신호는 보존한다.

---

## 폴더 구조

```
DRV-baseball-metrics/
├── R/                          # 분석 파이프라인 (실행 순서 = 파일 번호)
│   ├── 00_paths.R              # 공용: 경로 자동 탐지 + 헬퍼(write_excel_csv, read_bom, eb_k)
│   ├── 01_build_metrics.R      # [핵심] Statcast → DRV / DRV+ / DRWAR 산출
│   ├── 02_validate_vs_war.R    # fWAR·bWAR·wRC+·ERA+ 와 순위상관 검증
│   ├── 03_case_study.R         # 대표 선수 결과+과정 분해 요약
│   ├── 04_make_figures.R       # 모든 그림 생성 (상관·분해·비교·선수카드)
│   └── 05_export_webapp_data.R # 웹앱(타석 시뮬레이터)용 데이터(JS) 내보내기
├── webapp/                     # 인터랙티브 타석 시뮬레이터 (브라우저에서 index.html 열기)
│   ├── index.html  index_src.html  app.js  webapp_data.js  build.sh  JalnanGothic.ttf
├── output/                     # 산출물 (01~04가 생성). RE_count_*.csv 는 05 입력용으로 동봉
│   └── figures/                # 생성된 그림 PNG
├── data/                       # 원자료 (직접 배치 — data/README.md 참고, 저장소엔 비포함)
└── README.md
```

각 단계의 입력→출력은 스크립트 상단 주석에 명시되어 있다.

---

## 실행 방법

> **저장소 루트에서 실행**하면 경로가 자동으로 잡힌다. (다른 위치에서 실행하려면 환경변수
> `DRV_ROOT` 에 저장소 경로를 지정.)

```bash
# 1) 원자료를 data/ 에 배치 (data/README.md 참고)
# 2) 순서대로 실행
Rscript R/01_build_metrics.R       # → output/metrics_*, pa_detail_*, score_tables_*
Rscript R/02_validate_vs_war.R     # → output/war_correlation_summary.csv
Rscript R/03_case_study.R          # → output/case_study_summary.csv
Rscript R/04_make_figures.R        # → output/figures/*.png (+ cards/)
Rscript R/05_export_webapp_data.R  # → webapp/webapp_data.js
```

RStudio에서는 프로젝트를 저장소 루트로 열고 각 스크립트를 `Source` 하면 된다.

### 필요 패키지
```r
install.packages(c("dplyr","tidyr","ggplot2","showtext","jsonlite","stringi"))
```
`stringi` 는 선택(선수명 라틴 악센트 제거). 그림의 한글 글꼴은 `webapp/JalnanGothic.ttf`
(또는 시스템에 설치된 Jalnan)를 자동으로 찾고, 없으면 기본 sans 글꼴로 그린다.

---

## 산출 파이프라인 (01_build_metrics.R)

1. **Step 1 — 타구 질 격자 `A[EV,LA]`** : 타구 속도×발사각(5단위 bin)별 평균 ΔRE, 베이즈 수축.
2. **Step 2 — 상황 난이도 격자 `B` (936칸)** : 볼카운트×구종×위치(좌타 존 반전), 베이즈 수축.
3. **Step 3 — PA Score `= V − B`** : 인플레이는 A, 삼진/볼넷/사구는 확정 ΔRE 를 V로.
4. **Step 4 — `DRV = PA Score + Σ(ΔRE_p − Ê_league)`** : 중간 투구의 선구안/커맨드 가치 누적.
5. **Step 5 — `DRV+` / `DRWAR`** : 선수레벨 수축 후 100기준 지수 / 승수 환산(대체수준 r=0.0085, RPW=10).

자세한 수식·검증은 최종 보고서(`SpDA_FinalReport_Team6_casdc.pdf`) 참고.

---

## 인터랙티브 데모 (webapp)

`webapp/index.html` 을 브라우저로 열면 가상의 한 타석을 직접 입력(존·구종·결과·타구질)하며
선택한 선수의 DRV·DRV+·DRWAR 가 실시간으로 바뀌는 것을 볼 수 있다. 데이터는
`05_export_webapp_data.R` 가 `webapp_data.js` 로 내보낸다.
