# data/ — 원자료 배치 안내

저작권/용량 문제로 원자료는 저장소에 포함하지 않는다. 아래 구조로 직접 배치하면
`R/01_build_metrics.R` 부터 그대로 실행된다. (출처: MLB **Statcast**, FanGraphs(fWAR·wRC+),
Baseball-Reference(bWAR·ERA+).)

```
data/
├── 2025_data_csv/
│   ├── statcast_2025.csv          # Statcast 투구 단위(pitch-by-pitch) — 2025 전체 시즌
│   ├── fwar_2025_batter.csv       # FanGraphs 타자: 열 xMLBAMID, WAR, wRC_plus, PA
│   ├── fwar_2025_pitcher.csv      # FanGraphs 투수: 열 xMLBAMID, WAR, IP
│   ├── bwar_2025_batter.csv       # B-Ref 타자:    열 mlb_ID, WAR, PA
│   └── bwar_2025_pitcher.csv      # B-Ref 투수:    열 mlb_ID, WAR, ERA_plus, IPouts
├── 2026_data_csv/
│   └── ( 위와 동일, 2026 개막~5/23 )  statcast_2026.csv, fwar_2026_*, bwar_2026_*
└── Raw data/
    └── id_map.rds                 # 선수 ID 매핑 (key_mlbam ↔ name)
```

**Statcast 주요 열** : `delta_run_exp`(ΔRE), `launch_speed`(EV), `launch_angle`(LA),
`zone`, `balls`, `strikes`, `stand`, `pitch_type`, `description`, `events`,
`game_pk`, `at_bat_number`, `batter`, `pitcher`, `game_date`.

> `output/RE_count_2025_full.csv`, `output/RE_count_2026_partial.csv` 는 카운트별 기대득점표로,
> 웹앱 내보내기(`05_export_webapp_data.R`) 입력으로 저장소에 동봉되어 있다.
