# pjm/ - プロジェクト管理

プロジェクト管理関連の資料を格納するディレクトリ。

## ディレクトリ構成

```
pjm/
├── meetings/                # 会議体資料
│   ├── steering/2026/       # ステアリングコミッティ
│   ├── weekly/2026/         # 週次定例
│   ├── review/              # レビュー会議
│   └── adhoc/               # 臨時会議
│
├── plans/                   # 計画書
│   ├── master-schedule/     # マスタースケジュール
│   ├── wbs/                 # WBS
│   ├── milestones/          # マイルストーン
│   └── resource/            # リソース計画
│
├── reports/                 # 報告書
│   ├── progress/            # 進捗報告
│   ├── issues/              # 課題管理
│   ├── risks/               # リスク管理
│   └── quality/             # 品質報告
│
├── communications/          # コミュニケーション
│   ├── minutes/             # 議事録
│   ├── decisions/           # 決定事項
│   └── announcements/       # 周知事項
│
└── templates/               # ドキュメントテンプレート
```

## 命名規則

- 会議資料: `YYYYMMDD-{会議種別}-{内容}.{ext}`
- 例: `20260401-steering-kickoff.pptx`

## 注意事項

- 契約関連 (proposals/quotations/agreements) は Git 管理対象外
- 別途セキュアなストレージで管理すること
