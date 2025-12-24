# analysis/ - 分析成果物

各種分析の成果物を格納するディレクトリ。

## ディレクトリ構成

```
analysis/
├── legacy/                  # レガシーシステム分析
│   ├── findings/            # 分析所見
│   ├── mapping/             # 新旧マッピング
│   └── gaps/                # ギャップ分析
│
├── business/                # 業務分析
│   ├── processes/           # 業務プロセス
│   ├── requirements/        # 要件
│   └── use-cases/           # ユースケース
│
└── data/                    # データ分析
    ├── models/              # データモデル
    └── quality/             # データ品質
```

## 用途

| カテゴリ | 用途 |
|----------|------|
| `legacy/` | NAVSシステムの分析、移行計画 |
| `business/` | 業務要件、プロセス定義 |
| `data/` | データモデリング、品質評価 |
