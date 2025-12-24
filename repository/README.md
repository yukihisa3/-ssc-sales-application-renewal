# repository/ - レガシーシステム解析ユーティリティ

SQLite + React を用いたレガシーシステム解析ツールとデータを格納するディレクトリ。

## ディレクトリ構成

```
repository/
├── webapp/                      # React 解析UI
│
├── database/                    # SQLiteデータベース
│
├── json/                        # 構造化データ (パース済み)
│   ├── datas/
│   │   ├── data-schema/         # データスキーマ定義
│   │   ├── forward-lookup/      # 正引き (名前→定義)
│   │   └── reverse-lookup/      # 逆引き (定義→参照元)
│   └── functions/
│       ├── structure/           # 機能構造
│       ├── details/             # 機能詳細
│       └── dependencies/        # 依存関係
│
├── document-mds/                # Markdown (source/navs/documents から変換)
│   ├── analysed-concepts/       # 分析済み概念
│   ├── libraries/               # ライブラリドキュメント
│   └── summaries/               # 要約
│
├── src/                         # ユーティリティスクリプト
│
└── indexes/                     # 検索用インデックス
```

## データフロー

```
source/navs/documents/     → [変換] → repository/document-mds/
       (Excel+画像)                        (Markdown)

source/navs/function-specs/ → [パース] → repository/json/functions/
source/navs/datas/          → [パース] → repository/json/datas/
```

## 技術スタック

- **Frontend**: React
- **Database**: SQLite
- **Data Format**: JSON, Markdown
