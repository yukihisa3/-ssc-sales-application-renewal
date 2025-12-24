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
├── source-mds/                  # ソースコード Markdown変換
│   ├── analysed-concepts/       # 分析済み概念
│   ├── libraries/               # ソースコード (COBOL/JCL → Markdown)
│   │   ├── TOKSLIB/             # COBOLプログラム1 (911 files)
│   │   ├── TOKSLIBS/            # COBOLプログラム2 (1,416 files)
│   │   ├── TOKSRLIB/            # COBOLプログラム3 (850 files)
│   │   ├── TOKCLIB/             # JCL (995 files)
│   │   ├── TOKCLIBS/            # JCL (1,090 files)
│   │   └── TOKCLLIB/            # JCL (463 files)
│   └── summaries/               # 要約
│
├── src/                         # ユーティリティスクリプト
│
└── indexes/                     # 検索用インデックス
```

## 変換済みファイル

### source-mds/libraries/

| ライブラリ | 種別 | ファイル数 | 命名規則 |
|-----------|------|----------:|---------|
| TOKSLIB | COBOL | 911 | `{name}-COB.md` |
| TOKSLIBS | COBOL | 1,412 | `{name}-COB.md` |
| TOKSRLIB | COBOL | 850 | `{name}-COB.md` |
| TOKCLIB | JCL/COBOL | 995 | `{name}-CL.md` / `{name}-COB.md` |
| TOKCLIBS | JCL | 1,090 | `{name}-CL.md` |
| TOKCLLIB | JCL | 463 | `{name}-CL.md` |
| **合計** | | **5,721** | |

## データフロー

```
source/navs/cobol/programs/ → [変換] → repository/source-mds/libraries/
       (.COB, .CL)                        ({name}-COB.md, {name}-CL.md)

source/navs/function-specs/ → [パース] → repository/json/functions/
source/navs/datas/          → [パース] → repository/json/datas/
```

## 技術スタック

- **Frontend**: React
- **Database**: SQLite
- **Data Format**: JSON, Markdown
