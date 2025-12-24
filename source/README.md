# source/ - レガシーソースデータ

移行元システムの生データを格納するディレクトリ。

**重要**: このディレクトリは **READ ONLY** です。変更しないでください。

## ディレクトリ構成

```
source/
├── navs/                    # NAVSシステム
│   ├── cobol/               # COBOLソースコード
│   │   ├── programs/        # プログラム (.COB, .CL)
│   │   │   ├── TOKSLIB/     # COBOLプログラム1 (911 files)
│   │   │   ├── TOKSLIBS/    # COBOLプログラム2 (1,416 files)
│   │   │   ├── TOKSRLIB/    # COBOLプログラム3 (850 files)
│   │   │   ├── TOKCLIB/     # JCL (995 files)
│   │   │   ├── TOKCLIBS/    # JCL (1,090 files)
│   │   │   └── TOKCLLIB/    # JCL (463 files)
│   │   └── copybooks/       # コピー句 (.cpy)
│   ├── datas/               # データファイル
│   ├── documents/           # 原本ドキュメント (Excel+画像)
│   │   ├── displaies/       # 画面ドキュメント
│   │   ├── menus/           # メニュー資料
│   │   │   ├── 本社メニュー/  # 本社メニュー (261 files)
│   │   │   └── 倉庫メニュー/  # 倉庫メニュー (272 files)
│   │   └── 流通BMS登録情報/  # 流通BMS設定情報
│   ├── function-specs/      # 機能仕様書
│   ├── ライブラリ一覧.xlsx   # ライブラリ一覧 (原本)
│   └── ライブラリ一覧.md     # ライブラリ一覧 (Markdown変換)
│
├── regaze/                  # REGAZEシステム関連
│
└── ssc/                     # SSC関連
```

## ファイル統計

| ライブラリ | 種別 | ファイル数 |
|-----------|------|----------:|
| TOKSLIB | COBOL (.COB) | 911 |
| TOKSLIBS | COBOL (.COB) | 1,412 |
| TOKSRLIB | COBOL (.COB) | 850 |
| TOKCLIB | JCL (.CL) + COBOL | 995 |
| TOKCLIBS | JCL (.CL) | 1,090 |
| TOKCLLIB | JCL (.CL) | 463 |
| **合計** | | **5,721** |

## NAVSライブラリ構成

詳細は [ライブラリ一覧.md](navs/ライブラリ一覧.md) を参照。

| 分類 | ライブラリ |
|------|-----------|
| COBOL プログラム | TOKSLIB, TOKSLIBS, TOKSRLIB, OSKSLIB |
| JCL | TOKCLIB, TOKCLIBS, TOKCLLIB |
| オブジェクト格納 | TOKELIB, TOKELIBO, TOKCLIBO, TOKSOLIB, TOKCOLIB, OSKELIB |
| データ格納 | TOKFLIB, TOKKLIB, TOKDLIB, TOKDTLIB, ONLBLIB, D365DLIB, TOKWLIB, TOKJLIB, TOKWWLIB, OSKFLIB |
| 画面帳票定義体 | TOKMDLIB |
| メニュー | TOKMLIB, TOKMLIBS, TOKMLIBO |

## 注意事項

- このディレクトリのファイルは変更禁止
- 分析結果は `analysis/` に出力
- 変換済みデータは `repository/` に格納
