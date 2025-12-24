# source/ - レガシーソースデータ

移行元システムの生データを格納するディレクトリ。

**重要**: このディレクトリは **READ ONLY** です。変更しないでください。

## ディレクトリ構成

```
source/
├── navs/                    # NAVSシステム
│   ├── cobol/               # COBOLソースコード
│   │   ├── programs/        # プログラム (.cob, .cbl)
│   │   └── copybooks/       # コピー句 (.cpy)
│   ├── datas/               # データファイル
│   ├── documents/           # 原本ドキュメント (Excel+画像)
│   │   ├── displaies/       # 画面ドキュメント
│   │   └── libraries/       # ライブラリドキュメント
│   └── function-specs/      # 機能仕様書
│
├── regaze/                  # REGAZEシステム関連
│
└── ssc/                     # SSC関連
```

## 注意事項

- このディレクトリのファイルは変更禁止
- 分析結果は `analysis/` に出力
- 変換済みデータは `repository/` に格納
