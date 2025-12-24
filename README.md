# SSC Sales Application Renewal

株式会社サカタのタネ 国内営業2部向け NAVS撤廃・移行を伴う営業改革プロジェクト

## 概要

本プロジェクトは、既存のNAVSシステムを撤廃し、新たな営業支援システムへの移行を行う営業改革プロジェクトです。

## ディレクトリ構成

| ディレクトリ | 用途 |
|-------------|------|
| `src/` | ソースコード |
| `lib/` | 外部ライブラリ（シンボリックリンク） |
| `material/` | 資料・ドキュメント |
| `database/` | データベース関連 |
| `source/` | 元データ・ソース |
| `work/` | 作業用ファイル |
| `temp/` | 一時ファイル |
| `analysis/` | 分析・調査 |
| `repository-navs/` | NAVSリポジトリ関連 |

## 外部ライブラリ

- `lib/openai-cli` - OpenAI CLI ライブラリ
- `lib/nanobanana-cli` - Nanobanana CLI ライブラリ

## 開発ルール

- プロジェクトルートにはファイル・ディレクトリを作成しない（README.md, CLAUDE.md, git関連を除く）
- 作業ファイルは必ず適切なサブディレクトリに配置すること
- システムの `/tmp` は使用禁止、一時ファイルは `temp/` を使用
