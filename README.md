# SSC Sales Application Renewal

株式会社サカタのタネ 国内営業2部向け NAVS撤廃・移行を伴う営業改革プロジェクト

## 概要

本プロジェクトは、既存のNAVSシステム（FACOM/COBOL）を撤廃し、新たな営業支援システムへの移行を行う営業改革プロジェクトです。

## 著作権・作成者

- **著作権**: 株式会社サカタのタネ (Sakata Seed Corporation)
- **作成・更新**: REGAZE株式会社 (REGAZE Corporation)

## ディレクトリ構成

各ディレクトリには `README.md` が存在します。詳細は各ディレクトリの README.md を参照してください。

| ディレクトリ | 用途 | README |
|-------------|------|--------|
| `src/` | 新システム アプリケーションコード | [src/README.md](src/README.md) |
| `lib/` | 外部AI/LLMツール（シンボリックリンク） | [lib/README.md](lib/README.md) |
| `source/` | レガシーソースデータ (READ ONLY) | [source/README.md](source/README.md) |
| `repository/` | レガシー解析ユーティリティ (SQLite+React) | [repository/README.md](repository/README.md) |
| `analysis/` | 分析成果物 | [analysis/README.md](analysis/README.md) |
| `pjm/` | プロジェクト管理 | [pjm/README.md](pjm/README.md) |
| `material/` | 設計資料・仕様書 | [material/README.md](material/README.md) |
| `database/` | データベースファイル | - |
| `work/` | ユーザ/LLMエージェント作業領域 | - |
| `temp/` | ユーティリティスクリプト一時出力 | - |

## 外部ライブラリ (lib/)

| ツール | 説明 |
|--------|------|
| `openai-cli` | OpenAI GPT CLI - 対話モード、ファイルI/O、セッション管理 |
| `nanobanana-cli` | Google Gemini Pro 画像生成 CLI |
| `claude-cli` | Claude API CLI - ファイル入力、Markdown出力、セッション管理 |

## 開発ルール

- プロジェクトルートにはファイル・ディレクトリを作成しない（README.md, CLAUDE.md, git関連を除く）
- 作業ファイルは必ず適切なサブディレクトリに配置すること
- システムの `/tmp` は使用禁止、作業ファイルは `work/` を使用
- 契約関連資料は Git 管理対象外

## 技術スタック

- **Frontend**: React, TypeScript
- **Backend**: Node.js (予定)
- **Database**: PostgreSQL
- **Infrastructure**: AWS (Terraform)
- **Legacy Analysis**: SQLite, React
