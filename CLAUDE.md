# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

株式会社サカタのタネ 国内営業2部向け NAVS撤廃・移行を伴う営業改革プロジェクト (SSC Sales Application Renewal)

## 厳命事項 (MANDATORY RULES)

### 1. ファイル・ディレクトリ作成の制限
- プロジェクトルートには `CLAUDE.md`、`README.md`、git関連ファイル以外を作成してはならない
- 作業ファイルは必ず適切なサブディレクトリ配下に作成すること

### 2. README.md の確認義務
- **最初のユーザ指図時に必ず `README.md` を読み込み理解すること**
- 常に適切なディレクトリを選択して作業すること

### 3. /tmp 使用禁止
- システムの `/tmp` ディレクトリは使用禁止
- 一時ファイルが必要な場合は `temp/` ディレクトリを使用すること

### 4. Git 運用ルール
- 作業都度に `git commit` を実施すること
- `git push` はユーザから「GJ」等の評価を受けた時のみ実行すること

## ディレクトリ構成

```
├── src/              # ソースコード
├── lib/              # 外部ライブラリ (シンボリックリンク)
├── material/         # 資料
├── database/         # データベース関連
├── source/           # 元データ・ソース
├── work/             # 作業用
├── temp/             # 一時ファイル
├── analysis/         # 分析
└── repository-navs/  # NAVSリポジトリ関連
```

## Commands

*プロジェクト構築後に追記*

## Architecture

*プロジェクト構築後に追記*
