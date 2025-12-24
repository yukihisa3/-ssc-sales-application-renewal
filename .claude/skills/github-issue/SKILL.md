---
name: github-issue-workflow
description: GitHub Issue を起点としたワークフローを実行する。Issue の分析、作業計画、実行、レビューを連携して行う際に使用する。
allowed-tools: Bash, Read, Write, Edit, Grep, Glob, TodoWrite
---

# GitHub Issue Workflow Skill

GitHub Issue を起点とした開発ワークフローを支援するスキル。

## 概要

このスキルは以下のエージェントを連携させてワークフローを実行:

1. **issue-analyst** - Issue 分析・アクション検討
2. **task-executor** - 作業計画・実行
3. **code-reviewer** - 結果評価・フィードバック

## 使用方法

### Issue 分析
```
Issue #123 を分析して
```

### 分析→実行
```
Issue #123 を分析して実装して
```

### フルワークフロー
```
Issue #123 を分析→実装→レビューして
```

## GitHub CLI コマンド

### Issue 操作
```bash
# Issue 一覧
gh issue list

# Issue 詳細
gh issue view {number}

# Issue 作成
gh issue create --title "タイトル" --body "本文"

# Issue コメント
gh issue comment {number} --body "コメント"

# Issue クローズ
gh issue close {number}
```

### PR 操作
```bash
# PR 作成
gh pr create --title "タイトル" --body "本文"

# PR 一覧
gh pr list

# PR レビュー
gh pr review {number} --approve
```

## 作業記録

各エージェントの出力は以下に保存:

```
work-records/{user}/
├── issue-{num}-analysis.md   # issue-analyst
├── issue-{num}-execution.md  # task-executor
└── issue-{num}-review.md     # code-reviewer
```

## ワークフロー図

```
┌─────────────────┐
│  GitHub Issue   │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  issue-analyst  │ → issue-{num}-analysis.md
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  task-executor  │ → 実装 + issue-{num}-execution.md
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  code-reviewer  │ → issue-{num}-review.md
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  GitHub Issue   │ ← コメント投稿
└─────────────────┘
```
