# Session: Multi-Agent System Design
Date: 2025-12-25
User: claude-agent

## 目的
GitHub Issue を参照し、課題分析→作業計画→実行→評価を行うマルチエージェントシステムの設計

## 調査結果

Claude Code で利用可能な機能:
1. **Subagents** - 専門特化したエージェント (`.claude/agents/`)
2. **Skills** - 自動呼び出し可能なスキル (`.claude/skills/`)
3. **Hooks** - イベント駆動のシェルコマンド

## 提案アーキテクチャ

### Agent 1: Issue Analyst (issue-analyst)
```
役割: GitHub Issue を分析し、アクション案を提案
ツール: Bash (gh), Read, Grep, Glob
入力: Issue番号
出力: 分析レポート (work-records/)
```

### Agent 2: Task Planner/Executor (task-executor)
```
役割: 分析結果に基づき作業計画・TODO定義・実行
ツール: Read, Edit, Write, Bash, Glob, Grep
入力: 分析レポート
出力: 実装結果
```

### Agent 3: Reviewer (code-reviewer)
```
役割: 作業結果を評価しフィードバック提供
ツール: Read, Grep, Glob, Bash
入力: 実装結果
出力: 評価レポート
```

## ファイル構成

```
.claude/
├── agents/
│   ├── issue-analyst.md
│   ├── task-executor.md
│   └── code-reviewer.md
├── skills/
│   └── github-issue/
│       └── SKILL.md
└── settings.json (hooks)
```

## データフロー

```
GitHub Issue
    ↓
[issue-analyst] → work-records/{user}/issue-{num}-analysis.md
    ↓
[task-executor] → 実装 + work-records/{user}/issue-{num}-execution.md
    ↓
[code-reviewer] → work-records/{user}/issue-{num}-review.md
    ↓
GitHub Issue (コメント)
```

## 次のアクション
1. .claude/agents/ ディレクトリ作成
2. 各エージェント定義ファイル作成
3. GitHub Issue 連携スキル作成
4. 動作テスト
