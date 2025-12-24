# work-records/ - 共有セッション記録

ユーザ/LLMエージェントの作業セッション記録を格納するディレクトリ。

## ディレクトリ構成

```
work-records/
├── README.md
└── {user}/                          # ユーザ別ディレクトリ
    └── session-YYYYMMDD-{task}.md   # セッション記録

# 例:
work-records/
├── README.md
├── claude-agent/
│   └── session-20251225-agent-design.md
└── yukihisa/
    └── session-20260401-legacy-analysis.md
```

## 命名規則

- ファイル名: `session-YYYYMMDD-{task}.md`
- 例: `session-20260401-legacy-analysis.md`

## 記録内容 (推奨)

```markdown
# Session: {task}
Date: YYYY-MM-DD
User: {user}

## 目的
-

## 参照ファイル
-

## 作業内容
-

## 成果物
-

## 次のアクション
-
```

## work/ との違い

| ディレクトリ | Git管理 | 用途 |
|-------------|---------|------|
| `work/` | 対象外 | 個人ユーザ/LLMエージェント一時作業 (共有不要) |
| `work-records/` | 対象 | 共有すべきセッション記録 |
