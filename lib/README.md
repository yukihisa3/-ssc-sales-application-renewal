# lib/ - 外部ライブラリ

AI/LLMツール群へのシンボリックリンクを格納するディレクトリ。

## ディレクトリ構成

```
lib/
├── openai-cli      → ../../openai-cli/lib/openai-cli
├── nanobanana-cli  → ../../nanobanana-cli/lib/nanobanana-cli
└── claude-cli      → ../../claude-cli/lib/claude-cli
```

## ツール一覧

| ツール | 説明 |
|--------|------|
| `openai-cli` | OpenAI GPT CLI (Python) - 対話モード、ファイルI/O、セッション管理 |
| `nanobanana-cli` | Google Gemini Pro 画像生成 CLI (Python) |
| `claude-cli` | Claude API CLI (Python) - ファイル入力、Markdown出力、セッション管理 |

## 用途

- レガシーシステム分析・リファクタリング支援
- 第三者評価・コンサルティング
- ドキュメント作成・レビュー
