# src/ - アプリケーションソースコード

新システムのアプリケーションソースコードを格納するディレクトリ。

## ディレクトリ構成

```
src/
├── webapp/              # React フロントエンド
│   ├── components/      # UIコンポーネント
│   │   ├── common/      # 汎用コンポーネント
│   │   └── layout/      # レイアウトコンポーネント
│   ├── features/        # 機能モジュール
│   ├── hooks/           # カスタムフック
│   ├── pages/           # ページコンポーネント
│   ├── routes/          # ルーティング定義
│   ├── services/        # API通信
│   ├── store/           # 状態管理
│   ├── styles/          # CSS/SCSS
│   ├── types/           # TypeScript型定義
│   └── assets/          # 静的リソース (fonts, images)
│
├── api/                 # バックエンドAPI
│   ├── controllers/     # リクエストハンドラ
│   ├── routes/          # ルート定義
│   ├── middleware/      # 認証・ログ・エラーハンドリング
│   ├── services/        # ビジネスロジック
│   ├── repositories/    # データアクセス層
│   └── validators/      # 入力バリデーション
│
├── db-definitions/      # 新システムDB定義
│   ├── schema/          # DDL (テーブル・インデックス・制約)
│   ├── migrations/      # マイグレーションファイル
│   ├── seeds/           # 初期データ
│   │   ├── master/      # マスタデータ
│   │   └── demo/        # デモデータ
│   ├── functions/       # ストアドプロシージャ
│   └── views/           # データベースビュー
│
├── gateway/             # 外部連携 (実装未定)
│   ├── adapters/        # 外部システムアダプタ
│   └── config/          # ゲートウェイ設定
│
├── ops/                 # 運用ツール
│   ├── scripts/         # 運用スクリプト
│   ├── batch/           # バッチ処理
│   └── maintenance/     # メンテナンスツール
│
├── shared/              # 共有モジュール
│   ├── types/           # 共通型定義
│   ├── constants/       # 共通定数
│   └── utils/           # 共通ユーティリティ
│
└── infrastructure/      # Infrastructure as Code
    ├── terraform/       # Terraform (AWS)
    │   ├── environments/  # dev/stg/prod
    │   ├── modules/       # vpc/rds/ecs/lambda/s3
    │   └── variables/
    ├── docker/          # Docker設定
    ├── kubernetes/      # K8sマニフェスト
    ├── cicd/            # CI/CDパイプライン
    └── config/          # 環境設定
```

## 技術スタック

- **Frontend**: React, TypeScript
- **Backend**: Node.js (予定)
- **Database**: PostgreSQL
- **Infrastructure**: AWS (Terraform)
