# playwright-e2e-scaffold 設計書

## 概要
React + Vite (TypeScript) プロジェクト向けに Playwright E2E テストの雛形一式を生成するスキル。playwright.config.ts、APIモックヘルパー、汎用テストテンプレート（ナビゲーション、フォーム操作、設定パネル、CRUD操作等）を出力する。バックエンド不要でフロントエンド単独テストが可能な設計。

## ユースケース
- 新規 React/Vite プロジェクトに E2E テスト環境を一括構築
- API モック付きテストでバックエンド無しのフロントエンド検証
- CI/CD パイプラインで自動実行可能な E2E テストの整備
- 既存プロジェクトに Playwright を後付け導入する際のベストプラクティス適用

## 提案者
足軽4号

## 提案理由
neo_shogi プロジェクト（cmd_103）で Playwright E2E テストを実装した際、以下が汎用テンプレートになると判断:
- playwright.config.ts（webServer 連携、headless 設定）
- e2e/helpers/mock-api.ts（page.route による API モックパターン）
- テストファイル群（ナビゲーション遷移、設定パネル操作、データ表示確認等）

## 承認
- 承認者: 殿（上様）
- 承認日: 2026-02-10
- cmd_107 にて承認

## 生成物
1. `playwright.config.ts` — Playwright 設定（testDir, webServer, headless, screenshot）
2. `e2e/helpers/mock-api.ts` — API モックヘルパー（setupMock 関数群）
3. `e2e/navigation.spec.ts` — ナビゲーション遷移テスト
4. `e2e/settings.spec.ts` — 設定パネル開閉・値変更テスト
5. `e2e/crud.spec.ts` — CRUD 操作テスト（一覧・作成・詳細）
6. `package.json` への `test:e2e` スクリプト追加手順

## 入力パラメータ
| パラメータ | 説明 | デフォルト |
|-----------|------|-----------|
| baseURL | dev server URL | http://localhost:5173 |
| devCommand | dev server 起動コマンド | npm run dev |
| apiPrefix | API エンドポイントプレフィックス | /api |
| testDir | テストディレクトリ | ./e2e |

## 関連スキル
- vite-react-ts-scaffolder: Vite + React + TS の初期構築
- react-settings-panel-generator: 設定パネル UI 生成
- haskell-servant-tvar-api: バックエンド API 構築
