---
name: mermaid-flow-generator
description: 任意のMarkdownドキュメントとシナリオから最適なMermaid図を含むMarkdownファイルを自動生成する。業務フロー、API連携シーケンス、状態遷移、プロジェクト計画、データモデル、統計分布、クラス構造など、あらゆるドメインの図表作成に使用。flowchart, sequence, state, gantt, ER, pie, class の7種に対応し、シナリオからの自動選択も可能。
---

# Mermaid Flow Generator

## Overview

任意のMarkdownドキュメントとシナリオ（目的・テーマ）を入力として受け取り、最適なMermaid図を含むMarkdownファイルを自動生成する汎用スキル。GitHub上でのMermaid描画を前提とし、図の前後に補足テキスト（説明、表、凡例等）も生成できる。

対応する図タイプ:
- **flowchart**: 業務フロー、意思決定、インシデント対応、手順書
- **sequence**: API呼び出し、システム間連携、認証フロー
- **state**: 状態遷移、ライフサイクル、ステータス管理
- **gantt**: プロジェクト計画、スケジュール、タイムライン
- **er**: データモデル、テーブル関係、スキーマ設計
- **pie**: 割合、分布、統計データ
- **class**: クラス構造、アーキテクチャ、コンポーネント関係

## When to Use

- Markdownドキュメントの内容をMermaid図で可視化したい
- 業務フローやシステム連携を図表化したい
- 状態遷移やライフサイクルを図にまとめたい
- プロジェクト計画やタイムラインを Gantt チャートで表現したい
- データモデルやER図を作成したい
- 「図を作って」「フロー図を書いて」「可視化して」といった指示を受けた
- 既存のドキュメントから構造を抽出して図にしたい

## Instructions

### Step 1: 入力情報の確認

ユーザーから以下の情報を取得する（または推定する）：

1. **scenario**（必須）: 何を可視化したいか。フロー図の目的・テーマ
2. **output_file**（必須）: 出力先Markdownファイルパス
3. **source_files**（任意）: 図の内容の元となるMarkdownファイル群
4. **diagram_type**（任意、デフォルト: auto）: 図タイプの指定
5. **language**（任意、デフォルト: auto）: 出力言語
6. **include_supplementary_text**（任意、デフォルト: true）: 補足テキストの生成有無

### Step 2: 入力ソースの読み込みと内容把握

1. `source_files` が指定されている場合、各ファイルを Read で読み込む
2. 入力テキストの言語を検出し、`language: auto` の場合は入力に合わせる
3. テキストから以下のキーポイントを抽出する:
   - 登場するエンティティ（人、システム、プロセス、状態等）
   - 関係性やフロー（A→B、AがBに送る、CからDへ遷移する等）
   - 条件分岐（もし〜なら、〜の場合等）
   - 数値データ（割合、件数、期間等）
4. scenario と合わせて、図に含めるべき要素を特定する

### Step 3: 図タイプの選択

`diagram_type: auto` の場合、以下のキーワードマッチングで最適な図タイプを選択する:

| キーワード群 | 図タイプ |
|-------------|---------|
| フロー、手順、プロセス、承認、対応、判断、分岐、ワークフロー、flow, process, workflow, decision | flowchart |
| API、リクエスト、レスポンス、呼び出し、連携、通信、メッセージ、request, response, call, message | sequence |
| 状態、ステータス、遷移、ライフサイクル、ステージ、state, status, transition, lifecycle | state |
| 計画、スケジュール、タイムライン、マイルストーン、期間、schedule, timeline, milestone, sprint | gantt |
| テーブル、エンティティ、リレーション、スキーマ、外部キー、table, entity, relation, schema, FK | er |
| 割合、比率、分布、パーセント、構成比、ratio, percentage, distribution, proportion | pie |
| クラス、継承、インターフェース、コンポーネント、レイヤー、class, inherit, interface, component, layer | class |

scenario と source_files の両方でキーワードをカウントし、最多ヒットの図タイプを選択する。同数の場合は flowchart をデフォルトとする。

明示的に `diagram_type` が指定されている場合はそれを使用する。

### Step 4: Mermaid図の生成

選択された図タイプに応じてMermaidコードを生成する。以下のルールに従う:

**共通ルール:**
- ノードIDは英語大文字スネークケース（`DETECT`, `TRIAGE`, `APPROVE`）
- ラベルは出力言語（`language`）に合わせる
- フェーズ区切りには `%% === フェーズ名 ===` コメントを挿入
- GitHub上でのMermaid描画を前提とし、実験的機能は使わない

**flowchart 固有ルール:**
- 方向: ノード10以下は `LR`、11以上は `TD`
- 開始/終了: `([ラベル])`
- 処理: `[ラベル]`
- 判断: `{ラベル}`
- サブルーチン: `[[ラベル]]`
- 分岐ラベル: `-->|条件| ノード`
- ノード数20超の場合はサブグラフ `subgraph` で分割

**sequence 固有ルール:**
- participant に as で別名を付ける
- 同期: `->>` / `-->>` 、非同期: `-)` / `--)`
- 重要な注記には `Note over` を使用
- ループには `loop` ブロックを使用
- 代替フローには `alt` / `else` ブロックを使用

**state 固有ルール:**
- `stateDiagram-v2` を使用
- 開始: `[*] --> 初期状態`
- 終了: `最終状態 --> [*]`
- 遷移ラベル: `状態A --> 状態B: イベント`
- 複合状態には `state` ブロックを使用

**gantt 固有ルール:**
- `dateFormat YYYY-MM-DD` を明記
- section でフェーズ分割
- タスクIDを付与して依存関係を表現（`after タスクID`）
- マイルストーンには `milestone` を使用

**er 固有ルール:**
- カーディナリティ: `||--o{`（1対多）、`||--||`（1対1）等
- エンティティに属性を含める
- リレーションラベルで関係を説明

**pie 固有ルール:**
- `pie title タイトル` で表題を付ける
- 項目名と数値をペアで記述

**class 固有ルール:**
- アクセス修飾子: `+`（public）、`-`（private）、`#`（protected）
- 継承: `<|--`
- 実装: `<|..`
- 関連: `-->`

### Step 5: 補足テキストの生成

`include_supplementary_text: true` の場合、図タイプに応じた補足を生成する:

| 図タイプ | 生成する補足 |
|---------|------------|
| flowchart | 各フェーズの説明表、判断基準、関連リソースリスト |
| sequence | API仕様サマリ、エラーケース、リトライポリシー |
| state | 状態一覧と説明表、遷移条件、無効遷移の注記 |
| gantt | マイルストーン説明、依存関係、リスク事項 |
| er | エンティティ説明表、カーディナリティ凡例、インデックス推奨 |
| pie | データソース説明、集計条件、注意事項 |
| class | 設計方針、責務説明、拡張ポイント |

### Step 6: Markdownファイル出力

以下の構造で出力ファイルを生成:

```markdown
# {タイトル}

> {概要説明}

---

（目次: セクションが3つ以上ある場合）

## {図セクション見出し}

（補足テキスト: include_supplementary_text=true の場合、図の前の説明）

｀｀｀mermaid
{Mermaidコード}
｀｀｀

---

（補足セクション: include_supplementary_text=true の場合）

## {補足セクション1}
...

## {補足セクション2}
...
```

Write ツールで出力ファイルを生成する。

## Examples

### Example 1: インシデント対応フロー

**入力:**
- scenario: "SFCC連携システムのインシデント対応フロー"
- source_files: ["docs/observations.md"]
- diagram_type: auto → flowchart が選択される
- language: ja

**出力:** flowchart TD 形式の図。検知→トリアージ→観察→分析→システム特定→重大度判定→エスカレーション→対応→復旧→振り返り。補足として影響範囲マトリクス、死活監視、性能監視の表を含む。

### Example 2: CI/CDデプロイフロー

**入力:**
- scenario: "GitHub Actionsを使ったCI/CDパイプライン"
- source_files: []
- diagram_type: flowchart
- language: en

**出力:** flowchart TD 形式の図。PR→Lint→Test→Build→Review→Merge→Staging→E2E→Production。補足としてパイプラインステージ一覧表。

### Example 3: 注文ステータス遷移

**入力:**
- scenario: "ECサイト注文のステータス遷移。作成から完了・キャンセルまで"
- source_files: []
- diagram_type: state
- language: ja

**出力:** stateDiagram-v2 形式の図。注文作成→決済待ち→決済完了→出荷準備→出荷済み→配達完了→完了。キャンセルと返品の分岐を含む。補足として状態一覧と遷移条件の表。

### Example 4: OAuth2.0認証シーケンス

**入力:**
- scenario: "OAuth 2.0 Authorization Code Grant のシーケンス図"
- source_files: []
- diagram_type: sequence
- language: en

**出力:** sequenceDiagram 形式の図。User, Client App, Auth Server, Resource Server の4参加者。認可コード取得→トークン交換→リソースアクセス→トークンリフレッシュの全フロー。

### Example 5: データモデル（ER図）

**入力:**
- scenario: "ECサイトの受注データモデル"
- source_files: ["docs/database_design.md"]
- diagram_type: er
- language: ja

**出力:** erDiagram 形式の図。CUSTOMER, ORDER, LINE_ITEM, PRODUCT, PAYMENT のエンティティと関係。補足としてエンティティ属性一覧表。

## Guidelines

1. **GitHub Mermaid互換性**: GitHub上で描画できるMermaid記法のみ使用する。実験的機能やブラウザ依存の機能は避ける
2. **図の複雑度制御**: ノード数は最大30-40程度に抑える。それ以上は複数の図に分割するか、サブグラフで整理する
3. **ノードID命名**: 英語大文字スネークケース。意味のある名前を使い、STEP1, STEP2 のような連番は避ける
4. **ラベル言語**: ノードIDは常に英語だが、表示ラベルは `language` パラメータに従う
5. **コメント挿入**: フェーズ区切りやセクション境界には `%%` コメントを入れ、コードの可読性を確保する
6. **補足テキストの価値**: 図だけでは伝えきれない情報（具体的な条件値、一覧データ、設定値等）を表やリストで補完する
7. **入力が不十分な場合**: source_files が空で scenario のみの場合は、一般的なベストプラクティスに基づいて図を生成する。ドメイン固有の詳細は含めず、汎用的な構造にする
8. **日本語・英語両対応**: 入力テキストの言語を自動検出し、出力言語をそれに合わせる。`language` が明示されている場合はそちらを優先する
9. **図タイプ自動選択の透明性**: `auto` で図タイプを選択した場合、選択理由を出力ファイル内のコメントまたは概要文で言及する
10. **既存ファイルへの配慮**: output_file が既に存在する場合は上書きする前にユーザーに確認する
