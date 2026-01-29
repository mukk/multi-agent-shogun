# Skill Design: piece-movement-visualizer

> **Version**: 1.0.0
> **Status**: Draft
> **Created**: 2026-01-29
> **Author**: ashigaru7
> **Origin**: cmd_008/009/010 実績（standard_shogi_pieces.md 生成）

---

## 1. スキル概要

### 1.1 目的

将棋駒定義YAML（standard_shogi.yaml, taikyoku_*.yaml 等）を入力として、各駒の移動パターンをMarkdownテーブルまたはコードブロックでグリッド視覚化し、人間が直感的に理解できるドキュメントを自動生成するスキル。

### 1.2 ユースケース

| ユースケース | 説明 | 主要出力 |
|-------------|------|---------|
| 標準将棋の駒リファレンス生成 | 8種+6成り駒の移動パターン一覧 | 14駒×5x5グリッド |
| 大局将棋の全駒ドキュメント | 209種の駒の移動パターン全集 | 404駒×5x5/7x7グリッド |
| 駒定義YAML検証の補助 | YAMLの方向定義が意図通りか視覚的に確認 | 個別駒のグリッド |
| ゲーム説明書の自動生成 | 将棋バリアントのルールブック用素材 | TOC付きMarkdown |
| PR レビュー補助 | 駒定義変更の影響を視覚化 | 変更駒のbefore/after |

### 1.3 背景

本スキルは cmd_008/009/010 で手動作成した `standard_shogi_pieces.md` の生成プロセスを自動化したものである：

- `standard_shogi.yaml` から14駒の定義を読み込み
- 各駒の移動パターンを5x5グリッドのMarkdownテーブルに変換
- 目次、凡例、成り対応表を含む完全なドキュメントを生成
- 日英バイリンガル対応

---

## 2. 入力パラメータ

### 2.1 必須パラメータ

| パラメータ | 型 | 説明 |
|-----------|-----|------|
| `yaml_files` | List[String] | 駒定義YAMLファイル群（glob可） |
| `output_file` | String | 出力Markdownファイルパス |

### 2.2 オプションパラメータ

| パラメータ | 型 | デフォルト | 説明 |
|-----------|-----|-----------|------|
| `grid_size` | Int | `5` | グリッドサイズ（3, 5, 7）。走りや限定移動が遠い駒は7推奨 |
| `format` | Enum | `table` | 出力形式: `table`（Markdownテーブル）, `codeblock`（等幅コードブロック） |
| `include_toc` | Boolean | `true` | 目次（Table of Contents）を含むか |
| `language` | Enum | `both` | 表記言語: `ja`（日本語のみ）, `en`（英語のみ）, `both`（日英併記） |
| `group_by` | Enum | `type` | グループ化: `type`（基本駒/成り駒）, `file`（ファイル別）, `none` |
| `include_legend` | Boolean | `true` | 凡例セクションを含むか |
| `include_promotion_table` | Boolean | `true` | 成り対応表を含むか |

### 2.3 入力例

```yaml
# 標準将棋
yaml_files:
  - "config/pieces/standard_shogi.yaml"
output_file: "docs/standard_shogi_pieces.md"
grid_size: 5
format: table
language: both
```

```yaml
# 大局将棋（全駒）
yaml_files:
  - "config/pieces/taikyoku_a.yaml"
  - "config/pieces/taikyoku_b.yaml"
  - "config/pieces/taikyoku_c.yaml"
  - "config/pieces/taikyoku_d.yaml"
output_file: "docs/taikyoku_all_pieces.md"
grid_size: 7
format: table
language: both
include_toc: true
```

---

## 3. 出力フォーマット

### 3.1 全体構造

```markdown
# {タイトル}

> 自動生成ファイル（{source files} より生成）

## 目次 / Table of Contents          ← include_toc: true の場合
### 基本駒 / Base Pieces
- [漢字 名前 / EnglishName (id: N)](#anchor)
### 成り駒 / Promoted Pieces
- ...

## 概要 / Overview
- 基本駒: N種
- 成り駒: N種
- 合計: N種

## 凡例 / Legend                      ← include_legend: true の場合
| 記号 | 意味 |
|------|------|
| ● | 駒の位置 |
| ○ | 1マス移動可 |
| ☆ | 走り（何マスでも） |
| △ | 制限付き走り（N マスまで） |
| ◇ | ジャンプ移動 |
| ◎ | 範囲移動（獅子型） |
| . | 移動不可 |

## 基本駒 / Base Pieces
### {漢字} {名前} / {English} (id: {N})
**評価値 / Value**: {value}
{grid table}
- 一歩 / Step: {directions}
- 走り / Ranging: {directions}
→ **成り / Promotes to**: {name} (id: {N})

## 成り対応表 / Promotion Table       ← include_promotion_table: true の場合
| 元駒 | → | 成り駒 |
```

### 3.2 グリッドフォーマット（table）

5x5 グリッド例（飛車）:

```markdown
| . | . | ☆ | . | . |
|---|---|---|---|---|
| . | . | ☆ | . | . |
| ☆ | ☆ | ● | ☆ | ☆ |
| . | . | ☆ | . | . |
| . | . | ☆ | . | . |
```

7x7 グリッド例（大局将棋の走り駒）:

```markdown
| . | . | . | ☆ | . | . | . |
|---|---|---|---|---|---|---|
| . | . | . | ☆ | . | . | . |
| . | . | . | ☆ | . | . | . |
| ☆ | ☆ | ☆ | ● | ☆ | ☆ | ☆ |
| . | . | . | ☆ | . | . | . |
| . | . | . | ☆ | . | . | . |
| . | . | . | ☆ | . | . | . |
```

### 3.3 グリッドフォーマット（codeblock）

```
. . ☆ . .
. . ☆ . .
☆ ☆ ● ☆ ☆
. . ☆ . .
. . ☆ . .
```

### 3.4 記号体系

| 記号 | YAML対応 | 説明 |
|------|---------|------|
| ● | (center) | 駒の現在位置 |
| ○ | `type: single` | 1マス移動可能 |
| ☆ | `type: ranging` | 走り移動（無制限） |
| △ | `type: { limited: N }` | 制限付き移動（Nマスまで）。数字付き: △2, △3 等 |
| ◇ | `jumps: [[r,c]]` | ジャンプ移動（途中を飛び越え） |
| ◎ | `area_moves` | 範囲移動（獅子型等） |
| . | (empty) | 移動不可 |

---

## 4. 処理フェーズ詳細

### Phase 1: YAML読み込み・パース

```
1. yaml_files の各ファイルを Read で読み込む
2. yaml.safe_load でパース
3. metadata.piece_count と実際の pieces 数を照合（不一致は警告）
4. 全駒を id 順にソートした統合リストを構築
5. 基本駒（can_promote: true）と成り駒（can_promote: false）を分類
```

### Phase 2: 各駒の移動パターン解析

```
1. 各駒の movement フィールドを読み込む
2. steps: 方向(direction) と 種別(type) を解析
   - single → ○ 記号
   - ranging → ☆ 記号
   - { limited: N } → △N 記号
3. jumps: [row, col] デルタを座標に変換 → ◇ 記号
4. area_moves: 範囲を計算 → ◎ 記号
5. jump_overs: 飛び越え方向を記録 → ☆ (走りと同等)
6. special_rules: テキスト説明用に保存
```

### Phase 3: グリッドへのマッピング

方向文字列を座標オフセットに変換する。グリッドの中心が駒位置。

```
方向マッピング（5x5の場合、中心 = (2,2)）:
  "N"  → row: -1, col:  0
  "NE" → row: -1, col: +1
  "E"  → row:  0, col: +1
  "SE" → row: +1, col: +1
  "S"  → row: +1, col:  0
  "SW" → row: +1, col: -1
  "W"  → row:  0, col: -1
  "NW" → row: -1, col: -1
```

#### 3.1 single タイプ

中心から1マスの該当セルに ○ を配置。

#### 3.2 ranging タイプ

中心から該当方向の全セル（グリッド端まで）に ☆ を配置。

#### 3.3 limited タイプ

中心から該当方向にNマス分のセルに △ を配置。
グリッドサイズが足りない場合は、グリッド端まで描画し注釈を追加。

#### 3.4 jumps タイプ

[row_delta, col_delta] を中心座標に加算した位置に ◇ を配置。
グリッド外の場合は注釈を追加。

#### 3.5 area_moves タイプ

中心から radius 以内の全セルに ◎ を配置。

### Phase 4: Markdown生成

```
1. format パラメータに応じてテーブル or コードブロックを生成
2. テーブルの場合:
   - ヘッダ行: | で区切った記号列
   - セパレータ行: |---|
   - 各行: | 記号 | で区切り
3. コードブロックの場合:
   - ``` で囲んだ等幅テキスト
   - 各セル間はスペース区切り
4. グリッドの下に移動パターンテキスト説明を追加
5. 特殊ルールがあれば記載
6. 成り先があれば記載
```

### Phase 5: TOC生成（オプション）

```
1. include_toc: true の場合
2. 全駒のアンカーリンクを生成
3. group_by に応じてセクション分け:
   - type: 基本駒 / 成り駒
   - file: ソースファイル別
   - none: フラットリスト
4. language に応じた表記
```

### Phase 6: ファイル出力

```
1. 全セクションを結合
2. output_file に Write で出力
3. 出力サマリを報告:
   - 総駒数
   - 基本駒数 / 成り駒数
   - ファイルサイズ
```

---

## 5. 使用ツール・手法

| カテゴリ | ツール/手法 | 用途 |
|---------|-----------|------|
| ファイル読み込み | Read | YAMLファイルの読み込み |
| YAMLパース | Python yaml.safe_load (Bash経由) | 駒定義の構造化データ取得 |
| グリッド計算 | 内部ロジック | 方向→座標マッピング |
| Markdown生成 | Write | テーブル/コードブロック生成 |
| 検証 | Bash (Python) | piece_count照合、ID重複チェック |

---

## 6. Examples

### 6.1 標準将棋: 王将（5x5 table）

**入力YAML:**
```yaml
- id: 0
  name: "King"
  name_ja: "王将"
  name_kanji: "王"
  value: 10000
  movement:
    steps:
      - { direction: "N", type: single }
      - { direction: "NE", type: single }
      - { direction: "E", type: single }
      - { direction: "SE", type: single }
      - { direction: "S", type: single }
      - { direction: "SW", type: single }
      - { direction: "W", type: single }
      - { direction: "NW", type: single }
  special_rules: [Royal]
```

**出力Markdown:**
```markdown
### 王 王将 / King (id: 0)

**評価値 / Value**: 10000

| . | . | . | . | . |
|---|---|---|---|---|
| . | ○ | ○ | ○ | . |
| . | ○ | ● | ○ | . |
| . | ○ | ○ | ○ | . |
| . | . | . | . | . |

- 一歩 / Step: N, NE, E, SE, S, SW, W, NW

**特殊ルール / Special Rules:**
- 玉将（取られると負け） / Royal (game loss if captured)
```

### 6.2 大局将棋: 奔鷲（7x7 table）

**入力YAML:**
```yaml
- id: 430
  name: "Free eagle"
  name_ja: "奔鷲"
  name_kanji: "鷲"
  value: 5000
  movement:
    steps:
      - { direction: "N", type: ranging }
      # ... 全8方向 ranging
    area_moves:
      - { radius: 1, max_steps: 2, can_return: true }
    jump_overs: ["N", "NE", "E", "SE", "S", "SW", "W", "NW"]
  special_rules: [Igui]
```

**出力Markdown:**
```markdown
### 鷲 奔鷲 / Free eagle (id: 430)

**評価値 / Value**: 5000

| . | . | . | ☆ | . | . | . |
|---|---|---|---|---|---|---|
| . | . | ☆ | ☆ | ☆ | . | . |
| . | ☆ | ☆ | ☆ | ☆ | ☆ | . |
| ☆ | ☆ | ☆ | ● | ☆ | ☆ | ☆ |
| . | ☆ | ☆ | ☆ | ☆ | ☆ | . |
| . | . | ☆ | ☆ | ☆ | . | . |
| . | . | . | ☆ | . | . | . |

- 走り / Ranging: N, NE, E, SE, S, SW, W, NW
- 範囲移動 / Area: radius=1, max_steps=2, can_return=true
- 飛越走り / Jump over: N, NE, E, SE, S, SW, W, NW

**特殊ルール / Special Rules:**
- 居食い / Igui (capture without moving)
```

### 6.3 大局将棋: 車兵（7x7 table、制限付き移動）

**入力YAML:**
```yaml
- id: 434
  name: "Chariot soldier"
  name_ja: "車兵"
  name_kanji: "車"
  value: 1300
  movement:
    steps:
      - { direction: "N", type: ranging }
      - { direction: "NE", type: ranging }
      - { direction: "SE", type: ranging }
      - { direction: "S", type: ranging }
      - { direction: "SW", type: ranging }
      - { direction: "NW", type: ranging }
      - { direction: "E", type: { limited: 2 } }
      - { direction: "W", type: { limited: 2 } }
```

**出力Markdown:**
```markdown
### 車 車兵 / Chariot soldier (id: 434)

**評価値 / Value**: 1300

| . | . | ☆ | ☆ | ☆ | . | . |
|---|---|---|---|---|---|---|
| . | ☆ | . | ☆ | . | ☆ | . |
| ☆ | . | ☆ | ☆ | ☆ | . | ☆ |
| . | . | △ | ● | △ | . | . |
| ☆ | . | ☆ | ☆ | ☆ | . | ☆ |
| . | ☆ | . | ☆ | . | ☆ | . |
| . | . | ☆ | ☆ | ☆ | . | . |

- 走り / Ranging: N, NE, SE, S, SW, NW
- 制限移動 / Limited(2): E, W
```

---

## 7. 制限事項

### 7.1 技術的制限

| 制限 | 説明 | 回避策 |
|-----|------|--------|
| グリッドサイズ | 3/5/7 のみ対応。大きい限定移動はグリッドに収まらない場合がある | 注釈テキストで補完 |
| 複合移動の表現 | 獅子型の2手移動やhook moveの正確な視覚化は困難 | テキスト説明で補完 |
| 記号の優先度 | 同一セルに複数の移動タイプが重なる場合、優先度で1記号のみ表示 | 走り > 制限 > 単歩 > ジャンプ の優先度 |
| プレイヤー視点 | グリッドは常に先手(Sente)視点。N=上方向 | 後手視点は手動で回転 |
| jump座標 | jumps のデルタが大きい場合（桂馬の[-2,1]等）、5x5では端に寄る | grid_size: 7 を使用 |

### 7.2 前提条件

- 入力YAMLが本プロジェクトの駒定義フォーマットに準拠していること
- `movement.steps` の `direction` が "N","NE","E","SE","S","SW","W","NW" のいずれかであること
- `movement.steps` の `type` が `single`, `ranging`, `{ limited: N }` のいずれかであること

### 7.3 記号優先度

同一セルに複数の移動タイプが重なった場合の表示優先度:

1. ● (駒位置) - 最優先
2. ◎ (範囲移動)
3. ☆ (走り)
4. △ (制限付き)
5. ○ (1マス)
6. ◇ (ジャンプ)
7. . (移動不可) - 最低

---

## 8. 実績データ（cmd_008/009/010 からの知見）

### 8.1 生成結果サマリ

| 指標 | 値 |
|------|-----|
| ソースファイル | standard_shogi.yaml (1ファイル) |
| 出力ファイル | docs/standard_shogi_pieces.md |
| 総駒数 | 14（基本8 + 成り6） |
| グリッドサイズ | 5x5 |
| 言語 | 日英併記 (both) |
| TOC | あり |
| 凡例 | あり |
| 成り対応表 | あり |

### 8.2 手動作成での知見

- 5x5グリッドは標準将棋には十分だが、大局将棋の走り駒や限定3マス移動には7x7が必要
- Markdownテーブルは GitHub/VSCode で正しくレンダリングされることを確認済み
- 日英併記の場合、セクション見出しは「日本語 / English」の形式が読みやすい
- 成り対応表があると、駒間の関係が一目で分かり便利

---

## 9. 将来の拡張案

| 優先度 | 拡張案 | 説明 |
|--------|--------|------|
| high | diff モード | 2つのYAMLを比較し変更された駒のみ表示 |
| high | HTML出力 | カラー付きHTML表による高品質視覚化 |
| medium | SVG出力 | 将棋盤風のSVG画像生成 |
| medium | 双方向対応 | 先手/後手両視点のグリッド生成 |
| low | インタラクティブ | Mermaid/HTML+JS によるインタラクティブ表示 |
| low | バリアント比較 | 標準将棋 vs 中将棋 vs 大局将棋の同名駒比較 |

---

## 10. 参考資料

- cmd_008/009/010 生成物: `docs/standard_shogi_pieces.md`
- 駒定義フォーマット: `docs/designs/neo_shogi_piece_definition_design.md`
- 移動DSL設計: `docs/designs/neo_shogi_move_dsl_design.md`
- SKILL.md テンプレート: `skills/skill-creator/SKILL.md`
