---
name: piece-movement-visualizer
description: 将棋駒定義YAMLから移動パターン視覚化Markdownを自動生成する。5x5/7x7グリッドにMarkdownテーブルまたはコードブロックで駒の移動範囲を描画。標準将棋・中将棋・大局将棋など任意の将棋バリアントに対応。駒ドキュメント生成、YAML検証補助、ルールブック素材作成に使用。
---

# Piece Movement Visualizer

## Overview

将棋駒定義YAML（`standard_shogi.yaml`, `taikyoku_*.yaml` 等）を読み込み、各駒の移動パターンをグリッド上に記号で視覚化したMarkdownドキュメントを生成する。

出力には目次、凡例、各駒のグリッド図、移動パターンのテキスト説明、成り対応表を含む。

## When to Use

- 駒定義YAMLから移動パターンのドキュメントを生成したい
- YAMLの方向定義が正しいか視覚的に確認したい
- 将棋バリアントのルールブックや説明書を作成したい
- 駒定義の変更前後で移動パターンを比較したい
- `standard_shogi.yaml`, `taikyoku_*.yaml` 等のファイルがある状況

## Instructions

### Step 0: パラメータ確認

ユーザーから以下を確認（または推定）する：

| パラメータ | 必須 | デフォルト | 説明 |
|-----------|------|-----------|------|
| `yaml_files` | Yes | - | 駒定義YAMLファイル群 |
| `output_file` | Yes | - | 出力Markdownパス |
| `grid_size` | No | 5 | グリッドサイズ（3, 5, 7） |
| `format` | No | table | `table` or `codeblock` |
| `include_toc` | No | true | 目次を含むか |
| `language` | No | both | `ja`, `en`, `both` |
| `group_by` | No | type | `type`, `file`, `none` |
| `include_legend` | No | true | 凡例を含むか |
| `include_promotion_table` | No | true | 成り対応表を含むか |

### Step 1: YAML読み込み・パース

1. `yaml_files` の各ファイルを Read で読み込む
2. Bash で Python yaml.safe_load を使ってパースし、構造を確認:
   ```bash
   python3 -c "
   import yaml
   with open('path/to/file.yaml') as f:
       data = yaml.safe_load(f)
   print(f'piece_count: {data[\"metadata\"][\"piece_count\"]}')
   print(f'actual: {len(data[\"pieces\"])}')
   "
   ```
3. `metadata.piece_count` と実際の `pieces` 配列長を照合。不一致は警告
4. 全ファイルの駒を ID 昇順で統合リストにする
5. `can_promote: true` → 基本駒、`can_promote: false` → 成り駒 として分類

### Step 2: 移動パターン解析

各駒の `movement` フィールドを解析し、内部表現に変換する。

#### 方向→座標オフセットマッピング

```
"N"  → (row: -1, col:  0)
"NE" → (row: -1, col: +1)
"E"  → (row:  0, col: +1)
"SE" → (row: +1, col: +1)
"S"  → (row: +1, col:  0)
"SW" → (row: +1, col: -1)
"W"  → (row:  0, col: -1)
"NW" → (row: -1, col: -1)
```

#### 移動タイプ→記号マッピング

| YAML type | 記号 | グリッド配置 |
|-----------|------|------------|
| `single` | ○ | 中心から1マスの該当セル |
| `ranging` | ☆ | 中心から該当方向の全セル（端まで） |
| `{ limited: N }` | △ | 中心から該当方向にNマス分 |
| `jumps: [[r,c]]` | ◇ | 中心 + [r,c] の位置 |
| `area_moves` | ◎ | 中心から radius 以内の全セル |
| `jump_overs` | ☆ | 走りと同等（飛び越え後も走る） |

#### 記号優先度（同一セルに複数の場合）

1. ● (駒位置) - 最優先
2. ◎ (範囲移動)
3. ☆ (走り / 飛越走り)
4. △ (制限付き)
5. ○ (1マス)
6. ◇ (ジャンプ)
7. . (移動不可) - 最低

### Step 3: グリッド生成

grid_size に応じたグリッドを構築する。中心座標は `(grid_size // 2, grid_size // 2)`。

```
grid_size=5 → 中心=(2,2)、範囲: row 0-4, col 0-4
grid_size=7 → 中心=(3,3)、範囲: row 0-6, col 0-6
```

1. grid_size × grid_size の2D配列を "." で初期化
2. 中心に "●" を配置
3. Step 2 の解析結果に基づき各セルに記号を配置（優先度を考慮）
4. ranging/limited がグリッド外に延びる場合は注釈フラグを立てる

### Step 4: Markdown生成

#### format: table の場合

```markdown
| . | . | ☆ | . | . |
|---|---|---|---|---|
| . | . | ☆ | . | . |
| ☆ | ☆ | ● | ☆ | ☆ |
| . | . | ☆ | . | . |
| . | . | ☆ | . | . |
```

- 1行目はヘッダ行（パイプ区切り）
- 2行目はセパレータ（`|---|` の繰り返し）
- 3行目以降はデータ行

#### format: codeblock の場合

````markdown
```
. . ☆ . .
. . ☆ . .
☆ ☆ ● ☆ ☆
. . ☆ . .
. . ☆ . .
```
````

#### 駒セクションのテンプレート

```markdown
### {kanji} {name_ja} / {name} (id: {id})

**評価値 / Value**: {value}

{grid}

{movement description lines}

{promotion info if applicable}
```

移動パターンテキスト行:
- `- 一歩 / Step: N, NE, E` (single)
- `- 走り / Ranging: N, E, S, W` (ranging)
- `- 制限移動 / Limited(N): E, W` (limited)
- `- 跳躍 / Jump: [-2,1], [-2,-1]` (jumps)
- `- 範囲移動 / Area: radius=R, max_steps=M` (area_moves)
- `- 飛越走り / Jump over: N, E` (jump_overs)

成り情報:
- `→ **成り / Promotes to**: {name_ja} / {name} (id: {id})`

特殊ルール:
```markdown
**特殊ルール / Special Rules:**
- 玉将（取られると負け） / Royal
- 居食い / Igui
- 2枚取り / CanCaptureTwo
```

### Step 5: TOC・凡例・成り対応表の生成

#### TOC（include_toc: true の場合）

```markdown
## 目次 / Table of Contents

### 基本駒 / Base Pieces
- [王 王将 / King (id: 0)](#王-王将--king-id-0)
...

### 成り駒 / Promoted Pieces
- [龍 龍王 / DragonKing (id: 12)](#龍-龍王--dragonking-id-12)
...
```

アンカー生成: 漢字・記号はそのまま、スペースは `-`、`/` は空文字に。

#### 凡例（include_legend: true の場合）

```markdown
## 凡例 / Legend

| 記号 | 意味 |
|------|------|
| ● | 駒の位置（中央） / Piece position (center) |
| ○ | 1マス移動可 / Can move one square |
| ☆ | 走り（何マスでも移動可） / Ranging (unlimited) |
| △ | 制限移動（Nマスまで） / Limited range (up to N) |
| ◇ | ジャンプ / Jump (leaps over) |
| ◎ | 範囲移動 / Area move |
| . | 移動不可 / Cannot move |
```

#### 成り対応表（include_promotion_table: true の場合）

```markdown
## 成り対応表 / Promotion Table

| ID | 元駒 | → | 成り駒 | ID |
|----|------|---|--------|-----|
| 2 | 銀将 SilverGeneral | → | 成銀 PromotedSilver | 8 |
...
```

### Step 6: 統合・出力

1. 全セクションを以下の順序で結合:
   - タイトル + 自動生成注記
   - TOC（オプション）
   - 概要
   - 凡例（オプション）
   - 区切り線
   - 基本駒セクション
   - 成り駒セクション
   - 成り対応表（オプション）
2. `output_file` に Write で出力
3. 出力結果を報告:
   ```
   生成完了:
   - 出力: {output_file}
   - 基本駒: N種
   - 成り駒: N種
   - 合計: N種
   - グリッドサイズ: {grid_size}x{grid_size}
   ```

## Examples

### Example 1: 標準将棋（全14駒）

**入力:**
```
yaml_files: ["config/pieces/standard_shogi.yaml"]
output_file: "docs/standard_shogi_pieces.md"
grid_size: 5
format: table
language: both
```

**出力（飛車の部分）:**

```markdown
### 飛 飛車 / Rook (id: 6)

**評価値 / Value**: 1000

| . | . | ☆ | . | . |
|---|---|---|---|---|
| . | . | ☆ | . | . |
| ☆ | ☆ | ● | ☆ | ☆ |
| . | . | ☆ | . | . |
| . | . | ☆ | . | . |

- 走り / Ranging: N, E, S, W

→ **成り / Promotes to**: 龍王 / DragonKing (id: 12)
```

### Example 2: 大局将棋の獅鷹（7x7グリッド）

**入力:**
```
yaml_files: ["config/pieces/taikyoku_d.yaml"]
output_file: "docs/taikyoku_d_pieces.md"
grid_size: 7
format: table
language: both
```

**出力（獅鷹の部分）:**

```markdown
### 獅 獅鷹 / Lion hawk (id: 432)

**評価値 / Value**: 3500

| . | . | . | . | . | . | . |
|---|---|---|---|---|---|---|
| . | ☆ | . | . | . | ☆ | . |
| . | . | ◎ | ◎ | ◎ | . | . |
| . | . | ◎ | ● | ◎ | . | . |
| . | . | ◎ | ◎ | ◎ | . | . |
| . | ☆ | . | . | . | ☆ | . |
| ☆ | . | . | . | . | . | ☆ |

- 走り / Ranging: NE, SE, SW, NW
- 一歩 / Step: N, E, S, W
- 範囲移動 / Area: radius=2, max_steps=2, can_return=true

**特殊ルール / Special Rules:**
- 居食い / Igui (capture without moving)
- 2枚取り / CanCaptureTwo
```

## Guidelines

1. **YAMLフォーマット準拠**: 入力YAMLは本プロジェクトの駒定義フォーマット（`metadata` + `pieces` 配列）に従うこと
2. **方向値の検証**: `direction` が8方向（N/NE/E/SE/S/SW/W/NW）以外の場合は警告を出力
3. **グリッドサイズの選択**: 標準将棋は5、大局将棋の限定移動駒（limited: 3以上）は7を推奨
4. **記号の一貫性**: プロジェクト内の既存ドキュメント（standard_shogi_pieces.md）と同一の記号体系を使用
5. **language対応**: `both` の場合は「日本語 / English」の形式で併記。`ja` のみの場合は日本語、`en` のみの場合は英語
6. **大量駒の扱い**: 大局将棋（404駒）の場合、ファイルサイズが大きくなる。group_by: file で分割出力も検討
7. **先手視点固定**: グリッドは常に先手(Sente)の視点。N（前）= 上方向、S（後）= 下方向
8. **エッジケース**: `movement` フィールドが空の駒（成り駒で金将動きのみ等）は、テキスト注記で「金将と同じ動き」と記載
