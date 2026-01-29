---
name: shogi-piece-yaml-generator
description: Wiki・chessvariants.org等の駒移動パターン記述からneo_shogi DSL準拠のYAML駒定義を自動生成する。将棋バリアント（大局将棋・中将棋等）の駒データ作成、既存YAMLフォーマット統一、PyYAML再保存後の方向値クォート修復に使用。
---

# Shogi Piece YAML Generator

## Overview

将棋バリアントの駒移動パターン記述テキスト（Wiki・chessvariants.org・Betza記法等）を入力として、neo_shogi DSLに準拠したYAML駒定義ファイルを生成する。

主な機能:
- テキストからの移動パターン抽出（自然言語・Betza記法対応）
- DSL MovementPattern への変換（single/ranging/limited/jumps/area_moves/jump_overs）
- 評価値の自動推定
- フォーマット統一YAML出力（flow style steps、方向値ダブルクォート）
- パース検証（yaml.safe_load + ID重複 + 成り先参照 + 方向値チェック）

## When to Use

- 新しい将棋バリアントの駒定義YAMLを作成する時
- 駒の移動パターンをWikiやWebサイトからYAMLに変換する時
- 既存のYAML駒定義のフォーマットを統一する時（粒度統一）
- PyYAMLで再保存されたファイルの方向値クォートを修復する時
- Betza記法からDSL MovementPattern に変換する時

## Instructions

### Step 1: 入力情報の収集

ユーザーから以下を受け取る:

1. **駒移動パターンのテキスト** — Wiki、chessvariants.org、または手書きの説明
2. **駒リスト** — 各駒のID、英語名、日本語名、漢字、成り先ID
3. **出力先パス** — 生成するYAMLファイルのパス

オプション:
- グループ名（metadata用）
- ID範囲の説明
- 評価値（手動指定 or 自動推定）

### Step 2: 移動パターンの解析

入力テキストから各駒の移動能力を抽出する。以下の順で解析を試みる:

**2a. Betza記法が含まれる場合（最も精度が高い）**

```
Betza基本文字:
  R = 直行走り (N,E,S,W ranging)
  B = 斜行走り (NE,SE,SW,NW ranging)
  Q = 全方向走り (全8方向 ranging)
  K = 全方向1歩 (全8方向 single)
  W = 直行1歩 (N,E,S,W single)
  F = 斜行1歩 (NE,SE,SW,NW single)
  D = 直行2跳 (jumps: [±2,0], [0,±2])
  A = 斜行2跳 (jumps: [±2,±2])
  N = 桂馬跳 (jumps: [±2,±1], [±1,±2])

方向修飾子:
  f = forward:  N, NE, NW
  b = backward: S, SE, SW
  r = right:    E, NE, SE
  l = left:     W, NW, SW
  s = sideways: E, W
  v = vertical: N, S

距離修飾子:
  数字N = limited N (例: R3 = 直行limited 3)
```

変換例:
- `fRbB` → N: ranging, SE: ranging, SW: ranging
- `vR2sR` → N: limited 2, S: limited 2, E: ranging, W: ranging
- `Q2` → 全8方向 limited 2
- `fQbK` → N,NE,NW: ranging + E,SE,S,SW,W: single

**2b. 方向列挙の場合**

"Moves to N, NE, E, W, NW, S" → 列挙された方向をsingle/ranging/limitedに分類

**2c. 比喩的表現の場合**

既知駒パターンを参照テーブルから引く:
- "Like a Rook" → N,E,S,W: ranging
- "Like a Bishop" → NE,SE,SW,NW: ranging
- "Gold general movement" → N,NE,E,W,NW,S: single
- "Silver general movement" → N,NE,SE,SW,NW: single

### Step 3: DSL変換

解析結果をneo_shogi DSL MovementPattern に変換する。

```yaml
movement:
  steps:          # [MovementStep] — 方向付き移動
    - { direction: "N", type: single }
    - { direction: "NE", type: ranging }
    - { direction: "E", type: { limited: 3 } }
  jumps:          # [(Int, Int)] — ジャンプ座標
    - [-2, 1]     # deltaRow, deltaCol
  area_moves:     # [AreaMove] — 獅子型範囲移動
    - { radius: 2, max_steps: 2, can_return: true }
  jump_overs:     # [Direction] — 飛越移動方向
    - "N"
```

**StepType判定:**
- "1 square" / "single" / King/Wazir/Ferz系 → `single`
- "ranges" / "any number" / Rook/Bishop/Queen系 → `ranging`
- "up to N" / "limited N" / 数字修飾 → `{ limited: N }`

**is_ranged判定:**
- steps に `ranging` が1つでもあれば `true`
- jumps / area_moves / jump_overs が空でなければ `true`
- それ以外 `false`

**SpecialRule対応一覧:**
- `Royal` — 王将系（捕獲でゲーム終了）
- `NifuProhibited` — 二歩禁止（歩兵）
- `CannotDropInZone N` — 最終N段への打ち駒禁止
- `MustPromoteInZone N` — 最終N段で成り必須
- `Igui` — 居喰い（移動せず捕獲、獅子）
- `CanCaptureTwo` — 二枚取り（獅子）

未対応SpecialRuleは警告を出力し、YAMLには含めない。

### Step 4: 評価値の推定

手動指定がない場合、以下のヒューリスティックで推定する:

```
base = 0
for each step:
  if single:  base += 100
  if ranging: base += 300
  if limited N: base += 50 * N
for each jump:  base += 100
for each area_move: base += 300 * radius
if Royal: base = 10000
```

参考値:
| 駒 | 値 | 構成 |
|---|---|---|
| Pawn (fW) | 100 | single×1 |
| Lance (fR) | 400 | ranging×1 + 前方のみ補正 |
| Silver (5 single) | 500 | single×5 |
| Gold (6 single) | 600 | single×6 |
| Rook (R) | 1000 | ranging×4 |
| Bishop (B) | 900 | ranging×4 - やや低め |
| Free king (Q) | 2000 | ranging×8 |
| Great general (Q+fly-over) | 5000 | ranging×8 + fly-over |

### Step 5: YAML生成

以下のフォーマットで出力する:

```yaml
# {Title} Piece Definitions - {Group}
# {日本語タイトル} 駒定義 {グループ名}
# ID range: {range} ({first} ~ {last} + promoted forms)

metadata:
  name: "{group_name}"
  version: "1.0"
  piece_count: {count}

pieces:
  # --- {English name} ({漢字}) ---
  # {移動パターン説明（1行英語）}
  - id: {ID}
    name: "{English}"
    name_ja: "{日本語}"
    name_kanji: "{漢字}"
    value: {評価値}
    can_promote: {true/false}
    promotes_to: {ID}  # or omit if null
    movement:
      steps:
        - { direction: "N", type: single }
    is_ranged: {true/false}
    special_rules:
      - Royal
```

**フォーマットルール（厳守）:**

1. **方向値は必ずダブルクォート**: `"N"`, `"NE"`, `"E"`, `"SE"`, `"S"`, `"SW"`, `"W"`, `"NW"`
2. **stepsはflow style**: `- { direction: "N", type: single }`
3. **limitedもflow style**: `- { direction: "N", type: { limited: 2 } }`
4. **各駒にコメント2行**: `# --- Name (漢字) ---` + `# 移動説明`
5. **ID昇順**: 基本駒・成り駒の分離不要
6. **文字列値はダブルクォート**: name, name_ja, name_kanji
7. **special_rules空の場合**: 省略可（出力しない）
8. **promotes_to が null の場合**: フィールド自体を省略

### Step 6: パース検証

生成後に以下を実行:

```bash
python3 -c "
import yaml, sys
with open('OUTPUT_PATH') as f:
    data = yaml.safe_load(f)
pieces = data['pieces']
ids = [p['id'] for p in pieces]
# 駒数
print(f'Pieces: {len(pieces)}')
# ID重複
assert len(ids) == len(set(ids)), 'ID重複あり'
# ID順序
assert ids == sorted(ids), 'ID非昇順'
# 成り先
id_set = set(ids)
for p in pieces:
    pt = p.get('promotes_to')
    if pt is not None:
        assert pt in id_set, f'ID {p[\"id\"]} promotes_to {pt} 未定義'
# 方向値
valid = {'N','NE','E','SE','S','SW','W','NW'}
for p in pieces:
    for s in p['movement']['steps']:
        d = s['direction']
        assert isinstance(d, str) and d in valid, f'ID {p[\"id\"]}: 不正方向値 {d} ({type(d)})'
print('All checks passed')
"
```

**boolean汚染チェック（重要）:**

```bash
grep -n 'direction: false\|direction: true' OUTPUT_PATH
# 出力がなければOK。あればクォート漏れ。
```

## Examples

### Example 1: 大局将棋の3駒

**入力テキスト:**
```
Running rabbit (走兎): fQbK — Forward queen + backward king
Fire demon (火鬼): vR2sRB — Vertical limited 2, sideways + diagonal ranging
Whale (鯨鯢): vRbB — Vertical ranging + backward diagonals ranging
```

**生成YAML:**
```yaml
pieces:
  # --- Fire demon (火鬼) ---
  # Forward/backward limited 2, sideways + diagonals ranging (vR2sRB)
  - id: 136
    name: "Fire demon"
    name_ja: "火鬼"
    name_kanji: "火鬼"
    value: 1400
    can_promote: true
    promotes_to: 137
    movement:
      steps:
        - { direction: "N", type: { limited: 2 } }
        - { direction: "NE", type: ranging }
        - { direction: "E", type: ranging }
        - { direction: "SE", type: ranging }
        - { direction: "S", type: { limited: 2 } }
        - { direction: "SW", type: ranging }
        - { direction: "W", type: ranging }
        - { direction: "NW", type: ranging }
    is_ranged: true

  # --- Whale (鯨鯢) ---
  # Vertical ranging + backward diagonals ranging (vRbB)
  - id: 138
    name: "Whale"
    name_ja: "鯨鯢"
    name_kanji: "鯨"
    value: 800
    can_promote: true
    promotes_to: 139
    movement:
      steps:
        - { direction: "N", type: ranging }
        - { direction: "S", type: ranging }
        - { direction: "SE", type: ranging }
        - { direction: "SW", type: ranging }
    is_ranged: true

  # --- Running rabbit (走兎) ---
  # Forward queen (N, NE, NW ranging) + backward king (all other dirs single)
  - id: 140
    name: "Running rabbit"
    name_ja: "走兎"
    name_kanji: "走兎"
    value: 900
    can_promote: true
    promotes_to: 141
    movement:
      steps:
        - { direction: "N", type: ranging }
        - { direction: "NE", type: ranging }
        - { direction: "E", type: single }
        - { direction: "SE", type: single }
        - { direction: "S", type: single }
        - { direction: "SW", type: single }
        - { direction: "W", type: single }
        - { direction: "NW", type: ranging }
    is_ranged: true
```

### Example 2: ジャンプ駒

**入力テキスト:**
```
Knight (桂馬): Jumps 2 forward + 1 sideways (shogi Knight, forward only).
Special: Cannot drop in last 2 rows.
```

**生成YAML:**
```yaml
pieces:
  # --- Knight (桂) ---
  # Jumps 2 forward + 1 sideways (shogi Knight, forward only)
  - id: 5
    name: "Knight"
    name_ja: "桂馬"
    name_kanji: "桂"
    value: 350
    can_promote: true
    promotes_to: 6
    movement:
      steps: []
      jumps:
        - [-2, -1]   # forward-left
        - [-2, 1]    # forward-right
    is_ranged: false
    special_rules:
      - CannotDropInZone 2
```

### Example 3: DSL未対応パターン

**入力テキスト:**
```
Tengu (天狗): Hook mover on diagonals. Ranges diagonally, then can turn 90
degrees and continue ranging.
```

**生成YAML:**
```yaml
pieces:
  # --- Tengu (天狗) ---
  # Hook mover on diagonals: ranges diagonally, then can turn 90 degrees
  # TODO: Hook move mechanic not yet supported by DSL. Represented as diagonal ranging only.
  - id: 130
    name: "Tengu"
    name_ja: "天狗"
    name_kanji: "天狗"
    value: 1500
    can_promote: false
    movement:
      steps:
        - { direction: "NE", type: ranging }
        - { direction: "SE", type: ranging }
        - { direction: "SW", type: ranging }
        - { direction: "NW", type: ranging }
    is_ranged: true
```

## Guidelines

### 必須ルール

1. **方向値は例外なくダブルクォートで囲む** — YAML 1.1では `N` が `false` (No) として解釈される
2. **steps は flow style で記述** — `{ direction: "N", type: single }` の形式
3. **生成後に必ずパース検証を実行** — yaml.safe_load + ID重複 + 方向値チェック
4. **DSL未対応パターンにはTODOコメントを付与** — 鉤行、飛越捕獲、ジャンプ後走り等
5. **未対応のSpecialRuleはYAMLに含めない** — 警告メッセージのみ出力

### 品質チェックリスト

- [ ] 全方向値がダブルクォートで囲まれているか
- [ ] IDが昇順か
- [ ] ID重複がないか
- [ ] promotes_to が既存IDを参照しているか
- [ ] is_ranged の値が movement 内容と整合しているか
- [ ] 各駒にコメント（`# --- Name (漢字) ---` + 移動説明）があるか
- [ ] special_rules が対応リスト内のもののみか
- [ ] yaml.safe_load でパースが通るか
- [ ] `direction: false` / `direction: true` が含まれていないか

### PyYAML再保存時の注意

PyYAMLで既存ファイルを読み書きすると、方向値 `"N"` が `false` に変換される（YAML 1.1 boolean問題）。対策:

1. **推奨**: ruamel.yaml を使用（YAML 1.2準拠、クォート保持）
2. **代替**: 保存後にgrep検査 + sedで修復
   ```bash
   # 検出
   grep -n 'direction: false\|direction: true' file.yaml
   # 修復
   sed -i 's/direction: false/direction: "N"/g; s/direction: true/direction: "Y"/g' file.yaml
   ```

### 情報源の優先順位

移動パターンの情報源は以下の優先順位で参照:

1. **Betza記法** — 最も明確で機械的に変換可能
2. **chessvariants.org** — 詳細な英語解説
3. **Wikipedia（英語版）** — 一般的な解説
4. **Wikipedia（日本語版）** — 日本語名の確認
5. **既存のYAML定義** — 既に定義済みの類似駒との比較
