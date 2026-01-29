# Skill Design: shogi-piece-yaml-generator

> **Version**: 1.0.0
> **Status**: Draft
> **Created**: 2026-01-29
> **Author**: ashigaru1
> **Origin**: ashigaru5,7,8,4 提案 + cmd_005 実績（大局将棋209種404駒YAML定義作成）

---

## 1. スキル概要

### 1.1 目的

Wiki・chessvariants.org等の駒移動パターン記述テキストから、neo_shogi DSLに準拠したYAML駒定義ファイルを自動生成するスキル。手動作成で発生しがちな方向値クォート漏れ、移動パターン誤変換、ID重複等の問題を防止し、品質と効率を両立する。

### 1.2 ユースケース

| ユースケース | 説明 | 主要処理 |
|-------------|------|---------|
| 新変種の駒定義追加 | 中将棋・大局将棋等の大量駒をYAML化 | テキスト→YAML一括変換 |
| 既存駒のバリアント追加 | 成り駒・派生駒の定義追加 | テンプレート適用+ID管理 |
| 移動パターン検証 | Wiki記述とYAML定義の整合性確認 | パース検証 |
| 駒データのフォーマット統一 | PyYAML再保存等で崩れたフォーマットの修復 | 粒度統一ルール適用 |

### 1.3 背景

本スキルは cmd_005（大局将棋駒定義YAML作成）で手動実施した以下の作業を自動化したものである：

- Wiki・chessvariants.orgから209種の駒移動パターンを調査
- 各駒の移動パターンをDSL（single/ranging/limited/jumps/area_moves）に手動変換
- 404エントリ（基本駒+成り駒）のYAMLファイルを8パートに分けて作成
- PyYAML再保存による方向値 `"N"` → `false` のboolean問題を修正
- 未対応SpecialRule（RangeCapture, HookMove等）の検出と除去
- commit 30e5097 + 340eacc として統合（+15024行）

---

## 2. 入力パラメータ

### 2.1 必須パラメータ

| パラメータ | 型 | 説明 |
|-----------|-----|------|
| `source_text` | String | 駒移動パターンのテキスト（Wiki, chessvariants.org等からのコピー） |
| `piece_list` | List[PieceEntry] | 駒名・ID・成り先のリスト |
| `output_file` | String | 出力YAMLファイルパス |

### 2.2 PieceEntry 構造

```yaml
piece_list:
  - id: 100
    name: "King"
    name_ja: "王将"
    name_kanji: "王"
    can_promote: false
    promotes_to: null
```

### 2.3 オプションパラメータ

| パラメータ | 型 | デフォルト | 説明 |
|-----------|-----|-----------|------|
| `yaml_template` | Enum | `neo_shogi` | 出力YAMLテンプレート: `neo_shogi`, `standard_shogi` |
| `value_estimation` | Enum | `auto` | 評価値推定方法: `auto`, `manual`, `relative` |
| `group_name` | String | `null` | メタデータのグループ名（例: "Taikyoku Shogi Group A"） |
| `id_range` | String | `null` | ID範囲の説明文（例: "100-205"） |
| `verify` | Boolean | `true` | 生成後にyaml.safe_loadでパース検証するか |
| `format_style` | Enum | `flow` | steps の書式: `flow`（`{ direction: "N", type: single }`）, `block` |

### 2.4 入力例

```yaml
source_text: |
  King (王将): Moves 1-2 squares in any direction.
  Crown prince (太子): Steps 1 square in any direction. Royal piece.
  Gold general (金将): Moves 1 square to N, NE, E, W, NW, S. Promotes to Rook.

piece_list:
  - { id: 100, name: "King", name_ja: "王将", name_kanji: "王", can_promote: false, promotes_to: null }
  - { id: 102, name: "Crown prince", name_ja: "太子", name_kanji: "太", can_promote: true, promotes_to: 103 }
  - { id: 106, name: "Gold general", name_ja: "金将", name_kanji: "金", can_promote: true, promotes_to: 107 }

output_file: "config/pieces/taikyoku_a.yaml"
group_name: "Taikyoku Shogi Group A"
id_range: "100-205"
```

---

## 3. 出力フォーマット

### 3.1 ファイルヘッダー

```yaml
# Taikyoku Shogi Piece Definitions - {Group Name}
# 大局将棋 駒定義 {Group Name (ja)}
# ID range: {id_range} ({first piece} ~ {last piece} + promoted forms)

metadata:
  name: "{group_name}"
  version: "1.0"
  piece_count: {count}
```

### 3.2 駒定義フォーマット

```yaml
pieces:
  # --- {English name} ({漢字}) ---
  # {移動パターンの簡潔な説明（1行、英語）}
  - id: {ID}
    name: "{English}"
    name_ja: "{日本語}"
    name_kanji: "{漢字}"
    value: {評価値}
    can_promote: {true/false}
    promotes_to: {ID or null}
    movement:
      steps:
        - { direction: "N", type: single }
      jumps:          # オプション
        - [-1, 2]     # (deltaRow, deltaCol)
      area_moves:     # オプション
        - { radius: 2, max_steps: 2, can_return: true }
      jump_overs:     # オプション
        - "N"
    is_ranged: {true/false}
    special_rules: []
```

### 3.3 フォーマットルール

| ルール | 詳細 |
|--------|------|
| 方向値 | 必ずダブルクォート（`"N"`, `"NE"` 等） |
| steps書式 | flow style: `{ direction: "N", type: single }` |
| limited書式 | `{ direction: "N", type: { limited: 2 } }` |
| コメント | 各駒に `# --- Name (漢字) ---` + 移動パターン説明1行 |
| 並び順 | ID昇順（基本駒・成り駒の分離不要） |
| 文字列値 | name, name_ja, name_kanji はダブルクォート |

---

## 4. 処理フェーズ詳細

### Phase 1: 入力テキストから駒名・移動パターンを抽出

source_text から各駒の移動能力を構造化データとして抽出する。

#### 4.1.1 テキスト記述パターン

| 記述形式 | 例 | 抽出方法 |
|---------|-----|---------|
| 方向列挙 | "Moves to N, NE, E, W, NW, S" | 方向名をカンマ分割 |
| Betza記法 | "fRbB" (forward Rook, backward Bishop) | Betza→方向+ステップ型変換 |
| 比喩的表現 | "Moves like a chess Queen" | 既知駒パターンの参照 |
| 制限付き | "Moves up to 3 squares diagonally" | 数値+方向抽出 |
| 範囲移動 | "Ranges in all 8 directions" | ranging 全方向 |
| ジャンプ | "Jumps like a Knight" | jumps 座標リスト |
| 条件付き | "Forward ranging, backward single" | 方向ごとに個別StepType |

#### 4.1.2 Betza記法の変換ルール

| Betza | 意味 | DSL変換 |
|-------|------|---------|
| `R` | Rook（直行走り） | N, E, S, W: ranging |
| `B` | Bishop（斜行走り） | NE, SE, SW, NW: ranging |
| `Q` | Queen（全方向走り） | 全8方向: ranging |
| `K` | King（全方向1歩） | 全8方向: single |
| `W` | Wazir（直行1歩） | N, E, S, W: single |
| `F` | Ferz（斜行1歩） | NE, SE, SW, NW: single |
| `D` | Dabbaba（直行2跳） | jumps: [±2, 0], [0, ±2] |
| `A` | Alfil（斜行2跳） | jumps: [±2, ±2] |
| `N` | Knight（桂馬跳） | jumps: [-2, ±1] (将棋) or [±2, ±1], [±1, ±2] (チェス) |
| `f` | forward のみ | 該当方向のN側のみ適用 |
| `b` | backward のみ | 該当方向のS側のみ適用 |
| `r` | rightward のみ | 該当方向のE側のみ適用 |
| `l` | leftward のみ | 該当方向のW側のみ適用 |
| `s` | sideways のみ | E, W方向のみ適用 |
| `v` | vertical のみ | N, S方向のみ適用 |
| 数字N | limited N | `{ limited: N }` |

#### 4.1.3 方向修飾子の適用マップ

```
f (forward):  N, NE, NW
b (backward): S, SE, SW
r (right):    E, NE, SE
l (left):     W, NW, SW
s (sideways): E, W
v (vertical): N, S
```

### Phase 2: 移動パターンをDSLに変換

抽出した移動情報をneo_shogi DSLの MovementPattern 構造に変換する。

#### 4.2.1 DSL型マッピング

| 移動能力 | DSLフィールド | 型 |
|---------|-------------|-----|
| 方向付き移動 | `steps` | `[{ direction: Direction, type: StepType }]` |
| ジャンプ移動 | `jumps` | `[[deltaRow, deltaCol]]` |
| 範囲移動（獅子型） | `area_moves` | `[{ radius, max_steps, can_return }]` |
| 飛越移動 | `jump_overs` | `[Direction]` |

#### 4.2.2 StepType 判定ルール

| 条件 | StepType |
|------|----------|
| "1 square" / "single step" / Betza修飾なし+W/F/K | `single` |
| "any number" / "ranges" / Betza R/B/Q | `ranging` |
| "up to N squares" / "limited N" / Betza数字 | `{ limited: N }` |

#### 4.2.3 is_ranged 判定

```
is_ranged = true  if any step has type: ranging
                  OR jumps is non-empty
                  OR area_moves is non-empty
                  OR jump_overs is non-empty
is_ranged = false otherwise
```

### Phase 3: 評価値の推定

#### 4.3.1 評価値推定ルール

| 要素 | 評価値 | 説明 |
|------|--------|------|
| 基礎値（single方向数） | 100 × N方向 | 移動方向が多いほど高価値 |
| ranging ボーナス | +200 × N方向 | 走り駒は高価値 |
| limited N ボーナス | +50 × N × 方向数 | 制限距離に比例 |
| jump ボーナス | +100 × Nジャンプ | ジャンプは中程度価値 |
| area_move ボーナス | +300 × radius | 獅子型は高価値 |
| Royal | 10000 (固定) | 王将系 |
| fly-over | +1000 | 飛越能力は非常に高価値 |

#### 4.3.2 既知駒の参照値

| 駒名 | 評価値 | 参考 |
|------|--------|------|
| King (Q2, Royal) | 10000 | 最高値（Royal） |
| Great general (Q ranging) | 5000 | 全方向走り+飛越 |
| Free king (Q ranging) | 2000 | チェスQueen相当 |
| Rook (R ranging) | 1000 | 直行走り |
| Bishop (B ranging) | 900 | 斜行走り |
| Gold general (6dir single) | 600 | 標準金 |
| Silver general (5dir single) | 500 | 標準銀 |
| Lance (fR) | 400 | 前方走りのみ |
| Pawn (fW) | 100 | 前方1歩のみ |

### Phase 4: YAML生成

#### 4.4.1 生成手順

```
1. ファイルヘッダー生成（グループ名・ID範囲・駒数）
2. metadata セクション生成
3. pieces セクション開始
4. 各駒について ID 昇順で:
   a. コメント行生成: # --- {Name} ({Kanji}) ---
   b. 移動パターン説明コメント生成（1行英語）
   c. YAML駒定義生成（flow style steps）
   d. 方向値のダブルクォート確認
5. 末尾の改行
```

#### 4.4.2 方向値クォーティング（最重要）

YAML 1.1仕様では以下がbooleanとして解釈される：

| 値 | YAML 1.1解釈 | 対策 |
|----|-------------|------|
| `N` | `false` (No) | 必ず `"N"` |
| `Y` | `true` (Yes) | 必ず `"Y"` |
| `n` | `false` | 必ず `"n"` |
| `y` | `true` | 必ず `"y"` |
| `NE`, `NW`, `E`, `SE`, `S`, `SW`, `W` | 文字列 | 統一性のため `"NE"` 等クォート |

**ルール**: 全方向値を必ずダブルクォートで囲む。例外なし。

### Phase 5: パース検証

#### 4.5.1 検証項目

| 検証 | 方法 | 期待結果 |
|------|------|---------|
| YAMLパース | `python3 -c "import yaml; yaml.safe_load(open(path))"` | エラーなし |
| 駒数確認 | `len(data['pieces'])` | piece_list の件数と一致 |
| ID重複チェック | IDのset比較 | 重複なし |
| ID順序チェック | IDリストのソート検証 | 昇順 |
| 成り先参照チェック | promotes_to が既存IDを参照 | 全て解決可能 |
| 方向値チェック | 全direction値が有効な8方向 | 不正値なし |
| boolean汚染チェック | `direction: false` 等の検出 | 0件 |

#### 4.5.2 検証スクリプト

```python
import yaml, sys

with open(sys.argv[1]) as f:
    data = yaml.safe_load(f)

pieces = data['pieces']
ids = [p['id'] for p in pieces]

# 基本チェック
assert len(ids) == len(set(ids)), f"ID重複: {len(ids)} != {len(set(ids))}"
assert ids == sorted(ids), "IDが昇順でない"

# 成り先チェック
id_set = set(ids)
for p in pieces:
    if p.get('promotes_to') is not None:
        assert p['promotes_to'] in id_set, f"ID {p['id']} の成り先 {p['promotes_to']} が未定義"

# 方向値チェック
valid_dirs = {"N", "NE", "E", "SE", "S", "SW", "W", "NW"}
for p in pieces:
    for step in p['movement']['steps']:
        d = step['direction']
        assert d in valid_dirs, f"ID {p['id']} に不正方向値: {d}"
        assert isinstance(d, str), f"ID {p['id']} 方向値が文字列でない: {type(d)} ({d})"

print(f"OK: {len(pieces)} pieces verified")
```

---

## 5. 移動パターン変換ルール一覧

### 5.1 基本パターン

| パターン名 | 入力テキスト例 | DSL変換 |
|-----------|---------------|---------|
| 全方向1歩 | "King movement" / K | 全8方向 single |
| 全方向走り | "Queen movement" / Q | 全8方向 ranging |
| 直行走り | "Rook movement" / R | N,E,S,W ranging |
| 斜行走り | "Bishop movement" / B | NE,SE,SW,NW ranging |
| 金将型 | "Gold general" | N,NE,E,W,NW,S single |
| 銀将型 | "Silver general" | N,NE,SE,SW,NW single |
| 前方走り | "Lance" / fR | N ranging |
| 桂馬跳 | "Knight" | jumps: [-2, -1], [-2, 1] |

### 5.2 複合パターン

| パターン名 | 入力テキスト例 | DSL変換 |
|-----------|---------------|---------|
| 前方Queen+後方King | "fQbK" | N,NE,NW: ranging + E,SE,S,SW,W: single |
| 直行走り+斜行制限 | "RB2" | N,E,S,W: ranging + NE,SE,SW,NW: limited 2 |
| 方向別混合 | "N ranging, S limited 3" | 方向ごとに個別StepType |
| 走り+ジャンプ | "B + alfil jumps" | 斜行ranging + jumps: [±2, ±2] |

### 5.3 特殊パターン

| パターン名 | 入力テキスト例 | DSL変換 | 注意 |
|-----------|---------------|---------|------|
| 獅子型 | "Lion" | area_moves: [{radius: 2, max_steps: 2, can_return: true}] | |
| 飛越捕獲 | "Fly-over capture" | `jump_overs` リスト | DSL未完全対応 |
| 鉤行型 | "Hook mover" | ranging のみ（hook動作は未対応） | TODO警告出力 |
| 居喰い | "Igui" | special_rules: [Igui] | |
| 二枚取り | "Can capture two" | special_rules: [CanCaptureTwo] | |

---

## 6. Examples

### 6.1 大局将棋の実例

#### 入力

```
source_text: |
  Running rabbit (走兎): Forward queen (N, NE, NW ranging) plus backward king
  (all other dirs single step). Betza: fQbK

  Fire demon (火鬼): Forward/backward limited 2, sideways and all diagonals
  ranging. Betza: vR2sRB

  Right mountain eagle (右山鷲): All orthogonal ranging, forward diagonals
  ranging, SE ranging, SW limited 2. Plus right alfil jumps (NE, SE at 2
  squares diagonal). Betza: RfBbrBblB2rA
```

#### 生成結果

```yaml
pieces:
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
    special_rules: []

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
    special_rules: []

  # --- Right mountain eagle (右山鷲) ---
  # All orthogonal ranging, forward diag ranging, SE ranging, SW limited 2 + right alfil jumps
  - id: 132
    name: "Right mountain eagle"
    name_ja: "右山鷲"
    name_kanji: "右鷲"
    value: 1300
    can_promote: true
    promotes_to: 133
    movement:
      steps:
        - { direction: "N", type: ranging }
        - { direction: "NE", type: ranging }
        - { direction: "E", type: ranging }
        - { direction: "SE", type: ranging }
        - { direction: "S", type: ranging }
        - { direction: "SW", type: { limited: 2 } }
        - { direction: "W", type: ranging }
        - { direction: "NW", type: ranging }
      jumps:
        - [-2, 2]    # forward-right alfil (NE)
        - [2, 2]     # backward-right alfil (SE)
    is_ranged: true
    special_rules: []
```

### 6.2 標準将棋の実例

#### 入力

```
source_text: |
  Pawn (歩兵): Moves 1 square forward. Cannot have two in same column (Nifu).
  Cannot drop in last row.

  Knight (桂馬): Jumps to 2 forward + 1 sideways (like shogi Knight, forward only).
  Cannot drop in last 2 rows.
```

#### 生成結果

```yaml
pieces:
  # --- Pawn (歩) ---
  # Moves 1 square forward (fW in Betza notation)
  - id: 1
    name: "Pawn"
    name_ja: "歩兵"
    name_kanji: "歩"
    value: 100
    can_promote: true
    promotes_to: 2
    movement:
      steps:
        - { direction: "N", type: single }
    is_ranged: false
    special_rules:
      - NifuProhibited
      - CannotDropInZone 1

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

---

## 7. 制限事項

### 7.1 DSL未対応の移動パターン

| パターン | 説明 | 現状の対処 |
|---------|------|-----------|
| 鉤行 (Hook move) | 走り→90度転回→再走り | ranging のみ出力 + TODOコメント |
| 飛越捕獲 (Fly-over capture) | 経路上の全駒を捕獲 | ranging のみ出力 + TODOコメント |
| ジャンプ後走り (Jump+follow-up) | ジャンプ→着地点から走り | jumps のみ出力 + TODOコメント |
| 条件付き移動 (Conditional move) | 初手のみ2歩等 | 最大移動で出力 + コメント |
| 範囲捕獲 (Range capture) | 走りの途中で離れた駒を捕獲 | ranging のみ出力 + TODOコメント |

### 7.2 対応SpecialRule一覧

| SpecialRule | 対応状況 |
|-------------|---------|
| `Royal` | 対応 |
| `NifuProhibited` | 対応 |
| `CannotDropInZone N` | 対応 |
| `MustPromoteInZone N` | 対応 |
| `Igui` | 対応 |
| `CanCaptureTwo` | 対応 |

上記以外のSpecialRule名が入力された場合、警告を出力し除外する。

### 7.3 テキスト解析の限界

- 自然言語記述の曖昧性により、完全自動変換は保証しない
- Betza記法が提供されている場合の精度が最も高い
- 未知の記法やローカルルールは手動確認が必要
- 生成後の人間レビューを推奨

### 7.4 YAML出力の注意

- PyYAMLで再保存すると方向値のクォートが外れる（YAML 1.1 boolean問題）
- 生成ファイルをPyYAMLで再保存する場合は、保存後に方向値クォートの再確認が必要
- ruamel.yaml（YAML 1.2準拠）の使用を推奨

---

## 8. 実績データ（cmd_005 からの知見）

### 8.1 作業結果サマリ

| 指標 | 値 |
|------|-----|
| 対象 | 大局将棋（36×36盤、209駒種） |
| 生成エントリ数 | 404（基本駒+成り駒） |
| 分割数 | 8パート → 4ファイルに統合（cmd_007） |
| 総行数 | +15,024行 |
| 投入足軽数 | 8名（並列作業） |
| コミット | 30e5097 + 340eacc |

### 8.2 発生した問題と対策

| 問題 | 原因 | 対策 |
|------|------|------|
| `direction: false` | PyYAML再保存でN→false | 全方向値ダブルクォート必須 |
| Unknown SpecialRule | DSL未定義のルール名使用 | 対応ルール一覧との照合 |
| ID重複 | 複数足軽の並列作成で衝突 | ID範囲の事前割当 |
| 移動パターン誤り | Wiki記述の解釈ミス | 複数情報源のクロスチェック |
| フォーマット不統一 | 手動作成とPyYAML出力の混在 | 粒度統一ルール策定（cmd_007） |

---

## 9. 参考資料

- cmd_005 作業レポート: `queue/reports/` 配下の各足軽レポート
- cmd_007 統合作業: `config/pieces/taikyoku_a.yaml` 等
- neo_shogi DSL定義: `src/Shogi/Core/PieceDefinition.hs`
- standard_shogi.yaml: `config/pieces/standard_shogi.yaml`
- SKILL.md テンプレート: `skills/skill-creator/SKILL.md`
