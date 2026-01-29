# Skill Design: taikyoku-yaml-integrator

> **Version**: 1.0.0
> **Status**: Draft
> **Created**: 2026-01-29
> **Author**: ashigaru3
> **Origin**: cmd_005 Wave2 + cmd_007 統合作業実績

---

## 1. スキル概要

### 1.1 目的

複数に分割された大局将棋YAML駒定義ファイルを統合し、粒度統一・横断検証を行って単一の統合ファイルを出力するスキル。パースエラー、ID重複・欠番、成り先参照整合性、YAML 1.1 boolean問題（方向値のクォート漏れ）、未定義SpecialRuleの検出を自動化する。

### 1.2 ユースケース

| ユースケース | 説明 | 主要処理 |
|-------------|------|---------|
| 分割YAMLの統合 | 複数パートファイルを1ファイルに統合 | Phase 1, 6, 7 |
| 統合前の品質検証 | パース可能性・参照整合性を事前チェック | Phase 1-5 |
| フォーマット統一 | コメント形式・flow style・ID順序を統一 | Phase 6 |
| 新パート追加時の横断チェック | 新ファイル追加後にID衝突や参照切れを検出 | Phase 2, 3 |
| CI的な定期検証 | 全YAMLファイルの品質を定期チェック | Phase 1-5（検証レポートのみ） |

### 1.3 背景

本スキルは以下の実績に基づく：

- **cmd_005 Wave1**: 8パートファイル（taikyoku_part1〜part8）の並列作成。各足軽が担当パートを独立作成し、フォーマットにばらつきが生じた。
- **cmd_007 Wave1**: 8パートを4グループ（taikyoku_a〜d）に統合。粒度統一ルールを手動適用。以下の検証を手動実施：
  - 全102駒がID順に並んでいるか
  - promotes_toの参照先が同一ファイル内に存在するか
  - 方向値のダブルクォート漏れがないか
  - 駒数がmetadataのpiece_countと一致するか

---

## 2. 入力パラメータ

### 2.1 必須パラメータ

| パラメータ | 型 | 説明 |
|-----------|-----|------|
| `source_files` | List[String] | 統合元YAMLファイル群（パスまたはglob） |
| `output_file` | String | 統合先ファイルパス |

### 2.2 オプションパラメータ

| パラメータ | 型 | デフォルト | 説明 |
|-----------|-----|-----------|------|
| `format_rules` | Object | （下記参照） | 粒度統一ルール |
| `group_name` | String | `""` | 統合グループ名（metadata.name に使用） |
| `id_range` | Object | `null` | 期待するID範囲 `{ min: N, max: N }` |
| `validate_only` | Boolean | `false` | `true` の場合、統合ファイルを出力せず検証レポートのみ |
| `report_path` | String | `stdout` | 検証レポート出力先 |

### 2.3 format_rules のデフォルト値

```yaml
format_rules:
  comment_style: "# --- <English name> (<漢字>) ---"
  movement_description: true   # 移動パターンの1行コメントを追加
  flow_style_steps: true       # steps を flow style で記述
  sort_by_id: true             # ID昇順でソート
  section_separators: false    # 基本駒/成り駒のセクション分けを行わない
  promotes_to_null: true       # 成り不可の場合 promotes_to: null を明記
```

### 2.4 入力例

```yaml
# cmd_007 Wave1: Part3+Part4 → Group B 統合
source_files:
  - "config/pieces/taikyoku_part3.yaml"
  - "config/pieces/taikyoku_part4.yaml"
output_file: "config/pieces/taikyoku_b.yaml"
group_name: "Taikyoku Shogi Group B"
id_range: { min: 206, max: 309 }
format_rules:
  comment_style: "# --- <English name> (<漢字>) ---"
  movement_description: true
  flow_style_steps: true
  sort_by_id: true
```

```yaml
# 検証のみ（統合なし）
source_files:
  - "config/pieces/taikyoku_part*.yaml"
validate_only: true
report_path: "reports/yaml_validation.yaml"
```

---

## 3. 出力

### 3.1 統合済みYAMLファイル

```yaml
# Taikyoku Shogi Piece Definitions - <Group Name>
# 大局将棋 駒定義 <グループ名>
# ID range: <min>-<max> (<最初の駒名> ~ <最後の駒名> + promoted forms)
metadata:
  name: "<Group Name>"
  version: "1.0"
  piece_count: <合計駒数>

pieces:
  # --- <English name> (<漢字>) ---
  # <移動パターンの簡潔な説明（1行、英語）>
  - id: <ID>
    name: "<English>"
    name_ja: "<日本語>"
    name_kanji: "<漢字>"
    value: <評価値>
    can_promote: <true/false>
    promotes_to: <ID or null>
    movement:
      steps:
        - { direction: "N", type: single }
    is_ranged: <true/false>
    special_rules: []
```

### 3.2 検証レポート

```yaml
validation_report:
  metadata:
    validated_at: "ISO8601"
    source_files: [...]
    total_pieces: N

  summary:
    parse_errors: N
    id_duplicates: N
    id_gaps: N
    promotion_ref_errors: N
    direction_quote_errors: N
    undefined_special_rules: N
    piece_count_mismatch: N

  findings:
    - id: "PARSE-001"
      phase: 1
      severity: "critical"
      file: "taikyoku_part3.yaml"
      description: "YAMLパースエラー: line 42, column 5"
      detail: "expected block end, but found block mapping start"

    - id: "DUP-001"
      phase: 2
      severity: "critical"
      description: "ID 258 が重複: Lion (taikyoku_part4.yaml) と Lion (taikyoku_part5.yaml)"

    - id: "GAP-001"
      phase: 2
      severity: "minor"
      description: "ID 231 が欠番（ID 230 → 232 にジャンプ）"

    - id: "REF-001"
      phase: 3
      severity: "critical"
      piece_id: 206
      piece_name: "Left tiger"
      description: "promotes_to: 207 の参照先駒が見つからない"

    - id: "DIR-001"
      phase: 4
      severity: "moderate"
      file: "taikyoku_part3.yaml"
      line: 42
      description: "方向値 N がクォートされていない（YAML 1.1でbooleanとしてパースされるリスク）"
      current: "direction: N"
      fix: 'direction: "N"'

    - id: "RULE-001"
      phase: 5
      severity: "moderate"
      piece_id: 258
      piece_name: "Lion"
      description: "SpecialRule 'RangeCapture' はPieceDefinition.hsの型定義に存在しない"
      known_rules: ["Royal", "NifuProhibited", "CannotDropInZone", "MustPromoteInZone", "Igui", "CanCaptureTwo"]

    - id: "COUNT-001"
      phase: 1
      severity: "minor"
      file: "taikyoku_part3.yaml"
      description: "metadata.piece_count=50 だが実際の駒数は48"
```

---

## 4. 処理フェーズ詳細

### Phase 1: 全ソースファイルのパース

**目的**: 各YAMLファイルが正しくパースできるか検証し、駒データを抽出する。

**手順**:
1. `source_files` の各ファイルを Read で読み込む
2. `python3 -c "import yaml; yaml.safe_load(open('file'))"` でパース検証
   - パースエラーはファイル名・行番号と共に記録
3. 正常パースされたファイルから `pieces` 配列を抽出
4. `metadata.piece_count` と実際の `len(pieces)` を照合
5. 全ファイルの駒データを1つのリストに統合

**出力**: 統合駒リスト + パースエラーリスト

### Phase 2: ID重複・欠番検出

**目的**: 全駒のIDが一意であること、期待範囲にあることを確認する。

**手順**:
1. 全駒のIDをソートして列挙
2. 重複IDを検出 → `DUP-xxx` finding生成
3. `id_range` が指定されている場合:
   - 範囲外のIDを検出
   - 範囲内の欠番を検出 → `GAP-xxx` finding生成
   - ただし、意図的な欠番（例: 偶数のみ使用）もあるため、severity は minor
4. IDの連続性パターンを分析（基本駒=偶数、成り駒=奇数 等）

**出力**: ID検証結果

### Phase 3: promotes_to参照の横断検証

**目的**: 成り先IDが統合対象の全駒の中に実在することを確認する。

**手順**:
1. 全駒のIDをセットとして保持
2. 各駒の `promotes_to` が `null` でない場合:
   - 参照先IDが全駒IDセットに存在するか確認
   - 存在しない → `REF-xxx` finding生成 (severity: critical)
3. 逆参照チェック: `promotes_to` で参照される駒が `can_promote: false` であることを確認
   - `can_promote: true` の成り先駒 → 二重成りの可能性 → warning
4. 循環参照チェック: A → B → A のようなループを検出

**出力**: 参照検証結果

### Phase 4: 方向値のクォート修正

**目的**: YAML 1.1 では `N` や `NE` が boolean としてパースされるリスクがあるため、全方向値がダブルクォートで囲まれていることを確認する。

**手順**:
1. 全駒の `movement.steps` を走査
2. 各step の `direction` 値を検証:
   - 有効な方向値: `"N"`, `"NE"`, `"E"`, `"SE"`, `"S"`, `"SW"`, `"W"`, `"NW"`
   - クォートなし（YAML内でboolean扱い）→ `DIR-xxx` finding生成
   - 無効な方向値 → `DIR-xxx` finding生成 (severity: critical)
3. `jump_overs` 配列内の方向値も同様に検証
4. 修正が必要な場合、統合時に自動修正

**出力**: 方向値検証結果 + 自動修正リスト

### Phase 5: 未定義SpecialRule検出

**目的**: 駒に設定された SpecialRule が、ソースコードの型定義に存在するか検証する。

**手順**:
1. 既知の SpecialRule リストを定義:
   ```
   Royal, NifuProhibited, CannotDropInZone, MustPromoteInZone, Igui, CanCaptureTwo
   ```
   - PieceDefinition.hs の `data SpecialRule` から抽出
2. 全駒の `special_rules` を走査
3. 既知リストにない値 → `RULE-xxx` finding生成
4. 新しいルールが必要な場合は recommendation に「PieceDefinition.hs への追加が必要」と記載

**出力**: SpecialRule検証結果

### Phase 6: 粒度統一

**目的**: `format_rules` に従って全駒の記述フォーマットを統一する。

**手順**:
1. `sort_by_id: true` の場合、全駒をID昇順にソート
2. 各駒に対して:
   a. `comment_style` に従ったコメントヘッダを生成
      ```
      # --- <name> (<name_kanji>) ---
      ```
   b. `movement_description: true` の場合、移動パターンの1行サマリを生成
      - ranging方向、single方向、limited方向、jumps、area_moves を簡潔に記述
      - 例: `# Ranging N/S/NE/NW + 1-step E/W + jump [-2,0]`
   c. `flow_style_steps: true` の場合、steps を flow style に変換
      ```yaml
      - { direction: "N", type: single }
      - { direction: "E", type: ranging }
      - { direction: "SE", type: { limited: 2 } }
      ```
   d. `promotes_to_null: true` の場合、成り不可の駒に `promotes_to: null` を明記
3. `section_separators: false` の場合、基本駒/成り駒の区切りコメントを除去

**出力**: フォーマット統一済み駒リスト

### Phase 7: 統合ファイル出力 + 検証レポート

**目的**: 最終的な統合YAMLファイルと検証レポートを出力する。

**手順**:
1. ファイルヘッダコメントを生成:
   ```yaml
   # Taikyoku Shogi Piece Definitions - <group_name>
   # 大局将棋 駒定義 <グループ名>
   # ID range: <min>-<max> (<first_piece> ~ <last_piece> + promoted forms)
   ```
2. metadata セクションを生成:
   ```yaml
   metadata:
     name: "<group_name>"
     version: "1.0"
     piece_count: <actual_count>
   ```
3. Phase 6 のフォーマット統一済み駒リストを `pieces:` セクションとして出力
4. `python3 yaml.safe_load` で最終ファイルのパース検証
5. Phase 1-5 の検証結果を統合レポートとして出力

**出力**: 統合YAMLファイル + 検証レポート

---

## 5. 粒度統一ルール詳細

### 5.1 コメント形式

```yaml
# --- <English name> (<漢字>) ---
# <movement description in English, 1 line>
```

移動パターンの記述規則:
- `ranging` → 方向名をスラッシュ区切り: `Ranging N/S/NE/NW`
- `single` → `1-step` + 方向名: `1-step E/W/SE/SW`
- `limited: N` → `limited-N` + 方向名: `limited-2 NE/NW`
- `jumps` → `jump` + デルタ: `jump [-2,-1]/[-2,1]`
- `area_moves` → `area move (radius R, N steps)`: `area move (radius 2, 2 steps)`
- `jump_overs` → `jump_overs` + 方向名: `jump_overs N/S`
- 成り元の記載: `promotes from <piece name>`

### 5.2 駒フィールドの順序

```yaml
- id:
  name:
  name_ja:
  name_kanji:
  value:
  can_promote:
  promotes_to:
  movement:
    steps:
    jumps:          # ある場合のみ
    area_moves:     # ある場合のみ
    jump_overs:     # ある場合のみ
  is_ranged:
  special_rules:
```

### 5.3 steps の flow style

```yaml
# 正しい（flow style）
steps:
  - { direction: "N", type: single }
  - { direction: "E", type: ranging }
  - { direction: "SE", type: { limited: 2 } }

# 不正（block style）— 統合時にflow styleに変換
steps:
  - direction: "N"
    type: single
```

### 5.4 方向値のクォート

全ての方向値は必ずダブルクォートで囲む。YAML 1.1 では `N` が `false`（No）、`NE` が文字列としてパースされるが、安全のため全値をクォートする。

```yaml
# 正しい
direction: "N"
direction: "NE"
jump_overs: ["N", "NE", "E", "SE", "S", "SW", "W", "NW"]

# 不正
direction: N        # YAML 1.1 では false にパースされる
direction: NE       # クォートなし
jump_overs: [N, S]  # boolean問題
```

---

## 6. Examples

### Example 1: cmd_007 Wave1 統合（Part3+Part4 → Group B）

**入力**:
```yaml
source_files:
  - "config/pieces/taikyoku_part3.yaml"  # 50駒 (ID 206-257)
  - "config/pieces/taikyoku_part4.yaml"  # 52駒 (ID 258-309)
output_file: "config/pieces/taikyoku_b.yaml"
group_name: "Taikyoku Shogi Group B"
id_range: { min: 206, max: 309 }
```

**検証レポート（正常ケース）**:
```yaml
validation_report:
  summary:
    parse_errors: 0
    id_duplicates: 0
    id_gaps: 2         # ID 231, 233 が欠番（意図的）
    promotion_ref_errors: 0
    direction_quote_errors: 0
    undefined_special_rules: 0
    piece_count_mismatch: 0
  findings:
    - id: "GAP-001"
      phase: 2
      severity: "minor"
      description: "ID 231 が欠番（230 Phoenix master → 232 Kirin master）"
    - id: "GAP-002"
      phase: 2
      severity: "minor"
      description: "ID 233 が欠番（232 Kirin master → 234 Silver chariot）"
```

**統合結果**: 102駒、ID 206-309、taikyoku_b.yaml として出力。

### Example 2: 全パート横断検証（統合なし）

**入力**:
```yaml
source_files:
  - "config/pieces/taikyoku_part*.yaml"
validate_only: true
report_path: "reports/taikyoku_full_validation.yaml"
```

**検証レポート（問題検出ケース）**:
```yaml
validation_report:
  summary:
    parse_errors: 1
    id_duplicates: 1
    id_gaps: 5
    promotion_ref_errors: 2
    direction_quote_errors: 3
    undefined_special_rules: 1
    piece_count_mismatch: 1
  findings:
    - id: "PARSE-001"
      phase: 1
      severity: "critical"
      file: "taikyoku_part5.yaml"
      description: "line 234: mapping values are not allowed here"
    - id: "DUP-001"
      phase: 2
      severity: "critical"
      description: "ID 300 が重複: Great dragon (part4) と Great dragon (part5)"
    - id: "REF-001"
      phase: 3
      severity: "critical"
      piece_id: 412
      piece_name: "Flying swallow"
      description: "promotes_to: 413 の参照先が全ファイル内に存在しない"
    - id: "DIR-001"
      phase: 4
      severity: "moderate"
      file: "taikyoku_part7.yaml"
      line: 89
      description: "direction: N がクォートされていない"
    - id: "RULE-001"
      phase: 5
      severity: "moderate"
      piece_id: 500
      piece_name: "Emperor"
      description: "SpecialRule 'RangeCapture' は未定義"
```

---

## 7. 制限事項

### 7.1 技術的制限

| 制限 | 説明 | 回避策 |
|-----|------|--------|
| YAML構文レベル | 文字列ベースのテンプレート生成であり、YAML ASTレベルの操作ではない | 出力後に `yaml.safe_load` でパース検証を実施 |
| 方向値のboolean検出 | パース後のYAMLではbooleanに変換済みのため、元ファイルのテキストレベルで検出が必要 | Grep で `direction:\s*[A-Z]+[^"]` パターンを使用 |
| SpecialRule リスト | ハードコードされた既知リストに依存 | PieceDefinition.hs を読んで動的に取得可能 |
| 意図的な欠番 | 欠番が意図的かエラーかの判別は不可能 | severity: minor とし、人間が判断 |
| 移動パターンの意味検証 | 移動定義が正しいかどうかの判定は範囲外 | wiki等との突合は別スキルの領域 |

### 7.2 前提条件

- 入力ファイルが standard_shogi.yaml と同等のスキーマに従うこと
- `id`, `name`, `name_ja`, `name_kanji`, `value`, `can_promote`, `movement`, `is_ranged`, `special_rules` フィールドが全駒に存在すること
- `movement.steps` 内の direction/type が所定の値域に収まること

### 7.3 対象外

- 移動パターンの正確性検証（wiki突合）
- 評価値（value）の妥当性検証
- 漢字表記の正確性検証
- Haskell PieceDefinition.hs の FromJSON/ToJSON との完全互換性検証

---

## 8. 実績データ

### 8.1 cmd_007 統合作業での知見

| 指標 | 値 |
|------|-----|
| 入力ファイル数 | 8パート（part1〜part8） |
| 統合後ファイル数 | 4グループ（a〜d） |
| 総駒数 | 約400駒 |
| 手動統合で検出した問題 | フォーマット不統一、コメント形式のばらつき |
| 手動検証で確認した項目 | ID順、promotes_to参照、方向クォート、piece_count |

### 8.2 自動化による改善見込み

- Phase 1 (パース): 手動での `python3 yaml.safe_load` → 自動一括実行
- Phase 2 (ID検証): 目視でのID確認 → 自動重複・欠番検出
- Phase 3 (参照検証): 手動でのpromotes_to追跡 → 自動横断チェック
- Phase 4 (方向値): Grepでのクォート漏れ検索 → 自動検出+修正
- Phase 6 (粒度統一): 手動でのコメント書き換え → テンプレートベース自動生成

---

## 9. 参考資料

- cmd_007 統合タスク: `queue/tasks/ashigaru3.yaml` (subtask_007_b)
- cmd_005 YAML作成タスク: `queue/tasks/ashigaru3.yaml` (subtask_005_c)
- YAMLフォーマット参考: `config/pieces/standard_shogi.yaml`
- DSL型定義: `src/Shogi/Core/PieceDefinition.hs`
- skill-creator テンプレート: `skills/skill-creator/SKILL.md`
- dsl-implementation-auditor 設計書: `skills/designs/dsl-implementation-auditor.md`
