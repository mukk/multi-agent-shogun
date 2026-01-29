---
name: taikyoku-yaml-validator
description: 大局将棋YAMLファイルの構造検証を行う。YAMLパース、metadata整合性、ID重複・欠番、成り先参照整合性、方向値のYAML 1.1 boolean問題検出、必須フィールド存在確認、ファイル横断ID重複チェックの7フェーズ検証。将棋駒定義YAMLの品質保証に使用。
---

# Taikyoku YAML Validator

## Overview

大局将棋（Taikyoku Shogi）の駒定義YAMLファイルを7フェーズで構造検証し、問題を構造化レポートとして出力する：

- **Phase 1**: YAMLパース（yaml.safe_load による構文検証）
- **Phase 2**: metadata検証（piece_count 一致確認）
- **Phase 3**: ID検証（重複・ソート順・欠番・範囲チェック）
- **Phase 4**: promotes_to参照検証（存在確認・循環参照検出）
- **Phase 5**: 方向値型検証（YAML 1.1 boolean変換の検出）
- **Phase 6**: 必須フィールド検証（name, movement, value 等の存在確認）
- **Phase 7**: 横断検証（複数ファイル間のID重複・成り先参照）

## When to Use

- YAMLに新しい駒定義を追加した後、整合性を確認したい
- 複数パートファイルを統合した後のデータ完全性を検証したい
- PyYAML で再保存されたファイルの方向値が `"N"` → `false` に変換されていないか確認したい
- 全駒定義ファイルを一括検証し、品質保証レポートを出力したい
- PRレビューで駒定義変更がデータ整合性を壊していないか確認したい

## Instructions

### 必要な入力情報の確認

ユーザーから以下の情報を取得する（または推定する）：

1. **yaml_files**: 検証対象のYAMLファイルパス群
2. **cross_validate**: ファイル横断検証するか（デフォルト: true）

### Phase 1: YAMLパース検証

1. 各YAMLファイルを Bash で `python3 -c "import yaml; yaml.safe_load(open('FILE'))"` を実行
2. パースエラーがあれば行番号・カラム・エラー内容を記録
3. パースに成功したファイルのみ Phase 2 以降に進む

```python
import yaml
for f in yaml_files:
    try:
        data = yaml.safe_load(open(f))
        if data is None:
            report_error(f, "empty file")
        elif "pieces" not in data:
            report_error(f, "missing 'pieces' key")
    except yaml.YAMLError as e:
        report_error(f, str(e))
```

### Phase 2: metadata検証

1. `metadata.piece_count` が存在するか確認
2. 実際の `len(data["pieces"])` と比較
3. 不一致の場合は error を記録

```python
meta = data.get("metadata", {})
actual_count = len(data["pieces"])
expected_count = meta.get("piece_count")
if expected_count and expected_count != actual_count:
    report_error(f, f"piece_count mismatch: metadata={expected_count}, actual={actual_count}")
```

### Phase 3: ID検証

1. 全駒の `id` フィールドを収集
2. **重複チェック**: 同一IDが複数存在しないか
3. **ソート順チェック**: IDが昇順に並んでいるか
4. **欠番チェック**: ID範囲内に欠番がないか報告（info レベル）

```python
ids = [p["id"] for p in data["pieces"]]
# 重複チェック
duplicates = [id for id in ids if ids.count(id) > 1]
# ソート順チェック
is_sorted = ids == sorted(ids)
# 欠番チェック
full_range = set(range(min(ids), max(ids) + 1))
gaps = full_range - set(ids)
```

### Phase 4: promotes_to参照検証

1. 全駒の `promotes_to` 値を収集（null 以外）
2. 参照先IDが同一ファイル内（または横断検証時は全ファイル内）に存在するか確認
3. **循環参照チェック**: A→B→A のようなループを検出

```python
all_ids = set(p["id"] for p in data["pieces"])
for piece in data["pieces"]:
    target = piece.get("promotes_to")
    if target is not None and target not in all_ids:
        report_error(f, f"ID {piece['id']} promotes_to={target} not found")
    # can_promote 整合性
    if piece.get("can_promote") and target is None:
        report_warning(f, f"ID {piece['id']} can_promote=true but promotes_to=null")
    if not piece.get("can_promote") and target is not None:
        report_warning(f, f"ID {piece['id']} can_promote=false but promotes_to={target}")

# 循環参照チェック
def check_cycle(piece_id, visited=set()):
    if piece_id in visited:
        return True  # cycle detected
    visited.add(piece_id)
    target = promotion_map.get(piece_id)
    if target:
        return check_cycle(target, visited)
    return False
```

### Phase 5: 方向値型検証

YAML 1.1 では `N` → `false`, `Y` → `true` に自動変換される問題を検出する。

1. 全駒の `movement.steps` を走査
2. 各 step の `direction` フィールドの型を確認
3. **boolean型** → ERROR（変換済み）
4. **文字列型** → 許可値 `"N", "NE", "E", "SE", "S", "SW", "W", "NW"` に一致するか確認

```python
VALID_DIRECTIONS = {"N", "NE", "E", "SE", "S", "SW", "W", "NW"}
for piece in data["pieces"]:
    for i, step in enumerate(piece.get("movement", {}).get("steps", [])):
        d = step.get("direction")
        if isinstance(d, bool):
            report_error(f, f"ID {piece['id']} step[{i}] direction is boolean ({d}), expected quoted string")
        elif isinstance(d, str):
            if d not in VALID_DIRECTIONS:
                report_warning(f, f"ID {piece['id']} step[{i}] unknown direction: {d}")
        else:
            report_error(f, f"ID {piece['id']} step[{i}] direction has unexpected type: {type(d)}")
```

### Phase 6: 必須フィールド検証

1. デフォルト必須フィールドリスト:
   - `id`, `name`, `name_ja`, `name_kanji`, `value`, `can_promote`, `promotes_to`, `movement`, `is_ranged`, `special_rules`
2. 各駒について全必須フィールドの存在を確認
3. `movement.steps` が空リストでないか確認
4. 各 step に `direction` と `type` が存在するか確認

```python
REQUIRED = ["id", "name", "name_ja", "name_kanji", "value",
            "can_promote", "promotes_to", "movement", "is_ranged", "special_rules"]
for piece in data["pieces"]:
    for field in REQUIRED:
        if field not in piece:
            report_error(f, f"ID {piece.get('id', '?')} missing field: {field}")
    steps = piece.get("movement", {}).get("steps", [])
    if not steps:
        report_warning(f, f"ID {piece['id']} has empty movement.steps")
    for i, step in enumerate(steps):
        if "direction" not in step:
            report_error(f, f"ID {piece['id']} step[{i}] missing 'direction'")
        if "type" not in step:
            report_error(f, f"ID {piece['id']} step[{i}] missing 'type'")
```

### Phase 7: 横断検証（cross_validate: true の場合）

1. 全ファイルのIDを統合し、ファイル間のID重複を検出
2. ファイルを跨いだ promotes_to 参照の存在確認
3. 全体の駒数・ID範囲のサマリ出力

```python
global_ids = {}  # id -> file mapping
for f, data in all_files.items():
    for piece in data["pieces"]:
        pid = piece["id"]
        if pid in global_ids:
            report_error("cross", f"ID {pid} duplicated in {global_ids[pid]} and {f}")
        global_ids[pid] = f

# 横断成り先参照チェック
all_id_set = set(global_ids.keys())
for f, data in all_files.items():
    for piece in data["pieces"]:
        target = piece.get("promotes_to")
        if target is not None and target not in all_id_set:
            report_error(f, f"ID {piece['id']} promotes_to={target} not found in any file")
```

### レポート出力

全フェーズの結果を統合し、`output_format` に応じてレポートを出力する。

**YAML形式:**
```yaml
validation_result:
  metadata:
    validated_at: "2026-01-29T18:00:00"
    files_checked: [...]
    total_pieces: 404
  summary:
    errors: 0
    warnings: 2
    phase_results:
      phase1_parse: "pass"
      phase2_metadata: "pass"
      phase3_ids: "pass"
      phase4_promotion: "pass"
      phase5_direction: "pass"
      phase6_fields: "pass"
      phase7_cross: "pass"
  findings: []
```

**Markdown形式:**
```markdown
# Taikyoku YAML Validation Report

## Summary
- Files: 4
- Total pieces: 404
- Errors: 0
- Warnings: 2

## Phase Results
| Phase | Result |
|-------|--------|
| 1. Parse | PASS |
| 2. Metadata | PASS |
...

## Findings
(findings listed here)
```

## Examples

### Example 1: 全グループ一括検証

**入力:**
```
yaml_files:
  - config/pieces/taikyoku_a.yaml
  - config/pieces/taikyoku_b.yaml
  - config/pieces/taikyoku_c.yaml
  - config/pieces/taikyoku_d.yaml
cross_validate: true
```

**出力（正常時）:**
```yaml
validation_result:
  metadata:
    validated_at: "2026-01-29T18:45:00"
    files_checked:
      - "config/pieces/taikyoku_a.yaml"
      - "config/pieces/taikyoku_b.yaml"
      - "config/pieces/taikyoku_c.yaml"
      - "config/pieces/taikyoku_d.yaml"
    cross_validation: true
    total_pieces: 404

  summary:
    errors: 0
    warnings: 0
    passed_checks: 28
    phase_results:
      phase1_parse: "pass"
      phase2_metadata: "pass"
      phase3_ids: "pass"
      phase4_promotion: "pass"
      phase5_direction: "pass"
      phase6_fields: "pass"
      phase7_cross: "pass"

  findings: []

  file_details:
    - file: "taikyoku_a.yaml"
      piece_count: 101
      id_range: [100, 205]
      metadata_match: true
    - file: "taikyoku_b.yaml"
      piece_count: 102
      id_range: [206, 309]
      metadata_match: true
    - file: "taikyoku_c.yaml"
      piece_count: 101
      id_range: [310, 413]
      metadata_match: true
    - file: "taikyoku_d.yaml"
      piece_count: 100
      id_range: [414, 517]
      metadata_match: true
```

### Example 2: PyYAML再保存ファイルの検証（エラー検出時）

**入力:**
```
yaml_files:
  - config/pieces/taikyoku_part2.yaml
cross_validate: false
```

**出力（エラー検出時）:**
```yaml
validation_result:
  summary:
    errors: 15
    warnings: 3

  findings:
    - id: "E001"
      phase: 5
      severity: "error"
      file: "taikyoku_part2.yaml"
      description: "ID 154: step[0] direction is boolean (False), expected quoted string \"N\""
      piece: "Turtle dove (鳩槃)"

    - id: "E002"
      phase: 5
      severity: "error"
      file: "taikyoku_part2.yaml"
      description: "ID 155: step[0] direction is boolean (False), expected quoted string \"N\""
      piece: "Flying swallow (飛燕)"

    - id: "W001"
      phase: 6
      severity: "warning"
      file: "taikyoku_part2.yaml"
      description: "ID 160: missing field: special_rules"
      piece: "Rain dragon (雨龍)"
```

## Guidelines

1. **Phase 順序を守る**: Phase 1 でパース失敗したファイルは Phase 2 以降をスキップする。エラーの根本原因を先に修正させる
2. **severity を適切に判定**: データ破損・参照エラーは `error`、品質懸念・意図的可能性は `warning`、統計情報は `info`
3. **boolean 問題を最優先**: Phase 5 のYAML 1.1 boolean変換は大局将棋YAMLで最も頻出する問題。パース後の型チェックだけでなく、Raw テキストでの `direction: N`（クォートなし）パターンも Grep で補助検出する
4. **横断検証のスコープ**: cross_validate: true の場合のみ Phase 7 を実行。単一ファイル検証時はスキップ
5. **Python スクリプトの活用**: Phase 3-6 は Python ワンライナーまたは短いスクリプトで実行すると効率的。Bash tool で `python3 -c "..."` を使用
6. **修正提案を含める**: 各 finding に対して、具体的な修正方法を description または recommendation に記述する
7. **大局将棋以外にも適用可能**: required_fields をカスタマイズすることで、standard_shogi.yaml 等の他の将棋駒定義YAMLにも使用できる
8. **レポートの構造化**: 出力は常に構造化形式（YAML/Markdown）で。人間が読みやすく、かつプログラムでもパース可能な形式を維持する
