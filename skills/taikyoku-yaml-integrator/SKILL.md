---
name: taikyoku-yaml-integrator
description: 複数に分割された大局将棋YAML駒定義ファイルを統合・検証する。パース検証、ID重複・欠番検出、成り先参照整合性チェック、YAML 1.1方向値クォート修正、未定義SpecialRule検出、粒度統一（コメント形式・flow style・ID順ソート）を一括実行。cmd_007のような分割YAML統合作業時に使用。
---

# Taikyoku YAML Integrator

## Overview

複数パートに分割された大局将棋の駒定義YAMLファイルを1つのファイルに統合し、フォーマットの粒度統一と横断検証を行う。出力は統合済みYAMLファイルと検証レポートの2つ。

主な機能:
- **統合**: 複数YAMLの駒データを1ファイルにマージ（ID順ソート）
- **検証**: パース、ID重複・欠番、promotes_to参照、方向値クォート、SpecialRule
- **粒度統一**: コメント形式、flow style steps、フィールド順序の統一

## When to Use

- 複数の `taikyoku_part*.yaml` を `taikyoku_[a-d].yaml` に統合する作業
- 新しいパートファイルを追加した後の横断品質チェック
- 統合前に全パートの整合性を事前検証したい場合
- YAML駒定義のフォーマットを統一したい場合

## Instructions

### Step 1: 入力情報の確認

以下を確認する:

1. **source_files**: 統合元YAMLファイルのパスリスト
2. **output_file**: 統合先ファイルパス
3. **group_name**: グループ名（例: "Taikyoku Shogi Group B"）
4. **id_range**: 期待するID範囲（任意）
5. **validate_only**: 検証のみか統合も行うか

### Step 2: Phase 1 — YAMLパース検証

各ソースファイルを読み込み、パースできることを確認する。

```bash
python3 -c "
import yaml, sys
for f in sys.argv[1:]:
    try:
        data = yaml.safe_load(open(f))
        pieces = data.get('pieces', [])
        meta_count = data.get('metadata', {}).get('piece_count', '?')
        print(f'{f}: OK ({len(pieces)} pieces, metadata says {meta_count})')
        if len(pieces) != meta_count:
            print(f'  WARNING: piece_count mismatch')
    except yaml.YAMLError as e:
        print(f'{f}: PARSE ERROR - {e}')
" file1.yaml file2.yaml
```

**チェック項目**:
- パースエラーの有無
- `metadata.piece_count` と実際の駒数の一致

### Step 3: Phase 2 — ID重複・欠番検出

全駒のIDを収集し、重複と欠番を検出する。

```bash
python3 -c "
import yaml, sys, collections
all_ids = []
for f in sys.argv[1:]:
    data = yaml.safe_load(open(f))
    for p in data.get('pieces', []):
        all_ids.append((p['id'], p['name'], f))
all_ids.sort()
# 重複チェック
id_counts = collections.Counter(id for id, _, _ in all_ids)
for id_val, count in id_counts.items():
    if count > 1:
        dupes = [(n, f) for i, n, f in all_ids if i == id_val]
        print(f'DUPLICATE ID {id_val}: {dupes}')
# 欠番チェック（min〜maxの範囲内）
ids = sorted(id_counts.keys())
for i in range(ids[0], ids[-1]+1):
    if i not in id_counts:
        print(f'GAP: ID {i} missing')
print(f'Total: {len(all_ids)} pieces, ID range {ids[0]}-{ids[-1]}')
" file1.yaml file2.yaml
```

### Step 4: Phase 3 — promotes_to参照検証

成り先IDが全駒の中に存在するか確認する。

```bash
python3 -c "
import yaml, sys
all_pieces = {}
for f in sys.argv[1:]:
    data = yaml.safe_load(open(f))
    for p in data.get('pieces', []):
        all_pieces[p['id']] = p
for pid, p in sorted(all_pieces.items()):
    pt = p.get('promotes_to')
    if pt is not None and pt not in all_pieces:
        print(f'REF ERROR: {p[\"name\"]} (ID {pid}) promotes_to {pt} - NOT FOUND')
    if pt is not None and pt in all_pieces:
        target = all_pieces[pt]
        if target.get('can_promote', False):
            print(f'WARNING: {target[\"name\"]} (ID {pt}) is a promotion target but can_promote=true (double promotion?)')
print('Promotion reference check complete.')
" file1.yaml file2.yaml
```

### Step 5: Phase 4 — 方向値クォート検証

YAMLファイル内の方向値がダブルクォートで囲まれているか確認する。

Grep で以下のパターンを検索:
```
pattern: 'direction:\s+[A-Z]+\s*[,}]'  # クォートなし方向値
```

または:
```
pattern: 'direction:\s+[^"]'  # ダブルクォート以外で始まる方向値
```

有効な方向値: `"N"`, `"NE"`, `"E"`, `"SE"`, `"S"`, `"SW"`, `"W"`, `"NW"`

`jump_overs` 配列内も同様にチェック:
```
pattern: 'jump_overs:\s*\[[^"]*[A-Z]'  # クォートなしjump_overs値
```

### Step 6: Phase 5 — SpecialRule検証

駒定義で使用されている SpecialRule が既知リストに含まれるか確認する。

既知SpecialRule（PieceDefinition.hs の `data SpecialRule` より）:
- `Royal`
- `NifuProhibited`
- `CannotDropInZone` (引数付き)
- `MustPromoteInZone` (引数付き)
- `Igui`
- `CanCaptureTwo`

```bash
python3 -c "
import yaml, sys
KNOWN = {'Royal','NifuProhibited','Igui','CanCaptureTwo'}
KNOWN_PREFIX = {'CannotDropInZone','MustPromoteInZone'}
for f in sys.argv[1:]:
    data = yaml.safe_load(open(f))
    for p in data.get('pieces', []):
        for rule in p.get('special_rules', []):
            r = rule.split()[0] if isinstance(rule, str) else str(rule)
            if r not in KNOWN and r not in KNOWN_PREFIX:
                print(f'UNDEFINED: {p[\"name\"]} (ID {p[\"id\"]}) has unknown SpecialRule: {rule}')
" file1.yaml file2.yaml
```

### Step 7: Phase 6 — 粒度統一

全駒をID順にソートし、統一フォーマットで出力する。

**各駒のフォーマット**:
```yaml
  # --- <English name> (<漢字>) ---
  # <movement description>
  - id: <ID>
    name: "<English>"
    name_ja: "<日本語>"
    name_kanji: "<漢字>"
    value: <value>
    can_promote: <true/false>
    promotes_to: <ID or null>
    movement:
      steps:
        - { direction: "N", type: single }
      jumps:          # ある場合のみ
        - [-2, -1]
      area_moves:     # ある場合のみ
        - { radius: 2, max_steps: 2, can_return: true }
      jump_overs:     # ある場合のみ
        - ["N", "S"]
    is_ranged: <true/false>
    special_rules: []
```

**移動パターンの1行サマリ生成規則**:
- ranging方向: `Ranging N/S/NE/NW`
- single方向: `1-step E/W`
- limited方向: `limited-2 SE/SW`
- jumps: `jump [-2,-1]/[-2,1]`
- area_moves: `area move (radius 2, 2 steps, can return)`
- jump_overs: `jump_overs N/S`
- 複数種を `+` で結合: `Ranging N/S + 1-step E/W + limited-2 NE/NW`
- 成り駒は末尾に `; promotes from <piece name>` を追加

### Step 8: Phase 7 — 統合ファイル出力

1. ファイルヘッダを生成:
```yaml
# Taikyoku Shogi Piece Definitions - <Group Name>
# 大局将棋 駒定義 <グループ名日本語>
# ID range: <min>-<max> (<first piece> ~ <last piece> + promoted forms)
```

2. metadata セクションを生成（piece_count は実際の駒数を使用）
3. Phase 6 で統一した全駒を `pieces:` セクションとして出力
4. 出力ファイルを `python3 yaml.safe_load` で最終パース検証
5. 検証レポートを生成（Phase 1-5 の結果統合）

### Step 9: 検証レポート出力

```yaml
validation_report:
  metadata:
    validated_at: "<timestamp>"
    source_files: [...]
    output_file: "<path>"
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
    - id: "<TYPE-NNN>"
      phase: N
      severity: "critical | moderate | minor"
      description: "..."
```

## Examples

### Example 1: Part3+Part4 → Group B 統合

**状況**: cmd_007 Wave1 で taikyoku_part3.yaml (50駒) と taikyoku_part4.yaml (52駒) を Group B に統合する。

**実行**:
1. Phase 1: 両ファイルをパース → 正常
2. Phase 2: ID 206-309、欠番 ID 231, 233（意図的、Phoenix Master/Kirin Master のID構造による）
3. Phase 3: 全 promotes_to 参照が統合対象内に存在 → 正常
4. Phase 4: 全方向値がダブルクォート済み → 正常
5. Phase 5: 使用 SpecialRule は Igui, CanCaptureTwo のみ → 全て既知
6. Phase 6: ID順ソート、コメント統一、flow style統一
7. Phase 7: taikyoku_b.yaml (102駒) 出力、パース検証 OK

**出力**: taikyoku_b.yaml + 検証レポート（findings 2件: GAP-001, GAP-002、severity: minor）

### Example 2: 全8パートの横断検証（統合なし）

**状況**: 全パートファイルの品質を一括チェックしたい。

**実行**:
```yaml
source_files: ["config/pieces/taikyoku_part*.yaml"]
validate_only: true
```

1. Phase 1-5 を全ファイルに対して実行
2. 統合ファイルは出力しない
3. 検証レポートのみ出力

## Guidelines

1. **データを変更しない**: 粒度統一はフォーマット（コメント、空白、順序）のみ。movement, value 等の駒データは元ファイルをそのまま維持する
2. **方向値は必ずダブルクォート**: `"N"`, `"NE"` 等。YAML 1.1 では `N` が `false`（No）にパースされる問題を防止
3. **ID順ソートが基本**: セクション分け（基本駒/成り駒）よりもID昇順ソートを優先。これにより基本駒とその成り先が隣接する
4. **欠番は minor**: 意図的な欠番（ID体系の都合）もあるため、severity は minor とする。人間が判断
5. **promotes_to の参照エラーは critical**: 成り先が見つからないのはゲームロジックに直結するため critical
6. **出力後のパース検証は必須**: 統合ファイル生成後、必ず `python3 yaml.safe_load` で読み込めることを確認
7. **元ファイルは削除しない**: 統合元のパートファイルは残す。削除は別タスク（Wave2等）で行う
8. **flow style の一貫性**: steps 内は全て `{ direction: "X", type: Y }` 形式。limited の場合は `{ direction: "X", type: { limited: N } }`
