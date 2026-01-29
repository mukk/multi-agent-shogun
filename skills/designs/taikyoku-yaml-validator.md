# Skill Design: taikyoku-yaml-validator

> **Version**: 1.0.0
> **Status**: Draft
> **Created**: 2026-01-29
> **Author**: ashigaru2
> **Origin**: 足軽6 提案 + cmd_005/cmd_007 統合検証実績

---

## 1. スキル概要

### 1.1 目的

大局将棋（Taikyoku Shogi）のYAML駒定義ファイルの構造検証を行うスキル。YAMLパース、metadata整合性、ID体系、成り先参照、方向値の型安全性、必須フィールドの存在、ファイル横断でのID重複を7フェーズで検証し、構造化レポートとして出力する。

大局将棋は209種類・402枚の駒を持つ世界最大の将棋バリアントであり、駒定義YAMLファイルは4グループ（A-D）に分割されている。手動管理では検出困難な参照整合性やYAML 1.1 boolean問題を体系的に検出する。

### 1.2 ユースケース

| ユースケース | 説明 | 主要検証フェーズ |
|-------------|------|-----------------|
| 新規駒定義の追加後 | 新しい駒をYAMLに追加した際、IDの重複や成り先参照の整合性を確認 | Phase 3, 4 |
| パート統合後の検証 | 複数パートファイルを統合した後のデータ完全性を検証 | Phase 1-7 全て |
| PyYAML再保存後の検証 | PyYAMLで保存されたファイルの方向値がbooleanに変換されていないか確認 | Phase 5 |
| 駒データの定期監査 | 全YAMLファイルの一括検証による品質保証 | Phase 1-7 全て |
| PR レビュー補助 | 駒定義変更のPRで、データ整合性が壊れていないか確認 | Phase 3, 4, 6 |

### 1.3 背景

本スキルは以下の実績に基づく：

- **cmd_005 Wave2**: 8パートファイル（404駒）のYAML検証を手動実施。ID重複、成り先参照、方向値型チェックをPythonスクリプトで検証
- **cmd_007 Wave1**: 4グループへの統合時にyaml.safe_load + ID検証 + フィールド存在検証を実施
- **足軽6 提案**: taikyoku-yaml-validator として汎用スキル化を提案

---

## 2. 入力パラメータ

### 2.1 必須パラメータ

| パラメータ | 型 | 説明 |
|-----------|-----|------|
| `yaml_files` | List[String] | 検証対象のYAMLファイルパス群（glob可） |

### 2.2 オプションパラメータ

| パラメータ | 型 | デフォルト | 説明 |
|-----------|-----|-----------|------|
| `cross_validate` | Boolean | `true` | ファイル横断検証を行うか |
| `output_format` | Enum | `yaml` | 出力形式: `yaml`, `markdown` |
| `output_path` | String | `stdout` | レポート出力先パス |
| `strict_mode` | Boolean | `false` | true の場合、warning も error 扱い |
| `expected_id_range` | Object | `null` | `{ min: N, max: N }` ID範囲の期待値 |
| `required_fields` | List[String] | `["id", "name", "name_ja", "name_kanji", "value", "can_promote", "promotes_to", "movement", "is_ranged", "special_rules"]` | 各駒に必須のフィールド群 |

### 2.3 入力例

```yaml
# 単一ファイル検証
yaml_files:
  - "config/pieces/taikyoku_a.yaml"

# 全グループ横断検証
yaml_files:
  - "config/pieces/taikyoku_a.yaml"
  - "config/pieces/taikyoku_b.yaml"
  - "config/pieces/taikyoku_c.yaml"
  - "config/pieces/taikyoku_d.yaml"
cross_validate: true
output_format: markdown
```

```yaml
# カスタム必須フィールド（簡易版）
yaml_files:
  - "config/pieces/standard_shogi.yaml"
required_fields:
  - "id"
  - "name"
  - "movement"
cross_validate: false
```

---

## 3. 出力フォーマット

### 3.1 トップレベル構造

```yaml
validation_result:
  metadata:
    validated_at: "ISO8601 timestamp"
    files_checked: ["path1", "path2"]
    cross_validation: true
    total_pieces: N

  summary:
    errors: N
    warnings: N
    passed_checks: N
    phase_results:
      phase1_parse: "pass | fail"
      phase2_metadata: "pass | fail | N warnings"
      phase3_ids: "pass | fail | N warnings"
      phase4_promotion: "pass | fail | N warnings"
      phase5_direction: "pass | fail | N warnings"
      phase6_fields: "pass | fail | N warnings"
      phase7_cross: "pass | fail | skip"

  findings:
    # セクション 3.2

  file_details:
    # セクション 3.3
```

### 3.2 検出結果（findings）

```yaml
findings:
  - id: "E001"
    phase: 1
    severity: "error"
    file: "taikyoku_a.yaml"
    description: "YAMLパースエラー: line 42, column 5"
    detail: "found character '\\t' that cannot start any token"

  - id: "E002"
    phase: 3
    severity: "error"
    file: "taikyoku_a.yaml"
    description: "ID重複: ID 150 が2箇所に存在"
    pieces: ["Flying swallow (飛燕)", "Flying swallow promoted (奔燕)"]

  - id: "W001"
    phase: 4
    severity: "warning"
    file: "taikyoku_b.yaml"
    description: "成り先参照先不在: ID 250 の promotes_to=999 が未定義"
    piece: "Example piece (例駒)"

  - id: "E003"
    phase: 5
    severity: "error"
    file: "taikyoku_a.yaml"
    description: "方向値が boolean に変換済み: piece ID 105, direction value is True (expected \"N\")"
    piece: "Gold general (金将)"
    field_path: "movement.steps[0].direction"

  - id: "W002"
    phase: 3
    severity: "warning"
    file: "taikyoku_a.yaml"
    description: "ID欠番: 120-124 が未定義（連番でない場合は無視可）"
```

### 3.3 ファイル別詳細

```yaml
file_details:
  - file: "taikyoku_a.yaml"
    piece_count: 101
    id_range: [100, 205]
    metadata_piece_count: 101
    metadata_match: true
    direction_values_all_string: true
    missing_fields: []
```

### 3.4 severity 分類基準

| severity | 基準 | 例 |
|----------|------|-----|
| error | データ破損・パース不能・参照エラー | YAMLパース失敗、ID重複、成り先が未定義IDを参照、方向値がboolean |
| warning | データ品質の懸念、意図的な可能性あり | ID欠番、metadata件数不一致、promotes_to未設定の成り可能駒 |
| info | 統計情報、確認事項 | 検証通過、ファイル別駒数サマリ |

---

## 4. 検証フェーズ詳細

### Phase 1: YAMLパース検証

YAMLファイルが正しくパースできることを確認する。

#### 4.1.1 検証内容

```
1. 各 yaml_files を python3 yaml.safe_load (または同等のパーサ) でパース
2. パースエラーがあれば行番号・カラム・エラー内容を記録
3. パース成功したファイルのみ Phase 2 以降に進む
```

#### 4.1.2 検出パターン

| パターン | severity | 説明 |
|---------|----------|------|
| パースエラー | error | YAML構文エラー（インデント不正、不正文字等） |
| 空ファイル | error | ファイルは存在するが内容が空 |
| pieces キー不在 | error | トップレベルに `pieces` リストがない |

### Phase 2: metadata検証

`metadata` セクションの整合性を確認する。

#### 4.2.1 検証内容

```
1. metadata.piece_count が実際の pieces リスト長と一致するか
2. metadata.name が存在するか
3. metadata.version が存在するか
```

#### 4.2.2 検出パターン

| パターン | severity | 説明 |
|---------|----------|------|
| piece_count 不一致 | error | metadata の件数と実際の駒数が異なる |
| metadata 未定義 | warning | metadata セクション自体がない |
| name/version 未定義 | warning | metadata 内の必須フィールドがない |

### Phase 3: ID検証

駒IDの一意性・順序・範囲を検証する。

#### 4.3.1 検証内容

```
1. 全駒の id フィールドを収集
2. ID重複チェック（同一ファイル内）
3. IDソート順チェック（昇順であること）
4. ID欠番チェック（連番を期待する場合）
5. expected_id_range が指定されている場合、範囲外IDを検出
```

#### 4.3.2 検出パターン

| パターン | severity | 説明 |
|---------|----------|------|
| ID重複 | error | 同一ID が複数駒に割り当て |
| ソート順違反 | warning | IDが昇順でない |
| 範囲外ID | warning | expected_id_range 外のID |
| ID欠番 | info | 連番の隙間（意図的な場合あり） |

### Phase 4: promotes_to参照検証

成り先IDの参照整合性を検証する。

#### 4.4.1 検証内容

```
1. can_promote: true の駒について promotes_to が設定されているか
2. promotes_to の値が同一ファイル（または cross_validate 時は全ファイル）内に存在するか
3. 循環参照チェック: A→B→A のように成りが循環しないか
4. can_promote: false の駒に promotes_to が設定されていないか（null であること）
```

#### 4.4.2 検出パターン

| パターン | severity | 説明 |
|---------|----------|------|
| 成り先ID未定義 | error | promotes_to が指すIDが存在しない |
| 循環参照 | error | 成り先が循環（A→B→A） |
| 成り可能だが成り先なし | warning | can_promote: true だが promotes_to: null |
| 成り不可だが成り先あり | warning | can_promote: false だが promotes_to に値がある |

### Phase 5: 方向値型検証

YAML 1.1 の boolean 問題（`N` → `false`, `Y` → `true` 等）を検出する。

#### 4.5.1 背景

YAML 1.1 仕様では以下の値が boolean として解釈される：
- `y`, `Y`, `yes`, `Yes`, `YES` → `true`
- `n`, `N`, `no`, `No`, `NO` → `false`
- `on`, `On`, `ON` → `true`
- `off`, `Off`, `OFF` → `false`

将棋の方向値 `"N"` (North) がクォートなしの場合、`false` に変換されてしまう。

#### 4.5.2 検証内容

```
1. 全駒の movement.steps 内の direction フィールドを走査
2. direction 値の型を確認:
   - 文字列型 → OK（"N", "NE", "S" 等）
   - boolean型 → ERROR（N→false変換が発生済み）
   - 数値型 → ERROR（予期しない型）
3. 許可される方向値: "N", "NE", "E", "SE", "S", "SW", "W", "NW"
4. 上記以外の文字列値 → WARNING（不明な方向値）
```

#### 4.5.3 検出パターン

| パターン | severity | 説明 |
|---------|----------|------|
| boolean変換済み | error | direction が True/False（元の "N"/"Y" が変換された） |
| 不正型 | error | direction が数値や null |
| 不明方向値 | warning | 8方向以外の文字列値 |

### Phase 6: 必須フィールド検証

各駒に必要なフィールドが存在するかを検証する。

#### 4.6.1 検証内容

```
1. required_fields の各フィールドが全駒に存在するか確認
2. movement.steps が空リストでないか確認
3. 各 step に direction と type が存在するか確認
4. value が正の数値であるか確認
```

#### 4.6.2 デフォルト必須フィールド

```yaml
required_fields:
  - id            # 駒ID（整数）
  - name          # 英語名（文字列）
  - name_ja       # 日本語名（文字列）
  - name_kanji    # 漢字名（文字列）
  - value         # 評価値（正の数値）
  - can_promote   # 成り可否（boolean）
  - promotes_to   # 成り先ID（整数 or null）
  - movement      # 移動パターン（オブジェクト）
  - is_ranged     # 遠距離攻撃可否（boolean）
  - special_rules # 特殊ルール（リスト）
```

#### 4.6.3 検出パターン

| パターン | severity | 説明 |
|---------|----------|------|
| 必須フィールド欠損 | error | 必須フィールドが駒定義に存在しない |
| movement.steps 空 | warning | 移動パターンが定義されていない |
| step に direction/type 欠損 | error | ステップ定義が不完全 |
| value が非正値 | warning | 評価値が 0 以下（意図的な場合あり） |

### Phase 7: 横断検証

複数ファイル間でのデータ整合性を検証する（`cross_validate: true` の場合のみ）。

#### 4.7.1 検証内容

```
1. 全ファイルの ID を統合し、ファイル間のID重複を検出
2. promotes_to がファイルを跨いで参照している場合の存在確認
3. 全ファイルの合計駒数を報告
4. IDの全体範囲と欠番を報告
```

#### 4.7.2 検出パターン

| パターン | severity | 説明 |
|---------|----------|------|
| ファイル間ID重複 | error | 異なるファイルに同一IDが存在 |
| ファイル間成り先参照エラー | error | promotes_to が他ファイルのIDを参照しているが存在しない |
| IDギャップ（ファイル間） | info | ファイル間のID範囲に隙間がある |

---

## 5. 使用ツール・手法

| カテゴリ | ツール/手法 | 用途 |
|---------|-----------|------|
| ファイル検索 | Glob | YAMLファイルの列挙 |
| ファイル読み込み | Read | YAMLファイルの読み込み |
| パース検証 | Bash (python3 yaml.safe_load) | YAMLパース + 構造検査スクリプト |
| パターン検索 | Grep | direction値のbooleanパターン検出 |
| 構造化解析 | Python (インラインスクリプト) | ID重複、参照整合性、型チェック |
| 出力生成 | Write | YAML/Markdown レポート生成 |

---

## 6. 制限事項

### 6.1 技術的制限

| 制限 | 説明 | 回避策 |
|-----|------|--------|
| YAML パーサ依存 | yaml.safe_load の挙動に依存（YAML 1.1 vs 1.2 の差異） | パース前にRawテキストでのboolean値チェックも実施 |
| コメント検証不可 | yaml.safe_load ではコメントが消失するため、コメントの存在・品質は検証不可 | コメント検証にはRaw テキスト解析が必要 |
| カスタム型未対応 | movement.type のカスタム値（limited: N 等）の妥当性は検証対象外 | 将来的にスキーマ定義を追加可能 |
| 大局将棋固有ルール | 特殊ルールの意味的妥当性（Royal は王のみ等）は検証しない | ドメイン知識に基づく拡張Phase の追加で対応 |

### 6.2 前提条件

- YAMLファイルが `metadata` + `pieces` 構造を持つこと
- 各駒が `id` フィールドを持つこと（最低限のキー）
- Python 3 が実行環境で利用可能であること（yaml.safe_load 使用時）

### 6.3 精度について

- Phase 5（方向値型検証）はパース後の値の型チェックであり、パース前のソーステキストも補助的に確認することで精度向上
- Phase 4（循環参照）は直接循環のみ検出。3段以上の間接循環は検出対象だが、深い循環チェーンではパフォーマンスに影響する可能性あり
- Phase 7（横断検証）は指定されたファイル群のみ対象。未指定ファイルとの整合性は検証しない

---

## 7. 将来の拡張案

| 優先度 | 拡張案 | 説明 |
|--------|--------|------|
| high | スキーマ定義 | movement.type の許可値リスト、special_rules の許可値リストを外部定義 |
| high | コメント品質チェック | Raw テキスト解析で `# --- Name (漢字) ---` 形式のコメント有無を検証 |
| medium | flow style 検証 | steps が flow style `{ direction: "N", type: single }` で記述されているか |
| medium | CI統合 | pre-commit hook での自動検証 |
| medium | 差分モード | Git diff と連携して変更された駒のみ検証 |
| low | 移動パターン妥当性 | 方向と移動タイプの組み合わせが将棋ルールとして妥当か |
| low | 名前一貫性チェック | name, name_ja, name_kanji の対応が辞書と一致するか |

---

## 8. 実績データ（cmd_005/cmd_007 からの知見）

### 8.1 検証実績

| 指標 | 値 |
|------|-----|
| 検証対象ファイル | taikyoku_a/b/c/d.yaml（4ファイル） |
| 総駒数 | 404駒（101 + 102 + 101 + 100） |
| ID範囲 | 100-517 |
| 検出された問題例 | PyYAML再保存による方向値boolean変換、promotes_to フィールド欠損 |

### 8.2 検出された問題パターン

| 問題 | フェーズ | 発生頻度 | 対処 |
|------|---------|---------|------|
| 方向値 "N" → false 変換 | Phase 5 | PyYAML再保存ファイルで頻発 | ダブルクォート必須化 |
| promotes_to フィールド欠損 | Phase 6 | Part1系の非成り駒で散見 | `promotes_to: null` 統一付与 |
| special_rules フィールド欠損 | Phase 6 | Part1系の一部駒 | `special_rules: []` 統一付与 |
| metadata piece_count 不一致 | Phase 2 | 統合作業中の一時的不整合 | 統合後に件数再計算 |

### 8.3 手動検証からの自動化効果

- 手動検証: 4ファイル×7フェーズの検証をPythonワンライナーで個別実行
- スキル化により: 全フェーズを統合した一括検証、構造化レポート自動生成が可能

---

## 9. 参考資料

- cmd_005 統合検証: 足軽1-8 による404駒のYAML作成・検証実績
- cmd_007 Wave1/Wave2: 8パート→4グループ統合・粒度統一・最終検証
- taikyoku-yaml-integrator 提案: 足軽1 による統合検証スキル候補
- 大局将棋駒定義フォーマット: `config/pieces/taikyoku_b.yaml` 等
