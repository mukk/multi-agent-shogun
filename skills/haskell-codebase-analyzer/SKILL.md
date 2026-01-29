---
name: haskell-codebase-analyzer
description: Haskellプロジェクトの静的解析を体系的に行い、モジュール依存関係の可視化、コード品質問題の severity 分類検出、テストカバレッジ分析、ADT拡張性評価、リファクタリング提案の生成までを一貫して実施する。正規表現ベースの軽量解析で、ビルド不要。
---

# Haskell Codebase Analyzer

## Overview

Haskellプロジェクトの静的解析を体系的に行い、コードベースの構造・設計品質・テストカバレッジ・拡張性を評価するスキル。以下を一貫して実施する：

- **プロジェクト構造解析**: ビルドシステム検出、ディレクトリ構造、GHCバージョン・拡張の使用状況
- **モジュール依存関係グラフ**: import解析による依存DAG構築、循環依存・孤立モジュールの検出
- **コード品質問題検出**: severity 3段階分類（critical/moderate/minor）で問題を体系的に検出
- **テストカバレッジ分析**: テストフレームワーク検出、未テストモジュールの特定
- **ADT解析**: 型定義の抽出、パターンマッチ網羅性チェック、拡張性スコア算出
- **リファクタリング提案**: priority 分類（high/medium/low）で具体的な改善案を生成

正規表現ベースの軽量解析であり、ビルド環境やGHCのインストールは不要。

## When to Use

- 新規参画時にプロジェクト構造とモジュール依存関係を把握したい
- リファクタリング計画のためADT変更時の影響範囲を事前調査したい
- 設計レビューで拡張性・保守性の評価レポートを作成したい
- 技術的負債を可視化し、問題の重要度別分類と改善提案を得たい
- テスト品質を評価し、未テストモジュールを特定したい
- `error`/`undefined`/部分関数等の安全性問題を棚卸ししたい

## Instructions

### 必要な入力情報の確認

ユーザーから以下の情報を取得する（または推定する）：

1. **project_path**: Haskellプロジェクトのルートパス
2. **analysis_depth**: 解析深度（`quick`, `standard`, `full`。デフォルト: `full`）
3. **target_modules**: 特定モジュールに絞る場合のリスト（デフォルト: 全モジュール）
4. **output_format**: 出力形式（`yaml`, `json`, `markdown`。デフォルト: `yaml`）
5. **include_tests**: テストディレクトリを含めるか（デフォルト: `true`）

解析深度による実行範囲：

| 深度 | 実行項目 |
|------|---------|
| `quick` | ディレクトリ構造、モジュール一覧、GHC拡張、ビルド設定 |
| `standard` | quick + 依存関係グラフ、品質問題検出、テストカバレッジ |
| `full` | standard + ADT解析、パターンマッチ検出、アルゴリズム評価、リファクタリング提案 |

### Phase 1: プロジェクト検出（quick）

1. `project_path` から `.cabal`, `package.yaml`, `stack.yaml` を Glob で検出
2. `.cabal` または `package.yaml` を Read し、`hs-source-dirs` を抽出（なければ `src/`, `app/`, `lib/` を推定）
3. 対象ディレクトリから `**/*.hs` ファイルを Glob で再帰的に列挙
4. `stack.yaml` の resolver、`.cabal` の ghc-options から GHC バージョンを特定
5. Bash `tree` でディレクトリツリーを生成

### Phase 2: モジュール構造解析（quick）

1. 各 `.hs` ファイルの module 宣言を Grep で抽出：
   ```
   パターン: ^module\s+([\w.]+)
   ```
2. `{-# LANGUAGE ... #-}` プラグマを Grep で収集：
   ```
   パターン: \{-#\s*LANGUAGE\s+(\w+)\s*#-\}
   ```
3. `wc -l` で各ファイルの行数をカウント
4. モジュール一覧（名前、ファイルパス、行数、拡張）を生成

### Phase 3: 依存関係解析（standard）

1. import 文を Grep で抽出：
   ```
   パターン: ^import\s+(qualified\s+)?([\w.]+)(\s+as\s+([\w.]+))?(\s+hiding)?(\s*\([^)]*\))?
   ```
2. プロジェクト内モジュール間の依存グラフを構築
3. 循環依存の検出（依存グラフのサイクル探索）
4. 孤立モジュールの検出（他から参照されていないモジュール）
5. 階層分析：トポロジカルソートで依存の深さ・最上位/最下位モジュールを特定
6. 各モジュールの exports を Read で確認し、key_functions を要約

### Phase 4: 品質問題検出（standard）

以下の正規表現パターンで問題を Grep 検出し、severity を分類する：

**安全性問題（主に critical）:**

| 検出項目 | パターン |
|---------|---------|
| `error` 呼び出し | `\berror\s+"` |
| `undefined` 使用 | `=\s*undefined(\s\|$)` |
| `head`/`tail` 部分関数 | `\b(head\|tail)\s` |
| `fromJust` 使用 | `\bfromJust\b` |
| `unsafePerformIO` | `\bunsafePerformIO\b` |

**設計問題（主に moderate）:**
- 恒等関数のスタブ（`f x = x` 形式の未実装関数）
- 命名と実際の振る舞いの乖離
- O(n) 線形探索で O(1) が可能な箇所
- Legacy/V2 重複コード

**スタイル問題（主に minor）:**
- 不要な LANGUAGE プラグマ
- 未使用 import
- デッドコード（未使用の export 関数）

各問題に `Q-{severity初字}{連番}` の ID を付与する（例: Q-C01, Q-M01, Q-L01）。

### Phase 5: テストカバレッジ分析（standard）

1. `test/`, `tests/` ディレクトリから `*Spec.hs`, `*Test.hs` を Glob で列挙
2. import 文からテストフレームワークを検出（hspec, HUnit, tasty, QuickCheck）
3. テスト数をカウント：
   ```
   hspec: \bit\s+" の出現回数
   tasty: testCase/testProperty の出現回数
   ```
4. テスト対象モジュールを import 文から特定
5. 全モジュールとテスト対象モジュールの差分で未テストモジュールを検出
6. プロパティテスト・統合テスト・ゴールデンテストの有無を評価

### Phase 6: 依存パッケージ評価（standard）

1. `.cabal` または `package.yaml` から `build-depends` を Read で抽出
2. 各パッケージの import モジュールを Grep で追跡（例: `aeson` → `import Data.Aeson`）
3. import されていない依存パッケージを「未使用」として検出
4. 各パッケージの使用状況と妥当性を評価コメントとして生成

### Phase 7: ADT詳細解析（full）

1. `data`, `newtype`, `type` 宣言を Grep で検出：
   ```
   data宣言: ^data\s+(\w+)(\s+\w+)*\s*(=|where)
   newtype:  ^newtype\s+(\w+)(\s+\w+)*\s*=
   type:     ^type\s+(\w+)(\s+\w+)*\s*=
   ```
2. コンストラクタとフィールドを Read で抽出
3. `deriving` 句を解析
4. パターンマッチ箇所を Grep で検出（`case ... of` + 関数定義パターン）
5. 各 ADT について：
   - 使用箇所数（usage_count）をカウント
   - ワイルドカード `_` の有無で網羅性を判定
   - 拡張性スコア（0.0-1.0）を算出：コンストラクタ数、パターンマッチ分散度、ワイルドカード率に基づく

### Phase 8: アルゴリズム評価（full）

1. 主要アルゴリズム（探索、ソート、グラフ走査等）を含む関数を特定
2. 計算量を O 記法で分析
3. 正当性を評価（既知のアルゴリズムとの比較）
4. より効率的なアルゴリズムの存在を確認
5. 全ケースがカバーされているかの完全性を評価

### Phase 9: レポート生成（全深度）

Phase 1-8 の結果を統合し、以下の構造で出力：

```yaml
analysis_result:
  metadata:
    project_name: "string"
    analyzed_at: "ISO8601 timestamp"
    analysis_depth: "quick | standard | full"
    total_files: int
    total_modules: int
    total_lines: int
    build_system: "stack | cabal | neither"
    ghc_version: "string | null"

  directory_structure:
    root: "path"
    src_dirs: ["src/"]
    test_dirs: ["test/"]
    tree: "ASCII tree"

  module_graph:          # standard 以上
    modules: [...]
    layers: {...}
    issues:
      circular_dependencies: []
      orphan_modules: []

  quality_issues:        # standard 以上
    summary: { critical: N, moderate: N, minor: N }
    critical: [...]
    moderate: [...]
    minor: [...]

  test_coverage:         # standard 以上
    summary: { total_tests: N, coverage_ratio: float }
    untested_modules: [...]

  ghc_extensions:        # quick 以上
    summary: { total_unique: N, most_common: [...] }
    recommendations: [...]

  dependency_evaluation: # standard 以上
    build_depends: [...]
    issues: [...]

  adt_analysis:          # full のみ
    types: [...]
    extensibility_evaluation: {...}

  algorithm_evaluation:  # full のみ
    evaluations: [...]

  refactoring_proposals: # full のみ
    summary: { high: N, medium: N, low: N }
    proposals: [...]

  overall_assessment: "総合評価コメント"
```

## Examples

### Example 1: Haskell プロジェクトの full 解析

**入力:**
```
project_path: /mnt/e/creative/program/neo_shogi
analysis_depth: full
output_format: yaml
```

**出力（抜粋）:**
```yaml
analysis_result:
  metadata:
    project_name: "neo-shogi"
    analyzed_at: "2026-01-29T10:00:00"
    analysis_depth: full
    total_files: 21
    total_modules: 21
    total_lines: 4500
    build_system: stack
    ghc_version: "9.6.4"

  module_graph:
    modules:
      - name: "Shogi.Core.Types"
        file: "src/Shogi/Core/Types.hs"
        lines: 180
        role: "ゲームの基本型定義（盤面、手、駒）"
        exports: ["Position", "Piece(..)", "Move(..)"]
        dependencies: []
        dependents: ["Shogi.Core.Board", "Shogi.Rules.MoveGen"]

      - name: "Shogi.Engine.Search"
        file: "src/Shogi/Engine/Search.hs"
        lines: 350
        role: "Alpha-Beta探索エンジン"
        dependencies: ["Shogi.Core.Types", "Shogi.Rules.MoveGen"]
        dependents: ["Main"]

    layers:
      description: "3層構造: Core → Rules → Engine"
      graph:
        "Shogi.Engine.Search": ["Shogi.Rules.MoveGen", "Shogi.Core.Types"]
        "Shogi.Rules.MoveGen": ["Shogi.Core.Types", "Shogi.Core.Board"]

    issues:
      circular_dependencies: []
      orphan_modules: []

  quality_issues:
    summary:
      critical: 6
      moderate: 7
      minor: 3

    critical:
      - id: "Q-C01"
        location: "Search.hs:56, Search.hs:141"
        issue: "error呼び出しによるクラッシュ"
        detail: |
          合法手が0件のとき error で例外送出。
          IOアクション内なので Maybe/Either での安全な
          エラーハンドリングが望ましい。

      - id: "Q-C02"
        location: "GameState.hs:getBasePieceId"
        issue: "O(n)線形探索がホットパスに存在"
        detail: |
          PieceRegistryの全エントリを線形探索している。
          逆引きMapを用意すれば O(1) に改善可能。

    moderate:
      - id: "Q-M01"
        location: "MovePatternGenerator.hs:120"
        issue: "AreaMove.maxSteps パラメータが無視されている"
        detail: |
          generateAreaMoves で radius のみ使用し、
          maxSteps による深さ制限が未実装。

  test_coverage:
    summary:
      total_test_files: 5
      total_tests: 98
      modules_with_tests: 12
      modules_without_tests: 9
      coverage_ratio: 0.57

    untested_modules:
      - name: "Shogi.Engine.Search"
        file: "src/Shogi/Engine/Search.hs"
        importance: high
        reason: "探索エンジンのコアロジックだが専用テストがない"

    test_quality:
      has_property_tests: false
      has_integration_tests: true
      has_golden_tests: false
      notes: "QuickCheckによるプロパティテストの導入を推奨"

  refactoring_proposals:
    summary:
      high: 3
      medium: 4
      low: 2

    proposals:
      - id: "R01"
        priority: high
        target: "GameState.hs getBasePieceId"
        proposal: |
          PieceRegistryに逆引きMap（promotedId → baseId）を持たせる。
          線形探索を O(1) に改善。頻繁に呼ばれるパスなので効果大。

      - id: "R02"
        priority: high
        target: "Search.hs"
        proposal: |
          error 呼び出しを Maybe/Either に置換。
          合法手0件の場合は詰み/ステイルメイト判定に分岐。

  overall_assessment: |
    21モジュール・4500行のHaskellプロジェクト。
    3層の依存構造は整理されているが、安全性問題（error使用6件）と
    テストカバレッジ（57%）に改善の余地あり。
    ADTの拡張性は中程度で、大局将棋対応にはリファクタリングが必要。

```

### Example 2: quick 解析（構造把握のみ）

**入力:**
```
project_path: /home/user/my-haskell-project
analysis_depth: quick
```

**出力（抜粋）:**
```yaml
analysis_result:
  metadata:
    project_name: "my-haskell-project"
    analyzed_at: "2026-01-29T15:00:00"
    analysis_depth: quick
    total_files: 8
    total_modules: 8
    total_lines: 1200
    build_system: cabal
    ghc_version: "9.8.1"

  directory_structure:
    root: "/home/user/my-haskell-project"
    src_dirs: ["src/"]
    test_dirs: ["test/"]
    tree: |
      src/
      ├── Lib.hs
      ├── Types.hs
      ├── Parser/
      │   ├── Core.hs
      │   └── Utils.hs
      └── Eval/
          ├── Engine.hs
          └── Builtins.hs
      app/
      └── Main.hs
      test/
      └── Spec.hs

  ghc_extensions:
    summary:
      total_unique: 5
      most_common: ["OverloadedStrings", "DeriveGeneric"]
    recommendations:
      - extension: "OverloadedStrings"
        usage_count: 4
        note: "Parser モジュール群で適切に使用されている"
      - extension: "TemplateHaskell"
        usage_count: 1
        note: "Types.hs で宣言されているが使用箇所が確認できない。削除を検討"
```

## Guidelines

1. **ビルド不要**: 正規表現ベースの静的解析のため、GHC やビルドツールのインストールは不要。ソースファイルが読めれば解析可能
2. **段階的な深度**: `quick` で構造把握 → `standard` で品質評価 → `full` で詳細解析。ユーザーの目的に応じて適切な深度を選択する
3. **severity の厳密な分類**: critical はクラッシュ・データ破損の原因になるもの、moderate は機能不全・性能問題、minor はスタイル・冗長性。根拠を detail に記述する
4. **priority の具体性**: リファクタリング提案は「何をどう変更すべきか」まで具体的に書く。対象ファイル・関数名・変更内容を明記する
5. **false positive の抑制**: コメント内やドキュメント文字列内の出現は除外する。`error` がログメッセージの文字列内に含まれるケース等に注意
6. **プロジェクト非依存**: 特定プロジェクト（neo_shogi 等）に依存する判定ロジックを含めない。検出パターンは Haskell の一般的な構文に基づく
7. **正規表現の限界を認識**: 複数行にまたがる宣言や Template Haskell 生成コードは検出漏れの可能性がある。制限事項として明記する
8. **大規模プロジェクト対応**: モジュール数が多い場合は `target_modules` で対象を絞り込む。Grep 検索は並列実行で効率化する
9. **テスト品質の評価**: テスト数だけでなく、プロパティテスト・統合テスト・ゴールデンテストの有無も評価する。量と質の両面から判断する
10. **GHC 拡張の評価**: 宣言されているが実際に使われていない拡張は削除を推奨する。ただし、ヒューリスティック判定のため確実でない場合は「確認推奨」と記載する
