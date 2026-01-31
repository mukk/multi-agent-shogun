# neo_shogi 駒定義のData-Driven Design移行設計書

> **Version**: 1.0.0
> **Author**: ashigaru2 (Senior Software Engineer persona)
> **Date**: 2026-01-29
> **Status**: Draft

## 1. Executive Summary

本設計書は、neo_shogiプロジェクトの駒定義をハードコードされたADT（代数的データ型）から、データ駆動設計（Data-Driven Design）へ移行するための設計を記述する。

**目標**: 現在の8種類から209種類（大局将棋）の駒に対応可能なアーキテクチャへの移行

---

## 2. 現状分析

### 2.1 現在の PieceType 定義

**ファイル**: `src/Shogi/Core/Types.hs:71-80`

```haskell
data PieceType
  = King
  | GoldGeneral
  | SilverGeneral
  | Knight
  | Lance
  | Pawn
  | Rook
  | Bishop
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Hashable)
```

**問題点**:
- 駒種がコンパイル時に固定される
- 新しい駒の追加には型定義の変更と再コンパイルが必要
- `Enum`/`Bounded` の自動導出が静的な駒数に依存

### 2.2 影響を受けるファイル一覧

| ファイル | 影響度 | 影響内容 |
|----------|--------|----------|
| `Core/Types.hs` | 致命的 | PieceType定義、Hand型 |
| `Core/Piece.hs` | 高 | pieceValue, canPromote, pieceDirections, pieceIsRanged |
| `Core/Board.hs` | 中 | findKing (Kingとの比較) |
| `Rules/Moves.hs` | 高 | mustPromoteOnDrop, isNifuViolation |
| `Rules/Validation.hs` | 低 | 間接的にKingを参照 |
| `Rules/GameState.hs` | 中 | applyDropMove |
| `Engine/Evaluation.hs` | 高 | positionBonus, advancementBonus, handPieceValue |
| `Variants/MiniShogi.hs` | 中 | miniShogiPieceSet |
| `Utils/Zobrist.hs` | 高 | ハッシュ計算（駒種×位置） |

### 2.3 パターンマッチ箇所の詳細

#### 2.3.1 pieceValue (Piece.hs:14-28)
```haskell
pieceValue :: PieceType -> Promotion -> Int
pieceValue King _ = 10000
pieceValue GoldGeneral _ = 600
-- ... 8種類すべてに対するパターンマッチ
```

#### 2.3.2 canPromote (Piece.hs:31-35)
```haskell
canPromote :: PieceType -> Promotion -> Bool
canPromote King _ = False
canPromote GoldGeneral _ = False
canPromote _ Promoted = False
canPromote _ Unpromoted = True
```

#### 2.3.3 pieceDirections (Piece.hs:56-72)
```haskell
pieceDirections :: Piece -> [(Int, Int)]
pieceDirections piece =
  case (pieceType piece, piecePromoted piece) of
    (King, _) -> kingDirs
    (GoldGeneral, _) -> goldDirs player
    -- ... 全14パターン（8種×成/不成）
```

#### 2.3.4 pieceIsRanged (Piece.hs:99-105)
```haskell
pieceIsRanged :: Piece -> Bool
pieceIsRanged piece =
  case (pieceType piece, piecePromoted piece) of
    (Lance, Unpromoted) -> True
    (Rook, _) -> True
    (Bishop, _) -> True
    _ -> False
```

#### 2.3.5 Moves.hs での駒種別処理 (52-54, 68, 155-158)
- `mustPromoteOnDrop`: Pawn, Lance, Knight の移動不能チェック
- `isNifuViolation`: Pawn の二歩チェック
- 強制成りチェック

#### 2.3.6 Evaluation.hs での駒種別ボーナス (75-80)
```haskell
case pieceType' of
  Pawn -> advancement * multiplier
  Lance -> advancement * multiplier
  Knight -> advancement * multiplier
  SilverGeneral -> advancement * multiplier `div` 2
  _ -> 0
```

---

## 3. 新設計案

### 3.1 設計原則

1. **Data-Driven**: 駒定義を外部YAMLファイルから読み込む
2. **Type Safety**: Haskellの型システムを活用し、不正な状態を防ぐ
3. **Extensibility**: 新しい駒種の追加にコード変更を不要とする
4. **Performance**: 実行時ルックアップのオーバーヘッドを最小化

### 3.2 新しい型定義

#### 3.2.1 PieceId (駒識別子)

```haskell
-- | 駒の一意識別子（型安全なnewtype）
newtype PieceId = PieceId { unPieceId :: Int }
  deriving (Eq, Ord, Show, Hashable, FromJSON, ToJSON)

-- | 特別な駒ID（King検索用）
pattern KingId :: PieceId
pattern KingId = PieceId 0
```

**選定理由**:
- `Int`: 高速な比較・ハッシュ計算
- `newtype`: 型安全性（他のIntと混同しない）
- `pattern synonym`: Kingなど特別な駒への名前付きアクセス

#### 3.2.2 PieceDefinition (駒定義)

```haskell
-- | 駒の静的定義
data PieceDefinition = PieceDefinition
  { pdId           :: !PieceId
  , pdName         :: !Text           -- 英語名 "King"
  , pdNameJa       :: !Text           -- 日本語名 "王将"
  , pdNameKanji    :: !Text           -- 駒文字 "王"
  , pdValue        :: !Int            -- 評価値 (centipawns)
  , pdCanPromote   :: !Bool           -- 成り可否
  , pdPromotesTo   :: !(Maybe PieceId) -- 成り先の駒ID
  , pdMovement     :: !MovementPattern -- 移動パターン
  , pdIsRanged     :: !Bool           -- 走り駒か
  , pdSpecialRules :: ![SpecialRule]  -- 特殊ルール
  } deriving (Eq, Show, Generic)

instance FromJSON PieceDefinition
instance ToJSON PieceDefinition
```

#### 3.2.3 MovementPattern (移動パターンDSL)

```haskell
-- | 移動方向
data Direction
  = N | NE | E | SE | S | SW | W | NW  -- 8方向
  | KnightNE | KnightNW                 -- 桂馬跳び
  | Custom Int Int                      -- カスタム (deltaRow, deltaCol)
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | 移動パターン
data MovementPattern = MovementPattern
  { mpSteps     :: ![(Direction, StepType)]  -- 方向とステップタイプ
  , mpJumps     :: ![(Int, Int)]             -- ジャンプ移動 (桂馬等)
  , mpAreaMoves :: ![AreaMove]               -- エリア移動 (獅子等)
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | ステップタイプ
data StepType
  = Single        -- 1マスのみ
  | Ranging       -- 走り（飛車・角行等）
  | Limited Int   -- 制限付き走り
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | エリア移動（大局将棋の獅子等）
data AreaMove = AreaMove
  { amRadius    :: !Int    -- 移動範囲の半径
  , amMaxSteps  :: !Int    -- 最大ステップ数
  , amCanReturn :: !Bool   -- 元の位置に戻れるか
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)
```

#### 3.2.4 SpecialRule (特殊ルール)

```haskell
-- | 特殊ルール
data SpecialRule
  = NifuProhibited         -- 二歩禁止（歩兵）
  | CannotDropInZone Int   -- 段制限（桂馬: 2段、香車: 1段）
  | MustPromoteInZone Int  -- 強制成り
  | CanCaptureWithoutMove  -- 居食い（獅子）
  | Igui                   -- 居喰い
  | Royal                  -- 玉将（取られると負け）
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
```

#### 3.2.5 PieceRegistry (駒レジストリ)

```haskell
-- | 駒定義のレジストリ（実行時参照）
data PieceRegistry = PieceRegistry
  { prPieces    :: !(IntMap PieceDefinition)  -- ID -> 定義
  , prByName    :: !(Map Text PieceId)        -- 名前 -> ID
  , prKingId    :: !PieceId                   -- 王将のID（高速参照用）
  } deriving (Eq, Show)

-- | 駒定義の取得（O(1)）
lookupPiece :: PieceRegistry -> PieceId -> Maybe PieceDefinition
lookupPiece reg pid = IntMap.lookup (unPieceId pid) (prPieces reg)

-- | 駒定義の取得（エラー時はデフォルト）
getPiece :: PieceRegistry -> PieceId -> PieceDefinition
getPiece reg pid = fromMaybe defaultPiece (lookupPiece reg pid)
```

### 3.3 新しい Piece 型

```haskell
-- | 盤上の駒
data Piece = Piece
  { pieceOwner    :: !Player
  , pieceId       :: !PieceId      -- PieceType から変更
  , piecePromoted :: !Bool         -- Promotion型からBoolへ簡略化
  } deriving (Eq, Ord, Show, Generic, Hashable)
```

### 3.4 外部ファイルフォーマット（YAML）

```yaml
# config/pieces/standard_shogi.yaml
metadata:
  name: "Standard Shogi"
  version: "1.0"
  piece_count: 8

pieces:
  - id: 0
    name: "King"
    name_ja: "王将"
    name_kanji: "王"
    value: 10000
    can_promote: false
    promotes_to: null
    movement:
      steps:
        - direction: N
          type: single
        - direction: NE
          type: single
        # ... 8方向
      jumps: []
      area_moves: []
    is_ranged: false
    special_rules:
      - Royal

  - id: 1
    name: "GoldGeneral"
    name_ja: "金将"
    name_kanji: "金"
    value: 600
    can_promote: false
    promotes_to: null
    movement:
      steps:
        - direction: N
          type: single
        - direction: NE
          type: single
        - direction: E
          type: single
        - direction: W
          type: single
        - direction: SE
          type: single  # 後ろ斜めは不可
        - direction: S
          type: single
      jumps: []
      area_moves: []
    is_ranged: false
    special_rules: []

  - id: 6
    name: "Rook"
    name_ja: "飛車"
    name_kanji: "飛"
    value: 1000
    can_promote: true
    promotes_to: 14  # DragonKing
    movement:
      steps:
        - direction: N
          type: ranging
        - direction: E
          type: ranging
        - direction: S
          type: ranging
        - direction: W
          type: ranging
      jumps: []
      area_moves: []
    is_ranged: true
    special_rules: []

  # 成り駒の定義
  - id: 14
    name: "DragonKing"
    name_ja: "龍王"
    name_kanji: "龍"
    value: 1200
    can_promote: false
    promotes_to: null
    movement:
      steps:
        - direction: N
          type: ranging
        - direction: E
          type: ranging
        - direction: S
          type: ranging
        - direction: W
          type: ranging
        - direction: NE
          type: single
        - direction: NW
          type: single
        - direction: SE
          type: single
        - direction: SW
          type: single
      jumps: []
      area_moves: []
    is_ranged: true
    special_rules: []
```

### 3.5 大局将棋の駒サンプル

```yaml
# config/pieces/taikyoku_shogi_sample.yaml
# 大局将棋の一部駒サンプル（全209種類のうち抜粋）

pieces:
  # 獅子 (Lion) - 特殊な2手移動
  - id: 100
    name: "Lion"
    name_ja: "獅子"
    name_kanji: "獅"
    value: 2500
    can_promote: true
    promotes_to: 101
    movement:
      steps: []
      jumps: []
      area_moves:
        - radius: 2
          max_steps: 2
          can_return: true  # 元の位置に戻れる（じっと）
    is_ranged: false
    special_rules:
      - Igui  # 居喰い可能

  # 鳳凰 (Phoenix) - 斜め2マス跳び + 十字1マス
  - id: 102
    name: "Phoenix"
    name_ja: "鳳凰"
    name_kanji: "鳳"
    value: 800
    can_promote: true
    promotes_to: 103
    movement:
      steps:
        - direction: N
          type: single
        - direction: E
          type: single
        - direction: S
          type: single
        - direction: W
          type: single
      jumps:
        - [2, 2]   # 右上2マス跳び
        - [2, -2]  # 左上2マス跳び
        - [-2, 2]  # 右下2マス跳び
        - [-2, -2] # 左下2マス跳び
      area_moves: []
    is_ranged: false
    special_rules: []

  # 麒麟 (Kirin) - 十字2マス跳び + 斜め1マス
  - id: 104
    name: "Kirin"
    name_ja: "麒麟"
    name_kanji: "麒"
    value: 750
    can_promote: true
    promotes_to: 100  # 獅子に成る
    movement:
      steps:
        - direction: NE
          type: single
        - direction: NW
          type: single
        - direction: SE
          type: single
        - direction: SW
          type: single
      jumps:
        - [2, 0]   # 前2マス跳び
        - [-2, 0]  # 後2マス跳び
        - [0, 2]   # 右2マス跳び
        - [0, -2]  # 左2マス跳び
      area_moves: []
    is_ranged: false
    special_rules: []

  # 奔王 (Free King) - 8方向無制限走り
  - id: 110
    name: "FreeKing"
    name_ja: "奔王"
    name_kanji: "奔"
    value: 3000
    can_promote: false
    promotes_to: null
    movement:
      steps:
        - direction: N
          type: ranging
        - direction: NE
          type: ranging
        - direction: E
          type: ranging
        - direction: SE
          type: ranging
        - direction: S
          type: ranging
        - direction: SW
          type: ranging
        - direction: W
          type: ranging
        - direction: NW
          type: ranging
      jumps: []
      area_moves: []
    is_ranged: true
    special_rules: []
```

---

## 4. 移行計画

### 4.1 段階的移行のステップ

#### Phase 1: 基盤整備 (Week 1-2)
1. 新しい型定義の追加（既存コードと並存）
2. YAMLパーサーの実装
3. PieceRegistryの実装
4. 標準将棋の駒定義YAMLファイル作成

#### Phase 2: コア機能の移行 (Week 3-4)
1. `Types.hs`: Hand型をPieceId対応に変更
2. `Piece.hs`: 関数群をPieceRegistry参照に変更
3. `Board.hs`: findKingをPieceRegistry経由に変更
4. 単体テストの追加

#### Phase 3: ルール層の移行 (Week 5-6)
1. `Moves.hs`: 駒種別ロジックをSpecialRule参照に変更
2. `Validation.hs`: King判定をPieceRegistry経由に変更
3. `GameState.hs`: ドロップロジックの更新
4. 統合テストの追加

#### Phase 4: エンジン層の移行 (Week 7-8)
1. `Evaluation.hs`: 評価関数をPieceDefinition.pdValue参照に変更
2. `Zobrist.hs`: 動的な駒数対応
3. パフォーマンス測定・最適化

#### Phase 5: バリアント対応 (Week 9-10)
1. `Variants/StandardShogi.hs`: 9x9将棋の追加
2. `Variants/ChuShogi.hs`: 中将棋の追加
3. バリアント選択機能の実装

### 4.2 互換性維持の方針

```haskell
-- 移行期間中の互換性レイヤー
module Shogi.Compat where

-- 旧PieceTypeから新PieceIdへの変換
legacyPieceTypeToId :: LegacyPieceType -> PieceId
legacyPieceTypeToId LegacyKing = PieceId 0
legacyPieceTypeToId LegacyGoldGeneral = PieceId 1
-- ...

-- 旧API互換の関数（非推奨マーク付き）
{-# DEPRECATED pieceValue "Use PieceDefinition.pdValue instead" #-}
pieceValue :: PieceRegistry -> PieceId -> Promotion -> Int
pieceValue reg pid promo =
  case lookupPiece reg pid of
    Just def -> pdValue def
    Nothing -> 0
```

### 4.3 テスト戦略

#### 4.3.1 単体テスト
```haskell
-- test/Shogi/Core/PieceRegistrySpec.hs
spec :: Spec
spec = describe "PieceRegistry" $ do
  it "loads standard shogi pieces from YAML" $ do
    reg <- loadPieceRegistry "config/pieces/standard_shogi.yaml"
    length (prPieces reg) `shouldBe` 14  -- 8種 + 6成り駒

  it "finds King by special pattern" $ do
    reg <- loadPieceRegistry "config/pieces/standard_shogi.yaml"
    let kingDef = getPiece reg KingId
    pdName kingDef `shouldBe` "King"
    pdValue kingDef `shouldBe` 10000
```

#### 4.3.2 プロパティベーステスト
```haskell
-- QuickCheck properties
prop_movementPatternConsistent :: PieceDefinition -> Bool
prop_movementPatternConsistent def =
  (pdIsRanged def) == hasRangingSteps (pdMovement def)

prop_promotionChainValid :: PieceRegistry -> PieceId -> Bool
prop_promotionChainValid reg pid =
  case lookupPiece reg pid of
    Nothing -> True
    Just def -> case pdPromotesTo def of
      Nothing -> True
      Just promId -> isJust (lookupPiece reg promId)
```

#### 4.3.3 回帰テスト
- 既存のMini将棋テストケースが全てパスすること
- 手筋・定跡データベースとの照合

---

## 5. パフォーマンス考慮

### 5.1 最適化ポイント

1. **IntMap使用**: PieceIdがIntなのでIntMapで高速ルックアップ
2. **Strict評価**: `!` 注釈で遅延評価のオーバーヘッド削減
3. **インライン化**: 頻出関数に `{-# INLINE #-}` プラグマ
4. **キャッシュ**: 移動パターンの事前計算

### 5.2 メモリ使用量

| 項目 | 現行 | 新設計 |
|------|------|--------|
| PieceType | 1バイト (Enum) | 4バイト (Int) |
| Piece | 3バイト | 6バイト |
| PieceRegistry | N/A | ~10KB (209駒) |

**結論**: メモリ増加は許容範囲内

---

## 6. 将来の拡張

### 6.1 対応予定バリアント

| バリアント | 駒数 | 盤面 | 優先度 |
|-----------|------|------|--------|
| 標準将棋 | 8 | 9x9 | P0 |
| Mini将棋 | 6 | 5x5 | P0 (既存) |
| 中将棋 | 46 | 12x12 | P1 |
| 大将棋 | 65 | 15x15 | P2 |
| 大大将棋 | 96 | 17x17 | P2 |
| 摩訶大大将棋 | 100 | 19x19 | P3 |
| 泰将棋 | 111 | 25x25 | P3 |
| 大局将棋 | 209 | 36x36 | P3 |

### 6.2 追加機能候補

- GUIでの駒画像カスタマイズ
- 駒定義エディタ（Webベース）
- 棋譜形式の拡張（USI/CSA対応）

---

## 7. リスクと対策

| リスク | 影響度 | 対策 |
|--------|--------|------|
| パフォーマンス低下 | 中 | ベンチマーク駆動開発、プロファイリング |
| YAML構文エラー | 低 | スキーマバリデーション、テスト自動化 |
| 移行中のバグ | 高 | 互換性レイヤー、段階的リリース |
| 大局将棋の複雑なルール | 高 | SpecialRuleの拡張性確保 |

---

## 8. 結論

本設計により、neo_shogiは8種類から209種類への駒拡張に対応可能となる。Haskellの型システムを活かしつつ、データ駆動設計により柔軟性を確保する。段階的移行により、既存機能を維持しながら安全に新アーキテクチャへ移行できる。

---

## Appendix A: 完全な標準将棋駒定義

```yaml
# 標準将棋の8種類（成り駒6種類を含む計14エントリ）
pieces:
  # === 基本駒 ===
  - id: 0
    name: King
    name_ja: 王将
    name_kanji: 王
    value: 10000
    can_promote: false
    movement:
      steps: [{direction: N, type: single}, {direction: NE, type: single},
              {direction: E, type: single}, {direction: SE, type: single},
              {direction: S, type: single}, {direction: SW, type: single},
              {direction: W, type: single}, {direction: NW, type: single}]
    special_rules: [Royal]

  - id: 1
    name: GoldGeneral
    name_ja: 金将
    name_kanji: 金
    value: 600
    can_promote: false
    movement:
      steps: [{direction: N, type: single}, {direction: NE, type: single},
              {direction: E, type: single}, {direction: S, type: single},
              {direction: W, type: single}, {direction: NW, type: single}]

  - id: 2
    name: SilverGeneral
    name_ja: 銀将
    name_kanji: 銀
    value: 500
    can_promote: true
    promotes_to: 8
    movement:
      steps: [{direction: N, type: single}, {direction: NE, type: single},
              {direction: SE, type: single}, {direction: SW, type: single},
              {direction: NW, type: single}]

  - id: 3
    name: Knight
    name_ja: 桂馬
    name_kanji: 桂
    value: 400
    can_promote: true
    promotes_to: 9
    movement:
      jumps: [[2, 1], [2, -1]]  # 前2マス・横1マス
    special_rules: [CannotDropInZone 2]

  - id: 4
    name: Lance
    name_ja: 香車
    name_kanji: 香
    value: 400
    can_promote: true
    promotes_to: 10
    movement:
      steps: [{direction: N, type: ranging}]
    is_ranged: true
    special_rules: [CannotDropInZone 1]

  - id: 5
    name: Pawn
    name_ja: 歩兵
    name_kanji: 歩
    value: 100
    can_promote: true
    promotes_to: 11
    movement:
      steps: [{direction: N, type: single}]
    special_rules: [NifuProhibited, CannotDropInZone 1]

  - id: 6
    name: Rook
    name_ja: 飛車
    name_kanji: 飛
    value: 1000
    can_promote: true
    promotes_to: 12
    movement:
      steps: [{direction: N, type: ranging}, {direction: E, type: ranging},
              {direction: S, type: ranging}, {direction: W, type: ranging}]
    is_ranged: true

  - id: 7
    name: Bishop
    name_ja: 角行
    name_kanji: 角
    value: 900
    can_promote: true
    promotes_to: 13
    movement:
      steps: [{direction: NE, type: ranging}, {direction: SE, type: ranging},
              {direction: SW, type: ranging}, {direction: NW, type: ranging}]
    is_ranged: true

  # === 成り駒 ===
  - id: 8
    name: PromotedSilver
    name_ja: 成銀
    name_kanji: 全
    value: 600
    can_promote: false
    # 金将と同じ動き

  - id: 9
    name: PromotedKnight
    name_ja: 成桂
    name_kanji: 圭
    value: 600
    can_promote: false

  - id: 10
    name: PromotedLance
    name_ja: 成香
    name_kanji: 杏
    value: 600
    can_promote: false

  - id: 11
    name: Tokin
    name_ja: と金
    name_kanji: と
    value: 600
    can_promote: false

  - id: 12
    name: DragonKing
    name_ja: 龍王
    name_kanji: 龍
    value: 1200
    can_promote: false
    movement:
      steps: [{direction: N, type: ranging}, {direction: E, type: ranging},
              {direction: S, type: ranging}, {direction: W, type: ranging},
              {direction: NE, type: single}, {direction: SE, type: single},
              {direction: SW, type: single}, {direction: NW, type: single}]
    is_ranged: true

  - id: 13
    name: DragonHorse
    name_ja: 龍馬
    name_kanji: 馬
    value: 1100
    can_promote: false
    movement:
      steps: [{direction: NE, type: ranging}, {direction: SE, type: ranging},
              {direction: SW, type: ranging}, {direction: NW, type: ranging},
              {direction: N, type: single}, {direction: E, type: single},
              {direction: S, type: single}, {direction: W, type: single}]
    is_ranged: true
```

---

**Document End**
