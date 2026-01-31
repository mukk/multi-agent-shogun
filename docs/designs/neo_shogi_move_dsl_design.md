# neo_shogi 移動ルール DSL 設計書

> **Version**: 1.0.0
> **Date**: 2026-01-29
> **Author**: ashigaru3 (as Senior Software Engineer)
> **Status**: Draft

## 1. 概要

本設計書は、neo_shogiプロジェクトの移動ルールをDSL（Domain Specific Language）で表現するためのリファクタリング設計を定義する。

### 1.1 目的

- 現在のハードコードされた移動パターンを、データ駆動型に移行
- 大局将棋（約209種の駒）、中将棋（約46種）、大将棋（約130種）等への対応
- 外部ファイル（YAML/JSON）での駒定義を可能にする

### 1.2 スコープ

- 移動パターンのDSL設計
- Move型の拡張
- 外部ファイル定義のフォーマット
- 実装は別タスクとする

---

## 2. 現状分析

### 2.1 現在の移動パターン定義

**ファイル**: `src/Shogi/Core/Piece.hs:56-96`

```haskell
pieceDirections :: Piece -> [(Int, Int)]
pieceDirections piece =
  case (pieceType piece, piecePromoted piece) of
    (King, _) -> kingDirs
    (GoldGeneral, _) -> goldDirs player
    (SilverGeneral, Unpromoted) -> silverDirs player
    -- ... パターンマッチが続く
```

**問題点**:

| 項目 | 現状 | 問題 |
|------|------|------|
| 駒種定義 | `PieceType` Enum (8種) | 拡張に再コンパイル必要 |
| 移動パターン | パターンマッチ | 駒追加ごとにコード修正 |
| 走り駒判定 | `pieceIsRanged :: Piece -> Bool` | 距離制限に非対応 |
| 方向定義 | `(Int, Int)` タプル | 複合移動表現不可 |

### 2.2 現在の限界点

1. **スケーラビリティ**: 200種以上の駒をパターンマッチで定義は非現実的
2. **複合移動**: ライオンの「居食い」（取って戻る）が表現不可
3. **条件付き移動**: 初期位置のみ有効な移動（チェスのポーン2マス等）に非対応
4. **跳び越え**: 鷹・鳳凰の「飛び越え移動」に非対応
5. **距離制限**: 「最大Nマス」の走りに非対応

---

## 3. 移動パターン DSL 設計

### 3.1 基本方針

```
Haskellの型システムを最大限活用し、
不正な移動パターンをコンパイル時に検出できる設計とする。
```

### 3.2 方向（Direction）の表現

```haskell
-- | 相対方向（プレイヤー視点）
data RelativeDir
  = Forward        -- 前
  | Backward       -- 後
  | Left           -- 左
  | Right          -- 右
  | ForwardLeft    -- 左前
  | ForwardRight   -- 右前
  | BackwardLeft   -- 左後
  | BackwardRight  -- 右後
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | 絶対座標オフセット（盤面基準）
data Offset = Offset
  { offsetRow :: !Int
  , offsetCol :: !Int
  } deriving (Eq, Ord, Show)

-- | 方向の表現（相対または絶対）
data Direction
  = Relative RelativeDir
  | Absolute Offset
  deriving (Eq, Show)

-- | 相対方向をプレイヤーに応じてオフセットに変換
resolveDirection :: Player -> RelativeDir -> Offset
resolveDirection Sente Forward      = Offset (-1)   0
resolveDirection Sente ForwardLeft  = Offset (-1) (-1)
resolveDirection Sente ForwardRight = Offset (-1)   1
resolveDirection Sente Left         = Offset   0  (-1)
resolveDirection Sente Right        = Offset   0    1
resolveDirection Sente Backward     = Offset   1    0
resolveDirection Sente BackwardLeft = Offset   1  (-1)
resolveDirection Sente BackwardRight= Offset   1    1
resolveDirection Gote dir = negateOffset (resolveDirection Sente dir)

negateOffset :: Offset -> Offset
negateOffset (Offset r c) = Offset (-r) (-c)
```

### 3.3 移動パターン型（MovePattern）

```haskell
-- | 移動パターンのDSL
data MovePattern
  -- 基本移動
  = Step [Direction]
      -- ^ 1マス移動（金将、銀将等）

  | Slide [Direction]
      -- ^ 無制限の走り（飛車、角行等）

  | SlideN Int [Direction]
      -- ^ 最大Nマスの走り（奔王の前方3マス等）

  -- 特殊移動
  | Jump [Offset]
      -- ^ 指定位置への跳躍（桂馬等）
      -- 途中の駒を無視

  | JumpOver Direction
      -- ^ 1マス飛び越えて着地（鳳凰の斜め等）
      -- 隣接マスの駒を飛び越える

  | Area Int
      -- ^ Nマス以内の任意のマスへ移動（獅子の基本移動）

  | Igui
      -- ^ 居食い：隣接駒を取って元の位置に戻る
      -- 獅子の特殊能力

  | RangeCapture Direction Int
      -- ^ 遠隔取り：Nマス先の駒を取る（移動なし）
      -- 大局将棋の一部の駒

  -- 複合・条件付き
  | Conditional Condition MovePattern
      -- ^ 条件付き移動

  | Composite [MovePattern]
      -- ^ 複数パターンの合成（竜王 = 飛車 + 王）

  deriving (Eq, Show)

-- | 移動条件
data Condition
  = FirstMove          -- 初回移動のみ（チェスのポーン2マス）
  | NotFirstMove       -- 2回目以降のみ
  | FromZone [Int]     -- 特定の段からのみ
  | HasCaptured        -- 駒を取った後のみ
  | InPromotionZone    -- 敵陣内のみ
  deriving (Eq, Show)
```

### 3.4 獅子（ライオン）の移動表現

大局将棋・中将棋の獅子は特に複雑な移動を持つ：

```haskell
-- | 獅子の移動パターン定義例
lionMovePattern :: MovePattern
lionMovePattern = Composite
  [ Area 2           -- 2マス以内の任意のマスへ移動
  , Igui             -- 居食い（隣接駒を取って戻る）
  , LionDouble       -- 2回移動（後述）
  ]

-- | 獅子の2回移動を表現する専用型
data LionDoubleMove = LionDoubleMove
  { firstStep  :: Offset    -- 1歩目
  , secondStep :: Maybe Offset  -- 2歩目（Nothingなら1歩で停止）
  , captureAt  :: [Offset]  -- 取る駒の位置（0, 1, or 2）
  }
```

### 3.5 大局将棋の特殊駒サンプル

```haskell
-- | 奔王（Honoh）: 縦横斜め無制限 + 前方のみ3マス制限なし
honorMovePattern :: MovePattern
honorMovePattern = Composite
  [ Slide [Relative Forward, Relative Backward,
           Relative Left, Relative Right]
  , Slide [Relative ForwardLeft, Relative ForwardRight,
           Relative BackwardLeft, Relative BackwardRight]
  ]

-- | 鷹（Taka）: 斜め走り + 前後1マス + 左右跳び越え
hawkMovePattern :: MovePattern
hawkMovePattern = Composite
  [ Slide [Relative ForwardLeft, Relative ForwardRight,
           Relative BackwardLeft, Relative BackwardRight]
  , Step [Relative Forward, Relative Backward]
  , JumpOver (Relative Left)
  , JumpOver (Relative Right)
  ]

-- | 鳳凰（Hoo）: 前後左右1マス + 斜め跳び越え
phoenixMovePattern :: MovePattern
phoenixMovePattern = Composite
  [ Step [Relative Forward, Relative Backward,
          Relative Left, Relative Right]
  , JumpOver (Relative ForwardLeft)
  , JumpOver (Relative ForwardRight)
  , JumpOver (Relative BackwardLeft)
  , JumpOver (Relative BackwardRight)
  ]
```

---

## 4. Move型の拡張設計

### 4.1 現在のMove型

```haskell
-- 現状（src/Shogi/Core/Types.hs:116-126）
data Move
  = NormalMove
      { moveFrom    :: !Pos
      , moveTo      :: !Pos
      , movePromote :: !Bool
      }
  | DropMove
      { dropPiece   :: !PieceType
      , dropTo      :: !Pos
      }
```

### 4.2 拡張後のMove型

```haskell
data Move
  -- 基本移動（変更なし）
  = NormalMove
      { moveFrom    :: !Pos
      , moveTo      :: !Pos
      , movePromote :: !Bool
      }

  -- 駒打ち（変更なし）
  | DropMove
      { dropPiece   :: !PieceId  -- PieceType → PieceId に変更
      , dropTo      :: !Pos
      }

  -- 獅子型2段階移動（新規）
  | LionMove
      { lionFrom    :: !Pos
      , lionVia     :: !(Maybe Pos)  -- 経由地点（Nothingなら1手のみ）
      , lionTo      :: !Pos
      , lionCapture :: ![Pos]        -- 取った駒の位置（0-2個）
      }

  -- 居食い（新規）
  | IguiMove
      { iguiFrom    :: !Pos
      , iguiCapture :: !Pos          -- 取る駒の位置
      }

  -- 遠隔取り（新規）- 大局将棋用
  | RangeCaptureMove
      { rcFrom      :: !Pos
      , rcCapture   :: !Pos          -- 取る駒の位置（移動なし）
      }

  deriving (Eq, Ord, Show, Generic, Hashable)
```

### 4.3 履歴への記録

```haskell
-- | 棋譜記録用の詳細移動情報
data MoveRecord = MoveRecord
  { mrMove       :: !Move
  , mrPiece      :: !PieceId
  , mrCaptured   :: !(Maybe PieceId)
  , mrWasPromoted:: !Bool
  , mrTimestamp  :: !(Maybe UTCTime)
  , mrAnnotation :: !(Maybe Text)
  } deriving (Eq, Show)
```

---

## 5. 駒定義（PieceDefinition）

### 5.1 Haskell型定義

```haskell
-- | 駒のユニークID（外部ファイルで定義）
newtype PieceId = PieceId { unPieceId :: Text }
  deriving (Eq, Ord, Show, Hashable)

-- | 駒定義
data PieceDefinition = PieceDefinition
  { pdId          :: !PieceId
  , pdName        :: !Text              -- 日本語名
  , pdNameEn      :: !Text              -- 英語名
  , pdAbbrev      :: !Text              -- 略称（2文字）
  , pdMoves       :: ![MovePattern]     -- 移動パターン
  , pdPromotesTo  :: !(Maybe PieceId)   -- 成り先
  , pdCanPromote  :: !Bool              -- 成れるか
  , pdMustPromote :: !(Maybe PromoteCondition)  -- 強制成り条件
  , pdValue       :: !Int               -- 評価値
  , pdIsRoyal     :: !Bool              -- 玉将か
  } deriving (Eq, Show)

-- | 強制成り条件
data PromoteCondition
  = PromoteAtRow Int      -- 特定の段で強制成り
  | PromoteAtRows [Int]   -- 複数段で強制成り
  | PromoteIfImmobile     -- 動けなくなる場合に強制成り
  deriving (Eq, Show)
```

### 5.2 駒セット定義

```haskell
-- | ゲームバリアントの駒セット
data PieceSet = PieceSet
  { psName       :: !Text
  , psPieces     :: !(Map PieceId PieceDefinition)
  , psInitialPos :: ![(Pos, PieceId, Player)]  -- 初期配置
  } deriving (Eq, Show)

-- | 駒セットをロード
loadPieceSet :: FilePath -> IO (Either ParseError PieceSet)
```

---

## 6. 外部ファイル定義

### 6.1 YAMLフォーマット

```yaml
# pieces/standard_shogi.yaml
name: "標準将棋"
name_en: "Standard Shogi"

pieces:
  # 玉将
  - id: king
    name: 玉将
    name_en: King
    abbrev: 玉
    is_royal: true
    value: 10000
    moves:
      - type: step
        directions: [all8]  # 8方向全て

  # 飛車
  - id: rook
    name: 飛車
    name_en: Rook
    abbrev: 飛
    value: 1000
    promotes_to: dragon
    moves:
      - type: slide
        directions: [forward, backward, left, right]

  # 竜王（成り飛車）
  - id: dragon
    name: 竜王
    name_en: Dragon King
    abbrev: 竜
    value: 1200
    can_promote: false
    moves:
      - type: slide
        directions: [forward, backward, left, right]
      - type: step
        directions: [all_diagonal]

  # 桂馬
  - id: knight
    name: 桂馬
    name_en: Knight
    abbrev: 桂
    value: 400
    promotes_to: promoted_knight
    must_promote:
      type: at_rows
      rows: [0, 1]  # 先手基準
    moves:
      - type: jump
        offsets:
          - [-2, -1]  # 左前
          - [-2,  1]  # 右前

  # 歩兵
  - id: pawn
    name: 歩兵
    name_en: Pawn
    abbrev: 歩
    value: 100
    promotes_to: tokin
    must_promote:
      type: at_rows
      rows: [0]
    moves:
      - type: step
        directions: [forward]

initial_position:
  sente:
    - [8, 0, lance]
    - [8, 1, knight]
    - [8, 2, silver]
    # ... 以下略
```

### 6.2 大局将棋の特殊駒定義例

```yaml
# pieces/taikyoku_shogi_special.yaml

pieces:
  # 獅子
  - id: lion
    name: 獅子
    name_en: Lion
    abbrev: 獅
    value: 2500
    moves:
      - type: area
        range: 2
      - type: igui
      - type: lion_double

  # 奔王
  - id: free_king
    name: 奔王
    name_en: Free King
    abbrev: 奔
    value: 1800
    moves:
      - type: slide
        directions: [all8]

  # 鷹
  - id: hawk
    name: 鷹
    name_en: Hawk
    abbrev: 鷹
    value: 1400
    moves:
      - type: slide
        directions: [all_diagonal]
      - type: step
        directions: [forward, backward]
      - type: jump_over
        directions: [left, right]

  # 鳳凰
  - id: phoenix
    name: 鳳凰
    name_en: Phoenix
    abbrev: 鳳
    value: 1400
    moves:
      - type: step
        directions: [forward, backward, left, right]
      - type: jump_over
        directions: [all_diagonal]
```

### 6.3 パーサー設計

```haskell
module Shogi.Config.Parser
  ( parsePieceSet
  , parseMovePattern
  , ParseError(..)
  ) where

import Data.Yaml (decodeFileEither, FromJSON(..))
import qualified Data.Yaml as Yaml

-- | YAMLからPieceSetをパース
parsePieceSet :: FilePath -> IO (Either ParseError PieceSet)
parsePieceSet path = do
  result <- Yaml.decodeFileEither path
  case result of
    Left err -> return $ Left (YamlError err)
    Right raw -> return $ validateAndConvert raw

-- | MovePatternのパース
instance FromJSON MovePattern where
  parseJSON = withObject "MovePattern" $ \v -> do
    moveType <- v .: "type"
    case moveType :: Text of
      "step" -> Step <$> parseDirections v
      "slide" -> Slide <$> parseDirections v
      "slide_n" -> SlideN <$> v .: "max" <*> parseDirections v
      "jump" -> Jump <$> v .: "offsets"
      "jump_over" -> JumpOver <$> parseDirection v
      "area" -> Area <$> v .: "range"
      "igui" -> pure Igui
      "lion_double" -> pure LionDouble
      _ -> fail $ "Unknown move type: " ++ show moveType

-- | エラー型
data ParseError
  = YamlError Yaml.ParseException
  | ValidationError Text
  | MissingField Text
  deriving (Show)
```

---

## 7. 移動生成の実装方針

### 7.1 アーキテクチャ

```
PieceDefinition (YAML)
       │
       ▼ パース
[MovePattern]
       │
       ▼ 展開
MoveGenerator (型クラス)
       │
       ▼ 生成
[Move] (合法手リスト)
```

### 7.2 MoveGenerator 型クラス

```haskell
class MoveGenerator a where
  generateMoves :: Board -> Pos -> Piece -> a -> [Move]

instance MoveGenerator MovePattern where
  generateMoves board pos piece = \case
    Step dirs -> generateStepMoves board pos piece dirs
    Slide dirs -> generateSlideMoves board pos piece dirs
    SlideN n dirs -> generateSlideNMoves board pos piece n dirs
    Jump offsets -> generateJumpMoves board pos piece offsets
    JumpOver dir -> generateJumpOverMoves board pos piece dir
    Area n -> generateAreaMoves board pos piece n
    Igui -> generateIguiMoves board pos piece
    Conditional cond pat
      | checkCondition board pos piece cond -> generateMoves board pos piece pat
      | otherwise -> []
    Composite pats -> concatMap (generateMoves board pos piece) pats
```

### 7.3 パフォーマンス考慮

```haskell
-- | 移動パターンをコンパイルして高速化
data CompiledMovePattern = CompiledMovePattern
  { cmpStepTargets    :: !(Set Offset)      -- Step の事前計算
  , cmpSlideRays      :: ![(Offset, Int)]   -- Slide の方向と最大距離
  , cmpJumpTargets    :: !(Set Offset)      -- Jump の事前計算
  , cmpSpecialMoves   :: ![SpecialMoveGen]  -- 特殊移動の生成関数
  }

-- | 駒定義をコンパイル（ゲーム開始時に1回）
compilePieceDefinition :: PieceDefinition -> CompiledMovePattern
```

---

## 8. 実装優先度

| 優先度 | 項目 | 理由 |
|--------|------|------|
| P0 | Direction, MovePattern 基本型 | 全ての基盤 |
| P0 | Step, Slide, Jump | 標準将棋に必要 |
| P1 | SlideN, Conditional | 中将棋対応 |
| P1 | YAMLパーサー | 外部定義の基盤 |
| P2 | Area, Igui, LionDouble | 大局将棋の獅子対応 |
| P2 | JumpOver, RangeCapture | 大局将棋の特殊駒 |
| P3 | パフォーマンス最適化 | 大盤面での高速化 |

---

## 9. 互換性

### 9.1 既存コードとの互換性

```haskell
-- | 旧 pieceDirections との互換レイヤー
legacyPieceDirections :: Piece -> [(Int, Int)]
legacyPieceDirections piece =
  let def = lookupPieceDefinition (pieceType piece)
      patterns = pdMoves def
  in concatMap extractStepOffsets patterns

-- | 旧 pieceIsRanged との互換レイヤー
legacyPieceIsRanged :: Piece -> Bool
legacyPieceIsRanged piece =
  let def = lookupPieceDefinition (pieceType piece)
  in any isSlidePattern (pdMoves def)
```

### 9.2 段階的移行

1. **Phase 1**: 新型定義と旧型の並行運用
2. **Phase 2**: 標準将棋をYAML定義に移行
3. **Phase 3**: 旧 `pieceDirections` を deprecated 化
4. **Phase 4**: 中将棋・大局将棋の駒定義追加

---

## 10. 対応バリアント一覧

| バリアント | 盤面 | 駒種数 | 対応予定 |
|------------|------|--------|----------|
| 本将棋 | 9x9 | 8 | Phase 2 |
| ミニ将棋 | 5x5 | 6 | Phase 2 |
| 中将棋 | 12x12 | 46 | Phase 3 |
| 大将棋 | 15x15 | 130 | Phase 4 |
| 大局将棋 | 36x36 | 209 | Phase 4 |

---

## 11. 参考資料

- [中将棋の駒と動き](http://chushogi.jp/rules/pieces.html)
- [大局将棋 - Wikipedia](https://ja.wikipedia.org/wiki/大局将棋)
- Haskell Wiki: [Algebraic Data Types](https://wiki.haskell.org/Algebraic_data_type)

---

## 改訂履歴

| 日付 | バージョン | 変更内容 |
|------|------------|----------|
| 2026-01-29 | 1.0.0 | 初版作成 |
