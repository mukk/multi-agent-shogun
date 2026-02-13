---
name: sfen-position-parser
description: SFEN (Shogi Forsyth-Edwards Notation) 局面記法パーサー + バリデーター実装パターン。盤面⇔テキスト双方向変換、駒配置・持駒・手番のパース、不正局面検出（玉不在・二歩・不成可能駒・玉隣接・駒数過剰）を含む。将棋以外のボードゲーム（チェス FEN, 囲碁 SGF 等）にも応用可能。
---

# SFEN Position Parser

## Overview

SFEN (Shogi Forsyth-Edwards Notation) は将棋の局面をテキスト形式で表現する標準記法。このスキルはSFEN形式のパース・生成・検証を一貫して実装するパターンを提供する:

- **SFEN→Position変換**: テキスト → 内部データ構造
- **Position→SFEN変換**: 内部データ → テキスト（エクスポート）
- **局面検証**: 不正な局面を検出（玉不在、二歩、不成可能駒配置等）
- **エラー型定義**: 検証失敗時の詳細なエラー情報
- **駒パレット対応**: UI から駒を配置する際の ID ↔ 文字変換

将棋以外のボードゲームにも適用可能:
- チェス: FEN (Forsyth-Edwards Notation)
- 囲碁: SGF (Smart Game Format)
- オセロ/リバーシ: カスタム盤面記法

## When to Use

- 将棋の局面エディタ機能を実装する
- 棋譜ファイル (KIF/CSA) から局面を復元する
- 詰将棋問題をテキスト形式で保存・読込する
- ネットワーク越しに局面を送受信する
- 局面データベースのインデックスキーとして使う
- 他のボードゲームで盤面を文字列化する必要がある

## Instructions

### Step 1: SFEN フォーマット定義

SFEN は4つのセクションをスペース区切りで連結:

```
<board> <turn> <hand> <moveNumber>
```

**例: 標準将棋の初期局面**
```
lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1
```

#### セクション 1: 盤面 (`<board>`)

- 9段目（最奥）から1段目へ順に記述
- `/` で段を区切る
- 駒の記号:
  - 大文字 = 先手, 小文字 = 後手
  - `K`=玉, `R`=飛, `B`=角, `G`=金, `S`=銀, `N`=桂, `L`=香, `P`=歩
  - 成駒: `+R`=龍, `+B`=馬, `+S`=成銀, `+N`=成桂, `+L`=成香, `+P`=と
- 数字 = 空マス数（例: `9` = 9マス全て空）
- 圧縮: 連続する空マスは数字にまとめる

**Example:**
```
lnsgkgsnl/1r5b1/ppppppppp → 9段目、8段目、7段目
- l=香(後手), n=桂(後手), s=銀(後手), ...
- 1=空1マス, r=飛(後手), 5=空5マス, b=角(後手), 1=空1マス
```

#### セクション 2: 手番 (`<turn>`)

- `b` (black) = 先手の番
- `w` (white) = 後手の番

#### セクション 3: 持駒 (`<hand>`)

- `-` = 持駒なし
- 駒記号 + 枚数（省略時は1枚）
  - 例: `P2L` → 歩1枚 + 香2枚
  - 大文字 = 先手の持駒, 小文字 = 後手の持駒

#### セクション 4: 手数 (`<moveNumber>`)

- 現在の手数（1から開始）
- 内部的には 0-indexed で管理する場合が多い（パース時に -1）

### Step 2: パーサー実装（SFEN → Position）

#### Haskell

```haskell
data Position = Position
  { posBoard         :: !Board
  , posSenteHand     :: !Hand
  , posGoteHand      :: !Hand
  , posCurrentPlayer :: !Player
  , posMoveNumber    :: !Int
  } deriving (Show)

-- | Parse SFEN string into Position
parseSfen :: PieceRegistry -> BoardConfig -> Text -> Either Text Position
parseSfen reg config sfen = do
  let parts = T.words sfen
  when (length parts < 4) $
    Left "SFEN format requires at least 4 sections"

  let [boardPart, turnPart, handPart, moveNumPart] = take 4 parts

  board    <- parseSfenBoard reg config boardPart
  turn     <- parseSfenTurn turnPart
  (sHand, gHand) <- parseSfenHand reg handPart
  moveNum  <- parseMoveNumber moveNumPart

  return Position
    { posBoard         = board
    , posSenteHand     = sHand
    , posGoteHand      = gHand
    , posCurrentPlayer = turn
    , posMoveNumber    = moveNum - 1  -- 1-indexed → 0-indexed
    }

parseSfenBoard :: PieceRegistry -> BoardConfig -> Text -> Either Text Board
parseSfenBoard reg config boardText = do
  let ranks = T.splitOn "/" boardText
  when (length ranks /= boardRows config) $
    Left $ "Expected " <> show (boardRows config) <> " ranks"

  parsedRanks <- mapM (parseRank reg) (zip [boardRows config - 1, boardRows config - 2 .. 0] ranks)

  let emptyBd = emptyBoard config
      board = foldl' (\bd (r, pieces) ->
                foldl' (\bd' (c, pc) -> setPiece bd' (Pos r c) pc) bd pieces
              ) emptyBd parsedRanks

  return board

parseRank :: PieceRegistry -> (Int, Text) -> Either Text (Int, [(Int, Piece)])
parseRank reg (rowIdx, rankText) = do
  pieces <- parseRankPieces reg (T.unpack rankText) 0
  return (rowIdx, pieces)

-- | Parse rank characters (handles +R, +B, etc.)
parseRankPieces :: PieceRegistry -> String -> Int -> Either Text [Either Int Piece]
parseRankPieces _ [] _ = Right []
parseRankPieces reg ('+':c:rest) col = do
  pc <- charToPiece reg c True  -- promoted
  rest' <- parseRankPieces reg rest (col + 1)
  return $ Right pc : rest'
parseRankPieces reg (c:rest) col
  | isDigit c = do
      let n = read [c] :: Int
      rest' <- parseRankPieces reg rest (col + n)
      return $ Left n : rest'
  | otherwise = do
      pc <- charToPiece reg c False  -- not promoted
      rest' <- parseRankPieces reg rest (col + 1)
      return $ Right pc : rest'

charToPiece :: PieceRegistry -> Char -> Bool -> Either Text Piece
charToPiece reg ch promoted = do
  let owner = if isUpper ch then Sente else Gote
      pieceName = case toUpper ch of
        'K' -> "King"
        'R' -> "Rook"
        'B' -> "Bishop"
        'G' -> "Gold"
        'S' -> "Silver"
        'N' -> "Knight"
        'L' -> "Lance"
        'P' -> "Pawn"
        _   -> ""

  if T.null pieceName
    then Left $ "Unknown piece character: " <> T.pack [ch]
    else case lookupPieceByName reg pieceName of
      Nothing  -> Left $ "Piece not found in registry: " <> pieceName
      Just pid -> return $ Piece owner pid promoted

parseSfenTurn :: Text -> Either Text Player
parseSfenTurn "b" = Right Sente
parseSfenTurn "w" = Right Gote
parseSfenTurn t   = Left $ "Invalid turn: " <> t

parseSfenHand :: PieceRegistry -> Text -> Either Text (Hand, Hand)
parseSfenHand _ "-" = Right (emptyHand, emptyHand)
parseSfenHand reg handText = parseHandPieces reg (T.unpack handText) emptyHand emptyHand
```

#### TypeScript

```typescript
interface Position {
  board: (Piece | null)[][]
  senteHand: Hand
  goteHand: Hand
  currentPlayer: 'sente' | 'gote'
  moveNumber: number
}

function parseSFEN(sfen: string): Position {
  const parts = sfen.trim().split(' ')
  if (parts.length < 4) {
    throw new Error('SFEN requires 4 sections')
  }

  const [boardPart, turnPart, handPart, moveNumPart] = parts

  const board = parseSFENBoard(boardPart)
  const turn = turnPart === 'b' ? 'sente' : 'gote'
  const [senteHand, goteHand] = parseSFENHand(handPart)
  const moveNumber = parseInt(moveNumPart, 10) - 1  // 1-indexed → 0-indexed

  return { board, senteHand, goteHand, currentPlayer: turn, moveNumber }
}

function parseSFENBoard(boardPart: string): (Piece | null)[][] {
  const ranks = boardPart.split('/')
  const board: (Piece | null)[][] = []

  for (let rankIdx = 0; rankIdx < 9; rankIdx++) {
    const rank = ranks[rankIdx] || ''
    const row: (Piece | null)[] = []
    let i = 0

    while (i < rank.length) {
      const ch = rank[i]

      // Promoted piece (+R, +B, etc.)
      if (ch === '+' && i + 1 < rank.length) {
        const piece = charToPiece(rank[i + 1], true, rankIdx)
        row.push(piece)
        i += 2
      }
      // Empty squares (digit)
      else if (/\d/.test(ch)) {
        const emptyCount = parseInt(ch, 10)
        for (let j = 0; j < emptyCount; j++) {
          row.push(null)
        }
        i++
      }
      // Regular piece
      else {
        const piece = charToPiece(ch, false, rankIdx)
        row.push(piece)
        i++
      }
    }

    board.push(row)
  }

  return board
}

function charToPiece(ch: string, promoted: boolean, row: number): Piece {
  const isGote = ch === ch.toLowerCase()
  const color = isGote ? 'gote' : 'sente'
  const upper = ch.toUpperCase()

  const nameMap: Record<string, string> = {
    K: 'King', R: 'Rook', B: 'Bishop', G: 'Gold',
    S: 'Silver', N: 'Knight', L: 'Lance', P: 'Pawn'
  }

  return {
    name: nameMap[upper],
    color,
    isPromoted: promoted
  }
}
```

### Step 3: エクスポート実装（Position → SFEN）

#### Haskell

```haskell
toSfen :: PieceRegistry -> Position -> Text
toSfen reg pos = T.unwords
  [ boardToSfen reg (posBoard pos)
  , playerToSfen (posCurrentPlayer pos)
  , handToSfen reg (posSenteHand pos) (posGoteHand pos)
  , T.pack $ show (posMoveNumber pos + 1)  -- 0-indexed → 1-indexed
  ]

boardToSfen :: PieceRegistry -> Board -> Text
boardToSfen reg board =
  let config = boardConfig board
      rows = boardRows config
      rankTexts = map (rankToSfen reg board) [rows - 1, rows - 2 .. 0]
  in T.intercalate "/" rankTexts

rankToSfen :: PieceRegistry -> Board -> Int -> Text
rankToSfen reg board rowIdx =
  let config = boardConfig board
      cols = boardCols config
      squares = [getPiece board (Pos rowIdx c) | c <- [0 .. cols - 1]]
  in compressRank squares
  where
    compressRank :: [Maybe Piece] -> Text
    compressRank [] = ""
    compressRank sqs =
      let (empties, rest) = span isNothing sqs
          emptyCount = length empties
          prefix = if emptyCount > 0 then T.pack (show emptyCount) else ""
      in case rest of
        [] -> prefix
        (Just pc:rest') ->
          prefix <> pieceToSfenChar reg pc <> compressRank rest'

pieceToSfenChar :: PieceRegistry -> Piece -> Text
pieceToSfenChar reg pc =
  let baseName = pdName (getPieceDefinition reg (pieceId pc))
      baseChar = case baseName of
        "King"   -> 'K'
        "Rook"   -> 'R'
        "Bishop" -> 'B'
        "Gold"   -> 'G'
        "Silver" -> 'S'
        "Knight" -> 'N'
        "Lance"  -> 'L'
        "Pawn"   -> 'P'
        _        -> '?'
      ch = if pieceOwner pc == Sente then baseChar else toLower baseChar
      prom = if piecePromoted pc then "+" else ""
  in T.pack (prom ++ [ch])
```

#### TypeScript

```typescript
function boardToSFEN(board: (Piece | null)[][]): string {
  const ranks: string[] = []

  for (let r = 0; r < 9; r++) {
    let rankStr = ''
    let emptyCount = 0

    for (let c = 0; c < 9; c++) {
      const piece = board[r][c]
      if (!piece) {
        emptyCount++
      } else {
        if (emptyCount > 0) {
          rankStr += emptyCount.toString()
          emptyCount = 0
        }
        rankStr += pieceToChar(piece)
      }
    }

    if (emptyCount > 0) rankStr += emptyCount.toString()
    ranks.push(rankStr)
  }

  return `${ranks.join('/')} b - 1`
}

function pieceToChar(piece: Piece): string {
  const charMap: Record<string, string> = {
    King: 'K', Rook: 'R', Bishop: 'B', Gold: 'G',
    Silver: 'S', Knight: 'N', Lance: 'L', Pawn: 'P'
  }

  let ch = charMap[piece.name] || '?'
  if (piece.color === 'gote') ch = ch.toLowerCase()
  if (piece.isPromoted) ch = '+' + ch
  return ch
}
```

### Step 4: 局面検証

将棋ルールに違反する局面を検出する。

#### エラー型定義

```haskell
data PositionError
  = NoSenteKing                      -- 先手玉不在
  | NoGoteKing                       -- 後手玉不在
  | TooManyPawnsInColumn Int Player  -- 二歩（同一列に同色の歩が2枚以上）
  | PawnOnLastRank Player            -- 歩が最終段（不成不可）
  | LanceOnLastRank Player           -- 香が最終段（不成不可）
  | KnightOnLastTwoRanks Player      -- 桂が最終2段（不成不可）
  | KingsAdjacent                    -- 両玉が隣接（実戦では不可能）
  | InvalidPieceCount Text Int Int   -- 駒数超過 (駒名, 実数, 最大)
  deriving (Eq, Show)
```

#### 検証関数

```haskell
validatePosition :: PieceRegistry -> BoardConfig -> Position -> [PositionError]
validatePosition reg config pos =
  let board = posBoard pos
      allPieces = extractAllPieces board config
  in concat
    [ checkKings allPieces
    , checkNifu reg allPieces (boardCols config)
    , checkImmobilePieces reg allPieces (boardRows config)
    , checkAdjacentKings reg allPieces
    ]

checkKings :: [(Pos, Piece)] -> [PositionError]
checkKings pieces =
  let hasSenteKing = any (\(_, pc) -> isKing pc && pieceOwner pc == Sente) pieces
      hasGoteKing  = any (\(_, pc) -> isKing pc && pieceOwner pc == Gote) pieces
  in (if not hasSenteKing then [NoSenteKing] else [])
  ++ (if not hasGoteKing then [NoGoteKing] else [])

checkNifu :: PieceRegistry -> [(Pos, Piece)] -> Int -> [PositionError]
checkNifu reg pieces cols =
  let pawns = filter (\(_, pc) -> isPawn reg pc && not (piecePromoted pc)) pieces
      checkColumn col player =
        let colPawns = filter (\(Pos _ c, pc) -> c == col && pieceOwner pc == player) pawns
        in if length colPawns > 1 then [TooManyPawnsInColumn col player] else []
  in concatMap (\col -> checkColumn col Sente ++ checkColumn col Gote) [0 .. cols - 1]

checkImmobilePieces :: PieceRegistry -> [(Pos, Piece)] -> Int -> [PositionError]
checkImmobilePieces reg pieces rows =
  let checkPiece (Pos r _, pc)
        | piecePromoted pc = []
        | otherwise =
            let name = getPieceName reg pc
                owner = pieceOwner pc
                lastRank = if owner == Sente then rows - 1 else 0
                secondLast = if owner == Sente then rows - 2 else 1
            in case name of
              "Pawn"   | r == lastRank -> [PawnOnLastRank owner]
              "Lance"  | r == lastRank -> [LanceOnLastRank owner]
              "Knight" | r == lastRank || r == secondLast -> [KnightOnLastTwoRanks owner]
              _ -> []
  in concatMap checkPiece pieces

checkAdjacentKings :: PieceRegistry -> [(Pos, Piece)] -> [PositionError]
checkAdjacentKings reg pieces =
  let kings = filter (\(_, pc) -> isKing pc) pieces
  in case kings of
    [(pos1, pc1), (pos2, pc2)]
      | pieceOwner pc1 /= pieceOwner pc2 && adjacent pos1 pos2 -> [KingsAdjacent]
    _ -> []
  where
    adjacent (Pos r1 c1) (Pos r2 c2) =
      abs (r1 - r2) <= 1 && abs (c1 - c2) <= 1 && (r1 /= r2 || c1 /= c2)
```

### Step 5: API エンドポイント（フロントエンド連携）

#### Backend (Haskell Servant)

```haskell
type PositionAPI =
       "api" :> "position" :> "validate" :> ReqBody '[JSON] ValidatePositionRequest :> Post '[JSON] ValidatePositionResponse
  :<|> "api" :> "position" :> "from-sfen" :> ReqBody '[JSON] FromSfenRequest :> Post '[JSON] FromSfenResponse

data ValidatePositionRequest = ValidatePositionRequest
  { vprqSfen :: !Text
  } deriving (Generic)

data ValidatePositionResponse = ValidatePositionResponse
  { vprValid    :: !Bool
  , vprErrors   :: ![Text]
  , vprWarnings :: ![Text]
  } deriving (Generic)

handleValidatePosition :: ValidatePositionRequest -> Handler ValidatePositionResponse
handleValidatePosition req = do
  let reg = standardShogiRegistry
      config = standardShogiConfig
  case parseSfen reg config (vprqSfen req) of
    Left err -> return ValidatePositionResponse
      { vprValid = False, vprErrors = [err], vprWarnings = [] }
    Right pos ->
      let errors = validatePosition reg config pos
          (fatalErrors, warnings) = partition isFatal errors
      in return ValidatePositionResponse
        { vprValid = null fatalErrors
        , vprErrors = map formatError fatalErrors
        , vprWarnings = map formatError warnings
        }
```

#### Frontend (TypeScript)

```typescript
interface ValidationResult {
  valid: boolean
  errors: string[]
  warnings: string[]
}

async function validatePosition(sfen: string): Promise<ValidationResult> {
  const res = await fetch('/api/position/validate', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ sfen })
  })
  return res.json()
}

// Usage in React component
const handleValidate = async () => {
  const result = await validatePosition(currentSFEN)
  if (result.valid) {
    alert('✓ 有効な局面です')
  } else {
    alert(`✗ 無効な局面:\n${result.errors.join('\n')}`)
  }
}
```

## Examples

### Example 1: 初期局面のパース

```haskell
let sfen = "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1"
    reg = standardShogiRegistry
    config = standardShogiConfig

case parseSfen reg config sfen of
  Left err  -> putStrLn $ "Parse error: " ++ T.unpack err
  Right pos -> putStrLn $ "Parsed position: " ++ show pos
```

### Example 2: 詰将棋局面のエクスポート

```typescript
const tsumePosition: Position = {
  board: createTsumeBoard(),
  senteHand: { Rook: 1, Bishop: 1 },
  goteHand: {},
  currentPlayer: 'sente',
  moveNumber: 0
}

const sfen = boardToSFEN(tsumePosition.board)
console.log('SFEN:', sfen)
// → "...+R+B... b R2B -"
```

### Example 3: チェス FEN への応用

チェスの FEN (Forsyth-Edwards Notation) も同様のパターンで実装可能:

```
rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
```

- 盤面記号: `K`=King, `Q`=Queen, `R`=Rook, `B`=Bishop, `N`=Knight, `P`=Pawn
- 手番: `w`=White, `b`=Black
- キャスリング権: `KQkq`（将棋にはないので省略可）
- アンパッサン: `-`
- 50手ルール: `0`
- 手数: `1`

### Example 4: 二歩検証

```haskell
-- 不正局面: 5筋に先手の歩が2枚
let invalidSfen = "lnsgkgsnl/1r5b1/ppppppppp/9/4P4/9/PPPP1PPPP/1B5R1/LNSGKGSNL b - 1"

case parseSfen reg config invalidSfen of
  Right pos ->
    let errors = validatePosition reg config pos
    in if any isNifu errors
       then putStrLn "二歩エラー検出"
       else putStrLn "検証通過"
```

## Guidelines

1. **パース失敗時は Either で左辺にエラーメッセージを返す**: 例外を投げず、型安全にエラーを伝播させる

2. **空マス圧縮を必ず実装**: `111111111` → `9` に圧縮しないと SFEN が冗長になる。エクスポート時は連続空マスをカウントして数字化

3. **成駒は `+` プレフィックスで表現**: `+R`=龍, `+B`=馬。パース時は2文字ペアで処理（`+` + 駒文字）

4. **手数は1-indexed（SFEN）vs 0-indexed（内部）**: パース時に `-1`, エクスポート時に `+1` を忘れずに適用

5. **検証は段階的に実施**:
   - **Fatal**: 玉不在 → 対局不可能
   - **Warning**: 二歩、不成可能駒 → 実戦では起こらないが、詰将棋問題では許容される場合あり

6. **持駒は大文字=先手、小文字=後手で区別**: `P2L` → 先手の歩1+香2, `p2l` → 後手の歩1+香2

7. **駒数検証はオプション**: 40枚制限（先手20+後手20）を超える場合はエラー。詰将棋問題では緩和することも

8. **玉隣接チェックは詰将棋では無効化**: 詰将棋問題では玉が隣接することがあるため、検証をスキップするフラグを用意

9. **SFEN と内部表現の双方向性を保証**: `parseSfen` と `toSfen` は互いの逆関数になるべき（ラウンドトリップテスト必須）

10. **エラーメッセージは具体的に**: 「二歩」だけでなく「5筋に先手の歩が2枚」のように列・プレイヤーを明示
