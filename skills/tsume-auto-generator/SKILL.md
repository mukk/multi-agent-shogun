---
name: tsume-auto-generator
description: 自己対戦棋譜から詰将棋問題を自動生成するパイプライン実装パターン。JSONファイル読込→終盤局面抽出→詰み検証（深さ制限alpha-beta探索）→難易度推定→重複排除→JSON保存を含む。将棋以外（チェス mate-in-N, LLM 対話評価等）の自動問題生成にも応用可能。
---

# Tsume Auto-Generator

## Overview

自己対戦で生成された棋譜ファイルから、詰将棋問題を自動抽出するパイプライン。手動で詰将棋を作成するコストを削減し、大量の問題をデータ駆動で生成する:

- **棋譜読込**: JSON形式の自己対戦記録を読込
- **候補抽出**: 終盤N手前の局面を抽出（N=1,3,5,7,9手前）
- **詰み検証**: 深さ制限付き alpha-beta スタイルの詰み探索
- **難易度推定**: 詰み手数に基づく難易度判定（Easy/Medium/Hard）
- **重複排除**: 局面ハッシュで同一問題を除外
- **JSON保存**: 問題データを再利用可能な形式で保存

将棋以外の応用例:
- チェス: mate-in-N 問題の自動生成
- 囲碁: 詰碁問題の自動抽出（死活判定必要）
- LLM 評価: 対話履歴から良質な Q&A ペアを自動抽出

## When to Use

- 自己対戦エンジンで大量の棋譜を生成済み
- 詰将棋問題のデータセットを拡充したい
- 評価関数の学習データとして詰み局面を収集したい
- ユーザー向けコンテンツを自動生成したい
- チェスの mate-in-N タクティクス問題を作りたい
- LLM の対話ログから高品質なプロンプト例を抽出したい

## Instructions

### Step 1: データ型定義

#### Haskell

```haskell
-- | 難易度レベル
data TsumeDifficulty = Easy | Medium | Hard
  deriving (Show, Eq, Ord, Generic)

instance ToJSON TsumeDifficulty where
  toJSON Easy   = "easy"
  toJSON Medium = "medium"
  toJSON Hard   = "hard"

-- | 生成設定
data GenerationConfig = GenerationConfig
  { gcMaxGames  :: !Int  -- 処理する最大対局数
  , gcMaxMoves  :: !Int  -- 探索する最大詰み手数（奇数）
  , gcSkipMoves :: !Int  -- 終局から何手前まで遡るか
  } deriving (Show, Eq)

defaultGenerationConfig :: GenerationConfig
defaultGenerationConfig = GenerationConfig
  { gcMaxGames  = 10    -- 10局まで処理
  , gcMaxMoves  = 7     -- 7手詰まで探索
  , gcSkipMoves = 9     -- 終局9手前まで遡る
  }

-- | 候補局面
data TsumeCandidate = TsumeCandidate
  { tcGameId     :: !Text
  , tcMoveNumber :: !Int
  , tcPosition   :: !Position
  , tcAttacker   :: !Player
  } deriving (Show)

-- | 生成済み詰将棋問題
data GeneratedTsume = GeneratedTsume
  { gtHash       :: !Text          -- 重複排除用ハッシュ
  , gtSfen       :: !Text          -- SFEN局面
  , gtSolution   :: ![Text]        -- 解答手順
  , gtNumMoves   :: !Int           -- 詰み手数
  , gtDifficulty :: !TsumeDifficulty
  , gtGameId     :: !Text          -- 元の対局ID
  , gtMoveNumber :: !Int           -- 元の対局の手数
  } deriving (Show, Eq, Generic)
```

#### TypeScript

```typescript
type TsumeDifficulty = 'easy' | 'medium' | 'hard'

interface GenerationConfig {
  maxGames: number
  maxMoves: number
  skipMoves: number
}

interface TsumeCandidate {
  gameId: string
  moveNumber: number
  position: Position
  attacker: 'sente' | 'gote'
}

interface GeneratedTsume {
  hash: string
  sfen: string
  solution: string[]
  numMoves: number
  difficulty: TsumeDifficulty
  gameId: string
  moveNumber: number
}
```

### Step 2: 棋譜ファイル読込

自己対戦で保存された JSON ファイルを読み込む。

#### ディレクトリ構造

```
data/selfplay/
  game_001.json
  game_002.json
  game_003.json
  ...
```

#### JSON 形式

```json
{
  "gameId": "game_001",
  "result": "sente_win",
  "moves": 87,
  "kifu": ["7六歩", "3四歩", "2六歩", ..., "7三金"],
  "variant": "standard"
}
```

#### Haskell

```haskell
data SelfPlayRecord = SelfPlayRecord
  { sprGameId  :: !Text
  , sprResult  :: !Text
  , sprKifu    :: ![Text]
  , sprMoves   :: !Int
  , sprVariant :: !Text
  } deriving (Show, Generic)

instance FromJSON SelfPlayRecord where
  parseJSON = withObject "SelfPlayRecord" $ \o ->
    SelfPlayRecord
      <$> o .: "gameId"
      <*> o .: "result"
      <*> o .: "kifu"
      <*> o .: "moves"
      <*> o .: "variant"

loadSelfPlayRecords :: FilePath -> Int -> IO [SelfPlayRecord]
loadSelfPlayRecords dir maxFiles = do
  files <- listDirectory dir
  let jsonFiles = take maxFiles $ filter (\f -> takeExtension f == ".json") files
  results <- forM jsonFiles $ \f -> do
    bs <- BS.readFile (dir </> f)
    case eitherDecodeStrict' bs of
      Right rec -> return (Just rec)
      Left _    -> return Nothing
  return (catMaybes results)

isCheckmateGame :: SelfPlayRecord -> Bool
isCheckmateGame rec =
  let r = sprResult rec
  in "sente_win" == r || "gote_win" == r
```

#### TypeScript

```typescript
interface SelfPlayRecord {
  gameId: string
  result: string
  kifu: string[]
  moves: number
  variant: string
}

async function loadSelfPlayRecords(dir: string, maxFiles: number): Promise<SelfPlayRecord[]> {
  const files = await fs.readdir(dir)
  const jsonFiles = files.filter(f => f.endsWith('.json')).slice(0, maxFiles)

  const records: SelfPlayRecord[] = []
  for (const file of jsonFiles) {
    const content = await fs.readFile(path.join(dir, file), 'utf-8')
    try {
      const record: SelfPlayRecord = JSON.parse(content)
      records.push(record)
    } catch (e) {
      console.error(`Failed to parse ${file}:`, e)
    }
  }

  return records
}

function isCheckmateGame(record: SelfPlayRecord): boolean {
  return record.result === 'sente_win' || record.result === 'gote_win'
}
```

### Step 3: 候補局面の抽出

終盤N手前の局面を抽出する（N = 1, 3, 5, 7, 9手前）。

```
棋譜: [手1, 手2, ..., 手85, 手86, 手87(詰み)]
      ↓
候補: 手86 (1手前), 手84 (3手前), 手82 (5手前), ...
```

#### Haskell

```haskell
extractCandidates :: GenerationConfig -> SelfPlayRecord -> IO [TsumeCandidate]
extractCandidates config rec = do
  let reg = standardShogiRegistry
      bcfg = standardShogiConfig
      startPos = standardShogiStartPosition
      kifuMoves = sprKifu rec
      attacker = if sprResult rec == "sente_win" then Sente else Gote

  -- Replay the game to get all positions
  positions <- replayGame reg bcfg startPos kifuMoves

  let numPositions = length positions
      offsets = filter (<= gcSkipMoves config)
              $ filter (<= numPositions - 1)
              $ [1, 3, 5, 7, 9]
      candidates = mapMaybe (mkCandidate positions numPositions attacker (sprGameId rec)) offsets

  return candidates

replayGame :: PieceRegistry -> BoardConfig -> Position -> [Text] -> IO [Position]
replayGame reg bcfg startPos moves = do
  let go acc _pos [] = return (reverse acc)
      go acc pos (m:ms) =
        case parseMove bcfg reg (T.unpack m) of
          Nothing -> return (reverse acc)  -- Stop on unparseable move
          Just mv -> case makeMove reg pos mv of
            Nothing   -> return (reverse acc)  -- Stop on illegal move
            Just pos' -> go (pos' : acc) pos' ms
  go [startPos] startPos moves

mkCandidate :: [Position] -> Int -> Player -> Text -> Int -> Maybe TsumeCandidate
mkCandidate positions total attacker gameId offset =
  let idx = total - offset
  in if idx >= 0 && idx < length positions
     then Just TsumeCandidate
            { tcGameId     = gameId
            , tcMoveNumber = idx
            , tcPosition   = positions !! idx
            , tcAttacker   = attacker
            }
     else Nothing
```

#### TypeScript

```typescript
async function extractCandidates(
  config: GenerationConfig,
  record: SelfPlayRecord
): Promise<TsumeCandidate[]> {
  const startPos = getStandardStartPosition()
  const attacker = record.result === 'sente_win' ? 'sente' : 'gote'

  const positions = await replayGame(startPos, record.kifu)
  const numPositions = positions.length

  const offsets = [1, 3, 5, 7, 9]
    .filter(o => o <= config.skipMoves)
    .filter(o => o <= numPositions - 1)

  return offsets
    .map(offset => {
      const idx = numPositions - offset
      if (idx < 0 || idx >= numPositions) return null
      return {
        gameId: record.gameId,
        moveNumber: idx,
        position: positions[idx],
        attacker
      }
    })
    .filter((c): c is TsumeCandidate => c !== null)
}
```

### Step 4: 詰み検証（Alpha-Beta スタイル探索）

深さ制限付きの詰み探索を実装する。

#### アルゴリズム

```
攻め方の手番:
  - 全ての合法手を試す
  - 少なくとも1つが詰みに繋がればOK（OR探索）
  - 王手のみを試す（高速化）

受け方の手番:
  - 全ての合法手を試す
  - 全ての手が詰みに繋がることを確認（AND探索）
  - 1つでも逃れがあれば詰みではない
```

#### Haskell

```haskell
verifyTsume :: PieceRegistry -> Position -> Int -> Maybe [Move]
verifyTsume reg pos maxDepth
  | maxDepth <= 0 = Nothing
  | isCheckmate reg pos = Just []  -- Already checkmate
  | otherwise = searchMate reg pos maxDepth True

searchMate :: PieceRegistry -> Position -> Int -> Bool -> Maybe [Move]
searchMate _ _ 0 _ = Nothing
searchMate reg pos depth isAttacker
  | isAttacker =
      -- Attacker's turn: try all check moves, need at least one to force mate
      let moves = generateLegalMoves reg pos
          checkMoves = filter (givesCheck reg pos) moves
      in tryMoves reg pos checkMoves (depth - 1) False
  | otherwise =
      -- Defender's turn: all legal moves must lead to mate (forced)
      let moves = generateLegalMoves reg pos
      in if null moves
         then Just []  -- No legal moves = checkmate
         else allMovesForceMate reg pos moves (depth - 1)

tryMoves :: PieceRegistry -> Position -> [Move] -> Int -> Bool -> Maybe [Move]
tryMoves _ _ [] _ _ = Nothing
tryMoves reg pos (m:ms) depth isAtt =
  case makeMove reg pos m of
    Nothing -> tryMoves reg pos ms depth isAtt
    Just pos' ->
      if isCheckmate reg pos'
        then Just [m]
        else case searchMate reg pos' depth isAtt of
          Just sol -> Just (m : sol)
          Nothing  -> tryMoves reg pos ms depth isAtt

allMovesForceMate :: PieceRegistry -> Position -> [Move] -> Int -> Maybe [Move]
allMovesForceMate _ _ [] _ = Just []
allMovesForceMate reg pos (m:ms) depth =
  case makeMove reg pos m of
    Nothing -> allMovesForceMate reg pos ms depth
    Just pos' ->
      case searchMate reg pos' depth True of
        Nothing -> Nothing  -- Defender has an escape
        Just _  -> allMovesForceMate reg pos ms depth

givesCheck :: PieceRegistry -> Position -> Move -> Bool
givesCheck reg pos mv =
  case makeMove reg pos mv of
    Nothing   -> False
    Just pos' -> isInCheck reg pos' (posCurrentPlayer pos')
```

#### TypeScript

```typescript
function verifyTsume(position: Position, maxDepth: number): Move[] | null {
  if (maxDepth <= 0) return null
  if (isCheckmate(position)) return []
  return searchMate(position, maxDepth, true)
}

function searchMate(position: Position, depth: number, isAttacker: boolean): Move[] | null {
  if (depth === 0) return null

  if (isAttacker) {
    const moves = generateLegalMoves(position)
    const checkMoves = moves.filter(m => givesCheck(position, m))
    return tryMoves(position, checkMoves, depth - 1, false)
  } else {
    const moves = generateLegalMoves(position)
    if (moves.length === 0) return []
    return allMovesForceMate(position, moves, depth - 1)
  }
}

function tryMoves(position: Position, moves: Move[], depth: number, isAtt: boolean): Move[] | null {
  for (const move of moves) {
    const newPos = applyMove(position, move)
    if (!newPos) continue

    if (isCheckmate(newPos)) {
      return [move]
    }

    const solution = searchMate(newPos, depth, isAtt)
    if (solution) {
      return [move, ...solution]
    }
  }
  return null
}
```

### Step 5: 難易度推定

詰み手数に基づく難易度判定。

```haskell
estimateDifficulty :: Int -> TsumeDifficulty
estimateDifficulty n
  | n <= 1    = Easy
  | n <= 3    = Medium
  | otherwise = Hard
```

**カスタマイズ例:**
- Easy: 1手詰
- Medium: 3手詰
- Hard: 5手詰
- VeryHard: 7手詰以上

### Step 6: 重複排除

同一局面から複数の問題が生成されるのを防ぐため、局面ハッシュで重複を排除する。

```haskell
-- 局面をSFEN形式のハッシュに変換
positionHash :: BoardConfig -> PieceRegistry -> Position -> Text
positionHash bcfg reg pos = positionToSfen bcfg reg pos

-- 重複排除（nubByでハッシュ比較）
deduplicateTsume :: [GeneratedTsume] -> [GeneratedTsume]
deduplicateTsume = nubBy (\a b -> gtHash a == gtHash b)
```

### Step 7: 統合パイプライン

全ステップを組み合わせた生成関数。

```haskell
generateTsumeProblems :: GenerationConfig -> FilePath -> IO [GeneratedTsume]
generateTsumeProblems config dataDir = do
  -- 1. Load selfplay records
  records <- loadSelfPlayRecords dataDir (gcMaxGames config)

  -- 2. Filter for checkmate games
  let checkmateGames = filter isCheckmateGame records

  -- 3. Extract candidates
  candidates <- fmap concat $ forM checkmateGames $ \rec ->
    extractCandidates config rec

  -- 4. Verify and build tsume problems
  let verified = mapMaybe (verifyAndBuild config) candidates

  -- 5. Deduplicate
  let deduped = nubBy (\a b -> gtHash a == gtHash b) verified

  return deduped

verifyAndBuild :: GenerationConfig -> TsumeCandidate -> Maybe GeneratedTsume
verifyAndBuild config cand =
  let pos = tcPosition cand
      maxDepth = gcMaxMoves config
      reg = standardShogiRegistry
  in case verifyTsume reg pos maxDepth of
    Nothing -> Nothing
    Just solution ->
      let numMoves = length solution
          diff = estimateDifficulty numMoves
          bcfg = standardShogiConfig
          sfen = positionToSfen bcfg reg pos
          hash = positionHash bcfg reg pos
      in Just GeneratedTsume
           { gtHash       = hash
           , gtSfen       = sfen
           , gtSolution   = map formatMove solution
           , gtNumMoves   = numMoves
           , gtDifficulty = diff
           , gtGameId     = tcGameId cand
           , gtMoveNumber = tcMoveNumber cand
           }
```

### Step 8: JSON 保存

生成した問題を再利用可能な JSON 形式で保存。

```haskell
saveTsumeProblems :: FilePath -> [GeneratedTsume] -> IO ()
saveTsumeProblems dir problems = do
  createDirectoryIfMissing True dir
  forM_ problems $ \problem -> do
    let filename = dir </> T.unpack (gtHash problem) <> ".json"
    BL.writeFile filename (encode problem)
```

```typescript
async function saveTsumeProblems(dir: string, problems: GeneratedTsume[]): Promise<void> {
  await fs.mkdir(dir, { recursive: true })
  for (const problem of problems) {
    const filename = path.join(dir, `${problem.hash}.json`)
    await fs.writeFile(filename, JSON.stringify(problem, null, 2))
  }
}
```

## Examples

### Example 1: 将棋の詰将棋生成

```haskell
let config = GenerationConfig
      { gcMaxGames  = 100
      , gcMaxMoves  = 7
      , gcSkipMoves = 9
      }

problems <- generateTsumeProblems config "data/selfplay"

putStrLn $ "Generated " ++ show (length problems) ++ " tsume problems"

-- 出力例:
-- Generated 23 tsume problems
-- Easy: 8, Medium: 11, Hard: 4
```

### Example 2: チェス mate-in-N 問題生成

同じパターンでチェスの詰み問題を生成:

```haskell
data ChessMate = ChessMate
  { cmFen       :: !Text
  , cmSolution  :: ![ChessMove]
  , cmDepth     :: !Int
  , cmGameId    :: !Text
  }

generateChessMates :: GenerationConfig -> FilePath -> IO [ChessMate]
generateChessMates config dataDir = do
  games <- loadChessGames dataDir (gcMaxGames config)
  let candidates = extractChessCandidates games
      verified = mapMaybe (verifyChessMate config) candidates
  return verified
```

### Example 3: LLM 対話評価

LLM の対話履歴から高品質な Q&A ペアを抽出:

```python
@dataclass
class DialogCandidate:
    conversation_id: str
    turn_number: int
    context: list[str]
    question: str
    answer: str

def extract_qa_candidates(conversations: list[Conversation]) -> list[DialogCandidate]:
    candidates = []
    for conv in conversations:
        # 対話の後半（質の高い応答が出やすい）を抽出
        for i in range(len(conv.turns) - 9, len(conv.turns), 2):
            if i >= 0:
                candidates.append(DialogCandidate(
                    conversation_id=conv.id,
                    turn_number=i,
                    context=conv.turns[:i],
                    question=conv.turns[i],
                    answer=conv.turns[i + 1]
                ))
    return candidates

def verify_qa_quality(candidate: DialogCandidate, threshold: float) -> bool:
    # LLM による品質評価
    score = llm_evaluate_qa(candidate.question, candidate.answer)
    return score >= threshold
```

## Guidelines

1. **王手のみ探索で高速化**: 攻め方の手番では王手以外の手を枝刈り。詰みに繋がらない手を事前排除

2. **深さ制限は奇数に設定**: 詰み手数は攻め方視点なので奇数（1手詰、3手詰、5手詰...）。偶数だと受け方で終わる

3. **重複排除は局面ハッシュで**: 同一局面からの複数抽出を防ぐため、SFEN ベースのハッシュで `nubBy` を適用

4. **難易度は手数ベース**: 1手詰=Easy, 3手詰=Medium, 5手詰以上=Hard が目安。ゲームごとに調整可

5. **候補抽出は奇数オフセット**: 終局 1,3,5,7,9手前を抽出。偶数手前は受け方の手番になるため詰み探索不可

6. **エラー処理は寛容に**: 棋譜パースエラーや不正手は `Maybe` で包み、その対局をスキップして続行

7. **バッチ処理で実行**: 100局以上の棋譜を一括処理し、有効な問題を数十件抽出するのが実用的

8. **探索深度は5〜7手が現実的**: 9手詰以上は計算時間が指数的に増加。大量生成には深度5程度を推奨

9. **JSON 保存でデータ再利用**: 生成した問題はファイル化し、アプリ起動時に読込む構成にする

10. **並列処理で高速化**: 複数対局の候補抽出・検証は独立なので、並列化可能（Haskell `async`, TypeScript `Promise.all`）
