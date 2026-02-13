---
name: kifu-parser
description: KIF/CSA棋譜ファイルパーサーパターン。行単位状態蓄積型パース、文字マッピングテーブル（全角数字→列、漢数字→行、駒名変換）、ヘッダー抽出、手順テキスト+消費時間の分離、終局判定マーカーを含む。将棋・チェス・囲碁等のゲーム記録インポート、棋譜解析、対局データベース構築、フォーマット変換に使用。
---

# Kifu Parser

## Overview

KIF/CSA形式の棋譜ファイルをパースするためのスキル。以下の機能を含む:

- **複数フォーマット対応**: KIF（日本語表記）、CSA（英数表記）の両方をサポート
- **文字マッピングテーブル**: 全角数字・漢数字・駒名の変換表
- **ヘッダー抽出**: 対局者名、日時、棋戦名等のメタデータ取得
- **手順パース**: 手番・移動先・駒種・消費時間を分離
- **コメント保持**: 各手に付けられたコメントを保存
- **終局判定**: 投了、千日手、反則等の終局マーカーを検出
- **行単位状態蓄積**: fold的に各行を処理して最終結果を構築

## When to Use

- 棋譜ファイルをアップロードして盤面再生する機能を作る
- KIF/CSA形式のテキストをインポートして対局データベースに登録する
- 異なるフォーマット間で棋譜を変換する（KIF → CSA 等）
- 棋譜解析で手順を抽出してAI評価にかける
- 対局サイトの棋譜エクスポート機能を実装する
- チェス・囲碁等の他ゲームに同じパターンを適用する

## Instructions

### Step 1: データ型の定義

パース結果を表すデータ型を定義する。

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)

-- パース結果（メタデータ + 手順 + 結果）
data KifuData = KifuData
  { kdMetadata :: !KifuMetadata
  , kdMoves    :: ![KifMove]
  , kdResult   :: !(Maybe Text)
  } deriving (Show, Eq)

-- メタデータ（ヘッダー情報）
data KifuMetadata = KifuMetadata
  { kmSente  :: !(Maybe Text)  -- 先手（black）
  , kmGote   :: !(Maybe Text)  -- 後手（white）
  , kmDate   :: !(Maybe Text)  -- 対局日
  , kmEvent  :: !(Maybe Text)  -- 棋戦名
  , kmSite   :: !(Maybe Text)  -- 対局場所
  } deriving (Show, Eq)

emptyMetadata :: KifuMetadata
emptyMetadata = KifuMetadata Nothing Nothing Nothing Nothing Nothing

-- 1手の情報
data KifMove = KifMove
  { kmvNumber    :: !Int          -- 手番（1から開始）
  , kmvText      :: !Text         -- 移動テキスト（"７六歩" または "+7776FU"）
  , kmvTimeSpent :: !(Maybe Int)  -- 消費時間（秒）
  , kmvComment   :: !(Maybe Text) -- コメント
  } deriving (Show, Eq)
```

### Step 2: 文字マッピングテーブル

全角数字、漢数字、駒名を内部表現に変換する関数を作る。

```haskell
import qualified Data.Text as T

-- 全角数字 → 列番号（1-9）
fullWidthToCol :: Char -> Maybe Int
fullWidthToCol c = case c of
  '\65297' -> Just 1  -- '１'
  '\65298' -> Just 2  -- '２'
  '\65299' -> Just 3  -- '３'
  '\65300' -> Just 4  -- '４'
  '\65301' -> Just 5  -- '５'
  '\65302' -> Just 6  -- '６'
  '\65303' -> Just 7  -- '７'
  '\65304' -> Just 8  -- '８'
  '\65305' -> Just 9  -- '９'
  _        -> Nothing

-- 漢数字 → 行番号（1-9）
kanjiToRow :: Char -> Maybe Int
kanjiToRow c = case c of
  '\19968' -> Just 1  -- '一'
  '\20108' -> Just 2  -- '二'
  '\19977' -> Just 3  -- '三'
  '\22235' -> Just 4  -- '四'
  '\20116' -> Just 5  -- '五'
  '\20845' -> Just 6  -- '六'
  '\19971' -> Just 7  -- '七'
  '\20843' -> Just 8  -- '八'
  '\20061' -> Just 9  -- '九'
  _        -> Nothing

-- 日本語駒名 → 内部名（KIF形式用）
pieceFromJapanese :: Text -> Maybe Text
pieceFromJapanese t = case t of
  "\27497"         -> Just "Pawn"           -- 歩
  "\39321"         -> Just "Lance"          -- 香
  "\26690"         -> Just "Knight"         -- 桂
  "\37504"         -> Just "Silver"         -- 銀
  "\37329"         -> Just "Gold"           -- 金
  "\35282"         -> Just "Bishop"         -- 角
  "\39131"         -> Just "Rook"           -- 飛
  "\29577"         -> Just "King"           -- 玉
  "\29579"         -> Just "King"           -- 王
  -- 成駒
  "\12392"         -> Just "PromotedPawn"   -- と
  "\25104\39321"   -> Just "PromotedLance"  -- 成香
  "\25104\26690"   -> Just "PromotedKnight" -- 成桂
  "\25104\37504"   -> Just "PromotedSilver" -- 成銀
  "\39340"         -> Just "PromotedBishop" -- 馬
  "\40845"         -> Just "PromotedRook"   -- 龍
  "\31452"         -> Just "PromotedRook"   -- 竜
  _                -> Nothing

-- CSA駒コード → 内部名（CSA形式用）
csaPieceToName :: Text -> Maybe Text
csaPieceToName t = case t of
  "FU" -> Just "Pawn"
  "KY" -> Just "Lance"
  "KE" -> Just "Knight"
  "GI" -> Just "Silver"
  "KI" -> Just "Gold"
  "KA" -> Just "Bishop"
  "HI" -> Just "Rook"
  "OU" -> Just "King"
  -- 成駒
  "TO" -> Just "PromotedPawn"
  "NY" -> Just "PromotedLance"
  "NK" -> Just "PromotedKnight"
  "NG" -> Just "PromotedSilver"
  "UM" -> Just "PromotedBishop"
  "RY" -> Just "PromotedRook"
  _    -> Nothing
```

### Step 3: KIFパーサー（日本語形式）

行単位で状態を蓄積しながらパースする。

```haskell
import Data.Char (isDigit)
import qualified Data.Text.Read as TR

-- KIFファイル全体をパース
parseKif :: Text -> Either Text KifuData
parseKif input =
  let ls = T.lines input
      (meta, moves, result) = parseLines ls emptyMetadata [] Nothing Nothing
  in Right KifuData
       { kdMetadata = meta
       , kdMoves    = reverse moves  -- 末尾追加していたので反転
       , kdResult   = result
       }

-- 全行をfold的に処理（状態蓄積型）
parseLines :: [Text] -> KifuMetadata -> [KifMove] -> Maybe Text -> Maybe Text
           -> (KifuMetadata, [KifMove], Maybe Text)
parseLines [] meta moves _ result = (meta, moves, result)
parseLines (line : rest) meta moves lastComment result
  | T.null stripped = parseLines rest meta moves Nothing result

  -- コメント行（* で始まる）
  | "*" `T.isPrefixOf` stripped =
      let comment = T.strip (T.drop 1 stripped)
      in parseLines rest meta moves (Just comment) result

  -- ヘッダー行（先手：、後手：等）
  | isHeaderLine stripped =
      let meta' = parseHeaderLine stripped meta
      in parseLines rest meta' moves Nothing result

  -- 終局行（投了、千日手等）
  | isTermination stripped =
      let result' = Just (extractTermination stripped)
      in parseLines rest meta moves Nothing result'

  -- 手順行（数字で始まる）
  | isMoveLineCandidate stripped =
      case parseMoveLine stripped lastComment of
        Just mv -> parseLines rest meta (mv : moves) Nothing result
        Nothing -> parseLines rest meta moves Nothing result

  | otherwise = parseLines rest meta moves Nothing result
  where
    stripped = T.strip line

-- ヘッダー行の判定
isHeaderLine :: Text -> Bool
isHeaderLine t =
  any (`T.isInfixOf` t)
    [ "\20808\25163\65306"           -- 先手：
    , "\24460\25163\65306"           -- 後手：
    , "\38283\22987\26085\26178\65306" -- 開始日時：
    , "\22580\25152\65306"           -- 場所：
    , "\26847\25126\65306"           -- 棋戦：
    ]

-- ヘッダーのパースと更新
parseHeaderLine :: Text -> KifuMetadata -> KifuMetadata
parseHeaderLine t meta
  | "\20808\25163\65306" `T.isInfixOf` t =
      meta { kmSente = Just (extractHeaderValue "\20808\25163\65306" t) }
  | "\24460\25163\65306" `T.isInfixOf` t =
      meta { kmGote = Just (extractHeaderValue "\24460\25163\65306" t) }
  | "\38283\22987\26085\26178\65306" `T.isInfixOf` t =
      meta { kmDate = Just (extractHeaderValue "\38283\22987\26085\26178\65306" t) }
  | "\22580\25152\65306" `T.isInfixOf` t =
      meta { kmSite = Just (extractHeaderValue "\22580\25152\65306" t) }
  | "\26847\25126\65306" `T.isInfixOf` t =
      meta { kmEvent = Just (extractHeaderValue "\26847\25126\65306" t) }
  | otherwise = meta

-- ヘッダー値抽出（"先手：山田太郎" → "山田太郎"）
extractHeaderValue :: Text -> Text -> Text
extractHeaderValue key t =
  case T.splitOn key t of
    (_ : val : _) -> T.strip val
    _             -> ""

-- 手順行の判定（数字で始まる）
isMoveLineCandidate :: Text -> Bool
isMoveLineCandidate t =
  let s = T.stripStart t
  in not (T.null s) && isDigit (T.head s)

-- 手順行のパース（"   1 ７六歩(77)   ( 0:16/)"）
parseMoveLine :: Text -> Maybe Text -> Maybe KifMove
parseMoveLine line mComment =
  let parts = T.words (T.strip line)
  in case parts of
    (numT : rest)
      | Just moveNum <- readInt numT
      , not (null rest) ->
          let moveText = extractMoveText rest
              timeSpent = extractTime rest
          in Just KifMove
               { kmvNumber    = moveNum
               , kmvText      = moveText
               , kmvTimeSpent = timeSpent
               , kmvComment   = mComment
               }
    _ -> Nothing

-- 移動テキスト抽出（"７六歩" のみ取り出す）
extractMoveText :: [Text] -> Text
extractMoveText parts =
  let moveParts = takeWhile (\p -> not (isSourceSquare p) && not (isTimeInfo p)) parts
  in T.unwords moveParts

-- 元マス判定 "(77)"
isSourceSquare :: Text -> Bool
isSourceSquare t = "(" `T.isPrefixOf` t && ")" `T.isSuffixOf` t && T.length t <= 5

-- 時間情報判定 "( 0:16/)"
isTimeInfo :: Text -> Bool
isTimeInfo t = "(" `T.isPrefixOf` t && ":" `T.isInfixOf` t

-- 消費時間抽出（秒単位）
extractTime :: [Text] -> Maybe Int
extractTime parts =
  case filter isTimeInfo parts of
    (t : _) -> parseTimeToken t
    []      -> Nothing

-- 時間トークンパース "( 0:16/)" → 16秒
parseTimeToken :: Text -> Maybe Int
parseTimeToken t =
  let cleaned = T.filter (\c -> isDigit c || c == ':') t
  in case T.splitOn ":" cleaned of
    [minT, secT]
      | Just m <- readInt minT
      , Just s <- readInt secT -> Just (m * 60 + s)
    [secT]
      | Just s <- readInt secT -> Just s
    _ -> Nothing

-- 終局判定
isTermination :: Text -> Bool
isTermination t =
  any (`T.isInfixOf` t)
    [ "\25237\20102"               -- 投了
    , "\20013\26029"               -- 中断
    , "\21315\26085\25163"         -- 千日手
    , "\25345\12385\23558\37329"   -- 持将棋
    , "\21453\21063"               -- 反則
    ]

-- 終局テキスト抽出
extractTermination :: Text -> Text
extractTermination t =
  let stripped = T.strip t
      withoutNum = T.dropWhile (\c -> isDigit c || c == ' ') stripped
  in T.strip withoutNum

-- 安全なText→Int変換
readInt :: Text -> Maybe Int
readInt t = case TR.decimal t of
  Right (n, rest) | T.null rest -> Just n
  _                             -> Nothing
```

### Step 4: CSAパーサー（英数形式）

CSA v2形式（+7776FU等）をパースする。

```haskell
-- CSAファイル全体をパース
parseCsa :: Text -> Either Text KifuData
parseCsa input =
  let ls = T.lines input
      (meta, moves, result) = parseCsaLines ls emptyMetadata [] 1 Nothing
  in Right KifuData
       { kdMetadata = meta
       , kdMoves    = reverse moves
       , kdResult   = result
       }

-- CSA行の処理
parseCsaLines :: [Text] -> KifuMetadata -> [KifMove] -> Int -> Maybe Text
              -> (KifuMetadata, [KifMove], Maybe Text)
parseCsaLines [] meta moves _ result = (meta, moves, result)
parseCsaLines (line : rest) meta moves moveNum result
  | T.null stripped = parseCsaLines rest meta moves moveNum result

  -- バージョン行 "V2.2"
  | "V" `T.isPrefixOf` stripped =
      parseCsaLines rest meta moves moveNum result

  -- 先手名 "N+山田太郎"
  | "N+" `T.isPrefixOf` stripped =
      let name = T.strip (T.drop 2 stripped)
      in parseCsaLines rest (meta { kmSente = Just name }) moves moveNum result

  -- 後手名 "N-佐藤花子"
  | "N-" `T.isPrefixOf` stripped =
      let name = T.strip (T.drop 2 stripped)
      in parseCsaLines rest (meta { kmGote = Just name }) moves moveNum result

  -- 情報行 "$EVENT:竜王戦"
  | "$" `T.isPrefixOf` stripped =
      let meta' = parseCsaInfoLine stripped meta
      in parseCsaLines rest meta' moves moveNum result

  -- 終局行 "%TORYO"
  | "%" `T.isPrefixOf` stripped =
      let result' = Just (csaTerminationToText stripped)
      in parseCsaLines rest meta moves moveNum result'

  -- 手順行 "+7776FU"
  | isCsaMoveLine stripped =
      let mv = parseCsaMoveLine stripped moveNum
      in parseCsaLines rest meta (mv : moves) (moveNum + 1) result

  -- コメント行 "'これは良い手"
  | "'" `T.isPrefixOf` stripped =
      parseCsaLines rest meta moves moveNum result

  -- 盤面行 "P1-KY-KE..." （初期盤面定義、スキップ）
  | "P" `T.isPrefixOf` stripped =
      parseCsaLines rest meta moves moveNum result

  | otherwise = parseCsaLines rest meta moves moveNum result
  where
    stripped = T.strip line

-- CSA情報行パース "$EVENT:竜王戦"
parseCsaInfoLine :: Text -> KifuMetadata -> KifuMetadata
parseCsaInfoLine t meta
  | "$EVENT:" `T.isPrefixOf` t =
      meta { kmEvent = Just (T.strip (T.drop 7 t)) }
  | "$SITE:" `T.isPrefixOf` t =
      meta { kmSite = Just (T.strip (T.drop 6 t)) }
  | "$START_TIME:" `T.isPrefixOf` t =
      meta { kmDate = Just (T.strip (T.drop 12 t)) }
  | otherwise = meta

-- CSA手順行判定（+ or - で始まり、4桁数字が続く）
isCsaMoveLine :: Text -> Bool
isCsaMoveLine t =
  not (T.null t) &&
  (T.head t == '+' || T.head t == '-') &&
  T.length t >= 7 &&
  T.all isDigit (T.take 4 (T.drop 1 t))

-- CSA手順行パース "+7776FU" → KifMove
parseCsaMoveLine :: Text -> Int -> KifMove
parseCsaMoveLine line moveNum =
  KifMove
    { kmvNumber    = moveNum
    , kmvText      = T.strip line
    , kmvTimeSpent = Nothing  -- CSAには通常時間情報なし
    , kmvComment   = Nothing
    }

-- CSA終局マーカー変換
csaTerminationToText :: Text -> Text
csaTerminationToText t
  | "%TORYO"       `T.isPrefixOf` t = "\25237\20102"       -- 投了
  | "%SENNICHITE"  `T.isPrefixOf` t = "\21315\26085\25163" -- 千日手
  | "%TIME_UP"     `T.isPrefixOf` t = "\26178\38291\20999\12426" -- 時間切れ
  | "%ILLEGAL_MOVE" `T.isPrefixOf` t = "\21453\21063"      -- 反則
  | "%JISHOGI"     `T.isPrefixOf` t = "\25345\12385\23558\37329" -- 持将棋
  | "%KACHI"       `T.isPrefixOf` t = "\20837\21033"       -- 勝利
  | "%HIKIWAKE"    `T.isPrefixOf` t = "\24341\12365\20998\12369" -- 引き分け
  | "%CHUDAN"      `T.isPrefixOf` t = "\20013\26029"       -- 中断
  | otherwise                        = T.strip t
```

### Step 5: API統合

バックエンドでパーサーをエンドポイントに統合する。

```haskell
import Servant

type KifuAPI =
  "api" :> "kifu" :> "import"
    :> ReqBody '[JSON] KifuImportRequest
    :> Post '[JSON] (ApiResponse KifuData)

data KifuImportRequest = KifuImportRequest
  { kirText :: !Text
  } deriving (Show, Generic)

instance FromJSON KifuImportRequest

handleKifuImport :: KifuImportRequest -> Handler (ApiResponse KifuData)
handleKifuImport KifuImportRequest{..} = do
  -- フォーマット自動判定（CSAは "+" or "-" で始まる行が多い）
  let result = if isCsaFormat kirText
                 then parseCsa kirText
                 else parseKif kirText
  case result of
    Right kifu -> pure $ okResponse kifu
    Left err   -> throwError err400 { errBody = encodeUtf8 err }

-- CSAフォーマット判定（簡易版）
isCsaFormat :: Text -> Bool
isCsaFormat t =
  let moveLines = filter (\l -> not (T.null l) && (T.head l == '+' || T.head l == '-'))
                         (T.lines t)
  in length moveLines > 5  -- 5手以上のCSA手順行があればCSA
```

### Step 6: フロントエンド統合

TypeScriptでAPI呼び出しとUI表示を行う。

```typescript
// api/kifuApi.ts
export interface KifuData {
  metadata: KifuMetadata
  moves: KifMove[]
  result: string | null
}

export interface KifuMetadata {
  sente: string | null
  gote: string | null
  date: string | null
  event: string | null
  site: string | null
}

export interface KifMove {
  number: number
  text: string
  timeSpent: number | null
  comment: string | null
}

export async function importKifu(content: string): Promise<KifuData> {
  const body = { text: content }
  return requestWithTimeout<KifuData>('/kifu/import', {
    method: 'POST',
    body: JSON.stringify(body),
  })
}

// components/KifuImport.tsx
function KifuImport() {
  const [content, setContent] = useState('')
  const [kifu, setKifu] = useState<KifuData | null>(null)

  const handleImport = async () => {
    try {
      const result = await importKifu(content)
      setKifu(result)
      navigate('/kifu/player', { state: { kifu: result } })
    } catch (err) {
      console.error('Kifu import failed:', err)
    }
  }

  return (
    <div>
      <textarea value={content} onChange={e => setContent(e.target.value)} />
      <button onClick={handleImport}>インポート</button>
    </div>
  )
}
```

## Examples

### Example 1: KIFファイルのパース

```haskell
-- KIF形式の例
kifSample :: Text
kifSample = T.unlines
  [ "先手：山田太郎"
  , "後手：佐藤花子"
  , "開始日時：2025/01/15"
  , "棋戦：竜王戦"
  , "   1 ７六歩(77)   ( 0:16/00:00:16)"
  , "   2 ３四歩(33)   ( 0:12/00:00:12)"
  , "*この手は定石"
  , "   3 ２六歩(27)   ( 0:08/00:00:24)"
  , "まで3手で中断"
  ]

main :: IO ()
main = do
  case parseKif kifSample of
    Right kifu -> do
      print (kmSente $ kdMetadata kifu)  -- Just "山田太郎"
      print (length $ kdMoves kifu)      -- 3
      print (kdResult kifu)              -- Just "中断"
    Left err -> putStrLn $ "Parse error: " <> T.unpack err
```

### Example 2: CSAファイルのパース

```haskell
-- CSA形式の例
csaSample :: Text
csaSample = T.unlines
  [ "V2.2"
  , "N+山田太郎"
  , "N-佐藤花子"
  , "$EVENT:竜王戦"
  , "$SITE:東京"
  , "+7776FU"
  , "-3334FU"
  , "+2726FU"
  , "%CHUDAN"
  ]

main :: IO ()
main = do
  case parseCsa csaSample of
    Right kifu -> do
      print (kmEvent $ kdMetadata kifu)  -- Just "竜王戦"
      print (kmvText $ head $ kdMoves kifu)  -- "+7776FU"
    Left err -> putStrLn $ "Parse error: " <> T.unpack err
```

### Example 3: フォーマット自動判定

```typescript
// ファイル拡張子またはコンテンツでフォーマット判定
async function handleFileUpload(file: File) {
  const text = await file.text()

  // バックエンドが自動判定するので、そのまま送る
  const kifu = await importKifu(text)

  // メタデータ表示
  console.log(`先手: ${kifu.metadata.sente}`)
  console.log(`後手: ${kifu.metadata.gote}`)
  console.log(`総手数: ${kifu.moves.length}`)
}
```

## Guidelines

1. **型安全な変換関数**: `fullWidthToCol`, `kanjiToRow`, `pieceFromJapanese` は全て `Maybe` を返す。失敗時はスキップ推奨
2. **行単位状態蓄積**: `parseLines` は fold パターン。メタデータ・手順・結果を蓄積しながら処理
3. **コメント保持**: 直前の `*` コメント行を次の手に付ける。`lastComment` 引数で渡す
4. **エラーハンドリング**: パース失敗は `Left Text` で返す。ただし柔軟性のため部分的な失敗は無視（スキップ）もあり
5. **フォーマット自動判定**: CSAは `+` or `-` で始まる行が多い。KIFは日本語ヘッダー `先手：` 等を含む
6. **末尾反転**: 手順を `moves` リストの末尾追加で蓄積するため、最後に `reverse` が必要
7. **時間情報はオプショナル**: KIFには時間情報があるがCSAには通常ない。`Maybe Int` で表現
8. **終局マーカー検出**: 投了・千日手・反則等は日本語文字列で判定。CSAは `%` プレフィックス
9. **盤面復元は別処理**: このパーサーは **手順テキストのみ** を抽出。盤面状態再構築は別モジュールで行う
10. **Unicode文字コード直接指定**: 漢字を文字リテラルで書くとエンコード問題が起きる環境がある。`\数値` 表記を推奨
11. **プレフィックス判定**: `T.isPrefixOf` でヘッダー種別を判定。曖昧性を避けるため長いプレフィックスを優先
12. **テスト用サンプルデータ**: 実在の棋譜ファイルを用意してテスト。エッジケース（時間なし、コメントなし等）も確認
