---
name: board-position-editor
description: ボードゲーム局面エディタUI雛形。駒パレット（クリックで選択）+ 盤面（クリック配置）+ SFEN/FEN変換（インポート/エクスポート）+ 局面検証API + プリセット読込 + 対局開始/分析ボタンを含む。将棋・チェス・囲碁・オセロ等あらゆる盤面ゲームに適用可能。
---

# Board Position Editor

## Overview

ボードゲームの局面を GUI で編集する汎用 UI パターン。3パネル構成（駒パレット + 盤面 + コントロール）を基本として、以下の機能を統合する:

- **駒パレット**: 駒/石をクリックで選択 + 先手/後手切り替え
- **盤面クリック配置**: 選択した駒を盤面にクリックで配置
- **消去モード**: 選択解除で駒を削除するモード
- **SFEN/FEN 変換**: テキスト形式でインポート/エクスポート
- **局面検証**: サーバー API で不正局面を検出
- **プリセット読込**: 初期局面・定跡局面をワンクリック読込
- **対局開始**: 編集した局面から対局を開始
- **局面分析**: 編集した局面を分析モードで開く

将棋以外への応用:
- チェス: FEN 読込/エクスポート + 駒パレット（King/Queen/Rook/Bishop/Knight/Pawn）
- 囲碁: SGF 読込 + 石パレット（黒石/白石）
- オセロ/リバーシ: カスタム盤面記法 + 石パレット

## When to Use

- 詰将棋問題作成ツールを実装する
- チェスのタクティクス問題エディタを作る
- 任意局面から対局を開始する機能を追加する
- 棋譜再生時に局面を編集・分岐させる
- 教材作成で特定局面を示したい
- AI 評価関数のテストケースを作成する

## Instructions

### Step 1: コンポーネント構造設計

#### 3パネルレイアウト

```
┌─────────────┬──────────────┬──────────────┐
│ Piece       │    Board     │   Controls   │
│ Palette     │              │              │
│             │              │              │
│ [Color      │   9x9 Grid   │ SFEN Export  │
│  Selector]  │              │ SFEN Import  │
│             │              │ Validate     │
│ [Piece      │              │ Presets      │
│  Grid]      │              │ Start Game   │
│             │              │ Analyze      │
│ [Clear Sel] │              │              │
└─────────────┴──────────────┴──────────────┘
```

#### React

```tsx
interface EditorState {
  board: (Piece | null)[][]
  selectedPiece: PiecePalette | null
  selectedColor: 'sente' | 'gote'
  sfenInput: string
  validationResult: ValidationResult | null
}

export const PositionEditor: React.FC = () => {
  const [board, setBoard] = useState<(Piece | null)[][]>(createEmptyBoard())
  const [selectedPiece, setSelectedPiece] = useState<PiecePalette | null>(null)
  const [selectedColor, setSelectedColor] = useState<'sente' | 'gote'>('sente')
  const [sfenInput, setSfenInput] = useState<string>(STANDARD_SFEN)
  const [validationResult, setValidationResult] = useState<ValidationResult | null>(null)

  const currentSFEN = useMemo(() => boardToSFEN(board), [board])

  return (
    <div className="position-editor">
      <div className="editor-container">
        <PiecePalette
          selectedPiece={selectedPiece}
          selectedColor={selectedColor}
          onPieceSelect={setSelectedPiece}
          onColorChange={setSelectedColor}
          onClearSelection={() => setSelectedPiece(null)}
        />

        <BoardArea
          board={board}
          onSquareClick={handleSquareClick}
        />

        <ControlsPanel
          currentSFEN={currentSFEN}
          sfenInput={sfenInput}
          onSfenChange={setSfenInput}
          onLoadFromSFEN={handleLoadFromSFEN}
          onValidate={handleValidate}
          validationResult={validationResult}
          onStartGame={handleStartGame}
          onAnalyze={handleAnalyze}
          onClearBoard={handleClearBoard}
          onLoadStandardPosition={handleLoadStandardPosition}
        />
      </div>
    </div>
  )
}
```

### Step 2: 駒パレット実装

#### データ定義

```typescript
interface PiecePalette {
  name: string
  kanji: string
  promoted?: boolean
}

const SHOGI_PALETTE: PiecePalette[] = [
  { name: 'King', kanji: '王' },
  { name: 'Rook', kanji: '飛' },
  { name: 'Rook', kanji: '龍', promoted: true },
  { name: 'Bishop', kanji: '角' },
  { name: 'Bishop', kanji: '馬', promoted: true },
  { name: 'Gold', kanji: '金' },
  { name: 'Silver', kanji: '銀' },
  { name: 'Silver', kanji: '全', promoted: true },
  { name: 'Knight', kanji: '桂' },
  { name: 'Knight', kanji: '圭', promoted: true },
  { name: 'Lance', kanji: '香' },
  { name: 'Lance', kanji: '杏', promoted: true },
  { name: 'Pawn', kanji: '歩' },
  { name: 'Pawn', kanji: 'と', promoted: true },
]

const CHESS_PALETTE: PiecePalette[] = [
  { name: 'King', symbol: '♔' },
  { name: 'Queen', symbol: '♕' },
  { name: 'Rook', symbol: '♖' },
  { name: 'Bishop', symbol: '♗' },
  { name: 'Knight', symbol: '♘' },
  { name: 'Pawn', symbol: '♙' },
]
```

#### UI

```tsx
const PiecePalette: React.FC<Props> = ({
  selectedPiece, selectedColor, onPieceSelect, onColorChange, onClearSelection
}) => (
  <div className="piece-palette">
    <h3>駒パレット</h3>

    {/* Color Selector */}
    <div className="color-selector">
      <button
        className={selectedColor === 'sente' ? 'active' : ''}
        onClick={() => onColorChange('sente')}
      >
        ☗ 先手
      </button>
      <button
        className={selectedColor === 'gote' ? 'active' : ''}
        onClick={() => onColorChange('gote')}
      >
        ☖ 後手
      </button>
    </div>

    {/* Piece Grid */}
    <div className="palette-pieces">
      {SHOGI_PALETTE.map((piece, idx) => (
        <button
          key={idx}
          className={`palette-piece ${
            selectedPiece?.name === piece.name &&
            selectedPiece?.promoted === piece.promoted
              ? 'selected'
              : ''
          }`}
          onClick={() => onPieceSelect(piece)}
          title={`${piece.name}${piece.promoted ? ' (promoted)' : ''}`}
        >
          {piece.kanji}
        </button>
      ))}
    </div>

    {/* Clear Selection (消去モード) */}
    <button className="clear-selection-btn" onClick={onClearSelection}>
      ✕ 選択解除 (消去モード)
    </button>
  </div>
)
```

### Step 3: 盤面クリック処理

#### React

```tsx
const handleSquareClick = useCallback(
  (pos: { row: number; col: number }) => {
    const { row, col } = pos
    const newBoard = board.map(r => [...r])

    if (selectedPiece) {
      // Place the selected piece
      newBoard[row][col] = {
        pieceId: getPieceId(selectedPiece.name),
        name: selectedPiece.name,
        nameKanji: selectedPiece.kanji,
        color: selectedColor,
        position: { row, col },
        isPromoted: selectedPiece.promoted || false
      }
    } else {
      // Remove piece (clear mode)
      newBoard[row][col] = null
    }

    setBoard(newBoard)
    setValidationResult(null) // Clear validation on edit
  },
  [selectedPiece, selectedColor, board]
)
```

#### Vanilla JS

```javascript
function handleSquareClick(row, col) {
  if (selectedPiece) {
    board[row][col] = {
      type: selectedPiece.name,
      color: selectedColor,
      promoted: selectedPiece.promoted || false
    }
  } else {
    board[row][col] = null
  }
  renderBoard()
}
```

### Step 4: SFEN インポート/エクスポート

#### SFEN → Board

```typescript
function parseSFEN(sfen: string): (Piece | null)[][] {
  const parts = sfen.trim().split(' ')
  if (parts.length < 1) return createEmptyBoard()

  const ranks = parts[0].split('/')
  const board: (Piece | null)[][] = []

  for (let rankIdx = 0; rankIdx < 9; rankIdx++) {
    const rank = ranks[rankIdx] || ''
    const row: (Piece | null)[] = []
    let i = 0

    while (i < rank.length) {
      const ch = rank[i]

      if (ch === '+' && i + 1 < rank.length) {
        // Promoted piece (+R, +B, etc.)
        const piece = charToPiece(rank[i + 1], true, rankIdx)
        row.push(piece)
        i += 2
      } else if (/\d/.test(ch)) {
        // Empty squares
        const emptyCount = parseInt(ch, 10)
        for (let j = 0; j < emptyCount; j++) {
          row.push(null)
        }
        i++
      } else {
        // Regular piece
        const piece = charToPiece(ch, false, rankIdx)
        row.push(piece)
        i++
      }
    }

    while (row.length < 9) row.push(null)  // Pad to 9 squares
    board.push(row)
  }

  while (board.length < 9) {
    board.push(Array(9).fill(null))  // Pad to 9 ranks
  }

  return board
}
```

#### Board → SFEN

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
```

### Step 5: 局面検証 API

#### Backend (Haskell Servant)

```haskell
type PositionAPI =
  "api" :> "position" :> "validate" :> ReqBody '[JSON] ValidatePositionRequest :> Post '[JSON] ValidatePositionResponse

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

#### Frontend

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

const handleValidate = async () => {
  setIsValidating(true)
  try {
    const result = await validatePosition(currentSFEN)
    setValidationResult(result)
  } catch (error) {
    console.error('Validation error:', error)
    setValidationResult({
      valid: false,
      errors: ['サーバーエラー: 検証に失敗しました'],
      warnings: []
    })
  } finally {
    setIsValidating(false)
  }
}
```

### Step 6: 検証結果の表示

```tsx
{validationResult && (
  <div
    className={`validation-result ${
      validationResult.valid ? 'valid' : 'invalid'
    }`}
  >
    {validationResult.valid ? (
      <p className="success">✓ 有効な局面です</p>
    ) : (
      <p className="error">✗ 無効な局面です</p>
    )}

    {validationResult.errors.length > 0 && (
      <div className="errors">
        <strong>エラー:</strong>
        <ul>
          {validationResult.errors.map((err, i) => (
            <li key={i}>{err}</li>
          ))}
        </ul>
      </div>
    )}

    {validationResult.warnings.length > 0 && (
      <div className="warnings">
        <strong>警告:</strong>
        <ul>
          {validationResult.warnings.map((warn, i) => (
            <li key={i}>{warn}</li>
          ))}
        </ul>
      </div>
    )}
  </div>
)}
```

### Step 7: プリセット読込

```typescript
const STANDARD_SFEN = 'lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1'

const PRESETS: Record<string, string> = {
  standard: STANDARD_SFEN,
  tsume1: '9/9/9/9/9/9/9/9/k8 b R2B 1',
  tsume2: '9/9/9/9/9/9/9/1k7/9 b G 1',
  // ... more presets
}

const handleLoadStandardPosition = () => {
  const sfen = PRESETS.standard
  setSfenInput(sfen)
  const parsed = parseSFEN(sfen)
  setBoard(parsed)
  setValidationResult(null)
}
```

### Step 8: 対局開始/分析

```typescript
const handleStartGame = async () => {
  setIsCreatingGame(true)
  try {
    const result = await createGameFromPosition(currentSFEN)
    navigate(`/game/${result.gameId}`)
  } catch (error) {
    console.error('Failed to create game:', error)
    alert('対局開始に失敗しました')
  } finally {
    setIsCreatingGame(false)
  }
}

const handleAnalyze = async () => {
  setIsCreatingGame(true)
  try {
    const result = await createGameFromPosition(currentSFEN)
    navigate(`/game/${result.gameId}/review`)
  } catch (error) {
    console.error('Failed to create game for analysis:', error)
    alert('分析開始に失敗しました')
  } finally {
    setIsCreatingGame(false)
  }
}
```

#### Backend API

```haskell
type GameAPI =
  "api" :> "game" :> "from-position" :> ReqBody '[JSON] FromPositionRequest :> Post '[JSON] FromPositionResponse

data FromPositionRequest = FromPositionRequest
  { fprqSfen :: !Text
  } deriving (Generic)

data FromPositionResponse = FromPositionResponse
  { fprResGameId :: !Text
  } deriving (Generic)

handleCreateGameFromPosition :: FromPositionRequest -> Handler FromPositionResponse
handleCreateGameFromPosition req = do
  let reg = standardShogiRegistry
      config = standardShogiConfig
  case parseSfen reg config (fprqSfen req) of
    Left err -> throwError err400 { errBody = "Invalid SFEN: " <> encodeUtf8 err }
    Right pos -> do
      gameId <- generateGameId
      registerGame gameId pos
      return FromPositionResponse { fprResGameId = gameId }
```

## Examples

### Example 1: 将棋局面エディタ

```typescript
// 初期局面から対局を開始
const editor = new PositionEditor({
  initialSFEN: STANDARD_SFEN,
  palette: SHOGI_PALETTE,
  onStartGame: async (sfen) => {
    const game = await createGame(sfen)
    window.location.href = `/game/${game.id}`
  }
})
```

### Example 2: チェス局面エディタ

FEN 形式に対応:

```typescript
const CHESS_STANDARD_FEN = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1'

const ChessPositionEditor = () => {
  const [board, setBoard] = useState(parseFEN(CHESS_STANDARD_FEN))
  const [selectedPiece, setSelectedPiece] = useState<ChessPiece | null>(null)

  return (
    <div>
      <ChessPalette onSelect={setSelectedPiece} />
      <ChessBoard board={board} onSquareClick={(r, c) => {
        if (selectedPiece) {
          board[r][c] = selectedPiece
          setBoard([...board])
        }
      }} />
      <FENExport fen={boardToFEN(board)} />
    </div>
  )
}
```

### Example 3: 囲碁局面エディタ

```typescript
const GoBoardEditor = () => {
  const [board, setBoard] = useState<GoStone[][]>(createEmptyGoBoard())
  const [selectedStone, setSelectedStone] = useState<'black' | 'white' | null>(null)

  return (
    <div>
      <StonePalette onSelect={setSelectedStone} />
      <GoBoard board={board} onIntersectionClick={(r, c) => {
        if (selectedStone) {
          board[r][c] = { color: selectedStone }
          setBoard([...board])
        } else {
          board[r][c] = null
          setBoard([...board])
        }
      }} />
      <SGFExport sgf={boardToSGF(board)} />
    </div>
  )
}
```

### Example 4: 詰将棋問題作成ツール

```tsx
const TsumeMaker = () => {
  const [board, setBoard] = useState(createEmptyBoard())
  const [difficulty, setDifficulty] = useState<'easy' | 'medium' | 'hard'>('easy')

  const handleSaveProblem = async () => {
    const sfen = boardToSFEN(board)
    await fetch('/api/tsume/create', {
      method: 'POST',
      body: JSON.stringify({ sfen, difficulty })
    })
    alert('問題を保存しました')
  }

  return (
    <PositionEditor
      board={board}
      onBoardChange={setBoard}
      extraControls={
        <>
          <label>
            難易度:
            <select value={difficulty} onChange={e => setDifficulty(e.target.value)}>
              <option value="easy">初級</option>
              <option value="medium">中級</option>
              <option value="hard">上級</option>
            </select>
          </label>
          <button onClick={handleSaveProblem}>問題を保存</button>
        </>
      }
    />
  )
}
```

## Guidelines

1. **駒パレットはスクロール可能に**: 駒数が多い場合（将棋14種×2色=28種）、縦スクロールまたはタブ切替で表示

2. **選択状態を視覚的に明示**: 選択中の駒に `selected` クラスを付与し、背景色やボーダーで強調

3. **消去モードは選択解除で実現**: `selectedPiece = null` にすることで、クリック時に駒を削除

4. **SFEN 自動更新**: 盤面が変更されたら `useMemo` または `useEffect` で SFEN を自動生成し、リアルタイム表示

5. **検証は手動トリガー**: 盤面変更のたびに API 検証すると重いので、「検証ボタン」クリック時のみ実行

6. **エラー表示は詳細に**: 「無効な局面」だけでなく、「5筋に先手の歩が2枚」のように具体的に表示

7. **プリセットは汎用的に**: 初期局面、定跡局面、詰将棋テンプレート等を JSON で管理し、ドロップダウンで選択可能に

8. **undo/redo 機能**: 盤面履歴をスタックで管理し、操作の取り消し/やり直しを実装すると UX 向上

9. **モバイル対応**: タッチデバイスでは駒パレットをモーダルにし、盤面をピンチズーム可能に

10. **キーボードショートカット**: `Ctrl+Z` (undo), `Ctrl+Y` (redo), `Escape` (選択解除) を実装すると効率的
