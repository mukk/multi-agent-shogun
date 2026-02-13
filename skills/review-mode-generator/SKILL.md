---
name: review-mode-generator
description: 感想戦モード実装パターン。対局後の詳細分析（手順再生+AI推奨手表示+分岐試行+形勢グラフ+ヒートマップ）を含む。状態キャッシュによる高速ナビゲーション、手番ごとのAI分析、分岐作成・保存、評価値グラフとの連動を実装。ボードゲーム（将棋・チェス・囲碁）の戦術学習、棋譜解説、AIとの比較分析に使用。
---

# Review Mode Generator

## Overview

対局後の詳細分析モード（感想戦）を実装するためのスキル。以下の機能を含む:

- **手順再生**: 棋譜を1手ずつ進む/戻る、任意の手番にジャンプ
- **AI推奨手表示**: 各局面でのAI最善手を取得・矢印で可視化
- **分岐試行**: 別の手を試して「もしこう指したら？」を探索
- **形勢グラフ**: 手番ごとの評価値をグラフ表示、クリックでジャンプ
- **ヒートマップ**: マスごとの選択確率を色で可視化
- **状態キャッシュ**: 盤面状態を `Map` でキャッシュして高速ナビゲーション
- **悪手マーカー**: 評価値が急落した手を自動検出・マーク

## When to Use

- 対局後の振り返り機能を実装する
- AI推奨手と実際の手を比較して学習させる
- 「もし別の手を指したら？」を試せる分岐機能を追加する
- 棋譜解説ページを作る（評価値グラフ+コメント付き）
- トーナメント結果のリプレイ機能を作る

## Instructions

### Step 1: API エンドポイントの設計

感想戦モードに必要なAPIエンドポイントを定義する。

```haskell
-- Backend (Haskell + Servant)
type ReviewAPI =
  "api" :> "game" :> Capture "gameId" Text :> "review"
    :> "state" :> Capture "moveNumber" Int
    :> Get '[JSON] (ApiResponse GameState)
  :<|> "api" :> "game" :> Capture "gameId" Text :> "review"
    :> "best-move" :> Capture "moveNumber" Int
    :> Get '[JSON] (ApiResponse BestMoveResult)
  :<|> "api" :> "game" :> Capture "gameId" Text :> "review"
    :> "branch" :> ReqBody '[JSON] BranchRequest
    :> Post '[JSON] (ApiResponse BranchResponse)

data BestMoveResult = BestMoveResult
  { bmrMove  :: !MoveInput
  , bmrEval  :: !Int
  , bmrDepth :: !Int
  } deriving (Show, Generic)

data BranchRequest = BranchRequest
  { brGameId     :: !Text
  , brMoveNumber :: !Int
  , brMove       :: !MoveInput
  } deriving (Show, Generic)
```

```typescript
// Frontend API client
export interface BestMoveResult {
  move: MoveInput
  eval: number
  depth: number
}

export async function getReviewState(
  gameId: string,
  moveNumber: number
): Promise<GameState> {
  return requestWithTimeout(`/game/${gameId}/review/state/${moveNumber}`)
}

export async function getReviewBestMove(
  gameId: string,
  moveNumber: number
): Promise<BestMoveResult> {
  return requestWithTimeout(`/game/${gameId}/review/best-move/${moveNumber}`)
}

export async function createReviewBranch(
  gameId: string,
  moveNumber: number,
  move: MoveInput
): Promise<{ branchId: string }> {
  return requestWithTimeout(`/game/${gameId}/review/branch`, {
    method: 'POST',
    body: JSON.stringify({ gameId, moveNumber, move }),
  })
}
```

### Step 2: 状態管理とキャッシュ

React hooks で状態管理し、`useRef` で盤面状態をキャッシュする。

```typescript
import { useState, useEffect, useRef } from 'react'
import type { GameState, MoveRecord } from '../types'

function ReviewMode() {
  // 元対局データ
  const [history, setHistory] = useState<MoveRecord[]>([])
  const [evalData, setEvalData] = useState<EvalPoint[]>([])

  // ナビゲーション状態
  const [currentMove, setCurrentMove] = useState(0)
  const [boardState, setBoardState] = useState<GameState | null>(null)

  // AI 最善手
  const [bestMove, setBestMove] = useState<BestMoveResult | null>(null)

  // 状態キャッシュ（高速ナビゲーション）
  const stateCache = useRef<Map<number, GameState>>(new Map())

  // 対局データ読み込み
  useEffect(() => {
    async function load() {
      const hist = await getHistory(gameId)
      const evals = await getEvaluations(gameId)
      setHistory(hist)
      setEvalData(evals)

      // 最終局面から開始
      const finalMove = hist.length
      setCurrentMove(finalMove)
      const state = await fetchState(finalMove)
      setBoardState(state)
    }
    load()
  }, [gameId])

  // 盤面状態取得（キャッシュ利用）
  async function fetchState(moveNumber: number): Promise<GameState> {
    const cached = stateCache.current.get(moveNumber)
    if (cached) return cached

    const state = await getReviewState(gameId, moveNumber)
    stateCache.current.set(moveNumber, state)
    return state
  }

  return (
    <div className="review-mode">
      <Board
        board={boardState?.board || []}
        highlights={[]}
        onSquareClick={handleSquareClick}
      />
      <EvalGraph data={evalData} currentMove={currentMove} onClick={handleGraphClick} />
      <KifuPanel history={history} currentMove={currentMove} onJump={handleJump} />
    </div>
  )
}
```

### Step 3: ナビゲーション機能

手順を進む/戻る、任意の手番にジャンプする。

```typescript
const handleNext = useCallback(async () => {
  const nextMove = Math.min(currentMove + 1, history.length)
  setCurrentMove(nextMove)
  const state = await fetchState(nextMove)
  setBoardState(state)
}, [currentMove, history.length])

const handlePrev = useCallback(async () => {
  const prevMove = Math.max(currentMove - 1, 0)
  setCurrentMove(prevMove)
  const state = await fetchState(prevMove)
  setBoardState(state)
}, [currentMove])

const handleJump = useCallback(async (moveNumber: number) => {
  setCurrentMove(moveNumber)
  const state = await fetchState(moveNumber)
  setBoardState(state)
}, [])

// 評価グラフクリックでジャンプ
const handleGraphClick = useCallback((moveNumber: number) => {
  handleJump(moveNumber)
}, [handleJump])
```

### Step 4: AI 最善手表示

ボタンクリックでAI推奨手を取得し、矢印で可視化する。

```typescript
const [showHint, setShowHint] = useState(false)

const handleShowBestMove = useCallback(async () => {
  setIsLoadingBestMove(true)
  try {
    const result = await getReviewBestMove(gameId, currentMove)
    setBestMove(result)
    setShowHint(true)
  } catch (err) {
    console.error('Best move fetch failed:', err)
  } finally {
    setIsLoadingBestMove(false)
  }
}, [gameId, currentMove])

return (
  <div className="review-mode">
    <Board
      board={boardState?.board || []}
      highlights={[]}
      hintArrow={showHint && bestMove ? { from: bestMove.move.from, to: bestMove.move.to } : null}
    />
    <button onClick={handleShowBestMove} disabled={isLoadingBestMove}>
      AI推奨手を表示
    </button>
  </div>
)
```

### Step 5: 分岐試行

別の手を試して新しいゲームIDを作成し、そちらに遷移する。

```typescript
const handleTryMove = useCallback(async (move: MoveInput) => {
  try {
    const response = await createReviewBranch(gameId, currentMove, move)
    // 新しいゲームIDに遷移
    navigate(`/game/${response.branchId}/review`)
  } catch (err) {
    console.error('Branch creation failed:', err)
  }
}, [gameId, currentMove, navigate])

const handleSquareClick = useCallback((pos: Position) => {
  // マス選択→別の手を試す
  if (selectedFrom) {
    const move = { from: selectedFrom, to: pos }
    handleTryMove(move)
  } else {
    setSelectedFrom(pos)
  }
}, [selectedFrom, handleTryMove])
```

### Step 6: 形勢グラフ統合

評価値の推移をグラフ表示し、クリックで該当局面にジャンプする。

```typescript
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, ReferenceLine } from 'recharts'

interface EvalGraphProps {
  data: EvalPoint[]
  currentMove: number
  onClick: (moveNumber: number) => void
}

function EvalGraph({ data, currentMove, onClick }: EvalGraphProps) {
  return (
    <LineChart data={data} onClick={(e) => onClick(e.activeLabel)}>
      <CartesianGrid strokeDasharray="3 3" />
      <XAxis dataKey="moveNumber" label={{ value: '手数', position: 'insideBottom' }} />
      <YAxis label={{ value: '評価値', angle: -90, position: 'insideLeft' }} />
      <Tooltip />
      <ReferenceLine y={0} stroke="gray" strokeDasharray="3 3" />
      <Line type="monotone" dataKey="eval" stroke="#8884d8" dot={{ r: 3 }} />
      <ReferenceLine x={currentMove} stroke="red" strokeWidth={2} />
    </LineChart>
  )
}
```

## Examples

### Example 1: 基本的な感想戦モード

```typescript
export default function ReviewMode() {
  const { id: gameId } = useParams<{ id: string }>()
  const [history, setHistory] = useState<MoveRecord[]>([])
  const [currentMove, setCurrentMove] = useState(0)
  const [boardState, setBoardState] = useState<GameState | null>(null)

  useEffect(() => {
    async function load() {
      const hist = await getHistory(gameId)
      setHistory(hist)
      setCurrentMove(hist.length)
      const state = await getReviewState(gameId, hist.length)
      setBoardState(state)
    }
    load()
  }, [gameId])

  return (
    <div>
      <Board board={boardState?.board || []} />
      <button onClick={() => setCurrentMove(c => Math.max(0, c - 1))}>戻る</button>
      <button onClick={() => setCurrentMove(c => Math.min(history.length, c + 1))}>進む</button>
    </div>
  )
}
```

### Example 2: 悪手マーカーの自動検出

```typescript
function detectBlunders(evalData: EvalPoint[]): number[] {
  const blunderMoves: number[] = []
  for (let i = 1; i < evalData.length; i++) {
    const evalDrop = evalData[i].eval - evalData[i - 1].eval
    if (evalDrop < -200) {  // 評価値が200以上下がった手
      blunderMoves.push(evalData[i].moveNumber)
    }
  }
  return blunderMoves
}
```

## Guidelines

1. **状態キャッシュは必須**: `useRef<Map<number, GameState>>` で盤面状態をキャッシュ。毎回APIから取得すると遅い
2. **非同期取得は並列化**: `Promise.all([getHistory(...), getEvaluations(...)])` で複数APIを同時取得
3. **デフォルトは最終局面**: 初回表示は最終手から開始する（`setCurrentMove(history.length)`）
4. **グラフクリックでジャンプ**: 評価値グラフをインタラクティブにすると UX 向上
5. **分岐は新ゲームID**: 元の棋譜を上書きせず、新しいゲームIDで分岐を作成
6. **キーボードショートカット**: 矢印キー←→で進む/戻るを実装すると便利
7. **ローディング表示**: 盤面取得中はスピナーを表示
8. **エラーハンドリング**: API失敗時にフォールバック（元の棋譜に戻る等）
9. **モバイル対応**: タッチデバイスではスワイプで進む/戻るも検討
10. **コメント機能**: 各局面にユーザーコメントを保存できると学習に役立つ
