---
name: ai-selfplay-engine
description: AI同士を自動対局させるエンジンの実装パターン。並列実行管理（Semaphore + async）、進捗追跡（スレッドセーフ状態変数 + ポーリング）、結果永続化（JSON即時保存）、管理API（開始/停止/状態取得）、管理UI（設定フォーム + 進捗バー + 結果テーブル）を含む。ゲームAI（将棋・チェス・囲碁等）の強度テスト、評価関数比較、棋譜データ生成、リグレッションテストに使用。ゲーム以外（LLM評価、シミュレーション等）の対戦型ベンチマークにも適用可能。
---

# AI Self-Play Engine

## Overview

AI同士を自動対局させるエンジンの実装パターン。以下の構成要素を一貫して実装する：

- **Config型**: 対局数、探索深度（先手/後手）、バリアント、最大並列数等の設定
- **Result型**: 個別対局の結果（勝敗、手数、評価値、棋譜）
- **Status型**: セッション全体の進行状況（総数、完了数、実行中数）
- **Session型**: スレッドセーフなミュータブル状態（TVar / Mutex / AtomicReference）
- **対局ループ**: AI同士が交互に手を打つ再帰ループ（手数上限付き）
- **並列管理**: Semaphore + async による有界並列実行
- **結果保存**: 完了した対局をJSONファイルとして即時保存
- **API層**: 開始・停止・状態取得・結果取得のエンドポイント
- **管理UI**: 設定フォーム + 進捗バー + 結果テーブル + ポーリング

## When to Use

- AI同士を大量に自動対局させて強度テストを行う
- 新旧の評価関数（パラメータ）を対戦させてElo推定する
- 学習用の棋譜データを大量に自動生成する
- AIエンジン変更後のリグレッションテスト（性能劣化検証）
- LLM同士の対話評価やシミュレーションの自動実行
- 異なる探索深度・アルゴリズムのAIを比較ベンチマークする

## Instructions

### Step 1: Config / Result / Status 型の定義

対戦の設定、個別結果、セッション全体の進捗を表す3つのデータ型を定義する。

#### Haskell

```haskell
data SelfPlayConfig = SelfPlayConfig
  { spNumGames      :: !Int       -- 対局数
  , spSenteDepth    :: !Int       -- 先手の探索深度
  , spGoteDepth     :: !Int       -- 後手の探索深度
  , spVariant       :: !Text      -- バリアント名
  , spMaxConcurrent :: !Int       -- 最大並列数
  } deriving (Show, Generic)

data SelfPlayResult = SelfPlayResult
  { sprGameId    :: !Int          -- 対局番号
  , sprResult    :: !Text         -- "sente_win" | "gote_win" | "draw"
  , sprMoves     :: !Int          -- 手数
  , sprFinalEval :: !Int          -- 最終評価値
  , sprKifu      :: ![Text]       -- 棋譜（指し手の配列）
  } deriving (Show, Generic)

data SelfPlayStatus = SelfPlayStatus
  { spsTotal      :: !Int         -- 総対局数
  , spsCompleted  :: !Int         -- 完了数
  , spsInProgress :: !Int         -- 実行中数
  , spsResults    :: ![SelfPlayResult]
  } deriving (Show, Generic)
```

#### TypeScript

```typescript
interface SelfPlayConfig {
  numGames: number
  senteDepth: number
  goteDepth: number
  variant: string
  maxConcurrent: number
}

interface SelfPlayResult {
  gameId: number
  result: 'sente_win' | 'gote_win' | 'draw'
  moves: number
  finalEval: number
  kifu: string[]
}

interface SelfPlayStatus {
  total: number
  completed: number
  inProgress: number
  results: SelfPlayResult[]
}
```

#### Python

```python
from dataclasses import dataclass, field

@dataclass
class SelfPlayConfig:
    num_games: int
    sente_depth: int
    gote_depth: int
    variant: str
    max_concurrent: int

@dataclass
class SelfPlayResult:
    game_id: int
    result: str           # "sente_win" | "gote_win" | "draw"
    moves: int
    final_eval: int
    kifu: list[str]

@dataclass
class SelfPlayStatus:
    total: int = 0
    completed: int = 0
    in_progress: int = 0
    results: list[SelfPlayResult] = field(default_factory=list)
```

**重要ポイント:**
- Config は不変（Immutable）、Status はスレッドセーフなミュータブル
- Result は完了した対局ごとに生成し、即座に永続化
- フィールドはシリアライズ時のキー名と一致させる（プレフィックスを剥がす等）

### Step 2: スレッドセーフな Session 管理

複数の対局が並列実行されるため、状態管理はスレッドセーフでなければならない。

#### Haskell（STM TVar）

```haskell
import Control.Concurrent.STM

newtype SelfPlaySession = SelfPlaySession (TVar SelfPlayStatus)

newSelfPlaySession :: IO SelfPlaySession
newSelfPlaySession = SelfPlaySession <$> newTVarIO (SelfPlayStatus 0 0 0 [])

-- 状態更新（アトミック）
updateStatus :: SelfPlaySession -> (SelfPlayStatus -> SelfPlayStatus) -> IO ()
updateStatus (SelfPlaySession var) f = atomically $ modifyTVar' var f

-- 状態取得
getStatus :: SelfPlaySession -> IO SelfPlayStatus
getStatus (SelfPlaySession var) = readTVarIO var
```

#### TypeScript（Mutex パターン）

```typescript
class SelfPlaySession {
  private status: SelfPlayStatus = { total: 0, completed: 0, inProgress: 0, results: [] }
  private lock = new Mutex()  // async-mutex パッケージ等

  async updateStatus(fn: (s: SelfPlayStatus) => SelfPlayStatus): Promise<void> {
    const release = await this.lock.acquire()
    try {
      this.status = fn(this.status)
    } finally {
      release()
    }
  }

  getStatus(): SelfPlayStatus {
    return { ...this.status }  // 防御コピー
  }
}
```

Node.js はシングルスレッドのため、同期的な状態更新であれば Mutex は不要。
ただし async/await の合間にインターリーブが発生するため、複雑な更新では Mutex が安全。

#### Python（asyncio.Lock）

```python
import asyncio

class SelfPlaySession:
    def __init__(self):
        self.status = SelfPlayStatus()
        self._lock = asyncio.Lock()

    async def update_status(self, fn):
        async with self._lock:
            self.status = fn(self.status)

    def get_status(self) -> SelfPlayStatus:
        return self.status  # dataclass は値コピー
```

### Step 3: 対局ループの実装

AI同士が交互に手を打つ再帰ループ。手数上限・チェックメイト・ステイルメイトで終了。

```
擬似コード:
function gameLoop(position, moveCount, maxMoves, config):
    if moveCount >= maxMoves:
        return DrawResult(moveCount, evaluate(position))

    if isCheckmate(position):
        winner = opposite(position.turn)
        return WinResult(winner, moveCount, evaluate(position))

    if isStalemate(position):
        return DrawResult(moveCount, evaluate(position))

    depth = (position.turn == SENTE) ? config.senteDepth : config.goteDepth
    bestMove = search(position, depth)
    newPosition = applyMove(position, bestMove)

    return gameLoop(newPosition, moveCount + 1, maxMoves, config)
```

#### Haskell

```haskell
playSingleGame :: SelfPlayConfig -> Position -> Int -> IO SelfPlayResult
playSingleGame config initPos gameId = do
    let maxMoves = 500
    (result, moves, eval, kifu) <- gameLoop initPos 0 maxMoves [] config
    return SelfPlayResult
      { sprGameId    = gameId
      , sprResult    = result
      , sprMoves     = moves
      , sprFinalEval = eval
      , sprKifu      = kifu
      }
  where
    gameLoop pos moveCount maxMoves accKifu cfg
      | moveCount >= maxMoves = return ("draw", moveCount, evaluate pos, reverse accKifu)
      | isCheckmate pos = return (winnerText (opposite (turn pos)), moveCount, evaluate pos, reverse accKifu)
      | isStalemate pos = return ("draw", moveCount, evaluate pos, reverse accKifu)
      | otherwise = do
          let depth = if turn pos == Sente then spSenteDepth cfg else spGoteDepth cfg
              searchCfg = SearchConfig depth True True  -- depth, transTable, moveOrdering
          searchResult <- search searchCfg pos
          let bestMove = bestMoveFromResult searchResult
              newPos = makeMove pos bestMove
              notation = formatMove bestMove
          gameLoop newPos (moveCount + 1) maxMoves (notation : accKifu) cfg
```

#### TypeScript

```typescript
async function playSingleGame(
  config: SelfPlayConfig,
  initPosition: Position,
  gameId: number
): Promise<SelfPlayResult> {
  const maxMoves = 500
  let position = initPosition
  let moveCount = 0
  const kifu: string[] = []

  while (moveCount < maxMoves) {
    if (isCheckmate(position)) {
      return { gameId, result: winnerText(opposite(position.turn)), moves: moveCount, finalEval: evaluate(position), kifu }
    }
    if (isStalemate(position)) {
      return { gameId, result: 'draw', moves: moveCount, finalEval: evaluate(position), kifu }
    }

    const depth = position.turn === 'sente' ? config.senteDepth : config.goteDepth
    const bestMove = await search(position, depth)
    kifu.push(formatMove(bestMove))
    position = applyMove(position, bestMove)
    moveCount++
  }

  return { gameId, result: 'draw', moves: moveCount, finalEval: evaluate(position), kifu }
}
```

#### Python

```python
async def play_single_game(config: SelfPlayConfig, init_pos, game_id: int) -> SelfPlayResult:
    max_moves = 500
    position = init_pos
    move_count = 0
    kifu = []

    while move_count < max_moves:
        if is_checkmate(position):
            winner = opposite(position.turn)
            return SelfPlayResult(game_id, f"{winner}_win", move_count, evaluate(position), kifu)
        if is_stalemate(position):
            return SelfPlayResult(game_id, "draw", move_count, evaluate(position), kifu)

        depth = config.sente_depth if position.turn == "sente" else config.gote_depth
        best_move = await search(position, depth)
        kifu.append(format_move(best_move))
        position = apply_move(position, best_move)
        move_count += 1

    return SelfPlayResult(game_id, "draw", move_count, evaluate(position), kifu)
```

**重要ポイント:**
- 手数上限（500手等）を必ず設ける。無限ループ防止
- 探索深度は先手/後手で非対称に設定可能（強度比較のため）
- 棋譜（kifu）は各手を文字列として蓄積し、結果に含める
- 評価値は最終局面のものを記録（学習データとして有用）

### Step 4: 有界並列実行

Semaphore で同時実行数を制限しつつ、全対局を並列実行する。

#### Haskell（QSem + async）

```haskell
import Control.Concurrent.QSem
import Control.Concurrent.Async
import Control.Exception (bracket_, try, SomeException)

runAllGames :: SelfPlayConfig -> SelfPlaySession -> Position -> IO ()
runAllGames config session initPos = do
    sem <- newQSem (spMaxConcurrent config)

    -- 全対局を非同期タスクとして起動
    tasks <- forM [1..spNumGames config] $ \gameId -> async $ do
        bracket_ (waitQSem sem) (signalQSem sem) $ do
            -- 実行中カウンタを増加
            updateStatus session $ \s -> s { spsInProgress = spsInProgress s + 1 }
            result <- try $ playSingleGame config initPos gameId
            case result of
                Right r -> do
                    saveResult r  -- JSON即時保存
                    updateStatus session $ \s -> s
                        { spsCompleted  = spsCompleted s + 1
                        , spsInProgress = spsInProgress s - 1
                        , spsResults    = spsResults s ++ [r]
                        }
                Left (e :: SomeException) -> do
                    updateStatus session $ \s -> s
                        { spsInProgress = spsInProgress s - 1 }

    -- 全タスク完了を待機
    mapM_ wait tasks
```

#### TypeScript（p-limit）

```typescript
import pLimit from 'p-limit'

async function runAllGames(
  config: SelfPlayConfig,
  session: SelfPlaySession,
  initPosition: Position
): Promise<void> {
  const limit = pLimit(config.maxConcurrent)

  const tasks = Array.from({ length: config.numGames }, (_, i) =>
    limit(async () => {
      const gameId = i + 1
      await session.updateStatus(s => ({
        ...s,
        inProgress: s.inProgress + 1
      }))

      try {
        const result = await playSingleGame(config, initPosition, gameId)
        await saveResult(result)
        await session.updateStatus(s => ({
          ...s,
          completed: s.completed + 1,
          inProgress: s.inProgress - 1,
          results: [...s.results, result]
        }))
      } catch (e) {
        await session.updateStatus(s => ({
          ...s,
          inProgress: s.inProgress - 1
        }))
      }
    })
  )

  await Promise.all(tasks)
}
```

#### Python（asyncio.Semaphore）

```python
import asyncio

async def run_all_games(config: SelfPlayConfig, session: SelfPlaySession, init_pos):
    sem = asyncio.Semaphore(config.max_concurrent)

    async def run_one(game_id: int):
        async with sem:
            await session.update_status(
                lambda s: SelfPlayStatus(s.total, s.completed, s.in_progress + 1, s.results)
            )
            try:
                result = await play_single_game(config, init_pos, game_id)
                await save_result(result)
                await session.update_status(
                    lambda s: SelfPlayStatus(s.total, s.completed + 1, s.in_progress - 1, s.results + [result])
                )
            except Exception:
                await session.update_status(
                    lambda s: SelfPlayStatus(s.total, s.completed, s.in_progress - 1, s.results)
                )

    tasks = [asyncio.create_task(run_one(i + 1)) for i in range(config.num_games)]
    await asyncio.gather(*tasks)
```

**重要ポイント:**
- `maxConcurrent` はCPUコア数以下を推奨
- `bracket_`（Haskell）/ `try-finally`（TS/Python）で例外時もSemaphoreを確実に解放
- 対局完了ごとに即時ファイル保存（プロセスクラッシュ時のデータロスを最小化）
- エラー時は `inProgress` を減らすが、リトライはしない（結果が欠損として記録される）

### Step 5: 結果の即時永続化

対局が完了するたびにJSONファイルとして保存する。

```
ディレクトリ構造:
data/selfplay/
  game_001.json
  game_002.json
  ...
```

#### Haskell

```haskell
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode)
import System.Directory (createDirectoryIfMissing)

saveResult :: SelfPlayResult -> IO ()
saveResult result = do
    createDirectoryIfMissing True "data/selfplay"
    let filename = "data/selfplay/game_" ++ show (sprGameId result) ++ ".json"
    BL.writeFile filename (encode result)
```

#### TypeScript

```typescript
import { writeFile, mkdir } from 'fs/promises'

async function saveResult(result: SelfPlayResult): Promise<void> {
  await mkdir('data/selfplay', { recursive: true })
  const filename = `data/selfplay/game_${String(result.gameId).padStart(3, '0')}.json`
  await writeFile(filename, JSON.stringify(result, null, 2))
}
```

#### Python

```python
import json
from pathlib import Path

async def save_result(result: SelfPlayResult):
    Path("data/selfplay").mkdir(parents=True, exist_ok=True)
    filename = f"data/selfplay/game_{result.game_id:03d}.json"
    Path(filename).write_text(json.dumps(vars(result), ensure_ascii=False, indent=2))
```

### Step 6: REST API エンドポイント

3つ（+キャンセル用1つ）のエンドポイントを提供する。

```
POST /api/selfplay/start    → セッション開始（Config を受け取り、バックグラウンドで実行開始）
GET  /api/selfplay/status   → 進捗取得（total, completed, inProgress）
GET  /api/selfplay/results  → 結果一覧取得（完了した対局の配列）
POST /api/selfplay/cancel   → 実行中のセッションをキャンセル
```

#### Haskell（Servant）

```haskell
-- Routes.hs
type SelfPlayAPI =
       "api" :> "selfplay" :> "start"   :> ReqBody '[JSON] StartSelfPlayRequest :> Post '[JSON] StartSelfPlayResponse
  :<|> "api" :> "selfplay" :> "status"  :> Get '[JSON] SelfPlayStatus
  :<|> "api" :> "selfplay" :> "results" :> Get '[JSON] [SelfPlayResult]

-- Handlers.hs
handleStartSelfPlay :: AppState -> StartSelfPlayRequest -> Handler StartSelfPlayResponse
handleStartSelfPlay appState req = do
    let config = requestToConfig req
        initPos = setupPosition (spVariant config)
    liftIO $ startSelfPlay config (appSelfPlay appState) initPos
    return $ StartSelfPlayResponse "session-1"

handleSelfPlayStatus :: AppState -> Handler SelfPlayStatus
handleSelfPlayStatus appState = liftIO $ getStatus (appSelfPlay appState)

handleSelfPlayResults :: AppState -> Handler [SelfPlayResult]
handleSelfPlayResults appState = liftIO $ getResults (appSelfPlay appState)
```

#### TypeScript（Express）

```typescript
const router = express.Router()
const session = new SelfPlaySession()

router.post('/api/selfplay/start', async (req, res) => {
  const config: SelfPlayConfig = req.body
  const initPosition = setupPosition(config.variant)

  await session.updateStatus(() => ({
    total: config.numGames, completed: 0, inProgress: 0, results: []
  }))

  // バックグラウンドで実行開始（await しない）
  runAllGames(config, session, initPosition).catch(console.error)

  res.json({ sessionId: 'session-1' })
})

router.get('/api/selfplay/status', (req, res) => {
  res.json(session.getStatus())
})

router.get('/api/selfplay/results', (req, res) => {
  const status = session.getStatus()
  res.json(status.results)
})
```

#### Python（FastAPI）

```python
from fastapi import FastAPI

app = FastAPI()
session = SelfPlaySession()

@app.post("/api/selfplay/start")
async def start_selfplay(config: SelfPlayConfig):
    init_pos = setup_position(config.variant)
    session.status = SelfPlayStatus(total=config.num_games)

    # バックグラウンドで実行開始
    asyncio.create_task(run_all_games(config, session, init_pos))

    return {"session_id": "session-1"}

@app.get("/api/selfplay/status")
async def get_status():
    return session.get_status()

@app.get("/api/selfplay/results")
async def get_results():
    return session.get_status().results
```

**重要ポイント:**
- `start` はバックグラウンド実行を開始して即座にレスポンスを返す
- クライアントは `status` をポーリングして進捗を取得する
- `results` は完了済みの対局のみを返す

### Step 7: キャンセル機構

実行中のセッションを安全に中断するパターン。

```
方針:
1. Session にキャンセルフラグ（AtomicBool / Event）を持たせる
2. 対局ループの各手でフラグをチェック
3. フラグが立っていたら早期終了
4. 新規対局のスケジューリングを停止
```

#### Haskell

```haskell
data SelfPlaySession = SelfPlaySession
  { sessionStatus   :: TVar SelfPlayStatus
  , sessionCancelled :: TVar Bool
  }

cancel :: SelfPlaySession -> IO ()
cancel sess = atomically $ writeTVar (sessionCancelled sess) True

-- 対局ループ内でチェック
gameLoop pos moveCount maxMoves accKifu cfg sess = do
    cancelled <- readTVarIO (sessionCancelled sess)
    if cancelled
      then return ("cancelled", moveCount, evaluate pos, reverse accKifu)
      else ...  -- 通常の処理
```

#### TypeScript

```typescript
class SelfPlaySession {
  private cancelled = false

  cancel(): void { this.cancelled = true }
  isCancelled(): boolean { return this.cancelled }

  reset(): void { this.cancelled = false }
}

// 対局ループ内でチェック
while (moveCount < maxMoves) {
  if (session.isCancelled()) {
    return { gameId, result: 'cancelled', moves: moveCount, finalEval: evaluate(position), kifu }
  }
  // ... 通常の処理
}
```

#### Python

```python
class SelfPlaySession:
    def __init__(self):
        self._cancelled = asyncio.Event()

    def cancel(self):
        self._cancelled.set()

    def is_cancelled(self) -> bool:
        return self._cancelled.is_set()

    def reset(self):
        self._cancelled.clear()
```

### Step 8: 管理UI（フロントエンド）

設定フォーム、進捗表示、結果テーブルを組み合わせた管理パネル。

#### React

```tsx
type SessionState = 'idle' | 'running' | 'done'

function SelfPlayPanel() {
  const [state, setState] = useState<SessionState>('idle')
  const [config, setConfig] = useState<SelfPlayConfig>({
    numGames: 10, senteDepth: 3, goteDepth: 3, variant: 'standard', maxConcurrent: 4
  })
  const [status, setStatus] = useState<SelfPlayStatus | null>(null)

  // ポーリング（実行中のみ）
  useEffect(() => {
    if (state !== 'running') return
    const interval = setInterval(async () => {
      const s = await fetch('/api/selfplay/status').then(r => r.json())
      setStatus(s)
      if (s.completed >= s.total) {
        setState('done')
        clearInterval(interval)
      }
    }, 2000)
    return () => clearInterval(interval)
  }, [state])

  const handleStart = async () => {
    await fetch('/api/selfplay/start', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(config)
    })
    setState('running')
  }

  return (
    <div>
      {/* 設定フォーム（idle時のみ） */}
      {state === 'idle' && (
        <form onSubmit={e => { e.preventDefault(); handleStart() }}>
          <label>対局数 <input type="number" value={config.numGames}
            onChange={e => setConfig(c => ({ ...c, numGames: +e.target.value }))} /></label>
          <label>先手深度 <input type="number" value={config.senteDepth}
            onChange={e => setConfig(c => ({ ...c, senteDepth: +e.target.value }))} /></label>
          <label>後手深度 <input type="number" value={config.goteDepth}
            onChange={e => setConfig(c => ({ ...c, goteDepth: +e.target.value }))} /></label>
          <label>並列数 <input type="number" value={config.maxConcurrent}
            onChange={e => setConfig(c => ({ ...c, maxConcurrent: +e.target.value }))} /></label>
          <button type="submit">開始</button>
        </form>
      )}

      {/* 進捗バー（running時） */}
      {state === 'running' && status && (
        <div>
          <progress value={status.completed} max={status.total} />
          <span>{status.completed} / {status.total} 完了（実行中: {status.inProgress}）</span>
        </div>
      )}

      {/* 結果テーブル（結果がある場合） */}
      {status && status.results.length > 0 && (
        <table>
          <thead>
            <tr><th>#</th><th>結果</th><th>手数</th><th>評価値</th></tr>
          </thead>
          <tbody>
            {status.results.map(r => (
              <tr key={r.gameId}>
                <td>{r.gameId}</td>
                <td>{r.result}</td>
                <td>{r.moves}</td>
                <td>{r.finalEval}</td>
              </tr>
            ))}
          </tbody>
        </table>
      )}

      {/* 統計サマリ（done時） */}
      {state === 'done' && status && (
        <div>
          <p>先手勝率: {(status.results.filter(r => r.result === 'sente_win').length / status.total * 100).toFixed(1)}%</p>
          <p>後手勝率: {(status.results.filter(r => r.result === 'gote_win').length / status.total * 100).toFixed(1)}%</p>
          <p>引分: {status.results.filter(r => r.result === 'draw').length}</p>
          <p>平均手数: {(status.results.reduce((sum, r) => sum + r.moves, 0) / status.total).toFixed(1)}</p>
          <button onClick={() => { setState('idle'); setStatus(null) }}>新規セッション</button>
        </div>
      )}
    </div>
  )
}
```

#### APIクライアント

```typescript
// selfplayApi.ts
const BASE = '/api/selfplay'

export async function startSelfPlay(config: SelfPlayConfig) {
  const res = await fetch(`${BASE}/start`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(config)
  })
  return res.json()
}

export async function getSelfPlayStatus(): Promise<SelfPlayStatus> {
  const res = await fetch(`${BASE}/status`)
  return res.json()
}

export async function getSelfPlayResults(): Promise<SelfPlayResult[]> {
  const res = await fetch(`${BASE}/results`)
  return res.json()
}

export async function cancelSelfPlay(): Promise<void> {
  await fetch(`${BASE}/cancel`, { method: 'POST' })
}
```

## Examples

### Example 1: 将棋AIの探索深度比較

異なる探索深度のAI同士を対戦させ、深度ごとの勝率を測定する。

```typescript
// 深度3 vs 深度5 で100局
const config: SelfPlayConfig = {
  numGames: 100,
  senteDepth: 3,
  goteDepth: 5,
  variant: 'standard',
  maxConcurrent: 4
}

// 実行後の統計例:
// 先手(深度3) 勝率: 28.0%
// 後手(深度5) 勝率: 65.0%
// 引分: 7.0%
// → 深度5が有意に強い
```

### Example 2: 評価関数のA/Bテスト

新旧の評価関数を対戦させてElo推定する。

```python
# 評価関数v1（先手） vs 評価関数v2（後手） × 200局
# その後、先後を入れ替えて200局
# 合計400局で公平なElo推定

config_a = SelfPlayConfig(
    num_games=200, sente_depth=4, gote_depth=4,
    variant="standard", max_concurrent=8
)

config_b = SelfPlayConfig(
    num_games=200, sente_depth=4, gote_depth=4,
    variant="standard", max_concurrent=8
)

# config_a: v1=先手, v2=後手
# config_b: v2=先手, v1=後手
# 先後の偏りを排除して評価関数の純粋な強度を比較
```

### Example 3: LLM対話評価ベンチマーク

ゲームAI以外への応用例。LLM同士の対話品質を自動評価する。

```python
@dataclass
class LLMBattleConfig:
    num_rounds: int        # 対戦ラウンド数
    model_a: str           # "gpt-4"
    model_b: str           # "claude-3"
    judge_model: str       # 審判モデル
    max_concurrent: int    # 並列数
    prompts: list[str]     # 評価用プロンプト集

@dataclass
class LLMBattleResult:
    round_id: int
    prompt: str
    response_a: str
    response_b: str
    winner: str            # "model_a" | "model_b" | "tie"
    judge_reasoning: str

# 同じ並列実行 + 進捗管理 + 結果永続化パターンが適用可能
# Semaphore でAPIレートリミットを管理
# 各ラウンドの結果をJSON保存
# UIで進捗表示 + 勝率集計
```

### Example 4: ミニ将棋の棋譜データ生成

学習用の棋譜を大量生成する。

```haskell
-- 5x5ミニ将棋の棋譜を10000局生成
let config = SelfPlayConfig
      { spNumGames      = 10000
      , spSenteDepth    = 3     -- 浅い探索で高速生成
      , spGoteDepth     = 3
      , spVariant       = "mini"
      , spMaxConcurrent = 8     -- 8並列
      }

-- 生成された棋譜ファイル:
-- data/selfplay/game_0001.json ~ game_10000.json
-- 各ファイルに kifu フィールドで指し手列が保存される
-- これを機械学習の訓練データとして使用
```

## Guidelines

1. **手数上限を必ず設ける**: 500手程度の上限を設定し、千日手や無限ループを防止する。上限到達時は引き分けとして記録

2. **Semaphore で並列数を制限**: `maxConcurrent` はCPUコア数以下を推奨。探索エンジンがCPU集約型のため、過剰な並列はスラッシングを引き起こす

3. **例外安全な Semaphore 管理**: `bracket_`（Haskell）/ `try-finally`（JS/Python）で対局が例外終了してもSemaphoreが確実に解放されるようにする

4. **対局完了ごとに即時保存**: プロセスクラッシュ時のデータロスを最小化するため、メモリだけでなくファイルにも即座に書き出す

5. **先後入れ替えで公平性を確保**: 評価関数の比較時は、先手/後手を入れ替えた対局も同数実行し、先手有利のバイアスを排除する

6. **ステートマシンで UI 状態管理**: `idle → running → done` の3状態で管理し、各状態で表示するコンポーネントを切り替える。`running` 中は設定変更不可

7. **ポーリング間隔は2秒程度**: 進捗取得のポーリングは2秒間隔が実用的。短すぎるとサーバー負荷、長すぎるとUX劣化

8. **キャンセル時は対局ループ内でチェック**: 新規対局のスケジューリング停止に加え、実行中の対局ループ内でもキャンセルフラグを確認し、早期終了する

9. **探索深度が深いほど計算時間は指数関数的に増加**: 深度5以上は1局あたり数分〜数十分かかる場合がある。大量対局テストでは深度3〜4を推奨

10. **結果のJSON構造を統一**: `gameId`, `result`, `moves`, `finalEval`, `kifu` の5フィールドを標準とし、ゲーム固有の情報は追加フィールドとして拡張する
