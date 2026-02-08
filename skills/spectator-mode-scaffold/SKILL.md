---
name: spectator-mode-scaffold
description: リアルタイムWebSocketゲームに観戦モード（読み取り専用ビュー）を追加する。観戦専用WSチャンネル、観戦者追跡・カウント配信、コマンドフィルタリング、読み取り専用UIを実装する際に使用。Haskell (STM + wai-websockets) と TypeScript (React) の両面をカバー。
---

# Spectator Mode Scaffold

## Overview

既存のWebSocket対戦ゲームに「観戦モード」を追加するアーキテクチャパターン。以下を一貫して実施する：

- **観戦専用WSチャンネル**: `/ws/spectate/{gameId}` で通常プレイヤーと分離
- **二重登録パターン**: クライアントリスト（broadcast受信用）＋ 観戦者リスト（カウント用）
- **コマンドフィルタリング**: 観戦者のゲーム操作コマンド（待った要求等）を拒否
- **観戦者数リアルタイム配信**: 接続/切断時に全クライアントへカウントbroadcast
- **読み取り専用UI**: クリック・ホバー無効化、観戦バナー表示

**前提**: websocket-game-sync パターンによるゲーム状態broadcastが既に実装されていること。本スキルはその基盤に乗る形で観戦機能を追加する。

## When to Use

- WebSocket対戦ゲームに観戦（スペクテーター）機能を追加したい
- 観戦者をプレイヤーと区別し、操作を読み取り専用に制限したい
- 観戦者数をリアルタイムで表示したい
- 既存のbroadcast基盤を再利用して観戦者への配信を実現したい

**使わない場面**:
- ゲーム前のロビー管理 → lobby-game-bridge
- プレイヤー間のゲーム状態同期 → websocket-game-sync

## Instructions

### Phase 1: バックエンド — 観戦者追跡Mapの追加

既存のWsStateに観戦者専用のマップを追加する。

#### Haskell

```haskell
data WsState = WsState
  { wsClients    :: TVar (Map Text (Map ClientId Connection))
    -- ^ gameId -> (clientId -> connection): 全クライアント（プレイヤー+観戦者）
  , wsSpectators :: TVar (Map Text (Map ClientId Connection))
    -- ^ gameId -> (spectatorClientId -> connection): 観戦者のみ
  , wsNextId     :: TVar Int
  }

newWsState :: IO WsState
newWsState = WsState
  <$> newTVarIO Map.empty
  <*> newTVarIO Map.empty
  <*> newTVarIO 0
```

#### TypeScript (Node.js)

```typescript
class WsState {
  private clients = new Map<string, Map<string, WebSocket>>()
  private spectators = new Map<string, Map<string, WebSocket>>()
  private nextId = 0
}
```

**ポイント**: 観戦者は `wsClients` と `wsSpectators` の**両方**に登録される。`wsClients` に入ることで、既存の `broadcast` 関数が観戦者にもメッセージを送信できる。`wsSpectators` は観戦者数カウントのためだけに使う。

### Phase 2: バックエンド — WSパス分類に観戦パスを追加

```haskell
data WsPath
  = WsGamePath Text       -- /ws/game/{gameId}     (プレイヤー)
  | WsSpectatePath Text    -- /ws/spectate/{gameId} (観戦者)
  | WsLobbyPath            -- /ws/lobby
  | WsUnknownPath

classifyPath :: PendingConnection -> WsPath
classifyPath pending =
  let segments = splitPath (requestPath pending)
  in case segments of
    ["ws", "game", gid]     -> WsGamePath gid
    ["ws", "spectate", gid] -> WsSpectatePath gid
    ["ws", "lobby"]          -> WsLobbyPath
    _                        -> WsUnknownPath
```

### Phase 3: バックエンド — 観戦者接続ハンドリング

観戦者接続時に二重登録 + カウントbroadcastを行う。

```haskell
wsApp state pending = do
  conn <- acceptRequest pending
  clientId <- nextClientId state
  let wsPath = classifyPath pending

  case wsPath of
    WsGamePath gameId -> do
      addClient state gameId clientId conn
    WsSpectatePath gameId -> do
      -- 二重登録: broadcastリスト + 観戦者リスト
      addClient state gameId clientId conn
      addSpectator state gameId clientId conn
      broadcastSpectatorCount state gameId
    ...

  let isSpectator = case wsPath of
        WsSpectatePath _ -> True
        _                -> False

  withPingThread conn 30 (return ()) $
    handleConnection state clientId conn isSpectator
```

### Phase 4: バックエンド — コマンドフィルタリング

観戦者からのゲーム操作コマンドを拒否する。ガード句（パターンマッチ）で分岐。

```haskell
handleConnection state clientId conn isSpectator = do
  ...
  let cleanup = do
        forM_ games $ \gid -> do
          removeClient state gid clientId
          when isSpectator $ do
            removeSpectator state gid clientId
            broadcastSpectatorCount state gid

  flip finally cleanup $ forever $ do
    raw <- receiveData conn
    case decodeCommand raw of
      Right (GameAlteringCommand _)
        | isSpectator -> sendError conn "Spectators cannot perform this action"
        | otherwise   -> handleGameCommand ...
      Right PingCommand -> sendPong conn
      Right (SubscribeCommand gid) -> subscribe ...
      Left err -> sendError conn err
```

**原則**: `subscribe`, `unsubscribe`, `ping` は許可。ゲーム状態を変更するコマンド（手の適用、待った要求等）は拒否。

### Phase 5: バックエンド — 観戦者数broadcast

```haskell
addSpectator :: WsState -> Text -> ClientId -> Connection -> IO ()
addSpectator state gameId clientId conn =
  atomically $ modifyTVar' (wsSpectators state) $
    Map.insertWith Map.union gameId (Map.singleton clientId conn)

removeSpectator :: WsState -> Text -> ClientId -> IO ()
removeSpectator state gameId clientId =
  atomically $ modifyTVar' (wsSpectators state) $
    Map.adjust (Map.delete clientId) gameId

getSpectatorCount :: WsState -> Text -> IO Int
getSpectatorCount state gameId = do
  specMap <- readTVarIO (wsSpectators state)
  return $ maybe 0 Map.size (Map.lookup gameId specMap)

broadcastSpectatorCount :: WsState -> Text -> IO ()
broadcastSpectatorCount state gameId = do
  count <- getSpectatorCount state gameId
  broadcast state gameId $ SpectatorCountMessage count
```

メッセージ形式:
```json
{ "type": "spectator_count", "payload": 3 }
```

### Phase 6: フロントエンド — useSpectator フック

読み取り専用のゲーム状態取得 + WebSocket接続管理。

```typescript
interface UseSpectatorReturn {
  gameState: GameState | null
  history: MoveRecord[]
  spectatorCount: number
  connectionStatus: ConnectionStatus
  error: string | null
}

function useSpectator(gameId: string | null): UseSpectatorReturn {
  const [gameState, setGameState] = useState<GameState | null>(null)
  const [spectatorCount, setSpectatorCount] = useState(0)
  const [connectionStatus, setConnectionStatus] = useState<ConnectionStatus>('disconnected')
  const reconnectAttempts = useRef(0)

  useEffect(() => {
    if (!gameId) return

    // 1. REST APIで初期状態を取得
    fetchInitialState(gameId)

    // 2. WebSocket /ws/spectate/{gameId} に接続
    let closed = false
    const connect = () => {
      if (closed) return
      const ws = new WebSocket(`${WS_URL}/spectate/${gameId}`)

      ws.onopen = () => {
        reconnectAttempts.current = 0
        setConnectionStatus('connected')
      }

      ws.onmessage = (ev) => {
        const data = JSON.parse(ev.data)
        switch (data.type) {
          case 'state_update':
            setGameState(transformGameState(data.payload))
            break
          case 'spectator_count':
            setSpectatorCount(data.payload)
            break
          case 'move_notification':
            refetchHistory(gameId)
            break
        }
      }

      ws.onclose = (ev) => {
        if (closed || ev.code === 1000) return
        scheduleReconnect()
      }
    }

    // 指数バックオフ再接続（最大5回）
    const scheduleReconnect = () => {
      if (reconnectAttempts.current >= 5) {
        setConnectionStatus('error')
        return
      }
      const delay = 1000 * Math.pow(2, reconnectAttempts.current)
      reconnectAttempts.current++
      setConnectionStatus('reconnecting')
      setTimeout(connect, delay)
    }

    connect()

    // クリーンアップ: 正常切断（code 1000）
    return () => {
      closed = true
      ws?.close(1000, 'Spectator disconnect')
    }
  }, [gameId])

  return { gameState, history, spectatorCount, connectionStatus, error }
}
```

**ポイント**:
- `useGame`（プレイヤー用）とは別のフック。手の適用やアンドゥ等のミューテーション関数を持たない
- 初期状態はREST APIで取得（WS接続が確立するまでの空白を埋める）
- 指数バックオフ: 1s → 2s → 4s → 8s → 16s（最大5回）

### Phase 7: フロントエンド — SpectatorBanner コンポーネント

観戦中であることを視覚的に示すバナー。

```tsx
interface SpectatorBannerProps {
  spectatorCount: number
  onLeave: () => void
}

function SpectatorBanner({ spectatorCount, onLeave }: SpectatorBannerProps) {
  return (
    <div className="spectator-banner" role="status">
      <span className="spectator-banner-label">Spectating</span>
      <span className="spectator-banner-count">
        {spectatorCount} watching
      </span>
      <button className="spectator-banner-leave" onClick={onLeave}>
        Leave
      </button>
    </div>
  )
}
```

CSS:
```css
.spectator-banner {
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 8px 16px;
  background: var(--spectator-bg, #2563eb);
  color: var(--spectator-text, #fff);
  font-size: 14px;
  border-radius: 6px;
}

.spectator-banner-count {
  opacity: 0.8;
}

.spectator-banner-leave {
  margin-left: auto;
  background: transparent;
  color: inherit;
  border: 1px solid currentColor;
  border-radius: 4px;
  padding: 4px 12px;
  cursor: pointer;
}
```

### Phase 8: フロントエンド — Board の読み取り専用化

Boardコンポーネントに `isSpectator` propを追加し、操作を無効化する。

```tsx
interface BoardProps {
  // ...existing props
  isSpectator?: boolean
}

function Board({ ..., isSpectator = false }: BoardProps) {
  return (
    // 各マスのイベントハンドラを条件分岐
    <g
      onClick={isSpectator ? undefined : () => onSquareClick(pos)}
      onMouseEnter={isSpectator ? undefined : () => setHoverState(piece)}
      className={`square ${isSpectator ? 'square--spectator' : ''}`}
    >
      {/* ...rendering */}
    </g>
  )
}
```

```css
.square--spectator {
  cursor: default;  /* pointer → default に変更 */
}
```

## Examples

### Example 1: 将棋観戦（Haskell + React）

```
1. プレイヤーAとBが /ws/game/{gameId} で対局中

2. 観戦者Cが接続
   WebSocket: ws://localhost:8080/ws/spectate/{gameId}
   → サーバー: addClient + addSpectator + broadcastSpectatorCount
   → 全員に { "type": "spectator_count", "payload": 1 }

3. 観戦者Cのフロントエンド:
   - useSpectator(gameId) で状態取得
   - SpectatorBanner 表示: "Spectating | 1 watching | [Leave]"
   - Board に isSpectator={true} → クリック無効

4. プレイヤーAが手を指す → broadcast でstate_update
   → プレイヤーBの画面が更新
   → 観戦者Cの画面も同時に更新（同じbroadcast）

5. 観戦者Cが待ったを要求しようとする
   → サーバー: "Spectators cannot perform this action" エラー返却

6. 観戦者Dが接続
   → 全員に { "type": "spectator_count", "payload": 2 }

7. 観戦者Cが退出
   → removeClient + removeSpectator + broadcastSpectatorCount
   → 全員に { "type": "spectator_count", "payload": 1 }
```

### Example 2: チェス観戦（TypeScript/Node.js での適用例）

```typescript
// Express + ws での実装例
const wss = new WebSocketServer({ server })

wss.on('connection', (ws, req) => {
  const match = req.url?.match(/^\/ws\/spectate\/(.+)$/)
  if (match) {
    const gameId = match[1]
    const clientId = wsState.addClient(gameId, ws)
    wsState.addSpectator(gameId, clientId, ws)
    wsState.broadcastSpectatorCount(gameId)

    ws.on('message', (raw) => {
      const cmd = JSON.parse(raw.toString())
      if (isGameAlteringCommand(cmd)) {
        ws.send(JSON.stringify({ type: 'error', payload: 'Read-only mode' }))
        return
      }
      handleCommand(cmd)
    })

    ws.on('close', () => {
      wsState.removeClient(gameId, clientId)
      wsState.removeSpectator(gameId, clientId)
      wsState.broadcastSpectatorCount(gameId)
    })
  }
})
```

```typescript
// React フック使用
function SpectatorView({ gameId }: { gameId: string }) {
  const { gameState, spectatorCount, connectionStatus } = useSpectator(gameId)
  const navigate = useNavigate()

  if (!gameState) return <div>Loading...</div>

  return (
    <>
      <SpectatorBanner
        spectatorCount={spectatorCount}
        onLeave={() => navigate('/lobby')}
      />
      <Board
        board={gameState.board}
        isSpectator={true}
        onSquareClick={() => {}}
      />
    </>
  )
}
```

## Guidelines

1. **二重登録パターン**: 観戦者は `wsClients`（broadcast受信）と `wsSpectators`（カウント専用）の両方に登録する。`wsClients` のみに登録すると既存broadcastで配信されるが、観戦者数が計算できない。`wsSpectators` のみだと別途broadcast転送ロジックが必要になる
2. **コマンドフィルタリング**: 「許可リスト」ではなく「ブロックリスト」で実装する。`ping`, `subscribe`, `unsubscribe` は許可し、ゲーム状態変更コマンドのみブロック。新しいコマンド追加時にデフォルトで安全側に倒すため
3. **初期状態のREST取得**: WebSocket接続が確立するまでの間も盤面を表示するため、接続前にREST APIで現在の状態を取得する。WS接続後はリアルタイム更新で上書き
4. **指数バックオフ再接続**: 観戦者の再接続は 1s → 2s → 4s → 8s → 16s（最大5回）。プレイヤーより積極的にリトライする必要はない
5. **カウントbroadcastのタイミング**: 接続時と切断時のみ。カウントのポーリングは不要。切断検出はWebSocketのclose/finallyで確実に行う
6. **クリーンアップの順序**: `removeClient` → `removeSpectator` → `broadcastSpectatorCount` の順序を守る。カウント計算はSpectatorMapに基づくため、先にSpectatorMapから除去するとカウントが正しくなる
7. **UI無効化の粒度**: `isSpectator` propでイベントハンドラを `undefined` にする。`pointer-events: none` よりも確実で、ツールチップ等の非操作系イベントは残せる
8. **websocket-game-syncとの関係**: 本スキルは websocket-game-sync の基盤（`broadcast` 関数、`wsClients` マップ）が既に存在することを前提とする。先に websocket-game-sync を実装してから本スキルを適用すること
