---
name: lobby-game-bridge
description: マルチプレイヤーゲームでロビー部屋管理とゲームセッション作成を橋渡しする。部屋の作成→待機→プレイヤー集合→ゲーム自動開始→リアルタイム通知のライフサイクルを実装する際に使用。Haskell (STM + Servant) とTypeScript (React) の両面をカバー。
---

# Lobby-Game Bridge

## Overview

マルチプレイヤーゲームにおける「ロビー部屋管理」と「ゲームセッション作成」を疎結合に接続するアーキテクチャパターン。以下を一貫して実施する：

- **部屋ライフサイクル管理**: Waiting → Playing → Finished の状態遷移
- **自動ゲーム作成**: 必要人数が揃った時点でゲームセッションを自動生成
- **循環依存回避**: GameCreatorコールバックでロビーモジュールとゲームモジュールを疎結合に接続
- **リアルタイム通知**: WebSocketで部屋一覧更新・ゲーム開始をリアルタイム配信
- **ホスト/ゲスト非対称フロー**: ホストは待機→WS通知で遷移、ゲストはAPI応答で即遷移

## When to Use

- マルチプレイヤーゲームにロビー（対戦待機所）機能を追加する
- 部屋の作成→参加→ゲーム開始の一連のフローを実装する
- ロビーモジュールとゲームモジュールの循環依存を解消したい
- WebSocketでロビー状態のリアルタイム更新が必要
- ホスト（部屋作成者）とゲスト（参加者）で異なる遷移フローが必要

## Instructions

### アーキテクチャ概要

```
                        ┌──────────────┐
                        │   Frontend   │
                        └──────┬───────┘
                               │
                    ┌──────────┼──────────┐
                    │ REST API │ WebSocket│
                    │          │ (/lobby) │
                    └──────────┼──────────┘
                               │
┌──────────────┐  callback  ┌──┴───────────┐
│ Game Module  │◄───────────│ Lobby Module │
│ (sessions)   │            │ (rooms)      │
└──────────────┘            └──────────────┘
```

**核心**: Lobby Module は Game Module を直接importしない。GameCreatorコールバック（`variant -> IO (Either error gameId)` 型）を経由してゲームを作成する。これにより循環依存を回避し、モジュールの独立性を保つ。

### Phase 1: 部屋データモデル

#### Haskell

```haskell
data RoomStatus = Waiting | Playing | Finished
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Room = Room
  { roomId         :: !Text
  , roomHost       :: !Text          -- 作成者の表示名
  , roomVariant    :: !Text          -- ゲームバリアント
  , roomStatus     :: !RoomStatus
  , roomPlayers    :: ![Text]        -- 参加者（最大N人）
  , roomGameId     :: !(Maybe Text)  -- Playing時のゲームID
  }

newtype LobbyState = LobbyState
  { lobbyRooms :: TVar (Map Text Room)
  }
```

#### TypeScript

```typescript
interface Room {
  roomId: string
  host: string
  variant: string
  status: 'waiting' | 'playing' | 'finished'
  players: string[]
  gameId?: string
}
```

### Phase 2: GameCreatorコールバック（循環依存回避の核心）

#### 問題
Lobby ModuleがGame Moduleを直接importすると循環依存が発生する場合がある：
- Game Module は Lobby Module の `LobbyState` を参照（アプリ状態に含む）
- Lobby Module が Game Module の `createGame` を呼ぶと循環

#### 解決策: コールバック注入

```haskell
-- Lobby Module側: 型エイリアスのみ定義
type GameCreator = Text -> IO (Either Text Text)
  -- ^ variant -> Either errorMessage gameId

-- Lobby Serverはコールバックを引数に取る
lobbyServer :: LobbyState -> WsState -> GameCreator -> Server LobbyAPI
```

```haskell
-- 配線側 (Routes.hs): 実際の関数を渡す
shogiServer state =
       ...
  :<|> lobbyServer (appLobby state) (appWsState state) (createGameForLobby state)

-- Game Module側: 実際のゲーム作成関数
createGameForLobby :: AppState -> Text -> IO (Either Text Text)
createGameForLobby state variant = do
  -- バリアント設定を解決
  -- ゲームセッションを作成
  -- TVarに登録
  -- gameIdを返却
```

#### TypeScript (依存性注入)

```typescript
type GameCreator = (variant: string) => Promise<{ gameId: string }>

function createLobbyServer(
  lobbyState: LobbyState,
  wsServer: WsServer,
  createGame: GameCreator,
): Router { ... }
```

### Phase 3: 部屋参加時のゲーム自動作成

JoinRoom ハンドラで、必要人数が揃ったら自動的にゲームを作成する。

```haskell
handleJoinRoom lobby ws createGame roomId req = do
  let playerName = jrPlayerName req
  result <- liftIO $ atomically $ joinRoom lobby roomId playerName
  case result of
    Left err -> return $ errResult err
    Right room
      -- 必要人数が揃った → ゲーム作成
      | playerName `elem` roomPlayers room
        && length (roomPlayers room) == requiredPlayers -> do
          gameResult <- liftIO $ createGame (roomVariant room)
          case gameResult of
            Left err -> return $ errResult err
            Right gameId -> do
              -- 部屋をPlaying状態に遷移
              liftIO $ atomically $ setRoomPlaying lobby roomId gameId
              -- WebSocketでゲーム開始を通知（ホスト向け）
              liftIO $ broadcastLobbyUpdate ws $ toJSON $ object
                [ "event" .= ("game_started" :: Text)
                , "roomId" .= roomId
                , "gameId" .= gameId
                ]
              return $ okResult $ JoinRoomResult (Just gameId) "playing"

      -- まだ人数不足 → 待機継続
      | playerName `elem` roomPlayers room ->
          return $ okResult $ JoinRoomResult Nothing "waiting"
```

### Phase 4: WebSocketロビーチャンネル

ゲーム個別のWS（`/ws/game/{gameId}`）とは別に、ロビー専用のWSチャンネル（`/ws/lobby`）を設ける。

#### バックエンド: ロビーbroadcast

```haskell
-- WsState にロビー購読者マップを追加
data WsState = WsState
  { wsClients      :: TVar (Map Text (Map ClientId WS.Connection))
  , wsLobbyClients :: TVar (Map ClientId WS.Connection)  -- ロビー購読者
  , wsNextId       :: TVar Int
  }

-- ロビー購読者全員にbroadcast
broadcastLobbyUpdate :: WsState -> Value -> IO ()
broadcastLobbyUpdate state payload = do
  clients <- readTVarIO (wsLobbyClients state)
  let msg = object ["type" .= ("lobby_update" :: Text), "payload" .= payload]
  forM_ (Map.elems clients) $ \conn ->
    WS.sendTextData conn (encode msg)
      `catch` \(_ :: SomeException) -> return ()
```

#### 通知タイミング

| イベント | payload.event | 含まれるデータ |
|----------|---------------|---------------|
| 部屋作成 | `rooms_updated` | 全部屋リスト |
| プレイヤー参加 | `rooms_updated` | 全部屋リスト |
| ゲーム開始 | `game_started` | roomId, gameId |
| 部屋削除 | `rooms_updated` | 全部屋リスト |

### Phase 5: フロントエンド — ホスト/ゲスト非対称フロー

#### ホスト（部屋作成者）のフロー

```
1. 部屋作成 API → roomId 取得
2. 待機画面を表示（「対戦相手を待っています...」）
3. WebSocket /ws/lobby に接続
4. game_started イベント受信 → gameId 取得
5. ゲーム画面に遷移（playerColor = 先手）
```

#### ゲスト（参加者）のフロー

```
1. 部屋一覧から選択 → joinRoom API
2. API応答に gameId が含まれる（即座にゲーム開始）
3. ゲーム画面に遷移（playerColor = 後手）
```

#### React実装: ロビーコンポーネント

```tsx
function Lobby({ onJoinGame }: LobbyProps) {
  const [rooms, setRooms] = useState<RoomInfo[]>([])
  const [waitingRoomId, setWaitingRoomId] = useState<string | null>(null)
  const waitingRoomRef = useRef<string | null>(null)
  const onJoinGameRef = useRef(onJoinGame)

  // WebSocket購読: ロビー更新 + ゲーム開始通知
  useEffect(() => {
    let ws: WebSocket | null = null
    let closed = false

    const connect = () => {
      if (closed) return
      ws = new WebSocket('ws://localhost:8080/ws/lobby')
      ws.onmessage = (ev) => {
        const data = JSON.parse(ev.data)
        if (data.type === 'lobby_update' && data.payload) {
          const { event, rooms, roomId, gameId } = data.payload
          if (event === 'rooms_updated' && Array.isArray(rooms)) {
            setRooms(rooms)
          }
          if (event === 'game_started') {
            if (waitingRoomRef.current === roomId) {
              onJoinGameRef.current(gameId, 'sente')  // ホスト=先手
              setWaitingRoomId(null)
            }
          }
        }
      }
      ws.onclose = () => { if (!closed) setTimeout(connect, 3000) }
    }
    connect()
    return () => { closed = true; ws?.close() }
  }, [])

  // 部屋作成 → 待機状態
  const handleCreate = async () => {
    const res = await createRoom({ name, variant, timePreset })
    setWaitingRoomId(res.roomId)  // 待機画面表示
  }

  // 部屋参加 → 即ゲーム開始
  const handleJoin = async (roomId: string) => {
    const res = await joinRoom(roomId)
    if (res.gameId) {
      onJoinGame(res.gameId, 'gote')  // ゲスト=後手
    }
  }
}
```

### Phase 6: ゲーム参加フック

既存のゲームフック（`useGame`等）に `joinGame` 関数を追加し、ロビーから既存ゲームに参加できるようにする。

```typescript
// useGame.ts に追加
const joinGame = useCallback(async (id: string, color?: Color) => {
  setPlayerColor(color ?? null)
  const state = await api.getGameState(id)
  setGameState(state)
  const moves = await api.getLegalMoves(id)
  setLegalMoves(enrichLegalMoves(moves, state))
  gameWs.connect(id)  // WebSocket接続（リアルタイム同期開始）
}, [])

// 手番制御: マルチプレイヤー時は自分のターンのみ操作可能
const effectiveLegalMoves = useMemo(() => {
  if (playerColor && gameState && gameState.turn !== playerColor) {
    return []  // 相手のターン → 操作不可
  }
  return legalMoves
}, [playerColor, gameState, legalMoves])
```

## Examples

### Example 1: 将棋対戦ロビー

```
1. ホストが「ミニ将棋で対戦」部屋を作成
   POST /api/rooms { name: "ホスト名", variant: "mini", timePreset: "10min" }
   → { roomId: "room-abc123" }

2. ホストの画面: 「対戦相手を待っています...」+ WebSocket /ws/lobby 接続

3. ゲストが部屋一覧を閲覧（WebSocket経由でリアルタイム更新）
   → 「ホスト名」の部屋が表示される

4. ゲストが参加
   POST /api/rooms/room-abc123/join { playerName: "ゲスト名" }
   → バックエンドがゲーム自動作成
   → { gameId: "def456", status: "playing" }

5. WebSocketで全ロビー購読者に通知
   → { type: "lobby_update", payload: { event: "game_started", roomId: "room-abc123", gameId: "def456" } }

6. ホストがWS通知を受信 → ゲーム画面へ遷移（先手）
7. ゲストがAPI応答を受信 → ゲーム画面へ遷移（後手）
8. 両者がWebSocket /ws/game/def456 に接続 → リアルタイム対局開始
```

### Example 2: チェス対戦ロビー（TypeScript/Node.js での適用例）

```typescript
// Express + ws での実装例
const lobbyRouter = createLobbyRouter(lobbyState, wsServer, async (variant) => {
  const game = createChessGame(variant)
  await gameStore.save(game)
  return { gameId: game.id }
})

app.use('/api/rooms', lobbyRouter)

// WebSocket: /ws/lobby パス
wss.on('connection', (ws, req) => {
  if (req.url === '/ws/lobby') {
    lobbyState.addLobbySubscriber(ws)
    ws.on('close', () => lobbyState.removeLobbySubscriber(ws))
  }
})
```

## Guidelines

1. **循環依存回避**: Lobby Module から Game Module を直接importしない。必ずコールバック（GameCreator型）で疎結合にする。配線はエントリーポイント（Routes/Main）で行う
2. **STMトランザクション分離**: 部屋状態の更新（STM）とゲーム作成（IO）は別トランザクションで行う。ゲーム作成はIO操作（ランダムID生成等）を含むためSTM内に入れられない
3. **ホスト/ゲスト非対称フロー**: ホストはWebSocket通知、ゲストはAPI応答でゲームIDを取得する。この非対称性は部屋の「最後の参加者」がゲーム作成をトリガーする設計に起因する
4. **ロビーbroadcastの粒度**: 個別イベント（`game_started`）と全体更新（`rooms_updated`）を分ける。`game_started`はホストの遷移トリガー、`rooms_updated`は部屋一覧の更新
5. **プレイヤー色割り当て**: ホスト=先手、ゲスト=後手が最もシンプル。ランダム割り当てが必要な場合はバックエンドで決定し、応答に含める
6. **手番制御**: マルチプレイヤー時はフロントエンドで合法手をフィルタリングし、相手ターン時の操作を防ぐ。バックエンド側の検証は追加のセキュリティレイヤーとして実装
7. **WebSocket再接続**: ロビーWSは単純な再接続（3秒間隔）で十分。ゲームWSは指数バックオフ（1s→2s→4s...）を使用
8. **部屋のクリーンアップ**: ホスト切断時は部屋を削除、ゲスト切断時はプレイヤーリストから除去。Playing状態の部屋はゲーム終了まで保持
