---
name: spa-to-react-router-migrator
description: 単一ページSPAからReact Router v6への段階的移行を自動化する
---

# spa-to-react-router-migrator

## Overview

単一ページのReactアプリケーション（条件分岐で画面切替）をReact Router v6ベースのマルチページアプリに移行するスキル。

既存の状態管理やコンポーネントを壊さず、段階的にルーティング機能を追加し、最終的にURLベースのナビゲーションに移行する。

## When to Use

- App.tsxが500行を超え、複数の画面を条件分岐で管理している
- `currentView` や `currentPage` といった状態変数で画面を切り替えている
- ブラウザの戻る/進むボタンが効かない
- URLを共有しても同じ画面が開かない
- 画面間の遷移が複雑化し、バグの温床になっている
- SEO対策やディープリンク対応が必要

## Instructions

### Phase 1: 現状分析

1. **単一ページパターンの特定**
   ```tsx
   // ❌ 典型的なSPAアンチパターン
   function App() {
     const [currentView, setCurrentView] = useState<'home' | 'game' | 'review'>('home')

     return (
       <div>
         {currentView === 'home' && <HomePage onStart={() => setCurrentView('game')} />}
         {currentView === 'game' && <GamePage onReview={() => setCurrentView('review')} />}
         {currentView === 'review' && <ReviewPage onBack={() => setCurrentView('home')} />}
       </div>
     )
   }
   ```

2. **画面の列挙**
   - ホーム画面 (`currentView === 'home'`)
   - ゲーム画面 (`currentView === 'game'`)
   - レビュー画面 (`currentView === 'review'`)
   - 設定画面 (オプション)

3. **依存関係の確認**
   ```bash
   npm list react-router-dom
   # なければインストール
   npm install react-router-dom
   ```

### Phase 2: ルートコンポーネントの分離

1. **各画面を独立したコンポーネントに抽出**

   ```tsx
   // ✅ Before: App.tsx内にインライン
   {currentView === 'home' && (
     <div>
       <h1>ホーム</h1>
       <button onClick={() => setCurrentView('game')}>対局開始</button>
     </div>
   )}

   // ✅ After: pages/HomePage.tsx
   import React from 'react'
   import { useNavigate } from 'react-router-dom'

   export const HomePage: React.FC = () => {
     const navigate = useNavigate()

     return (
       <div>
         <h1>ホーム</h1>
         <button onClick={() => navigate('/game')}>対局開始</button>
       </div>
     )
   }
   ```

2. **ディレクトリ構造の整理**
   ```
   src/
   ├── pages/
   │   ├── HomePage.tsx      (新規)
   │   ├── GamePage.tsx      (新規)
   │   └── ReviewPage.tsx    (新規)
   ├── components/
   │   ├── Board.tsx         (既存)
   │   └── MoveList.tsx      (既存)
   └── App.tsx               (リファクタ対象)
   ```

### Phase 3: React Router のセットアップ

1. **main.tsx (または index.tsx) にBrowserRouterを追加**

   ```tsx
   // ✅ Before
   import React from 'react'
   import ReactDOM from 'react-dom/client'
   import { App } from './App'

   ReactDOM.createRoot(document.getElementById('root')!).render(
     <React.StrictMode>
       <App />
     </React.StrictMode>
   )

   // ✅ After
   import React from 'react'
   import ReactDOM from 'react-dom/client'
   import { BrowserRouter } from 'react-router-dom'
   import { App } from './App'

   ReactDOM.createRoot(document.getElementById('root')!).render(
     <React.StrictMode>
       <BrowserRouter>
         <App />
       </BrowserRouter>
     </React.StrictMode>
   )
   ```

2. **App.tsxでRoutes/Routeを定義**

   ```tsx
   // ✅ Before
   import { useState } from 'react'

   function App() {
     const [currentView, setCurrentView] = useState<'home' | 'game'>('home')

     return (
       <div className="app">
         {currentView === 'home' && <div>...</div>}
         {currentView === 'game' && <div>...</div>}
       </div>
     )
   }

   // ✅ After
   import { Routes, Route } from 'react-router-dom'
   import { HomePage } from './pages/HomePage'
   import { GamePage } from './pages/GamePage'
   import { ReviewPage } from './pages/ReviewPage'

   function App() {
     return (
       <div className="app">
         <Routes>
           <Route path="/" element={<HomePage />} />
           <Route path="/game" element={<GamePage />} />
           <Route path="/game/:gameId" element={<GamePage />} />
           <Route path="/game/:gameId/review" element={<ReviewPage />} />
           <Route path="*" element={<NotFound />} />
         </Routes>
       </div>
     )
   }
   ```

### Phase 4: ナビゲーションの置き換え

1. **状態更新をuseNavigateに置き換え**

   ```tsx
   // ❌ Before: 状態変数で画面遷移
   const [currentView, setCurrentView] = useState('home')

   const handleStartGame = () => {
     setCurrentView('game')  // 状態更新
   }

   // ✅ After: React Routerでナビゲーション
   import { useNavigate } from 'react-router-dom'

   const navigate = useNavigate()

   const handleStartGame = (gameId: string) => {
     navigate(`/game/${gameId}`)  // URL遷移
   }
   ```

2. **リンクコンポーネントをLinkに置き換え**

   ```tsx
   // ❌ Before
   <button onClick={() => setCurrentView('home')}>ホームへ</button>

   // ✅ After
   import { Link } from 'react-router-dom'

   <Link to="/">ホームへ</Link>
   ```

3. **戻るボタンの実装**

   ```tsx
   import { useNavigate } from 'react-router-dom'

   const navigate = useNavigate()

   <button onClick={() => navigate(-1)}>戻る</button>
   ```

### Phase 5: URL パラメータの活用

1. **動的ルート定義**

   ```tsx
   // ルート定義
   <Route path="/game/:gameId" element={<GamePage />} />
   <Route path="/game/:gameId/review" element={<ReviewPage />} />
   ```

2. **パラメータの取得**

   ```tsx
   import { useParams } from 'react-router-dom'

   export const GamePage: React.FC = () => {
     const { gameId } = useParams<{ gameId: string }>()

     useEffect(() => {
       if (gameId) {
         loadGame(gameId)
       }
     }, [gameId])

     return <div>Game ID: {gameId}</div>
   }
   ```

3. **クエリパラメータの活用**

   ```tsx
   import { useSearchParams } from 'react-router-dom'

   export const GamePage: React.FC = () => {
     const [searchParams, setSearchParams] = useSearchParams()
     const variant = searchParams.get('variant') || 'standard'

     const changeVariant = (newVariant: string) => {
       setSearchParams({ variant: newVariant })
     }

     return <div>Variant: {variant}</div>
   }
   ```

### Phase 6: 状態管理の整理

1. **グローバル状態の削減**

   ```tsx
   // ❌ Before: App.tsxでグローバル状態管理
   const [gameState, setGameState] = useState<GameState | null>(null)

   <GamePage gameState={gameState} setGameState={setGameState} />

   // ✅ After: 各ページで独立して状態管理
   export const GamePage: React.FC = () => {
     const [gameState, setGameState] = useState<GameState | null>(null)
     // ページ内で完結
   }
   ```

2. **本当にグローバルな状態のみContextで管理**

   ```tsx
   // UserContext.tsx
   export const UserContext = createContext<User | null>(null)

   // App.tsx
   <UserContext.Provider value={user}>
     <Routes>...</Routes>
   </UserContext.Provider>

   // 各ページ
   const user = useContext(UserContext)
   ```

### Phase 7: レイアウトコンポーネント

1. **共通レイアウトの抽出**

   ```tsx
   // layouts/MainLayout.tsx
   import { Outlet } from 'react-router-dom'

   export const MainLayout: React.FC = () => {
     return (
       <div className="main-layout">
         <header>
           <nav>
             <Link to="/">ホーム</Link>
             <Link to="/game">対局</Link>
           </nav>
         </header>
         <main>
           <Outlet />  {/* 子ルートをレンダリング */}
         </main>
         <footer>© 2026 Neo Shogi</footer>
       </div>
     )
   }

   // App.tsx
   <Routes>
     <Route element={<MainLayout />}>
       <Route path="/" element={<HomePage />} />
       <Route path="/game" element={<GamePage />} />
     </Route>
   </Routes>
   ```

### Phase 8: 404 と リダイレクト

1. **404ページの追加**

   ```tsx
   // pages/NotFound.tsx
   import { Link } from 'react-router-dom'

   export const NotFound: React.FC = () => {
     return (
       <div>
         <h1>404 - ページが見つかりません</h1>
         <Link to="/">ホームへ戻る</Link>
       </div>
     )
   }

   // App.tsx
   <Routes>
     <Route path="/" element={<HomePage />} />
     <Route path="/game" element={<GamePage />} />
     <Route path="*" element={<NotFound />} />
   </Routes>
   ```

2. **リダイレクトの設定**

   ```tsx
   import { Navigate } from 'react-router-dom'

   // 旧URL → 新URLへのリダイレクト
   <Route path="/old-game" element={<Navigate to="/game" replace />} />

   // 認証が必要なページ
   <Route path="/admin" element={
     user ? <AdminPage /> : <Navigate to="/login" />
   } />
   ```

### Phase 9: テストと検証

1. **ビルド確認**
   ```bash
   npm run build
   # Warnings/Errors をチェック
   ```

2. **ブラウザテスト**
   - [ ] 各ルートに直接アクセスして表示確認
   - [ ] ブラウザの戻る/進むボタンが動作する
   - [ ] URLを共有して同じページが開く
   - [ ] リロード (F5) で同じページが表示される

3. **TypeScript型チェック**
   ```bash
   npm run type-check
   ```

## Examples

### 移行例: 将棋アプリ (SPA → React Router)

#### Before: 単一ページアプリ

```tsx
// App.tsx (500行)
function App() {
  const [currentView, setCurrentView] = useState<'welcome' | 'game' | 'review'>('welcome')
  const [gameId, setGameId] = useState<string | null>(null)

  const handleNewGame = (id: string) => {
    setGameId(id)
    setCurrentView('game')
  }

  const handleReview = () => {
    setCurrentView('review')
  }

  return (
    <div className="app">
      {currentView === 'welcome' && (
        <div>
          <h1>将棋アプリ</h1>
          <button onClick={() => handleNewGame('game-001')}>対局開始</button>
        </div>
      )}

      {currentView === 'game' && gameId && (
        <div>
          <Board gameId={gameId} />
          <button onClick={handleReview}>検討モード</button>
        </div>
      )}

      {currentView === 'review' && gameId && (
        <div>
          <ReviewBoard gameId={gameId} />
          <button onClick={() => setCurrentView('welcome')}>ホームへ</button>
        </div>
      )}
    </div>
  )
}
```

#### After: React Router化

```tsx
// main.tsx
import { BrowserRouter } from 'react-router-dom'

ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <BrowserRouter>
      <App />
    </BrowserRouter>
  </React.StrictMode>
)

// App.tsx (50行)
import { Routes, Route } from 'react-router-dom'
import { WelcomePage } from './pages/WelcomePage'
import { GamePage } from './pages/GamePage'
import { ReviewPage } from './pages/ReviewPage'

function App() {
  return (
    <div className="app">
      <Routes>
        <Route path="/" element={<WelcomePage />} />
        <Route path="/game/:gameId" element={<GamePage />} />
        <Route path="/game/:gameId/review" element={<ReviewPage />} />
      </Routes>
    </div>
  )
}

// pages/WelcomePage.tsx
import { useNavigate } from 'react-router-dom'

export const WelcomePage: React.FC = () => {
  const navigate = useNavigate()

  const handleNewGame = async () => {
    const gameId = await createNewGame()
    navigate(`/game/${gameId}`)
  }

  return (
    <div>
      <h1>将棋アプリ</h1>
      <button onClick={handleNewGame}>対局開始</button>
    </div>
  )
}

// pages/GamePage.tsx
import { useParams, useNavigate } from 'react-router-dom'

export const GamePage: React.FC = () => {
  const { gameId } = useParams<{ gameId: string }>()
  const navigate = useNavigate()

  if (!gameId) {
    return <Navigate to="/" />
  }

  return (
    <div>
      <Board gameId={gameId} />
      <button onClick={() => navigate(`/game/${gameId}/review`)}>
        検討モード
      </button>
    </div>
  )
}

// pages/ReviewPage.tsx
import { useParams, Link } from 'react-router-dom'

export const ReviewPage: React.FC = () => {
  const { gameId } = useParams<{ gameId: string }>()

  return (
    <div>
      <ReviewBoard gameId={gameId!} />
      <Link to="/">ホームへ</Link>
    </div>
  )
}
```

### 段階的移行の例

```tsx
// Phase 1: React Router導入（既存の状態管理は維持）
<BrowserRouter>
  <App />  {/* 内部は従来のcurrentView使用 */}
</BrowserRouter>

// Phase 2: ホーム画面のみRouter化
<Routes>
  <Route path="/" element={<HomePage />} />
  <Route path="*" element={<OldApp />} />  {/* 他は従来のまま */}
</Routes>

// Phase 3: ゲーム画面もRouter化
<Routes>
  <Route path="/" element={<HomePage />} />
  <Route path="/game/:gameId" element={<GamePage />} />
  <Route path="*" element={<OldApp />} />
</Routes>

// Phase 4: 全画面Router化
<Routes>
  <Route path="/" element={<HomePage />} />
  <Route path="/game/:gameId" element={<GamePage />} />
  <Route path="/game/:gameId/review" element={<ReviewPage />} />
</Routes>
```

## Guidelines

### ベストプラクティス

1. **段階的移行**
   - 一度に全画面を移行せず、1画面ずつRouter化
   - 各段階でビルド＆動作確認

2. **URLの設計**
   ```
   ✅ Good:
   /                       - ホーム
   /game/:gameId           - 対局
   /game/:gameId/review    - 検討
   /settings               - 設定

   ❌ Bad:
   /page1
   /page2
   /view?type=game&id=123  (クエリ多用)
   ```

3. **コンポーネント粒度**
   - ページコンポーネント (pages/): ルーティング対象
   - UIコンポーネント (components/): 再利用可能な部品

4. **状態の配置**
   - URL state: ページID、フィルタ条件等
   - Local state: フォーム入力、UIトグル等
   - Global state (Context): ユーザー情報、テーマ等

5. **Lazy Loading** (大規模アプリ)
   ```tsx
   import { lazy, Suspense } from 'react'

   const GamePage = lazy(() => import('./pages/GamePage'))

   <Suspense fallback={<div>Loading...</div>}>
     <Routes>
       <Route path="/game/:id" element={<GamePage />} />
     </Routes>
   </Suspense>
   ```

### アンチパターン

1. **不必要なグローバル状態**
   - ❌ App.tsxで全ページの状態を管理
   - ✅ 各ページで独立した状態管理

2. **URL設計の欠如**
   - ❌ `/page?data=...` (全情報をクエリに)
   - ✅ `/game/:gameId/review` (RESTful設計)

3. **Navigate の過剰使用**
   - ❌ `<div onClick={() => navigate('/home')}>ホーム</div>`
   - ✅ `<Link to="/">ホーム</Link>` (アクセシビリティ向上)

4. **useEffectでのURL監視**
   - ❌ `useEffect(() => { if (url === '/game') ... }, [url])`
   - ✅ Routeコンポーネントに任せる

5. **ハードコードされたパス**
   - ❌ `navigate('/game/123/review')`
   - ✅ `navigate(\`/game/${gameId}/review\`)` (変数使用)

### 移行チェックリスト

- [ ] `react-router-dom` をインストール
- [ ] `main.tsx` に `<BrowserRouter>` を追加
- [ ] 各画面を `pages/` ディレクトリに分離
- [ ] `App.tsx` で `<Routes>` を定義
- [ ] `setCurrentView` を `navigate()` に置き換え
- [ ] `<button onClick>` を `<Link to>` に置き換え
- [ ] URL パラメータで状態を管理 (`useParams`)
- [ ] 404ページを追加
- [ ] ブラウザの戻る/進むボタンをテスト
- [ ] URLを直接入力して動作確認
- [ ] ビルドエラーがないことを確認

### トラブルシューティング

| 問題 | 原因 | 解決策 |
|------|------|--------|
| `useNavigate() may be used only in context of <Router>` | BrowserRouterの外でuseNavigate使用 | main.tsxでBrowserRouterでラップ |
| リロードで404 | Vite/webpack設定不足 | `vite.config.ts` に `historyApiFallback` 追加 |
| URL変わるが画面変わらない | Routeの定義ミス | `path` と `element` を確認 |
| `useParams()` が undefined | パスパラメータ名の不一致 | `/game/:gameId` と `useParams<{ gameId }>` を一致させる |
| 遷移が遅い | コンポーネントが重い | React.lazy でコード分割 |
