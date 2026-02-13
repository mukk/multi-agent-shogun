---
name: haskell-module-splitter
description: 肥大化したHaskellモジュールを機能単位で分割し、再エクスポートモジュールを生成する
---

# haskell-module-splitter

## Overview

肥大化した単一Haskellモジュール（500行以上）を機能別に複数の小モジュールに分割し、元のモジュールを後方互換性のある再エクスポートハブに変換するスキル。

コードの保守性・可読性を向上させつつ、既存のimport文を壊さない安全なリファクタリングを実現する。

## When to Use

- 単一モジュールが500行を超え、複数の責務を持っている
- 型定義、ハンドラ、ヘルパー関数が混在している
- モジュール内で明確な機能グループが識別できる
- 既存コードへの影響を最小限にリファクタリングしたい
- チーム開発で並行編集の競合を減らしたい

## Instructions

### Phase 1: 分析と設計

1. **モジュールサイズの確認**
   ```bash
   wc -l src/Module/Target.hs
   # 500行以上なら分割候補
   ```

2. **責務の識別**
   - 型定義 (data, newtype, type alias)
   - 状態管理 (TVar, IORef等の操作)
   - ビジネスロジック (ハンドラ、コアロジック)
   - ユーティリティ (変換関数、ヘルパー)
   - 外部API連携

3. **分割計画の作成**
   ```
   元: Shogi.Api.Handlers (865行)

   分割後:
   - Shogi.Api.Types (225行)         — JSON型定義、ApiResponse
   - Shogi.Api.AppState (70行)       — アプリケーション状態管理
   - Shogi.Api.Helpers (230行)       — 変換ヘルパー、共通関数
   - Shogi.Api.GameHandlers (274行)  — ゲームAPI実装
   - Shogi.Api.SelfPlayHandlers (70行) — 自己対戦API実装
   - Shogi.Api.Handlers (再エクスポート) — 後方互換ハブ
   ```

### Phase 2: 型定義モジュールの抽出

1. **Types.hs の作成**
   ```haskell
   {-# LANGUAGE DeriveGeneric #-}
   {-# LANGUAGE DeriveAnyClass #-}
   module Shogi.Api.Types
     ( ApiResponse(..)
     , okResponse
     , errorResponse
     , NewGameRequest(..)
     , NewGameResponse(..)
     -- ... 全型定義をエクスポート
     ) where

   import Data.Aeson (ToJSON, FromJSON)
   import GHC.Generics (Generic)

   data ApiResponse a = ApiResponse
     { success :: !Bool
     , message :: !Text
     , payload :: !a
     } deriving (Show, Eq, Generic, ToJSON, FromJSON)

   okResponse :: a -> ApiResponse a
   okResponse x = ApiResponse True "ok" x
   ```

2. **依存関係の最小化**
   - Typesモジュールは他のサブモジュールに依存しない
   - 基本的なライブラリ (aeson, text等) のみ使用

### Phase 3: 状態管理モジュールの抽出

1. **AppState.hs の作成**
   ```haskell
   module Shogi.Api.AppState
     ( AppState(..)
     , ServerSession(..)
     , createAppState
     , getSession
     , updateSession
     , deleteSession
     ) where

   import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, modifyTVar')
   import Data.Map.Strict (Map)
   import qualified Data.Map.Strict as Map

   data AppState = AppState
     { sessions :: TVar (Map Text ServerSession)
     , aiThreads :: TVar (Map Text ThreadId)
     }

   data ServerSession = ServerSession
     { sessionGame :: GameState
     , sessionHistory :: [Move]
     , sessionUndoStack :: [GameState]
     }
   ```

2. **状態操作の共通化**
   - 読み取り: `getSession :: Text -> AppState -> Handler (Maybe ServerSession)`
   - 更新: `updateSession :: Text -> ServerSession -> AppState -> Handler ()`
   - 削除: `deleteSession :: Text -> AppState -> Handler ()`

### Phase 4: ヘルパー関数の抽出

1. **Helpers.hs の作成**
   ```haskell
   module Shogi.Api.Helpers
     ( -- * 変換関数
       gameToResponse
     , moveToResponse
       -- * バリデーション
     , resolveVariant
       -- * 共通パターン
     , updateAndBroadcast
     , handleUndoRedo
     ) where
   ```

2. **共通パターンの抽出**
   ```haskell
   -- 重複コードを関数化
   updateAndBroadcast :: Text -> (ServerSession -> ServerSession)
                      -> AppState -> Handler ()
   updateAndBroadcast gameId updater state = do
     updateSession gameId updater state
     broadcastToRoom gameId (gameToResponse ...)

   -- Undo/Redo の共通化
   handleUndoRedo :: Bool -> Text -> AppState -> Handler (ApiResponse GameResponse)
   handleUndoRedo isUndo gameId state = ...
   ```

### Phase 5: ハンドラの分割

1. **機能別にハンドラモジュールを作成**
   ```haskell
   -- GameHandlers.hs
   module Shogi.Api.GameHandlers
     ( handleNewGame
     , handleGetGame
     , handleMove
     , handleUndo
     , handleRedo
     ) where

   import Shogi.Api.Types
   import Shogi.Api.AppState
   import Shogi.Api.Helpers

   handleNewGame :: AppState -> NewGameRequest -> Handler (ApiResponse NewGameResponse)
   handleNewGame state req = ...
   ```

2. **SelfPlayHandlers.hs (オプション)**
   - 自己対戦等、独立した機能がある場合は別モジュールに

### Phase 6: 再エクスポートハブの作成

1. **元のモジュール (Handlers.hs) を再エクスポートハブに変換**
   ```haskell
   {-|
   Module      : Shogi.Api.Handlers
   Description : REST API handler implementations (re-export hub)

   This module re-exports all API handler functions, types, and application state
   from their respective sub-modules for backward compatibility.
   -}
   module Shogi.Api.Handlers
     ( -- * Response Wrapper
       ApiResponse(..)
     , okResponse
     , errorResponse
       -- * Application State
     , AppState(..)
     , ServerSession(..)
       -- * Handlers
     , handleNewGame
     , handleGetGame
     , handleMove
     -- ... 全エクスポートリストを維持
     ) where

   import Shogi.Api.Types
   import Shogi.Api.AppState
   import Shogi.Api.Helpers
   import Shogi.Api.GameHandlers
   import Shogi.Api.SelfPlayHandlers
   ```

2. **エクスポートリストの維持**
   - 元のモジュールと同じエクスポートリストを保つ
   - 既存のimport文が壊れないことを保証

### Phase 7: package.yaml の更新

1. **exposed-modules に新モジュールを追加**
   ```yaml
   library:
     source-dirs: src
     exposed-modules:
       - Shogi.Api.Handlers        # 既存
       - Shogi.Api.Types           # 新規
       - Shogi.Api.AppState        # 新規
       - Shogi.Api.Helpers         # 新規
       - Shogi.Api.GameHandlers    # 新規
       - Shogi.Api.SelfPlayHandlers # 新規
   ```

### Phase 8: ビルドと検証

1. **ビルドエラーの解消**
   ```bash
   stack build
   ```

2. **よくあるエラーと対処**
   - **曖昧な名前**: 関数名が複数モジュールで重複 → qualified import
     ```haskell
     import qualified Shogi.Api.Helpers as H
     H.resolveVariant cfg
     ```

   - **循環依存**: A → B → A の依存 → モジュール構造の再設計
   - **未使用LANGUAGE pragma**: 分割後のモジュールに不要なpragmaが残る → 削除

3. **hlint でコード品質チェック**
   ```bash
   hlint src/Shogi/Api/
   # 指摘を解消（eta reduce, fromMaybe, lambda-case等）
   ```

4. **既存テストの実行**
   ```bash
   stack test
   # 全テストが通ることを確認
   ```

## Examples

### 実装例: cmd_106C (neo_shogi)

**元のモジュール構造:**
```
src/Shogi/Api/Handlers.hs (865行)
  - 型定義 (50行)
  - 状態管理 (80行)
  - ヘルパー関数 (200行)
  - ゲームハンドラ (400行)
  - 自己対戦ハンドラ (135行)
```

**分割後:**
```
src/Shogi/Api/
  ├── Types.hs (225行)           — JSON型定義、ApiResponse
  ├── AppState.hs (70行)         — AppState、ServerSession、状態操作
  ├── Helpers.hs (230行)         — 変換関数、resolveVariant、updateAndBroadcast
  ├── GameHandlers.hs (274行)    — handleNewGame, handleMove, handleUndo等
  ├── SelfPlayHandlers.hs (70行) — handleStartSelfPlay, handleStopSelfPlay等
  └── Handlers.hs (19行)         — 再エクスポートハブ（後方互換）
```

**再エクスポートハブのコード例:**
```haskell
module Shogi.Api.Handlers
  ( -- * Response Wrapper (from Types)
    ApiResponse(..)
  , okResponse
  , errorResponse
    -- * State (from AppState)
  , AppState(..)
  , ServerSession(..)
  , createAppState
    -- * Helpers (from Helpers)
  , gameToResponse
  , resolveVariant
    -- * Game Handlers (from GameHandlers)
  , handleNewGame
  , handleGetGame
  , handleMove
  , handleUndo
  , handleRedo
    -- * Self-Play (from SelfPlayHandlers)
  , handleStartSelfPlay
  , handleStopSelfPlay
  ) where

import Shogi.Api.Types
import Shogi.Api.AppState
import Shogi.Api.Helpers
import Shogi.Api.GameHandlers
import Shogi.Api.SelfPlayHandlers
```

**共通パターン抽出の例:**
```haskell
-- 元: 各ハンドラで重複していたコード
handleUndo gameId state = do
  sess <- getSession gameId state
  let newSess = undoMove sess
  updateSession gameId newSess state
  broadcastToRoom gameId (gameToResponse newSess)

handleRedo gameId state = do
  sess <- getSession gameId state
  let newSess = redoMove sess
  updateSession gameId newSess state
  broadcastToRoom gameId (gameToResponse newSess)

-- 共通化後: Helpers.hs
updateAndBroadcast :: Text -> (ServerSession -> ServerSession)
                   -> AppState -> Handler ()
updateAndBroadcast gameId updater state = do
  sess <- getSession gameId state
  let newSess = updater sess
  updateSession gameId newSess state
  broadcastToRoom gameId (gameToResponse newSess)

handleUndoRedo :: Bool -> Text -> AppState -> Handler (ApiResponse GameResponse)
handleUndoRedo isUndo gameId state = do
  updateAndBroadcast gameId (if isUndo then undoMove else redoMove) state
  ...
```

## Guidelines

### ベストプラクティス

1. **分割の粒度**
   - 1モジュール = 100〜300行が目安
   - 50行未満の極小モジュールは避ける（過剰分割）
   - 500行を超えたら再分割を検討

2. **依存関係の方向**
   ```
   Types (依存なし)
     ↑
   AppState → Types
     ↑
   Helpers → AppState, Types
     ↑
   GameHandlers → Helpers, AppState, Types
   ```
   - 循環依存を避ける
   - Typesモジュールは最下層（他に依存しない）

3. **エクスポートリストの明示**
   - `module Foo where` (全エクスポート) は避ける
   - `module Foo (...) where` で明示的に列挙
   - 内部関数の漏洩を防ぐ

4. **段階的移行**
   - 1. Types抽出 → ビルド確認
   - 2. AppState抽出 → ビルド確認
   - 3. Helpers抽出 → ビルド確認
   - 4. Handlers分割 → ビルド確認
   - 一度に全部やらず、段階的にコミット

5. **テストの並走**
   - 各段階でテストを実行
   - リファクタ前後で挙動が変わらないことを確認

### アンチパターン

1. **再エクスポートハブを忘れる**
   - ❌ 元のモジュールを削除 → 既存のimportが全て壊れる
   - ✅ 元のモジュールを再エクスポートハブとして残す

2. **過剰な細分化**
   - ❌ 20行のモジュールを10個作る
   - ✅ 適度な粒度（100〜300行）で分割

3. **循環依存の作成**
   - ❌ `A imports B, B imports A`
   - ✅ 依存関係を一方向に（Types → State → Helpers → Handlers）

4. **qualified import の乱用**
   - ❌ 全てのimportをqualified化
   - ✅ 名前衝突がある場合のみqualified

5. **エクスポートリストの不一致**
   - ❌ 再エクスポートハブが一部の関数を忘れる
   - ✅ 元のモジュールと完全に同じエクスポートリストを維持

### 保守運用

- **新機能追加時**: 適切なサブモジュールに追加（Handlersには書かない）
- **定期的なリファクタ**: サブモジュールが500行超えたら再分割を検討
- **ドキュメント更新**: 各モジュールのHaddockコメントを充実させる

### トラブルシューティング

| 問題 | 原因 | 解決策 |
|------|------|--------|
| `Ambiguous occurrence 'foo'` | 複数モジュールから同名関数をimport | qualified importを使用 |
| `Cycle in module dependencies` | A→B→Aの循環依存 | モジュール構造を再設計 |
| `Not in scope: 'Bar'` | エクスポートリストに含め忘れ | 再エクスポートハブに追加 |
| ビルドは成功するがテスト失敗 | 関数の実装ミス | git diffで変更を確認 |
