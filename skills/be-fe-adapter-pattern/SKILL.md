---
name: be-fe-adapter-pattern
description: Backend/Frontendインターフェース不整合を体系的に検出・修正するパターン
---

# be-fe-adapter-pattern

## Overview

Backend (Haskell/Servant) と Frontend (TypeScript/React) のAPI境界で発生するインターフェース不整合を、段階的に検出・修正するスキル。

型システムが異なる言語間でのデータ交換における5つの典型的な不整合パターンを特定し、型安全な変換レイヤーを構築する。

## When to Use

- BE実装後にFEで「データが取れない」「フィールドがundefined」エラーが出る
- JSONのフィールド名がBE/FEで異なる（snake_case vs camelCase等）
- BEがApiResponseでラップしているのにFEが生データを期待している
- Enumの値がBE/FEで一致しない（文字列表現の違い）
- BEが単一オブジェクトを返すのにFEが配列を期待している
- FEがモックデータに依存し、実際のAPIと結合していない

## Instructions

### Phase 1: 不整合の診断

1. **症状の特定**
   - ブラウザのNetwork Tabで実際のAPIレスポンスを確認
   - Console Errorで `undefined` エラーをチェック
   - FEのTypeScriptコンパイルエラーを確認

2. **5つの不整合パターンのチェックリスト**

   | パターン | BE | FE | 症状 |
   |----------|----|----|------|
   | 1. フィールド名 | `{ je_name: "..." }` | `entry.name` | `undefined` |
   | 2. レスポンス形式 | `ApiResponse { payload: {...} }` | `data.field` | `undefined` |
   | 3. Enum値 | `"Kakoi"` | `"kakoi"` | 不一致 |
   | 4. データ構造 | 単一オブジェクト | 配列期待 | Type error |
   | 5. モック依存 | 実API | モックデータ | 未結合 |

3. **curl でのAPI動作確認**
   ```bash
   # 実際のレスポンスを確認
   curl -X POST http://localhost:3001/api/joseki/search \
     -H "Content-Type: application/json" \
     -d '{"moves": ["7g7f", "3c3d"]}'
   ```

### Phase 2: Backend修正

#### パターン1: JSONフィールド名の統一

**問題:** Haskellのデータ型フィールドがそのままJSON化され、FEの期待と異なる

```haskell
-- ❌ 修正前
data JosekiEntry = JosekiEntry
  { je_name :: Text
  , je_moves :: [Text]
  } deriving (Generic, ToJSON, FromJSON)

-- JSON出力: { "je_name": "...", "je_moves": [...] }
-- FE期待:    { "name": "...", "moves": [...] }
```

**解決:** `fieldLabelModifier` でプレフィックスを除去

```haskell
-- ✅ 修正後
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson (genericToJSON, genericParseJSON, defaultOptions, Options(..), fieldLabelModifier)
import Data.Char (toLower)

data JosekiEntry = JosekiEntry
  { je_name :: Text
  , je_moves :: [Text]
  } deriving (Show, Eq, Generic)

-- カスタムJSON instance
instance ToJSON JosekiEntry where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = dropPrefix 3  -- "je_" (3文字) を除去
    }

instance FromJSON JosekiEntry where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = dropPrefix 3
    }

dropPrefix :: Int -> String -> String
dropPrefix n s =
  let rest = drop n s
  in case rest of
       (c:cs) -> toLower c : cs  -- 先頭を小文字化
       []     -> []

-- JSON出力: { "name": "...", "moves": [...] } ✅
```

#### パターン2: APIレスポンス形式の統一

**問題:** 一部のエンドポイントがApiResponseでラップされていない

```haskell
-- ❌ 修正前
handleJosekiSearch :: SearchRequest -> Handler JosekiMatch
handleJosekiSearch req = do
  result <- searchDB req
  return result  -- 生のJosekiMatchを返す

-- JSON: { "name": "...", "progress": "..." }
```

**解決:** 全エンドポイントをApiResponseでラップ

```haskell
-- ✅ 修正後
handleJosekiSearch :: SearchRequest -> Handler (ApiResponse JosekiMatch)
handleJosekiSearch req = do
  result <- searchDB req
  return $ okResponse result  -- ApiResponseでラップ

-- JSON: { "success": true, "message": "ok", "payload": { "name": "...", ... } }
```

#### パターン3: Enumの双方向変換

**問題:** HaskellのEnum値とFEの文字列表現が一致しない

```haskell
-- ❌ 修正前
data JosekiCategory = Ibisha | Furibisha | Kakoi
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- JSON: "Ibisha" (先頭大文字)
-- FE:   "ibisha" (全小文字) → 不一致
```

**解決:** カスタムToJSON/FromJSON instanceで変換

```haskell
-- ✅ 修正後
data JosekiCategory = Ibisha | Furibisha | Kakoi | Kyusen
  deriving (Show, Eq, Generic)

instance ToJSON JosekiCategory where
  toJSON = toJSON . categoryToText

instance FromJSON JosekiCategory where
  parseJSON = withText "JosekiCategory" $ \t ->
    return $ categoryFromText t

categoryToText :: JosekiCategory -> Text
categoryToText Ibisha    = "ibisha"
categoryToText Furibisha = "furibisha"
categoryToText Kakoi     = "kakoi"
categoryToText Kyusen    = "kyusen"

categoryFromText :: Text -> JosekiCategory
categoryFromText "ibisha"    = Ibisha
categoryFromText "furibisha" = Furibisha
categoryFromText "kakoi"     = Kakoi
categoryFromText "kyusen"    = Kyusen
categoryFromText _           = Other
```

#### パターン4: データ読込の完全性

**問題:** BEが部分的なデータしか返さない

```haskell
-- ❌ 修正前
loadJosekiBook :: FilePath -> IO JosekiBook
loadJosekiBook path = decodeFileEither path >>= ...

-- 単一ファイルのみ読込
```

**解決:** 全データを読み込む関数を追加

```haskell
-- ✅ 修正後
import System.Directory (listDirectory, doesDirectoryExist)
import Data.List (isSuffixOf)

loadAllJosekiBooks :: FilePath -> IO [JosekiBook]
loadAllJosekiBooks dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return []
    else do
      files <- listDirectory dir
      let yamlFiles = filter (".yaml" `isSuffixOf`) files
      mapM (loadJosekiBook . (dir </>)) yamlFiles
```

### Phase 3: Frontend修正

#### パターン5: モック依存の除去

**問題:** FEが実APIではなくモックデータを使用

```typescript
// ❌ 修正前
export async function searchJoseki(moves: string[]): Promise<JosekiMatch> {
  // return requestWithTimeout<JosekiMatch>(...);  // コメントアウト
  return {  // モックデータ
    name: "Sample",
    progress: "1/10",
    // ...
  }
}
```

**解決:** 変換レイヤーを追加して実APIに接続

```typescript
// ✅ 修正後
import { ApiResponse } from './types'

// BEレスポンス型
interface JosekiMatchResponse {
  name: string
  remaining: number
  description: string
  progress: string
}

// FE表示用型
export interface JosekiMatch {
  name: string
  remaining: number
  description: string
  progress: string
}

// 変換レイヤー
function transformJosekiMatch(raw: JosekiMatchResponse): JosekiMatch {
  return {
    name: raw.name,
    remaining: raw.remaining,
    description: raw.description,
    progress: raw.progress,
  }
}

// API呼び出し
export async function searchJoseki(moves: string[]): Promise<JosekiMatch> {
  try {
    const response = await requestWithTimeout<ApiResponse<JosekiMatchResponse>>(
      '/api/joseki/search',
      {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ moves }),
      }
    )

    if (!response.success) {
      throw new Error(response.message)
    }

    return transformJosekiMatch(response.payload)
  } catch (error) {
    console.error('Failed to search joseki:', error)
    throw error
  }
}
```

### Phase 4: 統合テスト

1. **手動確認**
   ```bash
   # BEサーバー起動
   stack run

   # FE開発サーバー起動
   npm run dev

   # ブラウザで動作確認
   # Network Tabでレスポンス確認
   ```

2. **型チェック**
   ```bash
   # BE
   stack build

   # FE
   npm run type-check
   ```

3. **E2Eテスト** (オプション)
   ```typescript
   test('Joseki search returns correct format', async () => {
     const result = await searchJoseki(['7g7f', '3c3d'])
     expect(result).toHaveProperty('name')
     expect(result).toHaveProperty('progress')
     expect(typeof result.remaining).toBe('number')
   })
   ```

## Examples

### 実装例: cmd_109 (neo_shogi 定跡DB)

**5つの不整合を全修正:**

#### 1. フィールド名不一致

```haskell
-- Before
data JosekiEntry = JosekiEntry
  { je_name :: Text, je_category :: JosekiCategory, ... }
  deriving (Generic, ToJSON, FromJSON)

-- After
instance ToJSON JosekiEntry where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = \s ->
        let rest = drop 3 s  -- "je_" を除去
        in case rest of
             (c:cs) -> toLower c : cs
             []     -> []
    }
```

#### 2. ApiResponse統一

```haskell
-- Before
josekiServer :: AppState -> Server JosekiAPI
josekiServer state = handleSearch state

handleSearch :: SearchRequest -> Handler JosekiMatch

-- After
josekiServer :: AppState -> Server JosekiAPI
josekiServer state = handleSearch state

handleSearch :: SearchRequest -> Handler (ApiResponse JosekiMatch)
handleSearch req = do
  result <- ...
  return $ okResponse result
```

#### 3. Enumカテゴリ追加

```haskell
-- Before
data JosekiCategory = Ibisha | Furibisha | Aigakari | Other
  deriving (Generic, ToJSON, FromJSON)  -- 自動JSON化（"Ibisha"）

-- After
data JosekiCategory = Ibisha | Furibisha | Aigakari | Kakoi | Kyusen | Other
  deriving (Show, Eq, Generic)

instance ToJSON JosekiCategory where
  toJSON = toJSON . categoryToText

categoryToText :: JosekiCategory -> Text
categoryToText Kakoi  = "kakoi"   -- 小文字化 + 日本語対応
categoryToText Kyusen = "kyusen"
-- ...
```

#### 4. 全YAML読込

```haskell
-- Before: 単一ファイルのみ
loadJosekiBook "config/joseki/居飛車.yaml"

-- After: ディレクトリ内の全YAMLを読込
loadAllJosekiBooks :: FilePath -> IO [JosekiBook]
loadAllJosekiBooks dir = do
  files <- listDirectory dir
  let yamlFiles = filter (".yaml" `isSuffixOf`) files
  books <- mapM (\f -> loadJosekiBook (dir </> f)) yamlFiles
  return $ rights books  -- 成功したもののみ
```

#### 5. FEモック除去

```typescript
// Before
export async function getJosekiCategories(): Promise<JosekiCategory[]> {
  // return requestWithTimeout(...);
  return ['ibisha', 'furibisha', 'aigakari']  // モック
}

// After
export async function getJosekiCategories(): Promise<JosekiCategory[]> {
  const response = await requestWithTimeout<ApiResponse<JosekiCategory[]>>(
    '/api/joseki/categories'
  )
  if (!response.success) {
    console.error('Failed to load categories:', response.message)
    return []
  }
  return response.payload
}
```

## Guidelines

### ベストプラクティス

1. **段階的修正**
   - 全5パターンを一度に修正せず、1つずつ確認
   - 各修正後にビルド＆動作確認

2. **型安全性の活用**
   ```typescript
   // BE変更時にFEでコンパイルエラーが出るようにする
   interface JosekiMatchResponse {
     name: string
     remaining: number  // BEで型変更 → TSエラー
   }
   ```

3. **エラーハンドリングの明示**
   ```typescript
   try {
     const response = await api.search(...)
     if (!response.success) {
       throw new Error(response.message)  // BEエラーを伝播
     }
     return response.payload
   } catch (error) {
     console.error('API error:', error)  // デバッグ情報
     throw error  // 上位で処理
   }
   ```

4. **変換レイヤーの分離**
   ```typescript
   // api/josekiApi.ts
   function transformJosekiMatch(raw: RawResponse): JosekiMatch { ... }

   // components/JosekiTrainer.tsx
   const match = await searchJoseki(moves)  // 変換済みデータを受け取る
   ```

5. **ドキュメント化**
   ```haskell
   {-|
   = JSON Output Format

   @
   {
     "name": "四間飛車",
     "category": "furibisha",  -- 小文字
     "moves": ["7g7f", "3c3d", ...]
   }
   @
   -}
   ```

### アンチパターン

1. **FE側でのフィールド名変換**
   - ❌ FEで `entry.je_name` → `entry.name` に変換
   - ✅ BEで `fieldLabelModifier` を使用

2. **型定義の重複**
   - ❌ BE/FEで別々に型定義 → 不整合が発生
   - ✅ OpenAPI/型生成ツールの活用を検討

3. **モックと実装の併存**
   - ❌ `if (isDev) return mockData else callAPI()`
   - ✅ 環境変数でAPIエンドポイントを切り替え

4. **エラーの握りつぶし**
   - ❌ `catch (e) { return [] }`
   - ✅ `catch (e) { console.error(e); throw e }`

5. **全APIを一度に修正**
   - ❌ 10個のエンドポイントを一度に変更 → デバッグ困難
   - ✅ 1エンドポイントずつ修正してコミット

### チェックリスト

修正前に確認:
- [ ] Network TabでBEの実際のレスポンスを確認した
- [ ] FEの期待する型と実際のレスポンスを比較した
- [ ] 5つの不整合パターンのどれに該当するか特定した

修正後に確認:
- [ ] `stack build` が成功する
- [ ] `npm run type-check` が成功する
- [ ] ブラウザのConsoleにエラーが出ない
- [ ] Network TabのレスポンスがFEの期待通り
- [ ] 実際にUIで操作して動作確認した

### トラブルシューティング

| 症状 | 原因 | 解決策 |
|------|------|--------|
| `field is undefined` | JSONフィールド名不一致 | fieldLabelModifier使用 |
| `payload is undefined` | ApiResponse未ラップ | okResponse適用 |
| Enum値が一致しない | 文字列表現の違い | カスタムToJSON instance |
| 404 Not Found | エンドポイントパス誤り | Routes.hsとFE URLを確認 |
| CORS error | オリジン不一致 | BE側でCORS設定 |
| Type error (TS) | BE/FE型定義の乖離 | 型定義を同期 |
