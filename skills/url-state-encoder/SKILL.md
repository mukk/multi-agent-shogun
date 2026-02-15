---
name: url-state-encoder
description: URLパラメータにアプリケーション状態をBase64エンコードで埋め込み、サーバー不要の共有リンク生成を実現する。対局リプレイURL、フォーム状態復元、設定プリセット共有等に使用。URL長制限・圧縮・フォールバック・ルーティング統合を含む。
---

# URL State Encoder

## Overview

アプリケーションの状態（ゲームの棋譜、フォーム入力値、設定、検索フィルタ等）をURLパラメータにエンコードして埋め込み、URLを共有するだけで状態を復元できるようにするパターン。以下を一貫して実施する：

- **エンコーダー/デコーダー関数**: JSON → Base64 / Base64 → JSON の双方向変換
- **URL長制限対策**: ブラウザの制限（2048文字）を考慮し、実用的には1500文字以内に収める
- **圧縮ライブラリ統合**: 大きなデータ（棋譜、ログ）には pako/lz-string で圧縮
- **ルーティング統合**: React Router, Vue Router, Svelte Kit との統合パターン
- **フォールバック処理**: 不正なURLパラメータや古いバージョンのデータに対するエラーハンドリング

## When to Use

- ゲームのリプレイURLを生成する（将棋の棋譜、チェスのPGN等）
- フォームの途中状態を共有可能にする（アンケート、設定画面、検索フィルタ）
- 設定プリセットをURLで配布する（テーマ、レイアウト、カスタマイズ）
- サーバー不要でアプリケーション状態をシェアしたい
- デモやプレビュー機能を実装したい（コードエディタ、デザインツール等）

## Instructions

### Step 1: 基本エンコーダー/デコーダーの実装

#### TypeScript 実装

```typescript
/**
 * アプリケーション状態を Base64URL エンコードする。
 * 標準 Base64 の +, /, = を URL-safe な文字に置換する。
 */
export function encodeState<T>(state: T): string {
  try {
    const json = JSON.stringify(state)
    const base64 = btoa(unescape(encodeURIComponent(json)))
    // URL-safe Base64: + → -, / → _, = 削除
    return base64.replace(/\+/g, '-').replace(/\//g, '_').replace(/=/g, '')
  } catch (error) {
    console.error('Failed to encode state:', error)
    return ''
  }
}

/**
 * Base64URL エンコードされた文字列をデコードして状態を復元する。
 */
export function decodeState<T>(encoded: string): T | null {
  try {
    // URL-safe Base64 → 標準 Base64
    let base64 = encoded.replace(/-/g, '+').replace(/_/g, '/')
    // パディング復元（4の倍数にする）
    while (base64.length % 4) {
      base64 += '='
    }
    const json = decodeURIComponent(escape(atob(base64)))
    return JSON.parse(json) as T
  } catch (error) {
    console.error('Failed to decode state:', error)
    return null
  }
}
```

#### JavaScript (ブラウザ互換)

```javascript
export function encodeState(state) {
  try {
    const json = JSON.stringify(state)
    const base64 = btoa(unescape(encodeURIComponent(json)))
    return base64.replace(/\+/g, '-').replace(/\//g, '_').replace(/=/g, '')
  } catch {
    return ''
  }
}

export function decodeState(encoded) {
  try {
    let base64 = encoded.replace(/-/g, '+').replace(/_/g, '/')
    while (base64.length % 4) base64 += '='
    const json = decodeURIComponent(escape(atob(base64)))
    return JSON.parse(json)
  } catch {
    return null
  }
}
```

**重要ポイント:**

- `unescape(encodeURIComponent(json))`: UTF-8文字列を正しくBase64エンコードする
- `decodeURIComponent(escape(atob(base64)))`: Base64デコード後にUTF-8に戻す
- URL-safe Base64: `+` → `-`, `/` → `_`, `=` 削除（URLパラメータで安全に使える）
- パディング復元: Base64は4の倍数長が必要。削除した `=` を復元する

### Step 2: URL長制限の管理

#### 制限値の確認

| 環境 | URL最大長 |
|------|-----------|
| Chrome/Edge/Firefox | 2048文字（de facto標準） |
| Safari | 80,000文字（実質無制限） |
| IE 11 | 2083文字 |
| 実用的安全値 | **1500文字** |

#### 長さチェック関数

```typescript
/**
 * エンコードされた状態がURL長制限内に収まるか確認する。
 */
export function isUrlSafe(encodedState: string, maxLength = 1500): boolean {
  // クエリパラメータ名 + "?" + "&" 等のオーバーヘッドを考慮
  const overhead = 50
  return encodedState.length + overhead <= maxLength
}

/**
 * 状態をエンコードし、長さをチェックして警告を出す。
 */
export function encodeWithCheck<T>(state: T): string | null {
  const encoded = encodeState(state)
  if (!encoded) return null

  if (!isUrlSafe(encoded)) {
    console.warn(`Encoded state is too long (${encoded.length} chars). Consider compression.`)
    return null
  }

  return encoded
}
```

### Step 3: 圧縮の統合（大きなデータ向け）

#### pako (Deflate/Gzip) の使用

`pako` は軽量で高圧縮率。バイナリデータに強い。

```bash
npm install pako
```

```typescript
import pako from 'pako'

/**
 * JSON → Gzip圧縮 → Base64URL エンコード
 */
export function encodeCompressed<T>(state: T): string {
  try {
    const json = JSON.stringify(state)
    // UTF-8 文字列を Uint8Array に変換
    const encoder = new TextEncoder()
    const data = encoder.encode(json)
    // Deflate 圧縮
    const compressed = pako.deflate(data)
    // Base64URL エンコード
    const base64 = btoa(String.fromCharCode(...compressed))
    return base64.replace(/\+/g, '-').replace(/\//g, '_').replace(/=/g, '')
  } catch (error) {
    console.error('Failed to encode compressed state:', error)
    return ''
  }
}

/**
 * Base64URL → Gzip解凍 → JSON
 */
export function decodeCompressed<T>(encoded: string): T | null {
  try {
    let base64 = encoded.replace(/-/g, '+').replace(/_/g, '/')
    while (base64.length % 4) base64 += '='
    // Base64 → Uint8Array
    const binary = atob(base64)
    const bytes = new Uint8Array(binary.length)
    for (let i = 0; i < binary.length; i++) {
      bytes[i] = binary.charCodeAt(i)
    }
    // Deflate 解凍
    const decompressed = pako.inflate(bytes)
    // Uint8Array → UTF-8 文字列
    const decoder = new TextDecoder()
    const json = decoder.decode(decompressed)
    return JSON.parse(json) as T
  } catch (error) {
    console.error('Failed to decode compressed state:', error)
    return null
  }
}
```

#### lz-string (LZ77圧縮) の使用

`lz-string` はJavaScript専用の軽量圧縮ライブラリ。依存なしでBase64エンコード機能内蔵。

```bash
npm install lz-string
```

```typescript
import LZString from 'lz-string'

/**
 * JSON → LZ77圧縮 → Base64URL エンコード
 */
export function encodeCompressedLZ<T>(state: T): string {
  try {
    const json = JSON.stringify(state)
    // compressToBase64 は標準 Base64 を返す
    const compressed = LZString.compressToBase64(json)
    // URL-safe に変換
    return compressed.replace(/\+/g, '-').replace(/\//g, '_').replace(/=/g, '')
  } catch {
    return ''
  }
}

/**
 * Base64URL → LZ77解凍 → JSON
 */
export function decodeCompressedLZ<T>(encoded: string): T | null {
  try {
    // URL-safe → 標準 Base64
    let base64 = encoded.replace(/-/g, '+').replace(/_/g, '/')
    while (base64.length % 4) base64 += '='
    const json = LZString.decompressFromBase64(base64)
    if (!json) return null
    return JSON.parse(json) as T
  } catch {
    return null
  }
}
```

#### 圧縮の適用判断基準

| データサイズ（JSON文字列） | 推奨 |
|---------------------------|------|
| 〜500文字 | **圧縮なし**（オーバーヘッドの方が大きい） |
| 500〜2000文字 | **lz-string**（軽量で十分な圧縮率） |
| 2000文字〜 | **pako**（高圧縮率、依存サイズは大きい） |

#### 自動判定ヘルパー

```typescript
export function encodeAuto<T>(state: T): string {
  const json = JSON.stringify(state)

  // 小さいデータは圧縮なし
  if (json.length < 500) {
    return encodeState(state)
  }

  // 中規模データは lz-string
  if (json.length < 2000) {
    const compressed = encodeCompressedLZ(state)
    return compressed || encodeState(state) // fallback
  }

  // 大規模データは pako
  const compressed = encodeCompressed(state)
  return compressed || encodeState(state) // fallback
}
```

### Step 4: ルーティング統合

#### React Router v6

```typescript
import { useSearchParams } from 'react-router-dom'

export function useUrlState<T>(
  key: string,
  defaultValue: T
): [T, (value: T) => void] {
  const [searchParams, setSearchParams] = useSearchParams()

  const state = (() => {
    const encoded = searchParams.get(key)
    if (!encoded) return defaultValue
    const decoded = decodeState<T>(encoded)
    return decoded ?? defaultValue
  })()

  const setState = (value: T) => {
    const encoded = encodeState(value)
    if (!encoded) return
    setSearchParams(prev => {
      const next = new URLSearchParams(prev)
      next.set(key, encoded)
      return next
    })
  }

  return [state, setState]
}
```

**使用例:**

```tsx
function GameReplayPage() {
  const [kifu, setKifu] = useUrlState('k', { moves: [] })

  return (
    <div>
      <KifuPlayer kifu={kifu} />
      <button onClick={() => setKifu({ moves: [...kifu.moves, newMove] })}>
        Add Move
      </button>
    </div>
  )
}
```

#### Vue Router

```typescript
import { useRoute, useRouter } from 'vue-router'
import { ref, watch } from 'vue'

export function useUrlState<T>(key: string, defaultValue: T) {
  const route = useRoute()
  const router = useRouter()

  const state = ref<T>((() => {
    const encoded = route.query[key] as string | undefined
    if (!encoded) return defaultValue
    return decodeState<T>(encoded) ?? defaultValue
  })())

  watch(state, (newValue) => {
    const encoded = encodeState(newValue)
    if (!encoded) return
    router.push({
      query: { ...route.query, [key]: encoded }
    })
  })

  return state
}
```

#### Svelte Kit

```typescript
import { page } from '$app/stores'
import { goto } from '$app/navigation'
import { writable, derived } from 'svelte/store'

export function createUrlState<T>(key: string, defaultValue: T) {
  const state = writable<T>(defaultValue)

  // ページロード時にURLから復元
  page.subscribe(($page) => {
    const encoded = $page.url.searchParams.get(key)
    if (encoded) {
      const decoded = decodeState<T>(encoded)
      if (decoded) state.set(decoded)
    }
  })

  // 状態変更時にURLを更新
  state.subscribe((value) => {
    const encoded = encodeState(value)
    if (!encoded) return
    const url = new URL(window.location.href)
    url.searchParams.set(key, encoded)
    goto(url, { replaceState: true, keepFocus: true })
  })

  return state
}
```

### Step 5: フォールバック処理

#### バージョン管理付きエンコード

将来の変更に備えて、状態にバージョン番号を埋め込む。

```typescript
interface VersionedState<T> {
  v: number  // バージョン番号
  d: T       // 実データ
}

export function encodeVersioned<T>(state: T, version = 1): string {
  const wrapped: VersionedState<T> = { v: version, d: state }
  return encodeState(wrapped)
}

export function decodeVersioned<T>(
  encoded: string,
  migrations: Record<number, (data: any) => any> = {}
): T | null {
  const wrapped = decodeState<VersionedState<any>>(encoded)
  if (!wrapped || typeof wrapped.v !== 'number') return null

  let data = wrapped.d
  const currentVersion = Math.max(...Object.keys(migrations).map(Number), 1)

  // マイグレーション実行（古いバージョン → 新しいバージョン）
  for (let v = wrapped.v; v < currentVersion; v++) {
    if (migrations[v + 1]) {
      data = migrations[v + 1](data)
    }
  }

  return data as T
}
```

**使用例:**

```typescript
// v1 → v2 のマイグレーション（フィールド名変更）
const migrations = {
  2: (data: any) => ({
    ...data,
    playerName: data.name, // name → playerName
  }),
}

const state = decodeVersioned<GameState>(encoded, migrations)
```

#### エラーハンドリングパターン

```typescript
export function safeDecodeState<T>(
  encoded: string | null | undefined,
  defaultValue: T
): T {
  if (!encoded) return defaultValue

  try {
    const decoded = decodeState<T>(encoded)
    // null チェック + 型検証（必須フィールドの存在確認等）
    if (!decoded || !isValidState(decoded)) {
      console.warn('Invalid state format, using default')
      return defaultValue
    }
    return decoded
  } catch (error) {
    console.error('Failed to decode state:', error)
    return defaultValue
  }
}

// 型検証ヘルパー（runtime type check）
function isValidState<T>(value: any): value is T {
  // 必須フィールドの存在確認
  return (
    typeof value === 'object' &&
    value !== null &&
    'moves' in value &&  // 例: 棋譜には moves が必須
    Array.isArray(value.moves)
  )
}
```

## Examples

### Example 1: 将棋の棋譜共有URL

```typescript
interface Kifu {
  moves: string[]
  sfen: string
  players: { black: string; white: string }
}

// エンコード
const kifu: Kifu = {
  moves: ['7g7f', '3c3d', '2g2f'],
  sfen: 'lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1',
  players: { black: 'Player A', white: 'Player B' }
}

const url = `https://example.com/replay?k=${encodeAuto(kifu)}`
// → https://example.com/replay?k=eyJtb3ZlcyI6WyI3Zzd...

// デコード（ページロード時）
const params = new URLSearchParams(window.location.search)
const encoded = params.get('k')
const restoredKifu = safeDecodeState(encoded, { moves: [], sfen: '', players: { black: '', white: '' } })
```

### Example 2: フォーム状態の復元

```typescript
interface FormState {
  name: string
  email: string
  preferences: { theme: string; notifications: boolean }
}

function ShareableForm() {
  const [form, setForm] = useUrlState<FormState>('form', {
    name: '',
    email: '',
    preferences: { theme: 'light', notifications: true }
  })

  return (
    <form>
      <input
        value={form.name}
        onChange={e => setForm({ ...form, name: e.target.value })}
      />
      <input
        value={form.email}
        onChange={e => setForm({ ...form, email: e.target.value })}
      />
      <button onClick={() => {
        const shareUrl = window.location.href
        navigator.clipboard.writeText(shareUrl)
        alert('フォーム状態を共有URLとしてコピーしました')
      }}>
        共有URLをコピー
      </button>
    </form>
  )
}
```

### Example 3: 検索フィルタの永続化

```typescript
interface SearchFilter {
  query: string
  category: string
  priceRange: [number, number]
  sortBy: 'price' | 'name' | 'date'
}

function SearchPage() {
  const [filter, setFilter] = useUrlState<SearchFilter>('f', {
    query: '',
    category: 'all',
    priceRange: [0, 1000],
    sortBy: 'name'
  })

  // フィルタ変更時に自動的にURLが更新され、ブックマーク可能
  return (
    <div>
      <input
        value={filter.query}
        onChange={e => setFilter({ ...filter, query: e.target.value })}
      />
      <select
        value={filter.sortBy}
        onChange={e => setFilter({ ...filter, sortBy: e.target.value as any })}
      >
        <option value="price">価格順</option>
        <option value="name">名前順</option>
        <option value="date">日付順</option>
      </select>
      <SearchResults filter={filter} />
    </div>
  )
}
```

## Guidelines

1. **URL-safe Base64 を必ず使用**: 標準 Base64 の `+`, `/`, `=` はURLパラメータで問題を起こす。必ず `-`, `_` に置換し、`=` を削除すること
2. **UTF-8 エンコードに注意**: `btoa()` は Latin-1 しか扱えない。`unescape(encodeURIComponent())` で UTF-8 を正しく変換する
3. **URL長制限は1500文字**: ブラウザの仕様上2048文字が上限だが、実用的には1500文字以内に収める。超える場合は圧縮を検討
4. **圧縮は500文字以上で**: 小さなデータを圧縮するとむしろ大きくなる。圧縮ライブラリのオーバーヘッドを考慮すること
5. **エラーハンドリングは必須**: URLパラメータは改変されやすい。必ず `try/catch` で囲み、デフォルト値にフォールバックする
6. **バージョン管理を導入**: 将来の状態フォーマット変更に備えて、最初からバージョン番号を埋め込んでおく
7. **型検証を実施**: デコード後の値が期待する型に一致するか確認する。特に必須フィールドの存在確認
8. **SEO への影響**: URLパラメータに状態を含めると、検索エンジンが大量の重複URLとみなす可能性がある。`<link rel="canonical">` を設定するか、`noindex` を付けることを検討
9. **プライバシー配慮**: URLは共有されやすい。個人情報や機密情報は絶対にエンコードしないこと
10. **パフォーマンス**: `encodeState` / `decodeState` は軽量。ただし頻繁な呼び出し（入力のたびに更新等）は debounce を使って制限する
