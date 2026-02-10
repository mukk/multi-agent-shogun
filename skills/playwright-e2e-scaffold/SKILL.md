---
name: playwright-e2e-scaffold
description: React + Vite (TypeScript) プロジェクト向けに Playwright E2E テストの雛形を生成する。playwright.config.ts、APIモックヘルパー、汎用テストテンプレートを出力。バックエンド不要でフロントエンド単独テストが可能。
---

# Playwright E2E Scaffold

## Overview

Playwright を使った E2E テストの初期構築を一貫して行うスキル。以下を生成する:

- **playwright.config.ts**: Vite dev server との連携、headless 実行、スクリーンショット設定
- **e2e/helpers/mock-api.ts**: `page.route()` による API モックパターン（バックエンド不要）
- **テストテンプレート**: ナビゲーション、設定パネル、CRUD 操作の汎用パターン

React + Vite + TypeScript を想定するが、Vue/Svelte でも `page.route()` モック部分はそのまま使える。

## When to Use

- 新規プロジェクトに E2E テスト環境を構築する
- バックエンド未完成の段階でフロントエンドの E2E テストを書きたい
- CI/CD で Playwright テストを自動実行したい
- 既存プロジェクトに Playwright を後付け導入する

## Instructions

### Step 0: 依存関係のインストール

```bash
npm install -D @playwright/test
npx playwright install chromium
```

`package.json` にテストスクリプトを追加:

```json
{
  "scripts": {
    "test:e2e": "playwright test"
  }
}
```

### Step 1: playwright.config.ts の作成

プロジェクトルートに配置する。`webServer` で dev server を自動起動。

```typescript
import { defineConfig } from '@playwright/test'

export default defineConfig({
  testDir: './e2e',
  timeout: 30_000,
  retries: 0,
  use: {
    baseURL: 'http://localhost:5173',  // Vite default port
    headless: true,
    screenshot: 'only-on-failure',
  },
  projects: [
    { name: 'chromium', use: { browserName: 'chromium' } },
    // 必要に応じて追加:
    // { name: 'firefox', use: { browserName: 'firefox' } },
    // { name: 'webkit', use: { browserName: 'webkit' } },
  ],
  webServer: {
    command: 'npm run dev',
    url: 'http://localhost:5173',
    reuseExistingServer: true,
    timeout: 30_000,
  },
})
```

**カスタマイズポイント:**

| 設定 | 説明 | 調整場面 |
|------|------|----------|
| `baseURL` | dev server の URL | ポート変更時（3000, 8080 等） |
| `timeout` | テスト全体のタイムアウト | 重いページは 60_000 に |
| `retries` | 失敗時のリトライ回数 | CI では `1` 推奨 |
| `webServer.command` | dev server 起動コマンド | yarn, pnpm の場合変更 |
| `screenshot` | スクリーンショット取得条件 | デバッグ時は `'on'` |

### Step 2: API モックヘルパーの作成

`e2e/helpers/mock-api.ts` に API モック関数を集約する。`page.route()` でリクエストをインターセプトし、モックデータを返す。

```typescript
import type { Page } from '@playwright/test'

// ============================================================
// Mock Data
// ============================================================

/**
 * アプリの初期状態を表すモックデータ。
 * バックエンドの API レスポンス型に合わせて定義する。
 */
const MOCK_ITEMS = [
  { id: '1', name: 'Item 1', status: 'active' },
  { id: '2', name: 'Item 2', status: 'inactive' },
]

// ============================================================
// API Response Helper
// ============================================================

/**
 * 統一レスポンス形式でラップする。
 * バックエンドが { success, message, payload } 形式の場合に使用。
 */
function apiOk(payload: unknown) {
  return JSON.stringify({
    success: true,
    message: 'OK',
    payload,
  })
}

function apiError(message: string) {
  return JSON.stringify({
    success: false,
    message,
    payload: null,
  })
}

// ============================================================
// Mock Setup Functions
// ============================================================

/**
 * 基本の CRUD API モック。
 * GET /api/items → 一覧, POST /api/items → 作成, etc.
 */
export async function setupCrudMock(page: Page) {
  // GET /api/items → item list
  await page.route('**/api/items', async route => {
    if (route.request().method() === 'GET') {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: apiOk(MOCK_ITEMS),
      })
    } else if (route.request().method() === 'POST') {
      // POST /api/items → create
      const body = route.request().postDataJSON()
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: apiOk({ id: '3', ...body }),
      })
    } else {
      await route.continue()
    }
  })

  // GET /api/items/:id → single item
  await page.route('**/api/items/*', async route => {
    if (route.request().method() === 'GET') {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: apiOk(MOCK_ITEMS[0]),
      })
    } else {
      await route.continue()
    }
  })
}

/**
 * ナビゲーション用モック。
 * 各ページが必要とする最低限の API をモックする。
 */
export async function setupNavMock(page: Page) {
  // 各ページの初期データ取得 API をモック
  await page.route('**/api/**', async route => {
    await route.fulfill({
      status: 200,
      contentType: 'application/json',
      body: apiOk([]),
    })
  })
}

/**
 * 認証モック（ログイン/ログアウト）。
 */
export async function setupAuthMock(page: Page) {
  await page.route('**/api/auth/login', async route => {
    await route.fulfill({
      status: 200,
      contentType: 'application/json',
      body: apiOk({ token: 'mock-jwt-token', user: { id: '1', name: 'Test User' } }),
    })
  })

  await page.route('**/api/auth/me', async route => {
    await route.fulfill({
      status: 200,
      contentType: 'application/json',
      body: apiOk({ id: '1', name: 'Test User' }),
    })
  })
}
```

**重要パターン:**

1. **`page.route('**/api/...')`**: ダブルアスタリスクで任意のオリジン + パスにマッチ
2. **`route.request().method()`**: GET/POST/PUT/DELETE を分岐
3. **`route.request().postDataJSON()`**: リクエストボディを取得
4. **`route.continue()`**: モック対象外のリクエストはそのまま通す
5. **遅延シミュレーション**: `await new Promise(r => setTimeout(r, 300))` でローディング表示をテスト可能

### Step 3: ナビゲーションテスト

ページ遷移と「戻る」操作をテストする。SPA のルーティングが正しく動作することを確認。

```typescript
// e2e/navigation.spec.ts
import { test, expect } from '@playwright/test'
import { setupNavMock } from './helpers/mock-api'

test.describe('Navigation', () => {
  test.beforeEach(async ({ page }) => {
    await setupNavMock(page)
    await page.goto('/')
  })

  test('home page renders correctly', async ({ page }) => {
    // ホームページの主要要素が表示される
    await expect(page.locator('h1, .app-title')).toBeVisible()
  })

  test('navigate to a sub-page and back', async ({ page }) => {
    // ナビゲーションリンク/ボタンをクリック
    await page.getByRole('link', { name: /about|settings|dashboard/i }).first().click()

    // 遷移先のページ固有要素が表示される
    await expect(page.locator('main, [class*="page"]')).toBeVisible()

    // 戻るボタンまたはブラウザバック
    const backBtn = page.getByRole('button', { name: /back|戻る/i })
    if (await backBtn.isVisible()) {
      await backBtn.click()
    } else {
      await page.goBack()
    }

    // ホームに戻る
    await expect(page.locator('h1, .app-title')).toBeVisible()
  })

  test('navigation buttons are all visible', async ({ page }) => {
    // 主要ナビゲーション要素が全て存在する
    const navLinks = page.getByRole('link').or(page.getByRole('button'))
    const count = await navLinks.count()
    expect(count).toBeGreaterThan(0)
  })
})
```

### Step 4: 設定パネルテスト

ドロップダウンやモーダルの開閉、設定値の変更をテスト。

```typescript
// e2e/settings.spec.ts
import { test, expect } from '@playwright/test'

test.describe('Settings Panel', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/')
  })

  test('open and close settings', async ({ page }) => {
    // 設定トリガー（歯車アイコン等）
    const trigger = page.locator(
      '[class*="settings"] button, [aria-label*="settings"], [aria-label*="設定"]'
    )
    await expect(trigger).toBeVisible()

    // パネルは初期状態で非表示
    const panel = page.locator('[class*="settings"][class*="dropdown"], [role="dialog"]')
    await expect(panel).not.toBeVisible()

    // クリックで開く
    await trigger.click()
    await expect(panel).toBeVisible()

    // 再クリックまたは Escape で閉じる
    await page.keyboard.press('Escape')
    await expect(panel).not.toBeVisible()
  })

  test('theme switch changes active state', async ({ page }) => {
    // 設定パネルを開く
    const trigger = page.locator(
      '[class*="settings"] button, [aria-label*="settings"]'
    )
    await trigger.click()

    // テーマボタンがある場合
    const themeButtons = page.locator('[class*="theme"] button')
    const count = await themeButtons.count()
    if (count > 1) {
      await themeButtons.nth(1).click()
      await expect(themeButtons.nth(1)).toHaveClass(/active|selected/)
    }
  })

  test('toggle switch changes state', async ({ page }) => {
    const trigger = page.locator(
      '[class*="settings"] button, [aria-label*="settings"]'
    )
    await trigger.click()

    // トグルボタン（ON/OFF スイッチ）
    const toggle = page.locator('[aria-pressed], [role="switch"]').first()
    if (await toggle.isVisible()) {
      const initialState = await toggle.getAttribute('aria-pressed')
      await toggle.click()
      const newState = await toggle.getAttribute('aria-pressed')
      expect(newState).not.toEqual(initialState)
    }
  })
})
```

### Step 5: CRUD 操作テスト

一覧表示、新規作成、詳細表示をテスト。

```typescript
// e2e/crud.spec.ts
import { test, expect } from '@playwright/test'
import { setupCrudMock } from './helpers/mock-api'

test.describe('CRUD Operations', () => {
  test.beforeEach(async ({ page }) => {
    await setupCrudMock(page)
    await page.goto('/')
  })

  test('list page shows items', async ({ page }) => {
    // 一覧ページに遷移（ナビゲーションがある場合）
    const listLink = page.getByRole('link', { name: /list|一覧|items/i })
    if (await listLink.isVisible()) {
      await listLink.click()
    }

    // アイテムが表示される（リスト要素やカード）
    const items = page.locator(
      '[class*="item"], [class*="card"], [class*="row"], li'
    )
    // モックデータが2件なので2件以上表示されるはず
    const count = await items.count()
    expect(count).toBeGreaterThanOrEqual(1)
  })

  test('create form submits successfully', async ({ page }) => {
    // 新規作成ボタンをクリック
    const createBtn = page.getByRole('button', { name: /new|create|追加|作成/i })
    if (await createBtn.isVisible()) {
      await createBtn.click()

      // フォームが表示される
      const form = page.locator('form, [class*="form"]')
      await expect(form).toBeVisible()

      // フィールドに入力
      const nameInput = page.getByLabel(/name|名前/i).or(
        page.locator('input[name="name"]')
      )
      if (await nameInput.isVisible()) {
        await nameInput.fill('Test Item')
      }

      // 送信ボタン
      const submitBtn = page.getByRole('button', { name: /submit|save|送信|保存/i })
      if (await submitBtn.isVisible()) {
        await submitBtn.click()
      }
    }
  })
})
```

### Step 6: ローディング状態テスト

API レスポンスに遅延を入れてローディング表示を検証。

```typescript
// e2e/loading.spec.ts
import { test, expect } from '@playwright/test'

test.describe('Loading States', () => {
  test('shows loading indicator during API call', async ({ page }) => {
    // API に遅延を挿入
    await page.route('**/api/**', async route => {
      await new Promise(r => setTimeout(r, 1000))
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({ success: true, message: 'OK', payload: [] }),
      })
    })

    await page.goto('/')

    // ローディングインジケータが表示される
    const loader = page.locator(
      '[role="status"], [class*="loading"], [class*="spinner"]'
    )
    // 表示されるか確認（タイミング依存のため soft assert）
    const isVisible = await loader.isVisible().catch(() => false)
    if (isVisible) {
      // ローディングが消える（API レスポンス後）
      await expect(loader).not.toBeVisible({ timeout: 5000 })
    }
  })
})
```

### Step 7: CI/CD 統合

#### GitHub Actions

```yaml
# .github/workflows/e2e.yml
name: E2E Tests
on: [push, pull_request]
jobs:
  e2e:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: 22
      - run: npm ci
      - run: npx playwright install --with-deps chromium
      - run: npm run test:e2e
      - uses: actions/upload-artifact@v4
        if: failure()
        with:
          name: playwright-report
          path: test-results/
```

#### .gitignore に追加

```
test-results/
playwright-report/
```

## Examples

### Example 1: React + Vite プロジェクト (最小構成)

```
my-app/
  src/
  e2e/
    helpers/
      mock-api.ts      ← API モック
    navigation.spec.ts ← ナビゲーションテスト
    settings.spec.ts   ← 設定パネルテスト
  playwright.config.ts ← Playwright 設定
  package.json         ← "test:e2e" 追加
```

### Example 2: REST API バックエンド付きプロジェクト

モックを使わず実 API をテストする場合、`setupCrudMock` を呼ばずに `page.goto()` だけで開始:

```typescript
test.beforeEach(async ({ page }) => {
  // バックエンドが起動している前提
  await page.goto('/')
})
```

`playwright.config.ts` の `webServer` に複数サーバーを設定:

```typescript
webServer: [
  {
    command: 'npm run dev',
    url: 'http://localhost:5173',
    reuseExistingServer: true,
  },
  {
    command: 'npm run api:dev',
    url: 'http://localhost:8080',
    reuseExistingServer: true,
  },
],
```

### Example 3: 認証付きアプリのテスト

```typescript
import { setupAuthMock } from './helpers/mock-api'

test.describe('Authenticated Pages', () => {
  test.beforeEach(async ({ page }) => {
    await setupAuthMock(page)
    // ログイン状態をシミュレート
    await page.goto('/login')
    await page.getByLabel('Email').fill('test@example.com')
    await page.getByLabel('Password').fill('password')
    await page.getByRole('button', { name: /login|ログイン/i }).click()
  })

  test('dashboard is accessible after login', async ({ page }) => {
    await expect(page.locator('[class*="dashboard"]')).toBeVisible()
  })
})
```

## Guidelines

1. **API モックはヘルパーに集約**: テストファイルに直接 `page.route()` を書かない。`e2e/helpers/mock-api.ts` に関数化して再利用する
2. **`page.route()` の順序**: 具体的なパスを先に、ワイルドカードを後に設定する。後から設定したルートが優先される（上書き可能）
3. **セレクタの優先順位**: `getByRole` > `getByLabel` > `getByText` > `locator` の順で使用する。ロール/ラベルベースのセレクタはリファクタリングに強い
4. **テストの独立性**: 各 `test()` は他のテストに依存しない。`beforeEach` で初期状態をリセットする
5. **タイムアウト設定**: デフォルト 30s で不足する場合はテスト単位で `test.setTimeout(60_000)` を設定。グローバルに変更しない
6. **スクリーンショット**: 失敗時のみ取得がデフォルト。デバッグ時は `screenshot: 'on'` に一時変更
7. **CI 環境**: `npx playwright install --with-deps chromium` でシステムライブラリも含めてインストール。WSL2 ローカルでは `--with-deps` に sudo が必要
8. **モックの遅延**: ローディング UI をテストするなら `setTimeout` で 500-1000ms の遅延を入れる。通常テストでは遅延不要（高速化のため）
9. **`reuseExistingServer: true`**: 開発中は dev server を手動起動しておくとテスト起動が速い
10. **テストファイルの命名**: `*.spec.ts` で統一。機能単位で分割（navigation, settings, crud, auth 等）
