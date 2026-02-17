---
name: production-rendering-audit
description: 本番HTML出力のレンダリングパイプライン分析パターン。PHPコードではなく実際に配信されるHTMLを取得して検証する。Lazy Loading二重化、CSS競合、srcset実効解決、キャッシュ層書き換え、UA別差分を検出し、PageSpeed最適化の実効性を診断する。
---

# Production Rendering Audit

## Overview

本番環境で実際に配信されるHTML出力を取得し、レンダリングパイプライン全体（テーマPHP → プラグイン書き換え → キャッシュ層 → CDN → ブラウザ）を分析するパターン。以下を一貫して実施する：

- **本番HTML実取得**: PHPコード読みではなく、実際にcurl/ブラウザで取得したHTMLを分析
- **Lazy Loading二重化検出**: Native Lazy Loading（`loading="lazy"`）とプラグイン（`data-src`等）の競合チェック
- **CSS override分析**: `!important` 競合、プラグイン注入CSS、メディアクエリ内の `display:none`/`visibility:hidden` 検出
- **srcset実効解決シミュレーション**: モバイルビューポート（375px）での `sizes` 評価 + Retina DPR考慮の候補URL特定 + 404チェック
- **キャッシュレイヤー分析**: W3TC等のHTML書き換え痕跡、Varyヘッダーによるデバイス別キャッシュ検出
- **UA別レスポンス差分**: デスクトップ/モバイルでのHTML差分検出

## When to Use

- PageSpeed Insights で警告が出ているが、原因がコードレベルで特定できない
- プラグインを導入したが効果が出ない（キャッシュやCDNが原因の可能性）
- デスクトップでは正常だがモバイルで画像が表示されない
- `srcset`/`sizes` を実装したが、期待したサイズの画像が配信されていない
- Lazy Loadingが動作しない、または二重ロードが発生している
- テーマ更新後にレンダリングが壊れた（CSSの競合、プラグインの書き換え失敗）

## Instructions

### Step 1: 本番HTML実取得手順

#### 方法A: curl でのUA偽装取得

**デスクトップUA:**

```bash
curl -A "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36" \
  https://example.com/ > desktop.html
```

**モバイルUA (iPhone):**

```bash
curl -A "Mozilla/5.0 (iPhone; CPU iPhone OS 16_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.0 Mobile/15E148 Safari/604.1" \
  https://example.com/ > mobile.html
```

**モバイルUA (Android):**

```bash
curl -A "Mozilla/5.0 (Linux; Android 13; Pixel 7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Mobile Safari/537.36" \
  https://example.com/ > android.html
```

#### 方法B: Chrome DevTools でのHTML取得

1. ページを開く
2. DevTools → Elements タブ
3. `<html>` 要素を右クリック → "Copy" → "Copy outerHTML"
4. テキストエディタに貼り付けて保存

**重要**: DevToolsはJavaScript実行後のDOMを返す。サーバー送信時の生HTMLとは異なる場合がある。

#### 方法C: DevTools Network Panel での生HTML取得

1. DevTools → Network タブ
2. "Disable cache" をチェック
3. ページをリロード
4. 最初のHTML（Docタイプ）を選択 → Response タブ
5. コピーして保存

**この方法が最も正確**（サーバーから送信された生HTMLそのもの）。

#### 差分検出

```bash
# デスクトップとモバイルの差分を検出
diff desktop.html mobile.html > diff.txt

# 差分があれば、サーバーがUA別にHTML出力を変えている
wc -l diff.txt
# 0行なら差分なし、数百行なら大幅な差分あり
```

### Step 2: Lazy Loading二重化検出

#### 問題パターン

| パターン | 症状 | 原因 |
|---------|------|------|
| Native + Plugin 併用 | 画像が2回遅延ロードされる | `loading="lazy"` と `data-src` が同一要素に存在 |
| Plugin のみ | スクロールで読み込まれない | JSが動作しない環境（NoScript等） |
| Native のみ | ファーストビュー画像も遅延 | `loading="eager"` の未設定 |

#### 検出コマンド

```bash
# Native Lazy Loading の使用箇所
grep -o 'loading="lazy"' output.html | wc -l

# プラグイン Lazy Loading の使用箇所（EWWW, Smush, WP Rocket等）
grep -oP 'data-(src|lazy-src|original)="[^"]*"' output.html | wc -l

# 同一要素で両方が使用されているか確認（二重化）
grep 'loading="lazy"' output.html | grep -E 'data-(src|lazy-src)' | wc -l
# → 0より大きければ二重化が発生
```

#### 具体例: EWWW Image Optimizer の二重化

**問題のあるHTML:**

```html
<img src="placeholder.svg"
     data-src="image.jpg"
     data-lazy-src="image.webp"
     loading="lazy"
     class="lazyload">
```

- `loading="lazy"`: ブラウザがネイティブLazy Loadingを実行
- `data-src`: EWWWのJSがスクロール時に `src` に置き換え
- 結果: 両方が動作し、2回ロードされる or JSが先に動作してネイティブが無視される

**修正方法:**

1. EWWWの設定で "Lazy Load" を無効化
2. または、`functions.php` で Native Lazy Loading を優先:

```php
add_filter('wp_lazy_loading_enabled', '__return_true');
add_filter('ewww_image_optimizer_lazy_load', '__return_false');
```

### Step 3: CSS override分析

#### 問題パターン

| CSS | 症状 | 検出方法 |
|-----|------|---------|
| `display: none !important` | 画像が完全に非表示 | DevTools Computed → `display: none` の適用元を確認 |
| `visibility: hidden` | スペースは確保されるが非表示 | 同上 |
| `opacity: 0` | 透明化（Lazy Load の遷移アニメ用途） | 遷移後に `opacity: 1` になるか確認 |
| `w-fit`/`h-fit` 誤用 | `<picture>` タグのレイアウト崩れ | Tailwind CSS と `<picture>` の相性問題 |

#### 検出手順

**1. HTML内の `<style>` / `<link>` タグを抽出:**

```bash
# インラインCSSの抽出
grep -oP '<style[^>]*>.*?</style>' output.html > inline.css

# 外部CSSのURL抽出
grep -oP '<link[^>]*rel="stylesheet"[^>]*href="\K[^"]*' output.html > stylesheets.txt
```

**2. `!important` 競合の検出:**

```bash
# CSS内の !important 使用箇所
curl https://example.com/style.css | grep -n '!important'

# display/visibility の !important 適用
curl https://example.com/style.css | grep -E '(display|visibility).*!important'
```

**3. メディアクエリ内の非表示設定:**

```bash
# モバイル用メディアクエリ内の display:none 検出
curl https://example.com/style.css | grep -A10 '@media.*max-width' | grep 'display: none'
```

**問題例: Tailwind CSS + `<picture>` タグ:**

```html
<picture class="w-fit h-fit">
  <source srcset="image.webp" type="image/webp">
  <img src="image.jpg" alt="説明">
</picture>
```

`<picture>` タグはデフォルトで `display: inline` だが、Tailwindの `w-fit`/`h-fit` が適用されると期待通りのレイアウトにならない。

**修正:**

```html
<picture class="block w-full">
  <source srcset="image.webp" type="image/webp">
  <img src="image.jpg" alt="説明" class="w-full h-auto">
</picture>
```

#### プラグイン注入CSSの検出

EWWW, Smush等のプラグインがHTML内に `<style>` を注入する場合がある。

```bash
# プラグイン名が含まれる <style> タグを検出
grep -oP '<style[^>]*>.*?(ewww|smush|lazy).*?</style>' output.html
```

### Step 4: srcset実効解決シミュレーション

#### 背景

ブラウザは以下のロジックで `srcset` から画像を選択する:

1. `sizes` 属性を評価してビューポート幅を算出（例: `(max-width: 600px) 100vw` → 375px）
2. DPR（Device Pixel Ratio）を乗算（例: Retina = 2 → 375 × 2 = 750px）
3. `srcset` から 750px 以上の最小候補を選択（例: `800w`）

**問題**: サーバー側で 800px の画像が生成されていない、または404になっている場合、画像が表示されない。

#### シミュレーション手順

**1. `srcset` / `sizes` の抽出:**

```bash
# 最初の画像の srcset 抽出
grep -oP 'srcset="[^"]*"' output.html | head -1
# → srcset="image-400.jpg 400w, image-800.jpg 800w, image-1200.jpg 1200w"

# 対応する sizes 抽出
grep -oP 'sizes="[^"]*"' output.html | head -1
# → sizes="(max-width: 600px) 100vw, 50vw"
```

**2. モバイルビューポート（375px）での候補計算:**

```
sizes="(max-width: 600px) 100vw" → 375px × 1 = 375px (DPR=1)
Retina iPhone → 375px × 2 = 750px (DPR=2)
```

→ `srcset` から 750px 以上の最小候補 = `image-800.jpg 800w`

**3. 候補URLの実在確認:**

```bash
curl -I https://example.com/wp-content/uploads/image-800.jpg
# → HTTP/2 200 → 正常
# → HTTP/2 404 → 画像が生成されていない or パスが間違い
```

**4. 欠損サイズの検出:**

```bash
# srcset の全候補を抽出して実在確認
for url in $(grep -oP 'srcset="[^"]*"' output.html | head -1 | grep -oP 'https?://[^ ,]+'); do
  echo "Checking $url"
  curl -I "$url" 2>&1 | grep "HTTP/"
done
```

**欠損例:**

```
Checking https://example.com/image-400.jpg
HTTP/2 200
Checking https://example.com/image-800.jpg
HTTP/2 404  ← 欠損
Checking https://example.com/image-1200.jpg
HTTP/2 200
```

→ Retina iPhoneでは 800w が選ばれるが、404のため画像が表示されない。

**修正方法:**

1. WordPressの画像サイズ設定を確認（Settings → Media）
2. `add_image_size()` で 800px のサイズを追加
3. "Regenerate Thumbnails" プラグインで既存画像を再生成

### Step 5: キャッシュレイヤー分析

#### W3 Total Cache の痕跡検出

W3TCがHTMLを書き換える場合、以下のコメントが挿入される:

```bash
grep -oP '<!-- Performance optimized by W3 Total Cache.*? -->' output.html
```

**書き換え例:**

```html
<!-- Performance optimized by W3 Total Cache. Learn more: https://www.boldgrid.com/w3-total-cache/
Page Caching using disk: enhanced
Database Caching 10/50 queries in 0.002 seconds using disk
Object Caching 20/40 objects using disk
-->
```

→ Page Caching が有効。HTML出力がディスクキャッシュされている。

**問題**: テーマや プラグインの変更が反映されない（キャッシュが古い）。

**対処:**

1. W3TC管理画面で "Empty all caches"
2. または、キャッシュディレクトリを削除:

```bash
rm -rf /path/to/wp-content/cache/page_enhanced/*
```

#### Vary ヘッダーによるデバイス別キャッシュ

```bash
curl -I https://example.com/ | grep -i vary
# → Vary: Accept-Encoding, User-Agent
```

- `Vary: User-Agent`: UA別にキャッシュを分ける（デスクトップ/モバイルで別HTML）
- `Vary: Accept-Encoding`: gzip/brotli圧縮対応

**問題**: モバイルUAでアクセスしているのにデスクトップ用HTMLが返される（CDNキャッシュの誤配信）。

**対処:**

1. CDNのキャッシュをパージ（Cloudflare, Fastly等）
2. Vary ヘッダー設定を確認

#### CDN書き換え痕跡

Cloudflare, Fastly等のCDNがHTMLを書き換える場合がある（Auto Minify, Rocket Loader等）。

```bash
# Cloudflare の Rocket Loader
grep -o 'data-cfasync' output.html | wc -l

# Cloudflare の Auto Minify（空白削除）
head -1 output.html
# → <!DOCTYPE html><html lang="ja"><head>... （改行なし） → Minify有効
```

**問題**: Rocket Loader が Lazy Load のJSと競合して画像が読み込まれない。

**対処:**

1. Cloudflare管理画面で Rocket Loader を無効化
2. または、特定のJSを除外: `<script data-cfasync="false">`

### Step 6: UA別レスポンス差分検出

#### デスクトップ/モバイルでのHTML差分

```bash
# デスクトップHTML取得
curl -A "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36" \
  https://example.com/ > desktop.html

# モバイルHTML取得
curl -A "Mozilla/5.0 (iPhone; CPU iPhone OS 16_0 like Mac OS X)" \
  https://example.com/ > mobile.html

# 差分検出
diff desktop.html mobile.html > ua_diff.txt
wc -l ua_diff.txt
```

**差分がある場合の原因:**

1. テーマが `wp_is_mobile()` でUA判定してHTML出力を変えている
2. プラグインがモバイル専用のHTML（AMP等）を生成している
3. サーバー側でUA判定してテンプレートを切り替えている

**診断:**

```bash
# テーマファイルで wp_is_mobile() の使用箇所を検索
grep -r "wp_is_mobile" /path/to/theme/
```

**問題例:**

```php
<?php if (wp_is_mobile()): ?>
  <img src="mobile-image.jpg">
<?php else: ?>
  <img src="desktop-image.jpg">
<?php endif; ?>
```

→ レスポンシブ画像（`srcset`/`sizes`）を使わず、サーバー側で振り分けている。これではブラウザ幅に応じた最適化ができない。

**推奨修正:**

```php
<img src="desktop-image.jpg"
     srcset="mobile-image.jpg 400w, desktop-image.jpg 800w"
     sizes="(max-width: 600px) 100vw, 50vw">
```

### Step 7: 実践例（cmd_142のSTS調査）

#### 背景

- STS（WordPressポートフォリオサイト）で画像が一部表示されない
- PageSpeed Insights で "Serve images in next-gen formats" 警告
- EWWW Image Optimizer導入済みだが、WebPが配信されていない

#### 診断手順

**1. 本番HTML取得:**

```bash
curl -A "Mozilla/5.0 (iPhone; CPU iPhone OS 16_0 like Mac OS X)" \
  https://studiotatsuroshoji.com/ > sts_mobile.html
```

**2. WebP配信確認:**

```bash
grep 'type="image/webp"' sts_mobile.html | wc -l
# → 0 → WebP未配信
```

**3. Lazy Loading二重化検出:**

```bash
grep 'loading="lazy"' sts_mobile.html | wc -l
# → 15

grep 'data-src' sts_mobile.html | wc -l
# → 15

grep 'loading="lazy"' sts_mobile.html | grep 'data-src' | wc -l
# → 15 → 全画像で二重化
```

**4. srcset候補の404チェック:**

```bash
grep -oP 'srcset="[^"]*"' sts_mobile.html | head -1
# → srcset="image-400.jpg 400w, image-800.jpg 800w, image-1200.jpg 1200w"

for size in 400 800 1200; do
  curl -I https://studiotatsuroshoji.com/wp-content/uploads/image-$size.jpg 2>&1 | grep "HTTP/"
done
# → 400: 200, 800: 404, 1200: 200
# → 800px サイズが欠損
```

**5. CSS競合検出:**

```bash
curl https://studiotatsuroshoji.com/wp-content/themes/sts/style.css | \
  grep -E '(display|visibility).*!important'
# → .mobile-hidden { display: none !important; }
# → モバイルで一部要素が強制非表示
```

**修正内容（cmd_144で実施）:**

1. EWWW の Lazy Load を無効化（Native Lazy Loading のみ使用）
2. 800px 画像サイズを追加 + Regenerate Thumbnails 実行
3. `.mobile-hidden` の `!important` を削除してメディアクエリで調整

## Guidelines

1. **PHPコード読みだけでは不十分**: 実際のHTML出力を取得して検証する。キャッシュ、プラグイン、CDNがHTML出力を変える
2. **curl でのUA偽装は必須**: サーバーがUA別にHTML出力を変えている場合がある。必ずデスクトップ/モバイル両方で取得
3. **DevTools Network Panel の生HTMLを使用**: DevToolsのElementsタブはJS実行後のDOM。Network → Response が正確
4. **Lazy Loading二重化は最優先で修正**: パフォーマンス低下の主因。Native Lazy Loading を優先し、プラグインは無効化
5. **srcset候補の404は致命的**: ブラウザが選んだ候補が404だと画像が表示されない。全候補の実在確認は必須
6. **キャッシュクリアは診断前に実施**: W3TC, CDN, ブラウザキャッシュを全てクリアしてから取得
7. **CSS競合はComputed Stylesで確認**: DevToolsのComputedタブで実際に適用されているCSSを確認。どのファイルの何行目が適用されているか特定
8. **Vary ヘッダーの確認**: `Vary: User-Agent` が設定されていない場合、CDNがUA別キャッシュを正しく配信しない
9. **モバイルビューポートは375pxで計算**: 最も一般的なモバイル幅。Retina(DPR=2)考慮で 750px の画像が必要
10. **診断結果はチェックリスト化**: 次回診断時の比較材料として記録。再発防止に役立つ
