---
name: wp-picture-webp-auditor
description: WordPress <picture>タグWebP実装の品質監査パターン。srcset候補のWebPファイル存在確認、Lazy Loading除外マーカー（skip-lazy）、CSS競合（w-fit/h-fit）、プラグインHTML書き換え結果の検証を実施。テーマの画像処理全般ではなく、<picture>タグWebP実装に特化。
---

# WordPress Picture WebP Auditor

## Overview

WordPress テーマの `<picture>` タグ WebP実装の品質を監査するパターン。テーマの画像処理全般（`wordpress-theme-image-audit`）ではなく、`<picture>` タグを使ったWebP配信の実装品質に特化する。以下を一貫して実施する：

- **`<picture>` タグ構造の検証**: `<source type="image/webp">` の正しい実装パターン確認
- **srcset候補のWebPファイル存在確認**: PHPの `file_exists()` パス変換、`wp_upload_dir()` とWebP変換パスの対応、欠損サイズ検出
- **Lazy Loading除外マーカー**: `skip-lazy`, `data-skip-lazy="1"` の適用基準、ファーストビュー画像の特定、`loading="eager"` vs `loading="lazy"` の使い分け
- **CSS競合検出（`<picture>` 固有）**: `w-fit`/`h-fit` によるレイアウト崩れ、`<picture>` 要素のデフォルト `display` 動作の検証
- **プラグインHTML書き換え検証**: EWWW/Smush等のプラグイン出力と期待出力の比較、`data-*` 属性の正当性確認
- **実践的監査チェックリスト**: コピペ可能な形式で提供

## When to Use

- `<picture>` タグを実装したが、WebPが配信されない or フォールバックが動作しない
- 一部の画像サイズ（400w, 800w等）でWebPが欠損している
- ファーストビュー画像がLazy Loadingで遅延し、LCP（Largest Contentful Paint）が悪化している
- Tailwind CSS等のユーティリティCSSと `<picture>` タグの相性問題でレイアウトが崩れる
- プラグイン（EWWW, Smush等）がHTML出力を書き換えているが、期待通りに動作しない
- `<picture>` タグを使ったテーマ実装のコードレビュー

## Instructions

### Step 1: `<picture>` タグ構造の検証

#### 正しい実装パターン

```html
<picture>
  <source
    srcset="image-400.webp 400w, image-800.webp 800w, image-1200.webp 1200w"
    sizes="(max-width: 600px) 100vw, 50vw"
    type="image/webp">
  <source
    srcset="image-400.jpg 400w, image-800.jpg 800w, image-1200.jpg 1200w"
    sizes="(max-width: 600px) 100vw, 50vw"
    type="image/jpeg">
  <img src="image-800.jpg" alt="説明" loading="lazy">
</picture>
```

**重要ポイント:**

1. WebP `<source>` は JPEG `<source>` より前に配置（ブラウザは上から順に評価）
2. 各 `<source>` に `type` 属性を指定（`image/webp`, `image/jpeg`）
3. `<img>` タグにフォールバック用の `src` 属性を指定
4. `srcset` と `sizes` は WebP/JPEG 両方に同じ値を設定
5. `loading` 属性は `<img>` タグに設定（`<source>` には不要）

#### 検証コマンド

```bash
# <picture> タグの使用箇所を抽出
grep -A10 '<picture' output.html | head -50

# WebP source の type 属性確認
grep -A10 '<picture' output.html | grep 'type="image/webp"'

# フォールバック <img> の src 属性確認
grep -A10 '<picture' output.html | grep -oP '<img[^>]*src="[^"]*"'
```

#### よくある実装ミス

**ミス1: WebP source が JPEG より後ろ**

```html
<!-- ❌ 間違い: JPEG が先に評価される -->
<picture>
  <source srcset="image.jpg" type="image/jpeg">
  <source srcset="image.webp" type="image/webp">
  <img src="image.jpg" alt="">
</picture>
```

**ミス2: type 属性の欠落**

```html
<!-- ❌ 間違い: ブラウザが MIME type を推測できない -->
<picture>
  <source srcset="image.webp">
  <img src="image.jpg" alt="">
</picture>
```

**ミス3: img タグの src 欠落**

```html
<!-- ❌ 間違い: フォールバックが動作しない -->
<picture>
  <source srcset="image.webp" type="image/webp">
  <img alt="説明">
</picture>
```

### Step 2: srcset候補のWebPファイル存在確認

#### 問題パターン

`<picture>` タグのHTMLは正しいが、実際のWebPファイルが生成されていない、または一部サイズが欠損している場合がある。

**例:**

```html
<source srcset="image-400.webp 400w, image-800.webp 800w, image-1200.webp 1200w" type="image/webp">
```

→ `image-800.webp` が存在しない（Regenerate Thumbnails未実行、EWWW変換エラー等）

#### 検証手順

**1. srcset からWebP URLを抽出:**

```bash
grep -oP '<source[^>]*type="image/webp"[^>]*srcset="\K[^"]*' output.html | head -1
# → image-400.webp 400w, image-800.webp 800w, image-1200.webp 1200w
```

**2. 各URLの実在確認:**

```bash
# srcset から URL を抽出して実在確認
for url in $(echo "image-400.webp 400w, image-800.webp 800w" | grep -oP '\S+\.webp'); do
  full_url="https://example.com/wp-content/uploads/2024/01/$url"
  echo "Checking $full_url"
  curl -I "$full_url" 2>&1 | grep "HTTP/"
done
```

**3. 欠損サイズの特定:**

```bash
# 全候補をループして 404 を検出
for url in image-{400,800,1200}.webp; do
  status=$(curl -I https://example.com/wp-content/uploads/$url 2>&1 | grep -oP 'HTTP/[^ ]+ \K[0-9]+')
  if [ "$status" != "200" ]; then
    echo "Missing: $url (HTTP $status)"
  fi
done
```

**出力例:**

```
Missing: image-800.webp (HTTP 404)
```

#### PHPでのfile_exists()パス変換検証

WordPress プラグイン（EWWW, Smush等）は以下のロジックでWebPファイルの存在を確認する:

```php
$upload_dir = wp_upload_dir();
$base_path = $upload_dir['basedir']; // /var/www/html/wp-content/uploads
$image_path = $base_path . '/2024/01/image.jpg';
$webp_path = $image_path . '.webp'; // image.jpg.webp

if (file_exists($webp_path)) {
  // <source srcset="image.webp"> を出力
}
```

**問題:**

- サーバー移行後にパスが変わる（`/var/www` → `/home/user`）
- `ABSPATH` や `WP_CONTENT_DIR` が古い値のまま
- シンボリックリンクが解決されない

**検証方法:**

```bash
# wp-config.php のパス設定を確認
grep -E "(ABSPATH|WP_CONTENT_DIR)" /path/to/wp-config.php

# 実際のアップロードディレクトリを確認
ls -la /var/www/html/wp-content/uploads/

# WebPファイルの命名パターンを確認（.jpg.webp vs .webp）
find /var/www/html/wp-content/uploads -name "*.webp" | head -5
# → image.jpg.webp（EWWWのパターン）
# → image.webp（変換後にリネーム）
```

#### 欠損WebPファイルの生成

**方法A: EWWW Image Optimizer の Bulk Optimize**

1. WordPress管理画面 → Media → Bulk Optimize
2. "Optimize Everything" を選択
3. "Scan for unoptimized images"
4. 最適化実行

**方法B: WP-CLI での一括変換**

```bash
wp media regenerate --yes
wp ewww-image-optimizer optimize all
```

**方法C: 手動でのWebP生成（cwebp コマンド）**

```bash
# cwebp のインストール（Ubuntu/Debian）
sudo apt install webp

# 単一ファイルのWebP変換
cwebp -q 80 image.jpg -o image.webp

# ディレクトリ内の全JPGをWebP化
find /path/to/uploads -name "*.jpg" -exec sh -c 'cwebp -q 80 "$1" -o "${1%.jpg}.webp"' _ {} \;
```

### Step 3: Lazy Loading除外マーカー

#### 問題: ファーストビュー画像のLazy Loading

ファーストビュー（Above the Fold）の画像をLazy Loadingすると、LCP（Largest Contentful Paint）が悪化する。

**PageSpeed Insights 警告:**

> "Largest Contentful Paint element was lazy loaded"

#### 除外マーカーの種類

| マーカー | 用途 | プラグイン |
|---------|------|-----------|
| `loading="eager"` | Native Lazy Loading を無効化 | WordPress Core, テーマ |
| `data-skip-lazy="1"` | EWWW の Lazy Load を除外 | EWWW Image Optimizer |
| `skip-lazy` クラス | WP Rocket, a3 Lazy Load を除外 | WP Rocket, a3 Lazy Load |
| `data-no-lazy="1"` | Jetpack Lazy Load を除外 | Jetpack |

#### 実装例

**ファーストビュー画像（ヒーロー画像）:**

```html
<picture>
  <source srcset="hero.webp" type="image/webp">
  <img src="hero.jpg" alt="ヒーロー画像" loading="eager" data-skip-lazy="1" class="skip-lazy">
</picture>
```

**スクロールが必要な画像（通常のコンテンツ）:**

```html
<picture>
  <source srcset="content.webp" type="image/webp">
  <img src="content.jpg" alt="コンテンツ画像" loading="lazy">
</picture>
```

#### ファーストビュー画像の特定方法

**方法A: Chrome DevTools でのLCP確認**

1. DevTools → Performance タブ
2. リロードして記録
3. Timings → LCP をクリック
4. 該当する画像要素が表示される

**方法B: PageSpeed Insights**

1. https://pagespeed.web.dev/ でURL入力
2. "View Treemap" → "Largest Contentful Paint element" を確認
3. 該当する画像のセレクタ（クラス名、ID等）を特定

**方法C: テーマテンプレートでの判定**

```php
<?php
// ヒーロー画像（ファーストビュー）
if (is_front_page() && is_main_query()) {
  $loading = 'eager';
  $skip_lazy = 'data-skip-lazy="1"';
} else {
  $loading = 'lazy';
  $skip_lazy = '';
}
?>
<img src="image.jpg" loading="<?php echo $loading; ?>" <?php echo $skip_lazy; ?>>
```

#### 除外マーカーの検証

```bash
# loading="eager" の使用箇所
grep -o 'loading="eager"' output.html | wc -l

# data-skip-lazy の使用箇所
grep -o 'data-skip-lazy' output.html | wc -l

# skip-lazy クラスの使用箇所
grep -o 'class="[^"]*skip-lazy' output.html | wc -l
```

**問題例:**

```bash
# ファーストビュー画像が lazy になっている
grep -A5 '<picture' output.html | head -10 | grep 'loading="lazy"'
# → <img src="hero.jpg" loading="lazy"> → LCP悪化の原因
```

### Step 4: CSS競合検出（`<picture>` 固有の問題）

#### 問題: `<picture>` タグのデフォルト display 動作

`<picture>` タグはデフォルトで `display: inline` として扱われる。これがTailwind CSS等のユーティリティクラスと競合する。

**問題のあるHTML:**

```html
<picture class="w-fit h-fit">
  <source srcset="image.webp" type="image/webp">
  <img src="image.jpg" alt="説明">
</picture>
```

**症状:**

- 画像が期待したサイズで表示されない
- `<picture>` タグが余白を持つ
- レイアウトが崩れる

**原因:**

Tailwindの `w-fit` / `h-fit` は `display: block` 前提の設計。`display: inline` の `<picture>` には正しく適用されない。

#### 修正方法

**方法A: `<picture>` に `display: block` を適用**

```html
<picture class="block w-full">
  <source srcset="image.webp" type="image/webp">
  <img src="image.jpg" alt="説明" class="w-full h-auto">
</picture>
```

**方法B: `<picture>` にスタイルを適用しない（`<img>` のみ）**

```html
<picture>
  <source srcset="image.webp" type="image/webp">
  <img src="image.jpg" alt="説明" class="w-full h-auto">
</picture>
```

**推奨**: 方法Bの方がシンプル。`<picture>` タグは構造的な要素として扱い、スタイルは `<img>` に適用する。

#### 検証手順

**1. CSS競合の検出:**

```bash
# <picture> タグに w-fit/h-fit が適用されているか確認
grep -oP '<picture[^>]*class="[^"]*[wh]-fit' output.html

# <picture> タグに display 関連のクラスが適用されているか確認
grep -oP '<picture[^>]*class="[^"]*' output.html | grep -E '(block|inline|flex|grid)'
```

**2. DevTools Computed Styles での確認:**

1. Chrome DevTools → Elements
2. `<picture>` タグを選択
3. Computed タブ → `display` の値を確認
   - `inline`: デフォルト（問題なし）
   - `block`, `flex` 等: CSS適用済み

**3. レイアウト崩れの確認:**

- 画像が親要素からはみ出している
- 画像の下に謎の余白がある（`vertical-align: baseline` の影響）

#### 他のCSS競合パターン

**問題1: `vertical-align: baseline` による余白**

```css
/* 問題のあるCSS */
picture {
  vertical-align: baseline; /* デフォルト値 */
}
```

→ `<picture>` の下に数pxの余白が発生。

**修正:**

```css
picture {
  display: block; /* inline を block に変更 */
}
/* または */
picture {
  vertical-align: top; /* baseline を top に変更 */
}
```

**問題2: `max-width` の未設定**

```html
<picture>
  <source srcset="large-image.webp" type="image/webp">
  <img src="large-image.jpg" alt=""> <!-- max-width 未設定 -->
</picture>
```

→ 画像が親要素の幅を超えてはみ出す。

**修正:**

```html
<picture>
  <source srcset="large-image.webp" type="image/webp">
  <img src="large-image.jpg" alt="" style="max-width: 100%; height: auto;">
</picture>
```

### Step 5: プラグインHTML書き換え検証

#### EWWW Image Optimizer の書き換えパターン

EWWW は以下のロジックでHTMLを書き換える:

1. テーマが `<img src="image.jpg">` を出力
2. EWWW のフィルター（`the_content`, `post_thumbnail_html` 等）が発火
3. `image.jpg.webp` の存在を確認
4. 存在すれば `<picture>` タグに変換

**期待される出力:**

```html
<!-- テーマ出力（元） -->
<img src="image.jpg" alt="説明">

<!-- EWWW書き換え後 -->
<picture>
  <source srcset="image.webp" type="image/webp">
  <img src="image.jpg" alt="説明">
</picture>
```

#### 検証手順

**1. プラグイン設定の確認:**

- EWWW → Settings → WebP → "Picture WebP Rewriting" が有効か確認
- "JS WebP Rewriting" は無効化（JavaScriptで書き換える方式は非推奨）

**2. HTML出力の確認:**

```bash
# <picture> タグが生成されているか確認
grep -c '<picture' output.html

# EWWW が書き換えた痕跡（data-eio 属性等）
grep -oP 'data-eio="[^"]*"' output.html
```

**3. 書き換え失敗のパターン:**

| 症状 | 原因 | 対処 |
|------|------|------|
| `<picture>` タグが生成されない | WebPファイルが存在しない | Bulk Optimize 実行 |
| `<source>` に `type` 属性がない | EWWWのバグ（古いバージョン） | プラグイン更新 |
| `srcset` が空 | `file_exists()` パス解決失敗 | `wp_upload_dir()` 確認 |

#### Smush の書き換えパターン

Smush は独自の `data-src` / `data-srcset` 属性を使用する:

```html
<!-- Smush書き換え後 -->
<picture>
  <source data-srcset="image.webp" type="image/webp">
  <img data-src="image.jpg" alt="説明" class="lazyload">
</picture>
```

**問題**: Lazy Load のJSが動作しないと画像が表示されない。

**修正**: Smush の "Lazy Load" を無効化し、Native Lazy Loading を使用。

### Step 6: 実践的監査チェックリスト

以下のチェックリストをコピペして使用する:

#### HTML構造

- [ ] `<picture>` タグが正しく使用されている
- [ ] WebP `<source>` が JPEG `<source>` より前に配置されている
- [ ] 各 `<source>` に `type="image/webp"` / `type="image/jpeg"` が指定されている
- [ ] `<img>` タグにフォールバック用の `src` 属性が設定されている
- [ ] `srcset` と `sizes` が WebP/JPEG 両方に同じ値で設定されている

#### WebPファイル存在確認

- [ ] `srcset` の全候補URL（400w, 800w, 1200w等）が実在する（curl -I で確認）
- [ ] WebPファイルの命名パターンが正しい（`.jpg.webp` or `.webp`）
- [ ] EWWW Bulk Optimize を実行済み（または WP-CLI で再生成）
- [ ] `file_exists()` のパス解決が正しい（デバッグログ確認）

#### Lazy Loading

- [ ] ファーストビュー画像に `loading="eager"` + `data-skip-lazy="1"` が設定されている
- [ ] スクロールが必要な画像に `loading="lazy"` が設定されている
- [ ] Native Lazy Loading とプラグインLazy Loadingが競合していない
- [ ] PageSpeed Insights で "Largest Contentful Paint element was lazy loaded" 警告が出ていない

#### CSS

- [ ] `<picture>` タグに `w-fit`/`h-fit` が適用されていない（または `display: block` で対処）
- [ ] `<img>` タグに `max-width: 100%; height: auto;` が設定されている
- [ ] `<picture>` タグの下に謎の余白がない（`vertical-align` 確認）
- [ ] レスポンシブレイアウトで画像が崩れない（Chrome DevToolsで確認）

#### プラグイン

- [ ] EWWW の "Picture WebP Rewriting" が有効
- [ ] EWWW の "JS WebP Rewriting" が無効（非推奨）
- [ ] Smush の "Lazy Load" が無効（Native Lazy Loading使用）
- [ ] W3 Total Cache のキャッシュをクリア済み
- [ ] CDN（Cloudflare等）のキャッシュをクリア済み

## Examples

### Example 1: cmd_144 STSテーマ修正

**背景:**

- STSテーマで `<picture>` タグを実装
- WebPファイルは存在するが、一部サイズ（800w）が404
- ファーストビュー画像が `loading="lazy"` になっている

**監査結果:**

```bash
# WebP候補の実在確認
for size in 400 800 1200; do
  curl -I https://studiotatsuroshoji.com/uploads/image-$size.webp 2>&1 | grep "HTTP/"
done
# → 400: 200, 800: 404, 1200: 200

# ファーストビュー画像のLazy Loading確認
grep -A10 '<picture' output.html | head -15 | grep 'loading'
# → <img src="hero.jpg" loading="lazy"> → LCP悪化
```

**修正内容:**

1. 800px サイズを `functions.php` で追加:

```php
add_image_size('medium-large', 800, 0, false);
```

2. Regenerate Thumbnails 実行
3. ヒーロー画像のLazy Load除外:

```php
<picture>
  <source srcset="hero.webp" type="image/webp">
  <img src="hero.jpg" alt="ヒーロー" loading="eager" data-skip-lazy="1">
</picture>
```

**結果:**

- PageSpeed Insights スコア: 72 → 89
- LCP: 3.2s → 1.8s

### Example 2: Tailwind CSS + `<picture>` レイアウト崩れ

**問題のあるHTML:**

```html
<picture class="w-fit h-fit">
  <source srcset="image.webp" type="image/webp">
  <img src="image.jpg" alt="説明">
</picture>
```

**症状:**

- 画像が親要素からはみ出す
- `<picture>` タグが余白を持つ

**修正:**

```html
<picture class="block">
  <source srcset="image.webp" type="image/webp">
  <img src="image.jpg" alt="説明" class="w-full h-auto">
</picture>
```

**結果:**

- レイアウト正常化
- 余白消失

## Guidelines

1. **`<picture>` タグは構造的要素**: スタイル（w-fit等）は `<img>` に適用し、`<picture>` には最小限のクラスのみ
2. **WebP候補の404は致命的**: `srcset` の全候補の実在確認は必須。欠損があればRegenerate Thumbnails
3. **ファーストビュー画像はLazy Load除外**: `loading="eager"` + `data-skip-lazy="1"` でLCPを改善
4. **type 属性は必須**: `<source>` タグには必ず `type="image/webp"` を指定。省略するとブラウザが推測に失敗
5. **srcset/sizes は両方に設定**: WebP/JPEG 両方の `<source>` に同じ `srcset`/`sizes` を設定
6. **プラグインLazy Loadは無効化**: Native Lazy Loading を優先。二重化はパフォーマンス低下の原因
7. **file_exists() パスは移行時に再確認**: サーバー移行後は `ABSPATH`/`WP_CONTENT_DIR` を確認
8. **DevTools Computed Stylesで検証**: CSS競合は Elements → Computed で実際に適用されているスタイルを確認
9. **チェックリストを埋めて記録**: 次回監査時の比較材料として記録。再発防止に役立つ
10. **wordpress-theme-image-auditとの使い分け**: 画像処理全般の診断は `wordpress-theme-image-audit`、`<picture>` タグWebP実装に特化した監査は本スキルを使用
