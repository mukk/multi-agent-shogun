---
name: wordpress-theme-image-audit
description: WordPressテーマの画像処理を網羅的に診断する。WebP配信、レスポンシブ画像、Lazy Loading、キャッシュプラグイン競合、file_exists()パス検証を含む。PHPコードだけでなく本番HTML出力の実取得による検証を実施（モバイル表示・パフォーマンス最適化）。
---

# WordPress Theme Image Audit

## Overview

WordPressテーマの画像処理が正しく動作しているか網羅的に診断するパターン。画像最適化プラグイン（EWWW Image Optimizer, Smush等）、キャッシュプラグイン（W3 Total Cache, WP Super Cache等）、テーマ独自の画像処理コード、ネイティブLazy Loadingの相互作用を確認する。以下を一貫して実施する：

- **WebP配信パターンの検証**: `<picture>` タグ + WebPフォールバック、`.htaccess` リライト、プラグイン設定の確認
- **レスポンシブ画像の検証**: `srcset` / `sizes` 属性の正当性、ブレークポイントの適切性
- **Lazy Loading二重化リスク**: Native Lazy Loading（`loading="lazy"`）とプラグインの競合チェック
- **キャッシュプラグイン競合**: HTML出力のキャッシュが画像最適化を妨げていないか確認
- **file_exists() パス変換検証**: プラグインがファイルパスを正しく変換しているか確認（WebP生成後の存在確認）
- **本番HTML出力の実取得**: PHPコード読みだけでは不十分。実際のHTML出力を取得して検証（cmd_142の教訓）
- **モバイル表示検証**: Chrome DevTools / 実機でのレンダリング確認、Network Panel でのリソース取得確認

## When to Use

- WordPressサイトで画像が表示されない・WebPが配信されない問題を診断する
- 画像最適化プラグインを導入したが効果が出ていない
- モバイルで画像が正しく表示されない（srcset/sizes の問題）
- PageSpeed Insights で画像関連の警告が出ている
- テーマ更新後に画像処理が壊れた
- カスタムテーマに画像最適化を導入する前に既存実装を調査する

## Instructions

### Step 1: 本番HTML出力の実取得

**重要**: PHPコードを読むだけでは不十分。実際のHTML出力を取得して検証する。

#### 方法A: curl でのHTML取得

```bash
# トップページのHTML取得
curl -A "Mozilla/5.0" https://example.com/ > output.html

# 特定の投稿ページのHTML取得
curl -A "Mozilla/5.0" https://example.com/post-slug/ > post.html

# User-Agent を指定してモバイル版HTML取得
curl -A "Mozilla/5.0 (iPhone; CPU iPhone OS 16_0 like Mac OS X)" \
  https://example.com/ > mobile.html
```

#### 方法B: Chrome DevTools でのHTML取得

1. ページを開く
2. DevTools → Elements タブ
3. `<html>` 要素を右クリック → "Copy" → "Copy outerHTML"
4. テキストエディタに貼り付けて保存

#### 方法C: wget でのミラーリング（複数ページ一括取得）

```bash
wget --mirror --convert-links --adjust-extension \
  --page-requisites --no-parent \
  --user-agent="Mozilla/5.0" \
  https://example.com/
```

### Step 2: WebP配信パターンの検証

#### Pattern A: `<picture>` タグ + WebPフォールバック

正しい実装例:

```html
<picture>
  <source srcset="image.webp" type="image/webp">
  <source srcset="image.jpg" type="image/jpeg">
  <img src="image.jpg" alt="説明">
</picture>
```

**検証手順:**

1. 取得したHTMLで `<picture>` タグを検索
2. `<source>` タグに `type="image/webp"` が存在するか確認
3. WebPファイルのURLをブラウザで直接開いて取得可能か確認
4. `<img>` タグにフォールバック用の `src` が設定されているか確認

**診断コマンド:**

```bash
# <picture> タグの使用箇所をカウント
grep -o '<picture' output.html | wc -l

# WebP source タグの抽出
grep -A2 '<picture' output.html | grep 'type="image/webp"'

# WebP URLの抽出（実在確認用）
grep -oP 'srcset="\K[^"]*\.webp' output.html
```

#### Pattern B: `.htaccess` リライト（透過的WebP配信）

EWWW Image Optimizer等が使用するパターン。JPG/PNGリクエストを自動的にWebPにリダイレクト。

`.htaccess` の確認:

```bash
# WordPress root の .htaccess を確認
cat /path/to/wordpress/.htaccess | grep -A10 "webp"
```

期待される記述:

```apache
<IfModule mod_rewrite.c>
  RewriteEngine On
  RewriteCond %{HTTP_ACCEPT} image/webp
  RewriteCond %{REQUEST_FILENAME} (.*)\.(jpe?g|png)$
  RewriteCond %{REQUEST_FILENAME}\.webp -f
  RewriteRule (.+)\.(jpe?g|png)$ $1.$2.webp [T=image/webp,E=accept:1]
</IfModule>

<IfModule mod_headers.c>
  Header append Vary Accept env=REDIRECT_accept
</IfModule>

AddType image/webp .webp
```

**検証:**

1. `.htaccess` に上記のルールが存在するか確認
2. `RewriteCond %{REQUEST_FILENAME}\.webp -f` がファイル存在確認を行っているか確認
3. WebPファイルが実際に生成されているか確認:

```bash
# uploads ディレクトリでWebPファイルを検索
find /path/to/wp-content/uploads -name "*.webp" | head -20
```

#### Pattern C: プラグイン設定の確認

**EWWW Image Optimizer:**

- 管理画面 → Settings → EWWW Image Optimizer
- "WebP Conversion" が有効か確認
- "JS WebP Rewriting" または "Picture WebP Rewriting" が有効か確認
- "Bulk Optimize" を実行して既存画像がWebP化されているか確認

**診断手順:**

```sql
-- EWWW 設定の確認（wp-config.php で定義された場合）
SELECT option_name, option_value FROM wp_options
WHERE option_name LIKE '%ewww%' AND option_name LIKE '%webp%';
```

### Step 3: レスポンシブ画像の検証

#### srcset / sizes 属性の確認

正しい実装例:

```html
<img src="image-800.jpg"
     srcset="image-400.jpg 400w,
             image-800.jpg 800w,
             image-1200.jpg 1200w"
     sizes="(max-width: 600px) 100vw,
            (max-width: 1200px) 50vw,
            33vw"
     alt="説明">
```

**検証手順:**

1. HTMLで `srcset` 属性を検索
2. 各URL（`image-400.jpg`, `image-800.jpg`, etc.）が実在するか確認
3. `sizes` 属性のブレークポイントがテーマのCSSと一致するか確認

**診断コマンド:**

```bash
# srcset 属性の抽出
grep -oP 'srcset="[^"]*"' output.html | head -5

# sizes 属性の抽出
grep -oP 'sizes="[^"]*"' output.html | head -5

# srcset で指定された画像URLの実在確認
for url in $(grep -oP 'srcset="[^"]*"' output.html | head -1 | grep -oP 'https?://[^ ,]+'); do
  echo "Checking $url"
  curl -I "$url" | grep "HTTP/"
done
```

#### WordPress Core の `wp_get_attachment_image()` 検証

WordPressは自動的にレスポンシブ画像を生成する。テーマが `the_post_thumbnail()` や `wp_get_attachment_image()` を使っているか確認。

```bash
# テーマファイルで the_post_thumbnail の使用箇所を検索
grep -r "the_post_thumbnail\|wp_get_attachment_image" /path/to/theme/
```

カスタム実装（`<img src="...">`を直接書いている）の場合、レスポンシブ画像が生成されない可能性がある。

### Step 4: Lazy Loading二重化リスクの確認

#### Native Lazy Loading（Chrome 76+, Firefox 75+）

ブラウザネイティブのLazy Loading:

```html
<img src="image.jpg" loading="lazy" alt="説明">
```

#### プラグインによるLazy Loading

- Lazy Load by WP Rocket
- a3 Lazy Load
- Jetpack Lazy Loading

**問題**: Native Lazy LoadingとプラグインLazy Loadingが同時に有効だと、以下の問題が発生する可能性:

- 画像が2回遅延ロードされる（パフォーマンス低下）
- プラグインが `data-src` 属性を使用し、ブラウザが `loading="lazy"` を無視する
- 一部の画像が読み込まれない

**検証手順:**

1. HTMLで `loading="lazy"` が使用されているか確認:

```bash
grep -o 'loading="lazy"' output.html | wc -l
```

2. プラグインの `data-src` / `data-lazy-src` 属性が使用されているか確認:

```bash
grep -oP 'data-(lazy-)?src="[^"]*"' output.html | head -10
```

3. 両方が存在する場合は競合の可能性がある

**修正方法:**

- プラグインのLazy Loadingを無効化（Native Lazy Loadingを優先）
- または、プラグイン設定で "Skip images with native lazy loading" を有効化

### Step 5: キャッシュプラグイン競合チェック

#### W3 Total Cache / WP Super Cache

キャッシュプラグインがHTML出力をキャッシュしている場合、画像最適化プラグインの変更が反映されない可能性がある。

**検証手順:**

1. キャッシュをクリア:
   - W3 Total Cache: Performance → Dashboard → "Empty all caches"
   - WP Super Cache: Settings → WP Super Cache → "Delete Cache"

2. キャッシュクリア後に再度HTMLを取得して確認:

```bash
curl -A "Mozilla/5.0" https://example.com/ > output-after-clear.html
diff output.html output-after-clear.html
```

3. 差分がある場合、キャッシュが原因で古いHTML出力が配信されていた

#### CDN キャッシュの確認

Cloudflare, Fastly等のCDNを使用している場合、CDN側のキャッシュもクリアする必要がある。

```bash
# Cloudflare API でキャッシュをパージ
curl -X POST "https://api.cloudflare.com/client/v4/zones/{zone_id}/purge_cache" \
  -H "Authorization: Bearer {api_token}" \
  -H "Content-Type: application/json" \
  --data '{"purge_everything":true}'
```

### Step 6: file_exists() パス変換の検証

#### 問題: WebP生成後のパス変換失敗

EWWW Image Optimizer等のプラグインは以下の処理を行う:

1. `image.jpg` をアップロード
2. `image.jpg.webp` を生成
3. PHP内で `file_exists($webp_path)` を確認
4. 存在すれば `<source srcset="image.webp">` を出力

**パス変換のバグ例:**

- WordPress の `wp-content/uploads/2024/01/image.jpg` を絶対パスで処理
- プラグインが `/var/www/html/wp-content/uploads/2024/01/image.jpg.webp` を期待
- 実際のパスは `/home/user/public_html/wp-content/uploads/2024/01/image.jpg.webp`
- `file_exists()` が `false` を返し、WebPが出力されない

**検証手順:**

1. プラグインのPHPコードで `file_exists()` の使用箇所を検索:

```bash
grep -r "file_exists.*webp" /path/to/wp-content/plugins/ewww-image-optimizer/
```

2. パス変換ロジックを確認（`ABSPATH`, `WP_CONTENT_DIR`, `wp_upload_dir()` の使用）

3. デバッグログを有効化してパスを出力:

```php
// functions.php に追記
add_filter('ewww_image_optimizer_webp_path', function($path) {
  error_log("WebP path: $path, exists: " . (file_exists($path) ? 'yes' : 'no'));
  return $path;
});
```

4. エラーログを確認:

```bash
tail -f /path/to/wordpress/wp-content/debug.log
```

#### 修正方法

- プラグイン設定で "Ludicrous Mode" や "Use WebP via rewrite rules" を試す
- `.htaccess` のリライトルールでファイルパスを修正
- サーバー移行後は `wp-config.php` の `ABSPATH` / `WP_CONTENT_DIR` を再確認

### Step 7: モバイル表示の検証

#### Chrome DevTools でのモバイルエミュレーション

1. Chrome DevTools → Device Toolbar (Cmd+Shift+M / Ctrl+Shift+M)
2. "Responsive" → "iPhone 14 Pro" 等を選択
3. Network Panel → Disable cache → リロード
4. 画像リクエストを確認:
   - WebPが配信されているか
   - srcset で適切なサイズが選択されているか（400w, 800w等）
   - Lazy Loadingが動作しているか（スクロールで読み込み）

#### 実機でのテスト

1. モバイル端末で対象URLを開く
2. ブラウザの開発者ツール（Safari: 設定 → Safari → 詳細 → Webインスペクタ）
3. または、リモートデバッグ:
   - Chrome: chrome://inspect
   - Safari: Develop → [Device] → [Page]

#### Network Panel での確認項目

| 項目 | 正常 | 異常 |
|------|------|------|
| Content-Type | `image/webp` | `image/jpeg` (WebP未配信) |
| ファイルサイズ | 元画像の50%以下 | 元画像と同等（圧縮失敗） |
| srcset選択 | 画面幅に応じた適切なサイズ | 常に最大サイズ（srcset無効） |
| Lazy Loading | スクロールで読み込み | 初回ロードで全画像読み込み |

#### PageSpeed Insights での診断

```bash
# PSI API でスコア取得（モバイル）
curl "https://www.googleapis.com/pagespeedonline/v5/runPagespeed?url=https://example.com/&strategy=mobile" \
  | jq '.lighthouseResult.audits["modern-image-formats"]'
```

**確認項目:**

- "Serve images in next-gen formats" → WebP配信の有無
- "Properly size images" → srcset/sizes の適切性
- "Defer offscreen images" → Lazy Loadingの実装

### Step 8: 診断チェックリスト

以下のチェックリストを使って網羅的に診断する:

#### WebP配信

- [ ] 本番HTML出力に `<source type="image/webp">` が存在する
- [ ] WebPファイルが実在する（curlで取得可能）
- [ ] `.htaccess` にWebPリライトルールが存在する（リライト方式の場合）
- [ ] EWWW Image Optimizer等のプラグイン設定でWebP変換が有効
- [ ] `Accept: image/webp` ヘッダー送信時にWebPが返される

#### レスポンシブ画像

- [ ] `srcset` 属性に複数サイズが指定されている
- [ ] `sizes` 属性がテーマのブレークポイントと一致する
- [ ] 各サイズの画像ファイルが実在する
- [ ] `wp_get_attachment_image()` / `the_post_thumbnail()` を使用している

#### Lazy Loading

- [ ] Native Lazy Loading（`loading="lazy"`）が実装されている
- [ ] プラグインLazy Loadingと競合していない
- [ ] スクロールで画像が遅延ロードされる（Network Panel確認）

#### キャッシュ

- [ ] W3 Total Cache / WP Super Cache のキャッシュをクリア後、変更が反映される
- [ ] CDNキャッシュをクリア後、変更が反映される
- [ ] HTML出力がキャッシュされていない（動的に生成される）

#### file_exists() パス検証

- [ ] WebPファイルのパスが正しい（デバッグログ確認）
- [ ] `file_exists()` が `true` を返す（プラグインログ確認）
- [ ] サーバー移行後もパスが正しい（ABSPATH確認）

#### モバイル表示

- [ ] Chrome DevTools のモバイルエミュレーションで正しく表示される
- [ ] 実機でWebPが配信される（Network Panel確認）
- [ ] PageSpeed Insights (Mobile) で "Serve images in next-gen formats" が緑

## Examples

### Example 1: EWWW Image Optimizer + W3 Total Cache の診断

```bash
# Step 1: 本番HTML取得
curl -A "Mozilla/5.0" https://example.com/ > output.html

# Step 2: WebP配信確認
grep 'type="image/webp"' output.html
# → 結果なし → WebP未配信

# Step 3: キャッシュクリア
# W3TC管理画面で "Empty all caches"

# Step 4: 再取得
curl -A "Mozilla/5.0" https://example.com/ > output-after.html
grep 'type="image/webp"' output-after.html
# → <source srcset="...webp" type="image/webp"> 発見
# → キャッシュが原因だった

# Step 5: WebPファイル実在確認
grep -oP 'srcset="\K[^"]*\.webp' output-after.html | head -1 | xargs curl -I
# → HTTP/2 200 → 配信成功
```

### Example 2: srcset/sizes の検証

```bash
# Step 1: srcset 属性の抽出
grep -oP 'srcset="[^"]*"' output.html | head -1
# → srcset="image-400.jpg 400w, image-800.jpg 800w, image-1200.jpg 1200w"

# Step 2: sizes 属性の抽出
grep -oP 'sizes="[^"]*"' output.html | head -1
# → sizes="(max-width: 600px) 100vw, 50vw"

# Step 3: 各サイズの実在確認
for url in https://example.com/image-{400,800,1200}.jpg; do
  echo "Checking $url"
  curl -I "$url" | grep "HTTP/"
done
# → 全て 200 OK → 正常
```

### Example 3: Lazy Loading二重化の検出

```bash
# Step 1: Native Lazy Loading の使用箇所
grep -o 'loading="lazy"' output.html | wc -l
# → 15

# Step 2: プラグインLazy Loading の使用箇所
grep -oP 'data-lazy-src="[^"]*"' output.html | wc -l
# → 15

# → 両方が同じ画像に適用されている可能性
# Step 3: 同一要素で両方が使用されているか確認
grep 'loading="lazy"' output.html | grep 'data-lazy-src'
# → 該当あり → 二重化が発生 → プラグインを無効化
```

## Guidelines

1. **PHPコード読みだけでは不十分**: 実際のHTML出力を取得して検証すること。キャッシュ、プラグイン競合、サーバー設定等でPHPコードと出力が一致しない場合がある（cmd_142の教訓）
2. **キャッシュクリアを最優先**: 診断前に必ず全キャッシュ（WordPress, CDN, ブラウザ）をクリアする
3. **モバイルとデスクトップを両方確認**: レスポンシブ画像は画面幅で動作が変わる。必ず両方でテストする
4. **User-Agent を指定**: `curl` でHTML取得時は `-A "Mozilla/5.0"` を付けてブラウザとして振る舞う。サーバーがUser-Agentで配信を変えている場合がある
5. **WebPファイルの実在確認**: HTML出力に `srcset="...webp"` があっても、ファイルが実在しない場合がある。必ず `curl -I` で確認
6. **`.htaccess` のリライトルール順序**: WebPリライトルールは他のルールより前に配置する。順序が間違うと動作しない
7. **file_exists() のパス検証**: サーバー移行後は必ず `ABSPATH` / `WP_CONTENT_DIR` を確認。パスが変わるとWebP配信が止まる
8. **Lazy Loading の優先順位**: Native Lazy Loading（`loading="lazy"`）を優先し、プラグインは無効化する。二重化はパフォーマンス低下を招く
9. **PageSpeed Insights は参考程度**: 実際のHTML出力とNetwork Panelでの確認が最優先。PSIはキャッシュされたデータを見ている場合がある
10. **診断結果のドキュメント化**: チェックリストを埋めて結果を記録する。次回診断時の比較材料になる
