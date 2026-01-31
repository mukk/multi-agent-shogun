# 主要関数型言語の動向調査 2025-2026

**調査日**: 2026-01-29
**調査者**: 足軽1号 (ashigaru1)
**タスクID**: subtask_001_1

---

## 概要

本報告書は、2025年から2026年初頭にかけての主要関数型言語6言語（Haskell, Scala, Elixir, Clojure, F#, OCaml）の動向をまとめたものである。

---

## 1. Haskell

### 最新バージョン
- **GHC 9.14** (LTS版): 2025年8月リリース - 初のLTSリリース
- **GHC 9.12.3**: 2025年12月リリース予定のバグフィックス版
- **HLS 2.13.0.0**: Haskell Language Serverの最新版

### 主要な新機能
- **デバッガ改善**: GHCiに`:stepout`コマンド追加、関数からのステップアウトが可能に
- **スタックトレース強化**: 任意データでコールスタックをアノテート可能な新primop
- **newtypeでの特殊化**: newtype存在下での特殊化が大幅に改善
- **SSE/AVX対応**: x86ネイティブコード生成バックエンドでのSIMD対応
- **Windowsツールチェーン**: 大幅アップデート
- **LinearTypes**: レコードフィールドを非線形で定義可能に
- **RequiredTypeArguments**: より多くのコンテキストで使用可能

### コミュニティ活発度
- **高い活発度**: Well-Typed社による定期的なエコシステムレポート公開（四半期ごと）
- **Discourse**: Haskell New Year Resolutions 2026など活発な議論
- **Google Summer of Code 2025**: シグネチャヘルプ機能などの新機能実装

### LTS戦略
- LTSリリースは最低2年間のサポート
- 連続するLTSリリース間に6ヶ月のサポート重複期間
- **GHC2024言語エディション**を推奨

### 参考リンク
- [GHC LTS Releases](https://blog.haskell.org/ghc-lts-releases/)
- [Haskell エコシステムレポート 2025年9-11月](https://well-typed.com/blog/2025/12/haskell-ecosystem-report-september-november-2025/)
- [HLS 2.13.0.0](https://blog.haskell.org/hls-2-13-0-0/)

---

## 2. Scala

### 最新バージョン
- **Scala 3.7.4**: 2025年9月9日リリース
- **Scala 3.3.7 LTS**: 2025年10月13日リリース

### ロードマップ
- **Scala 3.8**: 2025年Q4予定、Java 17が必須要件に
- **Scala 3.9**: 2026年Q2予定、新LTSディストリビューション

### 主要な新機能（Scala 3.8）
- **`into`修飾子**: 新構文
- **Better fors**: for式の改善
- **Capture checking**: 基本サポート
- **`runtimeChecked`メソッド**: 新機能
- **SIP-64コンテキスト境界**: IntelliJでサポート
- **名前付きタプル**: パターンマッチング対応
- **Opaque types**: 適切な解決

### コミュニティ活発度
- **Scala Days 2025**: EPFLで開催、約300人の開発者とビジネスリーダーが参加
- **テーマ**: 「Functional Programming And The Real World」
- **新規参加者多数**: 新世代のScalaプラクティショナーの参入

### 企業採用状況
- JetBrains調査で**59%がScala 3を定常的に使用**（446人中274人）
- 学術的な言語からエンタープライズ対応技術への進化
- **特化した優秀性**: 型安全性、パフォーマンス、関数型パラダイムが重要なユースケースで強み

### 参考リンク
- [Scala Highlights June 2025](https://www.scala-lang.org/highlights/2025/06/26/highlights-june-2025.html)
- [IntelliJ Scala Plugin 2025.3](https://blog.jetbrains.com/scala/2025/12/08/scala-plugin-2025-3-is-out/)
- [Next Scala 3 LTS](https://www.scala-lang.org/blog/next-scala-lts.html)

---

## 3. Elixir

### 最新バージョン
- **Elixir 1.18**: 2025年時点の最新
- **Elixir 1.20**: RC版リリース（型推論機能搭載）
- **Phoenix 1.8.2**: 現在の安定版
- **LiveView 1.1**: 最新版

### 主要な新機能

#### Elixir 1.20
- **型推論**: 全構文に対応

#### Phoenix 1.8
- **AGENTS.md**: LLM支援開発向けの新ファイル
- **ダークモード**: 待望の機能追加
- **phx.gen.auth**: マジックリンクサポート標準搭載
- **Scopes**: セキュアなデータアクセス機能
- **必須要件**: Erlang/OTP 25+

#### LiveView 1.1
- **Colocated Hooks**: 新機能
- **Keyed comprehensions**: 新機能

### コミュニティ活発度
- **Elixir 15周年**: 記念リリース
- **ElixirConf EU 2026**: Early Birdチケット販売中
- **活発なエコシステム**: 多数のライブラリ更新

### 注目プロジェクト
- **Sprites.dev**: Chris McCordによるFly.ioでのハードウェア分離実行環境
- **LiveVue v1.0**: Vue.jsとPhoenix LiveViewの統合
- **LiveDebugger v0.5.0**: Dead LiveViewsデバッグ機能
- **Gust**: Airflow代替のワークフローエンジン
- **MDEx v0.11.0**: Phoenix Components対応

### 参考リンク
- [Phoenix 1.8.0 Released](https://www.phoenixframework.org/blog/phoenix-1-8-released)
- [Elixir Status](https://elixirstatus.com/)
- [Phoenix Framework](https://phoenixframework.org/)

---

## 4. Clojure

### 最新バージョン
- **Clojure 1.12.3**: 最新安定版
- **ClojureScript 1.12.42**: 2025年5月リリース

### 主要な新機能
- **core.async.flow**: 新しい非同期フロー機能
- **ClojureScript**: Google Closure Compiler v20250402対応（Java 21必須）
- **Google Closure Library**: コミュニティがフォークしてメンテナンス継続

### コミュニティ活発度
- **Clojure Jam 2026**: 4月18-19日 & 25-26日（オンライン、無料）
- **Babashka Conf**: 5月8日、アムステルダム
- **Dutch Clojure Days 2026**: 5月9日、アムステルダム

### 企業採用状況
- **Nubank**: 1億人以上の顧客を持つ世界最大級のデジタルバンキングプラットフォーム
  - ブラジル、メキシコ、コロンビアでサービス展開
  - 2020年にCognitect（Clojure/Datomicの開発元）を買収
  - Clojure/ClojureScriptの主要企業スポンサー
- **その他採用企業**: Apple, Atlassian, Funding Circle, Netflix, Puppet, Walmart
- **政府機関**: NASA

### ツール・ライブラリ
- **clj-kondo 2026.01.19**: 静的解析・リンター
- **aleph 0.9.5**: 非同期ストリーミング通信

### 給与水準
- トップティア言語として**$95,000+**の給与水準

### 参考リンク
- [Clojure News](https://clojure.org/news/news)
- [ClojureScript 1.12.42 Release](https://clojurescript.org/news/2025-05-16-release)
- [Clojure Dev Changelog](https://clojure.org/releases/devchangelog)

---

## 5. F#

### 最新バージョン
- **F# 10**: .NET 10およびVisual Studio 2026と同時リリース

### 主要な新機能

#### F# 10
- **スコープ付き警告抑制**: `#warnon`ディレクティブ追加
  - 既存の`#nowarn`と組み合わせて警告の精密制御が可能
- **計算式の構文一貫性**: 改善
- **自動プロパティアクセサ**: サポート強化
- **型包摂キャッシュ**: コンパイルとインタラクティブツールの高速化

#### 2026年ロードマップ
- 大規模プログラムでのスレッド管理改善
- F#ツールのスタンドアロン実行ファイル化

### コミュニティ活発度
- **Microsoftの継続的サポート**: F#チームによる活発な開発
- **Visual Studio Developer Community**: フォーラムでの活発な議論

### 特徴
- 関数型、命令型、オブジェクト指向のマルチパラダイム言語
- オープンソース、クロスプラットフォーム
- 「Pythonのようなシンプルさ、C#/Javaを超える正確性とパフォーマンス」

### 参考リンク
- [What's new in F# 10](https://learn.microsoft.com/en-us/dotnet/fsharp/whats-new/fsharp-10)
- [Introducing F# 10](https://devblogs.microsoft.com/dotnet/introducing-fsharp-10/)
- [F# 10 Performance Improvements](https://www.infoq.com/news/2025/11/fsharp-10-performance/)

---

## 6. OCaml

### 最新バージョン
- **OCaml 5.3**: 2025年初頭リリース

### 主要な新機能

#### OCaml 5（マルチコア対応）
- **共有メモリ並列処理**: マルチコアアーキテクチャ対応
- **エフェクトハンドラ**: 並行処理用（産業用言語で初搭載）
- **2022年リリース**だが、2025年に企業での本格採用が進む

#### OCaml 5.3
- **MSVC ポート復活**: Windows上でのMSVCサポート再開
- **統計メモリプロファイリング復活**: マルチコア対応版
- **GC改善**: `Gc.ramp_up`でメモリ消費のランプアップフェーズを明示可能
- **ランタイムロック改善**: マルチコアランタイムでのデッドロック修正

### 企業採用状況

#### Jane Street
- ツール・コンパイラチーム拡大
- マルチコアランタイムへの本番移行完了

#### Docker
- Docker for Desktopで数億ユーザー向けにEioによるdirect-styleコードへ移行中

### OxCaml（Jane Streetの実験的ブランチ）
- **目標**: 正しさを保証したマルチコアコード、低レイテンシコード、全体的なパフォーマンス向上
- **Taridesがアップストリーム支援**: Multicore OCamlのアップストリーム経験を活かす

### 参考リンク
- [OCaml 5.3 Features and Fixes](https://tarides.com/blog/2025-01-09-ocaml-5-3-features-and-fixes/)
- [Jane Street and Docker on OCaml 5](https://anil.recoil.org/notes/icfp25-ocaml5-js-docker)
- [OxCaml Branch](https://tarides.com/blog/2025-07-09-introducing-jane-street-s-oxcaml-branch/)

---

## 総括

| 言語 | 最新版 | 主要動向 | 企業採用 |
|------|--------|----------|----------|
| **Haskell** | GHC 9.14 LTS | デバッガ改善、LTS戦略確立 | コンサルティング中心 |
| **Scala** | 3.7.4 / 3.3.7 LTS | Scala 3移行加速、Java 17必須化 | 59%がScala 3使用 |
| **Elixir** | 1.18 (1.20 RC) | 型推論、Phoenix 1.8 | Web/リアルタイム領域 |
| **Clojure** | 1.12.3 | core.async.flow | Nubank, Netflix等 |
| **F#** | F# 10 | 警告制御、パフォーマンス改善 | .NETエコシステム |
| **OCaml** | 5.3 | マルチコア本格採用 | Jane Street, Docker |

### 全体的傾向

1. **マルチコア/並列処理**: OCaml 5、Haskellでの継続的改善
2. **型システム強化**: Elixirの型推論、Scalaのcapture checking
3. **LTS戦略**: Haskell、Scalaで長期サポート版の重要性増大
4. **ツール・DX改善**: 各言語でIDEサポート、デバッガ、LSP強化
5. **企業採用拡大**: 特にOCaml（Jane Street, Docker）、Clojure（Nubank）で顕著

---

*報告終了*
