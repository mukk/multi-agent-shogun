# 関数型言語のプログラミング言語ランキング調査報告書

**task_id**: subtask_001_4
**調査日**: 2026-01-29
**調査担当**: 足軽4号

---

## 1. 調査概要

主要なプログラミング言語ランキングにおける関数型言語の順位変動を調査した。

### 調査対象インデックス
- TIOBE Index
- PYPL (PopularitY of Programming Language)
- RedMonk Programming Language Rankings
- GitHub Octoverse
- Stack Overflow Developer Survey

### 調査対象言語
Haskell, Scala, F#, Elixir, Clojure, OCaml, Erlang, Gleam

---

## 2. TIOBE Index

### 2024年後半〜2025年の関数型言語順位

| 言語 | 2024年順位 | 2025年順位 | シェア | 変動 |
|------|------------|------------|--------|------|
| Haskell | 34位 | 30-40位圏 | 0.47% | 横ばい |
| Elixir | 44位 | 40-50位圏 | 0.20% | 横ばい |
| F# | 26位→50位 | 51-100位 | 0.14% | 下降 |
| Clojure | 51-100位 | 51-100位 | - | 横ばい |
| OCaml | 51-100位 | 51-100位 | - | 横ばい |
| Scheme | 43位 | 51-100位 | - | 下降 |

### 主要トレンド（2025年）
- **C#が2025年のLanguage of the Year**を受賞（年間最大の成長率7.39%）
- トップ20の言語が市場の83.56%を占有（例年の75%より集中化）
- Go、Rubyがトップ10/20から脱落
- **関数型言語は軒並みトップ30外に位置**

**出典**: [TIOBE Index](https://www.tiobe.com/tiobe-index/), [TechRepublic](https://www.techrepublic.com/article/news-tiobe-index-language-rankings/)

---

## 3. PYPL Index

### 2025-2026年トップ10

| 順位 | 言語 | シェア |
|------|------|--------|
| 1 | Python | 29.48% |
| 2 | Java | 17.18% |
| 3 | JavaScript | 9.14% |
| 9 | Swift | - |
| 10 | Kotlin | - |

### 関数型言語の状況
- **純粋な関数型言語はトップ10に不在**
- Scala: サーバーサイドで約2.1%のウェブサイトで使用
- Rust: 関数型言語の中で最もトレンド上昇中（Google Trends）

**出典**: [PYPL Index](https://pypl.github.io/), [Ada Beat](https://adabeat.com/fp/most-popular-functional-programming-language-in-2025/)

---

## 4. RedMonk Programming Language Rankings（2025年1月）

### トップ20

| 順位 | 言語 |
|------|------|
| 1 | JavaScript |
| 2 | Python |
| 3 | Java |
| 14 (tie) | **Scala** |
| 14 (tie) | Kotlin |
| 19 | Rust |

### 関数型言語の位置
- **Scala**: 14位（タイ）- トップ20内で唯一の関数型言語
- **Haskell, Elixir, Clojure**: トップ20外に脱落
- 過去にはHaskellが15位、Clojureが19位の時期もあった

### 特筆事項
- トップ20内の変動が過去15年で最小
- Stack Overflowのクエリ量減少がランキング変動に影響
- AIの台頭がStack Overflowの価値に影響

**出典**: [RedMonk January 2025](https://redmonk.com/sogrady/2025/06/18/language-rankings-1-25/)

---

## 5. GitHub Octoverse 2025

### トップ言語
- **TypeScript**が2025年8月に初めてPython・JavaScriptを抜いて1位に
- 型付き言語へのシフト（AI支援コーディングとの相性）

### 関数型・関数型影響言語の成長率

| 言語 | YoY成長率 | 備考 |
|------|-----------|------|
| Rust | 50% | メモリ安全性で採用拡大 |
| Go | 25% | 安定成長 |
| Scala | - | 14位維持もGoに追い抜かれた |

### プラットフォーム全体
- 毎秒1人以上の新規開発者がGitHubに参加
- 年間3600万人以上の新規参加
- 総開発者数1億8000万人超

**出典**: [GitHub Octoverse 2025](https://octoverse.github.com/), [GitHub Blog](https://github.blog/news-insights/octoverse/octoverse-a-new-developer-joins-github-every-second-as-ai-leads-typescript-to-1/)

---

## 6. Stack Overflow Developer Survey 2025

### Most Admired Languages（最も愛されている言語）

| 順位 | 言語 | Admired率 | 種別 |
|------|------|-----------|------|
| 1 | **Rust** | 72% | システム/関数型影響 |
| 2 | **Gleam** | 70% | 関数型（新登場） |
| 3 | **Elixir** | 66% | 関数型 |
| 4 | Zig | 64% | システム |

### 関数型言語の評価
- **Elixir**: 66%で3位、安定した支持
- **Gleam**: 70%で2位、Erlang VM上の新興関数型言語として注目
- **Rust**: 厳密には関数型ではないが、関数型の影響を強く受けた設計

### 給与データ（関連）
- Go, Clojure, F#: 年収中央値$80,000以上
- Elixir, Rust: 年収$70,000以上

**出典**: [Stack Overflow Developer Survey 2025](https://survey.stackoverflow.co/2025/technology), [Elixir Forum](https://elixirforum.com/t/stack-overflow-developer-survey-2025/71073)

---

## 7. 総合分析

### 関数型言語の現状（2024-2025年）

#### 上昇傾向
| 言語 | 根拠 |
|------|------|
| **Rust** | 全指標で上昇、50% YoY成長、Most Admired 1位 |
| **Gleam** | Stack Overflow で70%のAdmired率、新興として注目 |
| **Elixir** | Most Admired 3位（66%）、安定した開発者満足度 |

#### 横ばい・下降傾向
| 言語 | 根拠 |
|------|------|
| **Scala** | RedMonk 14位維持も、Go/Rustに押され成長鈍化 |
| **Haskell** | TIOBE 34位、RedMonk Top20外、学術的影響は継続 |
| **F#** | TIOBE 26位→51-100位へ大幅下落 |
| **Clojure** | 各指標でトップ50外、ニッチな存在に |
| **OCaml** | 主要ランキングに登場せず |

### 市場全体の傾向

1. **言語の集中化**: トップ20言語が市場の83%を占有（例年75%）
2. **AI影響**: 型付き言語（TypeScript）の台頭、Stack Overflowの影響力低下
3. **関数型の影響拡散**: 純粋関数型よりも「関数型の影響を受けた言語」（Rust, TypeScript等）が成長
4. **実用主義の優勢**: Elixir、Rustなど実用性の高い言語が支持される一方、Haskell等の純粋関数型は学術領域に留まる傾向

---

## 8. 結論

### 主要な発見

1. **純粋関数型言語の主流化は進んでいない**
   - Haskell, F#, OCaml, Clojureは軒並みランキング下位または圏外

2. **関数型パラダイムは間接的に浸透**
   - Rust（所有権、パターンマッチング）
   - TypeScript（代数的データ型）
   - Kotlin/Swift（関数型機能の統合）

3. **注目すべき関数型言語**
   - **Elixir**: 開発者満足度が高く、実用的な関数型として定着
   - **Gleam**: 新興ながら高評価、今後の成長に注目
   - **Rust**: 関数型の影響を受けつつ実用性で急成長

4. **Scalaの停滞**
   - かつてはJVM関数型の旗手だったが、Kotlinの台頭とGoの成長で相対的地位低下

---

## 参考資料

- [TIOBE Index](https://www.tiobe.com/tiobe-index/)
- [PYPL Index](https://pypl.github.io/)
- [RedMonk Programming Language Rankings](https://redmonk.com/sogrady/2025/06/18/language-rankings-1-25/)
- [GitHub Octoverse 2025](https://octoverse.github.com/)
- [Stack Overflow Developer Survey 2025](https://survey.stackoverflow.co/2025/)
- [Ada Beat - Most Popular Functional Programming Language 2025](https://adabeat.com/fp/most-popular-functional-programming-language-in-2025/)
- [StatisticsTimes - Top Computer Languages 2025](https://www.statisticstimes.com/tech/top-computer-languages.php)
