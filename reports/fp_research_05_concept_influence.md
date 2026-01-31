# 関数型プログラミング概念の主流言語への影響調査

> **調査担当**: 足軽5号（シニアソフトウェアエンジニア）
> **調査日**: 2026-01-29
> **task_id**: subtask_001_5

## 概要

関数型プログラミング（FP）の概念は、もはやHaskellやLispなどの純粋関数型言語に限定されない。Java、Python、JavaScript、TypeScript、Kotlin、Swiftなどの主流言語がFPの機能を積極的に取り込んでいる。2023年の調査では、**52%の開発者がマルチパラダイム言語でFPテクニックを使用**していると報告されている。

---

## 1. イミュータビリティ（不変性）

### 概念
データが一度作成されると変更できないという原則。スレッドセーフティとプログラムの予測可能性を向上させる。

### 各言語への導入

| 言語 | 機能 | 導入時期 |
|------|------|----------|
| **Java** | `record` クラス（浅い不変性） | Java 14 (2020年3月) |
| **Java** | `final` 変数、`Collections.unmodifiableList()` | 初期から |
| **Python** | `frozen=True` の `@dataclass` | Python 3.7 (2018年) |
| **Python** | 不変組み込み型（tuple, frozenset, str） | 初期から |
| **JavaScript** | `Object.freeze()` | ES5 (2009年) |
| **JavaScript** | `const` 宣言 | ES6 (2015年) |
| **C#** | `readonly` フィールド、`record` 型 | C# 9 (2020年) |

### コード例

```java
// Java 14+ record（不変データホルダー）
public record Point(int x, int y) {}
```

```python
# Python 3.7+ frozen dataclass
from dataclasses import dataclass

@dataclass(frozen=True)
class Point:
    x: int
    y: int
```

```javascript
// JavaScript Object.freeze()
const point = Object.freeze({ x: 1, y: 2 });
```

---

## 2. 純粋関数と副作用の分離

### 概念
純粋関数は入力値のみに依存し、同じ入力に対して常に同じ結果を返し、外部状態を変更しない。テスト、デバッグ、並列化が容易になる。

### 各言語への影響

| 言語 | 状況 |
|------|------|
| **Java** | Stream APIが純粋関数的な操作を推奨。ただし強制メカニズムなし |
| **Python** | `functools` モジュール提供。言語レベルでの強制なし |
| **JavaScript** | Ramda、Lodashなどのライブラリが純粋関数を促進 |
| **Rust** | 所有権システムにより副作用を型レベルで追跡 |

### 影響の実態
- 主流言語は純粋関数を**サポート**するが**強制**はしない
- FPの哲学が広まり、副作用を意識した設計パターンが増加
- 関数の「純粋性」はコードレビューやリンターで担保される傾向

---

## 3. 型システム（代数的データ型、型推論）

### 代数的データ型（ADT）

代数的データ型は、既存の型を組み合わせて新しい型を作る仕組み。主に**直積型**（Product Type）と**直和型**（Sum Type）がある。

| 言語 | 機能 | 導入時期 |
|------|------|----------|
| **Java** | `sealed` インターフェース/クラス | Java 17 (2021年) |
| **Python** | `@typing.sealed` デコレータ（静的チェック用） | 提案段階 |
| **TypeScript** | Union型、Discriminated Union | 初期から |
| **Rust** | `enum` による直和型 | 初期から |

```java
// Java 17 sealed interface（直和型）
public sealed interface Shape permits Circle, Rectangle {}
public record Circle(double radius) implements Shape {}
public record Rectangle(double width, double height) implements Shape {}
```

### 型推論

| 言語 | 機能 | 導入時期 |
|------|------|----------|
| **Java** | `var` キーワード（ローカル変数型推論） | Java 10 (2018年) |
| **Python** | 型ヒント + 型チェッカー（mypy等） | PEP 484 (2014年) |
| **TypeScript** | 強力な型推論 | 初期から |
| **Kotlin** | 完全な型推論 | 初期から |
| **Swift** | 型推論 | 初期から |

**2025年Python Typing Survey（Meta）**: TypeScriptに触発された機能（Intersection型、Mapped型、Utility型）への要望が多数報告されている。

---

## 4. パターンマッチング

### 概念
値の構造に基づいて分岐し、同時にデータを抽出する機能。従来のswitch文より表現力が高い。

### 各言語への導入

| 言語 | 機能 | 導入時期 |
|------|------|----------|
| **Java** | `switch` 式でのパターンマッチング | Java 17+ (段階的導入) |
| **Python** | `match` 文（構造的パターンマッチング） | Python 3.10 (2021年) - PEP 622 |
| **JavaScript** | TC39提案段階 | 検討中 |
| **C#** | パターンマッチング | C# 7.0+ (2017年〜) |

### コード例

```java
// Java 21 パターンマッチング
String describe(Object obj) {
    return switch (obj) {
        case Integer i -> "Integer: " + i;
        case String s -> "String: " + s;
        case null -> "null";
        default -> "Unknown";
    };
}
```

```python
# Python 3.10 match文
def describe(obj):
    match obj:
        case int(i):
            return f"Integer: {i}"
        case str(s):
            return f"String: {s}"
        case _:
            return "Unknown"
```

---

## 5. 高階関数とラムダ式

### 概念
- **高階関数**: 関数を引数に取る、または関数を返す関数
- **ラムダ式**: 名前を持たない無名関数

### 各言語への導入

| 言語 | 機能 | 導入時期 |
|------|------|----------|
| **Java** | ラムダ式、Stream API | Java 8 (2014年) |
| **Python** | `lambda`、`map`、`filter`、`reduce` | Python 2.2 (1994年から部分的) |
| **JavaScript** | アロー関数 | ES6 (2015年) |
| **C++** | ラムダ式 | C++11 (2011年) |
| **C#** | ラムダ式 | C# 3.0 (2007年) |

### コード例

```java
// Java 8 Stream API
List<String> names = people.stream()
    .filter(p -> p.getAge() > 18)
    .map(Person::getName)
    .collect(Collectors.toList());
```

```javascript
// JavaScript ES6 アロー関数
const adults = people
    .filter(p => p.age > 18)
    .map(p => p.name);
```

### 注目点
- JavaScriptのアロー関数は`this`を語彙的にバインド（従来のfunction問題を解決）
- Pythonのlambdaは単一式に制限される
- 無名関数の概念はLisp（1958年）に遡る

---

## 6. モナドとエフェクトシステム

### 概念
モナドは計算をシーケンスとして構造化する方法。各ステップは値だけでなく、失敗の可能性、非決定性、副作用などの追加情報も生成する。

### 主流言語における「モナド的」構造

| 言語 | 構造 | 用途 |
|------|------|------|
| **JavaScript** | `Promise` | 非同期計算 |
| **JavaScript** | `async/await` | Promiseのdo記法的糖衣構文 |
| **Python** | `asyncio.Future` | 非同期計算 |
| **Rust** | `Result<T, E>` | エラーハンドリング |
| **Rust** | `Option<T>` | null安全性 |
| **Java** | `Optional<T>` | null安全性 |
| **Java** | `CompletableFuture` | 非同期計算 |
| **C#** | `Task<T>` | 非同期計算 |

### Promise ≈ モナド

```javascript
// JavaScript Promise（モナド的チェーン）
fetch('http://api.example.com')
    .then(response => response.json())
    .then(data => process(data))
    .then(result => display(result));

// async/await（do記法に相当）
async function main() {
    const response = await fetch('http://api.example.com');
    const data = await response.json();
    const result = await process(data);
    display(result);
}
```

### ReactiveX（Rx）ライブラリ
モナド的合成でI/O処理をチェーンするライブラリが各言語で普及：
- RxJava（Java）
- RxJS（JavaScript）
- RxPY（Python）

---

## 総括：FP概念の主流言語への浸透度

| 概念 | Java | Python | JavaScript | 浸透度 |
|------|------|--------|------------|--------|
| イミュータビリティ | ◎ record | ◎ frozen dataclass | ○ Object.freeze | 高 |
| 純粋関数 | △ 慣習のみ | △ 慣習のみ | △ 慣習のみ | 中 |
| 代数的データ型 | ◎ sealed | △ 提案段階 | ○ TypeScript | 中〜高 |
| 型推論 | ○ var | ○ type hints | ◎ TypeScript | 高 |
| パターンマッチング | ◎ switch式 | ◎ match文 | △ 提案中 | 中〜高 |
| 高階関数/ラムダ | ◎ Stream API | ◎ 初期から | ◎ アロー関数 | 非常に高 |
| モナド的構造 | ◎ Optional/CF | ◎ asyncio | ◎ Promise | 高 |

**凡例**: ◎ 十分なサポート / ○ 部分的サポート / △ 限定的・提案段階

---

## 今後の展望

1. **ハイブリッドアプローチの定着**: 純粋FP言語への完全移行ではなく、既存言語にFP概念を段階的に導入するアプローチが主流
2. **並行処理での優位性**: イミュータビリティと純粋関数の概念は、マルチスレッド/分散システムで特に価値が高い
3. **型システムの強化**: TypeScriptの成功を受け、他言語も型推論と型安全性を強化する傾向
4. **エフェクトシステムの進化**: Rustの所有権システムやKotlinのコルーチンなど、副作用を型レベルで追跡する仕組みが進化中

---

## 参考資料

- [Functional Programming - Wikipedia](https://en.wikipedia.org/wiki/Functional_programming)
- [PEP 622 – Structural Pattern Matching](https://peps.python.org/pep-0622/)
- [Algebraic Data Types and Pattern Matching with Java](https://blog.scottlogic.com/2025/01/20/algebraic-data-types-with-java.html)
- [Python Typing Survey 2025 - Meta Engineering](https://engineering.fb.com/2025/12/22/developer-tools/python-typing-survey-2025-code-quality-flexibility-typing-adoption/)
- [Option/Maybe, Either, and Future Monads in JavaScript, Python, Ruby, Swift, and Scala](https://www.toptal.com/javascript/option-maybe-either-future-monads-js)
- [Monad (functional programming) - Wikipedia](https://en.wikipedia.org/wiki/Monad_(functional_programming))
- [Lambda Expressions: A Guide](https://joshdata.me/lambda-expressions.html)
- [Higher-order function - Wikipedia](https://en.wikipedia.org/wiki/Higher-order_function)
