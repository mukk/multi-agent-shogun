# マルチパラダイム言語における関数型機能の採用状況調査

> **調査日**: 2026-01-29
> **調査者**: 足軽2号（シニアソフトウェアエンジニア）
> **Task ID**: subtask_001_2

## 概要

本調査では、主要なマルチパラダイム言語（TypeScript, Python, Kotlin, Java, C#, Swift, Rust）における関数型プログラミング機能の採用状況を調査した。近年、これらの言語では関数型機能の導入が加速しており、特にパターンマッチング、イミュータブルデータ構造、高階関数の採用が顕著である。

---

## 言語別調査結果

### 1. TypeScript

#### 追加された関数型機能

| 機能 | 説明 |
|------|------|
| `readonly` 修飾子 | プロパティの不変性を保証 |
| `ReadonlyArray<T>` | 配列の変更メソッドを除去 |
| `const` アサーション | リテラル型として推論 |
| Discriminated Unions | 代数的データ型の実現 |

#### 主要ライブラリ・フレームワーク

- **[ts-pattern](https://github.com/gvergnaud/ts-pattern)**: 型安全な網羅的パターンマッチングライブラリ。TC39のPattern Matching提案（Stage 1）を先取り実装
- **[fp-ts](https://github.com/gcanti/fp-ts)**: HaskellスタイルのFPライブラリ。Option, Either, Task等のモナドを提供
- **[Immer](https://immerjs.github.io/immer/)**: 不変データを「ドラフト」経由で直感的に操作。メモリ共有による効率化

#### 2025-2026の動向

- TypeScript 5.4でネイティブリアクティブ型の導入予定
- Effect Systemsパターンが分散システムで注目
- クラウドネイティブアーキテクチャとの統合深化

#### コミュニティ普及度

ts-patternやImmerの登場により、不変データ構造の採用が業界で大幅に促進。関数型スタイルは特にReact/Reduxエコシステムで標準的に。

---

### 2. Python

#### 追加された関数型機能

| 機能 | バージョン | 説明 |
|------|-----------|------|
| Structural Pattern Matching | 3.10 (PEP 634-636) | `match`/`case`文による構造的パターンマッチング |
| `@runtime_checkable` Protocol | 3.8+ | 構造的サブタイピングの実行時チェック |
| TypeAlias | 3.10+ | 型エイリアスの明示的宣言 |
| `dataclasses` | 3.7+ | 不変データクラス（`frozen=True`） |

#### パターンマッチングの特徴

```python
match command:
    case {"action": "quit"}:
        quit()
    case {"action": "move", "direction": direction}:
        move(direction)
    case _:
        unknown_command()
```

- [PEP 622](https://peps.python.org/pep-0622/): Haskell, Scala, Rust等の関数型言語からインスピレーション
- 動的言語でのパターンマッチング実装は独自の課題（Guido van Rossumらの論文で解説）

#### 2025の議論

- 2025年4月: 型パラメータの構造的パターンマッチングに関する議論
- 2025年7月: `typing.NewType`のメタクラス化提案（isinstance/パターンマッチング対応）

#### コミュニティ普及度

データサイエンス分野でイミュータブルなDataFrame操作（pandas, polars）が普及。Protocolによる構造的型付けの採用増加。

---

### 3. Kotlin

#### 追加された関数型機能

| 機能 | 説明 |
|------|------|
| 高階関数 | 関数を引数・戻り値として扱う |
| ラムダ式 | 簡潔な無名関数記法 |
| `data class` | 不変データモデル（copy()メソッド付き） |
| Scope Functions | `let`, `run`, `with`, `apply`, `also` |
| Sealed Classes | 代数的データ型の実現 |
| Context Receivers | 実験的機能（2.0.0） |

#### [Arrow-kt](https://arrow-kt.io/) エコシステム

- **Stars**: 6,501（2026年1月時点）
- **最終更新**: 2026年1月3日
- **主な機能**:
  - Either, Option, Validated などの関数型データ型
  - Raise DSL によるエラーハンドリング
  - コルーチンとの統合
  - Type Proofs（コンパイラプラグイン）: 型クラス、Union型、型精錬

#### コミュニティ普及度

Android開発でKotlinのFP機能が標準化。Arrow-ktは大規模プロジェクトでの採用増加。DSLビルダーパターンが広く活用。

---

### 4. Java

#### 追加された関数型機能

| 機能 | バージョン | 説明 |
|------|-----------|------|
| Records | 16 (正式) | 不変データキャリア |
| Sealed Classes | 17 (正式) | 継承の制限による代数的データ型 |
| Pattern Matching for instanceof | 16 (正式) | 型テストと変数バインドの統合 |
| Pattern Matching for switch | 21 (正式) | switch文でのパターンマッチング |
| Record Patterns | 21 (正式) | レコードの分解パターン |

#### 採用統計

- BellSoft 2024調査: **Recordsの採用率55%**（最も人気の現代的機能）

#### [JEP 440](https://openjdk.org/jeps/440): Record Patterns

```java
record Point(int x, int y) {}

static void printSum(Object obj) {
    if (obj instanceof Point(int x, int y)) {
        System.out.println(x + y);
    }
}
```

#### 2026年以降の展望

- **Value Classes** (JEP 401): JDK 26-27で安定化予定
- **Custom Deconstructors**: 任意クラスへのパターンマッチング拡張
- **Withers**: `with`式によるイミュータブルな変更

#### コミュニティ普及度

Java 21のLTS化により、Records + Sealed Classes + Pattern Matchingの組み合わせが急速に普及。データ指向プログラミングへのパラダイムシフトが進行。

---

### 5. C#

#### 追加された関数型機能

| 機能 | バージョン | 説明 |
|------|-----------|------|
| Records | 9.0 | 不変データ型（値ベース等価性） |
| `record struct` | 10.0 | 値型レコード |
| Pattern Matching | 7.0+ | 継続的な拡張 |
| Property Patterns | 8.0 | プロパティによるマッチング |
| `init` アクセサ | 9.0 | 初期化専用セッター |
| `ImmutableList<T>` | .NET Core | 不変コレクション |

#### パターンマッチングの進化

```csharp
var result = shape switch
{
    Circle { Radius: > 0 } c => $"Circle with radius {c.Radius}",
    Rectangle { Width: var w, Height: var h } => $"Rectangle {w}x{h}",
    _ => "Unknown shape"
};
```

#### F#との融合

- C#はF#の機能を継続的に取り込み
- 「[C# will become F#](https://gautiertalksmicrosoft.wordpress.com/2025/04/13/c-will-become-f/)」という見解も

#### コミュニティ普及度

.NETランタイムがFPパターン（不変性、パターンマッチング）を最適化。LINQ + Records + Pattern Matchingの組み合わせが宣言的コードの標準に。

---

### 6. Swift

#### 追加された関数型機能

| 機能 | 説明 |
|------|------|
| `let` キーワード | 不変変数宣言 |
| Optional型 | モナドとしてのnull安全 |
| Pattern Matching | switch文での強力なマッチング |
| `~=` 演算子 | カスタムパターンマッチング |
| Result型 | 明示的エラーハンドリング |
| Closures | 値キャプチャ付き無名関数 |

#### パターンマッチングの種類

1. **Switch文パターン**: 値に対する複数ケースマッチング
2. **Optionalパターン**: `if let`, `guard let`
3. **Tupleパターン**: `case (_, 0, 0)`
4. **Enum Case パターン**: 関連値の分解
5. **カスタム演算子**: `~=`オーバーロード

#### 関数型の特徴

- Haskell, Erlangからの影響
- Optionalはモナドパターンの実装
- 遅延評価（`lazy`キーワード）

#### コミュニティ普及度

SwiftUIの宣言的UIにより関数型スタイルがiOS開発で主流化。Combineフレームワークでリアクティブ関数型が標準に。

---

### 7. Rust

#### 関数型機能（言語設計に組み込み）

| 機能 | 説明 |
|------|------|
| Pattern Matching | `match`式による網羅的マッチング |
| Option/Result | モナド的エラーハンドリング |
| Iterators | 遅延評価・関数合成 |
| Closures | 環境キャプチャ付き無名関数 |
| 不変性デフォルト | `let`はデフォルト不変 |
| 式ベース | ほぼ全てが式として値を返す |

#### イテレータの豊富なアダプタ

```rust
let sum: i32 = vec![1, 2, 3, 4, 5]
    .iter()
    .filter(|&x| x % 2 == 0)
    .map(|x| x * 2)
    .sum();
```

- `map`, `filter`, `fold`, `zip`, `skip`, `take`, `flat_map`等
- **ゼロコスト抽象化**: コンパイル時に最適化

#### Option/Resultチェーン

```rust
fn process(input: &str) -> Result<i32, Error> {
    input
        .parse::<i32>()
        .map(|n| n * 2)
        .and_then(|n| validate(n))
}
```

#### コミュニティ普及度

「関数型Rustは適切でイディオマティックなRust」という認識。システムプログラミングでも関数型スタイルが標準。Haskellプログラマからの移行も活発。

---

## 総括比較表

| 言語 | パターンマッチング | 不変データ型 | 関数型ライブラリ | 普及度 |
|------|-------------------|-------------|-----------------|--------|
| TypeScript | ライブラリ (ts-pattern) | readonly, const | fp-ts, Immer | ★★★★☆ |
| Python | ネイティブ (3.10+) | dataclass (frozen) | functools | ★★★☆☆ |
| Kotlin | when式, sealed | data class | Arrow-kt | ★★★★☆ |
| Java | ネイティブ (21+) | Records | Vavr | ★★★★☆ |
| C# | ネイティブ (7.0+) | Records | LanguageExt | ★★★★☆ |
| Swift | ネイティブ | let, struct | Bow | ★★★☆☆ |
| Rust | ネイティブ | デフォルト不変 | 標準ライブラリ | ★★★★★ |

---

## 結論

1. **パターンマッチングの普及**: Java 21, Python 3.10の正式サポートにより、主要言語全てでパターンマッチングが利用可能に

2. **Recordsの標準化**: Java, C#, Kotlinで不変データ型が言語レベルでサポートされ、ボイラープレート削減と不変性保証を両立

3. **代数的データ型の実現**: Sealed Classes（Java, Kotlin）により、関数型言語のADTが主流言語で実現

4. **エコシステムの成熟**: Arrow-kt, fp-ts等の関数型ライブラリが安定し、実プロダクションでの採用増加

5. **Rustの先進性**: 言語設計レベルで関数型を取り込み、他言語のベンチマークに

---

## 参考資料

### TypeScript
- [TypeScript for Functional Programmers](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes-func.html)
- [ts-pattern - GitHub](https://github.com/gvergnaud/ts-pattern)
- [fp-ts - GitHub](https://github.com/gcanti/fp-ts)

### Python
- [PEP 622 - Structural Pattern Matching](https://peps.python.org/pep-0622/)
- [PEP 636 - Tutorial](https://peps.python.org/pep-0636/)
- [Real Python - Structural Pattern Matching](https://realpython.com/structural-pattern-matching/)

### Kotlin
- [Arrow-kt](https://arrow-kt.io/)
- [Functional Kotlin - kt.academy](https://kt.academy/book/functional_kotlin)

### Java
- [JEP 440: Record Patterns](https://openjdk.org/jeps/440)
- [Modern Java Features](https://www.javacodegeeks.com/2025/12/modern-java-language-features-records-sealed-classes-pattern-matching.html)
- [The Future of Java 2026](https://www.javacodegeeks.com/2026/01/the-future-of-java-what-to-expect-in-2026-and-beyond.html)

### C#
- [C# Record Types 2025](https://amarozka.dev/csharp-record-types-immutable-data/)
- [Immutable Data Structures in C#](https://softwarepatternslexicon.com/patterns-c-sharp/10/2/)

### Swift
- [Pattern Matching in Swift](https://www.swiftbysundell.com/articles/pattern-matching-in-swift/)
- [Functional Programming in Swift](https://matteomanferdini.com/swift-functional-programming/)

### Rust
- [Functional Features in Rust](https://doc.rust-lang.org/book/ch13-00-functional-features.html)
- [Rust's Approach to FP](https://softwarepatternslexicon.com/rust/functional-programming-patterns-in-rust/rust-s-approach-to-functional-programming/)
