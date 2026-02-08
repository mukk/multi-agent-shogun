---
name: web-audio-sfx-generator
description: Web Audio APIで効果音を合成生成する。外部音声ファイル不要で、クリック音・ビープ音・チャイム音・スウィープ音・ノイズバースト等を動的に生成する。ゲーム、UIフィードバック、通知音の実装時に使用。SoundManagerシングルトン + フレームワーク統合（React/Vue/Svelte）+ ON/OFF切替・音量調節・永続化を含む。
---

# Web Audio SFX Generator

## Overview

Web Audio API（AudioContext, OscillatorNode, GainNode, BiquadFilterNode, AudioBufferSourceNode）を使い、外部音声ファイルなしで各種効果音を合成生成するパターン。以下を一貫して実施する：

- **SoundManager クラス**: AudioContext のライフサイクル管理、サウンド種別ごとの再生メソッド
- **音の合成パターン**: ノイズバースト（クリック音）、オシレーター（ビープ/チャイム）、スウィープ（上昇/下降音）
- **状態管理**: ON/OFF切替、音量調節、localStorage 永続化
- **フレームワーク統合**: React hook / Vue composable / Svelte store として公開
- **UIコンポーネント**: トグルボタン + 音量スライダー

## When to Use

- ゲームアプリに効果音を追加する（駒移動、衝突、イベント通知）
- Webアプリにインタラクションフィードバック音を追加する（クリック、ホバー、エラー、成功）
- 外部音声ファイルをホスティングせずに軽量に効果音を実装したい
- プロトタイプ段階でサウンドを即座に追加したい
- 既存プロジェクトにサウンドON/OFF切替を追加する

## Instructions

### Step 1: SoundManager シングルトンの作成

AudioContext を遅延初期化するシングルトンクラスを作成する。

```typescript
class SoundManager {
  private ctx: AudioContext | null = null
  private _enabled: boolean
  private _volume: number

  constructor(storagePrefix: string = 'app') {
    this._enabled = localStorage.getItem(`${storagePrefix}-sound`) !== 'false'
    this._volume = parseFloat(localStorage.getItem(`${storagePrefix}-volume`) ?? '0.5')
  }

  get enabled() { return this._enabled }
  set enabled(v: boolean) {
    this._enabled = v
    localStorage.setItem(`${this.prefix}-sound`, String(v))
  }

  get volume() { return this._volume }
  set volume(v: number) {
    this._volume = Math.max(0, Math.min(1, v))
    localStorage.setItem(`${this.prefix}-volume`, String(this._volume))
  }

  private getContext(): AudioContext {
    if (!this.ctx) this.ctx = new AudioContext()
    if (this.ctx.state === 'suspended') this.ctx.resume()
    return this.ctx
  }

  play(type: string): void {
    if (!this._enabled) return
    try {
      // dispatch to the appropriate sound method
    } catch {
      // Audio failure is non-fatal — never let sound crash the app
    }
  }
}

export const soundManager = new SoundManager('my-app')
```

**重要ポイント:**
- `AudioContext` はユーザーインタラクション前は `suspended` になるため、`resume()` を毎回呼ぶ
- `try/catch` で囲み、音声再生失敗がアプリ全体に影響しないようにする
- `storagePrefix` でアプリ固有のキーを使い、他アプリとの衝突を防ぐ

### Step 2: 音の合成パターン

以下の基本パターンを組み合わせて効果音を作成する。

#### Pattern A: ノイズバースト（クリック音・打撃音）

ホワイトノイズ + BiquadFilter + 指数減衰エンベロープ。木の打撃音、UIクリック等に使用。

```typescript
playClick(frequency = 2800, q = 1.5, duration = 0.06): void {
  const ctx = this.getContext()
  const now = ctx.currentTime

  // ホワイトノイズ生成（指数減衰付き）
  const bufferSize = Math.ceil(ctx.sampleRate * duration)
  const buffer = ctx.createBuffer(1, bufferSize, ctx.sampleRate)
  const data = buffer.getChannelData(0)
  for (let i = 0; i < bufferSize; i++) {
    data[i] = (Math.random() * 2 - 1) * Math.exp(-i / (bufferSize * 0.15))
  }

  const source = ctx.createBufferSource()
  source.buffer = buffer

  // バンドパスフィルタで周波数帯域を絞る
  const filter = ctx.createBiquadFilter()
  filter.type = 'bandpass'
  filter.frequency.value = frequency  // 高い値→明るいクリック、低い値→重い打撃
  filter.Q.value = q                  // 高い値→共鳴強い、低い値→広帯域

  // 音量エンベロープ
  const gain = ctx.createGain()
  gain.gain.setValueAtTime(this._volume * 0.8, now)
  gain.gain.exponentialRampToValueAtTime(0.001, now + duration)

  source.connect(filter)
  filter.connect(gain)
  gain.connect(ctx.destination)
  source.start(now)
  source.stop(now + duration)
}
```

**パラメータガイド:**
| 用途 | frequency | Q | duration |
|------|-----------|---|----------|
| 軽いUIクリック | 3000-4000 | 1.0-2.0 | 0.03-0.05 |
| 木の打撃音 | 2500-3000 | 1.2-1.8 | 0.05-0.08 |
| 重い衝突音 | 1500-2200 | 0.8-1.5 | 0.08-0.12 |
| メタリッククリック | 4000-6000 | 2.0-4.0 | 0.02-0.04 |

#### Pattern B: 重い打撃音（ノイズ + 低音オシレーター）

クリック音にサイン波の低周波を重ねて重厚感を出す。

```typescript
playHeavyHit(): void {
  const ctx = this.getContext()
  const now = ctx.currentTime

  // 高域クリック（Pattern A と同様）
  this.playClick(2200, 1.2, 0.1)

  // 低音サンプ（低→超低にスウィープ）
  const osc = ctx.createOscillator()
  osc.type = 'sine'
  osc.frequency.setValueAtTime(150, now)
  osc.frequency.exponentialRampToValueAtTime(60, now + 0.08)

  const gain = ctx.createGain()
  gain.gain.setValueAtTime(this._volume * 0.4, now)
  gain.gain.exponentialRampToValueAtTime(0.001, now + 0.08)

  osc.connect(gain)
  gain.connect(ctx.destination)
  osc.start(now)
  osc.stop(now + 0.08)
}
```

#### Pattern C: ビープ音（単音 / 連続ビープ）

警告音、通知音、エラー音に使用。

```typescript
playBeep(frequency = 880, count = 1, interval = 0.14, beepDuration = 0.08): void {
  const ctx = this.getContext()
  const now = ctx.currentTime

  for (let i = 0; i < count; i++) {
    const t = now + i * interval
    const osc = ctx.createOscillator()
    osc.type = 'sine'
    osc.frequency.value = frequency

    const gain = ctx.createGain()
    gain.gain.setValueAtTime(0.001, t)
    gain.gain.linearRampToValueAtTime(this._volume * 0.35, t + 0.01)
    gain.gain.exponentialRampToValueAtTime(0.001, t + beepDuration)

    osc.connect(gain)
    gain.connect(ctx.destination)
    osc.start(t)
    osc.stop(t + beepDuration)
  }
}
```

**パラメータガイド:**
| 用途 | frequency | count | interval |
|------|-----------|-------|----------|
| 警告音 | 880 | 2 | 0.14 |
| エラー音 | 440 | 3 | 0.12 |
| 通知音 | 660 | 1 | — |
| アラーム | 1000 | 4 | 0.1 |

#### Pattern D: チャイム音（複数音ノートシーケンス）

成功音、開始/終了ジングル等に使用。

```typescript
playChime(notes: number[], interval = 0.15, noteDuration = 0.2): void {
  const ctx = this.getContext()
  const now = ctx.currentTime

  notes.forEach((freq, i) => {
    const t = now + i * interval
    const osc = ctx.createOscillator()
    osc.type = 'sine'
    osc.frequency.value = freq

    const gain = ctx.createGain()
    gain.gain.setValueAtTime(0.001, t)
    gain.gain.linearRampToValueAtTime(this._volume * 0.25, t + 0.02)
    gain.gain.setValueAtTime(this._volume * 0.25, t + noteDuration * 0.75)
    gain.gain.exponentialRampToValueAtTime(0.001, t + noteDuration)

    osc.connect(gain)
    gain.connect(ctx.destination)
    osc.start(t)
    osc.stop(t + noteDuration)
  })
}
```

**ノートシーケンス例:**
| 用途 | notes (Hz) | interval |
|------|------------|----------|
| 成功（上昇2音） | [440, 659] | 0.15 |
| 失敗（下降2音） | [659, 440] | 0.15 |
| 開始ジングル | [523, 659, 784] | 0.12 |
| 終了ジングル | [659, 523, 440] | 0.2 |
| レベルアップ | [523, 659, 784, 1047] | 0.1 |
| ゲームオーバー | [784, 659, 523, 440] | 0.25 |

**よく使う音階周波数:**
| 音名 | Hz |
|------|----|
| C4 | 262 |
| D4 | 294 |
| E4 | 330 |
| F4 | 349 |
| G4 | 392 |
| A4 | 440 |
| B4 | 494 |
| C5 | 523 |
| D5 | 587 |
| E5 | 659 |
| G5 | 784 |
| A5 | 880 |
| C6 | 1047 |

#### Pattern E: スウィープ音（周波数遷移）

レベルアップ、パワーアップ、トランジション等に使用。

```typescript
playSweep(startFreq: number, endFreq: number, duration = 0.15): void {
  const ctx = this.getContext()
  const now = ctx.currentTime

  const osc = ctx.createOscillator()
  osc.type = 'sine'
  osc.frequency.setValueAtTime(startFreq, now)
  osc.frequency.exponentialRampToValueAtTime(endFreq, now + duration)

  const gain = ctx.createGain()
  gain.gain.setValueAtTime(this._volume * 0.3, now)
  gain.gain.setValueAtTime(this._volume * 0.3, now + duration * 0.65)
  gain.gain.exponentialRampToValueAtTime(0.001, now + duration * 1.3)

  osc.connect(gain)
  gain.connect(ctx.destination)
  osc.start(now)
  osc.stop(now + duration * 1.3)
}
```

**パラメータガイド:**
| 用途 | startFreq | endFreq | duration |
|------|-----------|---------|----------|
| レベルアップ | 523 | 1047 | 0.15 |
| パワーダウン | 1047 | 523 | 0.15 |
| ホバーイン | 600 | 800 | 0.08 |
| ホバーアウト | 800 | 600 | 0.08 |

### Step 3: フレームワーク統合

#### React

```typescript
import { useState, useCallback } from 'react'

export function useSound() {
  const [enabled, setEnabled] = useState(soundManager.enabled)
  const [volume, setVolumeState] = useState(soundManager.volume)

  const toggle = useCallback(() => {
    const next = !soundManager.enabled
    soundManager.enabled = next
    setEnabled(next)
    if (next) soundManager.play('click') // feedback
  }, [])

  const setVolume = useCallback((v: number) => {
    soundManager.volume = v
    setVolumeState(v)
  }, [])

  return { enabled, volume, toggle, setVolume }
}
```

#### Vue 3 (Composition API)

```typescript
import { ref } from 'vue'

export function useSound() {
  const enabled = ref(soundManager.enabled)
  const volume = ref(soundManager.volume)

  function toggle() {
    const next = !soundManager.enabled
    soundManager.enabled = next
    enabled.value = next
    if (next) soundManager.play('click')
  }

  function setVolume(v: number) {
    soundManager.volume = v
    volume.value = v
  }

  return { enabled, volume, toggle, setVolume }
}
```

#### Svelte

```typescript
import { writable } from 'svelte/store'

export const soundEnabled = writable(soundManager.enabled)
export const soundVolume = writable(soundManager.volume)

export function toggleSound() {
  const next = !soundManager.enabled
  soundManager.enabled = next
  soundEnabled.set(next)
  if (next) soundManager.play('click')
}

export function setSoundVolume(v: number) {
  soundManager.volume = v
  soundVolume.set(v)
}
```

#### フレームワークなし (Vanilla JS)

```javascript
// トグルボタンにイベントリスナーを追加
document.getElementById('sound-toggle').addEventListener('click', () => {
  soundManager.enabled = !soundManager.enabled
  updateToggleUI(soundManager.enabled)
})

// 音量スライダー
document.getElementById('volume-slider').addEventListener('input', (e) => {
  soundManager.volume = parseFloat(e.target.value)
})
```

### Step 4: トグル UI コンポーネント

SVGアイコンでスピーカーのON/OFFを視覚的に切替える。

```tsx
// React example
export function SoundToggle() {
  const { enabled, volume, toggle, setVolume } = useSound()

  return (
    <div className="sound-toggle">
      <button
        onClick={toggle}
        aria-pressed={enabled}
        aria-label={enabled ? 'Mute' : 'Unmute'}
      >
        <svg viewBox="0 0 24 24" width="18" height="18"
             fill="none" stroke="currentColor" strokeWidth="2">
          <polygon points="11 5 6 9 2 9 2 15 6 15 11 19 11 5" />
          {enabled ? (
            <>
              <path d="M15.54 8.46a5 5 0 0 1 0 7.07" />
              <path d="M19.07 4.93a10 10 0 0 1 0 14.14" />
            </>
          ) : (
            <>
              <line x1="23" y1="9" x2="17" y2="15" />
              <line x1="17" y1="9" x2="23" y2="15" />
            </>
          )}
        </svg>
      </button>
      {enabled && (
        <input type="range" min="0" max="1" step="0.1"
               value={volume}
               onChange={e => setVolume(parseFloat(e.target.value))}
               aria-label="Volume" />
      )}
    </div>
  )
}
```

### Step 5: アプリケーション統合

ゲームロジックやUIイベントのコールバック内で `soundManager.play()` を呼ぶ。

```typescript
// ゲームの手が適用された後
async function makeMove(move) {
  const result = await api.applyMove(gameId, move)

  // 手の種類に応じた効果音
  if (move.isCapture) {
    soundManager.play('capture')        // Pattern B: 重い打撃
  } else if (move.isPromotion) {
    soundManager.play('promote')        // Pattern E: 上昇スウィープ
  } else {
    soundManager.play('move')           // Pattern A: クリック音
  }

  // 特殊状態の重ね再生（遅延をつけて重ならないように）
  if (result.isCheck) {
    setTimeout(() => soundManager.play('check'), 150)  // Pattern C: 警告ビープ
  }
}

// ゲーム開始
function onGameStart() {
  soundManager.play('gameStart')        // Pattern D: 上昇チャイム
}

// ゲーム終了
function onGameEnd() {
  soundManager.play('gameEnd')          // Pattern D: 下降チャイム
}
```

## Examples

### Example 1: 将棋ゲームの効果音セット

```typescript
const soundManager = new SoundManager('shogi')

// SoundType を定義
type ShogiSound = 'move' | 'capture' | 'check' | 'promote' | 'gameStart' | 'gameEnd'

// play メソッドで dispatch
play(type: ShogiSound) {
  switch (type) {
    case 'move':      this.playClick(2800, 1.5, 0.06); break
    case 'capture':   this.playHeavyHit(); break
    case 'check':     this.playBeep(880, 2, 0.14); break
    case 'promote':   this.playSweep(523, 1047, 0.15); break
    case 'gameStart': this.playChime([440, 659], 0.15); break
    case 'gameEnd':   this.playChime([659, 523, 440], 0.2); break
  }
}
```

### Example 2: フォームバリデーションのフィードバック音

```typescript
const soundManager = new SoundManager('form-app')

type FormSound = 'success' | 'error' | 'warning' | 'submit'

play(type: FormSound) {
  switch (type) {
    case 'success': this.playChime([523, 659], 0.12); break
    case 'error':   this.playBeep(440, 2, 0.12); break
    case 'warning': this.playBeep(660, 1); break
    case 'submit':  this.playClick(3500, 1.5, 0.04); break
  }
}
```

### Example 3: チャットアプリの通知音

```typescript
const soundManager = new SoundManager('chat')

type ChatSound = 'messageReceived' | 'messageSent' | 'mention' | 'connect' | 'disconnect'

play(type: ChatSound) {
  switch (type) {
    case 'messageReceived': this.playChime([659, 784], 0.1); break
    case 'messageSent':     this.playClick(3000, 1.0, 0.03); break
    case 'mention':         this.playBeep(880, 3, 0.1); break
    case 'connect':         this.playSweep(440, 880, 0.12); break
    case 'disconnect':      this.playSweep(880, 440, 0.12); break
  }
}
```

## Guidelines

1. **AudioContext の遅延初期化**: ブラウザはユーザーインタラクション前に AudioContext の再生を拒否する。初回の `play()` 呼び出し時に初期化し、`suspended` 状態なら `resume()` を呼ぶこと
2. **非致命的エラーハンドリング**: サウンド再生の失敗でアプリがクラッシュしてはならない。必ず `try/catch` で囲む
3. **ブラウザ互換性**: `AudioContext` は主要ブラウザ全てで対応済み（Chrome, Firefox, Safari, Edge）。`webkit` プレフィックスは不要（2020年以降）
4. **パフォーマンス**: 短い効果音はオーバーヘッドが小さい。ただし大量の同時再生は避ける（`setTimeout` で重ね合わせ間隔を設ける）
5. **`exponentialRampToValueAtTime` の制約**: 目標値は 0 より大きい正数でなければならない。無音にするには `0.001` 等の極小値を使う
6. **`linearRampToValueAtTime` の使い分け**: アタック（音の立ち上がり）には linear、リリース（減衰）には exponential を使うと自然なエンベロープになる
7. **音量の正規化**: `volume` は 0.0〜1.0 の範囲でクランプする。各サウンドメソッド内で `this._volume *` を掛けて適用する
8. **localStorage キーの命名**: アプリ固有のプレフィックスを使い（`{app}-sound`, `{app}-volume`）、他アプリとの衝突を防ぐ
9. **アクセシビリティ**: トグルボタンには `aria-pressed` と `aria-label` を必ず付ける。視覚障害者のスクリーンリーダー対応
10. **モバイル対応**: タッチデバイスではボタンの最小サイズを 44x44px にする（WCAG 2.5.5）。音量スライダーも太めに設定
