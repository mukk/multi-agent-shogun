# web-audio-sfx-generator 設計書

## 概要
Web Audio API を使って効果音を合成生成するパターンをスキル化。外部音声ファイルを一切使わず、OscillatorNode・BufferSource・BiquadFilter・GainNode を組み合わせてクリック音・ビープ音・チャイム音・スウィープ音等を生成する。SoundManager シングルトン + フレームワーク統合（React useSound フック等）+ ON/OFF切替・音量調節・localStorage 永続化を含む。

## ユースケース
- ゲームアプリケーションの効果音（駒移動、アクション、イベント通知）
- Web UIのインタラクションフィードバック音（ボタンクリック、通知、エラー警告）
- 外部音声ファイルのホスティングが不要な軽量アプリケーション
- プロトタイプやMVPでの即時サウンド実装

## 提案者
足軽4号

## 提案理由
neo_shogi フロントエンドでサウンドエフェクトを実装した際、Web Audio API による合成音生成パターンが汎用的であることを発見。ゲームやUI操作のフィードバック音として他プロジェクトでも再利用可能。

## 承認
- 承認者: 殿（上様）
- 承認日: 2026-02-09
- cmd_087 にて承認

## 関連スキル
- css-game-animation-generator: アニメーションと合わせてSFXを同期可能
- game-timer-system: タイマー警告音との連携
- react-toast-notification: 通知音の統合
