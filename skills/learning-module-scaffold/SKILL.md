---
name: learning-module-scaffold
description: TVar管理+forkIO非同期+ロジスティック回帰の学習モジュール雛形。スレッドセーフな状態管理（TVar）、バックグラウンド実行（forkIO）、段階的学習（自己対局→特徴抽出→勾配降下）、JSON永続化（GenerationSnapshot）を含む。AI評価関数の強化学習、パラメータチューニング、進化的アルゴリズムに使用。ゲームAI以外（推薦システム、最適化問題）にも適用可能。
---

# Learning Module Scaffold

## Overview

AI評価関数の学習モジュールを実装するためのスキル。以下の構成要素を含む:

- **Config型**: 学習率、エポック数、ゲーム数等の設定
- **Status型**: 現在のフェーズ（idle/selfplay/training/done）、進捗、損失値
- **Session型**: スレッドセーフなミュータブル状態（TVar）
- **バックグラウンド実行**: `forkIO` による非同期学習プロセス
- **段階的学習**:
  1. 自己対局で訓練データ収集
  2. 特徴抽出（局面→特徴ベクトル）
  3. ロジスティック回帰で勾配降下
  4. 新世代の重みを保存
- **Generation管理**: 世代スナップショット（weights + metadata）の永続化

## When to Use

- ゲームAIの評価関数をデータから学習させる
- 駒の価値（pieceの value）を対局結果から最適化する
- 強化学習のreward関数をパラメータ調整する
- 遺伝的アルゴリズムの世代管理に使う
- 既存の評価関数を新しいバリアントに適応させる

## Instructions

### Step 1: Config / Status 型の定義

学習の設定と進捗を表すデータ型を定義する。

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:))

-- 学習設定
data LearningConfig = LearningConfig
  { lcNumGames     :: !Int     -- 自己対局数（1エポックあたり）
  , lcLearningRate :: !Double  -- 勾配降下のステップサイズ
  , lcEpochs       :: !Int     -- 訓練エポック数
  } deriving (Show, Eq, Generic)

instance ToJSON LearningConfig where
  toJSON LearningConfig{..} = object
    [ "numGames"     .= lcNumGames
    , "learningRate" .= lcLearningRate
    , "epochs"       .= lcEpochs
    ]

instance FromJSON LearningConfig

-- 学習進捗状態
data LearningStatus = LearningStatus
  { lsPhase        :: !Text    -- "idle" | "selfplay" | "training" | "done"
  , lsCurrentEpoch :: !Int     -- 現在のエポック番号（0から開始）
  , lsTotalEpochs  :: !Int     -- 総エポック数
  , lsCurrentLoss  :: !Double  -- 現在の損失値
  , lsMessage      :: !Text    -- 状態メッセージ
  } deriving (Show, Generic)

instance ToJSON LearningStatus
instance FromJSON LearningStatus
```

### Step 2: スレッドセーフな Session 管理

TVar で状態を管理し、複数スレッドから安全にアクセスできるようにする。

```haskell
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, writeTVar)

newtype LearningSession = LearningSession (TVar LearningStatus)

newLearningSession :: IO LearningSession
newLearningSession = LearningSession <$> newTVarIO initialStatus
  where
    initialStatus = LearningStatus "idle" 0 0 0.0 "Ready"

-- 状態更新（アトミック）
updateStatus :: LearningSession -> (LearningStatus -> LearningStatus) -> IO ()
updateStatus (LearningSession var) f =
  atomically $ do
    s <- readTVar var
    writeTVar var (f s)

-- 状態取得
getLearningStatus :: LearningSession -> IO LearningStatus
getLearningStatus (LearningSession var) = readTVarIO var
```

### Step 3: バックグラウンド学習プロセス

`forkIO` で学習を非同期実行し、完了を待たずにAPIレスポンスを返す。

```haskell
import Control.Concurrent (forkIO)
import Control.Monad (void)

startLearning :: LearningSession -> LearningConfig -> IO ()
startLearning session config = void $ forkIO $ do
  -- Phase 1: Self-play
  updateStatus session $ \s -> s
    { lsPhase = "selfplay"
    , lsTotalEpochs = lcEpochs config
    , lsMessage = "Collecting training data..."
    }
  samples <- collectSamples (lcNumGames config)

  -- Phase 2: Training loop
  updateStatus session $ \s -> s { lsPhase = "training" }
  finalWeights <- trainLoop session config samples

  -- Phase 3: Save generation
  saveGeneration finalWeights

  -- Done
  updateStatus session $ \s -> s
    { lsPhase = "done"
    , lsMessage = "Learning complete"
    }

trainLoop :: LearningSession -> LearningConfig -> [Sample] -> IO Weights
trainLoop session config samples = go 0 initialWeights
  where
    go epoch weights
      | epoch >= lcEpochs config = pure weights
      | otherwise = do
          let (weights', loss) = gradientStep (lcLearningRate config) samples weights
          updateStatus session $ \s -> s
            { lsCurrentEpoch = epoch
            , lsCurrentLoss = loss
            , lsMessage = "Epoch " <> T.pack (show epoch) <> " / " <> T.pack (show (lcEpochs config))
            }
          go (epoch + 1) weights'
```

### Step 4: 特徴抽出と勾配降下

局面から特徴ベクトルを抽出し、ロジスティック回帰で勾配を計算する。

```haskell
type Sample = (FeatureVector, Outcome)  -- (特徴, 結果: 1.0=勝ち, 0.0=負け)
type FeatureVector = [Double]
type Weights = [Double]

-- 局面から特徴抽出（駒カウント等）
extractFeatures :: Position -> FeatureVector
extractFeatures pos = [fromIntegral (countPiece "King" pos),
                       fromIntegral (countPiece "Rook" pos),
                       -- ... 他の駒
                      ]

-- 勾配降下の1ステップ
gradientStep :: Double -> [Sample] -> Weights -> (Weights, Double)
gradientStep learningRate samples weights =
  let gradients = map (gradient weights) samples
      avgGrad = average gradients
      weights' = zipWith (\w g -> w - learningRate * g) weights avgGrad
      loss = meanSquaredError weights' samples
  in (weights', loss)

-- ロジスティック関数
logistic :: Double -> Double
logistic z = 1.0 / (1.0 + exp (-z))

-- 勾配計算（1サンプル）
gradient :: Weights -> Sample -> [Double]
gradient weights (features, outcome) =
  let prediction = logistic (dot weights features)
      error = prediction - outcome
  in map (* error) features

dot :: [Double] -> [Double] -> Double
dot xs ys = sum (zipWith (*) xs ys)
```

### Step 5: Generation Snapshot の保存

学習した重みを JSON ファイルとして永続化する。

```haskell
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Aeson as A

data GenerationSnapshot = GenerationSnapshot
  { gsGeneration :: !Int
  , gsTimestamp  :: !Text
  , gsWeights    :: !Weights
  , gsMetadata   :: !Metadata
  } deriving (Show, Generic)

instance ToJSON GenerationSnapshot
instance FromJSON GenerationSnapshot

saveGeneration :: Weights -> IO ()
saveGeneration weights = do
  gens <- listGenerations
  let nextGen = length gens
  now <- getCurrentTime
  let timestamp = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now
      snapshot = GenerationSnapshot nextGen timestamp weights (Metadata "Logistic Regression" 0.01)
  A.encodeFile ("data/generations/gen_" <> show nextGen <> ".json") snapshot
```

### Step 6: API 統合

Servant API で学習開始・状態取得エンドポイントを公開する。

```haskell
type LearningAPI =
  "api" :> "learning" :> "start"
    :> ReqBody '[JSON] LearningConfig
    :> Post '[JSON] (ApiResponse Text)
  :<|> "api" :> "learning" :> "status"
    :> Get '[JSON] (ApiResponse LearningStatus)

learningServer :: LearningSession -> Server LearningAPI
learningServer session = handleStart :<|> handleStatus
  where
    handleStart config = do
      liftIO $ startLearning session config
      pure $ okResponse "Learning started"

    handleStatus = do
      status <- liftIO $ getLearningStatus session
      pure $ okResponse status
```

## Examples

### Example 1: 将棋の駒価値学習

```haskell
main :: IO ()
main = do
  session <- newLearningSession
  let config = LearningConfig
        { lcNumGames = 100       -- 100局で訓練データ収集
        , lcLearningRate = 0.01  -- 学習率 1%
        , lcEpochs = 50          -- 50エポック
        }
  startLearning session config
  -- 非同期で実行されるため即座にリターン
  putStrLn "Learning started in background"
```

### Example 2: フロントエンドからのポーリング

```typescript
// React hook for polling learning status
function useLearning() {
  const [status, setStatus] = useState<LearningStatus | null>(null)

  useEffect(() => {
    const poll = async () => {
      const res = await fetch('/api/learning/status')
      const data = await res.json()
      setStatus(data.payload)
    }

    const interval = setInterval(poll, 2000)  // 2秒ごとにポーリング
    return () => clearInterval(interval)
  }, [])

  return status
}
```

## Guidelines

1. **TVar を必ず使う**: 複数スレッドからアクセスする状態は `TVar` で管理。`IORef` は不可（アトミック性なし）
2. **forkIO の戻り値は捨てる**: `void $ forkIO $ ...` でスレッドIDを無視。キャンセル機能が必要なら `Async` を使う
3. **エポックごとに状態更新**: UI が進捗を表示できるように、各エポックで `updateStatus` を呼ぶ
4. **損失値を記録**: 学習が収束しているか判定するため `lsCurrentLoss` を更新
5. **Phase の遷移を明示**: "idle" → "selfplay" → "training" → "done" の順序を守る
6. **世代番号は自動採番**: 既存の世代数を数えて `nextGen = length gens` で決定
7. **学習率は小さく**: デフォルト 0.01。大きすぎると発散する
8. **自己対局は並列化可**: `mapConcurrently` で複数対局を並列実行すると高速化
9. **特徴量は正規化**: 駒カウントをそのまま使わず、[-1, 1] にスケーリング推奨
10. **タイムアウト設定**: 学習が長時間かかる場合、エポック数の上限を設ける
