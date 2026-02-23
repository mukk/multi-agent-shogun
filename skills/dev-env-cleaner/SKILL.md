---
name: dev-env-cleaner
description: 開発環境のパッケージマネージャ（pip/npm/stack/scoop）を最新状態に保ち、キャッシュと不要ファイルを削除する。定期メンテナンス、ディスク容量逼迫時、環境リフレッシュ時に使用。
---

# Dev-Env-Cleaner

## Overview

pip（Python）、npm（Node.js）、stack（Haskell）、scoop（Windows）の4ツールについて、
更新・キャッシュクリア・古いバージョン削除を行う定期メンテナンス手順集。
WSL2/Linux環境とWindows環境の両方に対応。

## When to Use

- 定期的な開発環境メンテナンス（月次など）
- ディスク容量が逼迫しているとき
- ビルドや依存関係の不具合が続くとき
- 新しいプロジェクトを開始する前のクリーン化
- 「環境を一度リセットしたい」というユーザー要望時

## Instructions

### pip (Linux/WSL2)

```bash
# 1. pip 自体を更新
pip install --upgrade pip

# 2. アウトデートパッケージの確認
pip list --outdated

# 3. 個別パッケージの更新（慎重に実行）
pip install --upgrade <package>

# 全アウトデートパッケージを一括更新する場合（仮想環境内での使用を推奨）
pip list --outdated --format=freeze | grep -v '^\-e' | cut -d= -f1 | xargs -n1 pip install --upgrade

# 4. キャッシュのクリア
pip cache purge

# 5. 不要パッケージの削除（オプション・要インストール）
pip install pip-autoremove
pip-autoremove -L          # 削除対象の一覧表示
pip-autoremove <package>   # 指定パッケージと依存関係を削除
```

### npm (Linux/WSL2 + Windows両対応)

```bash
# 1. グローバルパッケージの更新
npm update -g

# 2. グローバルパッケージの一覧確認
npm list -g --depth=0

# 3. キャッシュのクリア
npm cache clean --force

# 4. nvm 使用時: 古いNode.jsバージョンの確認と削除
nvm ls                         # インストール済みバージョン一覧
nvm ls-remote --lts            # 利用可能なLTS一覧
nvm install --lts              # 最新LTSをインストール
nvm alias default lts/*        # デフォルトをLTSに設定
nvm uninstall <old-version>    # 古いバージョンを削除

# fnm 使用時（nvm互換の高速代替）
fnm list                       # インストール済み一覧
fnm install --lts
fnm default lts-latest
# ※ fnm は手動での古いバージョン削除を ~/.fnm/node-versions/ で行う

# 5. プロジェクト外の古い node_modules を検出
# カレントディレクトリ配下で更新日が90日以上前の node_modules を検索
find ~ -name "node_modules" -maxdepth 4 -type d -not -path "*/\.*" \
  -mtime +90 2>/dev/null | head -20

# Windows環境（PowerShell）でグローバルnpm更新
# npm update -g  ← 同じコマンド（WSL2からの場合は下記）
powershell.exe -Command "npm update -g"
```

### stack (Linux/WSL2)

```bash
# 1. snapshot index の更新
stack update

# 2. カレントプロジェクトのビルドキャッシュを削除
stack clean

# 3. ~/.stack/snapshots/ の古いスナップショット整理（手動）
ls ~/.stack/snapshots/                # スナップショット一覧確認
du -sh ~/.stack/snapshots/*/         # 各スナップショットのサイズ確認
# ※ 使用中スナップショットは削除しないこと（stack.yaml の resolver を確認）
cat stack.yaml | grep resolver       # 使用中のresolverを確認

# 古いスナップショットを手動削除（使用中でないことを確認してから）
rm -rf ~/.stack/snapshots/<old-lts-version>

# 4. 全ビルドアーティファクト削除（WARNING: 要確認）
# ⚠️ 次回ビルドに数十分かかる場合がある
stack purge
```

### scoop (Windows/PowerShell)

```powershell
# PowerShell で実行（管理者権限不要）

# 1. scoop 自体とバケット定義を更新
scoop update

# 2. 全パッケージを更新
scoop update *

# 3. 古いバージョンを削除
scoop cleanup *

# 4. ダウンロードキャッシュを削除
scoop cache rm *

# 5. 現在インストール済みパッケージの確認
scoop list
```

```bash
# WSL2からPowerShell経由で実行する場合
powershell.exe -Command "scoop update"
powershell.exe -Command "scoop update *"
powershell.exe -Command "scoop cleanup *"
powershell.exe -Command "scoop cache rm *"
```

---

## Optional Tools

### nvm / fnm（Node.jsバージョン管理）

```bash
# nvm のセルフ更新（インストールスクリプトを再実行）
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/master/install.sh | bash
# または: brew upgrade nvm

# fnm のセルフ更新
curl -fsSL https://fnm.vercel.app/install | bash
```

### pyenv（Pythonバージョン管理）

```bash
# pyenv のセルフ更新
pyenv update

# インストール済みPythonバージョン確認
pyenv versions

# 古いバージョンを削除
pyenv uninstall <old-version>
```

### rustup / cargo（Rust）

```bash
# rustup とツールチェーン更新
rustup update

# cargo のキャッシュクリア（cargo-cache クレートを使用）
cargo install cargo-cache
cargo cache --autoclean

# レジストリキャッシュのみクリア
cargo cache --remove-dir registry

# ビルドキャッシュ確認
du -sh ~/.cargo/registry/
du -sh ~/.cargo/git/
```

### gem（Ruby）

```bash
# gem のセルフ更新
gem update --system

# インストール済みgemを更新
gem update

# 古いバージョンを削除
gem cleanup

# キャッシュ確認
gem environment | grep CACHE
```

---

## Guidelines

### 安全性の注意事項

1. **破壊的操作の前に確認を促すこと**
   - `pip-autoremove`: 削除対象リストを `-L` で表示してから実行
   - `stack purge`: 全ビルドキャッシュが消える。次回ビルドに時間がかかる旨を事前に伝える
   - `scoop cleanup *`: 現在使用中バージョン以外が削除される（通常安全だが確認推奨）
   - 古い `node_modules` 削除: 対象ディレクトリが本当に不要か確認してから `rm -rf`

2. **仮想環境を確認してから実行**
   - pip 操作はグローバル環境を汚染しないよう、`venv` や `conda` 内で行うことを推奨
   - `which python`, `which pip` で現在の環境を確認

3. **dev-env-clean.sh スクリプトについて**
   - 同スキル内に `scripts/dev-env-clean.sh` がある場合、`--dry-run` フラグで事前確認可能
   - 本番実行前に必ず dry-run を推奨: `bash scripts/dev-env-clean.sh --dry-run`

4. **stack purge 警告**
   - `stack purge` はカレントプロジェクトの全ビルドアーティファクトを削除する
   - グローバルスナップショットは削除しないが、再ビルドに時間を要する
   - 複数プロジェクトが同じスナップショットを共有している場合は `stack clean` のみ推奨

5. **WSL2環境でのパス注意**
   - Windows側（`/mnt/c/`配下）のファイルを誤って削除しないよう、対象パスを `realpath` で確認すること
   - `find` コマンドのスコープを明示的に絞ること（`~` や特定プロジェクトディレクトリのみ）
