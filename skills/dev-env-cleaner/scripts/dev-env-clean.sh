#!/usr/bin/env bash
# dev-env-clean.sh — 開発環境キャッシュクリーナー
# chmod +x dev-env-clean.sh で実行権限を付与してから使用すること
#
# 使用例:
#   ./dev-env-clean.sh               # 対話メニュー
#   ./dev-env-clean.sh all           # 全ツール実行
#   ./dev-env-clean.sh pip           # pipのみ
#   ./dev-env-clean.sh all --dry-run # ドライラン

set -euo pipefail

# ─── カラー定義 ──────────────────────────────────────────────
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# ─── グローバル変数 ────────────────────────────────────────────
DRY_RUN=false
TOOL=""
TOTAL_BEFORE=0
TOTAL_AFTER=0

# ─── ユーティリティ ────────────────────────────────────────────
info()    { echo -e "${BLUE}[INFO]${NC}  $*"; }
success() { echo -e "${GREEN}[OK]${NC}    $*"; }
warn()    { echo -e "${YELLOW}[WARN]${NC}  $*"; }
error()   { echo -e "${RED}[ERROR]${NC} $*"; }

run_cmd() {
    if $DRY_RUN; then
        echo -e "${YELLOW}[DRY-RUN]${NC} would run: $*"
    else
        "$@"
    fi
}

# バイト数を返す（du -sb 互換、パスが存在しない場合は0）
dir_bytes() {
    local path="$1"
    if [[ -d "$path" ]]; then
        du -sb "$path" 2>/dev/null | awk '{print $1}' || echo 0
    else
        echo 0
    fi
}

# 人間が読みやすいサイズを返す
show_size() {
    local path="$1"
    local label="$2"
    if [[ -d "$path" ]]; then
        local size
        size=$(du -sh "$path" 2>/dev/null | awk '{print $1}' || echo "N/A")
        info "${label}: ${size}  (${path})"
    else
        info "${label}: (ディレクトリなし)  (${path})"
    fi
}

# ─── ヘルプ ────────────────────────────────────────────────────
usage() {
    cat <<EOF
使用方法: $(basename "$0") [TOOL] [OPTIONS]

TOOL:
  all    全ツールを実行
  pip    pipキャッシュをクリーン
  npm    npmキャッシュをクリーン
  stack  stackキャッシュをクリーン
  scoop  scoopキャッシュをクリーン (WSL2のみ)

OPTIONS:
  --dry-run  実際のコマンドを実行せず、内容を表示するだけ
  --help     このヘルプを表示

例:
  $(basename "$0")                # 対話メニュー
  $(basename "$0") all            # 全ツール実行
  $(basename "$0") pip            # pipのみ
  $(basename "$0") all --dry-run  # ドライラン
EOF
    exit 0
}

# ─── 引数解析 ──────────────────────────────────────────────────
parse_args() {
    for arg in "$@"; do
        case "$arg" in
            --dry-run) DRY_RUN=true ;;
            --help|-h) usage ;;
            all|pip|npm|stack|scoop) TOOL="$arg" ;;
            *)
                error "不明な引数: $arg"
                usage
                ;;
        esac
    done
}

# ─── 対話メニュー ──────────────────────────────────────────────
interactive_menu() {
    echo ""
    echo -e "${BLUE}=============================${NC}"
    echo -e "${BLUE}  dev-env-clean.sh${NC}"
    echo -e "${BLUE}=============================${NC}"
    echo "クリーンアップするツールを選択:"
    echo "  1) all   — 全ツール"
    echo "  2) pip   — pip"
    echo "  3) npm   — npm"
    echo "  4) stack — Stack (Haskell)"
    echo "  5) scoop — Scoop (WSL2のみ)"
    echo "  q) 終了"
    echo ""
    read -r -p "選択 [1-5/q]: " choice
    case "$choice" in
        1) TOOL="all" ;;
        2) TOOL="pip" ;;
        3) TOOL="npm" ;;
        4) TOOL="stack" ;;
        5) TOOL="scoop" ;;
        q|Q) info "終了します。"; exit 0 ;;
        *) error "無効な選択: $choice"; exit 1 ;;
    esac
}

# ─── pip ──────────────────────────────────────────────────────
pip_clean() {
    echo ""
    info "======== pip クリーンアップ ========"
    local pip_cache="${HOME}/.cache/pip"
    show_size "$pip_cache" "pip cache (前)"
    local before
    before=$(dir_bytes "$pip_cache")

    set +e

    info "pip: 古いパッケージ確認..."
    if command -v pip &>/dev/null; then
        if $DRY_RUN; then
            echo -e "${YELLOW}[DRY-RUN]${NC} would run: pip list --outdated"
        else
            pip list --outdated 2>/dev/null || warn "pip list --outdated 失敗 (スキップ)"
        fi

        info "pip: pip自体をアップグレード..."
        run_cmd pip install --upgrade pip 2>/dev/null || warn "pip upgrade 失敗 (スキップ)"

        info "pip: キャッシュをパージ..."
        run_cmd pip cache purge 2>/dev/null || warn "pip cache purge 失敗 (スキップ)"
        success "pip クリーンアップ完了"
    else
        warn "pip コマンドが見つかりません。スキップします。"
    fi

    set -e

    show_size "$pip_cache" "pip cache (後)"
    local after
    after=$(dir_bytes "$pip_cache")
    TOTAL_BEFORE=$((TOTAL_BEFORE + before))
    TOTAL_AFTER=$((TOTAL_AFTER + after))
}

# ─── npm ──────────────────────────────────────────────────────
npm_clean() {
    echo ""
    info "======== npm クリーンアップ ========"
    local npm_cache="${HOME}/.npm"
    show_size "$npm_cache" "npm cache (前)"
    local before
    before=$(dir_bytes "$npm_cache")

    set +e

    if command -v npm &>/dev/null; then
        info "npm: グローバル古いパッケージ確認..."
        if $DRY_RUN; then
            echo -e "${YELLOW}[DRY-RUN]${NC} would run: npm list -g --outdated"
        else
            npm list -g --outdated 2>/dev/null || warn "npm list -g --outdated 失敗 (スキップ)"
        fi

        info "npm: グローバルパッケージ更新..."
        run_cmd npm update -g 2>/dev/null || warn "npm update -g 失敗 (スキップ)"

        info "npm: キャッシュクリーン..."
        run_cmd npm cache clean --force 2>/dev/null || warn "npm cache clean 失敗 (スキップ)"
        success "npm クリーンアップ完了"
    else
        warn "npm コマンドが見つかりません。スキップします。"
    fi

    set -e

    show_size "$npm_cache" "npm cache (後)"
    local after
    after=$(dir_bytes "$npm_cache")
    TOTAL_BEFORE=$((TOTAL_BEFORE + before))
    TOTAL_AFTER=$((TOTAL_AFTER + after))
}

# ─── stack ────────────────────────────────────────────────────
stack_clean() {
    echo ""
    info "======== Stack (Haskell) クリーンアップ ========"
    local stack_snapshots="${HOME}/.stack/snapshots"
    local stack_programs="${HOME}/.stack/programs"
    show_size "${HOME}/.stack" "stack dir (前)"
    local before
    before=$(dir_bytes "${HOME}/.stack")

    set +e

    if command -v stack &>/dev/null; then
        info "stack: アップデート..."
        run_cmd stack update 2>/dev/null || warn "stack update 失敗 (スキップ)"

        info "stack: snapshotsディレクトリサイズ:"
        show_size "$stack_snapshots" "  snapshots"
        show_size "$stack_programs" "  programs"

        # stack clean --all は確認ステップ必須
        echo ""
        warn "stack clean --all を実行しますか？（ビルドキャッシュが全削除されます）"
        if $DRY_RUN; then
            echo -e "${YELLOW}[DRY-RUN]${NC} would prompt for: stack clean --all"
        else
            read -r -p "  実行しますか? [y/N]: " confirm
            if [[ "$confirm" =~ ^[Yy]$ ]]; then
                run_cmd stack clean --all 2>/dev/null || warn "stack clean --all 失敗 (スキップ)"
                success "stack clean --all 完了"
            else
                info "stack clean --all をスキップしました。"
            fi
        fi

        success "stack クリーンアップ完了"
    else
        warn "stack コマンドが見つかりません。スキップします。"
    fi

    set -e

    show_size "${HOME}/.stack" "stack dir (後)"
    local after
    after=$(dir_bytes "${HOME}/.stack")
    TOTAL_BEFORE=$((TOTAL_BEFORE + before))
    TOTAL_AFTER=$((TOTAL_AFTER + after))
}

# ─── scoop ────────────────────────────────────────────────────
scoop_clean() {
    echo ""
    info "======== Scoop クリーンアップ ========"

    # WSL2判定
    if ! grep -qi microsoft /proc/version 2>/dev/null; then
        warn "WSL2環境ではないため、Scoopクリーンアップをスキップします。"
        return
    fi

    if ! command -v powershell.exe &>/dev/null; then
        warn "powershell.exe が見つかりません。Scoopクリーンアップをスキップします。"
        return
    fi

    # Scoopが存在するか確認
    if ! powershell.exe -NoProfile -Command "Get-Command scoop -ErrorAction SilentlyContinue" &>/dev/null; then
        warn "Scoopがインストールされていません。スキップします。"
        return
    fi

    local scoop_cache
    scoop_cache=$(powershell.exe -NoProfile -Command "[Environment]::GetFolderPath('UserProfile') + '\scoop\cache'" 2>/dev/null | tr -d '\r' || echo "")

    info "Scoop cache: ${scoop_cache:-N/A}"

    set +e

    info "scoop: 全パッケージアップデート..."
    if $DRY_RUN; then
        echo -e "${YELLOW}[DRY-RUN]${NC} would run: powershell.exe -Command \"scoop update *\""
        echo -e "${YELLOW}[DRY-RUN]${NC} would run: powershell.exe -Command \"scoop cleanup *\""
        echo -e "${YELLOW}[DRY-RUN]${NC} would run: powershell.exe -Command \"scoop cache rm *\""
    else
        powershell.exe -NoProfile -Command "scoop update *" 2>/dev/null || warn "scoop update 失敗 (スキップ)"
        powershell.exe -NoProfile -Command "scoop cleanup *" 2>/dev/null || warn "scoop cleanup 失敗 (スキップ)"
        powershell.exe -NoProfile -Command "scoop cache rm *" 2>/dev/null || warn "scoop cache rm 失敗 (スキップ)"
        success "scoop クリーンアップ完了"
    fi

    set -e
}

# ─── サマリ表示 ────────────────────────────────────────────────
show_summary() {
    echo ""
    echo -e "${GREEN}=============================${NC}"
    echo -e "${GREEN}  クリーンアップ完了${NC}"
    echo -e "${GREEN}=============================${NC}"
    if $DRY_RUN; then
        warn "ドライランモードのため実際の変更はありません。"
    else
        local saved=$((TOTAL_BEFORE - TOTAL_AFTER))
        if [[ $saved -gt 0 ]]; then
            # バイトをMBに変換
            local saved_mb
            saved_mb=$(echo "scale=1; $saved / 1048576" | bc 2>/dev/null || echo "?")
            success "トータル削減量: 約 ${saved_mb} MB"
        else
            info "削減量: 計測対象ディレクトリなし、または変化なし"
        fi
    fi
}

# ─── メイン ────────────────────────────────────────────────────
main() {
    parse_args "$@"

    if $DRY_RUN; then
        warn "ドライランモードで実行します。実際の変更は行いません。"
    fi

    [[ -z "$TOOL" ]] && interactive_menu

    case "$TOOL" in
        all)
            pip_clean
            npm_clean
            stack_clean
            scoop_clean
            ;;
        pip)   pip_clean ;;
        npm)   npm_clean ;;
        stack) stack_clean ;;
        scoop) scoop_clean ;;
        *)
            error "不明なツール: $TOOL"
            usage
            ;;
    esac

    show_summary
}

main "$@"
