#!/usr/bin/env bash
# scripts/agent_status.sh — 全エージェントの稼働状態を一覧表示
# 将軍が「稼働確認」スキルで呼び出す。
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$SCRIPT_DIR"

# 共有ライブラリ読み込み
source "$SCRIPT_DIR/lib/agent_status.sh"
source "$SCRIPT_DIR/lib/cli_adapter.sh"

# Python (PyYAML)
PYTHON="${SCRIPT_DIR}/.venv/bin/python3"

# エージェント定義 (shutsujin_departure.sh:583 と同じ)
AGENTS=("karo" "ashigaru1" "ashigaru2" "ashigaru3" "ashigaru4" "ashigaru5" "ashigaru6" "ashigaru7" "gunshi")

# pane-base-index を動的取得
PANE_BASE=$(tmux show-options -gv pane-base-index 2>/dev/null || echo 0)

# タスクYAMLからtask_id/statusを取得
get_task_info() {
    local agent_id="$1"
    local yaml_file="$SCRIPT_DIR/queue/tasks/${agent_id}.yaml"
    if [[ ! -f "$yaml_file" ]]; then
        echo "--- ---"
        return
    fi
    "$PYTHON" -c "
import yaml, sys
try:
    with open('${yaml_file}') as f:
        data = yaml.safe_load(f) or {}
    task = data.get('task', data)
    tid = task.get('task_id', '---')
    status = task.get('status', '---')
    print(f'{tid} {status}')
except Exception:
    print('--- ---')
" 2>/dev/null || echo "--- ---"
}

# 未読inbox数を取得
get_unread_count() {
    local agent_id="$1"
    local inbox_file="$SCRIPT_DIR/queue/inbox/${agent_id}.yaml"
    if [[ ! -f "$inbox_file" ]]; then
        echo "0"
        return
    fi
    "$PYTHON" -c "
import yaml, sys
try:
    with open('${inbox_file}') as f:
        data = yaml.safe_load(f) or {}
    msgs = data.get('messages', [])
    unread = sum(1 for m in msgs if not m.get('read', False))
    print(unread)
except Exception:
    print('?')
" 2>/dev/null || echo "?"
}

# 日本語表示幅補正付きprintf
# 全角文字は表示幅2だがprintfは1バイト=1幅で計算するため補正が必要
print_row() {
    local agent="$1" cli="$2" pane="$3" task_id="$4" status="$5" inbox="$6"
    # paneカラムの表示幅補正（日本語3文字=表示幅6、ASCII幅3→パディング+3必要）
    local pane_pad=""
    case "$pane" in
        稼働中|待機中) pane_pad="   " ;;  # 全角3文字→+3スペース補正
        不在)         pane_pad="       " ;;  # 全角2文字→+5スペース補正
        *)            pane_pad="" ;;
    esac
    printf "%-10s %-7s %s%s %-42s %-10s %s\n" "$agent" "$cli" "$pane" "$pane_pad" "$task_id" "$status" "$inbox"
}

# ヘッダー
printf "\n"
printf "%-10s %-7s %-9s %-42s %-10s %s\n" "Agent" "CLI" "Pane" "Task ID" "Status" "Inbox"
printf "%-10s %-7s %-9s %-42s %-10s %s\n" "----------" "-------" "---------" "------------------------------------------" "----------" "-----"

# 各エージェントの行
for i in "${!AGENTS[@]}"; do
    agent="${AGENTS[$i]}"
    pane_idx=$((PANE_BASE + i))
    pane_target="multiagent:agents.${pane_idx}"

    # CLI種別
    cli_type=$(get_cli_type "$agent" 2>/dev/null || echo "?")

    # Pane状態
    pane_state=$(get_pane_state_label "$pane_target")

    # タスク情報
    task_info=$(get_task_info "$agent")
    task_id=$(echo "$task_info" | awk '{print $1}')
    task_status=$(echo "$task_info" | awk '{$1=""; print $0}' | sed 's/^ //')

    # 未読inbox
    unread=$(get_unread_count "$agent")

    print_row "$agent" "$cli_type" "$pane_state" "$task_id" "$task_status" "$unread"
done

printf "\n"
