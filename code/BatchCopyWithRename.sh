#!/usr/bin/env bash
#
# batch_copy.sh
#
# Description:
#   Copy and/or rename multiple files according to a list of actions,
#   with logging, error-handling, and optional dry-run mode.
#
# Usage:
#   ./batch_copy.sh            # actually perform the moves
#   ./batch_copy.sh --dry-run  # show what *would* be done, without executing

set -euo pipefail

# Configuration
LOG_DIR="$HOME/logs"
LOG_FILE="$LOG_DIR/$(basename "$0").log"
DRY_RUN=false

# Define your moves here: each line is "src|dest_dir|newname"
declare -a MOVES=(
"/c/rworking/deepseatools/code/20251001-0_release_to_obis.R|/c/rworking/dscrtp/code/|release_to_obis.R"
"/c/rworking/deepseatools/code/mod_load_current_ndb.R|/c/rworking/dscrtp/code/|load_current_ndb.R"
"/c/rworking/deepseatools/code/20250711-0_global_taxonomy_patch_NatDB_20250409-0.R|/c/rworking/dscrtp/code/|global_taxonomy_patch.R"
"/c/rworking/deepseatools/code/20251001-0_runner_datasetID_dashboard.R|/c/rworking/dscrtp/code/|runner_datasetID_dashboard.R"
"/c/rworking/deepseatools/code/code_searcher.R|/c/rworking/dscrtp/code/|code_searcher.R"
"/c/rworking/deepseatools/code/148433.R|/c/rworking/dscrtp/code/|148433.R"
"/c/rworking/deepseatools/code/100635.R|/c/rworking/dscrtp/code/|100635.R"
"/c/rworking/deepseatools/code/119136.R|/c/rworking/dscrtp/code/|119136.R"
"/c/rworking/deepseatools/code/120205.R|/c/rworking/dscrtp/code/|120205.R"
"/c/rworking/deepseatools/code/120206.R|/c/rworking/dscrtp/code/|120206.R"
"/c/rworking/deepseatools/code/121131.R|/c/rworking/dscrtp/code/|121131.R"
"/c/rworking/deepseatools/code/121717.R|/c/rworking/dscrtp/code/|121717.R"
"/c/rworking/deepseatools/code/122876.R|/c/rworking/dscrtp/code/|122876.R"
"/c/rworking/deepseatools/code/123176.R|/c/rworking/dscrtp/code/|123176.R"
"/c/rworking/deepseatools/code/123180.R|/c/rworking/dscrtp/code/|123180.R"
"/c/rworking/deepseatools/code/126062.R|/c/rworking/dscrtp/code/|126062.R"
"/c/rworking/deepseatools/code/126248.R|/c/rworking/dscrtp/code/|126248.R"
"/c/rworking/deepseatools/code/126519.R|/c/rworking/dscrtp/code/|126519.R"
"/c/rworking/deepseatools/code/126744.R|/c/rworking/dscrtp/code/|126744.R"
"/c/rworking/deepseatools/code/127479.R|/c/rworking/dscrtp/code/|127479.R"
"/c/rworking/deepseatools/code/127481.R|/c/rworking/dscrtp/code/|127481.R"
"/c/rworking/deepseatools/code/127481-2.R|/c/rworking/dscrtp/code/|127481-2.R"
"/c/rworking/deepseatools/code/127640.R|/c/rworking/dscrtp/code/|127640.R"
"/c/rworking/deepseatools/code/128548.R|/c/rworking/dscrtp/code/|128548.R"
"/c/rworking/deepseatools/code/128852.R|/c/rworking/dscrtp/code/|128852.R"
"/c/rworking/deepseatools/code/129429.R|/c/rworking/dscrtp/code/|129429.R"
"/c/rworking/deepseatools/code/130435.R|/c/rworking/dscrtp/code/|130435.R"
"/c/rworking/deepseatools/code/131006.R|/c/rworking/dscrtp/code/|131006.R"
"/c/rworking/deepseatools/code/131377.R|/c/rworking/dscrtp/code/|131377.R"
"/c/rworking/deepseatools/code/131476.R|/c/rworking/dscrtp/code/|131476.R"
"/c/rworking/deepseatools/code/132586.R|/c/rworking/dscrtp/code/|132586.R"
"/c/rworking/deepseatools/code/132928.R|/c/rworking/dscrtp/code/|132928.R"
"/c/rworking/deepseatools/code/132980.R|/c/rworking/dscrtp/code/|132980.R"
"/c/rworking/deepseatools/code/141042.R|/c/rworking/dscrtp/code/|141042.R"
"/c/rworking/deepseatools/code/141753.R|/c/rworking/dscrtp/code/|141753.R"
"/c/rworking/deepseatools/code/142452.R|/c/rworking/dscrtp/code/|142452.R"
"/c/rworking/deepseatools/code/142454.R|/c/rworking/dscrtp/code/|142454.R"
"/c/rworking/deepseatools/code/142456.R|/c/rworking/dscrtp/code/|142456.R"
"/c/rworking/deepseatools/code/142457.R|/c/rworking/dscrtp/code/|142457.R"
"/c/rworking/deepseatools/code/142458.R|/c/rworking/dscrtp/code/|142458.R"
"/c/rworking/deepseatools/code/142459.R|/c/rworking/dscrtp/code/|142459.R"
"/c/rworking/deepseatools/code/142706.R|/c/rworking/dscrtp/code/|142706.R"
"/c/rworking/deepseatools/code/142714.R|/c/rworking/dscrtp/code/|142714.R"
"/c/rworking/deepseatools/code/143180.R|/c/rworking/dscrtp/code/|143180.R"
"/c/rworking/deepseatools/code/143469.R|/c/rworking/dscrtp/code/|143469.R"
"/c/rworking/deepseatools/code/143699.R|/c/rworking/dscrtp/code/|143699.R"
"/c/rworking/deepseatools/code/145764.R|/c/rworking/dscrtp/code/|145764.R"
"/c/rworking/deepseatools/code/145769.R|/c/rworking/dscrtp/code/|145769.R"
"/c/rworking/deepseatools/code/145771.R|/c/rworking/dscrtp/code/|145771.R"
"/c/rworking/deepseatools/code/145772.R|/c/rworking/dscrtp/code/|145772.R"
"/c/rworking/deepseatools/code/145773.R|/c/rworking/dscrtp/code/|145773.R"
"/c/rworking/deepseatools/code/145952.R|/c/rworking/dscrtp/code/|145952.R"
"/c/rworking/deepseatools/code/148415.R|/c/rworking/dscrtp/code/|148415.R"
"/c/rworking/deepseatools/code/148430.R|/c/rworking/dscrtp/code/|148430.R"

  # Add more entries as needed
)

# Functions
log() {
  local msg="$1"
  echo "$(date '+%Y-%m-%d %H:%M:%S') | $msg" | tee -a "$LOG_FILE"
}

do_move() {
  local src="$1"
  local dest_dir="$2"
  local newname="$3"

  # Ensure destination directory exists
  if [ ! -d "$dest_dir" ]; then
    if $DRY_RUN; then
      log "DRY-RUN: Would create directory '$dest_dir'"
    else
      log "Creating directory '$dest_dir'"
      mkdir -p "$dest_dir"
    fi
  fi

  local dest_path="${dest_dir%/}/$newname"

  if $DRY_RUN; then
    log "DRY-RUN: Would copy '$src' → '$dest_path'"
  else
    if [ ! -e "$src" ]; then
      log "ERROR: Source file does not exist: '$src'"
      return 1
    fi

    log "Copying '$src' → '$dest_path'"
    cp -- "$src" "$dest_path"
    local status=$?
    if [ $status -ne 0 ]; then
      log "ERROR: Failed to copy '$src' (exit status $status)"
      return $status
    fi
  fi
}

# Parse options
for arg in "$@"; do
  case "$arg" in
    --dry-run)
      DRY_RUN=true
      shift
      ;;
    *)
      # unknown option
      ;;
  esac
done

# Start script
mkdir -p "$LOG_DIR"
log "=== Script start: $(basename "$0") (DRY_RUN=$DRY_RUN) ==="

# Loop through moves
for item in "${MOVES[@]}"; do
  IFS='|' read -r src dest_dir newname <<< "$item"
  do_move "$src" "$dest_dir" "$newname" || log "WARNING: Move failed for item: $item"
done

log "=== Script end: $(basename "$0") ==="
