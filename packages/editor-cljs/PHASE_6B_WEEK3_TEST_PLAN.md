# Phase 6B Week 3 - Test Plan

## Features Implemented

### 1. Read-only Buffer Enforcement
- **Location**: `src/lexicon/events.cljs:1814-1824`
- **Feature**: Buffers with `:is-read-only? true` reject all edit operations
- **Implementation**: Transaction processor checks read-only flag before processing operations

**Test Steps**:
1. Create a buffer with `:is-read-only? true`
2. Try to insert text → Should show "Buffer is read-only" message
3. Try to delete text → Should show "Buffer is read-only" message
4. Verify operation is rejected and queue continues processing

### 2. Special-mode Base Mode
- **Location**: `src/lexicon/modes/special_mode.cljs`
- **Feature**: Base mode for read-only buffers with standard keybindings
- **Keybindings**:
  - `q`: Quit/bury buffer (`:special-mode/quit-buffer`)
  - `g`: Refresh/revert buffer (`:special-mode/revert-buffer`)
  - `SPC`: Scroll down
  - `DEL`: Scroll up
  - `<`: Beginning of buffer
  - `>`: End of buffer
  - `TAB`: Forward button
  - `S-TAB`: Backward button

**Test Steps**:
1. Enable special-mode on a buffer: `(rf/dispatch [:special-mode/enable buffer-id])`
2. Press `q` → Should show quit message
3. Press `g` → Should call revert function (if defined) or show "no revert function" message
4. Verify buffer is read-only
5. Verify keymap is installed

### 3. Mode-line Formatter
- **Location**: `src/lexicon/ui/mode_line.cljs`
- **Feature**: Format mode-line with % constructs

**% Constructs Implemented**:
- `%b`: Buffer name
- `%f`: File name (or buffer name if no file)
- `%l`: Line number
- `%c`: Column number
- `%p`: Position percentage (0-100%)
- `%I`: Buffer size in characters
- `%*`: Modified indicator (`**` modified, `--` unmodified, `%%` read-only modified, `%-` read-only)
- `%+`: Plus-style modified indicator
- `%m`: Major mode name
- `%%`: Literal %

**Standard Format Strings**:
- `standard-mode-line-format`: `" %*%* %b   %l:%c  %p  (%m)"`
- `special-mode-line-format`: `" %- %b   %l:%c  %p  (%m)"`
- `help-mode-line-format`: `" %- %b  (%m)"`

**Test Steps**:
1. Subscribe to `:mode-line/format` for a buffer
2. Verify format string is processed correctly
3. Test with modified buffer → Should show `**`
4. Test with read-only buffer → Should show `%-`
5. Test with read-only modified buffer → Should show `%%`
6. Verify line/column display updates with cursor movement
7. Verify percentage updates

### 4. Help Mode
- **Location**: `src/lexicon/modes/help_mode.cljs`
- **Feature**: Display help documentation in read-only buffers

**Events**:
- `:help/show [buffer-name content]`: Create/update help buffer
- `:help/revert [buffer-id]`: Refresh help content
- `:help/describe-function [function-name]`: Show function help
- `:help/describe-variable [variable-name]`: Show variable help
- `:help/show-help`: Show general help

**Test Steps**:
1. Dispatch `:help/show-help` → Should create `*Help*` buffer with general help
2. Verify buffer is read-only
3. Verify mode is `:help-mode`
4. Verify mode-line shows "Help"
5. Press `q` in help buffer → Should show quit message
6. Press `g` in help buffer → Should show revert message
7. Test `:help/describe-function` → Should create help buffer for function
8. Test `:help/describe-variable` → Should create help buffer for variable

### 5. Buffer Menu Mode
- **Location**: `src/lexicon/modes/buffer_menu_mode.cljs`
- **Feature**: Interactive buffer list display

**Events**:
- `:buffer-menu/list-buffers`: Show buffer list
- `:buffer-menu/revert [buffer-id]`: Refresh buffer list
- `:buffer-menu/select-buffer`: Switch to buffer (placeholder)
- `:buffer-menu/mark-for-deletion`: Mark buffer for deletion (placeholder)
- `:buffer-menu/execute`: Execute marked deletions (placeholder)
- `:buffer-menu/save-buffer`: Save buffer (placeholder)
- `:buffer-menu/unmark`: Unmark buffer (placeholder)

**Additional Keybindings** (merged with special-mode-map):
- `RET`: Select buffer
- `d`: Mark for deletion
- `x`: Execute deletions
- `s`: Save buffer
- `u`: Unmark

**Test Steps**:
1. Dispatch `:buffer-menu/list-buffers` → Should create `*Buffer List*` buffer
2. Verify buffer shows formatted list of all buffers
3. Verify columns: MR (modified/readonly flags), Name, Size, Mode, File
4. Verify buffer is read-only
5. Verify mode is `:buffer-menu-mode`
6. Press `g` → Should refresh buffer list with current buffers
7. Press `q` → Should show quit message
8. Test placeholder keybindings (RET, d, x, s, u) → Should show "not yet implemented" messages

## Integration Tests

### Test 1: Read-only Enforcement in Special Buffers
1. Create help buffer via `:help/show-help`
2. Try to type text → Should be rejected with "Buffer is read-only"
3. Try to delete text → Should be rejected
4. Verify cursor doesn't move on edit attempts

### Test 2: Mode-line Display
1. Create regular buffer → Mode-line should show `--` (unmodified)
2. Modify buffer → Mode-line should show `**`
3. Create help buffer → Mode-line should show `%-` (read-only)
4. Verify mode name shows correctly: "(Help)" for help-mode

### Test 3: Special-mode Keymap Inheritance
1. Create buffer-menu buffer
2. Press `g` → Should refresh (buffer-menu revert)
3. Press `q` → Should quit (special-mode quit)
4. Press `RET` → Should trigger buffer-menu select (not special-mode)
5. Verify child mode keymap overrides parent where specified

## Manual Testing Scenarios

### Scenario 1: Basic Help System
```clojure
;; In REPL or via dispatch:
(rf/dispatch [:help/show-help])
;; Expected: *Help* buffer appears with general help text
;; Mode-line shows: " %- *Help*  (Help)"

;; Press 'g'
;; Expected: Message "Revert *Help* (not yet implemented)"

;; Press 'q'
;; Expected: Message "Quit *Help* (buffer switching not yet implemented)"
```

### Scenario 2: Buffer List
```clojure
(rf/dispatch [:buffer-menu/list-buffers])
;; Expected: *Buffer List* buffer with formatted buffer table
;; Example output:
;; MR  Name                  Size  Mode            File
;; --- ----                  ----  ----            ----
;; --  *scratch*            0  fundamental-mode
;; %-  *Help*               250  help-mode
;; %-  *Buffer List*        180  buffer-menu-mode

;; Press 'g'
;; Expected: Buffer list refreshes with current buffers

;; Create new buffer, press 'g' again
;; Expected: New buffer appears in refreshed list
```

### Scenario 3: Mode-line Format Testing
```clojure
;; Test standard format
(rf/dispatch [:mode-line/set-format buffer-id " %*%* %b   %l:%c  %p  (%m)"])
;; Move cursor around, verify line:col updates
;; Modify buffer, verify ** appears
;; Make read-only, verify %- appears

;; Test custom format
(rf/dispatch [:mode-line/set-format buffer-id " [%b] Line %l of %I (%p)"])
;; Verify custom format displays correctly
```

## Known Limitations

1. **Buffer switching not implemented**: `q` shows message but doesn't actually switch buffers
2. **Button navigation**: TAB/S-TAB show "not yet implemented" (requires text properties with :button)
3. **Buffer menu operations**: RET, d, x, s, u are placeholders
4. **Help content**: Static placeholder content, not integrated with actual function/variable documentation
5. **Mode-line display**: Subscription exists but not yet integrated into view rendering

## Next Steps (Phase 6B Week 4)

1. Integrate mode-line into view rendering
2. Implement actual buffer switching for `q` command
3. Add button support for TAB navigation
4. Implement buffer menu operations (select, delete, save)
5. Create theming system (light/dark themes)
6. Connect help system to actual documentation sources
