# Testing Checklist

Items marked ✅ are covered by automated UI integration tests (`tests/ui_integration/`).

## CLI / Launch Modes

- [x] ✅ No arguments: launches Welcome window
- [x] ✅ Two file paths: opens file diff window
- [x] ✅ Two directory paths: opens directory comparison window
- [x] ✅ Three file paths: opens 3-way merge window
- [x] ✅ One directory (git repo): opens VCS window
- [x] ✅ One directory (not a git repo): prints error and exits
- [x] ✅ Single file path: prints error and exits
- [x] ✅ Non-existent path: prints error and exits
- [x] ✅ File + directory mix: prints error
- [x] ✅ `-L`/`--label` flags: custom pane labels shown for 2-file and 3-file modes
- [x] ✅ `--version` / `--help`: correct output

## Welcome Window

- [ ] "Compare Files" button: two file dialogs, then opens diff; welcome closes
- [ ] "Compare Directories" button: two folder dialogs, then opens dir window; welcome closes
- [ ] "3-way Merge" button: three file dialogs, then opens merge; welcome closes
- [ ] Preferences button / Ctrl+,: opens Preferences dialog
- [ ] Ctrl+W: closes welcome window
- [ ] Ctrl+N: opens a new welcome window
- [ ] Cancel any file dialog: nothing happens

## File Diff (2-way)

### Display
- [ ] Chunk backgrounds: blue for replace, green for insert/delete
- [ ] Current chunk: bold dark borders when navigating
- [ ] Inline word-level highlighting: yellow (changed), pink (deleted), green (inserted)
- [ ] Filler lines: thin colored lines where the other side has more content
- [ ] Gutter: curved connecting bands between panes
- [ ] Chunk map strips: minimap of changes at far edges; clickable to jump
- [ ] Chunk map viewport indicator: visible rectangle showing current scroll position
- [ ] Syntax highlighting: detected from filename
- [ ] Line numbers: shown/hidden per settings

### Toolbar
- [x] ✅ Undo (Ctrl+Z) / Redo (Ctrl+Shift+Z): acts on last-focused pane
- [ ] Previous change (Alt+Up / Ctrl+E): navigates to previous diff chunk
- [ ] Next change (Alt+Down / Ctrl+D): navigates to next diff chunk
- [x] ✅ Chunk label: shows "N changes" or "Change X of N"
- [x] ✅ Go to line (Ctrl+L): entry appears; type number + Enter to jump; Escape to dismiss; focus returns to active pane
- [x] ✅ Blanks toggle: re-diffs ignoring blank lines
- [x] ✅ Spaces toggle: re-diffs ignoring whitespace
- [ ] Export patch (Ctrl+Shift+P): save dialog; generates unified diff
- [x] ✅ Swap panes: swaps content, labels, save paths, dirty state; title updates
- [x] ✅ Preferences (Ctrl+,): opens Preferences

### Gutter Interactions
- [ ] Click right arrow: copies left chunk to right
- [ ] Click left arrow: copies right chunk to left
- [ ] Right-click gutter: context menu with both copy options
- [ ] Paste highlight flash: 500ms blue flash on inserted text

### Scroll Sync
- [ ] Horizontal scroll synced between panes
- [ ] Vertical scroll synced by chunk between panes

### Find / Replace
- [x] ✅ Ctrl+F: opens find bar; Ctrl+H: opens find + replace
- [ ] Typing highlights all matches in both buffers; shows match count
- [ ] Find next (F3 / Enter) / Find prev (Shift+F3): wraps around
- [ ] F3/Shift+F3 works from find entry, replace entry, and source pane
- [x] ✅ Escape closes find bar from source pane (not just from find entry)
- [ ] Replace: replaces current selection if it matches
- [ ] Replace All: replaces in both buffers
- [ ] Close (X / Escape): hides bar, clears highlights, returns focus to pane
- [ ] After closing find bar, Ctrl+F / Ctrl+H work immediately

### Save
- [x] ✅ Ctrl+S: saves focused pane; button goes insensitive
- [x] ✅ Save button insensitive until buffer changes
- [x] ✅ Save writes correct file; button goes insensitive; error dialog on failure
- [ ] Save path tracks swaps: after swap, save writes to correct original file
- [ ] Save after swap: pane contents and labels remain consistent (no un-swap)
- [ ] Save after swap in dir file tab: watcher reload preserves swapped state
- [ ] Ctrl+Shift+S (Save As): file dialog; writes to chosen path; updates pane label and tooltip
- [ ] Save As updates tab-tracked path (close/unsaved dialog shows new name)
- [ ] Ctrl+Shift+L (Save All): saves all dirty panes in current view
- [ ] Ctrl+Shift+O: opens focused file in system default app
- [x] ✅ Ctrl+R / F5 (Refresh): reloads both files from disk
- [ ] Refresh with unsaved changes: confirm dialog before reloading
- [ ] Refresh on blank comparison pane: no-op (doesn't crash)

### File Watcher
- [ ] External file change: auto-reloads if no unsaved changes
- [ ] External change while unsaved: reload skipped
- [ ] Save suppresses reload for 600ms (no reload loop)

### Close / Dialogs
- [ ] Ctrl+W: closes window
- [ ] Close with no unsaved changes: closes immediately
- [ ] Close with unsaved changes: dialog with per-file checkboxes, Save/Close without Saving/Cancel
- [ ] Save failure in dialog: window does NOT close
- [ ] Overwrite confirm dialog: Escape key closes it
- [ ] Ctrl+Shift+P: export patch dialog opens (sourceview doesn't consume it)

### Chunk Copy (Alt+Left / Alt+Right)
- [ ] Alt+Right on a diff chunk: copies left chunk content to right (no file copy dialog)
- [ ] Alt+Left on a diff chunk: copies right chunk content to left (no file copy dialog)
- [ ] Alt+Left/Right with no current chunk: nothing happens
- [ ] In file diff tab opened from dir window: Alt+Left/Right copies chunks, does NOT trigger dir file copy

### Chunk Pull (Alt+Shift+Left / Alt+Shift+Right)
- [ ] Alt+Shift+Right: pulls content from right pane into focused pane
- [ ] Alt+Shift+Left: pulls content from left pane into focused pane
- [ ] Pull when focused on the same side as pull direction: no-op

### Delete Change (Alt+Delete)
- [x] ✅ Alt+Delete: deletes the current chunk's text from the focused pane
- [ ] Alt+Delete with no current chunk: nothing happens
- [ ] Alt+Delete at end of buffer: removes preceding newline too

### Pane Switching (Alt+PageUp / Alt+PageDown)
- [x] ✅ Alt+PageDown: switches focus to next pane (wraps around)
- [x] ✅ Alt+PageUp: switches focus to previous pane (wraps around)
- [ ] In 3-way merge: cycles left→middle→right→left

### Fullscreen (F11)
- [x] ✅ F11: toggles fullscreen mode
- [x] ✅ F11 again: exits fullscreen

### Edge Cases
- [x] ✅ Identical files: "Files are identical" info bar; edit one side -> bar disappears; undo -> reappears
- [x] ✅ Binary files: info bar, panes read-only, patch button disabled, swap/nav/blanks/spaces/undo/redo buttons disabled
- [ ] Empty files: no chunks, label shows "No changes"
- [x] ✅ Empty file vs non-empty file: all-insert chunks on one side
- [ ] Very large files (10K+ lines): background diff, UI stays responsive
- [ ] UTF-8 with multi-byte characters: inline highlighting correct
- [x] ✅ Files with only whitespace differences + Spaces toggle on: no differences shown
- [x] ✅ Files with only blank line differences + Blanks toggle on: no differences shown
- [x] ✅ Swap panes twice: returns to original state

## Navigation

- [x] ✅ Alt+Up/Down: cursor moves to chunk start line
- [ ] Click in pane, then Alt+Down: navigates from cursor position, not from top
- [ ] Navigate chunks with cursor in different positions: cursor-aware seeking
- [ ] Alt+Up/Down intercepted: sourceview move-lines action does not fire
- [ ] Wrap-around navigation: Alt+Down past last chunk wraps to first (if enabled in prefs)
- [x] ✅ Wrap-around off: Alt+Down on last chunk stays; Alt+Up on first chunk stays

## Directory Comparison

### Display
- [x] ✅ Tree view with expandable directories
- [ ] Columns: Name (with icon), Size, Modification time
- [ ] Directory headers above each pane with full path as tooltip
- [ ] Copy path button next to each header: copies full absolute path to clipboard
- [ ] Copy path after swap: copies the swapped (current) path, not the original
- [ ] Status colors: blue (different), orange (left-only), green (right-only), dim italic (missing side)

### Navigation
- [x] ✅ Double-click file row: opens diff in new notebook tab
- [ ] Enter on file row: opens diff in new notebook tab
- [x] ✅ Enter on directory row: expands/collapses folder
- [ ] Enter after closing a file tab: still works (no broken activation state)
- [ ] Left/Right arrow keys: switch focus between panes
- [ ] Selection syncs between panes (highlighted on both sides)
- [ ] Active pane: accent-color border; inactive pane: dim border + reduced opacity
- [ ] Switching panes: no scroll jump, keyboard cursor syncs to selected row

### Toolbar
- [ ] Copy to left (Alt+Left): copies right to left; confirms if overwriting
- [ ] Copy to right (Alt+Right): copies left to right; confirms if overwriting
- [ ] Delete (Delete key): trashes selected file with confirmation
- [x] ✅ Collapse all: collapses every expanded directory row
- [x] ✅ Expand all: expands every directory row
- [ ] Swap panes: rescans, updates headers and window title
- [ ] Preferences (Ctrl+,)

### Context Menu
- [ ] Right-click selects the clicked row first
- [ ] Right-click targets correct row: select row A, right-click row B, action operates on B (not A)
- [ ] "Open Diff": opens diff tab (disabled for directories)
- [ ] "Copy to Left" / "Copy to Right": enabled for appropriate statuses
- [ ] "Delete": enabled only when file exists on focused side
- [ ] "Open Externally" / "Copy File Path": enabled only when file exists on focused side
- [ ] Missing-side file: Copy, Delete, Open Externally, Copy Path all disabled

### Notebook Tabs
- [x] ✅ Directory tab labeled with "dir1 — dir2"
- [ ] File tabs labeled with dir-relative names
- [ ] Opening file diff tab: left pane has focus, cursor at line 1
- [ ] Tab close (X): prompts for unsaved changes
- [ ] Ctrl+W on Directory tab: closes entire window
- [x] ✅ Ctrl+W on file tab: closes that tab
- [x] ✅ Same file not opened twice: switches to existing tab
- [x] ✅ Alt+1..9: switches to tab by index
- [ ] Ctrl+N: opens New Comparison tab
- [ ] New Comparison tab: "Compare Files" and "3-Way Merge" buttons with file choosers
- [ ] New Comparison → Compare Files: opens diff tab, removes new-comparison tab
- [ ] New Comparison → 3-Way Merge: opens merge tab, removes new-comparison tab
- [ ] Blank comparison (Ctrl+T or from New Comparison): opens editable blank diff tab

### File Watcher
- [ ] Directory rescans on FS changes (500ms poll)
- [ ] Expanded directories and selection restored after rescan
- [ ] Open tab buffers reloaded unless unsaved

### Filters
- [ ] Filtered names (.git, node_modules, etc.) excluded from tree
- [ ] Custom filters from preferences applied
- [ ] Hide hidden files on (default): dotfiles excluded from tree
- [ ] Hide hidden files off: dotfiles shown in tree

### Edge Cases
- [ ] Collapse all with missing-side files expanded: no crash
- [ ] Deeply nested directory trees
- [ ] Copy directory recursively: all nested files copied
- [ ] Dir -> file tab -> swap -> edit -> save dialog shows correct filename
- [ ] Window close with unsaved tabs: dialog lists all unsaved files

## 3-Way Merge

### Display
- [x] ✅ Three panes: left (read-only), middle (editable), right (read-only)
- [x] ✅ Left/right save buttons hidden; only middle has save button
- [ ] Left gutter: bands connecting left to middle
- [ ] Right gutter: bands connecting middle to right
- [ ] Conflict backgrounds: red/pink overlays on middle where left/right overlap
- [ ] Chunk maps on both edges

### Toolbar
- [x] ✅ Undo/Redo (Ctrl+Z / Ctrl+Shift+Z): acts on active pane
- [x] ✅ Previous/Next change (Alt+Up/Down): per-pane cursor-based navigation
- [ ] Navigation only places cursor on involved panes (not the third uninvolved pane)
- [ ] Previous/Next conflict (Ctrl+J / Ctrl+K): navigates `<<<<<<<` markers in middle
- [ ] Conflict label: "N conflicts" / "Conflict X of N" / "No conflicts"
- [ ] Go to line (Ctrl+L)
- [x] ✅ Blanks / Spaces toggles
- [ ] Preferences (Ctrl+,)

### Gutter Interactions
- [ ] Only inward arrows: left gutter has right-arrows only, right gutter has left-arrows only
- [ ] Click left gutter arrow: copies left chunk into middle
- [ ] Click right gutter arrow: copies right chunk into middle
- [ ] Right-click context menus: "Copy Left -> Middle" / "Copy Right -> Middle"

### Scroll Sync
- [ ] Horizontal sync across all three panes
- [ ] Vertical scroll synced by chunk across all three panes

### Find / Replace
- [ ] Ctrl+F/H: find bar highlights matches across all three buffers
- [ ] Match count sums all three buffers
- [ ] Replace All replaces only in editable middle buffer (not read-only left/right)

### Save
- [x] ✅ Ctrl+S: saves middle pane
- [ ] Ctrl+Shift+S (Save As): saves middle to a new path; updates pane label
- [ ] Save As updates tab-tracked path (close/unsaved dialog shows new name)
- [ ] Ctrl+Shift+L (Save All): saves middle if dirty
- [ ] Ctrl+Shift+O: opens focused file externally (works for all 3 panes)
- [ ] Ctrl+R / F5 (Refresh): reloads all 3 files from disk
- [ ] Refresh with unsaved middle: confirm dialog before reloading
- [ ] After Save As, watcher reloads from new path (left/right still watch originals)

### Edge Cases
- [ ] Binary files: all panes show info bar; all read-only; middle save hidden
- [ ] All three identical: no changes, no chunks
- [ ] Both sides modify same region: conflict overlay on middle
- [ ] Copy left then copy right to same region: middle updates; undo works
- [ ] Undo/redo across panes: applies to active pane's buffer

## VCS (Git) Window

### Display
- [x] ✅ Window title: "mergers -- reponame (git)"
- [ ] ColumnView: Status, File, and Extra columns
- [ ] Status colors: Modified/Renamed = blue, Added/Untracked = green, Deleted = orange
- [x] ✅ Extra column: "Staged" for fully staged, "Partially staged" for staged+unstaged
- [x] ✅ Conflict files: Extra column empty (not falsely "Staged")
- [x] ✅ Repo path label and changed file count

### Opening Diffs
- [x] ✅ Double-click / Enter: opens diff tab (HEAD vs working copy)
- [ ] Modified/Renamed: HEAD content on left, working file on right
- [ ] Added/Untracked: empty left, working file on right
- [ ] Deleted: HEAD content on left, empty right
- [ ] Already-open tab: switches to existing tab

### Context Menu
- [ ] Right-click selects clicked row
- [ ] "Open Diff": opens diff (disabled for Untracked)
- [ ] "Discard Changes": runs `git checkout --` with confirmation (disabled for Untracked)
- [ ] "Stage": runs `git add` (disabled when already staged)
- [ ] "Stage" on conflicted file: confirmation dialog warning it marks conflict resolved
- [ ] "Unstage": runs `git restore --staged` (enabled only when staged)
- [ ] "Trash": moves to trash with confirmation (Untracked only)

### File Watcher
- [ ] Auto-refresh on FS changes (500ms poll)
- [ ] `.git` directory events ignored
- [ ] Smart update: skips rebuild if status unchanged

### Notebook / Close
- [x] ✅ "Changes" tab first; Ctrl+W on it closes window
- [ ] Ctrl+W on file tab: closes tab with unsaved check
- [ ] Window close: checks all tabs for unsaved changes
- [ ] Temp directory cleaned up on window destroy
- [ ] Alt+1..9: switches to tab by index
- [ ] Ctrl+N: opens New Comparison tab

### Security
- [ ] Paths with `..` components refused

## Preferences

- [ ] Font: chooser dialog; live-apply
- [ ] Color scheme: dropdown; live-apply; case-insensitive matching
- [ ] Show line numbers: toggle; live-apply
- [ ] Highlight current line: toggle; live-apply
- [ ] Word wrap (None/Word/Character): dropdown; live-apply
- [ ] Tab width (1-16): spin button; live-apply
- [ ] Hide hidden files: toggle; default on; saved to settings
- [ ] Wrap-around navigation: toggle; saved to settings
- [ ] File filters: add/remove entries; saved on dialog close
- [ ] Settings persisted to `~/.config/mergers/settings.toml`
- [ ] Changes applied to all views in the parent window
- [ ] Escape closes preferences dialog
- [ ] Close button at bottom closes preferences dialog

## Dark Mode

- [ ] Linux/GNOME: auto-detects system dark mode
- [ ] macOS: falls back to `defaults read -g AppleInterfaceStyle`
- [ ] Sets `gtk_application_prefer_dark_theme` for GTK widgets

## Global Shortcuts

| Shortcut | Action |
|---|---|
| Ctrl+Q | Quit (close all windows) |
| Ctrl+W | Close current tab/window |
| Ctrl+, | Preferences |
| Ctrl+Z | Undo |
| Ctrl+Shift+Z | Redo |
| Alt+Up / Ctrl+E | Previous change |
| Alt+Down / Ctrl+D | Next change |
| Ctrl+J | Previous conflict (merge only) |
| Ctrl+K | Next conflict (merge only) |
| Ctrl+F | Find |
| Ctrl+H (Cmd+Shift+H on macOS) | Find + Replace |
| F3 | Find next |
| Shift+F3 | Find previous |
| Ctrl+L | Go to line |
| Alt+PageUp | Previous pane |
| Alt+PageDown | Next pane |
| Alt+Shift+Left | Pull chunk from left |
| Alt+Shift+Right | Pull chunk from right |
| Alt+Delete | Delete current chunk |
| F11 | Toggle fullscreen |
| Ctrl+Shift+P | Export patch (file diff only) |
| Alt+Left | Copy chunk right→left (file diff) / Copy file to left (dir) |
| Alt+Right | Copy chunk left→right (file diff) / Copy file to right (dir) |
| Ctrl+S | Save active pane |
| Ctrl+Shift+S | Save As (file diff / merge) |
| Ctrl+Shift+L | Save All dirty panes |
| Ctrl+Shift+O | Open focused file externally |
| Ctrl+R / F5 | Refresh (reload from disk) |
| Ctrl+N | New Comparison tab (dir/VCS windows) |
| Alt+1..9 | Switch to tab by index (notebook windows) |
| Delete | Delete selected (dir only) |
| Enter | Open selected file / expand-collapse dir (dir/VCS) |
| Escape | Close find bar / go-to-line / preferences dialog |

## Preferences — Keyboard Shortcuts

- [ ] "Keyboard Shortcuts" button in Preferences: opens shortcuts dialog
- [ ] Dialog lists all shortcuts grouped by category
