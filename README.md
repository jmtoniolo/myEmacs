# myEmacs

One `init.el` and one `.tmux.conf`, shared between native Windows Emacs and
Emacs / tmux inside WSL. The files in this repo are the only copies; the
per-OS config locations just point here.

```
C:\Users\JohnM\myEmacs\               <- this repo (source of truth)
├── init.el                           <- shared Emacs config
├── .tmux.conf                        <- shared tmux config
├── .gitignore / .gitattributes       <- LF line endings, ignore custom.el & binaries
└── README.md

%APPDATA%\.emacs.d\init.el            <- Windows: 1-line stub that loads ..\myEmacs\init.el
~/.emacs.d/init.el                    <- WSL:     symlink -> /mnt/c/Users/JohnM/myEmacs/init.el
~/.tmux.conf                          <- WSL:     symlink -> /mnt/c/Users/JohnM/myEmacs/.tmux.conf
```

`.emacs.d` on each machine stays Emacs's runtime directory (`elpa/`,
`eln-cache/`, `custom.el`, session state) and is **not** tracked.

## Why a stub / symlink instead of putting the repo in `.emacs.d`

* Emacs does not search for `myEmacs`; on Windows it only looks in
  `%HOME%\.emacs.d\init.el` (if `HOME` is set), then `%APPDATA%\.emacs.d\init.el`.
  The stub lets it find the file where it always has.
* Windows symlinks need Developer Mode or admin, a `(load ...)` stub does not.
* WSL keeps `elpa/` and `eln-cache/` on its native filesystem rather than on
  the slow `/mnt/c` bridge -- only the two text files cross over.
* `init.el` sets `custom-file` to `~/.emacs.d/custom.el`, so anything Emacs's
  Custom UI writes stays per-machine and never lands in the shared file.

## Setup on a new machine

### 1. Clone

```powershell
git clone https://github.com/jmtoniolo/myEmacs C:\Users\JohnM\myEmacs
```

If the username or path differs, update it in the stub (step 2), the
symlinks (step 3) and the `wsl-home` / notes paths at the bottom of `init.el`.

### 2. Windows Emacs

Back up any existing init and write the stub:

```powershell
$d = "$env:APPDATA\.emacs.d"
New-Item -ItemType Directory -Force $d | Out-Null
if (Test-Path "$d\init.el") { Move-Item "$d\init.el" "$d\init.el.pre-shared" }
@'
;; Stub: the real config is shared with WSL and lives in the myEmacs git repo.
(load "c:/Users/JohnM/myEmacs/init.el")
'@ | Out-File -Encoding ascii "$d\init.el"
```

Cascadia Mono ships with Windows 11 / Windows Terminal, so the font just works.

### 3. WSL (Ubuntu)

```bash
cd ~
mkdir -p .emacs.d
[ -f .emacs.d/init.el ] && mv .emacs.d/init.el .emacs.d/init.el.pre-shared
[ -f .tmux.conf ]       && mv .tmux.conf       .tmux.conf.pre-shared
ln -s /mnt/c/Users/JohnM/myEmacs/init.el    .emacs.d/init.el
ln -s /mnt/c/Users/JohnM/myEmacs/.tmux.conf .tmux.conf

sudo apt install fonts-cascadia-code   # Cascadia Mono for GUI Emacs in WSL
```

### 4. First launch

Start Emacs on each side once; `use-package :ensure t` installs the packages
into that machine's `~/.emacs.d/elpa/`. Delete the `*.pre-shared` backups
when you are happy.

## Day-to-day

* Edit `init.el` / `.tmux.conf` **in the repo** (or through the stub/symlink --
  same file). Commit and push from `C:\Users\JohnM\myEmacs`.
* `git pull` on the other machine; nothing else to copy.
* Anything OS-specific goes in the `(when jmt/windows-p ...)` /
  `(when jmt/linux-p ...)` blocks near the bottom of `init.el`.
* If a machine's Emacs writes Custom settings, they go to
  `~/.emacs.d/custom.el` (git-ignored) and are loaded after the shared file.

## Keys worth remembering

| Key | Action |
|---|---|
| `F9` | cycle line display: truncate -> visual-line (word wrap) -> plain wrap |
| `F10` | toggle `whitespace-mode` |
| `F12` | toggle minimap |
| `C-;` | comment / uncomment region (`C-c ;` in a terminal, which can't send `C-;`) |
| `C-s` | swiper |
| `M-m` | expand-region (also the tmux prefix) |
| `C-c C-/` | copy buffer file path to clipboard |
| `C-c C-f` | open containing folder |
| `M-p` / `M-o` | origami close / open fold |
