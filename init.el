;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."

  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   dotspacemacs-configuration-layers
   '(javascript

     ;; Enable asciidoc layer for editing asciidoc content
     ;; Useful for docs.cider.mx editing
     asciidoc

     ;; Add tool tips to show doc string of functions
     ;; Show snippets in the autocompletion popup
     ;; Show suggestions by most commonly used
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t)
     ;; To have auto-completion on as soon as you start typing
     ;; (auto-completion :variables auto-completion-idle-delay nil)

     ;; Enable clj-refactor tools
     (clojure :variables
              clojure-toplevel-inside-comment-form t
              clojure-enable-linters 'clj-kondo)

     ;; SPC a L displays key and command history in a separate buffer
     command-log

     ;; Nyan cat tells you where you are in your file
     (colors :variables
             colors-enable-nyan-cat-progress-bar (display-graphic-p))

     ;; https://develop.spacemacs.org/layers/+lang/csv/README.html
     csv

     emacs-lisp

     emoji

     ;; SPC g s opens Magit git client full screen (q restores previous layout)
     ;; refine hunk 'all highlights characters changed on each line
     (git :variables
          git-magit-status-fullscreen t
          magit-diff-refine-hunk 'all)

     ;; SPC g h to use GitHub repositories
     ;; SPC g g to use GitHub Gists
     github

     ;; graphviz - open-source graph declaration system
     ;; Used to generated graphs of Clojure project dependencies
     ;; https://develop.spacemacs.org/layers/+lang/graphviz/README.html
     ;; graphviz

     ;; GNU Global is a source code tagging system
     ;; It queries symbol locations in source code, such as definitions or references
     ;; `sudo apt install ctags` for Clojure support
     ;; https://develop.spacemacs.org/layers/+tags/gtags/README.html
     ;; (gtags :variables
     ;;        gtags-enable-by-default t)

     helm
     ;; (helm :variables
     ;;       helm-enable-auto-resize t
     ;;       helm-position 'top  ; top, bottom, left, right
     ;;       helm-use-frame-when-more-than-two-windows nil)

     html
     javascript
     json

     ;; Clojure specific configuration in dotspacemacs/user-config
     ;; lsp

     markdown

     ;; Editing multiple lines of text concurrently
     ;; `g r' menu in Emacs normal state
     multiple-cursors

     ;; Customise the Spacemacs themes
     ;; https://develop.spacemacs.org/layers/+themes/theming/README.html
     ;; See example code in dotspacemacs/user-init
     ;; theming

     ;; Spacemacs Org mode
     (org :variables
          org-enable-github-support t
          org-enable-bootstrap-support t
          org-enable-reveal-js-support t
          org-want-todo-bindings t
          org-enable-org-journal-support t
          org-journal-dir "~/src/org-files/"
          org-journal-file-format "%Y-%m-%d"
          org-journal-date-prefix "#+TITLE: "
          org-journal-date-format "%A, %B %d %Y"
          org-journal-time-prefix "* "
          org-journal-time-format ""
          org-journal-carryover-items "TODO=\"TODO\"|TODO=\"DOING\"|TODO=\"BLOCKED\"|TODO=\"REVIEW\"")

     ;; Text-based file manager with preview
     ;; SPC a r
     (ranger :variables
             ranger-show-preview t
             ranger-show-hidden t
             ranger-cleanup-eagerly t
             ranger-cleanup-on-disable t
             ranger-ignored-extensions '("mkv" "flv" "iso" "mp4"))

     ;; SPC ' runs eshell in a popup buffer
     ;; To run your terminal shell, add
     ;; shell-default-shell 'multi-term
     (shell :variables
            shell-default-shell 'eshell
            shell-default-height 30
            shell-default-position 'bottom)

     ;; spacemacs-layouts layer added to set variables
     ;; SPC TAB restricted to current layout buffers
     ;; Kill buffers when killing layer - SPC l x
     (spacemacs-layouts :variables
                        spacemacs-layouts-restrict-spc-tab t
                        persp-autokill-buffer-on-remove 'kill-weak)

     ;; Spell as you type with Flyspell package,
     ;; requires external command - ispell, hunspell, aspell
     ;; SPC S menu, SPC S s to check current word
     spell-checking

     ;; Use original flycheck fringe bitmaps
     (syntax-checking :variables
                      syntax-checking-use-original-bitmaps t)

     ;; Visual file manager - `SPC p t'
     ;; treemacs-no-png-images t removes file and directory icons
     (treemacs :variables
               treemacs-indentation 1
               treemacs-use-filewatch-mode t
               treemacs-use-follow-mode t)

     ;; Highlight changes in buffers
     ;; SPC g . transient state for navigating changes
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)


     ) ;; End of dotspacemacs-configuration-layers


   ;; List of additional packages that will be installed without being wrapped in a layer.
   dotspacemacs-additional-packages '(general ox-jira)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization:
  This function is called at the very beginning of Spacemacs startup, before layer configuration.
  It should only modify the values of Spacemacs settings."

  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default

   dotspacemacs-enable-emacs-pdumper nil
   dotspacemacs-emacs-pdumper-executable-file "emacs"
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   dotspacemacs-use-spacelpa nil
   dotspacemacs-verify-spacelpa-archives t
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs

   dotspacemacs-startup-banner 'official

   dotspacemacs-startup-lists '((recents . 10)
                                (todos . 10)
                                (projects . 10)
                                (agenda . 10)
                                (bookmarks . 10))

   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; nil: to use Fundamental mode.
   dotspacemacs-new-empty-buffer-major-mode 'org-mode

   dotspacemacs-scratch-mode 'org-mode
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)

   dotspacemacs-themes '(doom-sourcerer
                         doom-solarized-dark
                         kaolin-valley-dark
                         spacemacs-dark)

   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)
   dotspacemacs-colorize-cursor-according-to-state t

   dotspacemacs-default-font '("Ubuntu Mono"
                               :size 24.0
                               :weight normal
                               :width normal)

   ;; The leader key
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Global"

   ;; If non-nil the default layout name is displayed in the mode-line.
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon start. (default nil)
   dotspacemacs-auto-resume-layouts t

   dotspacemacs-auto-generate-layout-names nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-enable-paste-transient-state t
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-undecorated-at-startup nil
   dotspacemacs-active-transparency 100
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers '(:visual t
                               :disabled-for-modes dired-mode
                                                   doc-view-mode
                                                   pdf-view-mode
                               :size-limit-kb 1000)

   ;; Code folding method. Possible values are `evil' and `origami'. (default 'evil)
   dotspacemacs-folding-method 'origami
   dotspacemacs-smartparens-strict-mode t
   dotspacemacs-smart-closing-parenthesis t
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-enable-server t
   dotspacemacs-server-socket-dir nil
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S: %a"
   dotspacemacs-icon-title-format nil
   dotspacemacs-whitespace-cleanup 'all
   dotspacemacs-zone-out-when-idle nil
   dotspacemacs-pretty-docs t))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  ;; custom theme modification
  ;; - overriding default height of modeline
  ;; (setq-default
  ;;   theming-modifications
  ;;     '((spacemacs-light
  ;;         (mode-line :height 0.92)
  ;;         (mode-line-inactive :height 0.92))
  ;;       (doom-solarized-light
  ;;        (mode-line :height 0.92)
  ;;        (mode-line-inactive :height 0.92))))

  )  ;; End of dotspacemacs/user-int

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; User key bindings
  ;;
  ;; org-journal user keybinding
  ;; - create a new journal entry
  (spacemacs/set-leader-keys "oj" 'org-journal-new-entry)
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Over-ride Spacemacs defaults
  ;;
  ;;
  (delete-selection-mode 1)
  (fset 'yes-or-no-p 'y-or-n-p)
  ;;
  ;;
  ;; Make helm-follow-mode sticky
  ;; scrolling through a helm list, helm-follow will preview the item.
  ;; When C-c C-f is used with a command, helm remembers
  (setq helm-follow-mode-persistent t)
  ;;
  ;;
  ;; Set new location for file bookmarks, SPC f b
  ;; Default: ~/.emacs.d/.cache/bookmarks
  (setq bookmark-default-file "~/.spacemacs.d/bookmarks")
  ;;
  ;;
  ;; Set new location for recent save files
  ;; Default: ~/.emacs.d/.cache/recentf
  (setq bookmark-default-file "~/.spacemacs.d/recentf")
  ;;
  ;;
  ;; native line numbers taking up lots of space?
  (setq-default display-line-numbers-width nil)
  ;;
  ;;
  ;; replace / search with helm-swoop in Evil normal state
  ;; (evil-global-set-key 'normal "/" 'helm-swoop)
  ;;
  ;;
  ;; Open ranger with the minus keybinding - not working
  ;; Currently opens with deer
  ;; (setq ranger-enter-with-minus t)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Magit - forge configuration
  ;;
  ;; Set the files that are searched for writing tokens
  ;; by default ~/.authinfo will be used
  ;; and write a token in unencrypted format
  (setq auth-sources '("~/.authinfo.gpg"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Version Control configuration - Git, etc
  ;;
  ;; diff-hl - diff hightlights in right gutter as you type
  (diff-hl-flydiff-mode)
  ;;
  ;; Load in magithub features after magit package has loaded
  ;; (use-package magithub
  ;;   :after magit
  ;;   :config (magithub-feature-autoinject t))
  ;;
  ;; Use Spacemacs as the $EDITOR (or $GIT_EDITOR) for git commits messages
  ;; when using git commit on the command line
  (global-git-commit-mode t)
  ;;
  ;; Set locations of all your Git repositories
  ;; with a number to define how many sub-directories to search
  ;; `SPC g L' - list all Git repositories in the defined paths,
  (setq magit-repository-directories
        '(("~/.emacs.d"  . 0)
          ("~/src/" . 2)
          ("~/git" . 2)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Org-mode configuration
  ;;
  ;; I should write a toggle function to show descriptive or literate links in Org-mode
  ;;(setq org-descriptive-links nil)
  ;;
  ;; Org-reveal - define were reveal.js files can be found
  ;; (I place reveal.js files in same directory as I write the org files)
  (setq org-reveal-root "")
  ;;
  ;; Define the location of the file to hold tasks
  (with-eval-after-load 'org
    (setq org-default-notes-file "~/Dropbox/todo-list.org"))
  ;;
  ;; Define a kanban style set of stages for todo tasks
  (with-eval-after-load 'org
    (setq org-todo-keywords
         '((sequence "TODO" "DOING" "BLOCKED" "REVIEW" "|" "DONE" "ARCHIVED"))))
  ;;
  ;; The default keywords all use the same colour.
  ;; Make the states easier to distinguish by using different colours
  ;; Using X11 colour names from: https://en.wikipedia.org/wiki/Web_colors
  ;; Setting colours (faces) using the `org-todo-keyword-faces' defcustom function
  ;; https://github.com/tkf/org-mode/blob/master/lisp/org-faces.el#L376
  ;; Using `with-eval-after-load' as a hook to call this setting when org-mode is run
  ;;
  (with-eval-after-load 'org
    (setq org-todo-keyword-faces
          '(("TODO" . "SlateGray")
            ("DOING" . "DarkOrchid")
            ("BLOCKED" . "Firebrick")
            ("REVIEW" . "Teal")
            ("DONE" . "ForestGreen")
            ("ARCHIVED" .  "SlateBlue"))))
  ;;
  ;;
  ;; Set TODO keyword faces if over-ridden by theme.
  (defun practicalli/set-todo-keyword-faces ()
    (interactive)
    (setq hl-todo-keyword-faces
          '(("TODO" . "SlateGray")
            ("DOING" . "DarkOrchid")
            ("BLOCKED" . "Firebrick")
            ("REVIEW" . "Teal")
            ("DONE" . "ForestGreen")
            ("ARCHIVED" .  "SlateBlue"))))
  ;;
  ;;
  ;; Progress Logging
  ;; When a TODO item enters DONE, add a CLOSED: property with current date-time stamp
  (with-eval-after-load 'org
    (setq org-log-done 'time))
  ;;
  ;;
  ;; customize org-mode's checkboxes with unicode symbols
  (add-hook
   'org-mode-hook
   (lambda ()
     "Beautify Org Checkbox Symbol"
     (push '("[ ]" . "☐") prettify-symbols-alist)
     (push '("[X]" . "☑" ) prettify-symbols-alist)
     (push '("[-]" . "❍" ) prettify-symbols-alist)
     (prettify-symbols-mode)))
  ;;
  ;; Markdown mode hook for orgtbl-mode minor mode
  (add-hook 'markdown-mode-hook 'turn-on-orgtbl)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Clojure configurations
  ;;
  ;;
  ;; CIDER 0.23 Lima release options
  ;; Configure the position of evaluation result
  ;; By default the result displays at the end of the current line
  ;; Set cider-result-overlay-position to `at-point' to display results right after the expression evaluated
  ;; Useful for evaluating nexsted expressions with `, e e'
  (setq cider-result-overlay-position 'at-point)
  ;;
  ;;
  ;; Pretty print in Clojure to use the Fast Idiomatic Pretty-Printer. This is approximately 5-10x faster than clojure.core/pprint
  (setq cider-pprint-fn 'fipp)
  ;;
  ;;
  ;; Indentation of function forms
  ;; https://github.com/clojure-emacs/clojure-mode#indentation-of-function-forms
  (setq clojure-indent-style 'align-arguments)
  ;;
  ;; Vertically align s-expressions
  ;; https://github.com/clojure-emacs/clojure-mode#vertical-alignment
  (setq clojure-align-forms-automatically t)
  ;;
  ;; Auto-indent code automatically
  ;; https://emacsredux.com/blog/2016/02/07/auto-indent-your-code-with-aggressive-indent-mode/
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Web-mode configuration
  ;;
  ;; Changing auto indent size for languages in html layer (web mode) to 2 (defaults to 4)
  (defun web-mode-indent-2-hook ()
    "Indent settings for languages in Web mode, markup=html, css=css, code=javascript/php/etc."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset  2)
    (setq web-mode-code-indent-offset 2))
  ;;
  (add-hook 'web-mode-hook  'web-mode-indent-2-hook)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Eshell visual enhancements
  ;;
  ;; Add git status visual labels

  (require 'dash)
  (require 's)

  (defmacro with-face (STR &rest PROPS)
    "Return STR propertized with PROPS."
    `(propertize ,STR 'face (list ,@PROPS)))

  (defmacro esh-section (NAME ICON FORM &rest PROPS)
    "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
    `(setq ,NAME
           (lambda () (when ,FORM
                        (-> ,ICON
                            (concat esh-section-delim ,FORM)
                            (with-face ,@PROPS))))))

  (defun esh-acc (acc x)
    "Accumulator for evaluating and concatenating esh-sections."
    (--if-let (funcall x)
        (if (s-blank? acc)
            it
          (concat acc esh-sep it))
      acc))

  (defun esh-prompt-func ()
    "Build `eshell-prompt-function'"
    (concat esh-header
            (-reduce-from 'esh-acc "" eshell-funcs)
            "\n"
            eshell-prompt-string))


  ;; Looking for unicode icons on Emacs
  ;; `list-character-sets' and select unicode-bmp
  ;; scroll through bitmaps list to find the one you want
  ;; some bitmaps seem to change

  ;; "\x26A5 "  (female-male symbol)
  ;; "\xf394"   (non-binary)
  ;; "\xf105"     (docker - changes)
  ;; "\xf105"   (leiningen - changes)
  ;; "\xe919"   (clojure logo - ??)
  ;; "\xf104"   (clojurescript logo - changes)
  ;; "\xf09b"   (github octocat)
  ;; "\xf397"  (git branch)
  ;; "\xf126"    (was git fork, changes..)
  ;; "\xf1d3"  ;  (git icon - changes)
  ;; "\xf5b0"   (git merge)
  ;; "\xf07b" 
  ;; "\xf114"   (closed folder - changes)
  ;; "\xf115"   (open folder - changes)
  ;; "\xf074" 
  ;; "\xe97c" 
  ;; "\xe943"  
  ;; "\xe566"  
  ;; "\xe422"  
  ;; "\xe907"  ; 
  ;; "\xe91b"  ;  
  ;; "\xf126"    (was git fork, changes..)
  ;; "\xf1d3"  ;  (git icon - changes)


  (esh-section esh-dir
               "\xf07c"  ;  (faicon folder)
               (abbreviate-file-name (eshell/pwd))
               '(:foreground "olive" :bold bold :underline t))

  (esh-section esh-git
               "\xf397"  ;  (git branch icon)
               (magit-get-current-branch)
               '(:foreground "maroon"))

  ;; (esh-section esh-python
  ;;              "\xe928"  ;  (python icon)
  ;;              pyvenv-virtual-env-name)

  (esh-section esh-clock
               ""  ;  (clock icon)
               (format-time-string "%H:%M" (current-time))
               '(:foreground "forest green"))

  ;; Below I implement a "prompt number" section
  (setq esh-prompt-num 0)
  (add-hook 'eshell-exit-hook (lambda () (setq esh-prompt-num 0)))
  (advice-add 'eshell-send-input :before
              (lambda (&rest args) (setq esh-prompt-num (incf esh-prompt-num))))


  ;; "\xf0c9"  ;  (list icon)
  (esh-section esh-num
               "\x2130"  ;  ℰ (eshell icon)
               (number-to-string esh-prompt-num)
               '(:foreground "brown"))

  ;; Separator between esh-sections
  (setq esh-sep " ")  ; or " | "

  ;; Separator between an esh-section icon and form
  (setq esh-section-delim "")

  ;; Eshell prompt header
  (setq esh-header "\n ")  ; or "\n┌─"

  ;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
  ;; your login, these can be the same.

  ;;(setq eshell-prompt-regexp " \x2130 ")   ; or "└─> "
  ;;(setq eshell-prompt-string " \x2130 ")   ; or "└─> "
  (setq eshell-prompt-regexp "└─> ")
  (setq eshell-prompt-string "└─> ")


  ;; Choose which eshell-funcs to enable
  ;; (setq eshell-funcs (list esh-dir esh-git esh-python esh-clock esh-num))
  ;; (setq eshell-funcs (list esh-dir esh-git esh-clock esh-num))
  (setq eshell-funcs (list esh-dir esh-git))

  ;; Enable the new eshell prompt
  (setq eshell-prompt-function 'esh-prompt-func)

  ;; Use zsh for default multi-term shell
  (setq multi-term-program "/usr/bin/zsh")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; My Personal Configuration
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;; Upload to Netsuite:
  (defun upload-to-netsuite ()
    "Send buffer to Netsuite."
    (interactive)
    (let ((cmd (concat "ns-upload" " " (buffer-file-name))))
      (message (shell-command-to-string cmd))
      ))
  ;; Upload to Netsuite ends here

  ;; JIRA replace:
  (defun replace-jira ()
    (interactive)
    (move-beginning-of-line 1)
    (replace-string "~" "" )
    (move-beginning-of-line 1)
    (replace-string "{anchor}" "_" ))
  ;; JIRA replace: ends here

  ;; Duplicate line:
  (defun duplicate-line ()
    (interactive)
    (let* ((cursor-column (current-column)))
      (move-beginning-of-line 1)
      (kill-line)
      (yank)
      (open-line 1)
      (next-line 1)
      (yank)
      (move-to-column cursor-column)))
  ;; Duplicate line: ends here

 ;; Keybindings & Hooks
  (general-define-key
   "C-a" 'mark-whole-buffer
   "<C-s-down>" 'duplicate-line
   "C-c d" 'define-word-at-point
   "C-c u" 'upload-to-netsuite
   "C-c C-x j" 'cider-jack-in
   )
  ;; Keybindings & Hooks:1 ends here

  )   ;; End of dot-spacemacs/user-config


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f"))))
 '(package-selected-packages
   (quote
    (tide typescript-mode tern nodejs-repl livid-mode skewer-mode js2-refactor multiple-cursors js2-mode js-doc import-js grizzl helm-gtags ggtags lsp-mode dash-functional ivy add-node-modules-path yasnippet-snippets xterm-color ws-butler writeroom-mode visual-fill-column winum web-mode web-beautify vterm volatile-highlights vi-tilde-fringe uuidgen treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil treemacs pfuture toc-org terminal-here tagedit symon symbol-overlay string-inflection spaceline-all-the-icons all-the-icons memoize spaceline powerline smeargle slim-mode shell-pop scss-mode sass-mode restart-emacs ranger rainbow-mode rainbow-identifiers rainbow-delimiters pug-mode prettier-js popwin persp-mode password-generator paradox ox-twbs ox-gfm overseer orgit org-re-reveal org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-journal org-download org-cliplink org-bullets org-brain open-junk-file nameless multi-term move-text mmm-mode markdown-toc magit-svn magit-section magit-gitflow magit-popup macrostep lorem-ipsum link-hint kaolin-themes autothemer json-navigator hierarchy json-mode json-snatcher json-reformat indent-guide impatient-mode simple-httpd hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-xref helm-themes helm-swoop helm-purpose window-purpose imenu-list helm-projectile helm-org-rifle helm-org helm-mode-manager helm-make helm-ls-git helm-gitignore request helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haml-mode graphviz-dot-mode google-translate golden-ratio gnuplot gitignore-templates gitignore-mode github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gist gh marshal logito pcache gh-md fuzzy forge markdown-mode ghub closql emacsql-sqlite emacsql treepy flyspell-correct-helm flyspell-correct flycheck-pos-tip flycheck-package package-lint let-alist flycheck-elsa flycheck-clj-kondo flycheck flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit git-commit with-editor transient evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens smartparens evil-args evil-anzu anzu eshell-z eshell-prompt-extras esh-help emr iedit clang-format projectile paredit list-utils emojify ht emoji-cheat-sheet-plus emmet-mode elisp-slime-nav editorconfig dumb-jump doom-themes diff-hl devdocs define-word csv-mode company-web web-completion-data company-statistics company-quickhelp pos-tip company-emoji company command-log-mode column-enforce-mode color-identifiers-mode clojure-snippets clean-aindent-mode cider-eval-sexp-fu eval-sexp-fu cider sesman spinner queue pkg-info parseedn clojure-mode parseclj a epl centered-cursor-mode browse-at-remote f dash s auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auto-compile packed aggressive-indent adoc-mode markup-faces ace-window ace-link ace-jump-helm-line helm avy helm-core ac-ispell auto-complete popup which-key use-package pcre2el org-plus-contrib hydra lv hybrid-mode font-lock+ evil goto-chg undo-tree dotenv-mode diminish bind-map bind-key async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
