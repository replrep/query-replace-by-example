;;;; query-replace-by-example.el --- Get replace string by example

;; Copyright (C) 2002-2004 Claus Brunzema <mail@cbrunzema.de>

;; Version: 1.1.2
;; $Id: query-replace-by-example.el,v 1.8 2005/01/03 21:21:17 chb Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;; -----------------------------------------------------------------------


;;; Commentary:

;; Motivation:
;;
;; M-% (query-replace) is often used to replace something wrong with a
;; new version. Unfortunately you often have to type the wrong version
;; into the minibuffer once more to correct it.
;; query-replace-by-example tries to save a few keystrokes by
;; comparing text before and after a change and giving the two
;; versions of the changed region to the standard query-replace. So
;; you give an example of the needed change and
;; query-replace-by-example tries to apply that change to the other
;; occurences of the old version.  You don't have to type the complete
;; old and new version into the minibuffer.

;; Thanks:
;;
;; - Stefan Kamphausen for long-time testing.
;; - Thorsten Bonow for bug reports.

;; Home:
;;
;; http://www.cbrunzema.de/software.html#query-replace-by-example

;; Platforms:
;;
;; query-replace-by-example was developed and tested with:
;; XEmacs 21.4 (patch 15) and
;; GNU Emacs 21.2.1

;; Installation:
;;
;; put this file in your load-path and the following lines in your
;; init file (~/.emacs or ~/.xemacs/init.el):
;;
;;   (autoload 'query-replace-by-example "query-replace-by-example"
;;             "Get replace string by example." t)
;;   (global-set-key (kbd "C-%") 'query-replace-by-example)

;; Usage:
;;
;; See documentation for function `query-replace-by-example'. Or just
;; try it: go to a line, hit C-%, change something, hit C-% or C-c C-c
;; to accept (C-g to cancel) and see what query-replace-by-example
;; thinks about your change.

;; History:
;; 2004-01-03  Claus Brunzema
;;         * Added (require 'overlay) to make it work under xemacs
;;           -vanilla (bug report from Thorsten Bonow).
;;         * Small documentation clarification (bug report from
;;           Thorsten Bonow).
;;         * Version 1.1.2
;; 2004-12-22  Claus Brunzema
;;         * Fixed stupid typo in provide-form
;;         * Version 1.1.1
;; 2004-08-30  Claus Brunzema
;;         * Improved usage message to use `substitute-command-keys'.
;; 2004-08-05  Claus Brunzema
;;         * Removed query-replace-by-example-highlight-face. We use
;;           the standard 'highlight face now.
;;         * Version 1.1.0
;; 2004-08-04  Claus Brunzema
;;         * Dumbed down extents to overlays, so it works in both XEmacs
;;           and GNUEmacs. We only need some basic functionality anyway.
;;         * Changed user interface: use C-c C-c to accept and C-g to
;;           cancel instead of weird line-change post-command-hook
;;           stuff.
;;         * Code cleanup.
;; 2002-06-15  Claus Brunzema
;;         * First real version (1.0.2)

;; ToDo:
;;
;; - provide more functions to use as
;;   query-replace-by-example-current-thing-func, maybe something that
;;   considers anything but [a-zA-Z] as a thing boundary.
;; - add some hooks.
;; - maybe change the longish 'query-replace-by-example'-prefix to 'qrbe-'


;;; Code:
(require 'cl)
(require 'overlay)


;; -- Customisation -------------------------------------------------------
(defvar query-replace-by-example-current-thing-func #'current-word
  "*Function to get a thing at the current position.
See documentation of function `query-replace-by-example' for details.")


;; -- Global internal variables -------------------------------------------
(defvar query-replace-by-example-state nil)
(defvar query-replace-by-example-before-string "")
(defvar query-replace-by-example-overlay nil)
(defvar query-replace-by-example-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-c") #'query-replace-by-example-end)
    (define-key keymap (kbd "C-g") #'query-replace-by-example-cleanup)
    keymap)
  "The keymap used in the query-by-replace highlighted region.")


;; -- Functions -----------------------------------------------------------
(defun query-replace-by-example-diff (string1 string2)
  "Compare STRING1 with STRING2.
Return differing portions of the strings and the starting position (in
a list). Return nil if the strings are equal."
  (let* ((stringlist1 (string-to-list string1))
         (stringlist2 (string-to-list string2))
         (start (mismatch stringlist1 stringlist2))
         (end (mismatch (reverse stringlist1) (reverse stringlist2))))
    (if start
        (list (concat (nthcdr start (butlast stringlist1 end)))
              (concat (nthcdr start (butlast stringlist2 end)))
              start)
      nil)))

(defun query-replace-by-example-get-thing (string pos)
  "Look for thing in STRING around POS."
  (save-excursion
    (with-temp-buffer
      (insert string)
      (goto-char pos)
      (funcall query-replace-by-example-current-thing-func))))
  
(defun query-replace-by-example-highlight-region (start end)
  "Highlight the region between START and END.
Return the overlay created."
  (let ((overlay (make-overlay start end (current-buffer) nil t)))
    (overlay-put overlay 'keymap query-replace-by-example-keymap)
    (overlay-put overlay 'face 'highlight)
    overlay))

(defun query-replace-by-example-cleanup ()
  "Clean up overlay and state."
  (interactive)
  (when query-replace-by-example-overlay
    (delete-overlay query-replace-by-example-overlay)
    (setq query-replace-by-example-overlay nil))
  (setq query-replace-by-example-before-string "")
  (setq query-replace-by-example-state nil))

(defun query-replace-by-example-start ()
  "Begin a query and replace by example session.
Highlight the region and set `query-replace-by-example-before-string'."
  (cond
   ((if (fboundp 'region-exists-p)
        (region-exists-p)                     ;XEmacs
      (and transient-mark-mode mark-active))  ;GNUEmacs
    (setq query-replace-by-example-state 'region)
    (setq query-replace-by-example-overlay
          (query-replace-by-example-highlight-region
           (region-beginning)
           (region-end)))
    (setq query-replace-by-example-before-string
          (buffer-substring (region-beginning) (region-end)))
    (if (fboundp 'zmacs-deactivate-region)
        (zmacs-deactivate-region)             ;XEmacs
      (deactivate-mark)))                     ;GNUEmacs
   ((= (point-at-bol) (point-at-eol))
    (message "query-replace-by-example is not useful on an empty line."))
   (t
    (setq query-replace-by-example-state 'line)
    (message
     "query-repace-by-example (press %s or %s to accept, %s to abort)"
     (key-description (this-command-keys))
     (substitute-command-keys
      (concat
      "\\<query-replace-by-example-keymap>"
      "\\[query-replace-by-example-end]"))
     (substitute-command-keys
      (concat
       "\\<query-replace-by-example-keymap>"
       "\\[query-replace-by-example-cleanup]")))
    (setq query-replace-by-example-overlay
          (query-replace-by-example-highlight-region (point-at-bol)
                                                     (point-at-eol)))
    (setq query-replace-by-example-before-string
          (buffer-substring (point-at-bol)
                            (point-at-eol))))))

(defun query-replace-by-example-end ()
  "End a query and replace by example session.
Spot the change and call the standard `query-replace' mechanism."
  (interactive)
  (let ((search "")
        (replace ""))
    (cond
     ((eq 'region query-replace-by-example-state)
      (setq search query-replace-by-example-before-string)
      (setq replace (buffer-substring
                     (overlay-start query-replace-by-example-overlay)
                     (overlay-end query-replace-by-example-overlay))))
     ((eq 'line query-replace-by-example-state)
      (let* ((after (buffer-substring (point-at-bol) (point-at-eol)))
             (ret-list (query-replace-by-example-diff
                        query-replace-by-example-before-string
                        after)))
        (if (null ret-list)
            (message "query-replace-by-example: no difference found.")
          (let ((pos (third ret-list)))
            (setq search (first ret-list))
            (setq replace (second ret-list))
            (when (zerop (length search))
              (setq search (query-replace-by-example-get-thing
                            query-replace-by-example-before-string
                            pos))
              (setq replace (query-replace-by-example-get-thing
                             after
                             pos))))))))
    (query-replace-by-example-cleanup)
    (when (and search replace
               (not (string-equal search replace)))
      (perform-replace search replace t nil nil))))


;;;###autoload
(defun query-replace-by-example ()
  "Get replace string by example.
`query-replace-by-example' uses two strategies to get the search and
replace strings.

If there is a region active when `query-replace-by-example' is called
for the first time, the content of the region is used as the search
string. After accepting the changes (by calling
`query-replace-by-example' again or by pressing
\\<query-replace-by-example-keymap>\\[query-replace-by-example-end])
inside the highlighted region, the changed content of the region is
used as the replace string for `query-replace'.

Without an active region the current line becomes the area of interest
and is highlighted after the first call to
`query-replace-by-example'. After accepting the changes, the
content of the line is compared to the old content. The two strings
are compared from the beginning and from the end. The portions in the
middle where the two strings differ will be the search and replace
strings for `query-replace'.

If the search string is empty (e.g. if you just added something in the
highlighted line), `query-replace-by-example' tries to get the search and
replace strings via the function in
`query-replace-by-example-current-thing-func' (default:
`current-word'). It tries to get the thing at the position of
the change.

For details about the interactive replacement of the search string
with the new string, see documentation for `query-replace'.

A query and replace by example session can be aborted by pressing
\\<query-replace-by-example-keymap>\\[query-replace-by-example-cleanup]
inside the highlighted region.

If you don't like the keybindings in the highlighted region, you can
change them in `query-replace-by-example-keymap'."
  (interactive)
  (if query-replace-by-example-state
      (query-replace-by-example-end)
    (query-replace-by-example-start)))

(provide 'query-replace-by-example)

;;; query-replace-by-example.el ends here
