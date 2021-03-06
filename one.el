;;; one.el --- "M-x one-*" to read several news sources  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2015 Chunyang Xu <xuchunyang56@gmail.com>

;; Author: Chunyang Xu
;; URL: https://github.com/xuchunyang/one.el
;; Package-Requires: ((emacs "24.1"))
;; Keywords: hackernews, zhihu, v2ex, sbbs
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is a simple extention to view some news sources, inspired by
;; One App (http://one.hackplan.com/).
;;
;; Currently, the following news sources are supported:
;; 1. Hacker News
;; 2. Zhihu Dailiy
;; 3. V2EX
;; 4. SBBS (Southeast University BBS)
;;
;; This package is base on https://github.com/clarete/hackernews.el.
;;
;; Enjoy!

;;; Code:

(require 'json)
(require 'url)

(defgroup one nil
  "Read all news in one place"
  :group 'external
  :prefix "one-")

(defface one-link-face
  '((t
     (:underline
      (:color foreground-color :style line)
      :foreground "#6699cc")))
  "Face used for links to articles"
  :group 'one)

(defvar hackernews-url "http://api.ihackernews.com/page"
  "The url to grab the list of news from Hacker News.")

(defvar zhihu-url "http://news.at.zhihu.com/api/1.2/news/latest"
  "The url to grab the list of news from Zhihu Daily.")

(defvar v2ex-url "https://www.v2ex.com/api/topics/hot.json"
  "The url to grab the list of news from V2EX.")

(defvar sbbs-url "http://bbs.seu.edu.cn/api/hot/topten.js"
  "The url to grab the list of news from SBBS.")

(defconst one-mode-header-line
  '("    "
    (:propertize "n p" face mode-line-buffer-id)
    ": Navigate"
    "    "
    (:propertize "RET" face mode-line-buffer-id)
    ": Visit externally"
    "    "
    (:propertize "t" face mode-line-buffer-id)
    ": Visit"
    "    "
    (:propertize "q" face mode-line-buffer-id)
    ": Quit")
  "Header-line used in `one-mode'.")

(defvar one-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'bury-buffer)
    ;; Movement
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    map)
  "Keymap for `one-mode'.")

(define-derived-mode one-mode nil "one"
  "Major mode to render results.
\\{one-mode-map}.
Turning on Text mode runs the normal hook `one-mode-hook'."
  (hl-line-mode 1)
  (setq header-line-format one-mode-header-line))

;;; Interactive functions

(defun one--entry (url)
  "The entry point of every client."
  (let ((progress-reporter
         (make-progress-reporter (format "Fetching from %s..." url)
                                 nil nil)))
    (condition-case ex
        (one--format-results
         url
         (one--parse (one-－retrieve url)))
      ('error
       (message (format "Bad news, bro: %s" (car (cdr ex))))))
    (progress-reporter-done progress-reporter)))

(defun one--goto-first-item (x-pos y-pos)
  "Move cursor to the first item, denoted by (X-POS, Y-POS)."
  (goto-char (point-min))
  (forward-line (1- y-pos))
  (forward-char x-pos))

(define-derived-mode sbbs-mode tabulated-list-mode "SBBS"
  "Major mode for viewing sbbs news."
  (setq tabulated-list-format [("点击" 4 t)
                               ("标题" 60 nil)
                               ("回复"  4 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "回复" nil))
  (tabulated-list-init-header))

(defvar-local sbbs-json nil
  "SBBS json content.")

(defun assoc-recursive (alist &rest keys)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

(defun make-sbbs-entries ()
  "Make entries for sbbs-mode."
  (let ((index 1) (list nil))
    (mapc (lambda (item)
            (push (list
                   (number-to-string index)
                   `[,(number-to-string (assoc-recursive item 'read))
                     ,(assoc-recursive item 'title)
                     ,(number-to-string (assoc-recursive item 'replies))])
                  list))
          (cdr (assoc 'topics sbbs-json)))
    (setq index (1+ index))
    list))

;;;###autoload
(defun one-sbbs ()
  "The entry point of SBBS client."
  (interactive)
  (pop-to-buffer "*SBBS*" nil)
  (sbbs-mode)
  (setq-local sbbs-json (one--parse (one-－retrieve sbbs-url)))
  (setq tabulated-list-entries #'make-sbbs-entries)
  (tabulated-list-print t))

;;;###autoload
(defun one-hackernews ()
  "The entry point of Hacker News client."
  (interactive)
  (one--entry hackernews-url)
  (one--goto-first-item 6 8))

;;;###autoload
(defun one-zhihu ()
  "The entry point of Zhihu Daily client."
  (interactive)
  (one--entry zhihu-url)
  (one--goto-first-item 3 8))

;;;###autoload
(defun one-v2ex ()
  "The entry point of V2EX client."
  (interactive)
  (one--entry v2ex-url)
  (one--goto-first-item 3 8))

;;; UI Functions

(defun one--create-link-in-buffer (title url)
  "Insert clickable string inside a buffer."
  (let ((title title)
                (url url)
                (map (make-sparse-keymap)))
    (define-key map (kbd "<RET>")
      #'(lambda (e) (interactive "p") (browse-url url)))
    (define-key map (kbd "t")
      #'(lambda (e) (interactive "p") (eww-browse-url url)))
    (define-key map (kbd "<down-mouse-1>")
      #'(lambda (e) (interactive "p") (browse-url url)))
    (insert
     (propertize
      title
      'face 'one-link-face
      'keymap map
      'mouse-face 'highlight))))

(defun one--space-fill (string n)
  "Make sure that `STRING' is at least N characters long.

And if it isn't, it adds SPACE-characters to the end."
  (while (< (length string) n)
    (setf string (concat string " ")))
  (identity string))

(defun one--encoding (string)
  "Encoding."
  (decode-coding-string
   (encode-coding-string string 'utf-8) 'utf-8))

(defun one--format-results (url results)
  "Create the buffer to render all the info."
  (cond ((string= url sbbs-url )
         (sbbs-format-results results))
        ((string= url hackernews-url)
         (hackernews-format-results results))
        ((string= url zhihu-url)
         (zhihu-format-results results))
        ((string= url v2ex-url)
         (v2ex-format-results results))
        (t (error (format "url %s is not implement yet" url))))
  (unless (eq major-mode 'one-mode)
    (one-mode)))


;;; SBBS specific

(defun sbbs-render-post (post)
  "Render a single post to the current buffer
Add the post title as a link, and print the points and number of
comments."
  (princ (one--space-fill
          (format "[%s]" (cdr (assoc 'read post))) 8))
  (one--create-link-in-buffer
   (one--encoding (cdr (assoc 'title post)))
   ;; TODO: use nForum
   (format "http://bbs.seu.edu.cn/bbscon.php?board=%s&id=%s"
           (cdr (assoc 'board post))
           (cdr (assoc 'id post))))
  (princ (format " (%s replies)" (cdr (assoc 'replies post))))
  (princ "\n"))

(defun sbbs-format-results (results)
  "Create the buffer to render all the info."
  (with-output-to-temp-buffer "*sbbs*"
    (switch-to-buffer "*sbbs*")
    (setq font-lock-mode nil)
    (use-local-map nil)
    (one--create-link-in-buffer "SBBS -- 虎踞龙蟠 BBS" "http://bbs.seu.edu.cn" )
    (princ "
 ____  ____  ____ ____
/ ___|| __ )| __ ) ___|
\\___ \\|  _ \\|  _ \\___ \\
 ___) | |_) | |_) |__) |
|____/|____/|____/____/\n\n")
    (mapcar #'sbbs-render-post
            (cdr (assoc 'topics results)))))

;;; Hacker News specific

(defun hackernews-comment-url (id)
  (format "https://news.ycombinator.com/item?id=%s" id))

(defun hackernews-link-of-url (url)
  (let ((url url)
        (hackernews-item "/comments/"))
    (if (string-prefix-p hackernews-item url)
        (hackernews-comment-url (substring url (length hackernews-item)))
      url)))

(defun hackernews-render-post (post)
  "Render a single post to the current buffer
Add the post title as a link, and print the points and number of
comments."
  (princ (one--space-fill
          (format "[%s]" (cdr (assoc 'points post))) 6))
  (one--create-link-in-buffer
   (one--encoding (cdr (assoc 'title post)))
   (hackernews-link-of-url (one--encoding (cdr (assoc 'url post)))))
  (one--create-link-in-buffer
   (format " (%d comments)" (cdr (assoc 'commentCount post)))
   (hackernews-comment-url (cdr (assoc 'id post))))
  (princ "\n"))

(defun hackernews-format-results (results)
  "Create the buffer to render all the info."
  (with-output-to-temp-buffer "*hackernews*"
    (switch-to-buffer "*hackernews*")
    (setq font-lock-mode nil)
    (use-local-map nil)
    (one--create-link-in-buffer "Hacker News" "https://news.ycombinator.com")
    (princ "
 _   _            _               _   _
| | | | __ _  ___| | _____ _ __  | \\ | | _____      _____
| |_| |/ _` |/ __| |/ / _ \\ '__| |  \\| |/ _ \\ \\ /\\ / / __|
|  _  | (_| | (__|   <  __/ |    | |\\  |  __/\\ V  V /\\__ \\
|_| |_|\\__,_|\\___|_|\\_\\___|_|    |_| \\_|\\___| \\_/\\_/ |___/\n\n")
    (mapcar #'hackernews-render-post
            (cdr (assoc 'items results)))))

;;; Zhihu Daily specific

(defun zhihu-render-post (post)
  "Render a single post to the current buffer
Add the post title as a link, and print the points and number of
comments."
  (princ (one--space-fill "¶" 3))
  (one--create-link-in-buffer
   (one--encoding (cdr (assoc 'title post)))
   (cdr (assoc 'share_url post)))
  (princ "\n"))

(defun zhihu-format-results (results)
  "Create the buffer to render all the info."
  (with-output-to-temp-buffer "*zhihu*"
    (switch-to-buffer "*zhihu*")
    (setq font-lock-mode nil)
    (use-local-map nil)
    (one--create-link-in-buffer "ZhiHu Daily -- 知乎日报" "http://daily.zhihu.com")
    (princ "
 ______     _ _   _
|__  / |__ (_) | | |_   _
  / /| '_ \\| | |_| | | | |
 / /_| | | | |  _  | |_| |
/____|_| |_|_|_| |_|\\__,_|\n\n")

    (mapcar #'zhihu-render-post
            (cdr (assoc 'news results)))))

;;; V2EX specific

(defun v2ex-format-results (results)
  "Create the buffer to render all the info."
  (with-output-to-temp-buffer "*v2ex*"
    (switch-to-buffer "*v2ex*")
    (setq font-lock-mode nil)
    (use-local-map nil)
    (one--create-link-in-buffer "V2EX" "http://www.v2ex.com")
    (princ "
__     ______  _______  __
\\ \\   / /___ \\| ____\\ \\/ /
 \\ \\ / /  __) |  _|  \\  /
  \\ V /  / __/| |___ /  \\
   \\_/  |_____|_____/_/\\_\\\n\n")

    (let ((c (length results)))
      (dotimes (n c)
        (let ((item-nth (elt results n)))
          (princ (one--space-fill "¶" 3))
          (one--create-link-in-buffer
           (one--encoding (cdr (assoc 'title item-nth)))
           (cdr (assoc 'url item-nth)))
          (princ (format " (%s replies)" (cdr (assoc 'replies item-nth))))
          (princ "\n"))))))

;;; Retrieving and parsing

(defun one-－retrieve (url)
  (let (json)
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (when (not (string-match "200 OK" (buffer-string)))
        (error "Problem connecting to the server"))
      (re-search-forward "^$" nil 'move)
      (setq json (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer (current-buffer)))
    json))

(defun one--parse (contents)
  (json-read-from-string contents))

(provide 'one)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; one.el ends here
