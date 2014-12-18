;;; one.el --- "M-x one" to read them all

;; Copyright (C) 2012  Lincoln de Sousa <lincoln@comum.org>
;; Copyright (C) 2014  Chunyang Xu <xuchunyang56@gmail.com>

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
;; This is a simple extention to fetch some news sources, inspired by
;; One App (http://one.hackplan.com/).
;;
;; Currently, the following news sources are supported:
;; 1. Hacker News
;; 2. Zhihu Dailiy
;; 3. V2EX
;; 4. SBBS (Southeast University BBS)
;;
;; Notes:
;; Stay with Emacs (a.k.a. Esc-Meta-Alt-Ctrl-Shift) and have fun!

;;; Code:

(require 'json)
(require 'url)
(eval-when-compile (require 'cl))

(defgroup one nil
  "Read all news in one place"
  :group 'external
  :prefix "one-")

(defface one-link-face
  '((t (:foreground "green")))
  "Face used for links to articles"
  :group 'one)

(defvar hackernews-url "http://api.ihackernews.com/page"
  "The url to grab the list of news from Hacker News")

(defvar zhihu-url "http://news.at.zhihu.com/api/1.2/news/latest"
  "The url to grab the list of news from Zhihu Daily")

(defvar v2ex-url "https://www.v2ex.com/api/topics/hot.json"
  "The url to grab the list of news from V2EX")

(defvar sbbs-url "http://bbs.seu.edu.cn/api/hot/topten.js"
  "The url to grab the list of news from SBBS")

(defvar one-mode-header-line
  '("    "
    (:propertize "n p" face mode-line-buffer-id)
    ": Navigate"
    "    "
    (:propertize "RET" face mode-line-buffer-id)
    ": View question"
    "    "
    (:propertize "v" face mode-line-buffer-id)
    ": Visit externally"
    "    "
    (:propertize "q" face mode-line-buffer-id)
    ": Quit")
  "Header-line used in `one-mode'.")

(defvar one-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'bury-buffer)
    (define-key map "RET" 'open-url-within-emacs)
    (define-key map "v" 'open-url-external)
    ;; Movement
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    map)
  "Keymap for `one-mode'.")

(define-derived-mode one-mode fundamental-mode "dictionary"
  "Major mode to render results.
\\{one-mode-map}.
Turning on Text mode runs the normal hook `one-mode-hook'."
  (setq header-line-format one-mode-header-line)
  (message "one-mode: init"))


;; TODO: Mode (UI and Keymap)

;;; Interactive functions

;;;###autoload
(defun one-entry (news-source url)
  "The entry point of every client"
  (condition-case ex
      (one-format-results
       news-source
       (one-parse (one-retrieve url)))
    ('error
     (message (format "Bad news, bro: %s" (car (cdr ex)))))))

(defun one-sbbs ()
  "The entry point of SBBS client"
  (interactive)
  (one-entry "sbbs" sbbs-url))

(defun one-hackernews ()
  "The entry point of Hacker News client"
  (interactive)
  (one-entry "hackernews" hackernews-url))

(defun one-zhihu ()
  "The entry point of Zhihu Daily client"
  (interactive)
  (one-entry "zhihu" zhihu-url))

(defun one-v2ex ()
  "The entry point of V2EX client"
  (interactive)
  (one-entry "v2ex" v2ex-url))

;;; UI Functions

(defun one-create-link-in-buffer (title url)
  "Insert clickable string inside a buffer"
  (lexical-let ((title title)
                (url url)
                (map (make-sparse-keymap)))
    (define-key map (kbd "<RET>")
      #'(lambda (e) (interactive "p") (browse-url url)))
    (define-key map (kbd "t")    ;FIXME: use 'eww', seems not work yet
      #'(lambda (e) (interactive "p") (eww-browse-url url)))
    (define-key map (kbd "<down-mouse-1>")
      #'(lambda (e) (interactive "p") (browse-url url)))
    (insert
     (propertize
      title
      'face 'one-link-face
      'keymap map
      'mouse-face 'highlight))))

(defun one-space-fill (string n)
  "Makes sure that string is at least n characters long, and
   if it isn't, it adds SPACE-characters to the end"
  (while (< (length string) n)
    (setf string (concat string " ")))
  (identity string))

(defun one-encoding (string)
  "encoding"
  (decode-coding-string
   (encode-coding-string string 'utf-8) 'utf-8))

(defun one-format-results (news-source results)
  "Create the buffer to render all the info"
  (cond ((string= news-source "sbbs")
         (sbbs-format-results results))
        ((string= news-source "hackernews")
         (hackernews-format-results results))
        ((string= news-source "zhihu")
         (zhihu-format-results results))
        ((string= news-source "v2ex")
         (v2ex-format-results results))
        (t (error "news-source not implement yet")))
  (unless (eq major-mode 'one-mode)
    (one-mode)))


;;; SBBS specific

(defun sbbs-render-post (post)
  "Render a single post to the current buffer
Add the post title as a link, and print the points and number of
comments."
  (princ (sbbs-space-fill
          (format "[%s]" (cdr (assoc 'read post))) 6))
  (one-create-link-in-buffer
   (one-encoding (cdr (assoc 'title post)))
   ;; TODO: use nForum
   (format "http://bbs.seu.edu.cn/bbscon.php?board=%s&id=%s"
           (cdr (assoc 'board post))
           (cdr (assoc 'id post))))
  (princ (format " (%s replies)" (cdr (assoc 'replies post))))
  (princ "\n"))

(defun sbbs-format-results (results)
  "Create the buffer to render all the info"
  (with-output-to-temp-buffer "*sbbs*"
    (switch-to-buffer "*sbbs*")
    (setq font-lock-mode nil)
    (use-local-map nil)
    (mapcar #'sbbs-render-post
	    (cdr (assoc 'topics results)))))

;;; Hacker News specific

(defun hackernews-comment-url (id)
  (format "https://news.ycombinator.com/item?id=%s" id))

(defun hackernews-link-of-url (url)
  (lexical-let ((url url)
		(hackernews-item "/comments/"))
    (if (string-prefix-p hackernews-item url)
	(hackernews-comment-url (substring url (length hackernews-item)))
      url)))

(defun hackernews-render-post (post)
  "Render a single post to the current buffer
Add the post title as a link, and print the points and number of
comments."
  (princ (hackernews-space-fill
          (format "[%s]" (cdr (assoc 'points post))) 6))
  (one-create-link-in-buffer
   (one-encoding (cdr (assoc 'title post)))
   (hackernews-link-of-url (one-encoding (cdr (assoc 'url post)))))
  (one-create-link-in-buffer
   (format " (%d comments)" (cdr (assoc 'commentCount post)))
   (hackernews-comment-url (cdr (assoc 'id post))))
  (princ "\n"))

(defun hackernews-format-results (results)
  "Create the buffer to render all the info"
  (with-output-to-temp-buffer "*hackernews*"
    (switch-to-buffer "*hackernews*")
    (setq font-lock-mode nil)
    (use-local-map nil)
    (mapcar #'hackernews-render-post
            (cdr (assoc 'items results)))))

;;; Zhihu Daily specific

(defun zhihu-render-post (post)
  "Render a single post to the current buffer
Add the post title as a link, and print the points and number of
comments."
  (one-create-link-in-buffer
   (one-encoding (cdr (assoc 'title post)))
   (cdr (assoc 'share_url post)))
  (princ "\n"))

(defun zhihu-format-results (results)
  "Create the buffer to render all the info"
  (with-output-to-temp-buffer "*zhihu*"
    (switch-to-buffer "*zhihu*")
    (setq font-lock-mode nil)
    (use-local-map nil)
    (mapcar #'zhihu-render-post
	    (cdr (assoc 'news results)))))

;;; V2EX specific

(defun v2ex-format-results (results)
  "Create the buffer to render all the info"
  (with-output-to-temp-buffer "*v2ex*"
    (switch-to-buffer "*v2ex*")
    (setq font-lock-mode nil)
    (use-local-map v2ex-map)
    (let ((c (length results)))
      (dotimes (n c)
        (let ((item-nth (elt results n)))
          (v2ex-create-link-in-buffer
           (v2ex-encoding (cdr (assoc 'title item-nth)))
           (cdr (assoc 'url item-nth)))
          (princ (format " (%s replies)" (cdr (assoc 'replies item-nth))))
          (princ "\n"))))))

;;; Retrieving and parsing

(defun one-retrieve (url)
  (let (json)
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (when (not (string-match "200 OK" (buffer-string)))
        (error "Problem connecting to the server"))
      (re-search-forward "^$" nil 'move)
      (setq json (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer (current-buffer)))
    json))

(defun one-parse (contents)
  (json-read-from-string contents))

(provide 'one)

;;; one.el ends here
