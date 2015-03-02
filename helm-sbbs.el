;;; helm-sbbs.el --- helm interface of sbbs

;; Copyright (C) 2015 by Chunyang Xu

;; Author: Chunyang Xu
;; URL: https://github.com/xuchunyang/one.el
;; Package-Requires: ((cl-lib "0.5") (helm "1.0"))
;; Keywords: hackernews, zhihu, v2ex, sbbs
;; Version: 0.1

;;; Commentary:

;;; Code:

(require 'cl-lib)

(require 'helm)
(require 'json)
(require 'browse-url)


(defvar helm-sbbs-url "http://bbs.seu.edu.cn/api/hot/topten.js")

(defgroup helm-sbbs nil
  "hacker news with helm interface"
  :group 'sbbs)

(defface helm-sbbs-title
  '((((class color) (background light))
     :foreground "red" :weight semi-bold)
    (((class color) (background dark))
     :foreground "green" :weight semi-bold))
  "face of post title"
  :group 'helm-sbbs)

(defun helm-sbbs-get-posts ()
  (with-temp-buffer
    (unless (zerop (call-process "curl" nil t nil "-s" helm-sbbs-url))
      (error "Failed: 'curl -s %s'" helm-sbbs-url))
    (let* ((json nil)
           (ret (ignore-errors
                  (setq json (json-read-from-string
                              (buffer-substring-no-properties
                               (point-min) (point-max))))
                  t)))
      (unless ret
        (error "Error: Can't get JSON response"))
      json)))

(defun helm-sbbs-init ()
  (let ((json-res (helm-sbbs-get-posts)))
    (sort (cl-loop with posts = (assoc-default 'topics json-res)
                   for post across posts
                   for read = (assoc-default 'read post)
                   for title = (assoc-default 'title post)
                   for replies = (assoc-default 'replies post)
                   for board = (assoc-default 'board post)
                   for id = (assoc-default 'id post)
                   for cand = (format "%s %s (%d comments)"
                                      (format "[%d]" read)
                                      (propertize title 'face 'helm-sbbs-title)
                                      replies)
                   collect
                   (cons cand
                         (list :read read
                               :url (format "http://bbs.seu.edu.cn/bbscon.php?board=%s&id=%s"
                                            board id))))
          'helm-sbbs-sort-predicate)))

(defun helm-sbbs-sort-predicate (a b)
  (let ((read-a (plist-get (cdr a) :read))
        (read-b (plist-get (cdr b) :read)))
    (> read-a read-b)))

(defun helm-sbbs-browse-link (cand)
  (browse-url (plist-get cand :url)))

(defvar helm-sbbs-source
  '((name . "虎踞龙蟠 BBS (aka SBBS)")
    (candidates . helm-sbbs-init)
    (action . (("Browse Link" . helm-sbbs-browse-link)))))

;;;###autoload
(defun helm-sbbs ()
  (interactive)
  (helm :sources '(helm-sbbs-source) :buffer "*helm-sbbs*"))

(provide 'helm-sbbs)

;;; helm-sbbs.el ends here
