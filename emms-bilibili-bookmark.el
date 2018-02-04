;;; emms-bilibili-bookmark.el --- The bookmarks function of EMMS Bilibili.

;;; Commentary:



;;; Code:

(require 'emms-bilibili-request)

(defvar emms-bilibili-playlist-alist nil
  "Video info list.")

(cl-defun emms-bilibili-generate--bookmark-url (vmid &optional (page 1) (pagesize 30))
  "Generate bookmark URL."
  (format
   "https://api.bilibili.com/x/v2/fav/video?vmid=%s&pn=%s&ps=%s&order=fav_time"
   vmid page pagesize))

(cl-defun emms-bilibili-bookmark-sync (&optional (page 1))
  "Sync Bilibili playlist to EMMS."
  (if (equal page nil)
      nil
    (url-retrieve
     (emms-bilibili-generate--bookmark-url emms-bilibili-mid page)
     (lambda (status)
       (let ((res (current-buffer))
             (json-array-type 'list))
         (with-current-buffer res
           (emms-bilibili-response-remove-headers)
           (let* ((json-raw (json-read-from-string
                             (decode-coding-string (buffer-string) 'utf-8)))
                  (pagecount (alist-get 'pagecount (alist-get 'data json-raw)))
                  (current-page (alist-get 'page (alist-get 'data json-raw)))
                  (data (assoc 'data json-raw))
                  )
             (when (= current-page 1)
               (setq emms-bilibili-playlist-alist nil))
             (setq emms-bilibili-playlist-alist
                   (append emms-bilibili-playlist-alist (alist-get 'archives data)))
             (message "EMMS Bilibili bookmark page [%d/%d]" current-page pagecount)
             (if (= pagecount current-page)
                 (run-hooks 'emms-bilibili-response-received-hook)
               (emms-bilibili-bookmark-sync (+ current-page 1))))))))))



(provide 'emms-bilibili-bookmark)

;;; emms-bilibili-bookmark.el ends here
