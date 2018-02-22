;;; emms-bilibili-favlist.el --- The favlist interface of EMMS Bilibili.

;;; Commentary:



;;; Code:

(require 'emms-bilibili-request)


(defvar emms-bilibili--favfolder-alist nil
  "A favlist list with spec.

((fid . FID) (name . NAME) (count . COUNT) (data . ORIGINAL_DATA))
")

(defun emms-bilibili--generate-favfolder-url ()
  "Generate user favourite list folder URL."
  (format "https://api.bilibili.com/x/v2/fav/folder?vmid=%s&type=json" emms-bilibili-mid))

(defun emms-bilibili--retrieve-favfolder ()
  "Retrieve Bilibili user's all favlist to EMMS."
  (url-retrieve-synchronously
   (emms-bilibili--generate-favfolder-url)
   (lambda (status)
     (let ((res (current-buffer))
           (json-array-type 'list))
       (with-current-buffer res
         (emms-bilibili-response-remove-headers)
         (let* ((json-raw (json-read-from-string
                           (decode-coding-string (buffer-string) 'utf-8)))
                (data (alist-get 'data json-raw)))
           ;; reset
           (setq emms-bilibili--favfolder-alist nil)
           ;; construct favlist list to spec `emms-bilibili--favfolder-alist'.
           (mapc
            (lambda (favlist)
              (let ((fid (alist-get 'fid favlist))
                    (name (alist-get 'name favlist))
                    (count (alist-get 'cur_count favlist))
                    fav-list)
                (push `(name . ,name) fav-list)
                (push `(fid . ,fid) fav-list)
                (push `(count . ,count) fav-list)
                (push `(data . ,favlist) fav-list)
                ;; add to favfolder list.
                (push fav-list emms-bilibili--favfolder-alist)
                ))
            data)
           )))))
  emms-bilibili--favfolder-alist)

(cl-defun emms-bilibili--generate-favlist-url (vmid &optional fid (page 1) (pagesize 30))
  "Generate user `VMID' favourite bookmark `FID' list URL.
If `FID' not specified, then retrieve Default Bookmark.
Optional argument `PAGE', and `PAGESIZE'."
  (format
   "https://api.bilibili.com/x/v2/fav/video?vmid=%s%s&pn=%s&ps=%s&order=fav_time"
   vmid (if fid (format "&fid=%s" fid) "") page pagesize))

(cl-defun emms-bilibili--retrieve-favlist (&optional fid (page 1))
  "Retrieve Bilibili favlist to EMMS."
  (if (equal page nil)
      nil
    (url-retrieve
     (emms-bilibili--generate-favlist-url emms-bilibili-mid fid page)
     (lambda (status)
       (let ((res (current-buffer))
             (json-array-type 'list))
         (with-current-buffer res
           (emms-bilibili-response-remove-headers)
           (let* ((json-raw (json-read-from-string
                             (decode-coding-string (buffer-string) 'utf-8)))
                  (pagecount (alist-get 'pagecount (alist-get 'data json-raw)))
                  (current-page (alist-get 'page (alist-get 'data json-raw)))
                  (data (assoc 'data json-raw)))
             (when (= current-page 1)
               (setq emms-bilibili-playlist-alist nil))
             (setq emms-bilibili-playlist-alist
                   (append emms-bilibili-playlist-alist (alist-get 'archives data)))
             (message "EMMS Bilibili bookmark page [%d/%d]" current-page pagecount)
             (if (= pagecount current-page)
                 (run-hooks 'emms-bilibili-response-received-hook)
               (emms-bilibili--retrieve-favlist (+ current-page 1))))))))))

(defun emms-bilibili-favlist-select ()
  "Interactive select which favlist to open."
  (let* ((fav-name (completing-read "Select favlist: "
                                    (mapcar
                                     (lambda (x) (alist-get 'name x))
                                     (emms-bilibili--retrieve-favfolder))))
         (favlist (seq-find
                   (lambda (x) (string= fav-name (alist-get 'name x)))
                   emms-bilibili--favfolder-alist)))
    (emms-bilibili--retrieve-favlist (alist-get 'fid favlist))))



(provide 'emms-bilibili-favlist)

;;; emms-bilibili-favlist.el ends here
