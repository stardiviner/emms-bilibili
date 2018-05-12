;;; emms-bilibili-request.el --- Bilibili request wrapper.

;;; Commentary:



;;; Code:

(require 'url)
(require 'json)
(require 'emms-bilibili-basic)

;; ssl magic
;; (setq tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"))

(defcustom emms-bilibili-mid nil
  "User mid."
  :type 'number
  :group 'emms-bilibili)

(defun emms-bilibili--get-mid ()
  "Prompt user for mid."
  (if (null emms-bilibili-mid)
      (progn
        (browse-url "https://space.bilibili.com/")
        (setq emms-bilibili-mid (read-from-minibuffer "Input your Bilibili user mid number: ")))
    (message "You Bilibili user mid number is `%s.'" emms-bilibili-mid)))

(defun emms-bilibili-generate--video-url (aid)
  "Generate video URL from `AID'."
  (format "https://www.bilibili.com/video/av%d/" aid))

(defun emms-bilibili-generate--bangumi-url (bangumi)
  "Generate Bangumi URL from `BANGUMI'."
  (format "https://bangumi.bilibili.com/anime/%d/" bangumi))

(defun emms-bilibili-response-remove-headers ()
  "Delete header from response buffer."
  (goto-char (point-min))
  (re-search-forward "^$")
  (delete-region (point) (point-min))
  (delete-blank-lines))

(defun emms-bilibili-response--json-parse (response)
  "Parse the `RESPONSE' to get JSON object data."
  (let ((response-buffer (current-buffer))
        (json-array-type 'list))
    (with-current-buffer response-buffer
      (emms-bilibili-response-remove-headers)
      (let* ((json-raw (json-read-from-string
                        (decode-coding-string (buffer-string) 'utf-8)))
             (pagecount (alist-get 'pagecount (alist-get 'data json-raw)))
             (current-page (alist-get 'page (alist-get 'data json-raw)))
             (data (assoc 'data json-raw))
             )
        data))))



(provide 'emms-bilibili-request)

;;; emms-bilibili-request.el ends here
