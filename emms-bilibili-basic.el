;;; emms-bilibili-basic.el --- provide basic API for other emms-bilibili files.

;;; Commentary:



;;; Code:

(require 'emms)
(require 'emms-browser)
(require 'emms-source-playlist)

(defgroup emms-bilibili nil
  "Play your Bilibili favourite videos in EMMS."
  :prefix "emms-bilibili-"
  :group 'emms)


(defun emms-bilibili-insert-track (element)
  "Create track and insert `ELEMENT' into emms-playlist."
  (let* ((title (alist-get 'title element))
         (artist (alist-get 'name (alist-get 'owner element)))
         (duration (alist-get 'duration element))
         (url (emms-bilibili-generate--video-url (alist-get 'aid element)))
         (state (alist-get 'state element))
         (track (emms-track 'url url)))
    (when (>= state 0)
      (emms-track-set track 'info-title title)
      (emms-track-set track 'info-artist artist)
      (emms-track-set track 'info-playing-time duration)
      (with-current-emms-playlist
        (emms-playlist-insert-track track)))))

(defvar emms-bilibili-playlist-alist nil
  "An alist storing `emms-bilibili' playlist.")

(defun emms-bilibili-sync-playlist (entry)
  "Dispatch Bilibili playlist to collect entries to the EMMS Playlist buffer.

Prompt for a Bilibili entry like bookmark to sync playlist."
  (cl-case (intern entry)
    ('bookmark
     (emms-bilibili-bookmark-sync))
    ('user-space
     ;; TODO:
     (message (format "%s currently is not supported by emms-bilibili yet.") entry)
     )
    ('bangumi
     ;; TODO:
     (message (format "%s currently is not supported by emms-bilibili yet.") entry)
     )))

;; insert tracks into EMMS Playlist after response received.
(defvar emms-bilibili-response-received-hook nil)
(add-hook 'emms-bilibili-response-received-hook
          (lambda () (mapcar 'emms-bilibili-insert-track emms-bilibili-playlist-alist)))
(add-hook 'emms-bilibili-response-received-hook
          (lambda () (message "EMMS Bilibili fetch playlist done.")))



(provide 'emms-bilibili-basic)

;;; emms-bilibili-basic.el ends here
