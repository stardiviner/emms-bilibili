;;; emms-bilibili-download.el --- Download support for EMMS Bilibili

;;; Commentary:



;;; Code:

(require 'magit-popup)
(require 'youtube-dl)

(defcustom emms-bilibili-use-popup nil
  "Whether use `magit-popup' like key interface by default."
  :type 'boolean
  :group 'emms-bilibili)

(defcustom emms-bilibili-downloader 'emms-bilibili-downloader--youtube-dl-el
  "Specify `emms-bilibili' track downloader."
  :type '(choice
          :tag "An option to set emms-bilibili downloader."
          (const :tag "youtube-dl-el" emms-bilibili-downloader--youtube-dl-el)
          (const :tag "youtube-dl" emms-bilibili-downloader--youtube-dl)
          (const :tag "aria2c" emms-bilibili-downloader--aria2c)
          (const :tag "aria2c-rpc" emms-bilibili-downloader--aria2c-rpc)
          (function :tag "user custom defined downloader function"))
  :group 'emms-bilibili)

;;; TODO: path compatible with MacOS, Windows.
(defcustom emms-bilibili-download-directory "~/Downloads/"
  "The default directory for `emms-bilibili' downloading videos."
  :type 'string
  :group 'emms-bilibili)


;;;###autoload
(eval-after-load "emms-bilibili"
  '(progn
     (if emms-bilibili-use-popup
         (define-key emms-mark-mode-map (kbd "d") #'emms-bilibili-download-dispatch-popup)
       (unless (boundp 'emms-bilibili-download-prefix)
         (define-prefix-command 'emms-bilibili-download-prefix))
       (add-hook 'emms-mark-mode
                 (lambda ()
                   (local-set-key (kbd "d") 'emms-bilibili-download-prefix)
                   (define-key emms-bilibili-download-prefix (kbd "d") 'emms-bilibili-download--with-youtube-dl-el)
                   (define-key emms-bilibili-download-prefix (kbd "y") 'emms-bilibili-download--with-youtube-dl)
                   ))
       )
     ))

;;; Support marked tracks actions.
;;;###autoload (autoload 'magithub-dispatch-popup "magithub" nil t)
(magit-define-popup emms-bilibili-download-dispatch-popup
  "Popup console for dispatching EMMS Bilibili download options."
  'emms-bilibili-downloaders
  :actions '("Downloaders"
             (?d "youtube-dl-el" emms-bilibili-download--with-youtube-dl-el)
             (?y "youtube-dl" emms-bilibili-download--with-youtube-dl)))


(defun emms-bilibili-download--dispatcher (downloader)
  "EMMS Bilibili `DOWNLOADER' dispatcher."
  (emms-bilibili-download--marked-tracks downloader))

(defun emms-bilibili-download--with-youtube-dl-el ()
  "Download tracks with youtube-dl-el."
  (emms-bilibili-download--dispatcher 'emms-bilibili-downloader--youtube-dl-el))

(defun emms-bilibili-download--with-youtube-dl ()
  "Download tracks with youtube-dl."
  (emms-bilibili-download--dispatcher 'emms-bilibili-downloader--youtube-dl))

(defun emms-bilibili-download--marked-tracks (downloader)
  "Download all marked tracks with `DOWNLOADER'."
  (let ((tracks (emms-mark-mapcar-marked-track 'emms-playlist-track-at t)))
    (setq emms-bilibili-downloader downloader)
    (if (null tracks)
        (message "No track marked!")
      (mapc
       (lambda (track) (emms-bilibili-download--track track emms-bilibili-downloader))
       tracks))))

(defun emms-bilibili-download--track (track downloader)
  "Download the tracks at point, or `TRACK' with `DOWNLOADER'."
  (if (null (emms-playlist-track-at))
      (message "No tracks at point!")
    ;; `downloader' passed in is a symbol.
    (funcall downloader track)))

(defun emms-bilibili-downloader--youtube-dl-el (track)
  "Download `TRACK' with `youtube-dl-el'."
  (let ((track-url (emms-track-name track))
        (default-directory (expand-file-name emms-bilibili-download-directory)))
    (if (null track-url)
        (message "Track URL property does not exist!")
      (message (format "EMMS Bilibili start downloading..."))
      ;; create directory for every vid to handle big FLV video split case.
      ;; use title as directory name.
      (mkdir (expand-file-name (emms-track-get track 'info-title)) t)
      (let ((default-directory (expand-file-name (emms-track-get track 'info-title))))
        (youtube-dl track-url
                    :directory default-directory)))))

(defun emms-bilibili-downloader--youtube-dl (track)
  "Download `TRACK' with `youtube-dl'."
  (let ((track-url (emms-track-name track))
        (default-directory (expand-file-name emms-bilibili-download-directory)))
    (if (null track-url)
        (message "Track URL property does not exist!")
      (message (format "EMMS Bilibili start downloading..."))
      ;; create directory for every vid to handle big FLV video split case.
      ;; use title as directory name.
      (mkdir (expand-file-name (emms-track-get track 'info-title)) t)
      (let ((default-directory (expand-file-name (emms-track-get track 'info-title))))
        (make-process
         :command (list "youtube-dl" "--no-progress" track-url)
         :sentinel (lambda (proc event)
                     (message (format "> proc: %s\n> event: %s" proc event))
                     (if (string= event "finished\n")
                         (progn
                           (message "EMMS Bilibili download *DONE* !!!")
                           (kill-buffer (process-buffer proc))
                           ;; (kill-process proc)
                           )
                       (warn (format "%s *FAILED* XXX" proc))
                       ))
         :name (format "emms-bilibili download %s" track-url)
         :buffer (format "*emms-bilibili download %s*" track-url)
         ;; :stderr (format "*emms-bilibili download %s error*" track-url)
         )
        ))))

(defun emms-bilibili-downloader--aria2c (track)
  "Download `TRACK' with `aria2c'."
  )

(defun emms-bilibili-downloader--aria2c-rpc (track &rest args)
  "Download `TRACK' with `aria2c-rpc'."
  )



(provide 'emms-bilibili-download)

;;; emms-bilibili-download.el ends here
