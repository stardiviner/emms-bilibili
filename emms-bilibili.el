;;; emms-bilibili.el --- Play Bilibili in EMMS.

;; Authors: Tristan <huangtc@outlook.com>, stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "25") (cl-lib "0.5") (magit-popup "2.4.0") (emms "4.4") (emms-player-mpv "0.0.12"))
;; Package-Version: 0.1
;; Keywords: emms bilibili
;; homepage: https://github.com/stardiviner/emms-bilibili

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; QUickstart:

;; 1) Add your directory to load path
;; (add-to-load-path "~/vcs/emms-bilibili")
;;
;; 2) import emms-bilibill
;; (require 'emms-bilibili)
;;
;; 3) run it
;; [M-x emms-bilibili]

;;; Code:

(require 'cl-lib)
(require 'emms-mark)
(require 'emms-bilibili-favlist)
(require 'emms-bilibili-download)


;;;###autoload
(defun emms-bilibili (entry)
  "Start emms-bilibili.

If the current buffer is an EMMS playlist buffer, make it the
main EMMS playlist buffer."
  (interactive (list (completing-read "Select Bilibili entry: "
                                      '("favlist" "bangumi"))))
  (when (and emms-playlist-buffer-p
             (not (eq (current-buffer) emms-playlist-buffer)))
    (emms-playlist-set-playlist-buffer (current-buffer)))
  (when (null emms-bilibili-mid)
    (emms-bilibili--get-mid))
  (with-current-emms-playlist
    (emms-playlist-clear)
    (emms-bilibili-sync-playlist entry))
  ;; auto open *EMMS Playlist* buffer.
  (emms))



(provide 'emms-bilibili)

;;; emms-bilibili.el ends here
