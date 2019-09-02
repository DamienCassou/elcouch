;;; elcouch.el --- View and manipulate CouchDB databases  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Url: https://gitlab.petton.fr/DamienCassou/elcouch
;; Package-requires: ((emacs "25.1") (json-mode "1.0.0") (libelcouch "0.9.0") (navigel "0.3.0"))
;; Version: 0.3.0
;; Keywords: data, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; View and manipulate CouchDB databases.

;;; Code:

(require 'tabulated-list)
(require 'json-mode)
(require 'navigel)

(require 'libelcouch)


;;; Customization

(defgroup elcouch nil
  "View and manipulate CouchDB databases."
  :group 'externa)


;;; Entity listing code

;;;###autoload
(defun elcouch-open (instance)
  "Open a new buffer listing CouchDB databases of INSTANCE.
Interactively, the user is asked to select a CouchDB instance from
`elcouch-couchdb-instances'."
  (interactive (list (libelcouch-choose-instance)))
  (elcouch-open-entity instance))

;;;###autoload
(defun elcouch-open-url (url)
  "Open entity pointed to by URL, a string."
  (interactive (list (read-from-minibuffer "URL: ")))
  (elcouch-open-entity (libelcouch-entity-from-url url)))

(defun elcouch-open-entity (entity)
  "Open a buffer showing ENTITY."
  (let ((navigel-app 'elcouch))
    (navigel-open entity nil)))


;;; Document view mode

(defvar-local elcouch--document nil
  "Remember the CouchDB document of current buffer.")

(defvar elcouch-document-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'elcouch-document-save)
    (define-key map (kbd "C-x C-q") #'elcouch-document-read-only-mode)
    map)
  "Keybindings for `elcouch-document-view-mode'.")

(define-derived-mode elcouch-document-view-mode json-mode "elcouch document"
  "Major mode to view and edit a CouchDB document.")

(defun elcouch--document-prepare-buffer (content)
  "Insert json CONTENT into current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert content)
    (json-pretty-print-buffer))
  ;; Give more keybindings to buffer navigation:
  (setq buffer-read-only t)
  (goto-char (point-min))
  (font-lock-ensure))

(defun elcouch-view-document (document &optional saved-point saved-mark)
  "Display a CouchDB DOCUMENT in a JSON read-write buffer.
If SAVED-POINT and/or SAVED-MARK are provided, the point and/or
mark are changed to those ones."
  (interactive (list (tabulated-list-get-id)))
  (libelcouch-document-content
   document
   (lambda (json-document)
     (with-current-buffer (get-buffer-create (format "*elcouch-doc-%s" (libelcouch-entity-full-name document)))
       (elcouch-document-view-mode)
       (setq-local elcouch--document document)
       (elcouch--document-prepare-buffer json-document)
       (switch-to-buffer (current-buffer))
       (when saved-point
         (goto-char saved-point))
       (when saved-mark
         (save-mark-and-excursion--restore saved-mark))
       (message "Press %s to edit the document."
                (substitute-command-keys "\\[elcouch-document-read-only-mode]"))))))

(defun elcouch-document-refresh (&optional buffer)
  "Refresh BUFFER with new document content.
Use current buffer if BUFFER is nil."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let ((saved-point (point))
          (saved-mark (save-mark-and-excursion--save)))
      (elcouch-view-document elcouch--document saved-point saved-mark))))

(defun elcouch-document-save ()
  "Save buffer's document to CouchDB."
  (interactive)
  (libelcouch-document-save elcouch--document nil #'elcouch-document-refresh))

(defun elcouch-document-read-only-mode ()
  "Toggle read-only mode in current buffer."
  (interactive)
  (call-interactively #'read-only-mode)
  (if buffer-read-only
      (message "Press %s to edit the document."
               (substitute-command-keys "\\[elcouch-document-read-only-mode]"))
    (message "You can now edit the document. Press %s to send changes to the server."
             (substitute-command-keys "\\[elcouch-document-save]"))))

(defun elcouch-document-delete (document)
  "Delete the CouchDB DOCUMENT."
  (interactive (list elcouch--document))
  (when (yes-or-no-p (format "Really delete %s? " (libelcouch-entity-full-name document)))
    (let* ((json-object (save-excursion
                          (goto-char (point-min))
                          (json-read)))
           (revision (map-elt json-object '_rev)))
      (libelcouch-document-delete
       document
       revision
       (lambda () (elcouch-open-entity (libelcouch-entity-parent document)))))))


;; navigel configuration

(navigel-method elcouch navigel-name (entity)
  (libelcouch-entity-name entity))

(navigel-method elcouch navigel-buffer-name (entity)
  (libelcouch-entity-full-name entity))

(navigel-method elcouch navigel-children (entity callback)
  (libelcouch-entity-list entity callback))

(navigel-method elcouch navigel-parent (entity)
  (libelcouch-entity-parent entity))

(navigel-method elcouch navigel-parent ((_entity libelcouch-instance))
  nil)

(navigel-method elcouch navigel-open ((document libelcouch-document) _target)
  (elcouch-view-document document))

(navigel-method elcouch navigel-delete ((document libelcouch-document) &optional function)
  (libelcouch-document-delete-latest document function))

(provide 'elcouch)
;;; elcouch.el ends here
