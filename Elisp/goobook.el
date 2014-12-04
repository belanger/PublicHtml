;;; goobook.el

;; Copyright 2014 Jay Belanger, Julien Danjou

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Changes.
;;  6 Nov 2014:  I'll start listing any changes I make.
;;
;;; Commentary:
;;
;; This requires goobook (https://pypi.python.org/pypi/goobook/)
;; to be installed and set up.
;;
;; Commands to start:
;;
;;   M-x goobook-search           Create a buffer of names whose records
;;                                   match a search string
;;   M-x goobook-information      Create a buffer with information about
;;                                   a contact (tab completion)
;;   M-x goobook-reload           Reload the goobook cache
;;
;;   M-x goobook-email-from-name  Prompt for a contact's name,
;;                                send an email to that contact.
;;
;;
;; The command
;;   M-x goobook-search
;; will bring up the results buffer, if it exists, otherwise will prompt
;; for a string and create a buffer of names of contacts whose information
;; matches the string.  An empty string will bring up all contacts.
;; In that buffer:
;;    RET will display information from Google Contacts about
;;        the person named on the current line.
;;    n   will move to the next line and display information about
;;        that person.
;;    p   will move to the previous line and display information about
;;        that person.
;;    m   will send an email to the person named on the current line.
;;    s   will do another search.
;;    R   will refresh the information.
;;    q   will bury the buffer.
;;
;; In the buffer containing information about a contact:
;;    m   will send an email to that person.
;;    n   will display information about the next person on the list.
;;        (The list will be the names list, if it exists, or the search list.)
;;    p   will display information about the previous person on the list.
;;    R   will refresh the information.
;;    q   will delete the window and bury the names buffer.
;;
;;
;; In message mode, TAB completion will use the Google Contacts list to complete
;; and email address.
;;
;; In gnus, the ";" key will create an information buffer for the person who sent
;; the email.  The ":" key will add the current author to the contact list.
;;
;;
;;; Code:

;;; The buffers

(defvar goobook-information-buffer-name "*Google Contact information*")

(defvar goobook-results-buffer-name "*Google Contacts results*")

(defvar goobook-search-string "")

;;; The list
(defvar goobook-namelist nil)

;;; Utility functions
(defun goobook-remove-whitespace (str)
  (replace-regexp-in-string "^ *" "" (replace-regexp-in-string "[ \n]*$" "" str)))

(defun goobook-name-order (name1 name2)
  "Put NAME1 and NAME2 in alphabetical order, by last names."
  (let ((namelist1 (nreverse (split-string (car name1))))
        (namelist2 (nreverse (split-string (car name2)))))
    (or (string< (nth 0 namelist1) (nth 0 namelist2))
        (and (string= (nth 0 namelist1) (nth 0 namelist2))
             (string< (nth 1 namelist1) (nth 1 namelist2))))))

(defun goobook-reload ()
  "Reload the goobook cache."
  (interactive)
  (shell-command-to-string "goobook reload"))

;;; Create the lists
(defun goobook-get-namelist (&optional refresh)
  "Return `goobook-namelist', a list of names and email addresses from Google Contacts.
Create it, if necessary, and update it if REFRESH is non-nil."
  (when refresh
    (setq goobook-namelist nil)
    (goobook-reload))
  (or goobook-namelist
      (with-temp-buffer
        ;;(shell-command "goobook query '' | cut -f 2,1" t nil)
        (shell-command "goobook query '' | gawk -F '\t' '/.+/ {print $2 \" \" $1}'"
                       t nil)
        (goto-char (point-min))
        (forward-line 1)
        (while (and
                (not (looking-at "Starred in Android"))
                (not (= (point) (point-max))))
          (looking-at "^\\(.*\\) \\([^ ]*@[^ ]*\\)$")
          (setq goobook-namelist 
                (cons (list 
                       (goobook-remove-whitespace (match-string 1))
                       (goobook-remove-whitespace (match-string 2)))
                      goobook-namelist))
          (forward-line 1))
        (setq goobook-namelist (sort goobook-namelist 'goobook-name-order)))))

(defun goobook-get-searchlist (str)
  "Create a list of names from Google contacts who match the query STR."
  (let ((searchlist nil))
    (with-temp-buffer
      (shell-command
       (concat "goobook query '" str "' | gawk -F '\t' '/.+/ {print $2 \" \" $1}'")
       t nil)
      (goto-char (point-min))
      (forward-line 1)
      (while (and
              (not (looking-at "Starred in Android"))
              (not (= (point) (point-max))))
        (looking-at "^\\(.*\\) \\([^ ]*@[^ ]*\\)$")
        (setq searchlist
              (cons (list (goobook-remove-whitespace (match-string 1))) searchlist))
        (forward-line 1)))
    (sort searchlist 'goobook-name-order)))

(defun goobook-email-from-name (&optional name)
  "Send an email to NAME."
  (interactive)
  (unless name
    (setq name (completing-read "Name: " (goobook-get-namelist))))
  (let* ((emails
          (shell-command-to-string (concat "goobook query " "\"" name "\" | cut -f 1")))
         (email-list (split-string emails)))
    (if email-list
        (if (= (length email-list) 1)
            (message-mail (concat name " <" (car email-list) ">"))
          (let ((str "")
                (num 0)
                (elist email-list)
                (selected nil)
                eaddress)
            (while elist
              (setq str (concat str (format "(%d): %s " num (car elist))))
              (setq num (1+ num))
              (setq elist (cdr elist)))
            (setq num (1- num))
            (setq selected (read-minibuffer (concat "Which email address? " str)))
            (while (not
                    (and
                     (integerp selected)
                     (<= 0 selected num)))
              (setq selected
                    (read-minibuffer
                     (concat "Invalid response.  Which email address? " str))))
            (message-mail (concat name " <" (nth selected email-list) ">"))))
      (message (concat "No email address for " name ".")))))

;;; Working with the list of contacts
(defun goobook-results-quit ()
  (interactive)
  (let ((win (get-buffer-window goobook-information-buffer-name)))
    (if win
        (delete-window win)))
  (bury-buffer))

(defun goobook-results-buffer-current-name-info ()
  "Create an information buffer for the contact name on the current line."
  (interactive)
  (let ((name (buffer-substring (line-beginning-position) (line-end-position))))
    (save-excursion
      (goobook-information name))))

(defun goobook-results-next ()
  "Move to the next line and create an information buffer."
  (interactive)
  (if (= (forward-line 1) 0)
      (goobook-results-buffer-current-name-info)))

(defun goobook-results-previous ()
  "Move to the previous line and create an information buffer."
  (interactive)
  (if (= (forward-line -1) 0)
      (goobook-results-buffer-current-name-info)))

(defun goobook-results-email ()
  "Send an email to the contact on the current line."
  (interactive)
  (let ((name (buffer-substring (line-beginning-position) (line-end-position))))
    (goobook-email-from-name name)))

;;; Working with the results of a search
(defun goobook-search (&optional string)
  "Prompt for a search string, and return buffer with the matching names."
  (interactive)
  (let ((rbuf (get-buffer goobook-results-buffer-name)))
    (if (and rbuf (not (eq (current-buffer) rbuf)))
        (switch-to-buffer goobook-results-buffer-name)
      (let* ((str (or string (read-string "Search string: ")))
             (searchlist (goobook-get-searchlist str)))
        (setq goobook-search-string str)
        (with-current-buffer (get-buffer-create goobook-results-buffer-name)
          (let ((inhibit-read-only t)
                (prev-name nil)
                (first t))
            (erase-buffer)
            (while searchlist
              (unless (string= (caar searchlist) prev-name)
                (if first
                    (insert (caar searchlist))
                  (insert "\n" (caar searchlist))))
              (setq first nil)
              (setq prev-name (caar searchlist))
              (setq searchlist (cdr searchlist)))
            (goto-char (point-min))
            (goobook-results-mode)
            (switch-to-buffer goobook-results-buffer-name)))))))

(defun goobook-results-refresh ()
  (interactive)
  (let ((lineno (line-number-at-pos)))
    (goobook-reload)
    (goobook-search goobook-search-string)
    (goto-line lineno)))

(defvar goobook-results-keymap nil
  "The keymap for the goobook search results.")

(if goobook-results-keymap
    nil
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'goobook-results-next)
    (define-key map "p" 'goobook-results-previous)
    (define-key map "m" 'goobook-results-email)
    (define-key map "q" 'goobook-results-quit)
    (define-key map "s" 'goobook-search)
    (define-key map "R" 'goobook-search-refresh)    
    (define-key map "\C-m" 'goobook-results-buffer-current-name-info)
    (setq goobook-search-keymap map)))

(defun goobook-results-mode ()
  "Mode for displaying Google Contacts search results.
In this buffer:
\\[goobook-results-buffer-current-name-info]
  Display information from Google Contacts about
  the person named on the current line.
\\[goobook-results-next]
  Move to the next line and display information about that person.
\\[goobook-results-previous]
  Move to the previous line and display information about that person.
\\[goobook-results-email]
  Send an email to the person named on the current line.
\\[goobook-search]
  Do another search.
\\[goobook-search-refresh]
  Refresh the information.
\\[goobook-results-quit]
  Bury the buffer.
"
  (kill-all-local-variables)
  (use-local-map goobook-search-keymap)
  (setq mode-name "Google contacts results")
  (setq major-mode 'goobook-results-mode)
  (setq buffer-read-only t))

;;; Working with individual information
(defun goobook-information (&optional name)
  "Create a buffer with the Google Contact information for NAME."
  (interactive)
  (unless name
    (setq name (completing-read "Name: " (goobook-get-namelist))))
  (with-current-buffer (get-buffer-create goobook-information-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (shell-command (concat "goobook dquery " (concat "\"" name "\"")) t nil)
      (goto-char (point-min))
      (forward-line 1)
      (delete-region (point-min) (point))
      (if (search-forward "Groups:" nil t)
          (delete-region (line-beginning-position) (point-max)))
      (goto-char (point-min))
      (goobook-information-mode)
      (display-buffer goobook-information-buffer-name))))

(defun goobook-information-quit ()
  (interactive)
  (delete-window))

(defun goobook-information-next ()
  "Create an information buffer for the next contact.
If there is a names buffer, then use the next contact from that buffer.
Otherwise, if there is a search buffer, use the next contact from that buffer."
  (interactive)
  (let ((name 
         (save-excursion
           (goto-char (point-min))
           (setq name (buffer-substring (line-beginning-position) (line-end-position)))))
        (namebuf (get-buffer goobook-results-buffer-name)))
    (if (not namebuf)
        (message "No results buffer.")
      (set-buffer namebuf)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward name)
        (if (= (forward-line 1) 0)
            (goobook-results-buffer-current-name-info))))))

(defun goobook-information-previous ()
  "Create an information buffer for the previous contact.
If there is a names buffer, then use the previous contact from that buffer.
Otherwise, if there is a search buffer, use the previous contact from that buffer."
  (interactive)
  (let ((name 
         (save-excursion
           (goto-char (point-min))
           (setq name (buffer-substring (line-beginning-position) (line-end-position)))))
        (namebuf (get-buffer goobook-results-buffer-name)))
    (if (not namebuf)
        (message "No name buffer.")
      (set-buffer namebuf)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward name)
        (if (= (forward-line -1) 0)
            (goobook-results-buffer-current-name-info))))))

(defun goobook-information-email ()
  "Send an email to the current contact."
  (interactive)
  (let (name)
    (save-excursion
      (goto-char (point-min))
      (setq name (buffer-substring (line-beginning-position) (line-end-position))))
    (goobook-email-from-name name)))

(defun goobook-information-refresh ()
  (interactive)
  (let ((lineno (line-number-at-pos))
        (name 
          (save-excursion
            (goto-char (point-min))
            (buffer-substring (line-beginning-position) (line-end-position)))))
;    (with-current-buffer google-name-buffer-name
;      (goobook-names-refresh))
    (goobook-reload)
    (goobook-information name)
    (goto-line lineno)))

(defvar goobook-information-keymap nil
  "The keymap for the goobook information page.")

(if goobook-information-keymap
    nil
  (let ((map (make-sparse-keymap)))
    (define-key map "m" 'goobook-information-email)
    (define-key map "R" 'goobook-information-refresh)
    (define-key map "q" 'goobook-information-quit)
    (define-key map "n" 'goobook-information-next)
    (define-key map "p" 'goobook-information-previous)
    (setq goobook-information-keymap map)))

(defun goobook-information-mode ()
  "Mode for displaying information about a Google Contact.
In this buffer:
\\[goobook-information-email]
  Send an email to the contact.
\\[goobook-information-next]
  Display information about the next contact.
\\[goobook-information-previous]
  Display information about the previous contact.
\\[goobook-information-refresh]
  Refresh the information.
\\[goobook-information-quit]
  Delete the information window and bury the names buffer.
"
  (kill-all-local-variables)
  (use-local-map goobook-information-keymap)
  (setq mode-name "Goobook information")
  (setq major-mode 'goobook-information-mode)
  (setq buffer-read-only t))

;;; This next part is adapted from google-contacts.el
;;; Complete name/email in message mode
(defun goobook-message-complete-function ()
  "Function used in `completion-at-point-functions' in `message-mode'."
  (let ((mail-abbrev-mode-regexp
         "^\\(Resent-To\\|To\\|B?Cc\\|Reply-To\\|From\\|Mail-Followup-To\\|Mail-Copies-To\\|Disposition-Notification-To\\|Return-Receipt-To\\):"))
    (when (mail-abbrev-in-expansion-header-p)
      (goobook-complete-name))))

(defun goobook-complete-name ()
  "Complete text at START with a user name and email."
  (let ((end (point))
        (start (save-excursion
                 (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                 (goto-char (match-end 0))
                 (point)))
        choices)
    (dolist (contact (goobook-get-namelist))
      (let* ((fullname (nth 0 contact))
             (email (nth 1 contact)))
        (add-to-list 'choices
                     (if (string= fullname "")
                         email
                       (concat fullname " <" email ">")))))
    (list start end (completion-table-case-fold choices))))

(add-hook 'message-mode-hook
          (lambda ()
            (add-to-list 'completion-at-point-functions
                         'goobook-message-complete-function)))


;;; Also from google-contacts
;;; Tie in with gnus

(defun goobook-information-from-article ()
  "Create an information buffer for the sender of the current email."
  (interactive)
  (let ((from (when (gnus-alive-p)
                (gnus-with-article-headers
                  (mail-extract-address-components
                   (or (mail-fetch-field "From") ""))))))
    (when from
      (let ((name (car from))
            (email (cadr from)))
        (if (assoc name (goobook-get-namelist))
            (goobook-information name)
          (if (assoc email (mapcar 'reverse (goobook-get-namelist)))
              (goobook-information email)
            (message (concat name " is not in your contact list."))))))))

(defun goobook-add-article-author ()
  "Add the current sender to Google Contacts."
  (interactive)
  (let ((from (when (gnus-alive-p)
                (gnus-with-article-headers
                  (mail-extract-address-components
                   (or (mail-fetch-field "From") ""))))))
    (when from
      (let ((name (car from))
            (email (cadr from)))
        (if (assoc name (goobook-get-namelist))
            (message (concat "The name " name " is already a contact."))
          (if (assoc email (mapcar 'reverse (goobook-get-namelist)))
              (message (concat "The email " email " is already a contact."))
            (shell-command-to-string (concat "goobook add \"" name "\"  "email))
            (goobook-reload)
            (message (concat name " had been added to your contacts."))))))))

(eval-after-load "gnus"
  '(progn
     (define-key gnus-summary-mode-map ";" 'goobook-information-from-article)
     (define-key gnus-summary-mode-map ":" 'goobook-add-article-author)
     (define-key gnus-article-mode-map ";" 'goobook-information-from-article)
     (define-key gnus-article-mode-map ":" 'goobook-add-article-author)))

(provide 'goobook)
