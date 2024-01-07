;;; slackmacs.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Radosław Kostrzewski

;; Author: Radosław Kostrzewski <radoslaw.kostrzewski@protonmail.com>
;; Keywords: slack, tools
;; Package-Requires: ((request "0.3.2"))
;; Version: 1.0.0

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

;; Slack client for Emacs written in elisp and rust.

;;; Code:

(require 'slackmacs-module-rs)
(require 'slackmacs-conversation-list)
(require 'slackmacs-message)
(require 'slackmacs-request)
(require 'slackmacs-websocket)


(defun slackmacs-start ()
    (interactive)
    (if (not (boundp 'slackmacs_rust_backend))
        (setq slackmacs_rust_backend 
            (start-process "slackmacs-rust-backend" "*slackmacs-backend*" 
                "find" 
                ".emacs.d/elpa" 
                "-name" 
                "slackmacs-rust-backend" 
                "-exec" 
                "{}" 
                ";"
            )
        )
    )
    (sleep-for 0 100)
    (if (not (boundp 'slackmacs_instance))
        (setq slackmacs_instance (slackmacs/start))
    )
    (slackmacs-force-refresh-conversation-list)
    (if (not (boundp 'slackmacs_websocket_handler))
        (progn
            (slackmacs-request "start" (lambda (data) (message data)))
            (setq slackmacs_websocket_handler (run-at-time t 3 'slackmacs-handle-websocket))
        )
    )
)

(defun slackmacs-websocket-close ()
    (interactive)
    (if (boundp 'slackmacs_websocket_handler)
        (cancel-timer slackmacs_websocket_handler)
        (makunbound 'slackmacs_websocket_handler)
    )
)

(defun slackmacs-close ()
    (interactive)

    (slackmacs-websocket-close)
    (delete-process "*slackmacs-backend*")
    (kill-buffer "*slackmacs-backend*")
    (makunbound 'slackmacs_instance)
    (makunbound 'slackmacs_rust_backend)
)

(provide 'slackmacs)
;;; slackmacs.el ends here
