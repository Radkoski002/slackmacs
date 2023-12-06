;;; slackmacs.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Radosław Kostrzewski

;; Author: Radosław Kostrzewski <radoslaw.kostrzewski@protonmail.com>
;; Keywords: slack, tools
;; Version: 0.0.1

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


(provide 'slackmacs)
;;; slackmacs.el ends here
