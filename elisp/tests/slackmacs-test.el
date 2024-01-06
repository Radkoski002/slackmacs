;; -*- lexical-binding: t -*-
(require 'ert)
(require 'slackmacs-conversation)
(require 'slackmacs-conversation-list)

(defun read-mock-file (file)
    (with-temp-buffer
        (insert-file-contents (format "../__mocks__/%s.json" file))
        (buffer-string)
    )
)

(defun populate-slackmacs-instance ()
    (let (
        (slackmacs_instance (slackmacs/start))
        (convs-data (read-mock-file "conversations"))
        (users-data (read-mock-file "users"))
        (messages-data (read-mock-file "messages"))
        (replies-data (read-mock-file "replies"))
        (id "C00000001")
        (ts "1698435687.161709")
        )
            (slackmacs/users-list-from-json users-data slackmacs_instance)
            (slackmacs/conversation-list-from-json convs-data slackmacs_instance)
            (slackmacs/messages-list-from-json messages-data id slackmacs_instance)
            (slackmacs/messages-list-replies-from-json replies-data id ts slackmacs_instance)
            slackmacs_instance
    )
)


(defun insert-conversations-list-data ()
    (setq slackmacs_instance (populate-slackmacs-instance))
    (rename-buffer "conversations")
    (get-cached-conversation-list)
    (makunbound 'slackmacs_instance)
)

(defun insert-messages-data ()
    (setq slackmacs_instance (populate-slackmacs-instance))
    (let ((id "C00000001"))
        (rename-buffer (format "conversation-%s" id))
        (slackmacs-update-conversation id)
    )
    (makunbound 'slackmacs_instance)
)

(defun insert-replies-data ()
    (setq slackmacs_instance (populate-slackmacs-instance))
    (let (
        (id "C00000001")
        (ts "1698435687.161709")
        )

            (rename-buffer (format "reply-%s" id ts))
            (slackmacs-update-replies id ts)
        )
    (makunbound 'slackmacs_instance)
)

(defun send-message ()
    (setq slackmacs_instance (populate-slackmacs-instance))
    (let (
        (id "C00000001")
        (data (read-mock-file "new_message"))
        )
        (rename-buffer (format "conversation-%s" id))
        (slackmacs/message-add slackmacs_instance data id)
        (slackmacs-update-conversation id)
    )
    (makunbound 'slackmacs_instance)
)

(defun edit-message ()
    (setq slackmacs_instance (populate-slackmacs-instance))
    (let (
        (id "C00000001")
        (ts "1703853029.543409")
        (text "edited message")
        )
        (rename-buffer (format "conversation-%s" id))
        (slackmacs/message-edit slackmacs_instance text id ts)
        (slackmacs-update-conversation id)
    )
    (makunbound 'slackmacs_instance)
)

(defun delete-message ()
    (setq slackmacs_instance (populate-slackmacs-instance))
    (let (
        (id "C00000001")
        (ts "1703853029.543409")
        )
        (rename-buffer (format "conversation-%s" id))
        (slackmacs/message-delete slackmacs_instance id ts)
        (slackmacs-update-conversation id)
    )
    (makunbound 'slackmacs_instance)
)

(defun edit-message-event ()
    (setq slackmacs_instance (populate-slackmacs-instance))
    (let (
        (data (read-mock-file "edit_message_event"))
        (id "C00000001")
        )
        (rename-buffer (format "conversation-%s" id))
        (slackmacs/websocket-handle-events data slackmacs_instance)
        (slackmacs-update-conversation id)
    )
    (makunbound 'slackmacs_instance)
)

(defun delete-message-event ()
    (setq slackmacs_instance (populate-slackmacs-instance))
    (let (
        (data (read-mock-file "delete_message_event"))
        (id "C00000001")
        )
        (rename-buffer (format "conversation-%s" id))
        (slackmacs/websocket-handle-events data slackmacs_instance)
        (slackmacs-update-conversation id)
    )
    (makunbound 'slackmacs_instance)
)

(ert-deftest conversation-list-buffer-test ()
    (ert-test-erts-file 
	"slackmacs-test-conversation-list.erts"
	(lambda () (insert-conversations-list-data))
    )
)

(ert-deftest messages-buffer-test ()
    (ert-test-erts-file 
	"slackmacs-test-messages.erts"
	(lambda () (insert-messages-data))
    )
)

(ert-deftest reply-buffer-test ()
    (ert-test-erts-file 
	"slackmacs-test-replies.erts"
	(lambda () (insert-replies-data))
    )
)

(ert-deftest send-message-test ()
    (ert-test-erts-file 
	"slackmacs-test-send-message.erts"
	(lambda () (send-message))
    )
)

(ert-deftest edit-message-test ()
    (ert-test-erts-file 
	"slackmacs-test-edit-message.erts"
	(lambda () (edit-message))
    )
)

(ert-deftest delete-message-test ()
    (ert-test-erts-file 
	"slackmacs-test-delete-message.erts"
	(lambda () (delete-message))
    )
)

(ert-deftest edit-message-event-test ()
    (ert-test-erts-file 
	"slackmacs-test-edit-message.erts"
	(lambda () (edit-message-event))
    )
)

(ert-deftest delete-message-event-test ()
    (ert-test-erts-file 
	"slackmacs-test-delete-message.erts"
	(lambda () (delete-message-event))
    )
)

(provide 'slackmacs-test-conversation-list)
