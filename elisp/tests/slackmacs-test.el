;; -*- lexical-binding: t -*-
(require 'ert)
(require 'slackmacs-conversation)
(require 'slackmacs-conversation-list)

(defun read-mock-file (file)
    (with-temp-buffer
        (insert-file-contents (format "../../__mocks__/%s.json" file))
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
        (id "C1")
        (ts "1")
        )
            (slackmacs/users-list-from-json users-data slackmacs_instance)
            (slackmacs/conversation-list-from-json convs-data slackmacs_instance)
            (slackmacs/messages-list-from-json messages-data id slackmacs_instance)
            (slackmacs/messages-list-replies-from-json replies-data id ts slackmacs_instance)
            slackmacs_instance
    )
)

(defun setup-slackmacs-test (test-func)
    (setq slackmacs_instance (populate-slackmacs-instance))
    (funcall test-func)
    (makunbound 'slackmacs_instance)
)

(defun setup-slackmacs-conversation-test (test-func)
    (setup-slackmacs-test
	(lambda ()
	    (let ((id "C1"))
		(rename-buffer (format "conversation-%s" id))
		(funcall test-func id)
		(slackmacs-update-conversation id)
	    )
	)
    )
)

(defun setup-slackmacs-replies-test (test-func)
    (setup-slackmacs-test
	(lambda ()
	    (let ((id "C1") (ts "1"))
		(rename-buffer (format "reply-%s-%s" id ts))
		(funcall test-func id ts)
		(slackmacs-update-replies id ts)
	    )
	)
    )
)

(defun setup-slackmacs-conversation-websocket-test (data-file-name)
    (setup-slackmacs-conversation-test
        (lambda (_)
            (let ((data (read-mock-file data-file-name)))
                (slackmacs/websocket-handle-events data slackmacs_instance)
            )
        )
    )
)

(defun setup-slackmacs-replies-websocket-test (data-file-name)
    (setup-slackmacs-replies-test
        (lambda (_ _)
            (let ((data (read-mock-file data-file-name)))
                (slackmacs/websocket-handle-events data slackmacs_instance)
            )
        )
    )
)

(defun insert-conversations-list-data ()
    (setup-slackmacs-test
        (lambda ()
            (rename-buffer "conversations")
            (get-cached-conversation-list)
        )
    )
)

(defun insert-messages-data ()
    (setup-slackmacs-conversation-test (lambda (id) nil))
)

(defun insert-replies-data ()
    (setup-slackmacs-replies-test (lambda (id ts) nil))
)

(defun send-message ()
    (setup-slackmacs-conversation-test
        (lambda (id)
            (let ((data (read-mock-file "new_message")))
                (slackmacs/message-add slackmacs_instance data id)
            )
        )
    )
)

(defun send-message-overlap ()
    (setup-slackmacs-conversation-test
        (lambda (id)
            (let ((ts "4") (api-data (read-mock-file "new_message")) (event-data (read-mock-file "new_message_event")))
                (slackmacs/message-add slackmacs_instance api-data id)
                (slackmacs/websocket-handle-events event-data slackmacs_instance)
            )
        )
    )
)

(defun send-message-overlap-2 ()
    (setup-slackmacs-conversation-test
        (lambda (id)
            (let ((ts "4") (api-data (read-mock-file "new_message")) (event-data (read-mock-file "new_message_event")))
                (slackmacs/websocket-handle-events event-data slackmacs_instance)
                (slackmacs/message-add slackmacs_instance api-data id)
            )
        )
    )
)

(defun send-reply ()
    (setup-slackmacs-conversation-test
        (lambda (id)
            (let ((data (read-mock-file "new_reply")) (parent_ts "1"))
                (slackmacs/message-reply-add slackmacs_instance data id parent_ts)
            )
        )
    )
)

(defun edit-message ()
    (setup-slackmacs-conversation-test
        (lambda (id)
            (let ((ts "4") (text "edited message"))
                (slackmacs/message-edit slackmacs_instance "{\"ok\": true}" text id ts)
            )
        )
    )
)

(defun delete-message ()
    (setup-slackmacs-conversation-test
        (lambda (id)
            (let ((ts "4"))
                (slackmacs/message-delete slackmacs_instance "{\"ok\": true}" id ts)
            )
        )
    )
)

(defun delete-message-overlap ()
    (setup-slackmacs-conversation-test
        (lambda (id)
            (let ((ts "4") (data (read-mock-file "delete_message_event")))
                (slackmacs/message-delete slackmacs_instance "{\"ok\": true}" id ts)
                (slackmacs/websocket-handle-events data slackmacs_instance)
            )
        )
    )
)

(defun delete-message-overlap-2 ()
    (setup-slackmacs-conversation-test
        (lambda (id)
            (let ((ts "4") (data (read-mock-file "delete_message_event")))
                (slackmacs/websocket-handle-events data slackmacs_instance)
                (slackmacs/message-delete slackmacs_instance "{\"ok\": true}" id ts)
            )
        )
    )
)

(defun conversation-new-message-event ()
    (setup-slackmacs-conversation-websocket-test "new_message_event")
)

(defun conversation-new-reply-event ()
    (setup-slackmacs-conversation-websocket-test "new_reply_event")
)

(defun conversation-edit-message-event ()
    (setup-slackmacs-conversation-websocket-test "edit_message_event")
)

(defun conversation-delete-message-event ()
    (setup-slackmacs-conversation-websocket-test "delete_message_event")
)

(defun conversation-delete-reply-event ()
    (setup-slackmacs-conversation-websocket-test "delete_reply_event")
)

(defun replies-new-reply-event ()
    (setup-slackmacs-replies-websocket-test "new_reply_event")
)

(defun replies-edit-reply-event ()
    (setup-slackmacs-replies-websocket-test "edit_reply_event")
)

(defun replies-delete-reply-event ()
    (setup-slackmacs-replies-websocket-test "delete_reply_event")
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

(ert-deftest send-message-overlap-test ()
    (ert-test-erts-file 
	"slackmacs-test-send-message.erts"
	(lambda () (send-message-overlap))
    )
    (ert-test-erts-file 
	"slackmacs-test-send-message.erts"
	(lambda () (send-message-overlap-2))
    )
)

(ert-deftest send-reply-test ()
    (ert-test-erts-file 
	"slackmacs-test-send-reply.erts"
	(lambda () (send-reply))
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

(ert-deftest delete-message-overlap-test ()
    (ert-test-erts-file 
	"slackmacs-test-delete-message.erts"
	(lambda () (delete-message-overlap))
    )
    (ert-test-erts-file 
	"slackmacs-test-delete-message.erts"
	(lambda () (delete-message-overlap-2))
    )
)

(ert-deftest conversation-new-message-event-test ()
    (ert-test-erts-file 
	"slackmacs-test-send-message.erts"
	(lambda () (conversation-new-message-event))
    )
)

(ert-deftest conversation-new-reply-event-test ()
    (ert-test-erts-file 
	"slackmacs-test-send-reply.erts"
	(lambda () (conversation-new-reply-event))
    )
)

(ert-deftest conversation-edit-message-event-test ()
    (ert-test-erts-file 
	"slackmacs-test-edit-message.erts"
	(lambda () (conversation-edit-message-event))
    )
)

(ert-deftest conversation-delete-message-event-test ()
    (ert-test-erts-file 
	"slackmacs-test-delete-message.erts"
	(lambda () (conversation-delete-message-event))
    )
)

(ert-deftest conversation-delete-reply-event-test ()
    (ert-test-erts-file 
	"slackmacs-test-delete-reply.erts"
	(lambda () (conversation-delete-reply-event))
    )
)

(ert-deftest replies-new-reply-event-test ()
    (ert-test-erts-file 
	"slackmacs-test-replies-send-message.erts"
	(lambda () (replies-new-reply-event))
    )
)

(ert-deftest replies-edit-reply-event-test ()
    (ert-test-erts-file 
	"slackmacs-test-replies-edit-message.erts"
	(lambda () (replies-edit-reply-event))
    )
)

(ert-deftest replies-delete-reply-event-test ()
    (ert-test-erts-file 
	"slackmacs-test-replies-delete-message.erts"
	(lambda () (replies-delete-reply-event))
    )
)


(provide 'slackmacs-test-conversation-list)
