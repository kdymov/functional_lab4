(load "splitseq/split-sequence.lisp")
(in-package :split-sequence)

; Word class
(defclass word()
	((content :accessor word_content
			  :initform (error "No word content supplied")
			  :initarg :content))
)

; Sentence class
(defclass sentence()
	((content :accessor sentence_content
			  :initform (error "No sentence content supplied")
			  :initarg :content))
)

; List of vowel chars
(defparameter vowels '(#\a #\o #\i #\u #\e #\y))

; Read file contents
(defun file-string (path)
	(with-open-file (stream path)
		(let ((data (make-string (file-length stream)))) (read-sequence data stream) data)
	)
)

;-------------------------------------------------------------------
; Printing methods
;-------------------------------------------------------------------
(defmethod print_word((w word) &key)
	(print (slot-value w 'content))
)

(defun print_word_list (ls)
	(cond 
		((null ls) 0)
		(t (print_word (car ls)) (print_word_list (cdr ls)))
	)
)
;-------------------------------------------------------------------

;-------------------------------------------------------------------
; Strange reading cleanup
;-------------------------------------------------------------------
(defmethod clean-word ((w Word))
	(setf ww (make-instance 'word :content (subseq (word_content w) 1)))
)

(defun cleanup-first (ls)
	(cons (clean-word (cadr ls)) (cddr ls))
)
;-------------------------------------------------------------------

;-------------------------------------------------------------------
; Sorting 
;-------------------------------------------------------------------
; Filter - if vowel return 1, else return 0
(defun filt (chr)
	(cond
		((member chr vowels) 1)
		(T 0)
	)
)

; Sum vowels
(defun sum (lst)
	(cond
		((null lst) 0)
		(T (+ (car lst) (sum (cdr lst))))
	)
)

; Vowel quantity in word
(defmethod vowels-word ((w word))
	(sum (map 'list #'filt (word_content w)))
)

; Letter quantity in word
(defmethod length-word ((w word))
	(length (word_content w))
)

; Comparator function - vowelQ/letterQ
(defmethod comparator ((w word))
	(/ (vowels-word w) (length-word w))
)

; Sorting method
(defmethod sorter ((s sentence))
	(sort (sentence_content s) #'< :key #'comparator)
)
;-------------------------------------------------------------------

(setq fname "input_text.txt")
(setq text (file-string fname))

(setq list_all (remove-if #'(lambda (x) (equal "" x)) (split-sequence-if-not 'alpha-char-p text)))
(setq all_words_list (map 'list (lambda (x) (make-instance 'word :content (string-downcase x))) (remove-duplicates list_all :test #'equal)))
(setq all_words (cleanup-first all_words_list))
(setq all (make-instance 'sentence :content all_words))

(format t "~%TEXT:~%")
(print_word_list (sentence_content all))
(format t "~%~%-----------------------------~%~%SORTED:~%")
(print_word_list (sorter all))