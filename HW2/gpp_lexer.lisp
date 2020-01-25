;; CSE341 - Programming Languages, Assignment 2, G++ Lexer
;; Written by Fatih Koç, 141044013


;; ********************************************************************************
;; Definitions and their helper functions for the G++ Lexer
(setq seperators '(#\Space #\Tab #\Newline))

(setq keywords '("and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))

(setq operators '("+" "-" "/" "*" "(" ")" "**" "\"" ","))
;; ********************************************************************************

;; ********************************************************************************
;; HELPERS
;; Converts character list to the string
(defun list-to-string (input)
	(coerce input 'string))

;; Converts string to the character list
(defun string-to-list (input)
	(coerce input 'list))

;; Returns true if the needle list is in the list of strings (haystack)
(defun contains (needle haystack)
	(dolist (item haystack)
        (if (equal item (list-to-string needle))
        	(return-from contains T))))

;; Returns the given operator characters token.
(defun get-operator-text (operator)
	;(format t "--------get-operator-text: ~A~%" op)
	(case (car operator)
		(#\+ "OP_PLUS")
		(#\- "OP_MINUS")
		(#\/ "OP_DIV")
		(#\* "OP_MULT")
		(#\( "OP_OP")
		(#\) "OP_CP")
		(#\" "OP_QUOTE")
		(#\, "OP_COMMA")
		("**" "OP_DBLMULT")))

;; Returns the given keywords token.
(defun get-keyword-text (keyword)
	(concatenate 'string "KW_" (string-upcase (list-to-string keyword))))
;; ********************************************************************************

;; ********************************************************************************
;; READING INPUT FILE
;; Checks each character on given line and parses it to the lexemes.
(defun parse-line (line &key (operators operators) (separators seperators) (token-list '()) (temp '()))
  ;(format t "input: ~A, temp: ~A, token-list: ~A~%" (car line) temp token-list)
  (cond ((null line) 
  			(nreverse (if temp (cons (nreverse temp) token-list) token-list)))
        ((member (car line) separators)
        	(parse-line (cdr line) 
        				:operators operators
        				:separators separators 
                        :token-list (if temp (cons (nreverse temp) token-list) token-list)
                        :temp '()))
        ((equal (car line) #\;)
        	(return-from parse-line (list line)))
        ((contains (string (car line)) operators)
        	(parse-line (cdr line) 
        				:operators operators
        				:separators separators
                        :token-list (if temp 
                        	(cons (list (car line)) (cons (nreverse temp) token-list)) 
                        	(cons (list (car line)) token-list))
                        :temp '()))
        (T 
        	(parse-line (cdr line) 
        				:operators operators
        				:separators separators
                        :token-list token-list
                        :temp (cons (car line) temp)))))

;; Reads given file line by line
(defun read-file-line-by-line (filename)
  (with-open-file (f filename :direction :input)
    (loop for line = (read-line f nil)
        ;do (format t "LINE: ~A~%" line)
		while line
		collect line)))

;; Reads given file and returns a list of its contents as a list of lexeme lists.
(defun read-as-list (filename)
	(mapcan (lambda (s) (parse-line (coerce s 'list))) 
          (coerce (read-file-line-by-line filename) 'list)))
;; ********************************************************************************

;; ********************************************************************************
;; PERFORM LEXICAL ANALYSIS
;; Checks if the given input list is a valid integer.
(defun integer-check (input)
	(if (equal nil input)
		T
		(if (digit-char-p (car input))
			(integer-check (cdr input))
			nil)))

;; Checks if the given input list is a valid value for G++ Language
(defun is-value (input)
	;(format t "--------is-value: ~A~%" input)
	(if (equal #\0 (car input))
		(equal nil (cdr input))
		(integer-check input)))

;; Checks if the given input list is a valid alphanumeric string.
(defun character-check (input)
	(if (equal nil input)
		T
		(if (alphanumericp (car input))
			(character-check (cdr input))
			nil)))

;; Checks if the given input list is a valid identifier for G++ Language
(defun is-identifier (input)
	;(format t "--------is-identifier: ~A~%" input)
	(if (digit-char-p (car input))
		nil
		(character-check input)))

;; Checks if the given input list is a valid comment for G++ Language
(defun is-comment (input)
	(and (equal (car input) #\;) (equal (cadr input) #\;))
)

;; Checks if the given input list is a valid keyword for G++ Language
(defun is-keyword (input)
	(contains (list-to-string input) keywords)
)

;; Checks if the given input list is a valid operator for G++ Language
(defun is-operator (input)
	(contains (list-to-string input) operators)
)

;; Tokenizes given input list, prints and returns all tokens as a list
(defun tokenizer (input-list &key (token-list '()))
	;(format t "input: ~A~%" (car input-list))
	(cond 	((null input-list) 
				(return-from tokenizer (nreverse token-list)))
			((is-comment (car input-list))
				;(format t "COMMENT~%")
				(tokenizer (cdr input-list) 
						:token-list token-list))
			((is-operator (car input-list))
				;(format t "~A~%" (get-operator-text (car input-list)))
				(tokenizer (cdr input-list) 
						:token-list (cons (cons (get-operator-text (car input-list)) (car input-list)) token-list)))
			((is-keyword (car input-list))
				;(format t "KW_~A~%" (string-upcase (list-to-string (car input-list))))
				(tokenizer (cdr input-list) 
						:token-list (cons (cons (get-keyword-text (car input-list)) (car input-list)) token-list)))
			((is-value (car input-list))
				;(format t "VALUE~%")
				(tokenizer (cdr input-list) 
						:token-list (cons (cons "VALUE" (car input-list)) token-list)))
			((is-identifier (car input-list))
				;(format t "IDENTIFIER~%")
				(tokenizer (cdr input-list) 
						:token-list (cons (cons "IDENTIFIER" (car input-list)) token-list)))
			(T
				(format t "SYNTAX_ERROR ~A cannot be tokenized. ~%" (list-to-string (car input-list)))
				(return-from tokenizer nil))
	)
)

;; Main function for the homework
(defun gpp-lexer (input-file)
	(setq input (read-as-list input-file))
	;(format t "~%Given input as a list:~%~A~%" input)
	(setq tokenized-input (tokenizer input))
	;(format t "~%~{~A ~%~}~%" tokenized-input)
)


;; Universal test method
(defun test ()
	;(print "....................................................")
	;(format t "~%KEYWORDS: ~A~%" keywords)
	;(format t "OPERATORS: ~A~%" operators)

	;(setq document (read-as-list "C:\\Users\\FK\\Desktop\\HW2\\test1.txt"))
	;(format t "~%DOCUMENT: ~%~{~{~A ~}~%~}~%" document)
	;(print "....................................................")

	(gpp-lexer "C:\\Users\\FK\\Desktop\\HW3\\test1.txt")
)

(test)
;;(load "C:\\Users\\FK\\Desktop\\HW3\\gpp_lexer.lisp")