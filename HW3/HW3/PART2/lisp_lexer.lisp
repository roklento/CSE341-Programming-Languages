(setq tokenList nil )
(setq in nil)
(setq valueList nil)
(setq identifierSequence nil)
(setq quotesCount 0 )
(setq commentCount 0 )
(setq token -1 )

(defun GetLine ()
  "Read a line from the input stream or from the `in` variable."
  (cond
   ((null in) (read-line))
   (t (read-line in nil))))


(defun ProgressLine (string)
  (let ((string (concatenate 'string string (list #\newline)))
        (n (length string))
        (currentTokenn ""))
    (dotimes (i n)
      (setq currentTokenn (OldProgress currentTokenn string (1+ i)))
      (when (member currentTokenn '("SYNTAX_ERROR" "KW_EXIT"))
        (return-from ProgressLine currentTokenn)))))




(defun OldProgress (token string i)
  (let ((currentChar (char string (- i 1))))
    (when (not (or (char= currentChar #\newline)
                   (char= currentChar #\space)
                   (char= currentChar #\tab)))
      (setq token (concatenate 'string token (list currentChar))))
    (when (char= currentChar #\newline)
      (setq commentCount 0))
    (if (string= token "")
        token
        (if (eq commentCount 0)
            (oldProgressLogic token string i)
            ""))))


(defun oldProgressLogic (token string i)
  (let ((checkRes (DFA token string i)))
    (cond
      ((string= checkRes "") token)
      ((string= checkRes "SYNTAX_ERROR") "SYNTAX_ERROR")
      ((string= checkRes "KW_EXIT") "KW_EXIT")
      (t ""))))

(defun CheckNumber(firstChar)
  (if (and (char>= firstChar #\0 )(char<= firstChar #\9)) 
      1
      0))
(defun CheckNumberFunc (token)
  (dotimes (i (length token))
    (let ((ch (char token i)))
      (if (not (CheckNumber ch))
          (return 0))))
  1)

(defun CheckLetter (firstChar)
  (if (or (and (char>= firstChar #\a) (char<= firstChar #\z))
          (and (char>= firstChar #\A) (char<= firstChar #\Z)))
      1
      0))

(defun CheckIfStatement(token)
  (if (reduce #'(lambda (acc x) (and acc (or (CheckNumber x) (CheckLetter x)))) token :initial-value t)
      1
      0))

(defun CheckNewlineAndSpaceAndTab(str i)
  (cond
    ((and (< i (length str)) (= (CheckNextOperator str i) 1)) 1)
    (t 0)))


(defun CheckNextOperator(str i)
  (let ((currentChar (char str i)))
    (case currentChar
      (#\+ 1)
      (#\- 1)
      (#\/ 1)
      (#\* 1)
      (#\( 1)
      (#\) 1)
      (#\" 1)
      (#\, 1)
      (#\; 1)
      (#\newline 1)
      (#\tab 1)
      (#\space 1)
      (otherwise 0))))

(defun Check-OP-Token(token str i)
  (if (string= token "+")
      "OP_PLUS"
    (if (string= token "-")
        "OP_MINUS"
      (if (string= token "/")
          "OP_DIV"
        (if (string= token "*")
            (MultiProgress token str i)
          (if (string= token "(")
              "OP_OP"
            (if (string= token ")")
                "OP_CP"
                (if (string= token ",")
                    "OP_COMMA"
                  (if (string= token ";")
                      (CheckCommentOperator str i)
                    "")))))))))

(defun Check-KW-Token(token)
  (if (string= token "and")
      "KW_AND"
    (if (string= token "or")
        "KW_OR"
      (if (string= token "not")
          "KW_NOT"
        (if (string= token "equal")
            "KW_EQUAL"
          (if (string= token "deffun")
              "KW_DEFFUN"
            (if (string= token "for")
                "KW_FOR"
              (if (string= token "if")
                  "KW_IF"
                (if (string= token "exit")
                    "KW_EXIT"
                      (if (string= token "true")
                          (progn
                            (if (eq valueList nil)
                                (push 'true valueList)
                                (push 'true (cdr (last valueList)))
                                )
                            "KW_TRUE")
                        (if (string= token "false")
                            (progn
                              (if (eq valueList nil)
                                  (push 'false valueList)
                                  (push 'false (cdr (last valueList)))
                                  )
                              "KW_FALSE")
                          "")))))))))))

(defun Check-OP_OC()
  (let ((quotesCount 1))
    "OP_OC"))

(defun Check-OP_CC()
	(let ((quotesCount 0))
    "OP_CC"))

(defun CheckQuote() 
  (cond
    ((= quotesCount 0) (Check-OP_OC))
    (t (Check-OP_CC))))

(defun CheckCommentOperator(str i)
  (cond
    ((and (< i (length str)) (char= #\; (char str i))) (CheckCommentOperatorFunc))
    (t "")))

(defun CheckCommentOperatorFunc()
  (let ((commentCount 1))
    "COMMENT"))

(defun DFA (token string i)
  (let ((check (Check-OP-Token token string i)))
    (cond
      ((not (string= check "")) (PrintToken check))
      (t (let ((nextOperator (CheckNewlineAndSpaceAndTab string i)))
           (if (= nextOperator 0)
               ""
               (let ((check (Check-KW-Token token)))
                 (if (not (string= check ""))
                     (PrintToken check)
                     (let ((checkIdentifierAndValueError (DFA-Func token)))
                       (if (string= checkIdentifierAndValueError "SYNTAX_ERROR")
                           (progn
                             (terpri)
                             (princ checkIdentifierAndValueError)
                             (princ " ")
                             (princ token)
                             (princ " ")
                             (princ "cannot be tokenized"))
                           (PrintToken checkIdentifierAndValueError)))))))))))


(defun DFA-Func (token)
  (let ((value 0)
        (identifier 0)
        (firstChar (char token 0)))
    (cond
      ((= (CheckNumber firstChar) 1) (setq value (CheckNumberFunc token)))
      ((= (CheckLetter firstChar) 1) (setq identifier (CheckIfStatement token))))
    (cond
      ((= value 1)
       (if (eq valueList nil)
           (push (parse-integer token) valueList)
           (push (parse-integer token) (cdr (last valueList))))
       "VALUE")
      ((= identifier 1)
       (if (eq identifierSequence nil)
           (push token identifierSequence)
           (push token (cdr (last identifierSequence))))
       "IDENTIFIER")
      (t "SYNTAX_ERROR"))))


(defun PrintToken(token)
  (cond
    ( (null tokenList) (setq tokenList (list (read-from-string token))) )
    ( t (setq tokenList (append tokenList (list (read-from-string token)))) )
  )
  token)

(defun MultiProgress(token str i )
  (cond 
    ((and ( < i (length str )) (char= #\* (char str i)) ) (dblmulthelper))
    (t "OP_MULT")
  ))
