( load "lisp_lexer.lisp" )
(setq Interpreter nil)
(setq tokens nil)
(setq tokenList nil)
(setq symbolList nil)


(defun RuleStart()
  (expi (printf "SYNTAX OK..\nResult %d\n" $1))
  (explisti (printf "SYNTAX OK..\nResult " (print-list $1)))
  (expb (printf "SYNTAX OK..\nResult %d\n" $1))
  (start expi (printf "SYNTAX OK..\nResult %d\n" $2))
  (start explisti (printf "SYNTAX OK..\nResult " (print-list $2)))
  (start expb (printf "SYNTAX OK..\nResult %d\n" $2))
  (comment (printf "COMMENT\n"))
  (start comment (printf "COMMENT\n"))
  (op-op kw-exit op-cp (free-list) (exit 0))
  (start op-op kw-exit op-cp (free-list) (exit 0)))

(defun FirstRuleExpi(tokens)
  (let ((res (cond
              ((eq (car tokens) 'VALUE) (GetValueFunc))
              ((eq (car tokens) 'IDENTIFIER) (CheckIdentifierToValue))
              ((eq (isSequenceClosed tokens) t)
               (or (RuleOperatorExpi tokens)
                   (RuleIDExpi tokens)))
              (t nil))))
    (if (null tokens)
        nil
      res)))

(defun CheckFirstRuleExpi(token)
  (cond
   ((eq (CheckIdOrValue token) t) t)
   ((and (listp token) (eq (car token) 'OP_OP))
    (cond
     ((eq (cadr token) 'KW_PLUS) t)
     ((eq (cadr token) 'KW_MINUS) t)
     ((eq (cadr token) 'KW_MULT) t)
     ((eq (cadr token) 'KW_DIV) t)
     ((eq (cadr token) 'KW_EQUAL) t)
     ((eq (cadr token) 'KW_LESS) t)))
   (t nil)))

(defun SecondRuleExpi (tokens)
	(if (null tokens)
		nil
		(RuleExplistExpi tokens)))

(defun ThirdRuleExpi (tokens)
  (cond
    ((member (car tokens) '(KW_TRUE KW_FALSE)) 
     (GetValueFunc))
    ((member (car tokens) '(VALUE IDENTIFIER))
     (CheckIdentifierToValue))
    ((isSequenceClosed tokens) 
     (Rule23-26Expi tokens))
    (t 
     nil)))

(defun MyInterpreter(rule)

	(setq Interpreter nil)

	(cond
	((eq rule 1) (setq Interpreter (FirstRuleExpi tokenList)))
	((eq rule 2) (setq Interpreter (ThirdRuleExpi tokenList)))
	((eq rule 3) (setq Interpreter (SecondRuleExpi tokenList))))
	
	(if (not (eq Interpreter nil))
    Interpreter
  (progn
    (if (eq rule 3)
        (error "Syntax error.")
      (MyInterpreter (+ rule 1))))))

(defun MyInterpreterExpiFunc(rule tokens)
  (cond
   ((= rule 1) (FirstRuleExpi tokens))
   ((= rule 2) (ThirdRuleExpi tokens))
   ((= rule 3) (SecondRuleExpi tokens))
   (t (progn (print "Syntax error.") (exit)))))

(defun ExpiFunction (&optional rule shortenList)
  (let ((res (cond
              ((eq (car tokens) 'VALUE) (GetValueFunc))
              ((eq (car tokens) 'IDENTIFIER) (CheckIdentifierToValue))
              ((eq (isSequenceClosed tokens) t)
               (or (RuleOperatorExpi tokens)
                   (RuleIDExpi tokens)))
              (t nil))))
    (if (null tokens)
        nil
      res)))

(defun Rule23-26Expi (tokens)
  (let ((tokens (MyList tokens)))
    (let ((varOperator (car tokens)))
      (when (CheckRuleOperatorExpiFunc varOperator)
        (setq tokens (cdr tokens))
        (if (equal varOperator 'KW_NOT)
            (let ((lhs (ThirdRuleExpi tokens)))
              (Rule23-26ExpbFunc lhs nil varOperator))
          (let ((lhs (if (CheckIsTrueOrFalse (car tokens))
                         (ThirdRuleExpi (list (car tokens)))
                       nil)))
            (if lhs
                (setq tokens (cdr tokens))
                (setq lhs (ThirdRuleExpi tokens)))
            (if lhs
                (let ((rhs (if (CheckIsTrueOrFalse (car tokens))
                               (ThirdRuleExpi (list (car tokens)))
                             nil)))
                  (if rhs
                      (when (null (cdr tokens))
                        (Rule23-26ExpbFunc lhs rhs varOperator))
                    (CheckSyntaxErrorFunc "Invalid syntax")))
              (CheckSyntaxErrorFunc "Invalid syntax "))))))))
          
(defun Rule23-26ExpbFunc (lhs rhs varOperator)
  (cond
   ((and (eq varOperator 'KW_LESS) (eq varOperator 'KW_EQUAL))
    (cond
     ((and (CheckIsTrueOrFalseFunc lhs) (CheckIsTrueOrFalseFunc rhs)) t)
     ((and (eq rhs nil) (eq varOperator 'KW_NOT) (or (eq lhs 'true) (eq lhs 'false))) t)
     (t (CheckSyntaxErrorFunc "EXPBI invalid value type"))))
   ((numberp lhs)
    (if (not (numberp rhs))
        (CheckSyntaxErrorFunc "Invalid EXPBI parameter ")
      t))
   (t
    (if (not (symbolp rhs))
        (CheckSyntaxErrorFunc "Invalid EXPBI parameter ")
      t)))
  (cond
   ((eq varOperator 'KW_AND)
    (if (and (eq lhs 'true) (eq rhs 'true))
        'true
      'false))
   ((eq varOperator 'KW_OR)
    (if (or (eq lhs 'true) (eq rhs 'true))
        'true
      'false))
   ((eq varOperator 'KW_EQUAL)
    (if (equal lhs rhs)
        'true
      'false))
   ((eq varOperator 'KW_NOT)
    (if (equal lhs 'true)
        'false
      'true))
   ((eq varOperator 'KW_LESS)
    (if (< lhs rhs)
        'true
      'false))))

(defun GetValue (values)
  (CheckValues values)
  (let ((newTempList '()))
    (dotimes (i (length values))
      (push (GetValueFunc) newTempList))
    (nreverse newTempList)))
  
(defun GetValueFunc()
	(let ((res (car valueList)))
		(setq valueList (cdr valueList))
			res))

(defun RuleIDExpi (tokens)

    (setq tokens (MyList tokens) )
    ( let 
        ( ( varOperator (car tokens) ) ) 
        (if (not (eq varOperator 'KW_SET))
            (return-from RuleIDExpi nil))
        (setq tokens ( cdr tokens ) )
        (if (not (and 
                    (eq (car tokens) 'IDENTIFIER)
                    (eq (car (cdr tokens)) 'VALUE)
            )   )
            (CheckSyntaxErrorFunc "Set operation takes IDENTIFIER and VALUE"))
        (let ((lhs (ThirdRuleExpi (list (car tokens))))
                (rhs (ThirdRuleExpi (list (car (cdr tokens))))))
            (if (eq (cdr (cdr tokens)) nil)
                (rule-18-expi-exec lhs rhs varOperator)
                (CheckSyntaxErrorFunc "Too many arguments for EXPBI")))))

(defun RuleOperatorExpi(tokens)
	(setq tokens (MyList tokens) )
	( let 
		( ( varOperator (car tokens) ) ) 
		(if (eq (CheckRuleOperatorExpi varOperator) nil)
			(return-from RuleOperatorExpi nil)
		)
		(setq tokens ( cdr tokens ) )

		(if (eq (CheckIdOrValue (car tokens)) T )
			(progn
				( let 
					( ( lhs (FirstRuleExpi (list (car tokens) ) ) ) ) 
					(setq tokens (cdr tokens))
					(if (eq (CheckIdOrValue (car tokens)) T )
						(progn
							( let 
								( ( rhs (FirstRuleExpi (list (car tokens) ) ) ) )
								(setq tokens (cdr tokens))
								(if (eq tokens nil)
									(progn 
										(return-from  RuleOperatorExpi (RuleOperatorExpiFunc lhs rhs varOperator))
									)
									(progn
										(CheckSyntaxErrorFunc "Too many arguments for EXPI ")))))					
						(progn
							(if 
								(eq (isSequenceClosed tokens) T)
								(progn
									(setq rhs (FirstRuleExpi tokens))
									(return-from  RuleOperatorExpi (RuleOperatorExpiFunc lhs rhs varOperator))
								)
								(CheckSyntaxErrorFunc "Invalid syntax"))))))
			(progn 
				(if (not (or ( eq varOperator 'KW_IF)(eq varOperator 'KW_FOR) ) )
					(progn
						(if (eq (isSequenceClosedLhs tokens) T)
							(progn 
								(let (  (count_op (CountOperatorFunc tokens) ) )
									( let ( ( tokens_new (CheckParenthesisExpi tokens count_op) ) ) 
										( let ((tokens_lhs (car tokens_new)) (tokens_rhs (cadr tokens_new)) ) 
											
											
											( let ((lhs (FirstRuleExpi tokens_lhs)) (rhs (FirstRuleExpi tokens_rhs) )) 
												(if (not 
														(or 
															(eq lhs nil)(eq rhs nil) )
													)
													(return-from  RuleOperatorExpi (RuleOperatorExpiFunc lhs rhs varOperator))
													(CheckSyntaxErrorFunc "RHS or LHS of EXPI not valid")))))))
							(progn
								
								(CheckSyntaxErrorFunc "Invalid syntax"))))
					(progn 
						(let (  (count_op (CheckParenthesisCount tokens) ) )
							( let ( ( tokens_new (CheckParenthesisExpi tokens count_op) ) ) 
								( let ((tokens_lhs (car tokens_new)) (tokens_rhs (cadr tokens_new)) ) 
									
									(if (eq (car tokens_rhs) 'VALUE)
										(progn
											(let ( (lhs (ThirdRuleExpi tokens_lhs)) (rhs1 (FirstRuleExpi (list (car tokens_rhs)))) (rhs2 (FirstRuleExpi (cdr tokens_rhs))))
												
												(return-from  RuleOperatorExpi (RuleOperatorExpiFunc lhs (list rhs1 rhs2) varOperator))))
										(progn 
											(let (  (count_op_rhs (CheckParenthesisCount tokens_rhs) ) )
												( let ( ( tokens_new_rhs (CheckParenthesisExpi tokens_rhs count_op_rhs) ) )
													( let ((lhs (ThirdRuleExpi tokens_lhs))  (rhs_1 ( FirstRuleExpi (car tokens_new_rhs))) (rhs_2 ( FirstRuleExpi (cadr tokens_new_rhs))) ) 
														(return-from  RuleOperatorExpi (RuleOperatorExpiFunc lhs (list rhs_1 rhs_2) varOperator))))))))))))))))

(defun CheckIdentifierToValue()
  (let ((identifier (car tokens)))

	(setq tokens (cdr tokens))

	(let ((value (assoc identifier symbolList)))
	  (if (not (eq value nil))
		  (cdr value)
		(error "Undefined identifier.")))))


(defun CheckSyntaxErrorFunc (&optional temp)
  (format t "Syntax error~@[: ~A~]" temp)
  (terpri)
  (exit))

(defun isSequenceClosed(tokens)
  (let ((open-count 0))
    (loop for token in tokens
          do (if (eq token 'OP_OP) (incf open-count)
               (if (eq token 'OP_CP) (decf open-count))))
    (if (zerop open-count) t nil)))

(defun isSequenceClosedLhs(tokens)
  (let ((open-count 0))
    (loop for token in tokens
          do (if (eq token 'OP_OP) (incf open-count)
               (if (eq token 'OP_CP) (decf open-count))))
    (if (zerop open-count) t nil)))

(defun CheckIsTrueOrFalse(symbol)
  (member symbol '(KW_TRUE KW_FALSE VALUE IDENTIFIER)))

(defun CheckIdOrValue(symbol)
  (cond
    ((or (eq symbol 'IDENTIFIER) (eq symbol 'VALUE)) t)
    (t nil)))

(defun CheckRuleOperatorExpi (varOperator)
	(member varOperator '(OP_PLUS OP_MINUS OP_MULT OP_DBLMULT OP_DIV KW_EQUAL KW_LESS)))

(defun CountOperatorFunc (TempList)
	(if (null TempList)0
		(if (eq (car TempList) 'OP_CP) 0
			(+ (if (eq (car TempList) 'OP_OP) 1 0)
				(CountOperatorFunc (cdr TempList))))))

(defun CheckParenthesisCount (TempList)
	(if (null TempList)
		(CheckSyntaxErrorFunc "CheckParenthesisCount Unbalanced parenthesis")
		(CheckParenthesisCountFunc TempList 1)))

(defun CheckParenthesisCountFunc (TempList count)
	(if (null TempList) count
		(let ((countNum (if (eq (car TempList) 'OP_CP) (1- count)
			(if (eq (car TempList) 'OP_OP)
				(1+ count) count))))
			(if (= countNum 0)
			(CheckSyntaxErrorFunc "CheckParenthesisCount Unbalanced parenthesis")
			(CheckParenthesisCountFunc (cdr TempList) countNum)))))

(defun CheckParenthesisExpi (inputList CountOperatorFunc)
	(if (null inputList)
		(CheckSyntaxErrorFunc "Unbalanced parenthesis EXPI lhs")
		(CheckParenthesisExpiFunc inputList '() CountOperatorFunc)))

(defun CheckParenthesisExpiFunc (inputList newList CountOperatorFunc)
	(cond ((null inputList) (list newList nil))
		((= CountOperatorFunc 0) (list newList inputList))
		(t (let ((tempNewList (append newList (list (car inputList)))))
				(if (eq (car inputList) 'OP_CP)
					(CheckParenthesisExpiFunc (cdr inputList) tempNewList (1- CountOperatorFunc))
					(CheckParenthesisExpiFunc (cdr inputList) tempNewList CountOperatorFunc))))))


(defun CheckIsTrueOrFalseFunc (operand)
	(cond ((or (eq operand 'true) (eq operand 'false)) t)
		(t nil)))

(defun RuleOperatorExpiFunc (lhs rhs varOperator)
  (case varOperator
    ('OP_PLUS (+ lhs rhs))
    ('OP_MINUS (- lhs rhs))
    ('OP_DIV (/ lhs rhs))
    ('OP_MULT (* lhs rhs))
    ('KW_IF (CheckIfStatment lhs rhs))
    ('KW_FOR (CheckForStatment lhs rhs))))

(defun CheckIfStatment (lhs rhs)
  (cond
   ((eq lhs 'true) (car rhs))
   ((eq lhs 'false) (cadr rhs))
   (t (CheckSyntaxErrorFunc "IF first parameter has to be a boolean value"))))

(defun CheckForStatment (lhs rhs)
  (cond
   ((eq lhs 'true) (car rhs))
   ((eq lhs 'false) 'NIL_)
   (t (CheckSyntaxErrorFunc "FOR first parameter has to be a boolean value"))))

(defun CheckRuleOperatorExpiFunc (symb)
  (cond
   ((eq symb 'KW_OR) t)
   ((eq symb 'KW_NOT) t)
   ((eq symb 'KW_AND) t)
   ((eq symb 'KW_EQUAL) t)
   ((eq symb 'KW_EQUAL) t)
   ((eq symb 'KW_LESS) t)
   (t nil)))


(defun RuleIdentifier(shortenList)
	(cond
		((eq (car shortenList) 'IDENTIFIER)
		(let ((nameOfVar (car identifierSequence)))
			(setq identifierSequence (cdr identifierSequence))
			(return-from RuleIdentifier (CheckIdentifierToValue nameOfVar))))
	((eq (car shortenList) 'KW_TRUE)
		(return-from RuleIdentifier 'true))
	((eq (car shortenList) 'KW_FALSE)
		(return-from RuleIdentifier 'false))
	((numberp (car shortenList))
		(return-from RuleIdentifier (car shortenList)))
	((eq (car shortenList) 'OP_OP)
		(return-from RuleIdentifier (RuleIdentifier (cdr shortenList))))
	(t (return-from RuleIdentifier (car shortenList)))))

(defun CheckList (tokens)
	
	(if (and (eq (car tokens) 'OP_OP) ( eq (cadr tokens) 'OP_CP))
	nil
	(GetValue tokens)))



(defun CheckValues (currentValue)
  (if currentValue
      (if (eq (car currentValue) 'VALUE)
          (CheckValues (cdr currentValue))
        (CheckSyntaxErrorFunc "Values rule each element should be value"))
    t))

(defun MyList (myList)
  (if (or (null myList) (null (cdr myList)))
      nil
    (cdr (butlast myList))))

(defun PrintList (list)
  (let ((res ""))
    (loop for item in list
          do (setq res (concatenate 'string res (format nil "~a " item))))
    res))


(defun PrintResultFunc ()
	(cond
	((eq (car tokenList) 'COMMENT)
		(print "SYNTAX OK.")
		(terpri))
	((eq (car (last tokenList)) 'COMMENT)
		(setq tokenList (remove 'COMMENT tokenList)))
	(t nil))
	(when (not (null tokenList))
		(let ((perse_res (MyInterpreter 1)))
		(when perse_res
			(print "SYNTAX OK.")
			(terpri)
			(princ "Result: ")
			(write Interpreter)
			(terpri))))
	(setq valueList nil)
	(setq tokenList nil)
	(setq identifierSequence nil)
  
  (main))


(defun main ()
  (let ((str (getLine)))
    (when str
      (let ((ret-value (ProgressLine str)))
        (and (not (string= ret-value "SYNTAX_ERROR"))
             (not (string= ret-value "KW_EXIT"))
             (PrintResultFunc))))))

(cond
 ((null *args*) (main))
 (t (main (first *args*))))
