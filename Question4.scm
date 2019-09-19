#lang racket

; a)
(define separator?
  (lambda (c)
    (or (char=? c #\space) (char=? c #\tab) (char=? c #\newline))
    )
  )

; b)
(define cpy
  (lambda (list)
    (if (null? list) '()
     (let ((firstElement (car list)))
       (if (separator? firstElement)
           '()
           (cons firstElement (cpy (cdr list)))
           )
       )
     )
    )
  )

;(cpy '(#\H #\e #\l #\l #\o #\space #\W #\o #\r #\l #\d))

; c)
(define drop
  (lambda (list)
    (if (null? list)
        '()
        (if (separator? (car list))
            (cdr list)
            (drop (cdr list)))
        )
    )
  )

;(drop '(#\H #\e #\l #\l #\o #\space #\W #\o #\r #\l #\d))

; d)
(define same?
  (lambda (list1 list2)
    (let ((truncatedList1 (cpy list1)))
      (equal? truncatedList1 list2)
      )
    )
  )

;(same? '(#\H #\e #\l #\l #\o #\tab #\W #\o #\r #\l #\d) '(#\H #\e #\l #\l #\o))
;(same? '(#\H #\e #\l #\l #\o #\space #\W #\o #\r #\l #\d) '(#\W #\o #\r #\l #\d))

; e)
(define replace
  (lambda (list matchList replaceList)
    (cond
      [(or (null? matchList) (null? replaceList)) "Error: the second and third lists must not be null."]
      [(null? list) '()]
      [(same? list matchList) (append replaceList (cons (list-ref list (length (cpy list))) (replace (drop list) matchList replaceList)))]
      [else (cons (car list) (replace (cdr list) matchList replaceList))])
    )
  )

;(list->string (replace (string->list "a bird eats a tomato") (string->list "a") (string->list "the")))
