#| Assignment 1 - Parsing (due Oct 11, noon)

***Write the names and CDF accounts for each of your group members below.***
<Chongjian Fan>, <g3fancho>
<Jiawei Li>, <g3jiawei>
|#

#lang racket
(provide parse-html-tag make-text-parser
         parse-non-special-char parse-plain-char
         either both star
         parse-html parse-close parse-msg parse-attr parse-attr-helper
         appendx trim parsetag-w-attr parsetag
         )

#|
(parse-html-tag str)
  If str starts with "<html>", returns a pair (list "<html>" rest), where
  rest is the part of str after "<html>".
  Otherwise, returns (list 'error "hi"), signifying an error.

> (parse-html-tag "<html></html>")
'("<html>" "</html>")
> (parse-html-tag "<hey><html>")
'(error "<hey><html>")
|#
(define (parse-html-tag str) 
  (cond
    [(< (string-length str) 6) (list 'error str)]
    [(string=? "<html>" (substring str 0 6)) (list (substring str 0 6)(substring str 6))]
    [else (list 'error str)]
    )
  )


#|
(make-text-parser t)
  Return a parser that tries to read *one* occurrence of t at the
  start of its input.

> (define parse-hi (make-text-parser "hi"))
> (parse-hi "hiya!")
'("hi" "ya!")
> (parse-hi "goodbye hi")
'(error "goodbye hi")
|#
(define (make-text-parser t) 
  (lambda (str) 
    (cond
      [(< (string-length str) (string-length t)) (list 'error str)]
      [(string=? t (substring str 0 (string-length t))) 
       (list (substring str 0 (string-length t)) (substring str (string-length t)))]
      [else (list 'error str)]
      )
    )
  )


#|
(parse-non-special-char str)
  Try to parse *one* non-special character at the start of str.

> (parse-non-special-char "hi")
'(#\h "i")
> (parse-non-special-char "<html>")
'(error "<html>")
|#
(define (parse-non-special-char str) 
  (cond
    [(string=? str "") (list 'error str)]
    [(not 
      (member (string-ref str 0) (list #\< #\> #\= #\" #\/))
      ) 
     (list (string-ref str 0) (substring str 1))
     ]
    [else (list 'error str)]
    )
  )

#|
(parse-plain-char str)
  Try to parse *one* non-special, non-white character at the start of str.

> (parse-plain-char "hi")
'(#\h "i")
> (parse-plain-char " hello!")
'(error " hello!")
|#
(define (parse-plain-char str) 
  (cond
    [(string=? str "") (list 'error str)]
    [(not 
      (member (string-ref str 0) (list #\space #\< #\> #\= #\" #\/))
      ) 
     (list (string-ref str 0) (substring str 1))
     ]
    [else (list 'error str)]
    )
  )


#| Parsing Combinators |#

#|
(either parser1 parser2)

  Return a new parser that does the following:
    - Try to apply parser 1; if success, return that result
    - Otherwise, return the result of applying parser 2

> ((either parse-plain-char parse-html-tag) "hello")
'(#\h "ello")
> ((either parse-plain-char parse-html-tag) "<html>hello")
'("<html>" "hello")
> ((either parse-plain-char parse-html-tag) "<xml>hello")
'(error "<xml>hello")
|#
(define (either parser1 parser2) 
  (lambda (str)
    (cond
      [(equal? (car (parser1 str)) 'error) (parser2 str)]
      [(equal? (car (parser1 str)) '()) (parser2 str)]
      [else (parser1 str)]
      )
    )
  )


#|
(both parser1 parser2)

  Return a new parser that does the following:
    - Apply parser1; if failure, return failure
    - Otherwise, apply parser2 to the rest of the string
      not parsed by parser1
    - If failure, emit failure, together with *original* string
    - If success, return (list data rest), where data is a *LIST*
      containing the data parsed by parser1 and parser2, in that order,
      and rest is the part of the string not parsed by either
      parser1 or parser2.

> ((both parse-html-tag parse-plain-char) "<html>hello")
'(("<html>" #\h) "ello")
> ((both parse-html-tag parse-plain-char) "<xml>hello")
'(error "<xml>hello")
> ((both parse-html-tag parse-plain-char) "<html> hello")
'(error "<html> hello")
|#
(define (both parser1 parser2)
  (lambda (str)
    (
     let ([result1 (parser1 str)])
      (cond
        [(equal? (car result1) 'error) result1]
        [else
         (let ([result2 (parser2 (list-ref result1 1))])
          (cond
           [(equal? (car result2) 'error) (list 'error str)]
           [else (cons 
                  (append (list (car result1)) (list (car result2))) (cdr result2)
                  )]
           )
           )
         ]
        )
      )
    )
  )


#|
(star parser)

  Return a new parser that tries to parse using parser
  0 or more times, returning as its data a list of *all*
  parsed values. This new parser should be *greedy*: it
  always uses the input parser as many times as it can,
  until it reaches the end of the string or gets an error.

  Note that the new parser never returns an error; even if
  the first attempt at parsing fails, the data returned
  is simply '().

> ((star parse-plain-char) "hi")
'((#\h #\i) "")
> ((star parse-plain-char) "hi there")
'((#\h #\i) " there")
> ((star parse-plain-char) "<html>hi")
'(() "<html>hi")
|#

(define (star parser) 
  (lambda (str [result (list '())])
    (let ([result1 (parser str)])
      (cond
        [(equal? (string-length str) 0) (append result '(""))]
        [(equal? (car result1) 'error) (append result (cdr result1))]
        [else ((star parser) (list-ref result1 1) (list(append (first result) (list (car result1)))))]
        )
      )
    )  
  )


#|
(parsetag str) 

if the string starts with the form "<name>"
returns (name, rest), where rest is the rest of the string after <name>.
Otherwise returns ('error, str)

|#
(define (parsetag str) 
  (cond
    [(string=? str "") (list 'error str)]
    [(equal? (equal? (string-ref str 0)#\<) #f) (list 'error str)]
    [else
     (let* ([tag (apply string (car((star parse-plain-char) (substring str 1))))]
            [restlength (string-length tag)])
       (cond
         [(> (+ restlength 2)(string-length str)) (list 'error str)]
         [(equal? (string-ref (substring str (+ restlength 1)) 0) #\>) (list (list(string-append  tag ) '()) (substring str (+ restlength 2)))]
         [else (list 'error str)]
         )
       )
     ] 
    )  
  )

#|
(parsetag-w-attr str) 

Parses the first 
returns (list name, rest), where rest is the rest of the string after "<name".
Otherwise returns (list 'error, str)

|#

(define (parsetag-w-attr str) 
  (cond
    [(string=? str "") (list 'error str)]
    [(equal? (equal? (string-ref str 0)#\<) #f) (list 'error str)]
    [else
     (let* ([tag (apply string (car((star parse-plain-char) (substring str 1))))]
            [restlength (string-length tag)])
       (cond
         [(> (+ restlength 2)(string-length str)) (list 'error str)]
         [(equal? (string-ref (substring str (+ restlength 1)) 0) #\space) (list (string-append  tag ) (substring str (+ restlength 2)))]
         [else (list 'error str)]
         )
       )
     ] 
    )  
  )

#|
(trim str) 

Removes any leading whitespace from str

|#

(define (trim str)
  (string-trim str #:right? #f))

#|
(appendx lst e) 

If e is not a list, append (list e) to list.
Else append lst and e

|#

(define (appendx lst e) 
  (cond
    [(equal? (list? e) #t) (append lst e)]
    [else (append lst (list e))]
   )
  )

#|
(parsetag-w-attr str) 

Parses one attribute and its name. Returns (list(list"name" "value") rest)
where name is the attribute name, and value is its value. Rest is the
string after the attribute. Note that "value" most be enclosed in quotations
in order for them to be considered valid. Returns (list 'error str) if no attribute
is found at the start.

|#

(define (parse-attr-helper str) 
  (cond
    [(string=? str "") (list 'error str)]
    [else
     (let ([attr-name (apply string (car((star parse-plain-char) (trim str))))]
           [rest1 (trim (list-ref((star parse-plain-char) (trim str)) 1))])
       [cond
         [(not(equal? (string-ref rest1 0) #\=))(list 'error str)]
         [else
          (let* (
                [rest2 ((make-text-parser "\"") (trim (substring rest1 1)))]
                [attr-value ((star parse-plain-char) (list-ref rest2 1))]
                [rest3 ((make-text-parser "\"") (list-ref attr-value 1))])
            (cond
              [(equal? (list-ref rest2 0) 'error) (list 'error str)]
              [(equal? (list-ref attr-value 0) 'error) (list 'error str)]
              [(equal? (list-ref rest3 0) 'error) (list 'error str)]
              [else (list (list attr-name (apply string (car attr-value))) (trim(list-ref rest3 1)))]
              )
            )
          ]
         ]
       )
     ]
    )
  )

#|
(parsetag-attr str) 

Parses all the attributes of a tag. Returns (list data, rest), where
data is a list of all the attribute and value pairs, and rest is the str
after ">". Returns (list 'error str) if there is no ">" after the attributes.

|#

(define (parse-attr str)
   (let ([attr ((star parse-attr-helper)str)])
     (cond
        [(equal?(string-ref (list-ref attr 1)0)#\>) (list (list-ref attr 0) (substring (list-ref attr 1)1))]
        [else (list 'error str)]
      )
    )
  )

#|
(parsetag-attr str) 

Parses the message in a tag. Returns (list data, rest), where
data is the message, and rest is the str
after the message. Returns (list 'error str) otherwise.

|#

(define (parse-msg str) 
  (let ([result ((star parse-non-special-char) str) ])
    (list (apply string (car result)) (list-ref result 1))
  )
  )

#|
(parsetag-attr str) 

Returns true is the str starts with </name>, false otherwise

|#

(define (parse-close str name)
  (cond
    [(< (string-length str) (+(string-length name) 1)) (list #f str)]
    [(string=? (string-append "</" name ">") (substring str 0 (+(string-length name) 3)))
         (list #t (substring str (+(string-length name) 3)))]
    [else (list #f str)]
    )
  )

#|(parse-html str)

Parses out one html tag from the given string. Returns (list data rest)
where data is the tree representation of the tag, and rest is the string after it.
The general idea is first parse the tag, then either
parse-html or parse the msg, and combine the results

|#
(define (parse-html str)
 
  (cond
  [(string=? str "") (list 'error  str)]  
  [else
   (let ([tag ((either parsetag (both parsetag-w-attr parse-attr)) (trim str)) ])
    (cond
     [(equal? (list-ref tag 0) 'error) (list 'error str)]
     [else
       (let ([body ((either (star parse-html) parse-msg) (list-ref tag 1))])
         (cond
          [(equal? (list-ref body 0) 'error) (list 'error str)]
          [else
            (let ([rest (parse-close (trim(list-ref body 1)) (list-ref(list-ref tag 0) 0))])
              (cond
                [(equal? (list-ref rest 0) #f) (list 'error str)]
                [(list-ref rest 0) (list (appendx (list-ref tag 0) (list-ref body 0)) (list-ref rest 1))]                                                                                                                                      
              )
             )
            ]
          )
        ) 
       ]
     )
   )
   ]
  )
  )

#| HTML Parsing |#

#|
(parse-html str)

  Parse HTML content at the beginning of str, returning (list data rest),
  where data is the tree representation of the parsed HTML specified in the
  assignment handout, and rest is the rest of str that has not been parsed.

  If the string does not start with a valid html string, return
  (list 'error str) instead.

> (parse-html "<html><body class=\"hello\" >Hello, world!</body></html> Other")
'(("html"
   ()
   ("body"
    (("class" "hello"))
    "Hello, world!"))
  " Other")
> (parse-html "<blAh></blAh>")
'(("blAh"
   ()
   "")
  "")
> (parse-html "<body><p>Not good</body></p>")
'(error "<body><p>Not good</body></p>")
|#

