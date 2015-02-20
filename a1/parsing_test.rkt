#lang racket
#| Assignment 1 - Parsing TESTS (due Oct 11, noon) 

***Write the names and CDF accounts for each of your group members below.***
<Chongjian Fan>, <g3fancho>
<Jiawei Li>, <g3jiawei>
|#
(require "parsing.rkt")
(require test-engine/racket-tests)

(check-expect (parse-html-tag "<html>hello")'("<html>" "hello"))
(check-expect (parse-html-tag "") (list 'error ""))
(check-expect (parse-html-tag "<hey><html>") (list 'error "<hey><html>"))

(define t1 (make-text-parser "hi"))
(define t2 (make-text-parser ""))
(define t3 (make-text-parser "<sd>"))
(check-expect (t1 "hiya!") (list "hi" "ya!"))
(check-expect (t2 "hiya!") (list "" "hiya!"))
(check-expect (t3 "hiya!") (list 'error "hiya!"))
(check-expect (t3 "") (list 'error ""))

(check-expect (parse-non-special-char "hi") '(#\h "i"))
(check-expect (parse-non-special-char "<hi") (list 'error "<hi"))
(check-expect (parse-non-special-char ">hi") (list 'error ">hi"))
(check-expect (parse-non-special-char "=hi") (list 'error "=hi"))
(check-expect (parse-non-special-char "/hi") (list 'error "/hi"))
(check-expect (parse-non-special-char "\"hi") (list 'error "\"hi"))

(check-expect (parse-plain-char "hi") '(#\h "i"))
(check-expect (parse-plain-char " hi") (list 'error " hi"))
(check-expect (parse-plain-char "<hi") (list 'error "<hi"))
(check-expect (parse-plain-char ">hi") (list 'error ">hi"))
(check-expect (parse-plain-char "=hi") (list 'error "=hi"))
(check-expect (parse-plain-char "/hi") (list 'error "/hi"))
(check-expect (parse-plain-char "\"hi") (list 'error "\"hi"))


(check-expect ((either parse-plain-char parse-html-tag) "hello") (list #\h "ello"))
(check-expect ((either parse-plain-char parse-html-tag) "<html>hello") (list "<html>" "hello"))
(check-expect ((either parse-plain-char parse-html-tag) "<hello")(list 'error "<hello"))
(check-expect ((either parse-plain-char parse-html-tag) "")(list 'error ""))

(check-expect ((both parse-html-tag parse-plain-char) "<html>hello") (list (list "<html>" #\h) "ello"))
(check-expect ((both parse-html-tag parse-plain-char) "<xml>hello")  '(error "<xml>hello"))
(check-expect ((both parse-html-tag parse-plain-char) "<html> hello") '(error "<html> hello"))


(check-expect ((star parse-plain-char) "hi<>") '((#\h #\i) "<>") )
(check-expect ((star parse-plain-char) "!! ?/?") '((#\!#\!) " ?/?") )
(check-expect ((star parse-non-special-char) "  <xml>") '((#\space  #\space)"<xml>"))

(check-expect (parsetag "<html>asdasd") (list (list "html" '()) "asdasd"))
(check-expect (parsetag "<h/tml>asdasd") (list 'error "<h/tml>asdasd"))
(check-expect (parsetag "") (list 'error ""))
(check-expect (parsetag "<html a = b>asdasd") (list 'error "<html a = b>asdasd"))

(check-expect (parsetag-w-attr "<html a = b>asdasd") (list "html" "a = b>asdasd"))
(check-expect (parsetag-w-attr "<html = b>asdasd") (list  "html" "= b>asdasd"))
(check-expect (parsetag-w-attr "<html/ a = b>asdasd") (list  'error "<html/ a = b>asdasd"))
(check-expect (parsetag-w-attr "") (list  'error ""))

(check-expect (trim "  asd") "asd")
(check-expect (trim "asd  ") "asd  ")
(check-expect (trim "a s d") "a s d")

(check-expect (appendx '(1) 2) (list 1 2))
(check-expect (appendx '(1) '(1)) (list 1 1))

(check-expect (parse-attr-helper "class=\"hello\" >Hello, world!</body></html> Other") 
              (list (list "class" "hello") ">Hello, world!</body></html> Other"))

(check-expect (parse-attr-helper "class=hello ></body></html> Other") 
              (list 'error "class=hello ></body></html> Other"))

(check-expect (parse-attr-helper "class   = \"hello\" ></body></html> Other") 
              (list (list "class" "hello") "></body></html> Other"))

(check-expect (parse-attr "class=\"hello\" a=\"b\" ></body></html> Other") 
              (list (list (list "class" "hello")(list "a" "b")) "</body></html> Other"))

(check-expect (parse-attr "class=\"hello\" a=b ></body></html> Other") 
              (list 'error "class=\"hello\" a=b ></body></html> Other"))

(check-expect (parse-msg "This is a message!") 
              (list "This is a message!" ""))

(check-expect (parse-msg "This is a <bad> message!") 
              (list "This is a " "<bad> message!"))

(check-expect (parse-msg "This is //a <bad> message!") 
              (list "This is " "//a <bad> message!"))

(check-expect (parse-close "class=\"hello\" a=b ></body></html> Other" "html") 
              (list #f "class=\"hello\" a=b ></body></html> Other"))

(check-expect (parse-close "</html>class=\"hello\" a=b ></body></html> Other" "html") 
              (list #t "class=\"hello\" a=b ></body></html> Other"))

(check-expect (parse-close "</html>class=\"hello\" a=b ></body></html> Other" "body") 
              (list #f "</html>class=\"hello\" a=b ></body></html> Other"))

(check-expect (parse-html "<html>
<body>
<p>The hr tag defines a horizontal rule:</p>
<p>This is a paragraph.</p>
<p>This is a paragraph.</p>
<p>This is a paragraph.</p>
</body>
</html>") '(("html" () ("body" () ("p" () "The hr tag defines a horizontal rule:") ("p" () "This is a paragraph.") ("p" () "This is a paragraph.") ("p" () "This is a paragraph."))) "") )

(check-expect(parse-html "<html><body class=\"hello\" >Hello, world!</body></html> Other")
  '(("html"
   ()
   ("body"
   (("class" "hello"))
    "Hello, world!"))
  " Other"))

(check-expect(parse-html "<blAh?></blAh?>")
'(("blAh?"
   ()
   "")
  ""))

(check-expect(parse-html "<html><a>apple</a><b><c></c></b></html>")
 '(("html" () ("a" () "apple") ("b" () ("c" () ""))) ""))

(check-expect(parse-html "<html><a>apple</a><b><c></c></b>")
 (list 'error "<html><a>apple</a><b><c></c></b>"))

(check-expect(parse-html "<html>
<body>
    <h1>This is a heading!</h1>
    <div>
        <p>This is a paragraph.</p>
        <h2>This is a subheading.</h2>
        <p>This is another paragraph.</p>
    </div>
</body>
</html>")
'(("html"
  ()
  ("body"
   ()
   ("h1" () "This is a heading!")
   ("div"
    ()
    ("p" () "This is a paragraph.")
    ("h2" () "This is a subheading.")
    ("p" () "This is another paragraph."))))""))

(test)