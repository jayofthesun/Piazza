;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname combinedpiazza) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;combined piazza
(require racket/string)
(require 2htdp/image)
(require 2htdp/universe)
(define LENGTH 500)
(define WIDTH 500) 
(define BACKGROUND (rectangle LENGTH WIDTH "solid" "white"))

; A World is one of 
; - EditviewPosts
; - SearchPosts
; INTERPRETATION: Represents two different "views" in your program.

#;(define (world-temp w)
    (cond
      [(editview? w) (editview-temp w)]
      [(search? w) (search-temp w)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; A Edit is a String
; INTERPRETATION: the contents of the post the user is currently
; editing.

(define edit-ex1 "hello wo")
(define edit-ex2 "")
(define edit-ex3 "Yes.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; A History is a List of Strings
; INTERPRETATION: the prior posts received from the server.

(define history-ex1 '())
(define history-ex2 (list "Hello world" "Yes" "no"))
(define history-ex3 (list "a" "b" "c" "d"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; A Search is a String
; INTERPRETATION: the current search term the user is looking for

(define search-ex1 "He")
(define search-ex2 "e")
(define search-ex3 "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-struct editview [edit history search])

; A EditviewPosts is a (make-editview Edit History Search)
; INTERPRETATION: Means the user is viewing all posts and
; potentially typing in a new one.

(define ev-ex1 (make-editview edit-ex1 history-ex1 search-ex1))
(define ev-ex2 (make-editview edit-ex2 history-ex2 search-ex2))
(define ev-ex3 (make-editview edit-ex3 history-ex3 search-ex3))

#;(define (editview-temp ev)
    (...(editview-edit ev)...(editview-history ev)...(editview-search ev)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-struct search [edit history search])

; A SearchPosts   is a (make-search Edit History Search)
; INTERPRETATION: Means the user is trying to view only a subset
; of the existing messages.

(define s-ex1 (make-search edit-ex1 history-ex1 search-ex1))
(define s-ex2 (make-search edit-ex2 history-ex2 search-ex2))
(define s-ex3 (make-search edit-ex3 history-ex3 search-ex3))

#;(define (search-temp s)
    (...(search-edit s)...(search-history s)...(search-search s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define the-server "dictionary.ccs.neu.edu")
(define the-port 10005)
;; World program that aggregates text posts

;; piazza : Forum -> Forum

;(define initial-edit (make-editview "" '() ""))

(define (piazza p)
  (big-bang p
            [name "lee.sunj:8931"]
            [register the-server]
            [port the-port]
            [on-key key-handler]
            [to-draw show-posts]
            [on-receive receive-post]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  DATA DEFINITIONS OVA

;; To create a string representative of the body of a post on a message board
;; String is accepted by the user.
;; Used with on-key
;; key-handler : World KeyEvent -> World/Package

(check-expect (key-handler s-ex1 "f1") ev-ex1)
(check-expect (key-handler s-ex2 "f2") s-ex2)

(check-expect (key-handler ev-ex1 "f1") ev-ex1)
(check-expect (key-handler ev-ex2 "f2") s-ex2)
                           
(check-expect (key-handler ev-ex1 "r") (make-editview "hello wor" history-ex1 search-ex1))
(check-expect (key-handler ev-ex1 "\b") (make-editview "hello w" history-ex1 search-ex1))
(check-expect (key-handler ev-ex2 "H") (make-editview "H" history-ex2 search-ex2))

(define (key-handler w ke)
  (cond
    [(and (key=? ke "f1") (not (editview? w)))
     (make-editview (search-edit w) (search-history w) (search-search w))]
     
    [(and (key=? ke "f2") (not (search? w)))
     (make-search (editview-edit w) (editview-history w) (editview-search w))]

    [else
     (cond
       [(editview? w) (write-body w ke)]
       [(search? w) (search-handler w ke)])]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Key Handler for the user's typing text or backspace while in *search mode*
;; search-handler : Search KeyEvent -> Search

(check-expect (search-handler s-ex1 "r") (make-search edit-ex1 history-ex1 "Her"))
(check-expect (search-handler s-ex1 "\b")(make-search edit-ex1 history-ex1 "H"))
(check-expect (search-handler s-ex2 "H") (make-search edit-ex2 history-ex2 "eH"))
(check-expect (search-handler s-ex2 "\b") (make-search edit-ex2 history-ex2 ""))
(check-expect (search-handler s-ex3 "\r") (make-search edit-ex3 history-ex3 ""))

(define (search-handler s a-key)
  (cond
    [(key=? a-key "\b") (if (= (string-length (search-search s)) 0) ;if empty, 
                            s
                            (make-search
                             (search-edit s)
                             (search-history s)
                             (substring
                              (search-search s) 0 (- (string-length (search-search s)) 1))))]

    [(or (key=? a-key "left")
         (key=? a-key "right")
         (key=? a-key "up")
         (key=? a-key "down")
         (key=? a-key "\b")
         (key=? a-key "\r")
         (key=? a-key "f1")
         (key=? a-key "f2"))
     s]
         

    [else (make-search
           (search-edit s)
           (search-history s)
           (string-append (search-search s) a-key))]))
           

  

              


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Key Handler for the user's typing text, backspace or enter while in *editview mode*
;; write-body: EditView KeyEvent -> EditView/Package

(check-expect (write-body ev-ex1 "r") (make-editview "hello wor" history-ex1 search-ex1))
(check-expect (write-body ev-ex1 "\b") (make-editview "hello w" history-ex1 search-ex1))
(check-expect (write-body ev-ex2 "H") (make-editview "H" history-ex2 search-ex2))
(check-expect (write-body ev-ex2 "\b") (make-editview edit-ex2 history-ex2 search-ex2))
(check-expect (write-body ev-ex3 "\r") (make-package
                                        (make-editview "" history-ex3 search-ex3)
                                        (editview-edit ev-ex3)))


(define (write-body ev a-key)
  (cond
    [(key=? a-key "\b") (if (= (string-length (editview-edit ev)) 0) ;if empty, 
                            ev ;return empty
                            (make-editview ; otherwise take one char off
                             (substring (editview-edit ev) 0 (- (string-length (editview-edit ev)) 1))
                             (editview-history ev) ; and don't touch the rest
                             (editview-search ev)))]
                             
    
    [(key=? a-key "\r")
     (if (= (string-length (editview-edit ev)) 0) ;if empty, 
         (make-editview "" (editview-history ev) (editview-search ev)) ;return empty
         (make-package (make-editview "" (editview-history ev) (editview-search ev))
                       (editview-edit ev)))] ;othwise send it, return ""

    [(or (key=? a-key "left")
         (key=? a-key "right")
         (key=? a-key "up")
         (key=? a-key "down")
         (key=? a-key "f1")
         (key=? a-key "f2"))
     ev]

    [else (make-editview
           (string-append (editview-edit ev) a-key)
           (editview-history ev) (editview-search ev))]))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To display a forum of messagaes in the form of an image
;; Used with to-draw
;; show-posts: World -> Image

(check-expect (show-posts ev-ex1)
              (above
               (text "EDIT MODE" 24 "goldenrod")
               (overlay
                (text (editview-edit ev-ex1) 12 "black")
                BACKGROUND)))

(check-expect (show-posts ev-ex2)
              (above
               (text "EDIT MODE" 24 "goldenrod")
               (overlay
                (above
                 (text (editview-edit ev-ex2) 12 "black")
                 (text "Hello world" 12 "black")
                 (text "Yes" 12 "black")
                 (text "no" 12 "black"))
                BACKGROUND)))

(check-expect (show-posts ev-ex3)
              (above
               (text "EDIT MODE" 24 "goldenrod")
               (overlay
                (above
                 (text (editview-edit ev-ex3) 12 "black")
                 (text "a" 12 "black")
                 (text "b" 12 "black")
                 (text "c" 12 "black")
                 (text "d" 12 "black"))
                BACKGROUND)))

(check-expect (show-posts s-ex2)
              (above
               (text "SEARCH MODE" 24 "steelblue")
               (overlay
                (above
                 (text (search-search s-ex2) 12 "blue")
                 (text "Hello world" 12 "black")
                 (text "Yes" 12 "black"))
                BACKGROUND)))

(define (show-posts w)
  (cond
    [(editview? w)
     (above
      (text "EDIT MODE" 24 "goldenrod")
      (overlay  
       (above
        (text (editview-edit w) 12 "black")
        (draw-post (editview-history w)))
       BACKGROUND))]
    
    [(search? w)
     (above
      (text "SEARCH MODE" 24 "steelblue")
      (overlay
       (above
        (text (search-search w) 12 "blue")
        (draw-post (draw-search (search-history w) (search-search w))))
       BACKGROUND))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
;; Applys given search filter to a given history of posts
;; draw-search:[String] [String -> Boolean] [History] -> [History]
; produces a list from those items on lx for which p holds
#; (define (draw-search h s)
     (local
       ((...
         )))
     (filter ...))
(define (draw-search h s)
  (local
    ((define (helper-contains history-string)
       (if (string-contains? history-string s) #t #f)
       ))
    (filter helper-contains h)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; To display text posts in a given list
; draw-post: History -> Image

(define test-los (cons "hello" (cons "goodbye" '())))
(define test-los2 (cons "ERROR: Nothing in text field" (cons "hello" '())))

(check-expect (draw-post test-los)
              (above
               (text "hello" 12 "black")
               (text "goodbye" 12 "black")))

(check-expect (draw-post test-los2)
              (above
               (text "ERROR: Nothing in text field" 12 "black")
               (text "hello" 12 "black")))

; foldr: [String Image] [String Image -> Image] Image [History] ----> Image
; map: [String Image] [String -> Image] [History] -> [List-of Image]
; sort: [String] [History] [String String -> Boolean] -> [History]
; convert-image: String-> Image
; convert-image: converts a string into an image
; condition: String String -> Boolean
; comdition: dcelares if the given string is an error or not
#; (define (draw-post-temp h)
     (cond
       [(empty? h) ...]
       [else
        (local
          ((...
            )))
        (foldr ...)]))
(define (draw-post h)
  (cond
    [(empty? h) empty-image]  
    [else
     (local
       ((define error "error")
        (define (condition h error)
          (and(>= (string-length h) 5) (string=? (substring h 0 5) error)))
        (define (convert-image h)
          (text h 12 "black"))
        )
       (foldr above empty-image (map convert-image (sort h condition))))]))
        
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; To add an incoming message to the forum
; To be used with on-receive
; receive-post : World String -> Forum

(check-expect (receive-post ev-ex1 "name:message:hello")
              (make-editview edit-ex1 (cons "name:message:hello" history-ex1) search-ex1))


(check-expect (receive-post ev-ex2 "ERROR: Wrong name")
              (make-editview edit-ex2 (cons  "ERROR: Wrong name" history-ex2) search-ex2))


(check-expect (receive-post s-ex1 "name:message:hello")
              (make-search edit-ex1 (cons "name:message:hello" history-ex1) search-ex1))


(check-expect (receive-post s-ex2 "ERROR: Wrong name")
              (make-search edit-ex2 (cons "ERROR: Wrong name" history-ex2) search-ex2))


(define (receive-post w s)
  (cond
    [(editview? w) (make-editview (editview-edit w)
                                  (cons s (editview-history w))
                                  (editview-search w))]
    [(search? w) (make-search (search-edit w)
                              (cons s (search-history w))
                              (search-search w))]))

