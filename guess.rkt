;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname guess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))

(require "animals.rkt")

;; An Example is a (cons Sym (listof Sym))
;; Requires: each attribute in the rest is unique

;; A Histogram is a (listof (list Sym Nat))
;; Requires: A symbol can appear in only one pair.

;; An Augmented Histogram (AH) is a (listof (list Sym Nat Nat))
;; Requires: A symbol can appear in only one triple.

;; An Entropy Association List (EAL) is a (listof (list Sym Num))
;; Requires: A symbol can appear in only one pair.

;; A Decision Tree (DT) is one of:
;; * Bool
;; * (list Sym DT DT)


(define seen
  (list
   (list 'squirrel 'small 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'crow 'medium 'flies 'angry)))

;; (collect-attributes examples) produces a list of attributes
;;    contained in example-lst with no duplicates.
;; Example:
(check-expect
 (collect-attributes seen)
 (list 'medium 'flies 'swims 'large 'angry 'small))

;; collect-attributes: (listof Example) -> (listof Sym)
(define (collect-attributes examples)
  (local [(define (collect examples attribute-lst)
            (cond [(empty? examples) attribute-lst]
                  [else (collect (rest examples)
                                 (add-attributes (rest (first examples))
                                                 attribute-lst))]))
          
          (define (add-attributes attributes attribute-lst)
            (cond [(empty? attributes) attribute-lst]
                  [(member? (first attributes) attribute-lst)
                   (add-attributes (rest attributes) attribute-lst)]
                  [else (add-attributes (rest attributes)
                                        (cons (first attributes)
                                              attribute-lst))]))]

    (collect examples empty)))

;; Tests:
(check-expect (collect-attributes empty) empty)


(define split-test
  (list (list 'goose 'animal)
        (list 'squirrel 'animal)
        (list 'rabbit 'animal)))

;; (split-examples examples symbol) produces a list of two lists of examples,
;;    with the first containing the examples containing symbol and the second
;;    containing the examples not containing symbol.
;; Examples:
(check-expect
 (split-examples seen 'goose) ; splitting on a label
 (list (list (list 'goose 'large 'swims 'flies 'angry)
             (list 'goose 'large 'swims 'flies 'angry))
       (list (list 'crow 'medium 'flies 'angry)
             (list 'squirrel 'small 'angry))))
(check-expect
 (split-examples seen 'small) ; splitting on an attribute
 (list (list (list 'squirrel 'small 'angry))
       (list (list 'crow 'medium 'flies 'angry)
             (list 'goose 'large 'swims 'flies 'angry)
             (list 'goose 'large 'swims 'flies 'angry))))

;; split-examples: (listof Example) Sym -> (list (lisof Example) (listof Example))
(define (split-examples examples symbol)
  (local
    [(define (split examples yes-contains no-contains)
       (cond [(empty? examples) (list yes-contains no-contains)]
             [(member? symbol (first examples))
              (split (rest examples)
                     (cons (first examples) yes-contains)
                     no-contains)]
             [else (split (rest examples)
                          yes-contains
                          (cons (first examples) no-contains))]))]
    
    (split examples empty empty)))

;; Tests:
(check-expect (split-examples empty 'goose) '(() ()))
(check-expect
 (split-examples seen 'rabbit)
 (list (list )
       (list (list 'crow 'medium 'flies 'angry)
             (list 'goose 'large 'swims 'flies 'angry)
             (list 'goose 'large 'swims 'flies 'angry)
             (list 'squirrel 'small 'angry))))
(check-expect
 (split-examples split-test 'animal)
 (list (list (list 'rabbit 'animal)
             (list 'squirrel 'animal)
             (list 'goose 'animal))
       (list )))


;; (histogram examples) produces a list of attributes/count pairs, with each
;;    pair indicating how many times that attribute appears in the examples.
;; Example:
(check-expect
 (histogram seen)
 (list
  (list 'small 1) (list 'angry 4) (list 'large 2)
  (list 'swims 2) (list 'flies 3) (list 'medium 1)))

;; histogram: (listof Example) -> Histogram
(define (histogram examples)
  (local
    [(define (count-attributes examples histogram)
       (cond [(empty? examples) histogram]
             [else (count-attributes
                    (rest examples)
                    (ex-update-histogram (rest (first examples))
                                         histogram))]))

     (define (ex-update-histogram attributes histogram)
       (cond [(empty? attributes) histogram]
             [else (ex-update-histogram
                    (rest attributes)
                    (update-histogram (first attributes) histogram))]))

     (define (update-histogram attribute histogram)
       (cond [(empty? histogram)
              (cons (list attribute 1) empty)]
             [(symbol=? attribute (first (first histogram)))
              (cons (list attribute
                          (add1 (second (first histogram))))
                    (rest histogram))]
             [else (cons (first histogram)
                         (update-histogram attribute (rest histogram)))]))]
    
    (count-attributes examples empty)))

;; Tests:
(check-expect (histogram empty) empty)


;; (augment-histogram histogram attributes total) adds any missing attributes to
;;    histogram with counts of zero and adds to each element the number of
;;    examples not containing the attribute by using total.
;; Examples:
(check-expect
 (augment-histogram
  (list (list 'a 100) (list 'c 50))
  (list 'a 'b 'c)
  200)
 (list (list 'a 100 100) (list 'b 0 200) (list 'c 50 150)))
(check-expect
 (augment-histogram empty (list 'x 'y) 10)
 (list (list 'x 0 10) (list 'y 0 10)))

;; augment-histogram: Histogram (listof Sym) Num -> AH
(define (augment-histogram histogram attributes total)
  (local
    [(define (update-pair histogram attribute)
       (cond [(empty? histogram) (list attribute 0 total)]
             [(symbol=? attribute (first (first histogram)))
              (list attribute
                    (second (first histogram))
                    (- total (second (first histogram))))]
             [else (update-pair (rest histogram) attribute)]))]
    
    (cond [(empty? attributes) empty]
          [else (cons (update-pair histogram (first attributes))
                      (augment-histogram histogram
                                         (rest attributes) total))])))

;; Tests:
(check-expect (augment-histogram empty empty 10) empty)


;; (entropy positive-counts negative-counts) produces the entropy of
;;    positive-counts and negative-counts.
;; Examples:
(check-within (entropy (list 'large 126 59) (list 'large 146 669))
              #i0.5663948489858 0.001)
(check-within (entropy (list 'small 17 168) (list 'small 454 361))
              #i0.5825593868115 0.001)
(check-expect (entropy (list 'a 0 100) (list 'b 100 0)) 0.0)

;; entropy: (list Sym Nat Nat) (list Sym Nat Nat) -> Num
(define (entropy positive-counts negative-counts)
  (local
    [(define a (second positive-counts))
     (define b (second negative-counts))
     (define c (third positive-counts))
     (define d (third negative-counts))

     (define (P n m)
       (cond [(> (+ n m) 0) (/ n (+ n m))]
             [else 0.5]))
      
     (define (e p)
       (cond [(zero? p) 0]
             [else (* (- p) (log p 2))]))]

    (+ (* (P (+ a b) (+ c d)) (+ (e (P a b)) (e (P b a))))
       (* (P (+ c d) (+ a b)) (+ (e (P c d)) (e (P d c)))))))


;; (entropy-attributes positive negative) produces a list of attribute/entropy
;;    pairs using positive and negative splits for each attribute.
;; Example:
(check-within
 (entropy-attributes
  (list
   (list 'large 126 59) (list 'angry 161 24)
   (list 'small 17 168) (list 'flies 170 15)
   (list 'swims 162 23) (list 'medium 42 143))
  (list
   (list 'large 146 669) (list 'angry 469 346)
   (list 'small 454 361) (list 'flies 615 200)
   (list 'swims 365 450) (list 'medium 215 600)))
 (list
  (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
  (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
  (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677)) 0.001)

;; entropy-attributes: AH AH -> EAL
(define (entropy-attributes positive negative)
  (cond [(empty? positive) empty]
        [else (cons (list (first (first positive))
                          (entropy (first positive) (first negative)))
                    (entropy-attributes (rest positive) (rest negative)))]))


;; (best-attribute entropies) produces an attribute with the minimum entropy
;;    from entropies.
;; Example:
(check-expect
 (best-attribute
  (list
   (list 'angry #i0.6447688190492) (list 'large #i0.5663948489858)
   (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
   (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677)))
 'large)

;; best-attribute: EAL -> Sym
;; Requires: entropies is non-empty.
(define (best-attribute entropies)
  (local [(define (min-entropy entropies)
            (cond [(empty? (rest entropies)) (second (first entropies))]
                  [else (min (second (first entropies))
                             (min-entropy (rest entropies)))]))

          (define (find-attribute entropy entropies)
            (cond [(= entropy (second (first entropies)))
                   (first (first entropies))]
                  [else (find-attribute entropy (rest entropies))]))]

    (find-attribute (min-entropy entropies) entropies)))

;; Tests:
(check-expect (best-attribute (list (list 'a 0) (list 'b 0))) 'a)


;; (build-dt examples label) produces a DT using examples and label.
;; Example:
(check-expect (build-dt (random-animals 1000) 'emu) false)
(check-expect (build-dt empty 'goose) false)
(check-expect (build-dt seen 'goose) (list 'swims true false))

;; build-dt: (listof Example) Sym -> DT
(define (build-dt examples label)
  (local [(define attributes (collect-attributes examples))
          (define split-list (split-examples examples label))
          (define positive-examples (first split-list))
          (define negative-examples (second split-list))]

    (cond [(empty? positive-examples) false]
          [(empty? negative-examples) true]
          [(empty? attributes)
           (> (length positive-examples)
              (length negative-examples))]
          [else
           (local [(define root-attribute
                     (best-attribute
                      (entropy-attributes
                       (augment-histogram (histogram positive-examples)
                                          attributes
                                          (length positive-examples))
                       (augment-histogram (histogram negative-examples)
                                          attributes
                                          (length negative-examples)))))

                   (define (remove-root positive-examples)
                     (cond [(empty? positive-examples) empty]
                           [else
                            (cons (removed-root (first positive-examples))
                                  (remove-root (rest positive-examples)))]))

                   (define (removed-root example)
                     (cond [(empty? example) empty]
                           [(symbol=? root-attribute (first example))
                            (removed-root (rest example))]
                           [else (cons (first example)
                                       (removed-root (rest example)))]))

                   (define split-root (split-examples examples root-attribute))
                   (define positive-root (remove-root (first split-root)))
                   (define negative-root (second split-root))
                   (define left-subtree (build-dt positive-root label))
                   (define right-subtree (build-dt negative-root label))]
             
             (cond [(equal? left-subtree right-subtree) left-subtree]
                   [else (list root-attribute
                               left-subtree
                               right-subtree)]))])))


;; (train-classifier examples label) produces a predicate for label that 
;;    consumes a list of attributes and produces a decision examples.

;; train-classifier: (listof Example) Sym -> ((listof Sym) -> Bool)
(define (train-classifier examples label)
  (local [(define decision-tree (build-dt examples label))

          (define (classify los)
            (classify-list decision-tree los))
          
          (define (classify-list dt los)
            (cond [(boolean? dt) dt]
                  [(member? (first dt) los) (classify-list (second dt) los)]
                  [else (classify-list (third dt) los)]))]
    classify))

;; Tests:
(define goose? (train-classifier (random-animals 1000) 'goose))
(check-expect (goose? (list 'large 'angry 'flies 'swims)) true)
(check-expect (goose? (list 'small 'angry)) false)
(define squirrel? (train-classifier (random-animals 1000) 'squirrel))
(check-expect (squirrel? (list 'large 'angry 'flies 'swims)) false)
(check-expect (squirrel? (list 'small 'angry)) true)
(define crow? (train-classifier (random-animals 1000) 'crow))
(check-expect (crow? (list 'angry 'flies 'medium)) true)


(define performance-test
  '((squirrel large angry swims flies)
    (goose small angry)))

;; (performance classifier? examples label) produces the sensitivity and
;;    sensitivity for classifier? on examples.
;; Examples:
(check-expect (performance goose? seen 'goose)
              (list 'goose 100 100))

;; performance: (listof Sym -> Bool) (listof Example) Sym -> (list Sym Nat Nat)
(define (performance classifier? examples label)
  (local [(define split (split-examples examples label))
          (define positive-length (length (first split)))
          (define negative-length (length (second split)))
          
          (define (performance/acc examples sensitivity specificity)
            (cond [(empty? examples)
                   (list label
                         (round (* 100 (/ sensitivity positive-length)))
                         (round (* 100 (/ specificity negative-length))))]
                  [(and (symbol=? label (first (first examples)))
                        (not (classifier? (rest (first examples)))))
                   (performance/acc (rest examples)
                                    (sub1 sensitivity) specificity)]
                  [(and (not (symbol=? label (first (first examples))))
                        (classifier? (rest (first examples))))
                   (performance/acc (rest examples)
                                    sensitivity (sub1 specificity))]
                  [else (performance/acc (rest examples)
                                         sensitivity specificity)]))]
    
    (performance/acc examples positive-length negative-length)))

;; Tests:
(check-expect (performance goose? performance-test 'goose)
              (list 'goose 0 0))

