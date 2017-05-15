;#lang racket
(require db)
(require racket/date)
;(include "2305.rkt")

(define (listaData)
   (let [(db (sqlite3-connect #:database "farmonitor.db" #:mode 'create))]
     (let [(dataAtualSeconds (date->seconds (current-date)))]
       (let [(limite (- dataAtualSeconds (* 30 24 60 60)))]
         (let [(lista '())]
           (set! lista (for/list [(cod (in-query db "select codData from datas"))
                                  (dia (in-query db "select dia from datas"))
                                  (mes (in-query db "select mes from datas"))
                                  (ano (in-query db "select ano from datas"))
                                  ]
                         (list cod dia mes ano)))
          ;(displayln (~a dataAtualSeconds "   " limite))
           (let [(lista2 (for/list [(a (in-list lista))]
                           (match a
                             [(list a b c d) (list (find-seconds 0 0 0 (string->number b) (string->number c) (string->number d)) a)])))]
             ;(displayln lista2)
             (let [(entre? (flat-named-contract 'enter? (and/c (<=/c dataAtualSeconds)(>=/c limite))))]
               (let [(lista3 (filter entre? (map (lambda(x) (car x))lista2)))]
                 
                 (let [(lista4 (for/list [(a (in-list lista3))]
                                 (assoc a lista2)))]
                   lista4)
             
           ))))))))