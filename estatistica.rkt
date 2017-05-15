;#lang racket

(require pict)
(include "2305.rkt")
(require db)
(require plot)
(require racket/gui)
;(include "funcGui.rkt")

(plot-new-window? #t)
;;tabelar
(define (mostraAnimais)
  (let [(db (sqlite3-connect #:database "farmonitor.db" #:mode 'create))]
    (let [(lista '())]
      (for [(codAnimal (in-query db "select codAnimal from animais"))
            (codRaca  (in-query db "select codRaca from animais"))
            (codSexo (in-query db "select codSexo from animais"))
            (codCor (in-query db "select codCor from animais"))
            (codNascimento (in-query db "select codNascimento from animais"))]
        (set! lista (cons (list codAnimal (assc codRaca "racas" "raca" "codRaca")
                                (assc codSexo "sexos" "sexo" "codSexo") (assc codCor "cores" "cor" "codCor") 
                                (list (assc codNascimento "datas" "ano" "codData")
                                      (assc codNascimento "datas" "mes" "codData")
                                      (assc codNascimento "datas" "dia" "codData"))
                                ) lista))
        )
      lista)))

(define (criarArquivoRaca raca)
  (let [(animaisRaca (mostraAnimaisRaca raca))]
    ;(displayln animaisRaca)
      (call-with-output-file (string-append (assc raca "racas" "raca" "codRaca") ".xml")
        #:mode 'text
        #:exists 'replace
        (lambda(p)
          (displayln (~a "<?xml version='1.0' encoding='UTF-8'?>
<?xml-stylesheet type='text/xsl' href='raca.xsl'?>
<relatorio>
<raca>") p)
          (displayln (~a (assc raca "racas" "raca" "codRaca"))p)
          (displayln (~a "</raca>") p)
          (for [(elemento (in-list animaisRaca))]
            (displayln (~a "<animal>") p)
            ;(displayln (cdr elemento))
            (displayln (~a "<codigo>") p)
            (displayln (~a (apply string-append (cdr (esticar elemento)))) p)
            (displayln (~a "</codigo>") p)
            (displayln (~a "<sexo>") p)
            (match elemento 
              [(list _ _ c _ _)
               (displayln (~a (assc c "sexos" "sexo" "codSexo")) p)]
              [_ (displayln (~a " ") p)])
            (displayln (~a "</sexo>") p)
            (displayln (~a "<cor>") p)
            (match elemento 
              [(list _ _ _ b _)
               (displayln (~a (assc b "cores" "cor" "codCor")) p)]
              [_ (displayln (~a " ") p)])
            (displayln (~a "</cor>") p)
            (displayln (~a "</animal>") p)
            )
          (displayln (~a "</relatorio>") p)
        )
    )
  ))

(define (criarArquivoAnimal codAnimal animais n)
  (let* [(animal (list-ref animais n))
         (codAnimal (number->string (car animal)))]   
    (let [(db (sqlite3-connect #:database "farmonitor.db" #:mode 'create))]
      (let [(str 
             (string-append (car (query-list db (string-append "select codRaca from animais where codAnimal='" codAnimal "'")))
                            (car (query-list db (string-append "select codCor from animais where codAnimal='" codAnimal "'")))
                            (let [(ano(assc (car (query-list db (string-append "select codNascimento from animais where codAnimal='" codAnimal "'")))
                                            "datas" "ano" "codData"))]
                              ano)
                            (let [(mes(assc (car (query-list db (string-append "select codNascimento from animais where codAnimal='" codAnimal "'")))
                                            "datas" "mes" "codData"))]
                              mes)
                            (let [(ano (assc (car (query-list db (string-append "select codNascimento from animais where codAnimal='" codAnimal "'")))
                                             "datas" "dia" "codData"))]
                              ano)
                            ))]
        (call-with-output-file (string-append str ".xml")
          #:mode 'text
          #:exists 'replace
          (lambda(p)
            (displayln (~a "<?xml version='1.0' encoding='UTF-8'?>
<?xml-stylesheet type='text/xsl' href='fazenda.xsl'?>
<relatorio>
<animal>") p)
            (displayln (~a str "</animal>") p)
            
            (for [(lista4 (in-list (listaData)))]
              (displayln (~a "<dia>")p)
              (displayln (~a "<data>" (assc (cadr lista4) "datas"  "ano" "codData") "/" (assc (cadr lista4) "datas" "mes" "codData") "/" (assc (cadr lista4) "datas"  "dia" "codData")  "</data>" )p)
              (let [(b (query-list db (string-append "select quantidade from producaoLeite where codAnimal='" codAnimal "' and codData='" (cadr lista4) "'") ))]
                (cond [(not(null? b))
                       (for [(c (in-list b))]
                         (displayln (~a "<qtd>" c "</qtd>") p))]))
              (displayln (~a "<diag>")p)
              
              (let [(b (query-list db (string-append "select codOcorrencia from diagnosticos where codAnimal='" codAnimal "' and codData='" (cadr lista4) "'") ))]
                (cond [(not(null? b))
                       (for [(c (in-list b))]
                         (displayln (~a "<doencas>" (assc c "doencas" "doenca" "codDoenca") "</doencas>") p))]))
              
              (let [(b (query-list db (string-append "select medicamentos from tratamentos where codAnimal='" codAnimal "' and codData='" (cadr lista4) "'") ))]
                (cond [(not(null? b))
                       (for [(c (in-list b))]
                         (displayln (~a "<tratamento>" c "</tratamento>") p))]))
              (displayln (~a " </diag>
    </dia>")p))
            (displayln (~a "</relatorio>") p)
                        
            )
          ))
      ))
  )

  (define (criarArquivoSexo sexo)
  (let [(animaisRaca (mostraAnimaisSexo2 sexo))]
    ;(displayln animaisRaca)
      (call-with-output-file (string-append (assc sexo "sexos" "sexo" "codSexo") ".xml")
        #:mode 'text
        #:exists 'replace
        (lambda(p)
          (displayln (~a "<?xml version='1.0' encoding='UTF-8'?>
<?xml-stylesheet type='text/xsl' href='sexo.xsl'?>
<relatorio>
<sexo>") p)
          (displayln (~a (assc sexo "sexos" "sexo" "codSexo"))p)
          (displayln (~a "</sexo>") p)
          (for [(elemento (in-list animaisRaca))]
            ;(displayln (cdr elemento))
            (displayln (~a "<animal>") p)
            (displayln (~a "<codigo>") p)
            (displayln (~a (apply string-append (cdr (esticar elemento)))) p)
            (displayln (~a "</codigo>") p)
            (displayln (~a "<raca>") p)
            (match elemento 
              [(list _ c _ _ _)
               (displayln (~a (assc c "racas" "raca" "codRaca")) p)]
              [_ (displayln (~a " ") p)])
            (displayln (~a "</raca>") p)
            (displayln (~a "<cor>") p)
            (match elemento 
              [(list _ _ _ b _)
               (displayln (~a (assc b "cores" "cor" "codCor")) p)]
              [_ (displayln (~a " ") p)])
            (displayln (~a "</cor>") p)
            (displayln (~a "</animal>") p)
            )
          (displayln (~a "</relatorio>") p)
        )
    )
  ))

(define (tabelarAnimais)
   (show-pict (table 7
         (map (λ (x) (text (format "~a" x)))
              (esticar (cons '("Código" "Raça" "Sexo" "Cor" "Ano" "Mês" "Dia") ;(esticar (verDB "animais" "farmonitor.db"))
                            (mostraAnimais))))
         cc-superimpose
         cc-superimpose
         14
         14)))

(define (esticar lista)
  (reverse (trocar lista)))

(define (trocar l)
  (let repita [(list l) (acc '()) ]
    (cond   ((null? list ) acc)
            
            ((not (pair? list)) (print "n lista"))
            (else (repita (cdr list) ( cond ( (not (pair? (car list))) (cons (car list) acc))
                                            (else (repita (car  list)  acc))  
                                         )))
            )     
    
    ) )

(define (assc codigo tabela coluna qcodigo)
  (let [(db (sqlite3-connect #:database "farmonitor.db" #:mode 'create))]
    (let [(lista (query-list db (string-append "select " coluna " from " tabela " where " qcodigo "='" codigo "'")))]
      (match lista
        [(list a _) a]
        [(list a _ _ _) a]
        [(list a _ _) a]
        [(list a _ _ _ _) a]
        [(list a) a]
        ['() ""]
        [_ ""]))))
;;fim

(define (leiteAnimal);;antiga
 (let [(db (sqlite3-connect #:database "farmonitor.db" #:mode 'create))]
   (displayln "Olhe os animais cadastrados com (tabelarAnimais)")
   ;(pict->bitmap (tabelarAnimais))
   (displayln "Digite o código escolhido:")
   (let [(codAnimal (number->string (read)))]
    (let[(lista '())
         (listaCod '())
         (listVetor '())]
     (for [(codData (in-query db (string-append "select codData from producaoLeite where codAnimal=" "'" codAnimal "'")))
           (quantidade (in-query db (string-append "select quantidade from producaoLeite where codAnimal=" "'" codAnimal "'")))
            ]
       (set! listaCod (cons (list codData quantidade) listaCod))
       (set! listVetor (cons (vector (dataBarra codData) (string->number quantidade)) listVetor))
       ;(displayln vetor)
       (set! lista (cons (list (list (assc codData "datas" "ano" "codData")
                                      (assc codData "datas" "mes" "codData")
                                      (assc codData "datas" "dia" "codData")) quantidade
                                ) lista))
        )
     
         (show-pict (table 4
         (map (λ (x) (text (format "~a" x)))
              (esticar (cons '("Dia" "Mês" "Ano" "Quantidade(kg)")
                            lista)))
         cc-superimpose
         cc-superimpose
         14
         14))
      (plot (discrete-histogram listVetor))
       ))))

(define (dataBarra codData);;formato americano
  (let [(lista (list (assc codData "datas" "ano" "codData")
                                      (assc codData "datas" "mes" "codData")
                                      (assc codData "datas" "dia" "codData")))]
    (match lista
      [(list ano mes dia) (string-append ano "/" mes "/" dia)]
      [_ "nao definido"])))

(define (leiteRaca);;antiga
  (let [(db (sqlite3-connect #:database "farmonitor.db" #:mode 'create))]
   (displayln "Digite a Raça:")
   (let* [(raca (symbol->string (read)))
          (codRaca (assc raca "racas" "codRaca" "raca"))]
    ; (displayln codRaca)
     (let [(lista (query-list db (string-append "select codAnimal from animais where codRaca=" "'" codRaca "'")))
           (listaCod '())
           (listaInformacoes '())]
       ;(displayln lista)
       ;(displayln (pair? lista))
       (for [(a (in-list lista))]
         (for [(codData (in-query db (string-append "select codData from producaoLeite where codAnimal=" "'" (number->string a) "'")))
           (quantidade (in-query db (string-append "select quantidade from producaoLeite where codAnimal=" "'" (number->string a) "'")))]
          ; (displayln (~a codData quantidade))
           (set! listaCod (cons (list a codData quantidade) listaCod))
           (set! listaInformacoes (cons (list a (list (assc codData "datas" "ano" "codData")
                                      (assc codData "datas" "mes" "codData")
                                      (assc codData "datas" "dia" "codData")) quantidade
                                ) listaInformacoes))
           ))
       ;(displayln listaCod)
       
         (show-pict (table 5
         (map (λ (x) (text (format "~a" x)))
              (esticar (cons '("Codigo Animal" "Dia" "Mês" "Ano" "Quantidade(kg)")
                            listaInformacoes)))
         cc-superimpose
         cc-superimpose
         14
         14))
       ))))

(define (leiteDia);;testei:nao;antiga
  (let [(db (sqlite3-connect #:database "farmonitor.db" #:mode 'create))]
    (displayln "Data:")
    (displayln "Ano:")
    (let [(ano (read))]
      (displayln "Mês:")
      (let [(mes  (read))]
        (displayln "Dia:")
        (let [(dia (read))]
          (let [(listaData (query-list db (string-append "select codData from datas where ano='" 
                                                       (number->string ano) "' and mes='" (number->string mes) "' and dia='" (number->string dia) "'")))]
            (let [(codData (match listaData
                             [(list a) a]
                             [_ "a"]))]
              (let [(lista (query-rows db (string-append "select codAnimal, quantidade from producaoLeite where codData='" (if (string? codData) codData
                                                                                                                               (number->string codData)) "'")))]
                (table 2
                        (map (λ (x) (text (format "~a" x)))
                             (esticar (cons '("código Animal" "Quantidade(kg)") (map (lambda(x) (vector->list x)) lista)))
                              cc-superimpose
                              cc-superimpose
                              14
                              14))
               ; lista
                )
  )))))))

(define (producaoDiaria ano mes dia)
  (let [(db (sqlite3-connect #:database "farmonitor.db" #:mode 'create))]
    (let [(listaData (query-list db (string-append "select codData from datas where ano='" 
                                                       (number->string ano) "' and mes='" (number->string mes) "' and dia='" (number->string dia) "'")))]
      (let [(codData 
             (match listaData
               [(list a) a]
               [_ ""]))]
        (let [(listaVetor (query-rows db (string-append "select codAnimal, quantidade from producaoLeite where codData='" (if (string? codData) codData
                                                                                                                              (number->string codData)) "'")))]
          ;(displayln listaVetor)
          (call-with-output-file (string-append (if (number? ano)
                                                    (number->string ano)
                                                    ano) 
                                                (if (number? mes)
                                                    (number->string mes)
                                                    mes)
                                                (if (number? dia)
                                                    (number->string dia)
                                                    dia) ".xml")
            #:mode 'text #:exists 'replace
            (lambda(p)
              (displayln (~a "<?xml version='1.0' encoding='UTF-8'?>
<?xml-stylesheet type='text/xsl' href='producaoDiaria.xsl'?>
<relatorio>") p)          
              (displayln (~a "<dia>" ano "/" mes "/" dia "</dia>") p)
              (for [(cadaVaca (in-list listaVetor))]
                (match cadaVaca
                  [(vector codAnimal quantidade)
                   (displayln (~a "<producao>") p)
                   (displayln (~a "<codigo>" (assc codAnimal "animais" "codRaca" "codAnimal") (assc codAnimal "animais" "codSexo" "codAnimal") 
                                  (assc codAnimal "animais" "codCor" "codAnimal") (assc (assc codAnimal "animais" "codNascimento" "codAnimal") "datas" "ano" "codData") 
                                  (assc (assc codAnimal "animais" "codNascimento" "codAnimal") "datas" "mes" "codData")
                                  (assc (assc codAnimal "animais" "codNascimento" "codAnimal") "datas" "dia" "codData")"</codigo>") p)
                   (displayln (~a "<quantidade>" quantidade "</quantidade>") p)
                   (displayln (~a "</producao>") p)]
                  [_ ""]
                  )
                )
              (displayln (~a "</relatorio>") p)
              )
            )
          )
        )
      )
    )
  )

(define (quantidades)
  (let [(db (sqlite3-connect #:database "farmonitor.db" #:mode 'create))]
    (let [(sstr 
    (call-with-output-string 
    (lambda(a)
      (let [(animais (length (query-list db "select codAnimal from animais")))]
        (let [(lista (eliminaR (query-list db "select codRaca from racas")))]
          (let [(listaQnt '())
                (listVetor '())
                (contador 0)]
            (for [(a (in-list lista))]
              (set! listaQnt (cons (list a (length (query-list db (string-append "select codAnimal from animais where codRaca='" a "'")))) listaQnt
                                   ))
              (set! listVetor (cons (vector (assc a "racas" "raca" "codRaca") (length (query-list db (string-append "select codAnimal from animais where codRaca='" a "'")))) listVetor))
              (set! contador (+ contador (length (query-list db (string-append "select codAnimal from animais where codRaca='" a "'")))))
              )
            (set! listVetor (cons (vector "Não identificado" (- animais contador)) listVetor))
            (displayln (~a "Número:" animais) a)
            (for [(l listaQnt)]
              (displayln (~a "Raca: " (assc (car l) "racas" "raca" "codRaca") " Quantidade: " (cadr l)) a))
            (displayln (~a "Não identificado: " (- animais contador)) a)
            ;(displayln listVetor)
            (parameterize ([plot-x-tick-label-anchor  'top-right]
                           [plot-x-tick-label-angle   30])(plot (discrete-histogram listVetor)))
          )
        )
      (let [(lista (eliminaR (query-list db "select codSexo from sexos")))]
        (let [(listaQnt '())
              (listVetor '())]
          (for [(a (in-list lista))]
            (set! listaQnt (cons (list a (length (query-list db (string-append "select codAnimal from animais where codSexo='" a "'")))) listaQnt
                                 ))
            (set! listVetor (cons (vector (assc a "sexos" "sexo" "codSexo") (length (query-list db (string-append "select codAnimal from animais where codSexo='" a "'")))) listVetor))
            )
          (displayln (~a "Números:" animais) a)
          (for [(l listaQnt)]
            (displayln (~a "Sexo: " (assc (car l) "sexos" "sexo" "codSexo") " Quantidade: " (cadr l)) a)) 
          (parameterize ([plot-x-tick-label-anchor  'top-right]
                 [plot-x-tick-label-angle   30])(plot (discrete-histogram listVetor)))))))))]
    sstr)))

(define pertenece (lambda (X L) 
                    (if (null? L) #f (if (equal? (car L) X) #t (pertenece X (cdr L))))))

(define eliminaR (lambda (L)
                   (if (null? L) '()
                       (if (not (pertenece (car L) (cdr L)))
                           (cons (car L) (eliminaR (cdr L)))
                           (eliminaR (cdr L))))))


