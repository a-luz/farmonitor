;;Definiçoes para mostrar animais baseado na inforamçao desejada (cor, raca, sexo).

(define (mostraCor) 
  (let [(db (sqlite3-connect #:database "farmonitor.db" #:mode 'create))]        
    (let [(lista (eliminaR (query-list db "select codCor from cores")))]
      (let [(escolhas(for/list [(a (in-list lista))]
                       (let [(parcial (query-list db (string-append "select cor from cores where codCor='" a "'")))]
                         (cond [(pair? parcial) (car parcial)]))                                                       
                       ))]
        (esticar escolhas)
        ))))

(define (mostraRaca)
  (let [(db (sqlite3-connect #:database "farmonitor.db" #:mode 'create))]        
    (let [(lista (eliminaR (query-list db "select codRaca from racas")))]
      (let [(escolhas(for/list [(a (in-list lista))]
                       (let [(parcial (query-list db (string-append "select raca from racas where codRaca='" a "'")))]
                         (cond [(pair? parcial) (car parcial)]))                                                       
                       ))]
        (esticar escolhas)
        ))))
 
(define (mostraDoenca) (let [(db (sqlite3-connect #:database "farmonitor.db" #:mode 'create))]        
                              (let [(lista (eliminaR (query-list db "select codDoenca from doencas")))]
                                (let [(escolhas(for/list [(a (in-list lista))]
                                                 (let [(parcial (query-list db (string-append "select doenca from doencas where codDoenca='" a "'")))]
                                                   (cond [(pair? parcial) (car parcial)]))                                                       
                                                 ))]
                                  (esticar escolhas)
                                  ))))
                            
(define (mostraAnimaisSexo codSexo)
  (let [(db (sqlite3-connect #:database "farmonitor.db" #:mode 'create))]
    (let [(lista '())]
      (for [(codAnimal (in-query db (string-append "select codAnimal from animais where codSexo=" "'" codSexo "'")))
            (codRaca  (in-query db (string-append "select codRaca from animais where codSexo=" "'" codSexo "'")))
            (codSexo (in-query db (string-append "select codSexo from animais where codSexo=" "'" codSexo "'")))
            (codCor (in-query db (string-append "select codCor from animais where codSexo=" "'" codSexo "'")))
            (codNascimento (in-query db (string-append "select codNascimento from animais where codSexo=" "'" codSexo "'")))]
        (set! lista (cons (list codAnimal (assc codRaca "racas" "raca" "codRaca")
                                (assc codSexo "sexos" "sexo" "codSexo") (assc codCor "cores" "cor" "codCor")
                                (list (assc codNascimento "datas" "ano" "codData")
                                      (assc codNascimento "datas" "mes" "codData")
                                      (assc codNascimento "datas" "dia" "codData"))
                                ) lista))
        )
      lista)))

(define (mostraAnimaisSexo2 codSexo)
  (let [(db (sqlite3-connect #:database "farmonitor.db" #:mode 'create))]
    (let [(lista '())]
      (for [(codAnimal (in-query db (string-append "select codAnimal from animais where codSexo=" "'" codSexo "'")))
            (codRaca  (in-query db (string-append "select codRaca from animais where codSexo=" "'" codSexo "'")))
            (codSexo (in-query db (string-append "select codSexo from animais where codSexo=" "'" codSexo "'")))
            (codCor (in-query db (string-append "select codCor from animais where codSexo=" "'" codSexo "'")))
            (codNascimento (in-query db (string-append "select codNascimento from animais where codSexo=" "'" codSexo "'")))]
        (set! lista (cons (list codAnimal  codRaca codSexo codCor
                                (list (assc codNascimento "datas" "ano" "codData")
                                      (assc codNascimento "datas" "mes" "codData")
                                      (assc codNascimento "datas" "dia" "codData"))
                                ) lista))
        )
      lista)))

(define (mostraAnimaisRaca codRaca)
  (let [(db (sqlite3-connect #:database "farmonitor.db" #:mode 'create))]
    (let [(lista '())]
      (for [(codAnimal (in-query db (string-append "select codAnimal from animais where codRaca=" "'" codRaca "'")))
            (codRaca  (in-query db (string-append "select codRaca from animais where codRaca=" "'" codRaca "'")))
            (codSexo (in-query db (string-append "select codSexo from animais where codRaca=" "'" codRaca "'")))
            (codCor (in-query db (string-append "select codCor from animais where codRaca=" "'" codRaca "'")))
            (codNascimento (in-query db (string-append "select codNascimento from animais where codRaca=" "'" codRaca "'")))]
        (set! lista (cons (list codAnimal #|(assc codRaca "racas" "raca" "codRaca")
                                (assc codSexo "sexos" "sexo" "codSexo") (assc codCor "cores" "cor" "codCor")|#
                                codRaca codSexo codCor
                                (list (assc codNascimento "datas" "ano" "codData")
                                      (assc codNascimento "datas" "mes" "codData")
                                      (assc codNascimento "datas" "dia" "codData"))
                                ) lista))
        )
      lista)))


;;==================================================================================================================================================================================================
;;Definiçoes para cadastrar a data.

(define (cadastrarData ano mes dia)
  (let [(db (sqlite3-connect #:database "farmonitor.db" #:mode 'create))]
    (let [(str (string-append "select codData from datas where ano='" (number->string ano) "' and mes='"
                               (number->string mes) "'  and dia='" (number->string dia) "'"))]
      ;(set! data (string-append (number->string ano) (number->string mes) (number->string dia) data))
      (let [(lista (query-list db str))]
        (let [(codData (match lista
                         [(list a) a]
                         [(list a _) a]
                         [(list a _ _) a]
                         [_ ""]))]
          ;(displayln codData)
          (cond [(not (string=? codData "")) codData]
                [else                        
                 (let [(codOrdenados (ordemID "datas" "farmonitor.db" "codData"))]
                   (cond [(null? codOrdenados) (number->string 1)
                                               (let [(strData (string-append "'" (number->string (+ 1 1)) "', '" (number->string ano) "', '" (number->string mes) "', '" (number->string dia) "'" ))]
                                                 (inserirDados "datas" "farmonitor.db" "'codData', 'ano', 'mes', 'dia'" strData))  ]
                         [else (let [(CodAtual (string->number (list-ref codOrdenados (- (length codOrdenados) 1))))]
                                 (let [(strData (string-append "'" (number->string (+ CodAtual 1)) "', '" (number->string ano) "', '" (number->string mes) "', '" (number->string dia) "'" ))]
                                   (inserirDados "datas" "farmonitor.db" "'codData', 'ano', 'mes', 'dia'" strData))
                                 (number->string (+ CodAtual 1)))]))])
          ))    
      )))




(define (cadastrarHora hora minuto)
  (let [(db (sqlite3-connect #:database "farmonitor.db" #:mode 'create))]
    (let [(str (string-append "select codHora from horas where hora='" (number->string hora) "' and minuto='"
                               (number->string minuto) "'"))]
      ;(set! data (string-append (number->string ano) (number->string mes) (number->string dia) data))
      ;(displayln str)
      (let [(lista (query-list db str))]
        ;(displayln lista)
        (let [(codHora (match lista
                         [(list a) a]
                         [_ ""]))]
          ;(displayln codHora)
          (cond [(not (string=? codHora "")) codHora]
                [else                                  
                 (let [(codOrdenados (ordemID "horas" "farmonitor.db" "codHora"))]
                   (cond [(null? codOrdenados) (number->string 1)
                                               (let [(strHora (string-append "'" (number->string (+ 1 1)) "', '" (number->string hora) "', '" (number->string minuto)"'" ))]
                                                 (inserirDados "horas" "farmonitor.db" "'codHora', 'hora', 'minuto'" strHora))  ]
                         [else (let [(CodAtual (string->number (list-ref codOrdenados (- (length codOrdenados) 1))))]
                                 (let [(strHora (string-append "'" (number->string (+ CodAtual 1)) "', '" (number->string hora) "', '" (number->string minuto) "'"))]
                                   (inserirDados "horas" "farmonitor.db" "'codHora', 'hora', 'minuto'" strHora))
                                 (number->string (+ CodAtual 1)))]))])
          ))    
      )))


(define (ordemID nomeTabela nomeBanco coluna)
   (if (file-exists? nomeBanco)
      (let [(db (instanciaBanco nomeBanco))]
        (let [(str "SELECT ")]
          (set! str (string-append str coluna))
          (set! str (string-append str " FROM "))
          (set! str (string-append str nomeTabela))
          ;(set! str (string-append str " ORDER BY "))
         ; (set! str (string-append str coluna))
        (cond [(table-exists? db nomeTabela) 
               (let [(listaOrdenada (query-list db str))]
                 
                 (map (lambda(b) (number->string b))(sort (map (lambda(a) 
                                                                 (if (string? a)
                                                                 (string->number a)
                                                                 a))listaOrdenada) <)))]
              [else "Tabela Inválida. Por favor, crie essa nova Tabela ou use uma tabela válida."])))
             
      "Arquivo nao encontrado. Por favor, use a função criarDB para criar um Banco de Dados."))


;;==========================================================================================================================================================================================
;;Definiçao para criar "|" na tabela

(define (linhaTabela linha num)
  (let [(str "")]
    (let loop [(lista (esticar linha)) (acc 1)]
      (cond [(not (null? lista))
      (if (number? (car lista))
          (cond [(< acc num) (set! str (string-append str (number->string (car lista)) " | "))
                         (loop (cdr lista) (+ acc 1))]
            [(= acc num) (set! str (string-append str (number->string(car lista))))
                         (loop (cdr lista) (+ acc 1))]
            )
          (cond [(< acc num) (set! str (string-append str (car lista) " | "))
                         (loop (cdr lista) (+ acc 1))]
            [(= acc num) (set! str (string-append str (car lista)))
                         (loop (cdr lista) (+ acc 1))]
            ))]))
    str))
