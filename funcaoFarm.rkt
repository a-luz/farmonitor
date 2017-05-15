;;Callback do cadastro.

(define (enterCadastro) (let [(db (sqlite3-connect #:database "farmonitor.db" #:mode 'create))]
                        (let* [(raca (send escolhaRaca get-string-selection))
                                        (racacomquote (string-append raca "'"))
                                        (strr (string-append "select codRaca from racas where raca='" racacomquote))]
                          (let [(listar (query-list db strr))]
                                     (let [(codRaca (match listar
                                                      [(list a) a]
                                                      [_ ""]))]
                                       (let* [(sexo (send escolhaSexo get-string-selection))
                                              (sexocomquote (string-append sexo "'"))
                                              (strs (string-append "select codSexo from sexos where sexo='" sexocomquote))]
                                         (let [(listas (query-list db strs))]
                                           (let [(codSexo (match listas
                                                            [(list a) a]
                                                            [_ ""]))]
                                             ;(displayln (~a codRaca codSexo))
                                             (let* [(cor (send escolhaCor get-string-selection))
                                                    (corcomquote (string-append cor "'"))
                                                    (strc (string-append "select codCor from cores where cor='" corcomquote))]
                                               (let [(listac (query-list db strc))]
                                                 (let [(codCor (match listac
                                                                 [(list a) a]
                                                                 [_ ""]))]
                                                   ;(displayln (~a codRaca codSexo codCor))
                                                   (let [(ano (string->number (send anoCad get-value)))]
                                                     (let [(mes (string->number (send mesCad get-value)))]
                                                       (let [(dia (string->number (send diaCad get-value)))]
                                                         ;(displayln (~a ano mes dia))
                                                         (let [(codNascimento (cadastrarData ano mes dia))]
                                                           (let [(lista (ordemID "animais" "farmonitor.db" "codAnimal"))
                                                                 (codAtual 1)]
                                                             (cond [(not (null? lista))
                                                                    (set! codAtual(+ (if (string? (list-ref lista (- (length lista) 1)))
                                                                                         (string->number (list-ref lista (- (length lista) 1)))
                                                                                         (list-ref lista (- (length lista) 1)))
                                                                                         1))])
                                                             (let [(strfinal (string-append "'" (number->string codAtual) "', '" codRaca "', '" codSexo "', '" codCor "', '" codNascimento  "'"))]
                                                               (inserirDados "animais" "farmonitor.db" "'codAnimal', 'codRaca', 'codSexo', 'codCor', 'codNascimento'" strfinal)
                                                               
                                                               )
                                        ;(displayln codNascimento)
                                                             )
                                                           ))
                                                       )))
                                                 ))))
                                         
                                         )))))
  (send anoCad set-value "")
  (send mesCad set-value "")
  (send diaCad set-value ""))

;;=====================================================================================================================================================================================
;;Callback da Producao

(define (enterProduçao) 
  (let [(animais (mostraAnimaisSexo "F"))]
    (let [(n (send animaisProducao get-selection))]
      (let* [(animal (list-ref animais n))
             (codAnimal (number->string (car animal)))]
        (let [(ano (string->number(send anoProd get-value)))
              (mes (string->number(send mesProd get-value)))
              (dia (string->number(send diaProd get-value)))
              (quantidade (send quantidadeProduzida get-value))]
          (let* [(codData (cadastrarData ano mes dia))
                 (codHora "1")]
            (let [(strfinal (string-append "'" codAnimal "', '" codData "', '" codHora "', '" quantidade "'"))]
                                        ;strfinal
              (inserirDados "producaoLeite" "farmonitor.db" "'codAnimal', 'codData', 'codHora', 'quantidade'" strfinal)
              ))))))
        (send anoProd set-value "")
        (send mesProd set-value "")
        (send diaProd set-value "")
        (send quantidadeProduzida set-value ""))
;;====================================================================================================================================================================================
;;Callback do Diagnostico

(define (enterDiagnostico) 
  (let [(animais (mostraAnimais))]
    (let [(n (send escolhaAnimais get-selection))]
      (let* [(animal (list-ref animais n))
             (codAnimal (number->string (car animal)))]
        (let [(ano (string->number(send anoDiag get-value)))
              (mes (string->number(send mesDiag get-value)))
              (dia (string->number(send diaDiag get-value)))
              (doenca (send escolhaDoenca get-string-selection))
              (tratamento (send escolhaTratamento get-string-selection))]
          (let* [(codData (cadastrarData ano mes dia))
                 (codHora "1")]
            (let [(codDoenca (assc doenca "doencas" "CodDoenca" "doenca"))]
              (let [(strfinal (string-append "'" codAnimal "', '" codData "', '" codHora "', '" codDoenca "'"))
                    (strfinal2 (string-append "'" codAnimal "', '" codData "', '" codHora "', '" codDoenca "'"))]
                (inserirDados "diagnosticos" "farmonitor.db" "'codAnimal', 'codData', 'codHora', 'codOcorrencia'" strfinal)
                (inserirDados "tratamentos" "farmonitor.db" "'codAnimal', 'codData', 'codHora', 'medicamentos'" strfinal2))))))))
  (send anoDiag set-value "")
  (send mesDiag set-value "")
  (send diaDiag set-value ""))


;;====================================================================================================================================================================================
;;Callbacks das Estatisticas

(define (estatisticaAnimal) 
  (let [(animais (mostraAnimais))]
    (let [(n (send escolhaAnimais get-selection))]
      (let* [(animal (list-ref animais n))
             (codAnimal (number->string (car animal)))]
        (criarArquivoAnimal codAnimal animais n)))))

(define (estatisticaRaca)
  (let* [(raca (send escolhaEstRaca get-string-selection))
         (codRaca (assc raca "racas" "codRaca" "raca"))]
    (criarArquivoRaca codRaca)))

(define (estatisticaSexo)
  (let* [(sexo (send escolhaEstSexo get-string-selection))
         (codSexo (assc sexo "sexos" "codSexo" "sexo"))]
    (criarArquivoSexo codSexo)))
        
;;===================================================================================================================================================================================
;;Seta Remedio

(define (setaRemedio)
  (let [(doenca (send escolhaDoenca get-string-selection))]
    (let [(db (sqlite3-connect #:database "farmonitor.db" #:mode 'create))]        
      (let [(lista (query-list db (string-append "select codMed from doencas where doenca='" doenca "'")))]
        (let [(medicamentos 
               (for/list [(codMed (in-list lista))]
                 (query-list db (string-append "select medicamento from medicamentos where codMed='" codMed "'")) ))]
          (send escolhaTratamento clear)
          (for ([a (in-list (esticar medicamentos))])
            (send escolhaTratamento append a)
            )
          )))
    ))
