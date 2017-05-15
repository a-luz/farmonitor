#lang racket/load
;;criar banco
(include "2305.rkt")
(require db)

;Funçoes:
;(criarFarm)
;(inseirCor2)
;(inserirRaca2)
;(inserirSexo2)
;(inserirDoenca2)
;(inserirCodigo2)

(define (criarFarm)
  (cond [(file-exists? "farmonitor.db") (displayln "banco já criado, deseja sobrescrever(s,n)?") (let [(a (read))]
                                                                                                    (cond [ (equal? a 's)
                                                                                                        (delete-file "farmonitor.db")   
                                                                                                        (criarTabelaDB "racas" "farmonitor.db" "raca TEXT, codRaca TEXT")
                                                                                                        (criarTabelaDB "cores" "farmonitor.db" "cor TEXT, codCor TEXT")
                                                                                                        (criarTabelaDB "sexos" "farmonitor.db" "sexo TEXT, codSexo TEXT")
                                                                                                        (criarTabelaDB "datas" "farmonitor.db" "codData TEXT, ano TEXT, mes TEXT, dia TEXT")
                                                                                                        (criarTabelaDB "horas" "farmonitor.db" "codHora TEXT, hora TEXT, minuto TEXT")
                                                                                                        (criarTabelaDB "diagnosticos" "farmonitor.db" "codAnimal TEXT, codData TEXT, codHora TEXT, codOcorrencia TEXT")
                                                                                                        (criarTabelaDB "codigos" "farmonitor.db" "significado TEXT, cod TEXT")
                                                                                                        (criarTabelaDB "producaoLeite" "farmonitor.db" "codAnimal TEXT, codData TEXT, codHora TEXT, quantidade TEXT")
                                                                                                        (criarTabelaDB "tratamentos" "farmonitor.db" "codAnimal TEXT, codData TEXT, codHora TEXT, medicamentos TEXT")
                                                                                                        (criarTabelaDB "animais" "farmonitor.db" "codAnimal INT PRIMARY KEY, codRaca TEXT, codSexo TEXT, codCor TEXT, codNascimento TEXT")
                                                                                                        (criarTabelaDB "doencas" "farmonitor.db" "doenca TEXT, codDoenca TEXT, medicamento TEXT")
                                                                                                        ]))]
        [else (criarTabelaDB "racas" "farmonitor.db" "raca TEXT, codRaca TEXT")
              (criarTabelaDB "cores" "farmonitor.db" "cor TEXT, codCor TEXT")
              (criarTabelaDB "sexos" "farmonitor.db" "sexo TEXT, codSexo TEXT")
              (criarTabelaDB "datas" "farmonitor.db" "codData TEXT, ano TEXT, mes TEXT, dia TEXT")
              (criarTabelaDB "horas" "farmonitor.db" "codHora TEXT, hora TEXT, minuto TEXT")
              (criarTabelaDB "diagnosticos" "farmonitor.db" "codAnimal TEXT, codData TEXT, codHora TEXT, codOcorrencia TEXT")
              (criarTabelaDB "codigos" "farmonitor.db" "significado TEXT, cod TEXT")
              (criarTabelaDB "producaoLeite" "farmonitor.db" "codAnimal TEXT, codData TEXT, codHora TEXT, quantidade TEXT")
              (criarTabelaDB "tratamentos" "farmonitor.db" "codAnimal TEXT, codData TEXT, codHora TEXT, medicamentos TEXT")
              (criarTabelaDB "animais" "farmonitor.db" "codAnimal INT PRIMARY KEY, codRaca TEXT, codSexo TEXT, codCor TEXT, codNascimento TEXT")
              (criarTabelaDB "doencas" "farmonitor.db" "doenca TEXT, codDoenca TEXT, medicamento TEXT")]))




;;inserir tabelas informações

(define (inserirRaca2)
  (displayln "digite a Raça:")
  (let [(raca (symbol->string (read)))]
    (displayln "digite o Codigo da Raça:")
    (let [(codRaca (symbol->string (read)))]
      (let [(str "")]
        (set! str (string-append str "'"))
        (set! str (string-append str raca))
        (set! str (string-append str "'"))
        (set! str (string-append str ", "))
        (set! str (string-append str "'"))
        (set! str (string-append str codRaca))
        (set! str (string-append str "'"))
        ;str
        (inserirDados "racas" "farmonitor.db" "'raca', 'codRaca'" str)
        ))))

(define (inserirSexo2)
  (displayln "digite o Sexo:")
  (let [(sexo (symbol->string (read)))]
    (displayln "digite o Codigo do Sexo:")
    (let [(codSexo (symbol->string (read)))]
  (let [(str "")]
    (set! str (string-append str "'"))
    (set! str (string-append str sexo))
    (set! str (string-append str "'"))
    (set! str (string-append str ", "))
    (set! str (string-append str "'"))
    (set! str (string-append str codSexo))
    (set! str (string-append str "'"))
    ;str
    (inserirDados "sexos" "farmonitor.db" "'sexo', 'codSExo'" str)
  ))))

(define (inserirDoenca2)
  (displayln "digite a doença:")
  (let [(doenca (symbol->string (read)))]
    (displayln "digite o Codigo da doença:")
    (let [(codDoenca (symbol->string (read)))]
      (displayln "digite os medicamentos disponiveis para o tratamento:")
      (let [(medicamento (symbol->string (read)))]
      (let [(str "")]
        (set! str (string-append str "'"))
    (set! str (string-append str doenca))
    (set! str (string-append str "'"))
    (set! str (string-append str ", "))
    (set! str (string-append str "'"))
    (set! str (string-append str codDoenca))
    (set! str (string-append str "'"))
    (set! str (string-append str ", "))
    (set! str (string-append str "'"))
    (set! str (string-append str medicamento))
    (set! str (string-append str "'"))
    ;str
    (inserirDados "doencas" "farmonitor.db" "'doenca', 'codDoenca', 'medicamento'" str)
    )))))

(define (inserirCor2)
  (displayln "digite a Cor:")
  (let [(cor (symbol->string (read)))]
    (displayln "digite o Codigo da Cor:")
    (let [(codCor (symbol->string (read)))]
      (let [(str "")]
    (set! str (string-append str "'"))
    (set! str (string-append str cor))
    (set! str (string-append str "'"))
    (set! str (string-append str ", "))
    (set! str (string-append str "'"))
    (set! str (string-append str codCor))
    (set! str (string-append str "'"))
    ;str
    (inserirDados "cores" "farmonitor.db" "'cor', 'codCor'" str)
    ))
))

(define (inserirCodigo2)
  (displayln "digite o significado:")
  (let [(sig (symbol->string (read)))]
    (displayln "digite o Codigo:")
    (let [(cod (symbol->string (read)))]
  (let [(str "")]
    (set! str (string-append str "'"))
    (set! str (string-append str sig))
    (set! str (string-append str "'"))
    (set! str (string-append str ", "))
    (set! str (string-append str "'"))
    (set! str (string-append str cod))
    (set! str (string-append str "'"))
    ;str
    (inserirDados "codigos" "farmonitor.db" "'significado', 'cod'" str)
    ))))

;; sem read

(define (inserirRaca raca codRaca)
  (let [(str "")]
    (set! str (string-append str "'"))
    (set! str (string-append str raca))
    (set! str (string-append str "'"))
    (set! str (string-append str ", "))
    (set! str (string-append str "'"))
    (set! str (string-append str codRaca))
    (set! str (string-append str "'"))
    ;str
    (inserirDados "racas" "farmonitor.db" "'raca', 'codRaca'" str)
    ))

(define (inserirSexo sexo codSexo)
  (let [(str "")]
    (set! str (string-append str "'"))
    (set! str (string-append str sexo))
    (set! str (string-append str "'"))
    (set! str (string-append str ", "))
    (set! str (string-append str "'"))
    (set! str (string-append str codSexo))
    (set! str (string-append str "'"))
    ;str
    (inserirDados "sexos" "farmonitor.db" "'sexo', 'codSExo'" str)
  ))

(define (inserirDoenca doenca codDoenca medicamento)
  (let [(str "")]
    (set! str (string-append str "'"))
    (set! str (string-append str doenca))
    (set! str (string-append str "'"))
    (set! str (string-append str ", "))
    (set! str (string-append str "'"))
    (set! str (string-append str codDoenca))
    (set! str (string-append str "'"))
    (set! str (string-append str ", "))
    (set! str (string-append str "'"))
    (set! str (string-append str medicamento))
    (set! str (string-append str "'"))
    ;str
    (inserirDados "doencas" "farmonitor.db" "'doenca', 'codDoenca', 'medicamento'" str)
    ))

(define (inserirCor cor codCor)
  (let [(str "")]
    (set! str (string-append str "'"))
    (set! str (string-append str cor))
    (set! str (string-append str "'"))
    (set! str (string-append str ", "))
    (set! str (string-append str "'"))
    (set! str (string-append str codCor))
    (set! str (string-append str "'"))
    ;str
    (inserirDados "cores" "farmonitor.db" "'cor', 'codCor'" str)
    ))

(define (inserirCodigo sig codSig)
  (let [(str "")]
    (set! str (string-append str "'"))
    (set! str (string-append str sig))
    (set! str (string-append str "'"))
    (set! str (string-append str ", "))
    (set! str (string-append str "'"))
    (set! str (string-append str codSig))
    (set! str (string-append str "'"))
    ;str
    (inserirDados "codigos" "farmonitor.db" "'significado', 'cod'" str)
    ))