;#lang racket/load ;quando for incluir em outro arquivo, apagar essa linha

(module bancoDados racket
  
  (define str? (flat-named-contract 'str string?))
  (define lista? (flat-named-contract 'lista list?))
  (define conectado? (flat-named-contract 'conectado connected?))
  (provide/contract
   [instanciaBanco (((nomeBanco str?)) . ->i . (result conectado?))]
   [criarTabelaDB (((nomeTabela (and/c str?)) (nomeBanco (and/c str?)) (strColunas (and/c str?))) . ->i . (result str?))]
   [inserirDados (((nomeTabela (and/c str?)) (nomeBanco (and/c str?)) (colunas (and/c str?)) (dados (and/c str?))) 
                  . ->i . (result str?))]
   [verDB (((nomeTabela (and/c str?)) (nomeBanco (and/c str?))) . ->i . (result (or/c str? lista?)))]
   [removeDados (((nomeTabela (and/c str?)) (nomeBanco (and/c str?)) (nomeDado (and/c str?)) (nomeColuna (and/c str?)))
                 . ->i . (result? str?))]
   [verLinhaDB (((nomeTabela (and/c str?)) (nomeBanco (and/c str?)) (nomeColuna (and/c str?)) (dado (and/c str?))) 
                . ->i . (result (or/c lista? str?)))]
   )
  (require db)
  (define (instanciaBanco nomeBanco)
        (let [(db (sqlite3-connect #:database nomeBanco #:mode 'create))]
          db))

(define (len lista)
  (let [(a 0)]
    (for [(l lista)]
      (set! a (+ a 1)))
    a))

(define (invert lista)
  (let [(a '())]
    (for [(b lista)]
      (set! a (cons b a)))
    a))

(define (contarVirgula str)
  (let [(cont 0)]
    (for [(c str)]
      (cond [(char=? c #\,)
          (set! cont (+ cont 1))]))
    cont))

(define (criarTabelaDB nomeTabela nomeBanco strColunas)
  (let [(db (instanciaBanco nomeBanco))]
    (when (not (table-exists? db nomeTabela)) 
      (let [(str "create table ")]
        (set! str (string-append str nomeTabela))
        (set! str (string-append str " ("))
        (set! str (string-append str strColunas))
        (set! str (string-append str ")"))
        (query-exec db str))))
  "Operação concluida")
                 
(define (inserirDados nomeTabela nomeBanco colunas dados)
  (if (file-exists? nomeBanco)
    (let [(db (instanciaBanco nomeBanco))]
      (let [(str "insert into ")]
        (set! str (string-append str nomeTabela))
        (set! str (string-append str " ("))
        (set! str (string-append str colunas))
        (set! str (string-append str ")"))
        (set! str (string-append str " values ("))
        (set! str (string-append str dados))
        (set! str (string-append str ")"))
        (cond [(table-exists? db nomeTabela)
         ; str
               (query-exec db str) "Operação concluida"]
              [else  "Tabela Inválida. Por favor, crie essa nova Tabela ou use uma tabela válida."]
          )))
    "Arquivo nao encontrado. Por favor, use a função criarDB para criar um Banco de Dados."))
               
(define (verDB nomeTabela nomeBanco)
  (if (file-exists? nomeBanco)
    (let [(db (instanciaBanco nomeBanco))]
      (map (lambda(o) (vector->list o))(query-rows db (string-append "select * from " nomeTabela))))
     "Arquivo nao encontrado. Por favor, use a função criarDB para criar um Banco de Dados."))
                 
(define (removeDados nomeTabela nomeBanco nomeDado nomeColuna)
  (if (file-exists? nomeBanco)
    (let [(db (instanciaBanco nomeBanco))]
      (let [(str "delete from ")]
        (set! str (string-append str nomeTabela))
        (set! str (string-append str " where "))
        (set! str (string-append str nomeColuna))
        (set! str (string-append str " = '"))
        (set! str (string-append str nomeDado))
        (set! str (string-append str "'"))
        (cond [(table-exists? db nomeTabela)
          ;str
               (query-exec db str) "Operação concluida"]
              [else "Tabela Inválida. Por favor, crie essa nova Tabela ou use uma tabela válida."]
            )))
    "Arquivo nao encontrado. Por favor, use a função criarDB para criar um Banco de Dados."))


(define (verLinhaDB nomeTabela nomeBanco nomeColuna dado) 
  (if (file-exists? nomeBanco)
      (let [(db (instanciaBanco nomeBanco))]
        (let [(str "SELECT * FROM ")]
          (set! str (string-append str nomeTabela))
          (set! str (string-append str " WHERE "))
          (set! str (string-append str nomeColuna))
          (set! str (string-append str "='"))
          (set! str (string-append str dado))
          (set! str (string-append str "'"))
          (cond [(table-exists? db nomeTabela)
              ;str
                 (map (lambda(x) (vector->list x)) (query-rows db str))]
                [else "Tabela Inválida. Por favor, crie essa nova Tabela ou use uma tabela válida."]
              )))
      "Arquivo nao encontrado. Por favor, use a função criarDB para criar um Banco de Dados."))
)
(require 'bancoDados)