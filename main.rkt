#lang racket/gui
(require net/url)
(require pict)
(require rnrs/sorting-6)
(require db)
(include "date.rkt")
(include "estatistica.rkt")
(include "funcoesGUI.rkt")
(include "funcaoFarm.rkt")


;;==================================================================================================================
;;Definicoes aleatorias

(define fontButton (make-object font% 13 'symbol))
(define lpadrao 950)
(define apadrao 700)

;;==================================================================================================================
;;Pagina de Cadastro

(define frameCadastro (new frame% [label "Cadastro de Animais"]
                           [width 500]
                           [height 450]))
(define painelCadastro (new vertical-panel% [parent frameCadastro]
                            [alignment '(center center)]
                            [style '(auto-vscroll)]))


(define escolhaRaca (new choice% [parent painelCadastro]
                         [label "Raça:   "]
                         [choices (mostraRaca)]))
                         

(define escolhaSexo (new choice% [parent painelCadastro]
                         [label "Sexo:   "]
                         [choices '("Macho" "Femea")]))
                        

(define escolhaCor (new choice% [parent painelCadastro]
                        [label "Cor:   "]
                        [choices (mostraCor)]))
                        

(define anoCad (new text-field%
                          [parent painelCadastro]
                          [label "Ano: "]))

(define mesCad (new text-field%
                          [parent painelCadastro]
                          [label "Mês: "]))

(define diaCad (new text-field%
                          [parent painelCadastro]
                          [label "Dia: "]))
(define botaoDataAtual (new button% [parent painelCadastro]
                            [label "Horário Atual"]
                            [callback (lambda(on-event button-event)
                                        (let [(dataAtual (current-date))]
                                          (send anoCad set-value (number->string (date-year dataAtual)))
                                          (send mesCad set-value (number->string(date-month dataAtual)))
                                          (send diaCad set-value (number->string(date-day dataAtual)))))]))                                    

(define botaoSalvar (new button% [parent painelCadastro]
                         [label "Salvar"]
                         [font fontButton]
                         [callback (lambda(button event)
                                     (enterCadastro))]
                         ))

;;===================================================================================================================
;;Pagina de Produçao

(define frameProducao (new frame% [label "Produçao"]
                           [width 500]
                           [height 450]))

(define painelProducao (new vertical-panel%
                            [parent frameProducao]
                            [alignment '(center center)]
                            [style '(auto-vscroll)]))

(define animaisProducao (new choice%
                             [parent painelProducao]
                             [label "Fêmeas: "]
                             [choices 
                              (for/list [(a (mostraAnimaisSexo "F"))]
                                      (linhaTabela a 7))]))
                             
(define anoProd (new text-field%
                          [parent painelProducao]
                          [label "Ano:  "]))

(define mesProd (new text-field%
                          [parent painelProducao]
                          [label "Mês: "]))

(define diaProd (new text-field%
                          [parent painelProducao]
                          [label "Dia:   "]))
(define PbotaoDataAtual (new button% [parent painelProducao]
                            [label "Horário Atual"]
                            [callback (lambda(on-event button-event)
                                        (let [(dataAtual (current-date))]
                                          (send anoProd set-value (number->string (date-year dataAtual)))
                                          (send mesProd set-value (number->string(date-month dataAtual)))
                                          (send diaProd set-value (number->string(date-day dataAtual)))))])) 
(define quantidadeProduzida (new text-field%
                                 [parent painelProducao]
                                 [label "Quantidade Produzida (Litros): "]))
(define botaoSalvarProducao (new button%
                                 [parent painelProducao]
                                 [label "Salvar Produçao"]
                                 [font fontButton]
                                 [callback (lambda(button event)
                                             (enterProduçao))]))

;;==================================================================================================================
;;Pagina de Diagnostico

(define frameDiagnostico (new frame% [label "Diagnosticos"]
                              [width 450]
                              [height 500]))
(define painelAnimal (new vertical-panel% [parent frameDiagnostico]
                          [alignment '(center center)]
                          [style '(auto-vscroll)]))
(define painelDiag (new horizontal-panel% [parent frameDiagnostico]
                        [alignment '(center center)]
                        [style '(auto-vscroll)]))

;;Objetos do painelAnimal
(define escolhaAnimais (new choice%
                             [parent painelAnimal]
                             [label "Animais: "]
                             [choices 
                              (for/list [(a (mostraAnimais))]
                                      (linhaTabela a 7))]))

(define anoDiag (new text-field%
                          [parent painelAnimal]
                          [label "Ano:  "]))

(define mesDiag (new text-field%
                          [parent painelAnimal]
                          [label "Mês: "]))

(define diaDiag (new text-field%
                          [parent painelAnimal]
                          [label "Dia:   "]))
(define DbotaoDataAtual (new button% [parent painelAnimal]
                            [label "Horário Atual"]
                            [callback (lambda(on-event button-event)
                                        (let [(dataAtual (current-date))]
                                          (send anoDiag set-value (number->string (date-year dataAtual)))
                                          (send mesDiag set-value (number->string(date-month dataAtual)))
                                          (send diaDiag set-value (number->string(date-day dataAtual)))))]))


(define escolhaDoenca (new choice% 
                           [parent painelDiag]
                           [label "Doenças:  "]
                           [choices (mostraDoenca)]
                           [callback
                            (lambda (clique a)
                              (setaRemedio))]))
                           
                          
(define escolhaTratamento (new choice%
                               [parent painelDiag]
                               [label "Tratamento:  "]
                               [choices 
                                '("                ")
                                ]
                               ))

(define painelSalvar (new horizontal-panel% 
                          [parent frameDiagnostico]
                          [alignment '(center center)]))
(define botaoSalvarDiag (new button%
                             [parent painelSalvar]
                             [label "Salvar Diagnostico"]
                             [callback (lambda(button event)
                                         (enterDiagnostico))]))

;;===================================================================================================================
;;Pagina de Estatistica
(define frameEstatistica (new frame% [label "Estatistica"]
                              [width 500]
                              [height 600]))
(define painelGrafico (new horizontal-panel% [parent frameEstatistica]
                           [alignment '(center center)]
                           [style '(auto-vscroll)]))
(define painelEstAnimal (new vertical-panel% [parent frameEstatistica]
                             [alignment '(center center)]
                             [style '(auto-vscroll)]))
(define painelEstRaca (new vertical-panel% [parent frameEstatistica]
                             [alignment '(center center)]
                             [style '(auto-vscroll)]))
(define painelEstSexo (new vertical-panel% [parent frameEstatistica]
                             [alignment '(center center)]
                             [style '(auto-vscroll)]))


(define botaoGrafico (new button% [parent painelGrafico]
                          [label "Gerar Graficos"]
                          [callback (lambda (button event)
                                      (quantidades))]))

(define escolhaEstAnimal (new choice%
                              [parent painelEstAnimal]
                              [label "Animais: "]
                              [choices  
                               (for/list [(a (mostraAnimais))]
                                 (linhaTabela a 7))]))
(define botaoEstAnimal (new button%
                            [parent painelEstAnimal]
                            [label "Gerar Arquivo (Animal)"]
                            [callback (lambda (button event)
                                        (estatisticaAnimal))]))

(define escolhaEstRaca (new choice%
                              [parent painelEstRaca]
                              [label "Raças: "]
                              [choices (mostraRaca)]))
                                 
(define botaoEstRaca (new button%
                            [parent painelEstRaca]
                            [label "Gerar Arquivo (Raca)"]
                            [callback (lambda (button event)
                                        (estatisticaRaca))]))
(define escolhaEstSexo (new choice%
                            [parent painelEstSexo]
                            [label "Animais: "]
                            [choices '("Macho" "Femea")]))
(define botaoEstSexo (new button%
                            [parent painelEstSexo]
                            [label "Gerar Arquivo (Sexo)"]
                            [callback (lambda (button event)
                                        (estatisticaSexo))]))
                            
;;===================================================================================================================
;;Pagina Inicial


(define framePrincipal (new frame% [label "Farmonitor"]
                       [width lpadrao]
                       [height apadrao]))

(define painelPrincipal (new horizontal-panel%
                   [parent framePrincipal]
                   [alignment '(center bottom)]
                   [style '(auto-vscroll)]))

(define botaoCadastro (new button% [parent painelPrincipal]
                           [label "Cadastrar Animais"]
                           [font fontButton]
                           [callback (lambda (button event)
                                       (send frameCadastro show #t))]))
(define botaoProducao (new button% [parent painelPrincipal]
                           [label "Cadastrar Produção"]
                           [font fontButton]
                           [callback (lambda(button event)
                                       (send frameProducao show #t))]))

(define botaoDiagnostico (new button% [parent painelPrincipal]
                              [label "Diagnostico"]
                              [font fontButton]
                              [callback (lambda(button event)
                                          (send frameDiagnostico show #t))]))

(define botaoEstatistica (new button% [parent painelPrincipal]
                              [label "Estatistica"]
                              [font fontButton]
                              [callback (lambda(button event)
                                          (send frameEstatistica show #t))]))

;;===========================================================================================
;;MAIN
(define (main)
  (send framePrincipal show #t))
