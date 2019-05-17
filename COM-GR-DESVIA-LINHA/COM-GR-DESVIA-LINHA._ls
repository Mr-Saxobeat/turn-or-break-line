;; DESVIA LINHAS - WEIGLAS RIBEIRO                                              
;; Cria um "desvio" de várias linhas que interceptam outras retas.              
;; Pede-se primeiro as retas a desviar e depois as retas que são interceptadas. 
(setenv "DISTCENTRO" "0.1")
(setenv "MARGEMEXT" "0.2")
(defun C:DES(/ oldLayer newLayer SSLDes SSLPer opcao distC margemExt ;|en el itsects i j pt1 pt2 ptm ptm2|;)
  (vl-load-com)
  (GuardaVariaveis (list))
  
  ;Pegar as linhas
  (prompt "\nSelecione linhas a desviar: ")
  (setvar "NOMUTT" 1)
  (setq SSLDes (ssget))
  (setvar "NOMUTT" 0)
  (prompt "\nSelecione as linhas fixas: ")
  (setvar "NOMUTT" 1)
  (setq SSLPer (ssget))
  (setvar "NOMUTT" 0)

  ;Pergunta se confirma os valores atuais, ou se deseja mudar
  (setq opcao
	 (strcase (getstring
	   (strcat"\nConfirmar valores ou Redefinir? <Distância do centro: " (getenv "DISTCENTRO") "/ Margem Exterior: " (getenv "MARGEMEXT") "> [ENTER/Redefinir]: "))))

  ;Se deseja mudar
  (if (equal opcao "R")

    ;Se a opção digitada foi para redefinir
    (progn
      
    ;Pega valor da distância do centro da elipse, eixo menor
    (setq distC (cond
		  ;Se o usuário digitar algum valor
		  ((setq distC (getreal (strcat "\nDigite a distâcia do centro <" (getenv "DISTCENTRO") ">:")))
		   ;Atualiza a variável de ambiente
		   (setenv "DISTCENTRO" (rtos distC 2 2))
		   
		   ;Como último retorno para "setq" retorna o valor da variável de ambiente já atualizada
		   (setq distC (distof (getenv "DISTCENTRO") 2))
		   )

		  ;Se o usuário não digita nada, pega o valor da variável de ambiente
		  ((setq distC (distof (getenv "DISTCENTRO") 2)))
		  )
	  )
    
    ;Pega valor da distância da margem exterior das retas perpendiculares
    (setq margemExt (cond
		      ;Se o usuário digitar algum valor
		      ((setq margemExt (getreal (strcat "\nDigite a margem exterior <" (getenv "MARGEMEXT") ">: ")))

		       ;Atualiza a variável de ambiente
		       (setenv "MARGEMEXT" (rtos margemExt 2 2))

		       ;Como último retorno para "setq", retorna o valor da variável de ambiente já atualizada
		       (setq margemExt (distof (getenv "MARGEMEXT") 2))
		       )
		      
		      ;Se o usuário não digita nada, pega o valor da variável de ambiente
		      ((setq margemExt (distof (getenv "MARGEMEXT") 2))))
		      )
	  )

    ;Se a opção digitada foi "ENTER" para não redefinir
    ;apenas atualiza as variáveis pegando os valores das 'variáveis de ambiente'
    (progn
      (setq distC (distof (getenv "DISTCENTRO") 2))
      (setq margemExt (distof (getenv "MARGEMEXT") 2))
      )
    )

  (setq oldLayer (getvar "CLAYER"))
  (setq en (ssname SSLDES 0))
  (setq el (entget en))
  (setq newLayer (cdr (assoc 8 el)))
  (setvar "CLAYER" newLayer)

  (setq i 0)
  (command "_.Undo" "_BE")
  (setvar "OSMODE" 0)

  ;Inicia o grupo "UNDO"
  
  (repeat (sslength SSLDes)

    ;A cada instância, pegar os pontos de interseção da linha a ser desviada
   (repeat (sslength SSLDes)
     (setq LDes (vlax-ename->vla-object (ssname SSLDes i)))

    ;Lista dos pontos de interseção
    (setq itsects (append))

    ;Armazenar o ponto interseção com cada linha permanente na lista "itsects"
    (setq j 0)
    (repeat (sslength SSLPer)
      (setq LPer (vlax-ename->vla-object (ssname SSLPer j)))
      (setq itsects (append (LM:intersections LDes LPer acextendnone) itsects))
      (setq j (1+ j))
      )
    )
    ;*****************************************************************************************

    ;Se houver mais de uma linha interseptora
    (if (> (length itsects) 1)
      (progn
	;Pega os dois pontos mais distantes entre sí
	(setq pts (maxDistPts itsects))
  	(setq pt1 (car pts))
  	(setq pt2 (cadr pts))
	
	;Ponto médio entre "p1" e "p2"
        (setq ptm (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2)))
    	;ptm2 será usado na criação da elipse, o eixo menor
    	(setq ptm2 (polar ptm (+ (angle pt1 pt2) (/ (* pi 3) 2.0)) distC))

	(setq pt1 (polar pt1 (angle pt2 pt1) margemExt))
        (setq pt2 (polar pt2 (angle pt1 pt2) margemExt))
	)
      
      (progn
	(setq pt1 (car itsects))
	(setq pt2 (car itsects))
	(setq ptm pt1)
	(setq el (entget (ssname SSLDes i)))
	(setq ptm2 (polar ptm (+ (angle (cdr (assoc 10 el)) (cdr (assoc 11 el))) (/ pi 2.0)) distC))
	(setq pt1 (polar pt1 (angle pt1 (cdr (assoc 10 el))) margemExt))
	(setq pt2 (polar pt2 (angle pt2 (cdr (assoc 11 el))) margemExt))
	)
      )
    

    ;O zoom foi usado por causa de erros no momento de selecionar os pontos com a tela muito longe
    (if (/= pt1 pt2)
    	(command "_.ZOOM" "C" ptm (distance pt1 pt2))
        (command "_.ZOOM" "C" ptm (distance pt1 ptm2))
      )

   
    (command "ELLIPSE" "C" ptm ptm2 pt2)

    ;Trim da metade da elipse
    (command "TRIM" (ssname SSLDes i) "" ptm2 "")

    ;Trim da parte da reta que fica dentro da elipse
    (command "TRIM" "L" "" "C" (polar ptm (angle ptm pt1) (/ margemExt 2)) "")
    
    (setq i (1+ i))
    )
  (setvar "CLAYER" oldLayer)
  (command "_.UNDO" "_E")
  
  (RestauraVariaveis)
  (princ)
  )

;Retorna os dois pontos mais distantes entre sí
;de uma lista
(defun maxDistPts (listPts / matrix i j tam r1 r2 pt1 pt2)

  (setq tam (length listPts))
  (setq maxDist 0)

  ;Matriz quadrada que armazena a distância entre dois pontos
  (setq ArDists (vlax-make-safearray vlax-vbDouble (cons 0 tam) (cons 0 tam)))

  (setq i 0)
  
  (repeat tam
    (setq pt1 (nth i listPts))

    (setq j 0)
    (repeat tam
      (setq pt2 (nth j listPts))

      ;Se os pontos são diferentes
      (if (/= i j)
	(progn
	  	;Se a distância entre eles ainda não foi armazenada
      		(if (= (vlax-safearray-get-element ArDists i j) 0)
			(progn
			  (setq dist (distance pt1 pt2))
			  (if (> dist maxDist)
			    (setq maxDist dist
				  r1 pt1
				  r2 pt2
				  )
			    )

			  ;Armazena a distância na matriz tanto em "i X j" quanto em "j X i"
			  (vlax-safearray-put-element ArDists i j dist)
			  (vlax-safearray-put-element ArDists j i dist)
			  )
		  )
	  )
	)

      (setq j (1+ j))
      )

    (setq i (1+ i))
    )
  
  ;Retorna os pontos com maior distância
  (list r1 r2)
  )