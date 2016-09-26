(require scheme/gui/base)
(require (lib "graphics.ss" "graphics"))
(open-graphics) 
(define p_rutas '())


; Ventana laberinto
(define ventana_laberinto (open-viewport "Laberinto" 700 800)) 

; Ventana principal
(define ventana_inicio (new frame% [label "Cargar Laberinto"] 
                            [width 250]  
                            [height 250] 
                            [x 100]
                            [y 100]
                            [style '(no-resize-border)]))

(define tamano (new radio-box%   [label "Seleccionael el tamaño"]   
                       [choices '("15x15" "10x10" )]   
                       [parent ventana_inicio]))

(define bot_inicio (new button% [label "Cargar"] [parent ventana_inicio] [callback (lambda (button event) (star (send tamano get-selection)))]))
(define bot_inicio (new button% [label "Resolver"] [parent ventana_inicio] [callback (lambda (button event) (star1 (send tamano get-selection)))]))

(define tex_tamano (new text-field% [label "Otro tamaño"] [parent ventana_inicio]))
(define boton_ingresar (new button% [parent ventana_inicio] [label "Cargar"][callback (lambda (button event) (tamanoCustom))]))
(define boton_ingresar (new button% [parent ventana_inicio] [label "Resolver"][callback (lambda (button event) (tamanoCustom1))]))

(define (tamanoCustom)
  (cargarLaberinto (leer "custom.txt") (string->number (send tex_tamano get-value))))

(define (tamanoCustom1)
  (resolver (leer "custom.txt") (string->number (send tex_tamano get-value))))




;________definicion de la funcion de inicion del sistema, prepara todas las variables dependiendo del tipo de juego que sea________.
(define (star n)
  (cond ((zero? n)
         (cargarLaberinto (leer "laberinto15x15.txt") 15))
        (else 
         (cargarLaberinto (leer "laberinto10x10.txt") 10))))

;________definicion de la funcion de inicion del sistema, prepara todas las variables dependiendo del tipo de juego que sea________.
(define (star1 n)
  (cond ((zero? n)
         (resolver (leer "laberinto15x15.txt") 15))
         (else 
          (resolver (leer "laberinto10x10.txt") 10))))



(define(cargarLaberinto Laberinto tam)
  (pared Laberinto 0 0 0 '("X") tam))

;Llama los metodos para resolver el laberinto
(define(resolver Laberinto tam)
  (profundidad Laberinto '("&") 0 0 tam)
  (cond((null? p_rutas)
        (message-box "" (string-append "No hay solucion" ))))
  (camino Laberinto(Menor p_rutas '()) tam)
  )

;Retorna la ruta menor
(define (Menor Lista listaVacia)
  (cond((null? Lista)
        listaVacia)
       (else
        (cond ((and(not (null? listaVacia))(<(cargarElementos listaVacia) (cargarElementos (car Lista))))
               (Menor (cdr Lista)  listaVacia))
              
              (else
               (cond ((null? listaVacia)
                      (Menor (cdr Lista) (append (car Lista) listaVacia)))
                     (else
                      (Menor (cdr Lista) (append (car Lista) '())))))))))

;Cantidad de elementos de la ruta
(define cargarElementos (lambda (Lista) 
                          (if (null? Lista) 0 (+ 1 (cargarElementos (cdr Lista))))))


(define (camino M Lista tam)
  (cond ((null? Lista) #t)
        (else
         (caminoDisponible M Lista 0 0 0 (car Lista) tam)
         (sleep/yield 0.1)
         (camino M (cdr Lista) tam))))

;Mueve la imagen por la mejor ruta
(define (caminoDisponible M Lista x y i ele tam)
  (cond ((equal? i tam) '())  
        ((equal? y tam) (caminoDisponible M Lista (+ x 1) 0 (+ i 1) ele tam))
        ((equal? (list-ref (list-ref M x) y) ele) 
         (((draw-pixmap-posn "camino.bmp" 'bmp) ventana_laberinto) (make-posn (+ (* y 40) 40) (+ (* x 40) 40) ) #f) )
        (else (caminoDisponible M Lista x (+ y 1) i ele tam)) ))

;-*****************************************
(define (profundidad M ele ini fin tam)
  (profundidad_aux M ele (list(list (list-ref (list-ref M ini) fin))) tam))

;---------------------------------------------------------
;Realiza la profundidad de la búsqueda
(define (profundidad_aux M ele lista tam) 
  (cond ((null? lista)'())
        ((fin (car(car lista)))
          (set! p_rutas (cons (reverse(car lista)) p_rutas))
          (profundidad_aux M ele (append 
                           (extender M (car lista) tam)
                           (cdr lista))tam)) 
        (else
          (profundidad_aux M ele (append 
                           (extender M (car lista) tam)
                           (cdr lista)) tam))))

;extiende la lista de la ruta enviada
(define (extender M ruta tam)
  (remove-if null? (map (lambda(x) 
                          (cond ((member x  ruta) '())
                                (else (cons x  ruta))))
                        (vecinos M 0 0 0 (car ruta) tam) )))


;--------------------------------------------------------

(define (vecinos M x y i ele tam)
  (cond ((equal? i tam) '())  
        ((equal? y tam) (vecinos M (+ x 1) 0 (+ i 1) ele tam))
        ((equal? (list-ref (list-ref M x) y) ele ) 
         (vecinos_aux M x y tam))
        (else (vecinos M x (+ y 1) i ele tam)) ))

(define (vecinos_aux M x y tam)
  (caminoValido '("X"); se envia una "lista", con las posibles opciones, y el obstaculo
   (append (arriba M x y)
           (izquierda M x y)
           (derecha M x y tam)
           (abajo M x y tam) 
           )))

;Obtiene si es un camino valido
(define (caminoValido E Lista)
  (cond ((null? Lista) Lista)
        ((equal? E (car Lista)) (caminoValido E (cdr Lista)))
        (else (cons (car Lista) (caminoValido E (cdr Lista))))))

(define (izquierda M x y)
  (cond ((>= y 1) 
         (list (list-ref (list-ref M x) (- y 1))))
        (else '()) ))

(define (derecha M x y tam)
  (cond ((<= y (- tam 2));(<= y 3) 
         (list (list-ref (list-ref M x) (+ y 1))))
        (else '()) ))

(define (arriba M x y)
  (cond ((>= x 1) 
         (list (list-ref (list-ref M (- x 1)) y)))
        (else '()) ))

(define (abajo M x y tam)
  (cond ((<= x (- tam 2));(<= x 3)
         (list (list-ref (list-ref M (+ x 1)) y)))
        (else '()) ))


(define (remove-if fun? lista)
  (apply append
          (map (lambda(x) (cond ((fun? x) '())
                                (else (list x))))
               lista)))


;Verifica si una ruta llega al final
(define (fin lista)
  (cond ((equal? (car lista) "B") #t)
        (else #f)))


;Dibuja el laberinto
(define (pared M x y i elemento tam)
  (cond ((equal? i tam) '())   
        ((equal? y tam) (pared M (+ x 1) 0 (+ i 1) elemento tam))
        ((equal? (list-ref (list-ref M x) y) elemento) 
         (((draw-pixmap-posn "pared.bmp" 'bmp) ventana_laberinto) (make-posn (+ (* y 40) 40) (+ (* x 40) 40) ) #f)
         (pared M x (+ y 1) i elemento tam))
        (else (pared M x (+ y 1) i elemento tam))))


; Lee el archivo
(define (leer nombre)
  (leer_archivo2_temp (open-input-file nombre)))

(define (leer_archivo2_temp archivo)
  (let ((lectura (read archivo)))
    (cond ((eof-object? lectura)(close-input-port archivo) '())
          (else (cons lectura (leer_archivo2_temp archivo))))))

; Levanta la progra
(send ventana_inicio show #t)
