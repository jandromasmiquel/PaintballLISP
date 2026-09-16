;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiantes: VGP, AMC.
;; 03/05/2026
;; Professor: AOT.
;; Lliurament: primera convocatòria.
;;clase separada que a partir de los datos del estado y de la unidad devuelve la vista de vision de la unidad

(defun get-vision (estat unidad)
    "Calcula la vision de una unidad a partir del estado global y su posicion." 
    (let* ((mapa (get-key 'mapa estat))
           (coord-unidad (get-key 'coordenada unidad))
           (tipo-unidad (get-key 'tipo unidad))
           (rango-vision (obtener-rango-vision tipo-unidad))
           (unidades (get-key 'unidades estat))
           (laboratorios (get-key 'laboratorios estat))
           (dimensiones (obtener-tam-mapa mapa))
           (max-y (car dimensiones))
           (max-x (cadr dimensiones)))
        (cond
            ((null mapa) nil)
            ((null rango-vision) nil)
            (t 
                ;;se costruye la lista en orden inverso para evitar el costo de append, luego se invierte al final para devolverla en orden creciente de coordenadas
                (reverse 
                    (recorrer-mapa-para-vision mapa 0 max-y 0 max-x coord-unidad rango-vision unidades laboratorios nil)
                )
            )
        )
    )
)

;; Recorre todas las casillas del mapa y filtra por rango de vision.
(defun-tco recorrer-mapa-para-vision (mapa y max-y x max-x coord-unidad rango-vision unidades laboratorios resultado)
    "Recorre recursivamente el mapa y construye la lista de casillas en rango de vision."
    (cond
        ;; Fin: hemos recorrido todas las filas
        ((>= y max-y) resultado)
        ;; Fin de columnas en esta fila: pasar a siguiente
        ((>= x max-x)
            (recorrer-mapa-para-vision mapa (+ y 1) max-y 0 max-x coord-unidad rango-vision unidades laboratorios resultado)
        )
        ;; Procesar casilla actual
        (t
            (let ((coord-actual (list x y)))
                (cond
                    ;; Si esta en rango, construir info y agregar a resultado
                    ((<= (dist-cuadrada coord-unidad coord-actual) rango-vision)
                        (let ((info-casilla (construir-info-casilla mapa coord-actual x y unidades laboratorios)))
                            (recorrer-mapa-para-vision
                                mapa y max-y (+ x 1) max-x coord-unidad rango-vision unidades laboratorios
                                (cond
                                    ((null info-casilla) resultado)
                                    (t (cons info-casilla resultado))
                                )
                            )
                        )
                    )
                    ;; Si no esta en rango, continuar
                    (t (recorrer-mapa-para-vision mapa y max-y (+ x 1) max-x coord-unidad rango-vision unidades laboratorios resultado))
                )
            )
        )
    )
)

;; Calcula la distancia euclidiana al cuadrado entre dos coordenadas.
(defun dist-cuadrada (coord1 coord2)
    "Devuelve la distancia euclidiana al cuadrado entre dos coordenadas (x,y)."
    (let ((dx (- (car coord1) (car coord2)))
          (dy (- (cadr coord1) (cadr coord2))))
        (+ (* dx dx) (* dy dy))
    )
)

;; Obtiene el rango de vision segun el tipo de unidad.
(defun obtener-rango-vision (tipo-unidad)
    "Devuelve el rango de vision (en u^2) segun el tipo de unidad."
    (cond
        ((eq tipo-unidad 'base) 64)
        ((eq tipo-unidad 'bolla) 20)
        (t nil)
    )
)

;; Construye la informacion de una casilla segun su contenido.
(defun construir-info-casilla (mapa coord x y unidades laboratorios)
    "Construye la lista de informacion para una casilla en rango de vision."
    (let* ((celda (obtener-casilla-del-mapa mapa x y))
           (tipo-casella (car celda))
           (contenido (cdr celda)))
        (cond
            ;; AGUA: solo (coordenada 'aigua)
            ((eq tipo-casella 'aigua) (list coord 'aigua))
            
            ;; TIERRA
            ((eq tipo-casella 'terra)
                (let* ((color-tierra (car contenido))
                       (unidad-aqui (buscar-unidad-en unidades coord))
                       (lab-aqui (buscar-laboratorio-en laboratorios coord)))
                    (cond
                        ;; TIERRA CON BASE
                        (unidad-aqui
                            (cond
                                ((eq (get-key 'tipo unidad-aqui) 'base)
                                    (list 
                                        coord 'terra color-tierra 'base
                                        (get-key 'equipo unidad-aqui)
                                        (get-key 'colores-pintados unidad-aqui)
                                        nil
                                        nil
                                        nil
                                    )
                                )
                                ;; TIERRA CON BOLLA
                                ((eq (get-key 'tipo unidad-aqui) 'bolla)
                                    (list 
                                        coord 'terra color-tierra 'bolla
                                        (get-key 'equipo unidad-aqui)
                                        (get-key 'colores-pintados unidad-aqui)
                                        (get-key 'color unidad-aqui)
                                        (get-key 'tr-pintar unidad-aqui)
                                        (get-key 'tr-moure unidad-aqui)
                                    )
                                )
                            )
                        )
                        ;; TIERRA CON LAB
                        (lab-aqui
                            (list 
                                coord 'terra color-tierra 'lab
                                (cadr lab-aqui);; equipo del laboratorio
                            )
                        )
                        ;; TIERRA VACIA
                        (t (list coord 'terra color-tierra))
                    )
                )
            )
            (t nil)
        )
    )
)

;; Obtiene una casilla del mapa segun coordenadas x, y.
(defun obtener-casilla-del-mapa (mapa x y)
    "Obtiene la casilla en la posicion (x, y) del mapa."
    (let ((fila (obtener-fila-del-mapa mapa y)))
        (cond
            ((null fila) nil)
            (t (obtener-elemento-fila fila x))
        )
    )
)

;; Obtiene una fila del mapa segun indice y.
(defun obtener-fila-del-mapa (mapa y)
    "Obtiene la fila y del mapa."
    (cond
        ((< y 0) nil)
        ((null mapa) nil)
        ((eq y 0) (car mapa))
        (t (obtener-fila-del-mapa (cdr mapa) (- y 1)))
    )
)

;; Obtiene un elemento de una fila segun indice x.
(defun obtener-elemento-fila (fila x)
    "Obtiene el elemento en posicion x de una fila."
    (cond
        ((< x 0) nil)
        ((null fila) nil)
        ((eq x 0) (car fila))
        (t (obtener-elemento-fila (cdr fila) (- x 1)))
    )
)


;; Busca si existe una unidad en una coordenada dada.
(defun buscar-unidad-en (unidades coord)
    "Busca la unidad en la coordenada especificada, devuelve nil si no hay."
    (cond
        ((null unidades) nil)
        ((equal (get-key 'coordenada (car unidades)) coord) (car unidades))
        (t (buscar-unidad-en (cdr unidades) coord))
    )
)

;; Busca si existe un laboratorio en una coordenada dada.
(defun buscar-laboratorio-en (laboratorios coord)
    "Busca el laboratorio en la coordenada especificada, devuelve nil si no hay."
    (cond
        ((null laboratorios) nil)
        ((equal (caar laboratorios) coord) (car laboratorios))
        (t (buscar-laboratorio-en (cdr laboratorios) coord))
    )
)