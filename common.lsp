;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiantes: VGP, AMC.
;; 03/05/2026
;; Professor: AOT.
;; Lliurament: primera convocatòria.
;; <Funciones comunes para los diferentes modulos.>

;; Lee el contenido completo del mapa desde el fichero.
(defun datos-mapa (ruta)
    (let ((archivo (open ruta :direction :input)))
        (cond
            (archivo
                (let ((datos (read archivo)))
                    (close archivo)
                    datos)
            )
            (t nil)
        )
    )
)

;; Obtiene las dimensiones (x,y) del mapa.
(defun obtener-tam-mapa (mapa-data)
    "Devuelve (filas columnas) del mapa."
    (let ((max-x (length mapa-data)))
        (cond
            ((null mapa-data) (list 0 0))
            (t (list max-x (length (car mapa-data))))
        )
    )
)

;; Devuelve el item n de una lista.
(defun-tco get-n (n lista)
    (cond
        ((null lista) nil)
        ((eq n 0) (car lista))
        (t (get-n (1- n) (cdr lista)))
    )
)

;; Busca una clave dentro de una lista asociada.
(defun get-key (llave lista)
    (cond
        ((null lista) nil)
        ((equal llave (car (car lista))) (cdr (car lista)))
        (t (get-key llave (cdr lista)))
    )
)

;; Inserta o sustituye una clave dentro de una lista asociada.
(defun set-key (llave valor lista)
    (cond
        ((null lista) (list (cons llave valor)))
        ((equal llave (car (car lista))) (cons (cons llave valor) (cdr lista)))
        (t (cons (car lista) (set-key llave valor (cdr lista))))
    )
)

(defun get-unidad-id (unidades id)
    "Devuelve la unidad con el id dado."
    (cond
        ((null unidades) nil)
        (t
            (let ((u (car unidades)))
                (cond
                    ((equal (get-key 'id u) id) u)
                    (t (get-unidad-id (cdr unidades) id))
                )
            )
        )
    )
)