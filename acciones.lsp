;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiantes: VGP, AMC.
;; 03/05/2026
;; Professor: AOT.
;; Lliurament: primera convocatòria.
;; Acciones del motor: reciben argumentos de accion, estado y unidad.

(defun accio-crea-bolla (args estat unidad)
    "Crea una bolla y ejecuta sus acciones inmediatamente en el mismo turno."
    (cond
        ((not (eq (get-key 'tipo unidad) 'base)) estat)
        ((or (null args) (null (cdr args))) estat)
        (t
            (let* (
                (color-propio (car args))
                (coordenada (cadr args))
                (equipo (get-key 'equipo unidad))
                (pintura (get-key 'pintura estat))
                )
                (cond
                    ((and (equal equipo 'e1) (> 50 (car pintura))) estat)
                    ((and (equal equipo 'e2) (> 50 (cadr pintura))) estat)
                    (t 
                        (let*(
                            (mapa-valido (coordenada-en-mapa-p estat coordenada))
                            (id-nuevo (+ 1 (max-id-unidad (get-key 'unidades estat))))
                            (unidad-nova 
                                (list
                                    (cons 'id id-nuevo)
                                    (cons 'tipo 'bolla)
                                    (cons 'equipo equipo)
                                    (cons 'coordenada coordenada)
                                    (cons 'colores-pintados nil)
                                    (cons 'color color-propio)
                                    (cons 'tr-pintar 0)
                                    (cons 'tr-moure 0)))
                            (unidades-actualizadas (append (get-key 'unidades estat) (list unidad-nova)))
                            (estat-con-bolla (set-key 'unidades unidades-actualizadas estat)))
                            (pintar-crea-bolla coordenada color-propio equipo)
                            (cond
                                ((equal equipo 'e1) (set-key 'pintura (list (- (car pintura) 50) (cadr pintura)) estat-con-bolla))
                                ((equal equipo 'e2) (set-key 'pintura (list (car pintura) (- (cadr pintura) 50)) estat-con-bolla))
                            )
                        )
                    )
                )
            )
        )
    )
)

(defun accio-pinta (args estat unitat)
    "Pinta la coordenada objetivo con el color de la bolla, actualizando unidades/labs/ganador."
    (let* ((coordenada (car args))
           (color-disparo (get-key 'color unitat))
           (equipo-disparo (get-key 'equipo unitat)))
        (cond
            ((null color-disparo) estat)
            (t
                (let* 
                    (
                    (mapa (get-key 'mapa estat))
                    (estat0 (set-key 'mapa (actualiza-mapa mapa (car coordenada) (cadr coordenada) color-disparo coordenada) estat))
                    (estat1 (pinta-laboratori estat0 coordenada equipo-disparo))
                    (estat2 (pinta-unidad-en-coord estat1 coordenada color-disparo))
                    (unidades (get-key 'unidades estat2))
                    (unidad-estado (get-unidad-id unidades (get-key 'id unitat)))
                    (coordenada-unidad (get-key 'coordenada unidad-estado))
                    (coste-disparo (get-coste-disp coordenada-unidad color-disparo estat))
                    (tr-pintar (get-key 'tr-pintar unidad-estado))
                    (unitat-final (set-key 'tr-pintar (+ tr-pintar coste-disparo) unidad-estado))
                    (unidades-actualizadas (actualiza-unidad unidades unidad-estado unitat-final))
                    )
                    (set-key 'unidades unidades-actualizadas estat2)
                )
            )
        )
    )
)

(defun get-coste-disp (origen color-disparo estat)
    "Calcula el coste de disparo basado en la distancia entre origen y destino."
    (let* (
        (mapa (get-key 'mapa estat))
        (color-celda (get-color-pos origen 0 0 mapa (car mapa)))
        )
        (+ 3
            (cond
                ((equal color-celda color-disparo) 0)
                (t 6)
            )
        )
    )
)

(defun accio-mou (args estat unitat)
    "Mueve la unidad actuante a la coordenada objetivo."
    (let* 
        (
            (desti (car args))
            (id-unitat (get-key 'id unitat))
            (unidades (get-key 'unidades estat))
            (inici (get-key 'coordenada unitat))
            (equip (get-key 'equipo unitat))
            (col (get-key 'color unitat))
            (colores-pintados (get-key 'colores-pintados unitat))
            (mapa (get-key 'mapa estat))
            (color-inici (get-color-pos inici 0 0 mapa (car mapa)))
            (tr-moure (get-key 'tr-moure unitat))
            (color-desti (get-color-pos desti 0 0 mapa (car mapa)))
            (coste-mov (get-coste-rec inici desti color-desti col))
            (unidades-actualizadas (actualiza-coordenada-unitat unidades id-unitat desti tr-moure coste-mov))
        )
        (cond
            ((not (coordenada-en-mapa-p estat desti)) estat)
            (t 
                (pintar-mou-bolla inici color-inici desti col equip colores-pintados)
                (set-key 'unidades unidades-actualizadas estat)
            )
        )
    )
)

(defun accio-escriu-memoria (memoria-nueva estat equipo)
    (let ((memorias (get-key 'memoria estat)))
        (cond
            ((equal 'e1 equipo)
                (set-key 'memoria (list memoria-nueva (cadr memorias)) estat)
            )
            (t
                (set-key 'memoria (list (car memorias) memoria-nueva) estat)
            )
        )
    )
)

(defun get-coste-rec (origen destino color-desti color-bola)
    "Calcula el coste de movimiento basado en la distancia y el color de la celda de destino."
    (* 1
       (cond
           ((and (not (= (car origen) (car destino)))
                 (not (= (cadr origen) (cadr destino))))
            1.4142)
           (t 1)
       )
       (cond
           ((equal color-desti color-bola) 1)
           (t 3)
       )
    )
)

(defun max-id-unidad (unidades)
    "Devuelve el mayor id presente en la lista de unidades."
    (cond
        ((null unidades) 0)
        (t (max-id-unidad-rec unidades 0))
    )
)

(defun max-id-unidad-rec (unidades actual)
    (cond
        ((null unidades) actual)
        (t
            (let ((id (get-key 'id (car unidades))))
                (max-id-unidad-rec (cdr unidades)
                    (cond
                        ((> id actual) id)
                        (t actual)
                    )
                )
            )
        )
    )
)

(defun coordenada-en-mapa-p (estat coordenada)
    "Comprueba si una coordenada cae dentro de los limites del mapa actual."
    (let* ((mapa (get-key 'mapa estat))
           (dimensiones (obtener-tam-mapa mapa))
           (filas (car dimensiones))
           (columnas (cadr dimensiones))
           (x (car coordenada))
           (y (cadr coordenada)))
        (cond
            ((or (null coordenada) (null mapa)) nil)
            ((or (< x 0) (< y 0)) nil)
            ((or (>= x columnas) (>= y filas)) nil)
            (t t)
        )
    )
)

(defun pinta-laboratori (estat coordenada equipo)
    "Si hay laboratorio en la coordenada, lo captura para el equipo indicado."
    (let ((labs (get-key 'laboratorios estat)))
        (set-key 'laboratorios (captura-lab-en-coord labs coordenada equipo) estat)
    )
)

(defun captura-lab-en-coord (labs coordenada equipo)
    (cond
        ((null labs) nil)
        (t
            (let* ((lab (car labs))
                   (coord-lab (car lab)))
                (cond
                    ((equal coord-lab coordenada)
                        (captura-lab (car coordenada) (cadr coordenada) equipo)
                        (cons (list coordenada equipo)
                              (cdr labs))
                    )
                    (t
                        (cons lab (captura-lab-en-coord (cdr labs) coordenada equipo))
                    )
                )
            )
        )
    )
)

(defun pinta-unidad-en-coord (estat coordenada color)
    "Añade color de pintado a la unidad en coordenada y resuelve explosiones."
    (let* ((unidades (get-key 'unidades estat))
           (resultado (pinta-unidad-en-coord-rec unidades coordenada color nil))
           (unidades-finales (car resultado))
           (unitat-pintada (cadr resultado)))
        (cond
            ((null unitat-pintada)
                (set-key 'unidades unidades-finales estat)
            )
            ((eq (get-key 'tipo unitat-pintada) 'base)
                (cond
                    ((eq 3 (length (get-key 'colores-pintados unitat-pintada)))
                        (let* (
                            (equipo-perdedor (get-key 'equipo unitat-pintada))
                            (estat-sin-unidad (set-key 'unidades unidades-finales estat)))
                            (cond
                                ((eq equipo-perdedor 'e1) (set-key 'ganador 'e2 estat-sin-unidad))
                                ((eq equipo-perdedor 'e2) (set-key 'ganador 'e1 estat-sin-unidad))
                                (t estat-sin-unidad)
                            )
                        )
                    )
                    (t 
                        (dibujar-base (car coordenada) (cadr coordenada) (get-key 'equipo unitat-pintada) (get-key 'colores-pintados unitat-pintada))
                        (set-key 'unidades unidades-finales estat)
                    )
                )
            )
            ((and (eq (get-key 'tipo unitat-pintada) 'bolla) (> 2 (length (get-key 'colores-pintados unitat-pintada))))
                (dibuja-unidad (car coordenada) (cadr coordenada) (get-key 'color unitat-pintada) (get-key 'equipo unitat-pintada) (get-key 'colores-pintados unitat-pintada))
                (set-key 'unidades unidades-finales estat)
            )
            (t
                (set-key 'unidades unidades-finales estat)
            )
        )
    )
)

(defun-tco pinta-unidad-en-coord-rec (unidades coordenada color acumuladas)
    (cond
        ((null unidades) (list (reverse acumuladas) nil))
        (t
            (let* (
                (u (car unidades))
                (coord-u (get-key 'coordenada u))
                (color-u (get-key 'color u))
                (tipo-u (get-key 'tipo u)))
                (cond
                    ((and (equal coord-u coordenada) (not (equal color-u color))) ;;si es del mismo color no se pinta
                        (let* (
                            (colores-actuales (get-key 'colores-pintados u))
                            (colors-nous (añadir-color colores-actuales color))
                            (equipo-u (get-key 'equipo u))
                            (u-act (set-key 'colores-pintados colors-nous u)))
                            (cond
                                ((and (eq tipo-u 'bolla) (eq (length colors-nous) 2))
                                    (list (append (reverse acumuladas) (cdr unidades)) u-act) ;; Unidad eliminada, no se añade a acumuladas ni a resultado
                                )
                                ((and (eq tipo-u 'base) (eq (length colors-nous) 3))
                                    (list (append (reverse acumuladas) (cdr unidades)) u-act) ;; Unidad eliminada, no se añade a acumuladas ni a resultado
                                )
                                (t
                                    (list (append (reverse acumuladas) (list u-act) (cdr unidades)) u-act) ;; Unidad actualizada
                                )
                            )
                        )
                    )
                    (t
                        (pinta-unidad-en-coord-rec (cdr unidades) coordenada color (cons u acumuladas))
                    )
                )
            )
        )
    )
)

(defun actualiza-coordenada-unitat (unidades id-unitat desti tr-moure coste-mov)
    (cond
        ((null unidades) nil)
        (t
            (let ((u (car unidades)))
                (cond
                    ((eq (get-key 'id u) id-unitat)
                        (cons (set-key 'coordenada desti (set-key 'tr-moure (+ tr-moure coste-mov) u))
                              (actualiza-coordenada-unitat (cdr unidades) id-unitat desti tr-moure coste-mov))
                    )
                    (t
                        (cons u (actualiza-coordenada-unitat (cdr unidades) id-unitat desti tr-moure coste-mov))
                    )
                )
            )
        )
    )
)

(defun añadir-color (colors color)
    "Añade un color si no estaba presente."
    (cond
        ((null colors) (list color))
        ((member color colors) colors)
        (t (cons color colors))
    )
)

(defun equipo-rival (equipo)
    (cond
        ((eq equipo 'e1) 'e2)
        ((eq equipo 'e2) 'e1)
        (t nil)
    )
)

(defun reverse-list (lst)
    "Reverse recursivo para evitar depender de implementaciones externas."
    (reverse-list-rec lst nil)
)

(defun reverse-list-rec (lst acc)
    (cond
        ((null lst) acc)
        (t (reverse-list-rec (cdr lst) (cons (car lst) acc)))
    )
)

(defun actualiza-mapa (mapa x y nuevo-color coordenada-pintar)
    (cond
        ((null mapa) nil)
        ((zerop x)
            (cons 
                (actualiza-columna (car mapa) x y nuevo-color coordenada-pintar)
                (cdr mapa)
            )
        )
        (t 
            (cons 
                (car mapa) 
                (actualiza-mapa (cdr mapa) (1- x)  y nuevo-color coordenada-pintar)
            )
        )
    )
)

(defun actualiza-columna (fila x y nuevo-color coordenada-pintar)
    (cond
        ((null fila) nil)
        ((zerop y)
            (let ((celda (car fila)))
            (vacia-pos (car coordenada-pintar) (cadr coordenada-pintar) nuevo-color) ;; Se repinta el color de la zelda
            (cons 
                (append (list (car celda)) (list nuevo-color) (cddr celda)) ;; Asumiendo (tipo color etc)
                (cdr fila)
            )
            )
        )
        (t  
            (cons 
                (car fila) 
                (actualiza-columna (cdr fila) x (1- y) nuevo-color coordenada-pintar)
            )
        )
    )
)

(defun-tco get-color-pos (coordenada x y mapa fila)
    "Actualiza el mapa con el nuevo color en la coordenada dada."
    (cond 
        ((null mapa) nil)
        ((null fila) (get-color-pos coordenada 0 (+ y 1)  (cdr mapa) (car (cdr mapa))))
        (t 
            (cond 
                ((equal coordenada (list x y))
                    (cadr (car fila))
                )
                (t 
                    (get-color-pos coordenada (1+ x) y mapa (cdr fila)))
            )
        )
    )   
)