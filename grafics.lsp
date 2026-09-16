;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiantes: VGP, AMC.
;; 03/05/2026
;; Professor: AOT.
;; Lliurament: primera convocatòria.
;; Fichero del modulo grafico.

;; Constantes de configuracion de graficos.
;; +mapa-max-x+
;; +mapa-max-y+
;; +mapa-x+
;; +mapa-y+
;; +x-00+
;; +y-00+
;; +lado-tile+

;; Funcion que fija las constantes de los graficos llamada solo una vez al inicializar una partida
(defun set-const (mapa-x mapa-y)
    (let* 
        (
            (eje-mayor (cond ((> mapa-x mapa-y) mapa-x) (t mapa-y)))
            (l-tile (floor (/ 360 eje-mayor)))
        )
        
        (setq +lado-tile+ l-tile)
        
        (setq +mapa-x+ mapa-x)
        (setq +mapa-y+ mapa-y)

        (setq +mapa-max-x+ (* +mapa-x+ +lado-tile+))
        (setq +mapa-max-y+ (* +mapa-y+ +lado-tile+))

        (setq +x-00+ 272)
        (setq +y-00+ 8)
    )
)


;;Funciones iniciales

(defun inicializar-graficos (data-mapa pintura mapa-x mapa-y)
    (cls)
    (set-const mapa-x mapa-y)
    (leer-mapa data-mapa 0 0)
    (pinta-horizontales 0 +mapa-x+)
    (pinta-verticales 0 +mapa-y+)
    (actualiza-pintura pintura)
    t
)

(defun pinta-horizontales (cur max)
    (cond
        ((<= cur max)   
            (move (get-x 0) (get-y cur))
            (drawrel +mapa-max-y+ 0)
            (pinta-horizontales (+ cur 1) max)
        )
    )
)

(defun pinta-verticales (cur max)
    (cond
        ((<= cur max)
            (move (get-x cur) (get-y 0))
            (drawrel 0 +mapa-max-x+)
            (pinta-verticales (+ cur 1) max)
        )
    )
)

(defun actualiza-pintura (pintura)
    (pinta-rect (- +x-00+ 101) +y-00+ 100 240 220 220 220)
    (pinta-rect (- +x-00+ 41) (+ +y-00+ 20) 20 (car pintura) 255 255 255)
    (pinta-rect (- +x-00+ 81) (+ +y-00+ 20) 20 (cadr pintura) 0 0 0)
)

;; Lectura del mapa

(defun leer-mapa (data-mapa x y)
    (cond 
        ((null data-mapa) t)
        (t 
            (leer-fila (car data-mapa) 0 y)
            (leer-mapa (cdr data-mapa) 0 (1+ y))
        )
    )
)

(defun leer-fila (fila x y)
    (cond
        ((null fila) t)
        (t
            (let* 
                (
                    (celda (car fila))
                    (tipo (car celda))
                    (color (cadr celda))
                    (tam (length celda))
                )
                (cond
                    ((eq tipo 'aigua) (pinta-rect (1+ (get-x x)) (1+ (get-y y)) (- +lado-tile+ 1) (- +lado-tile+ 1) 135 206 250))
                    ((eq tam 4) (pinta-base x y (cadddr celda)))
                    ((eq tam 3) (pinta-lab x y))
                    (t (pinta-cuad-rgb (1+ (get-x x)) (1+ (get-y y)) (- +lado-tile+ 1) (get-color-code color)))
                )
                (leer-fila (cdr fila) (1+ x) y)
            )
        )
    )
)

;; Funciones auxliares

(defun pinta-cuad (x y lado r g b)
    (color r g b)
    (pinta-relleno x y 0 lado lado)
    (color 0 0 0 255 255 255)
)

(defun pinta-cuad-rgb (x y lado color)
    (color (car color) (cadr color) (caddr color))
    (pinta-relleno x y 0 lado lado)
    (color 0 0 0 255 255 255)
)

(defun pinta-rect (x y ancho alto r g b)
    (color r g b)
    (pinta-relleno x y 0 ancho alto)
    (color 0 0 0 255 255 255)
)

(defun-tco pinta-relleno (x y i ancho alto)
    (cond
    ((= i alto) t)
    (t 
       (move x (+ y i))
       (drawrel ancho 0)
       (pinta-relleno x y (+ i 1) ancho alto)
    )
  )
)

(defun get-color-eq (equipo)
    (cond 
        ((eq equipo 'e1) '(0 0 0))
        (t '(255 255 255))    
    )
)

(defun get-color-code (code)
    (cond 
        ((eq code 'r) '(255 230 230))
        ((eq code 'g) '(230 255 230))
        ((eq code 'b) '(230 230 255))
        (t '(0 0 0))
    )
)

(defun pinta-base (x y equipo)
    (pinta-cuad-rgb (1+ (get-x x)) (1+ (get-y y)) (- +lado-tile+ 1) (get-color-eq equipo))
)

(defun pinta-lab (x y)
    (pinta-cuad (1+ (get-x x)) (1+ (get-y y)) (- +lado-tile+ 1) 255 215 0)  
)

(defun vacia-pos (x y color)
    (pinta-cuad-rgb (1+ (get-x x)) (1+ (get-y y)) (- +lado-tile+ 1) (get-color-code color))
)

(defun captura-lab (x y equipo)
    (let* 
        (
            (px (get-x x))
            (py (get-y y))
            (lado-peq (floor +lado-tile+ 2))
            (offset (floor (- +lado-tile+ lado-peq) 2))
        )

        (pinta-cuad-rgb (1+ px) (1+ py) (- +lado-tile+ 1) (get-color-eq equipo))
        (pinta-cuad (+ px offset) (+ py offset) lado-peq 255 215 0)
    )
)

"(defun pinta-bolla (x y equipo code-color-bolla)
    (let*
        (
            (color-bolla (get-color-code code-color-bolla))
            (r1 (car color-bolla))
            (g1 (cadr color-bolla))
            (b1 (caadr color-bolla))
            (color-equipo (get-color-eq equipo))
            (r2 (car color-equipo))
            (g2 (cadr color-equipo))
            (b2 (caadr color-equipo))
        )
        (pinta-rect (get-x x) (get-y y) +lado-tile+ (floor (/ +lado-tile+ 2)) r1 g1 b1)
        (pinta-rect (get-x x) (+ (get-y y) (floor (/ +lado-tile+ 2))) +lado-tile+ (floor (/ +lado-tile+ 2)) r2 g2 b2)
    )
)"

(defun pinta-bolla (x y equipo code-color-bolla)
  (let* (
         ;; 1. Calculamos el centro de la celda (cx, cy)
         (cx (+ (get-x x) (floor (/ +lado-tile+ 2))))
         (cy (+ (get-y y) (floor (/ +lado-tile+ 2))))
         
         ;; 2. Configuracion del cuadrado (tamaño lado / 4)
         (tam-cuad (floor (/ +lado-tile+ 4)))
         (offset-cuad (floor (/ tam-cuad 2)))
         (rgb-cuad (color-rgb-de-simbolo code-color-bolla))
         
         ;; 3. Configuracion del circulo (radio un poco mayor que el cuadrado)
         ;; Usamos un radio que cubra visualmente el cuadrado
         (radio-circ (floor (/ +lado-tile+ 3)))
         (rgb-equipo (color-rgb-de-equipo equipo))
        )
    
    ;; --- PASO 1: Pintar el cuadrado relleno ---
    ;; Usamos tu funcion pinta-rect: (x y ancho alto r g b)
    ;; Para centrarlo, restamos la mitad de su tamaño al centro de la celda
    (pinta-rect (- cx offset-cuad) (- cy offset-cuad) 
                tam-cuad tam-cuad 
                (car rgb-cuad) (cadr rgb-cuad) (caddr rgb-cuad))

    ;; --- PASO 2: Pintar el contorno del circulo ---
    ;; Usamos tu funcion dibuja-contorno-circulo: (cx cy radio rgb)
    (dibuja-contorno-circulo cx cy radio-circ rgb-equipo)
    
    ;; Reset del color a negro/blanco para no ensuciar otros dibujos
    (color 0 0 0 255 255 255)
  )
)

(defun get-x (x)
    "Calcula la posición horizontal en píxeles para la columna X"
    (+ +x-00+ (* x +lado-tile+))
)

(defun get-y (y)
    "Calcula la posición vertical en píxeles para la fila Y"
    (+ +y-00+ (* y +lado-tile+))
)

;; Funciones externas

(defun pintar-crea-bolla (coordenadas color-bolla equipo)
    (dibuja-unidad (car coordenadas) (cadr coordenadas) color-bolla equipo nil)
)

(defun pintar-mou-bolla (coordenadas-ini color-inici coordenadas-fin color-bolla equipo colores-pintados)
    (vacia-pos (car coordenadas-ini) (cadr coordenadas-ini) color-inici)
    (dibuja-unidad (car coordenadas-fin) (cadr coordenadas-fin) color-bolla equipo colores-pintados)
)


;; Jandro bolas grandes

;; Dibuja una unidad en su coordenada.

(defun dibujar-base (x y equipo colores-pintados)
    (let* 
        (
           (cx (+ (get-x x) (floor (/ +lado-tile+ 2))))
           (cy (+ (get-y y) (floor (/ +lado-tile+ 2))))
           (radio (floor (/ +lado-tile+ 3)))
        )
        (pinta-base x y equipo)
        (dibuja-color-pintado cx cy radio (car colores-pintados))
        (dibuja-color-pintado (+ 3 cx) cy radio (cadr colores-pintados))
        (color 0 0 0 255 255 255)
    )
)


(defun dibuja-unidad (x y color-bolla equipo colores-pintados)
    (let* 
        (
           (cx (+ (get-x x) (floor (/ +lado-tile+ 2))))
           (cy (+ (get-y y) (floor (/ +lado-tile+ 2))))
           (radio (floor (/ +lado-tile+ 3)))
        )
        (dibuja-bola-circulo cx cy radio color-bolla equipo)
        (dibuja-color-pintado cx cy radio (car colores-pintados));;solo puede haber uno pq si no muere
        (color 0 0 0 255 255 255)
    )
)

(defun dibuja-color-pintado (cx cy radio color-pintado)
    (cond
        ((null color-pintado) nil)
        (t
            (let* 
                (
                    (rgb (color-rgb-de-simbolo color-pintado))
                    (offset (floor (/ radio 2)))
                )
                (pinta-rect (- cx offset) (- cy offset) 
                            offset offset 
                            (car rgb) (cadr rgb) (caddr rgb))
            )
        )
    )
)

;; Dibuja una bola como circulo coloreado.
 (defun dibuja-bola-circulo (cx cy radio color-simbolo equipo)
    (let ((rgb (color-rgb-de-simbolo color-simbolo))
          (rgb-contorno (color-rgb-de-equipo equipo)))
        (color (car rgb) (cadr rgb) (caddr rgb)
               (car rgb) (cadr rgb) (caddr rgb))
        (dibuja-circulo-relleno cx cy radio rgb)
        (dibuja-contorno-circulo cx cy radio rgb-contorno)
    )
)

;; Convierte simbolo de color a RGB.
(defun color-rgb-de-simbolo (col)
    (cond
        ((eq col 'r) '(255 0 0))
        ((eq col 'g) '(0 255 0))
        ((eq col 'b) '(0 0 255))
        (t '(128 128 128))
    )
)

;; Convierte simbolo de color a RGB.
(defun color-rgb-de-equipo (eq)
    (cond
        ((eq eq 'e2) '(255 255 255))
        ((eq eq 'e1) '(0 0 0))
        (t '(128 128 128))
    )
)

;; Rellena una circunferencia con el color de la bola.
(defun dibuja-circulo-relleno (cx cy radio rgb)
    (color (car rgb) (cadr rgb) (caddr rgb)
            (car rgb) (cadr rgb) (caddr rgb))
    (dibuja-circulo-relleno-linea cx cy radio (- radio))
)

;; Dibuja las filas interiores del circulo.
(defun dibuja-circulo-relleno-linea (cx cy radio dy)
    (cond
        ((> dy radio)
            nil
        )
        (t
            (let ((dx (floor (sqrt (- (* radio radio) (* dy dy))))))
                (move (- cx dx) (+ cy dy))
                (drawrel (+ (* 2 dx) 1) 0)
                (dibuja-circulo-relleno-linea cx cy radio (+ dy 1))
            )
        )
    )
)

;; Dibuja el contorno del circulo.
(defun dibuja-contorno-circulo (cx cy radio rgb)
    (color (car rgb) (cadr rgb) (caddr rgb)
            (car rgb) (cadr rgb) (caddr rgb))
    (dibuja-contorno-circulo-linea cx cy radio (- radio))
)

;; Dibuja el contorno fila a fila.
(defun dibuja-contorno-circulo-linea (cx cy radio dy)
    (cond
        ((> dy radio)
            nil
        )
        (t
            (let ((dx (floor (sqrt (- (* radio radio) (* dy dy))))))
                (dibuja-punto (+ cx dx) (+ cy dy))
                (dibuja-punto (- cx dx) (+ cy dy))
                (dibuja-contorno-circulo-linea cx cy radio (+ dy 1))
            )
        )
    )
)

;; Pinta un punto individual.
(defun dibuja-punto (x y)
    (move x y)
    (drawrel 1 0)
)
