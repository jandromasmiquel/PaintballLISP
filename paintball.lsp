;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiantes: VGP, AMC.
;; 03/05/2026
;; Professor: AOT.
;; Lliurament: primera convocatòria.
;; Fichero del controlador principal.

;; Per iniciar el joc basta executar (inicio),
;; Si s'executa sense cap argument el mapa que es carregara sera maps/basic1.map
;; En cas de voler iniciar un mapa de la memoria es crida a (inicio map-path) a on map-path es de la forma "maps/nombre.map"
;; Per anar pasant les rondes es pitja enter

;; Extras:
;; Mapes de fins a 60x60
;; Limit de 1500 torns
;; Bolla pinta casella
;; Color de casella
;; Optimitzacio de pintar
;; Memoria compartida
;; Mapes de qualsevol tamany
;;Informació de la pintura dels equips

;; Optimitzación de llamadas recursivas TCO
(load 'tco)

;; Carga de los modulos necesarios.
(load 'grafics)
(load 'agent-vgp522)
(load 'agent-amc358)
(load 'vision)
(load 'acciones)
(load 'common)

;; Constantes globales de configuracion.
(setq +pb-default-map+ "maps/basic1.map")
(setq +pb-max-rounds+ 1500)
(setq +pb-initial-paint+ 200)
(setq +pb-memory-limit+ 100000)
(setq +agente1+ 'agent-amc358)
(setq +agente2+ 'agent-amc358)

(defun inicio (&optional map-path)
    "Inicializa la ventana y pinta el mapa base."
    (let* 
        (
            (data-mapa 
                (datos-mapa 
                    (cond
                        (map-path map-path)
                        (t +pb-default-map+)
                    )
                )
            )
            (tam-mapa (obtener-tam-mapa data-mapa))
            (mapa-x (car tam-mapa))
            (mapa-y (cadr tam-mapa))
            (estado-inicial (inicializar-estado data-mapa))
            (pintura (get-key 'pintura estado-inicial))
        )
        (inicializar-graficos data-mapa pintura mapa-x mapa-y)
        (bucle-partida estado-inicial)
    )    
)

;; Inicializacion --------------------------------------------------

;; Crea el estado base de la partida.
(defun inicializar-estado (data-mapa)
    "Construye la estructura inicial del estado de partida."
    (list
        (cons 'mapa data-mapa)
        (cons 'ronda 0)
        (cons 'pintura (list +pb-initial-paint+ +pb-initial-paint+)) ;; Pintura e1, Pintura e2
        (cons 'unidades (buscar-mapa-unidades data-mapa (car data-mapa) 0 0 0 nil));;lista con bases y bolas vivas
        (cons 'laboratorios (buscar-mapa-laboratorios data-mapa (car data-mapa) 0 0 nil)) ;;lista con coordenada y equipo de cada laboratorio
        (cons 'memoria (list nil nil)) ;;memoria compartida de cada equipo, inicialmente vacia. e1, e2
        (cons 'ganador nil)
    ) 
)

;; Función recursiva para buscar bases.
(defun-tco buscar-mapa-unidades (data-mapa fila x y id acc)
    (cond 
        ((null data-mapa) acc);;fin del mapa
        ;;final de la fila
        ((null fila) (buscar-mapa-unidades (cdr data-mapa) (car (cdr data-mapa)) 0 (1+ y) id acc))
        (t 
            (let* ((celda (car fila))
                   (info-base (member 'base celda)))
                (cond 
                    (info-base 
                        (buscar-mapa-unidades 
                            data-mapa (cdr fila) (1+ x) y (1+ id) 
                            (cons 
                                (inicializar-unidad id 'base (cadr info-base) (list x y) nil) ;;el color de la base es irrelevante
                                acc
                            )
                        )
                    )
                    (t 
                        (buscar-mapa-unidades data-mapa (cdr fila) (1+ x) y id acc))
                )
            )
        )
    )
)

;; Crea una unidad dados sus datos.
(defun inicializar-unidad (id tipo equipo coordenada color)
    "Construye la estructura minima de una unidad."
    (list
        (cons 'id id)
        (cons 'tipo tipo)
        (cons 'equipo equipo)
        (cons 'coordenada coordenada)
        (cons 'colores-pintados nil)
        (cons 'color color)
        (cons 'tr-pintar 0)
        (cons 'tr-moure 0)
    )
)

;; Función recursiva para buscar laboratorios.
(defun-tco buscar-mapa-laboratorios (data-mapa fila x y acc)
    (cond 
        ((null data-mapa) acc)
        ((null fila) (buscar-mapa-laboratorios (cdr data-mapa) (car (cdr data-mapa)) 0 (1+ y) acc))
        (t 
            (let* 
                (
                    (celda (car fila))
                    (lab (member 'lab celda))
                )
                (cond
                    (lab 
                        (buscar-mapa-laboratorios
                            data-mapa (cdr fila) (1+ x) y 
                            (cons 
                                (list (list x y) nil)
                                acc
                            )
                        )
                    )
                    (t (buscar-mapa-laboratorios data-mapa (cdr fila) (1+ x) y acc))
                )
            )
        )
    )
)

;; Bucle principal --------------------------------------------------

;; Bucle principal de la partida.
(defun-tco bucle-partida (estado)
    (goto-xy 0 0)
    (format t "Ronda: ~a~%" (get-key 'ronda estado))
    ;;(goto-xy 0 3) ;;baja el puntero para los debug
    (actualiza-pintura (get-key 'pintura estado))
    (let
        (
            (estado2 (comprueba-fin estado))
        )
        (cond
            ((not (null (get-key 'ganador estado2))) (muestra-ganador estado2))
            (t
                (bucle-partida (actualiza-estado estado2))
            )
        )
    )
)

;; Comprueba si la partida ha terminado.
(defun comprueba-fin (estat)
    "Si se ha llegado al limite de rondas actualiza el ganador"
    (cond
        ((>= (get-key 'ronda estat) +pb-max-rounds+) (set-ganador estat (get-key 'unidades estat)) estat)
        (t estat)
    )
)

(defun set-ganador (estat unidades)
    (let*
        (
            (ganador (get-ganador unidades 0 0))
            (pintura (get-key 'pintura estat))
        )
        (cond
            ((null ganador) 
                (cond
                    ((null (get-ganador-pintura pintura)) (set-key 'ganador (get-ganador-random) estat))
                    (t (set-key 'ganador (get-ganador-pintura pintura) estat))
                )    
            )
            (t (set-key 'ganador ganador estat))
        )
    )
)

(defun get-ganador (unidades e1 e2)
    (cond
        ((null unidades) 
                        (cond
                            ((> e1 e2) 'e1)
                            ((< e1 e2) 'e2)
                            ((eq e1 e2) nil)
                        ))
        (t
            (cond
                ((and (eq (get-key 'equipo (car unidades)) 'e1) (eq (get-key 'tipo (car unidades))'bolla)) (get-ganador (unidades (+ e1 1) e2)))
                ((and (eq (get-key 'equipo (car unidades)) 'e2) (eq (get-key 'tipo (car unidades))'bolla)) (get-ganador (unidades e1 (+ e2 1))))
            )
        )
    )
)

(defun get-ganador-pintura (pintura)
    (cond 
        ((> (car pintura) (cadr pintura)) 'e1)
        ((< (car pintura) (cadr pintura)) 'e2)
        (t nil)
    )
)

(defun get-ganador-random ()
    (let 
        (
            (n (random 2))
        )
        (cond 
            ((= n 0) 'e1)
            ((= n 1) 'e2)
        )
    )
)

;; Muestra el resultado final por consola.
(defun muestra-ganador (estat)
    "Imprime el resultado final de la partida." 
    (format t "~%Ganador: ~a" (get-key 'ganador estat))
)

;; Actulizacion del estado --------------------------------------------------

;; Actualiza el estado al fin de un turno.
(defun actualiza-estado (estado)
    "llama a turno-equipo para en funcion de la ronda equipo y actualiza el estado con el resultado"
    (let* 
        (
            (ronda (get-key 'ronda estado))
            (estado-act (turno-equipo ronda estado))
            (equipo (cond ((eq (mod ronda 2) 0) 'e1) (t 'e2)))
        )
        (set-key 'ronda (1+ ronda) (recupera-pintura estado-act equipo));; aumenta la ronda despues de resetear la pintura
    )
)

(defun recupera-pintura (estado equipo)
    "Recupera pintura al final del turno, dependiendo de los laboratorios capturados."
    (let* ((laboratorios (get-key 'laboratorios estado))
           (pintura (get-key 'pintura estado))
           (pintura-recuperada (calcula-pintura-labs laboratorios equipo 2)));;la pintura recuperada es 2 + 1 por laboratorio capturado
        (cond
            ((eq equipo 'e1) (set-key 'pintura (list (+ pintura-recuperada (car pintura)) (cadr pintura)) estado))
            ((eq equipo 'e2) (set-key 'pintura (list (car pintura) (+ pintura-recuperada (cadr pintura))) estado))
            (t estado)
        )
    )
)

(defun-tco calcula-pintura-labs (laboratorios equipo acumulat)
    "Cuenta la pintura que recupera un equipo segun los laboratorios que controla."
    (cond
        ((null laboratorios) acumulat)
        (t
            (let* ((lab (car laboratorios))
                   (equipo-lab (cadr lab)))
                (cond
                    ((eq equipo-lab equipo) (calcula-pintura-labs (cdr laboratorios) equipo (+ acumulat 1)))
                    (t (calcula-pintura-labs (cdr laboratorios) equipo acumulat))
                )
            )
        )
    )
)

(defun turno-equipo (ronda estado)
    "Ejecuta el turno procesando primero bases, luego bolas."
    (let* 
        (
            (equipo (cond ((eq (mod ronda 2) 0) 'e1) (t 'e2)))
            (unidades (get-key 'unidades estado))
            
            (bases (get-unidad-tipo unidades 'base equipo nil))
            (estado-bases (procesa-unidades estado bases equipo))

            (unidades-act (get-key 'unidades estado-bases))
            (bolas (get-unidad-tipo unidades-act 'bolla equipo nil))
            
            (estado-final (procesa-unidades estado-bases bolas equipo))
        )
    
    estado-final)
)

(defun-tco get-unidad-tipo (unidades tipo equipo acc)
    "Devuelve la lista de unidades del tipo y equipo dados que pueden moverse."
    (cond
        ((null unidades) acc)
        (t
            (let* 
                (
                    (unidad (car unidades))
                    (tipo-unidad (get-key 'tipo unidad))
                    (equipo-unidad (get-key 'equipo unidad))
                    (mou (get-key 'tr-moure unidad))
                )
                (cond
                    ((and (equal tipo-unidad tipo) (equal equipo-unidad equipo))
                        (get-unidad-tipo (cdr unidades) tipo equipo (cons unidad acc)))
                    (t 
                        (get-unidad-tipo (cdr unidades) tipo equipo acc))
                )
            )
        )
    )
)

;; Motor que procesa una lista de unidades, aplicando las acciones de cada una y actualizando el estado secuencialmente.
(defun-tco procesa-unidades (estat-actual unidades equip)
    (cond
        ((null unidades) estat-actual)
        (t
            (let* 
                (
                    (unidad (car unidades))
                    (acciones (pide-acciones (agent-equipo equip) (datos-unidad estat-actual unidad)))
                )
                (procesa-unidades (aplica-acciones-unidad estat-actual unidad acciones) (cdr unidades) equip)
            )
        )
    )   
)

;;devuelve el agente correspondiente en funcion del parametro equipo
(defun agent-equipo (equipo)
    (cond
        ((eq equipo 'e1) +agente1+)
        ((eq equipo 'e2) +agente2+)
        (t nil)
    )
)

;; Aplica todas las acciones de una unidad.
(defun-tco aplica-acciones-unidad (estat unidad acciones)
    "Procesa secuencialmente las acciones de una unidad."
    (cond
        ((null acciones) (recupera-cooldown unidad estat))
        (t
            (aplica-acciones-unidad
                (aplica-accion estat unidad (car acciones))
                unidad
                (cdr acciones)
            )
        )
    )
)

(defun recupera-cooldown (unidad estat)
    "Recupera el cooldown de una unidad al final de su turno."
    (let* 
        (
            (id (get-key 'id unidad))
            (unidades (get-key 'unidades estat))
            (unidad-estado (get-unidad-id unidades id))
            (tr-pintar (get-key 'tr-pintar unidad-estado))
            (tr-moure (get-key 'tr-moure unidad-estado))
            (unidad1 (set-key 'tr-pintar (max 0 (- tr-pintar 1)) unidad-estado))
            (unidad-final (set-key 'tr-moure (max 0 (- tr-moure 1)) unidad1))
            (unidades-act (actualiza-unidad unidades unidad unidad-final))
        )
        (set-key 'unidades unidades-act estat)
    )
)


(defun actualiza-unidad (unidades unidad unidad-final)
    (cond
        ((null unidades) nil)
        (t
            (let ((u (car unidades)))
                (cond
                    ((equal (get-key 'id u) (get-key 'id unidad))
                        (cons unidad-final
                              (actualiza-unidad (cdr unidades) unidad unidad-final))
                    )
                    (t
                        (cons u (actualiza-unidad (cdr unidades) unidad unidad-final))
                    )
                )
            )
        )
    )
)

;; Aplica una accion validada al nuevo estado.
;;para los nombres de las funciones de acciones vamos a copiar el nombre q aparece en el enunciado
(defun aplica-accion (estat unidad accion)
    "Devuelve un nuevo estado despues de aplicar una accion." 
    (let ((nom-accio (car accion))
          (llista-arguments (cadr accion)))
        (cond
            ((eq nom-accio 'pinta)
                (accio-pinta llista-arguments estat unidad)
            )
            ((eq nom-accio 'mou)
                (accio-mou llista-arguments estat unidad)
            )
            ((eq nom-accio 'crea-bolla)
                (accio-crea-bolla llista-arguments estat unidad)
            )
            ((eq nom-accio 'escriu-memoria)
                (accio-escriu-memoria (car llista-arguments) estat (get-key 'equipo unidad))
            )
            (t estat)
        )
    )
)

;; Recoge los datos necesarios de una unidad para pasarselos al agente.
(defun datos-unidad (estat unidad)
    "Devuelve la lista en el orden requerido por el controlador."
    (let ((equipo (get-key 'equipo unidad))
        (vision (get-vision estat unidad)))
        (list
            (get-key 'ronda estat) ;1 ronda
            equipo ;2 equipo
            (get-pintura estat equipo) ;3 pintura
            (get-key 'id unidad) ;4 id-unitat
            (get-key 'tipo unidad) ;5 tipus-unitat
            (get-key 'coordenada unidad) ;6 coordenada
            (get-key 'colores-pintados unidad) ;7 colors-pintat
            (get-key 'color unidad) ;8 color-propi
            (get-key 'tr-pintar unidad) ;9 tr-pintar    
            (get-key 'tr-moure unidad) ;10 tr-moure
            vision ;;11 vision
            (cond ;;12 memoria-compartida
                ((equal 'e1 equipo)
                    (car (get-key 'memoria estat))
                )
                (t
                    (cadr (get-key 'memoria estat))
                )
            )
        )
    )
)

(defun get-pintura (estat equipo)
    "Devuelve la cantidad de pintura disponible para un equipo."
    (let ((pintura (get-key 'pintura estat)))
        (cond
            ((eq equipo 'e1) (car pintura))
            (t (cadr pintura))
        )
    )
)

;; Pide las acciones al automata de una unidad.
(defun pide-acciones (agent datos)
    "Lanza el automata correspondiente y recoge la respuesta."
    (cond
        ((null agent) nil)
        (t
            (let ((acciones (funcall agent datos)))
                (cond
                    ((listp acciones) acciones)
                    (t nil)
                )
            )
        )
    )
)