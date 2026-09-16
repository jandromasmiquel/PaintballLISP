;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiantes: vgp.
;; 03/05/2026
;; Professor: AOT.
;; Lliurament: primera convocatòria.
;; Agent intel·ligent vgp522.

;; ------guia de datos------
;; ronda: entero
;; equipo: 'e1 o 'e2
;; pintura: entero
;; id: entero
;; tipo: 'base o 'bolla
;; coordenada: (x y)
;; color-pintado: lista de colores nil o ('r 'g 'b)
;; color: 'r, 'g, 'b o nil
;; tr-pintar: entero
;; tr-moure: entero
;;---vision procesada---
;;se procesa la vision para clasificar la informacion relevante para la toma de decisiones
;;se filtran datos irrelevantes y se simplifica la estructura de los datos para facilitar su uso:
;;las coord son (x y)
;; 1 base-enemiga: (coord colores-pintado)
;; 2 bolas-enemigas: lista de (coord color-propio colores-pintado)
;; 3 labs-capturables: lista de (coord)
;; 4 amigos: lista de (coord)
;; 5 casillas-libres: lista de (coord color) color('r 'g o 'b) 
;;---memoria compartida---
;;estructura de la lista de memoria compartida y tipos de registro:
;;tamano memoria: entero (espacios de memoria disponibles)
;;registro base aliada: (1 x y) 3at
;;la cabecera siempre es (tamano base-aliada) y el resto de registros pueden venir en cualquier orden, se identifican por su primer elemento (codigo):
;;registro de escuadron de una unidad: (1 idbola idesc) 3at
;;registro de escuadron y todas sus unidades: (2 idesc idbola1 idbola2 idbola3) 5at si contene menos de 3 bolas, los espacios de las bolas vacactes estan vacios
;;un registro de escuadron incompleto puede ser (1 idesc idbola11 idbola22) o (1 idesc idbola11) o (1 idesc)
;;rol de un escuadron: (3 idesc rol) 3at rol: 'ataque, 'defensa o 'exploracion
;;direccion de un escuadron: (4 idesc dir) 3at dir: 'n, 's, 'e o 'o
;;registro de base enemiga: (5 x y) 3at
;;registro de un objetivo temporal (lab o bola enemiga): (6 x y tipo) 4at tipo: 'lab o 'bolla
;;registro de una bola: (7 idbola x y color) 5at
;;en caso de que haya escuadrones incompletos el tamaño de la cabecera sera menor al real (aunque es irrelevante)
;;---memoria compartida procesada---
;;se procesa la memoria compartida 
;;tamano memoria: entero
;;base aliada: (x y)
;;1-escuadron: idesc al que pertenece la bola
;;2-escuadrones: lista de (idesc idbola1 idbola2 idbola3) 
;;un escuadron incompleto puede ser (idesc idbola11 idbola22) o (idesc idbola11) o (idesc)
;;3-rol escuadron: lista de (idesc rol) rol: 'a, 'd o 'e (ataque, defensa o exploracion)
;;4-direccione de escuadrones: lista de(idesc dir) dir: 'n, 's, 'e o 'o
;;5-base enemiga: (x y).
;;6-objetivo temporal: lista de (x y tipo) tipo: 'lab o 'bolla
;;7-bola amiga: lista de (idbola x y color) color: 'r, 'g o 'b
;;--acciones permitidas--
;;crear bola: ('crea-bolla color coord) color: 'r, 'g o 'b coord: (x y)
;;pintar: ('pinta color) color: 'r, 'g o 'b
;;mover: ('mou coord) coord: (x y)
;;escribir memoria: ('escriu-memoria memoria) memoria: lista de registros
;;el agente retorna una lista de acciones permitidas

(defun agent-vgp522 (datos)
    "Retorna la jugada que realiza el agente vgp522 en un turno dadas las datos de la partida."
    (let* (
        (l datos)
        (ronda (car l)) (l (cdr l))
        (equipo (car l)) (l (cdr l))
        (pintura (car l)) (l (cdr l))
        (id (car l)) (l (cdr l))
        (tipo (car l)) (l (cdr l))
        (coordenada (car l)) (l (cdr l))
        (color-pintado (car l)) (l (cdr l))
        (color (car l)) (l (cdr l))
        (tr-pintar (car l)) (l (cdr l))
        (tr-moure (car l)) (l (cdr l))
        (vision (car l)) (l (cdr l))
        (memoria-compartida (car l)))
        (agente-vgp522-tomar-decision 
            ronda equipo pintura id tipo coordenada color-pintado color tr-pintar tr-moure 
            (agente-vgp522-procesar-vision vision equipo coordenada nil nil nil nil nil) 
            (agente-vgp522-procesar-memoria memoria-compartida id)
            memoria-compartida
        )
    )
)

(defun-tco agente-vgp522-procesar-vision (vision equipo mi-coordenada base-enemiga bolas-enemigas labs-capturables amigos casillas-libres)
    "Itera recursivamente para clasificar las casillas de la visión, filtrando datos irrelevantes."
    (cond
        ((null vision) 
            (list base-enemiga bolas-enemigas labs-capturables amigos casillas-libres))
        (t (let* (
            (casilla (car vision))
            (coord (car casilla))
            (tipo-casilla (car (cdr casilla)))
            (resto (cdr (cdr casilla)))
            (color-casilla (car resto))
            (tipo-elemento (car (cdr resto)))
            (equip-elemento (car (cdr (cdr resto))))
            (colores-pintado (car (cdr (cdr (cdr resto)))))
            (color-propio (car (cdr (cdr (cdr (cdr resto)))))))
            (cond
                ;; Agua: se descarta
                ((eq tipo-casilla 'aigua)
                    (agente-vgp522-procesar-vision (cdr vision) equipo mi-coordenada base-enemiga bolas-enemigas labs-capturables amigos casillas-libres)
                )
                ;; Base enemiga (solo guardo coord y colores-pintado)
                ((and (eq tipo-elemento 'base) (not (eq equip-elemento equipo)))
                    (agente-vgp522-procesar-vision (cdr vision) equipo mi-coordenada (list coord colores-pintado) bolas-enemigas labs-capturables amigos casillas-libres)
                )
                ;; Bola enemiga guardo (coord, color-propio y colores-pintado)
                ((and (eq tipo-elemento 'bolla) (not (eq equip-elemento equipo)))
                    (agente-vgp522-procesar-vision (cdr vision) equipo mi-coordenada base-enemiga (cons (list coord color-propio colores-pintado) bolas-enemigas) labs-capturables amigos casillas-libres)
                )
                 ;; Laboratorio capturable neutro o enemigo (solo coord)
                ((and (eq tipo-elemento 'lab) (not (eq equip-elemento equipo)))
                    (agente-vgp522-procesar-vision (cdr vision) equipo mi-coordenada base-enemiga bolas-enemigas (cons coord labs-capturables) amigos casillas-libres)
                )
                 ;; Elemento amistoso (solo coord y tipo)
                ((and (or (eq tipo-elemento 'bolla) (eq tipo-elemento 'base)) (eq equip-elemento equipo))
                    (agente-vgp522-procesar-vision (cdr vision) equipo mi-coordenada base-enemiga bolas-enemigas labs-capturables (cons (list coord tipo-elemento) amigos) casillas-libres)
                )
                 ;; Casilla libre de tierra (si está a distancia <= 2, se guarda solo la coord)
                ((and (null tipo-elemento) (<= (agente-vgp522-distancia mi-coordenada coord) 2))
                    (agente-vgp522-procesar-vision (cdr vision) equipo mi-coordenada base-enemiga bolas-enemigas labs-capturables amigos (cons (list coord color-casilla) casillas-libres))
                )
                 ;; Caso por defecto (ignorar otras casillas libres alejadas y labs aliados)
                (t  (agente-vgp522-procesar-vision (cdr vision) equipo mi-coordenada base-enemiga bolas-enemigas labs-capturables amigos casillas-libres))
            ))
        )
    )
)

(defun agente-vgp522-procesar-memoria (memoria idbola)
    "Procesa toda la memoria compartida clasificada por su cabecera. Retorna: (tamano base-aliada t1 t2 t3 t4 base-enemiga t6 t7)"
    (cond
        ((null memoria) (list 100000 nil nil nil nil nil nil nil))
        (t (let (
            (tam (car memoria))
            (base-al (car (cdr memoria)))
            (lista (cdr (cdr memoria))))
            (agente-vgp522-procesar-memoria-rec lista tam base-al nil nil nil nil nil nil nil idbola))
        )
    )
)

(defun-tco agente-vgp522-procesar-memoria-rec (lista tamano base-aliada esc t2 t3 t4 base-enemiga t6 t7 idbola)
    "Clasifica los registros de la memoria de forma recursiva."
    (cond
        ((null lista) 
            (list tamano base-aliada esc t2 t3 t4 base-enemiga t6 t7)
        )
        (t 
            (let* (
                (registro (car lista))
                (codigo (car registro)))
                (cond
                    ((and (eq codigo 1) (eq (cadr registro) idbola)) ;; registro del escuadron de una unidad (1 idbola idesc) 3at
                        (agente-vgp522-procesar-memoria-rec (cdr lista) tamano base-aliada (caddr registro) t2 t3 t4 base-enemiga t6 t7 idbola)
                    )
                    ((eq codigo 2) ;;registro de escuadron y todas sus unidades (2 idesc idbola1 idbola2 idbola3) 5at
                        (agente-vgp522-procesar-memoria-rec (cdr lista) tamano base-aliada esc (cons (cdr registro) t2) t3 t4 base-enemiga t6 t7 idbola)
                    )
                    ((eq codigo 3) ;;rol de un escuadron (3 idesc rol) 3at
                        (agente-vgp522-procesar-memoria-rec (cdr lista) tamano base-aliada esc t2 (cons (cdr registro) t3) t4 base-enemiga t6 t7 idbola)
                    )
                    ((eq codigo 4) ;;direccion de un escuadron (4 idesc dir) 3at
                        (agente-vgp522-procesar-memoria-rec (cdr lista) tamano base-aliada esc t2 t3 (cons (cdr registro) t4) base-enemiga t6 t7 idbola)
                    )
                    ((eq codigo 5) ;;registro de base enemiga (5 x y) 3at
                        (agente-vgp522-procesar-memoria-rec (cdr lista) tamano base-aliada esc t2 t3 t4 (cdr registro) t6 t7 idbola)
                    )
                    ((eq codigo 6) ;;registro de un objetivo temporal (lab o bola enemiga) (6 x y tipo) 4at
                        (agente-vgp522-procesar-memoria-rec (cdr lista) tamano base-aliada esc t2 t3 t4 base-enemiga (cons (cdr registro) t6) t7 idbola)
                    )
                    ((eq codigo 7) ;;registro de una bola amiga (7 idbola x y color) 5at
                        (agente-vgp522-procesar-memoria-rec (cdr lista) tamano base-aliada esc t2 t3 t4 base-enemiga t6 (cons (cdr registro) t7) idbola)
                    )
                    (t 
                        (agente-vgp522-procesar-memoria-rec (cdr lista) tamano base-aliada esc t2 t3 t4 base-enemiga t6 t7 idbola)
                    )
                )
            )
        )
    )
)

(defun agente-vgp522-tomar-decision (ronda equipo pintura id tipo coordenada color-pintado color tr-pintar tr-moure informacion mem-procesada memoria-compartida)
    "Elige las acciones de la unidad (base o bola) según la información recopilada."
    (let (
        (labs-capturables (nth 2 informacion))
        (amigos (nth 3 informacion))
        (casillas-libres (nth 4 informacion))
        (tamano-mem (nth 0 mem-procesada))
        (escuadron (nth 2 mem-procesada))
        (escuadrones (nth 3 mem-procesada))
        (unidades-aliadas (nth 8 mem-procesada)))
        (cond
        ;;las bases solo crean una bola por turno
        ;;estrategicamente no da mucha ventaja q puedan crear mas ya q solo pasara en los primeros 4 turnos, con lo cual se ignora
            ((eq tipo 'base)
                ;; BASE
                (cond
                    ((and (>= pintura 50) (not (null casillas-libres)))
                        ;;Buscar escuadron con hueco para crear una bola nueva de ese color
                        ;;la base no puede registrar en memoria la bola pq no puede conocer su id
                        (let (
                            (rojo-vacante (vgp-buscar-escuadron-vacante escuadrones unidades-aliadas 'r)))
                            (cond
                                ((null rojo-vacante)
                                    (let (
                                        (verde-vacante (vgp-buscar-escuadron-vacante escuadrones unidades-aliadas 'g)))
                                        (cond
                                            ((null verde-vacante)
                                                (let (
                                                    (azul-vacante (vgp-buscar-escuadron-vacante escuadrones unidades-aliadas 'b)))
                                                    (cond
                                                        ((null azul-vacante) ;;no hay ningun color vacante, rojo por defecto
                                                            (list 
                                                                (cond ;;solo es necesario en la ronda 0 y 1
                                                                    ((or (= ronda 0) (= ronda 1))
                                                                        (list 'escriu-memoria (list (list 99997 coordenada)))
                                                                    )
                                                                ) 
                                                                (list 'crea-bolla (list 'r (caar casillas-libres)))
                                                            )
                                                        )
                                                        (t ;;hay escuadron con vacante azul, crear bola azul
                                                            (list (list 'crea-bolla (list 'b (caar casillas-libres)))) ;;accion crear bola
                                                        )
                                                    )
                                                )
                                            )
                                            (t ;;hay escuadron con vacante verde, crear bola verde
                                                (list (list 'crea-bolla (list 'g (caar casillas-libres)))) ;;accion crear bola
                                            )
                                        )
                                    )
                                )
                                (t ;;hay escuadron con vacante roja, crear bola roja y la registra
                                    (list (list 'crea-bolla (list 'r (caar casillas-libres))))
                                )
                            )
                        )
                    )
                    (t nil)
                )
            )
            ((eq tipo 'bolla)
                ;; BOLA
                (cond
                    ((null escuadron);;si no tiene escuadron se intenta unir o crear uno nuevo
                        (let* (
                            (escuadron-vacante (vgp-buscar-escuadron-vacante escuadrones unidades-aliadas color))
                            (registro-bola ;;si la bola ya estaba registrada, no hace falta crear un nuevo registro
                                (cond 
                                    ((vgp-comprobar-reg unidades-aliadas id) nil) 
                                    (t (list 7 id (car coordenada) (cadr coordenada) color))
                                )
                            )
                            (tamaño-registro (cond ((null registro-bola) 0) (t 5)))) ;;si no hay registro, se necesitan 5 espacios de memoria para crear uno nuevo
                            (cond
                                ((null escuadron-vacante)
                                    ;; No hay escuadrones con hueco, intentar crear uno nuevo si hay espacio
                                    (cond
                                        ((> tamano-mem (+ 8 tamaño-registro)) ;;minimo 8 registros para un escuadron completo
                                            (let
                                                ((id-nuevo-escuadron (+ 1 (vgp-max-id-escuadron escuadrones 0))))
                                                (let*  ;;actualiza toda la memoria y toma la decision como si la memoria ya estuviera actualizada en el motor
                                                       ;;si hay problemas alomejor toma decisiones en base a una memoria q no llega ver reflejada en el motor
                                                    ((nueva-memoria
                                                        (append 
                                                            (list (- (car memoria-compartida) (+ 8 tamaño-registro))) ;;actualizar tamaño de memoria 
                                                            (cdr memoria-compartida) ;;mantener la memoria excepto la cabecera con el tamaño
                                                            (list (list 1 id id-nuevo-escuadron)) ;;nuevo registro de esc al que se une la bola
                                                            (list (list 2 id-nuevo-escuadron id)) ;;registro completo del nuevo esc
                                                            (cond
                                                                ((null registro-bola) nil)
                                                                (t (list registro-bola))
                                                            )
                                                        )
                                                    )
                                                    (nueva-memoria-procesada (agente-vgp522-procesar-memoria nueva-memoria id)))
                                                        (vgp-tomar-decision-bolaesc ronda equipo id coordenada color-pintado color tr-pintar tr-moure informacion nueva-memoria-procesada nueva-memoria)
                                                )
                                            )
                                        )    
                                        (t nil) ;; No se puede crear un nuevo escuadrón, no hacer nada
                                    )
                                )
                                (t
                                    ;; Hay un escuadrón con hueco, unirse a él 
                                    (cond
                                        ((> tamano-mem (+ 3 tamaño-registro))
                                            (let* (
                                                (memoria-actualizada 
                                                    (append 
                                                        (list (- (car memoria-compartida) (+ 3 tamaño-registro))) ;;actualizar cabecera tamaño de memoria
                                                        (vgp-actualizar-escuadron (append escuadron-vacante (list id)) (cdr memoria-compartida) nil) ;;modificar el escuadron de la memoria para añadir la nueva bola
                                                        (cond ;;si la bola no estaba registrada, crear un nuevo registro de bola
                                                            ((null registro-bola) nil)
                                                            (t (list registro-bola))
                                                        )
                                                        (list (list 1 id (car escuadron-vacante))) ;;registro de esc al que se une la bola
                                                    )
                                                )
                                                (memoria-procesada-actualizada (agente-vgp522-procesar-memoria memoria-actualizada id)))
                                                (vgp-tomar-decision-bolaesc ronda equipo id  coordenada color-pintado color tr-pintar tr-moure informacion memoria-procesada-actualizada memoria-actualizada)
                                            )
                                        )   
                                        (t nil) ;; No se puede unir al escuadrón por falta de memoria, no hacer nada
                                    ) 
                                )
                            )
                        )
                    )
                    (t ;;si tiene escuadron
                        (vgp-tomar-decision-bolaesc ronda equipo id coordenada color-pintado color tr-pintar tr-moure informacion mem-procesada memoria-compartida)
                    )
                )
            )
        )
    )
)

(defun vgp-tomar-decision-bolaesc (ronda equipo id coordenada color-pintado color tr-pintar tr-moure informacion memoria-procesada memoria-compartida)
    ;;primero confirmar que las bolas de mi escuadron estan en la coordenada que la memoria dice, si no es asi, significa que han muerto (a no ser que esten fuera de rango)
    ;;en ese caso, actualizar la memoria eliminando la bola muerta del escuadron
    ;;en segundo lugar comprueba la informacion de la vision junto a la memoria compartida para actualizar los objetivos y rol del escuadron
    ;;por ejemplo si encuentra por primera vez la base enemiga la registra o si ve una bola o un lab capturable, lo registra como objetivo temporal del escuadron
    ;;si por ejemplo objetivos temporales anteriores ya no son visibles, los elimina de la memoria para no perseguir objetivos que ya no existen
    ;;siempre tiene q haber un escuadron completo con rol de defensa, si no lo hay, el escuadron al que pertenece la bola se asigna ese rol. 
    ;;el rol de explorar solo sirve para buscar la base enemiga. Si esta registrada, todos los escuadrones restantes se asignan a atacar. 
    ;;si un escuadron esta incompleto se le asigna el rol de defensa hasta que este completo
    ;;--una vez actualizada la memoria--
    ;;las acciones q puede hacer una bola son pintar y moverse. puede hacer ambas
    ;;primero toma la decision de pintar (en base a toda la info nueva) la cual no actualiza la memoria
    ;;segundo toma la decision de moverse. si se mueve actualiza la memoria para reflejar la nueva posicion
    ;;tercero guarda la memoria actualizada con los objetivos y roles nuevos del esc y su nueva posicion
    (let* (
        (memoria-actualizada (vgp-act-esc-rol-obj memoria-compartida informacion memoria-procesada id coordenada))
        (pintar (vgp-decision-pintar informacion memoria-procesada memoria-actualizada id coordenada tr-pintar color))
        (movimiento-memoria (vgp-decision-mover informacion memoria-procesada memoria-actualizada id coordenada color tr-moure))
        )
        (cond
            ((null movimiento-memoria) ;;si no se mueve
                (remove nil ;;por si pintar es nil, no incluirlo en la lista de acciones
                    (list
                        (cond 
                            ((null pintar) nil)
                            (t (list 'pinta pintar))
                        )
                        (list 'escriu-memoria (list memoria-actualizada))
                    )
                )
            )
            (t ;;si se mueve
                (remove nil
                    (list
                        (cond 
                            ((null pintar) nil)
                            (t (list 'pinta pintar))
                        )
                        (list 'mou (car movimiento-memoria))
                        (list 'escriu-memoria (list (cadr movimiento-memoria)))
                    )
                )
            )
        )
    
    )
    
)

(defun-tco vgp-comprobar-reg (unidades id)
  "Comprueba si una unidad con la id dada ya tiene un registro en memoria."
    (cond
        ((null unidades) nil) ;; No encontrada
        ((eq id (caar unidades)) t) ;; Encontrada 
        (t (vgp-comprobar-reg (cdr unidades) id))
    )
) 

(defun-tco vgp-actualizar-escuadron (nueva-info memoria-compartida acc)
    "Actualiza un registro de escuadrón en memoria manteniendo el orden."
    (cond
        ((null memoria-compartida)
            (reverse acc))
        (t 
            (let* (
                (registro (car memoria-compartida))
                (codigo (car registro))
                (id-escuadron (cadr registro)))
                (cond
                    ((and (eq codigo 2) (eq id-escuadron (car nueva-info)))
                        ;; Encontrado: reconstruir la lista invertida + actualizada + resto
                        (append 
                            (reverse acc) 
                            (cons (cons '2 nueva-info) (cdr memoria-compartida))
                        )
                    )
                    (t 
                        (vgp-actualizar-escuadron nueva-info (cdr memoria-compartida) (cons registro acc))
                    ) 
                )
            )
        )
    )
)

(defun-tco vgp-buscar-escuadron-vacante (escuadrones unidades color)
    "Busca un escuadrón que tenga un hueco para el color dado. Retorna el id del escuadrón o nil si no hay."
    (cond
        ((null escuadrones) nil)
        (t 
            (let ((escuadron (car escuadrones)))
                (cond
                    ((< (length escuadron) 4) ;;si midemos el tamaño del escuadron y el numero de unidades es menor, hay hueco
                        (let* (
                            (id-escuadron (car escuadron))
                            (unidad1 (cadr escuadron))
                            (unidad2 (caddr escuadron))
                            (color1 (vgp-color-unidad unidad1 unidades))
                            (color2 (vgp-color-unidad unidad2 unidades)))
                            (cond
                                ;; Si ambos colores son distintos al que queremos añadir, hay hueco
                                ((and (not (equal color1 color)) (not (equal color2 color)))
                                    (remove nil (list id-escuadron unidad1 unidad2))
                                )
                                (t  ;; Seguir buscando en el resto de escuadrones
                                    (vgp-buscar-escuadron-vacante (cdr escuadrones) unidades color)
                                ) 
                            )
                        )
                    )
                    (t ;; No hay hueco, seguir buscando
                        (vgp-buscar-escuadron-vacante (cdr escuadrones) unidades color)
                    )
                )
            )    
        )
    )
)

(defun-tco vgp-color-unidad (id unidades)
    "Dada una id de unidad, devuelve su color consultando la lista de unidades aliadas."
    (cond
        ((or (null id) (null unidades))
            nil
        )
        (t 
            (let* (
                (unidad (car unidades))
                (id-unidad (car unidad))
                (color-unidad (cadddr unidad)))
                (cond
                    ((eq id id-unidad)
                        color-unidad
                    )
                    (t (vgp-color-unidad id (cdr unidades)))
                )
            )
        )
    )
)

(defun-tco vgp-max-id-escuadron (escuadrones max-actual)
    "Devuelve el id máximo de escuadrón en la lista, o 0 si no hay escuadrones."
    (cond
        ((null escuadrones) max-actual)
        (t (vgp-max-id-escuadron (cdr escuadrones) (max (car (car escuadrones)) max-actual)))
    )
)

(defun agente-vgp522-distancia (coord1 coord2)
    "Devuelve la distancia al cuadrado (euclidiana) entre dos coordenadas."
    (let ((dx (- (car coord1) (car coord2)))
          (dy (- (car (cdr coord1)) (car (cdr coord2)))))
        (+ (* dx dx) (* dy dy))
    )
)

(defun vgp-act-esc-rol-obj (memoria-compartida informacion memoria-procesada id coordenada)
    "Actualiza el rol del escuadron del agente y mantiene objetivos visibles."
    (let* (
        (tamano (car memoria-compartida))
        (base-aliada (cadr memoria-compartida))
        (base-enemiga (car informacion))
        (bolas-enemigas (cadr informacion))
        (labs-capturables (caddr informacion))
        (amigos (cadddr informacion))
        (casillas-libres (nth 4 informacion))
        (escuadron-id (nth 2 memoria-procesada))
        (escuadrones (nth 3 memoria-procesada))
        (roles (nth 4 memoria-procesada))
        (base-enemiga-memoria (nth 6 memoria-procesada))
        (objetivos (nth 7 memoria-procesada))
        (amigos-memoria (nth 8 memoria-procesada)))
        (labels
            (
                (vgp-escuadron-completo-p (id-esc)
                    (let ((escuadron (vgp-buscar-escuadron id-esc escuadrones)))
                        (cond
                            ((null escuadron) nil)
                            ((>= (length escuadron) 4) t)
                            (t nil)
                        )
                    )
                )
                (vgp-hay-defensa-completa-p (lista)
                    (cond
                        ((null lista) nil)
                        ((and (not (equal escuadron-id (caar lista))) (eq (cadr (car lista)) 'defensa) (vgp-escuadron-completo-p (car (car lista)))) t)
                        (t (vgp-hay-defensa-completa-p (cdr lista)))
                    )
                )
                (vgp-añadir-base-enemiga (memoria base-enemiga registro-memoria)
                    (cond
                        ((null base-enemiga) memoria)
                        ((null registro-memoria) (append memoria (list (list 5 base-enemiga))))
                        (t memoria)
                    )
                )
            )
            (let* (;;las actualizaciones de objetivos estan comentadas porque se dejan de lado en esta version mas basica
                (esc-completo (vgp-escuadron-completo-p escuadron-id))
                (rol-actual (cadr (vgp-buscar-escuadron escuadron-id roles)))
                (rol-deseado 
                    (cond
                        ((not esc-completo)  'defensa) 
                        ((not (vgp-hay-defensa-completa-p roles)) 'defensa)
                        ((null base-enemiga) 'exploracion)
                        (t 'ataque)
                    )
                )
                (bolas-desaparecidas (vgp-buscar-bolas amigos-memoria amigos coordenada));;comprueba si las bolas en rango visible registradas en memoria estan visibles
                ;;(objetivos-desaparecidos (vgp-buscar-objetivos objetivos labs-capturables bolas-enemigas))
                ;;(nuevos-objetivos (vgp-anade-objetivos-labs labs-capturables (vgp-anade-objetivos-bolas bolas-enemigas objetivos)))
                (memoria-actualizada0 (vgp-añadir-base-enemiga memoria-compartida base-enemiga base-enemiga-memoria)) ;;si la base enemiga es visible, actualizar su registro en memoria (si ya estaba registrada, no se añade nada)
                (memoria-actualizada1 (vgp-eliminar-bolas (car memoria-actualizada0) (cadr memoria-actualizada0) (cddr memoria-actualizada0) bolas-desaparecidas nil));;se eliminan las bolas desaparecidas de la memoria y se quitan de su escuadron
                (direccion-actual (cond ;;si el rol deseado es exploracion, busca la direccion actual
                    ((equal rol-deseado 'exploracion) (cadr (vgp-buscar-escuadron escuadron-id (nth 5 memoria-procesada))))
                    (t memoria-actualizada1)
                ))
                (direccion-deseada (cond ;;si el rol deseado es exploracion, calcula la direccion deseada
                    ((equal rol-deseado 'exploracion) (vgp-calcular-direccion coordenada casillas-libres))
                    (t memoria-actualizada1)
                ))
                (memoria-actualizada2 (cond ;;si el rol deseado es exploracion, y cambia la direccion, actualizar la memoria con la nueva direccion 
                    ((and (not (equal direccion-actual direccion-deseada)) (equal rol-deseado 'exploracion)) (vgp-actualizar-direccion direccion-deseada (car memoria-actualizada1) (cdr memoria-actualizada1) escuadron-id coordenada nil))
                    (t memoria-actualizada1)
                ))
                ;;(memoria-actualizada2 (vgp-eliminar-objetivos memoria-actualizada1 objetivos-desaparecidos));;se eliminan los objetivos desaparecidos de la memoria
                ;;(memoria-actualizada3 (vgp-anadir-objetivos memoria-actualizada2 nuevos-objetivos));;se añaden los nuevos objetivos de la vision a la memoria
                )
                (cond
                    ((equal rol-actual rol-deseado)
                        memoria-actualizada2
                    )
                    (t
                        (vgp-reasignar-rol rol-deseado rol-actual escuadron-id (car memoria-actualizada2) (cdr memoria-actualizada2) nil)
                    )
                )
            )
        )
    )
)

(defun vgp-buscar-bolas (lista-amigos-memoria amigos-vision coordenada);;memoria (idbola x y color) vision (coord color-propio colores-pintado)
    ;;para cada amigo en memoria comprueba si esta en rango y si es asi comprueba q esta en vision
    "lista de idbolas en rango de vision que no se ven pero que en memoria estaban registradas"
    (cond
        ((null lista-amigos-memoria) nil)
        (t 
            (let* (
                (amigo-memoria (car lista-amigos-memoria))
                (coord-amigo-memoria (list (cadr amigo-memoria) (caddr amigo-memoria)))
            )
                (cond
                    ((>= 20 (agente-vgp522-distancia coordenada coord-amigo-memoria))
                        (let (
                            (amigo-vision (vgp-buscar-amigo coord-amigo-memoria amigos-vision)))
                            (cond
                                ((null amigo-vision) (cons (car amigo-memoria) (vgp-buscar-bolas (cdr lista-amigos-memoria) amigos-vision coordenada)))
                                (t (vgp-buscar-bolas (cdr lista-amigos-memoria) amigos-vision coordenada))
                            )
                        )
                    )
                    (t (vgp-buscar-bolas (cdr lista-amigos-memoria) amigos-vision coordenada)) ;;si no esta en rango, no se puede ver
                )
            )
        )
    )
)
(defun vgp-buscar-amigo (coord lista-amigos)
    "Busca un amigo en la lista de amigos de la vision por su coordenada. Retorna el amigo o nil si no se encuentra."
    (cond
        ((null lista-amigos) nil)
        ((equal coord (caar lista-amigos)) (car lista-amigos))
        (t (vgp-buscar-amigo coord (cdr lista-amigos)))
    )
)

(defun vgp-calcular-direccion (coordenada casillas-libres)
  "Calcula la dirección usando recursión pura, cond y sin setq."
    (labels (
        ;; Función recursiva para contar casillas en las 4 direcciones
        ;; Retorna una lista con el formato (n s e o)
        (contar-vistas (cx cy lista)
            (cond 
            ((null lista) (list 0 0 0 0)) ; Caso base: lista vacía
                (t 
                    (let* ((item (car lista))
                        (tx (first (first item))) ; Coordenada X del elemento
                        (ty (second (first item))) ; Coordenada Y del elemento
                        (resto (contar-vistas cx cy (cdr lista)))
                        (n (first resto)) (s (second resto))
                        (e (third resto)) (o (fourth resto)))
                        ;; Construimos la lista incrementando según la posición relativa
                        (list (cond ((< ty cy) (+ 1 n)) (t n))
                        (cond ((> ty cy) (+ 1 s)) (t s))
                        (cond ((> tx cx) (+ 1 e)) (t e))
                        (cond ((< tx cx) (+ 1 o)) (t o)))
                    )
                )
            )
        )

        ;; Función recursiva para encontrar la dirección con el valor máximo
        (obtener-mejor (direcciones)
            (cond 
                ((null (cdr direcciones)) (car direcciones)) ; Solo queda uno
                (t 
                    (let ((mejor-del-resto (obtener-mejor (cdr direcciones))))
                        (cond 
                            ((> (cdr (car direcciones)) (cdr mejor-del-resto)) (car direcciones))
                            (t mejor-del-resto)
                        )
                    )
                )
            )
        )
        )

        (let* ((cx (first coordenada))
            (cy (second coordenada))
            (conteos (contar-vistas cx cy casillas-libres))
            (n-count (first conteos))
            (s-count (second conteos))
            (e-count (third conteos))
            (o-count (fourth conteos)))
        
        (cond
            ;; 1. Lógica de paredes (si una dirección es 0, ir al lado opuesto)
            ((= n-count 0) 's)
            ((= s-count 0) 'n)
            ((= e-count 0) 'o)
            ((= o-count 0) 'e)
            
            ;; 2. Si no hay paredes inmediatas, buscar el máximo
            (t (car 
                (obtener-mejor (list (cons 'n n-count) (cons 's s-count) (cons 'e e-count) (cons 'o o-count))))
            )
        )
        )
    )
)

(defun-tco vgp-reasignar-rol (rol-deseado rol-actual id-escuadron tam memoria acc)
    "Actualiza el rol de un escuadrón en memoria."
    (cond
        ((null memoria)  
                        ;; no encontrado: reconstruir la lista invertida + nuevo registro + resto
                        (append 
                            (list (- tam 3)) ;;actualizar tamaño de memoria
                            (reverse acc) 
                            (list (list 3 id-escuadron rol-deseado)) 
                            (cdr memoria)
                        ))
        (t 
            (let* (
                (celda (car memoria))
                (codigo (car celda))
                (id-esc (cadr celda)))
                (cond
                    ((and (eq codigo 3) (eq id-esc id-escuadron))
                        ;; Encontrado: reconstruir la lista invertida + actualizada + resto
                        (append 
                            (list tam)
                            (reverse acc) 
                            (list (list 3 id-escuadron rol-deseado)) 
                            (cdr memoria)
                        )
                    )
                    (t 
                        (vgp-reasignar-rol rol-deseado rol-actual id-escuadron tam (cdr memoria) (cons celda acc))
                    ) 
                )
            )
        )
    )
)

(defun-tco vgp-actualizar-direccion (direccion-nueva tam bruto-memoria id-escuadron coordenada acc)
    "Actualiza el registro de direccion del escuadron en memoria (codigo 4) a esa nueva direccion"
    (cond
        ((null bruto-memoria)  
            ;; no encontrado: reconstruir la lista invertida + nuevo registro + resto
            (append 
                (list (- tam 3)) ;;actualizar tamaño de memoria
                (reverse acc) 
                (list (list 4 id-escuadron direccion-nueva)) 
            )
        )
        (t 
            (let* (
                (celda (car bruto-memoria))
                (codigo (car celda))
                (id-esc (cadr celda)))
                (cond
                    ((and (eq codigo 4) (eq id-esc id-escuadron))
                        ;; Encontrado: reconstruir la lista invertida + actualizada + resto
                        (append 
                            (list tam)
                            (reverse acc) 
                            (list (list 4 id-escuadron direccion-nueva)) 
                            (cdr bruto-memoria)
                        )
                    )
                    (t 
                        (vgp-actualizar-direccion direccion-nueva tam (cdr bruto-memoria) id-escuadron coordenada (cons celda acc))
                    ) 
                )
            )
        )
    )
)

(defun-tco vgp-eliminar-bolas (tamaño base-aliada bruto-memoria bolas-eliminar acc)
    "Elimina de la memoria los registros de las bolas que han desaparecido."
    (cond
        ((null bolas-eliminar) (append (list tamaño base-aliada) bruto-memoria))
        ((null bruto-memoria) (append (list tamaño base-aliada) (reverse acc)))
        (t 
            (labels
                ((vgp-en-eliminar (idbola lista)
                    (cond
                        ((null lista) nil)
                        ((eq idbola (car lista)) t)
                        (t (vgp-en-eliminar idbola (cdr lista)))
                    )
                ))
                (let* (
                    (celda (car bruto-memoria))
                    (codigo (car celda))
                    )
                    (cond ;;se mira la longitud de la celda para no confundirlo con la coord de la base de la cabecera de la memoria
                        ((and (eq (length celda) 5) (eq codigo 7)) ;;registro de bola amiga (7 idbola x y color) 
                            (cond
                                ((vgp-en-eliminar (cadr celda) bolas-eliminar) ;;si la id de la bola coincide con la que queremos eliminar, no la añadimos a la memoria actualizada
                                    (vgp-eliminar-bolas (- tamaño 5) base-aliada (cdr bruto-memoria) bolas-eliminar acc)
                                )
                                (t ;;si no coincide, mantenemos el registro en la memoria y seguimos buscando
                                    (vgp-eliminar-bolas tamaño base-aliada (cdr bruto-memoria) bolas-eliminar (cons celda acc))
                                )
                            )
                        )
                        ((and (eq (length celda) 3) (eq codigo 1));;registro de escuadron (1 idbola idesc)
                            (cond
                                ((vgp-en-eliminar (cadr celda) bolas-eliminar) ;;si la id de la bola coincide con la que queremos eliminar, no la añadimos a la memoria actualizada
                                    (vgp-eliminar-bolas (- tamaño 3) base-aliada (cdr bruto-memoria) bolas-eliminar acc)
                                )
                                (t ;;si no coincide, mantenemos el registro en la memoria y seguimos buscando
                                    (vgp-eliminar-bolas tamaño base-aliada (cdr bruto-memoria) bolas-eliminar (cons celda acc))
                                )
                            )
                        )
                        ((eq codigo 2) ;; registro de escuadron (2 idesc idbola1 idbola2 idbola3)
                            (let* (
                                (idesc (cadr celda))
                                (idbola1 (caddr celda))
                                (idbola2 (nth 3 celda))
                                (idbola3 (nth 4 celda))
                                (bolas-nuevas 
                                    (append ;;reconstruye la celda solo con las bolas que no hay q eliminar
                                        (cond ((or (null idbola1) (vgp-en-eliminar idbola1 bolas-eliminar)) nil) (t (list idbola1)))
                                        (cond ((or (null idbola2) (vgp-en-eliminar idbola2 bolas-eliminar)) nil) (t (list idbola2)))
                                        (cond ((or (null idbola3) (vgp-en-eliminar idbola3 bolas-eliminar)) nil) (t (list idbola3)))
                                    )
                                )
                                (celda-nueva (cons 2 (cons idesc bolas-nuevas)))
                                )
                                (vgp-eliminar-bolas tamaño base-aliada (cdr bruto-memoria) bolas-eliminar (cons celda-nueva acc))
                            )
                        )
                        (t ;;si no es un registro de bola, lo mantenemos en la memoria y seguimos buscando
                            (vgp-eliminar-bolas tamaño base-aliada (cdr bruto-memoria) bolas-eliminar (cons celda acc))
                        )
                    )
                )
            )
        )
    )
)

(defun vgp-decision-pintar (informacion memoria-procesada memoria-actualizada id coordenada tr-pintar color)
    "Elige el mejor objetivo de pintado disponible dentro de rango."
    (cond
        ((>= tr-pintar 1) nil)
        (t
            (let* (
                (base-enemiga (car informacion))
                (bolas-enemigas-sin-color (vgp-bolas-sin-color (cadr informacion) color))
                (bolas-enemigas (vgp-extrae-coords-visibles bolas-enemigas-sin-color))
                (labs-capturables (caddr informacion))
                (objetivo 
                    (cond
                        ;;si esta en rango, esta registrada y no esta pintada del color de la bola
                        ((and (not (null base-enemiga)) (not (member color (cadr base-enemiga))) (<= (agente-vgp522-distancia coordenada (car base-enemiga)) 5))
                            (car base-enemiga)
                        ) 
                        ;;buscar bolas en rango
                        ((vgp-mejor-coord-en-rango coordenada bolas-enemigas))
                        ;;buscar labs en rango
                        ((vgp-mejor-coord-en-rango coordenada labs-capturables))
                        ;;sin objetivos en rango
                        (t nil)
                    )))
                (cond
                    ((null objetivo) nil)

                    (t (list objetivo))
                )
            )
        )
    )
)

(defun vgp-bolas-sin-color (lista color)
    (cond
        ((null lista) nil)
        ((or (equal color (cadr (car lista))) (member color (caddr (car lista))))
            (vgp-bolas-sin-color (cdr lista) color)
        )
        (t
            (cons (car lista) (vgp-bolas-sin-color (cdr lista) color))
        )
    )
)

(defun vgp-extrae-coords-visibles (lista)
    (cond
        ((null lista) nil)
        (t 
            (cons (car (car lista)) (vgp-extrae-coords-visibles (cdr lista)))
        )
    )
)

(defun vgp-mejor-coord-en-rango (origen coords)
    (labels
        ((rec (lista mejor)
            (cond
                ((null lista) mejor)
                (t
                    (let* ((coord (car lista)) (dist (agente-vgp522-distancia origen coord)))
                        (cond
                            ((and (<= dist 5) (or (null mejor) (< dist (agente-vgp522-distancia origen mejor))))
                                (rec (cdr lista) coord)
                            )
                            (t (rec (cdr lista) mejor))
                        )
                    )
                )
            )
        ))
        (rec coords nil)
    )
)

(defun vgp-decision-mover1 (informacion mem-procesada memoria-compartida id coordenada color tr-moure)
    "Elige el mejor desplazamiento y devuelve una lista con la nueva coordenada y la memoria con la posicion de la bola actualizada."
    (let* (
        (casillas-libres (nth 4 informacion))) ;;lista de ((x y) color)
        (cond
            ((or (not (equal tr-moure 0)) (null (cadddr casillas-libres)))
                nil
            )
            (t
                (list       
                    (cadddr casillas-libres)
                    (vgp-actualiza-memoria-posicion (cddr memoria-compartida) (car memoria-compartida) (cadr memoria-compartida) id (car (cadddr casillas-libres)) nil)
                )
            )
        )
    )
)

(defun vgp-decision-mover (informacion mem-procesada memoria-compartida id coordenada color tr-moure)
    "Elige el mejor desplazamiento y devuelve una lista con la nueva coordenada y la memoria con la posicion de la bola actualizada."
    (cond
        ((>= tr-moure 1) nil)
        (t
            (let* (
                (base-enemiga (car informacion)) ;;((x y) colores-pintado)
                (bolas-enemigas (cadr informacion)) ;;lista de ((x y) color-propio) 
                (labs-capturables (caddr informacion)) ;;lista de (x y)
                (aliados-vista (cadddr informacion)) ;;lista de (coord tipo)
                (casillas-libres (nth 4 informacion)) ;;lista de ((x y) color)
                (tamano (car memoria-compartida)) ;; entero
                (base-aliada (cadr memoria-compartida)) ;;(x y)
                (escuadron-id (nth 2 mem-procesada));; entero
                (escuadrones (nth 3 mem-procesada));; lista de (id-esc idbola1 idbola2 idbola3)
                (roles (nth 4 mem-procesada));; lista de (id-esc rol)
                (direcciones (nth 5 mem-procesada)) ;;lista de (id-esc direccion)
                (base-enemiga-memoria (nth 6 mem-procesada)) ;;(x y) o nil
                ;;(objetivos (nth 7 mem-procesada)) ;;se dejan de lado los objetivos en esta version mas basica
                (unidades-aliadas (nth 8 mem-procesada)) ;;lista de (idbola x y color)
                (memoria-bruta (cddr memoria-compartida)) ;;lista de registros sin la cabecera (tamaño y coordenada de base aliada)
                (rol-escuadron (cadr (vgp-buscar-escuadron escuadron-id roles))) ;; 'defensa, 'exploracion o 'ataque
                (direccion-escuadron (cadr (vgp-buscar-escuadron escuadron-id direcciones))) ;;'n, 's, 'e o 'o
                (escuadron (vgp-buscar-escuadron escuadron-id escuadrones));; (id-esc idbola1 idbola2 idbola3)
                (punto-medio-escuadron (vgp-calcular-punto-medio (cdr escuadron) unidades-aliadas))
                )
                (cond
                    ;;si el rol es exploracion, moverse en la direccion asignada a ese rol
                    ((equal rol-escuadron 'exploracion)
                        ;;se mueve en la direccion aignada a la casilla libre mas cercana del punto medio del escuadron
                    )
                    ((equal rol-escuadron 'defensa)
                        ;;se queda en cualquier casilla libre a cercana a la base pero sin ser adyacente a la base
                    )
                    ((equal rol-escuadron 'ataque)
                        ;;se mueve hacia la casilla libre mas cercana a la base enemiga, si hay un lab capturable, solo se mueve hacia el lab si este esta mas cerca que la base enemiga. de estas coordenadas elige la mas cercana al punto medio del escuadron
                    )
                )
            )
        )
    )
)

(defun-tco vgp-actualiza-memoria-posicion (memoria-bruta tamano base-aliada id coordenada-nueva acc)
    "cambia la coordenada de la bola en su registro de memoria"
    (cond
        ((null memoria-bruta)
            (append (list tamano base-aliada) (reverse acc))
        )
        (t 
            (let* (
                (celda (car memoria-bruta))
                (codigo (car celda))
                )
                (cond 
                    ((and (eq id (cadr celda)) (eq codigo 7)) ;;registro de bola amiga (7 idbola x y color) que queremos actualizar
                        (append 
                            (list tamano base-aliada)
                            (reverse acc) 
                            (list (list 7 id (car coordenada-nueva) (cadr coordenada-nueva) (nth 4 celda))) ;;nuevo registro con la coordenada actualizada
                            (cdr memoria-bruta)
                        )
                    )
                    (t ;;si no es un registro de bola, lo mantenemos en la memoria y seguimos buscando
                        (vgp-actualiza-memoria-posicion (cdr memoria-bruta) tamano base-aliada id coordenada-nueva (cons celda acc))
                    )
                )
            )
        )
    )
)

(defun vgp-calcular-punto-medio (ids unidades)
    "Dada una lista de ids de bolas y la lista de unidades aliadas, calcula el punto medio de las coordenadas de esas bolas."
    (let*(
        (coord1 (vgp-coordenada-bola (car ids) unidades))
        )
    )
)


(defun vgp-coordenada-bola (id-bola unidades) ;;(idbola x y color)
    "Dada el id de una bola y la lista de unidades aliadas, devuelve sus coordenadas."
    (cond
        ((null unidades) nil)
        ((eq id-bola (caar unidades)) (list (cadr (car unidades)) (caddr (car unidades))))
        (t (vgp-coordenada-bola id-bola (cdr unidades)))
    )
)

(defun vgp-buscar-escuadron (id-esc lista)
    (cond
        ((null lista) nil)
        ((eq id-esc (car (car lista))) (car lista))
        (t (vgp-buscar-escuadron id-esc (cdr lista)))
    )
)

(defun vgp-min-dist-coords (origen coords)
    (labels
        ((rec (lista mejor)
            (cond
                ((null lista) mejor)
                (t
                    (let ((coord-actual (car lista)))
                        (let ((dist (agente-vgp522-distancia origen coord-actual)))
                            (rec (cdr lista) (cond ((or (null mejor) (< dist mejor)) dist) (t mejor)))
                        )
                    )
                )
            )
        ))
        (rec coords nil)
    )
)