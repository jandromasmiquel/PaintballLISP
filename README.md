# Paintball LISP 🎨

Práctica final de la asignatura **Llenguatges de Programació** (LISP).  
Un juego de simulación de estrategia y combate Paintball por turnos con agentes autónomos inteligentes y representación gráfica bidimensional sobre mapas de cuadrícula.

---

## 📋 Descripción del Proyecto

El proyecto consiste en un motor de juego en **XLISP** donde dos equipos compiten por controlar el mapa pintando casillas y eliminando/inhabilitando unidades enemigas. Los agentes toman decisiones autónomas en cada turno utilizando escuadrones, roles estratégicos (exploración, ataque, defensa), memoria compartida y visibilidad limitada (línea de visión / niebla de guerra).

### ✨ Características Principales
- **Motor de Juego por Turnos:** Hasta 1500 rondas con gestión de unidades, laboratorios, pintura y economía.
- **Interfaz Gráfica Integrada:** Visualización en tiempo real del tablero, casillas pintadas por cada equipo y estado de unidades mediante primitivas XLISP Win32.
- **Inteligencia Artificial Autónomas:** Agentes (`agent-amc358.lsp`, `agent-vgp522.lsp`) con arquitectura de escuadrones y toma de decisiones basada en roles.
- **Línea de Visión (Fog of War):** Cálculo dinámico de visibilidad para cada unidad (`vision.lsp`).
- **Optimización TCO (Tail Call Optimization):** Macro `defun-tco` (`tco.lsp`) que transforma recursión de cola en bucles iterativos para evitar desbordamientos de pila (*stack overflow*).
- **Soporte de Mapas:** Soporta mapas de diversos tamaños (hasta 60x60) incluidos en la carpeta `maps/`.

---

## 🗂️ Estructura del Código

```plain
├── xlwin32.exe         # Intérprete de XLISP 2.1 / XLISP-STAT para Windows
├── paintball.lsp       # Controlador principal y bucle del juego (inicio, rondas, fin)
├── grafics.lsp         # Módulo de renderizado gráfico de la interfaz
├── acciones.lsp        # Definición de acciones de unidades (mover, disparar, pintar, etc.)
├── vision.lsp          # Algoritmo de línea de visión y detección de objetivos
├── tco.lsp             # Macro defun-tco para optimización de llamadas de cola
├── common.lsp          # Funciones auxiliares y estructuras de datos generales
├── agent-amc358.lsp    # Agente IA del Equipo 1 (AMC358)
├── agent-vgp522.lsp    # Agente IA del Equipo 2 (VGP522)
├── a.lsp               # Script de carga rápida
└── maps/               # Colección de mapas (.map)
```

---

## 🚀 Cómo Ejecutar y Probar el Juego

### Requisitos
- Sistema Operativo **Windows** (el proyecto incluye `xlwin32.exe`).

### Pasos para Ejecutar

1. **Abrir el ejecutable:**
   Haz doble clic en `xlwin32.exe` en la raíz del proyecto. Se abrirá la consola interactiva de XLISP y la ventana gráfica.

2. **Cargar el juego:**
   En la consola de XLISP, escribe el siguiente comando y pulsa `Enter`:
   ```lisp
   (load "a.lsp")
   ```
   *(También puedes usar `(load "paintball.lsp")`)*

3. **Iniciar la partida:**
   Para iniciar con el mapa por defecto (`maps/basic1.map`):
   ```lisp
   (inicio)
   ```
   Para especificar un mapa concreto:
   ```lisp
   (inicio "maps/lake.map")
   ```
   *(Mapas disponibles en `maps/`: `basic1.map`, `basic2.map`, `lake.map`, `random.map`, `huge.map`, `flag.map`, etc.)*

4. **Avanzar rondas:**
   Pulsa la tecla `ENTER` en la consola para hacer avanzar cada turno. Mantenla pulsada para avanzar de forma continua hasta el final de la partida.

---

## 👥 Autores
- **Estudiantes:** VGP y AMC  
- **Asignatura:** Llenguatges de Programació  
- **Fecha:** 03/05/2026  
