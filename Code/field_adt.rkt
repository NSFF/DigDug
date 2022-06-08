#lang r5rs

(#%require "position_adt.rkt")
(#%require (only racket/base error))
(#%require (only racket/base random))

(#%provide field)

;field ADT: holds information where all the game-elements are located onto the screen (into a vector)
(define (field w h character_w/h gameboard_resolution speler_speed air_space)
  
  (let ((initialized-field (make-vector (expt gameboard_resolution 2) 'ground)))

    (define (reinitialize-field!)
      (set! initialized-field (make-vector (expt gameboard_resolution 2) 'ground)))
    
    (define (field_x_y x y);puur estetische rede dat dit bestaat
      (+ x (* y gameboard_resolution)))

    ;returns the keyword at location x y of the field vector
    (define (field_get x y)
      (vector-ref initialized-field  (field_x_y x y)))

    ;changes a keyword to another if necessary
    (define (field_set! x y keyword)
      ;keywords: 'empty ; 'ground ; 'enemy ; 'rock
      (if (not (eq? (field_get x y) keyword))
          (vector-set! initialized-field (field_x_y x y) keyword)))

    ;checks if at position x y the keyword is empty or an enemy
    (define (field_empty? x y)
      (if (and (< x gameboard_resolution) (>= x 0)
               (< y gameboard_resolution) (>= y 0))
          (eq? (field_get x y) 'empty)
          #f))
    (define (field_enemy? x y)
      (if (and (< x gameboard_resolution) (>= x 0)
               (< y gameboard_resolution) (>= y 0))
          (eq? (field_get x y) 'enemy)
          #f))

    ;makes a rectangle amount of points in the vector the same keyword
    (define (field_set_rect_keyword! x0 y0 x1 y1 keyword);replace a rectangle in the field to the keyword
      (define (iter cntr-x cntr-y)
        (cond ((>= cntr-y y1))
              ((>= cntr-x x1)
               (field_set! cntr-x cntr-y keyword)
               (iter 0 (+ cntr-y 1)))
              (else
               (field_set! cntr-x cntr-y keyword)
               (iter (+ cntr-x 1) cntr-y))))
      (iter x0 y0))

    ;sets a point in the field to another keyword
    (define (field_set_character_keyword! x y keyword)
      (field_set! x y keyword))

    ;spawns a tunnel into the field vector and draws it through the teken-adt
    (define (spawn_tunnel! x y teken-adt length)
      (define (iter x n)
        (cond ((or (= n 0) (>= x gameboard_resolution)))
              (else (field_set_character_keyword! x y 'empty)
                    ((teken-adt 'teken-tunnel!) (* x speler_speed) (* y speler_speed))
                    (iter (+ x 1) (- n 1)))))
      (iter x length))

    ;initializes the tunnels, this is called at the start of the game-loop
    (define (initialize_tunnel! amount_of_tunnels air_space teken-adt min_tunnel_length max_tunnel_length)
      (define (random_number_between min max)
        (let ((number (random max)))
          (if (< number min)
              (random_number_between min max)
              number)))
      
      (define (iter amount_of_tunnels)
        (cond ((= amount_of_tunnels 0))
              (else (spawn_tunnel! (random_number_between 0 (- gameboard_resolution max_tunnel_length));this prevents tunnels being spawned at the edge of the map
                                   (random_number_between air_space gameboard_resolution)
                                   teken-adt
                                   (random_number_between min_tunnel_length max_tunnel_length))
               (iter (- amount_of_tunnels 1)))))
      (iter amount_of_tunnels))

    ;gives a random position in one of the tunnels
    (define (get_random_tunnel_pos keyword)
      (define (random_y max)
      (let ((y (random max)))
        (cond ((<= y air_space) (+ y air_space 1))
              (else y))))
      
      (define (keyword=? keyword init_keyword)
        (eq? keyword init_keyword))
      
      (define (iter)
        (let ((x (random gameboard_resolution))
              (y (random_y gameboard_resolution)))
        (cond ((eq? (field_get x y) keyword)
               (((position) 'new) x  y))
              (else (iter)))))
      (iter))

    
    (define (dispatch msg)
      (cond ((eq? msg 'get_random_tunnel_pos) (get_random_tunnel_pos 'empty))
            ((eq? msg 'get_random_rock_pos) (get_random_tunnel_pos 'ground))
            ((eq? msg 'initialize_tunnel!) initialize_tunnel!)
            ((eq? msg 'reinitialize-field!) (reinitialize-field!))
            ((eq? msg 'field_get) field_get)
            ((eq? msg 'field_set!) field_set!)
            ((eq? msg 'field_empty?) field_empty?)
            ((eq? msg 'field_enemy?) field_enemy?)
            ((eq? msg 'field_set_rect_keyword!) field_set_rect_keyword!)
            ((eq? msg 'field_set_character_keyword!) field_set_character_keyword!)
            ((eq? msg 'print) (display initialized-field))
            (else (error "not supported: " msg))))
    dispatch))