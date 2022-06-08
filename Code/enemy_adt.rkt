#lang r5rs

(#%require "position_adt.rkt")
(#%require "field_adt.rkt")
(#%require (only racket/base error))
(#%require (only racket/base random))

(#%provide enemy)

;enemy ADT: provides the location and movement of an enemy
(define (enemy x y speler_speed initialized_field keyword enemy_number)
  (let* ((enemy_position (((position) 'new) x y))
        (monster keyword)
        (enemy_ID enemy_number)
        (vector_of_enemies (vector 'fygar 'pooka))
        (amount_of_different_kind_of_enemies (vector-length vector_of_enemies))
        (attack_status 'off)
        (random_goggle_time (random 20000))
        (ghostly_goggle_time 0)
        (ghostly_goggle_status 'off)
        (attack-time 0)
        (direction_x 1) ;1= rechts -1= links 0= geen richting
        (direction_y 0)) ;1= boven -1= onder 0= geen richting

    (define (lower_ID!)
      (set! enemy_ID (- enemy_ID 1)))
    
    (define (x)
      (((position) 'x) enemy_position))
    (define (y)
      (((position) 'y) enemy_position))
    
    (define (set-x! x)
      (((position) 'set-x!) enemy_position x))
    (define (set-y! y)
      (((position) 'set-y!) enemy_position y))
    
    (define (set-xy! x y)
      (set-x! x)
      (set-y! y))
    
    (define (teken! teken-adt);redraws all the enemies currently in the game (it can be used to make sure all positions are visually updated)
      (teken-adt 'teken-enemies!))
    
    (define (switch_appearance! teken-adt keyword);changes the appearance of the monster to a ghostly goggle
      ((teken-adt 'change_enemy_appearance!) (x) (y) keyword))

    (define (add_attack_time! time)
      (set! attack-time (+ attack-time time)))
    
    (define (add_ghostly_goggle_time! time);this procedure is similar to add_attack_time but was intentionaly split because of reading purposes
      (set! ghostly_goggle_time (+ ghostly_goggle_time time)))

    ;this procedure gives you a flame attack in the game(only used on Fygar monster)
    (define (attack! teken-adt)
      (set! attack-time 0)
      (if (check_direction)
          (begin ((initialized_field 'field_set_character_keyword!) (/ (+ (x) (* direction_x speler_speed))
                                                                       speler_speed)
                                                                    (/ (+ (y) (* direction_y speler_speed))
                                                                       speler_speed)
                                                                    'enemy)
                 ((teken-adt 'teken-fygar-aanval!) (+ (x) (* direction_x speler_speed))
                                                   (+ (y) (* direction_y speler_speed)))
                 (set! attack_status 'on))))
   
              

    ;gives a random direction and updates the direction variables
    (define (random_direction!)
      (let ((n (random 4)))
        (cond ((= n 0) (set! direction_x 1)
                       (set! direction_y 0))
              ((= n 1) (set! direction_x -1)
                       (set! direction_y 0))
              ((= n 2) (set! direction_x 0)
                       (set! direction_y 1))
              (else (set! direction_x 0)
                    (set! direction_y -1)))))

    ;checks if there is collision or not
    (define (no_collision?)
      ((initialized_field 'field_empty?) (/ (+ (x) (* direction_x speler_speed))
                                            speler_speed)
                                         (/ (+ (y) (* direction_y speler_speed))
                                            speler_speed)))
    
    ;checks if the direction chosen by "random_direction!" gives no collision if we move
    (define (Check_direction)
      (define (iter n)
        (cond ((= n 0) #f)
              ((no_collision?) #t)
              (else (random_direction!)
                    (iter (- n 1)))))
      (iter 16))
                                  
    ;updates the position of the enemy and field vector           
    (define (new_enemy_position!)
      ((initialized_field 'field_set_character_keyword!) (/ (x) speler_speed) (/ (y) speler_speed) 'empty)
      (set-xy! (+ (x) (* direction_x speler_speed))
               (+ (y) (* direction_y speler_speed))))

    ;status = 'on or 'off
    (define (switch_ghostly_goggle_status! keyword)
      (set! ghostly_goggle_status keyword))

    ;stopping the ghostly goggle action
    (define (stop_ghostly_goggle! teken-adt)
      (switch_appearance! teken-adt monster)
      (set! ghostly_goggle_time 0)
      (switch_ghostly_goggle_status! 'off))
    
    ;calculating where the enemy has to go to move to towards the player + actually moving the enemy
    (define (move_ghostly_goggle! speler-adt)
      (let ((speler_x (* (speler-adt 'x) speler_speed))
            (speler_y (* (speler-adt 'y) speler_speed))
            (x (x))
            (y (y)))
        (cond ((and (>= speler_x x) (>= speler_y y)) (set-xy! (+ x speler_speed)
                                                             (+ y speler_speed)))
              ((and (>= speler_x x) (<= speler_y y)) (set-xy! (+ x speler_speed)
                                                             (- y speler_speed)))
              ((and (<= speler_x x) (<= speler_y y)) (set-xy! (- x speler_speed)
                                                             (- y speler_speed)))
              ((and (<= speler_x x) (>= speler_y y)) (set-xy! (- x speler_speed)
                                                             (+ y speler_speed))))))
        
    ;checking if the enemy reached another tunnel yet or not + actually moving the enemy in ghostly goggle state
    (define (ghostly_goggle_movement! teken-adt speler-adt)
      (cond (((initialized_field 'field_empty?) (/ (x) speler_speed) (/ (y) speler_speed)) (stop_ghostly_goggle! teken-adt))
            (else (move_ghostly_goggle! speler-adt))))

    ;determining the movement of the enemy, triggering flame attacks if the monster is fygar, and stransforming into ghostly goggle state
    (define (beweeg! teken-adt speler-adt)
      (cond ((eq? ghostly_goggle_status 'on) (ghostly_goggle_movement! teken-adt speler-adt)
                                             (teken! teken-adt))
            ((> ghostly_goggle_time (+ random_goggle_time 10000)) ((initialized_field 'field_set_character_keyword!) (/ (x) speler_speed)
                                                                                                                     (/ (y) speler_speed)
                                                                                                                     'empty)
                                                                  (switch_appearance! teken-adt 'goggle)
                                                                  (move_ghostly_goggle! speler-adt)
                                                                  (switch_ghostly_goggle_status! 'on)
                                                                  (beweeg! teken-adt speler-adt)) 
            ((and (eq? monster 'fygar) (> attack-time (+ (random 5000) 2000))) (attack! teken-adt))
            ((eq? attack_status 'on) (set! attack_status 'off)
                                     ((initialized_field 'field_set_character_keyword!) (/ (+ (x) (* direction_x speler_speed))
                                                                                           speler_speed)
                                                                                        (/ (+ (y) (* direction_y speler_speed))
                                                                                           speler_speed)
                                                                                        'empty))
            ((check_direction) (new_enemy_position!)
                               ((initialized_field 'field_set_character_keyword!) (/ (x) speler_speed) (/ (y) speler_speed) 'enemy))))


    ;procedure to add enemies
    (define (add-enemies teken-adt)
        (let ((pos (initialized_field 'get_random_tunnel_pos)))
                 ((initialized_field 'field_set_character_keyword!) (((position) 'x) pos) (((position) 'y) pos) 'enemy)
                 ((teken-adt 'add-enemy!) (* (((position) 'x) pos) speler_speed) (* (((position) 'y) pos) speler_speed) (vector-ref vector_of_enemies (random amount_of_different_kind_of_enemies)))
                 (teken-adt 'teken-enemies!)))
    
    (define (dispatch msg)
      (cond ((eq? msg 'x) (x))
            ((eq? msg 'y) (y))
            ((eq? msg 'set-x!) set-x!)
            ((eq? msg 'set-y!) set-y!)
            ((eq? msg 'set-xy!) set-xy!)
            ((eq? msg 'lower_ID!) (lower_ID!))
            ((eq? msg 'add_attack_time!) add_attack_time!)
            ((eq? msg 'add_ghostly_goggle_time!) add_ghostly_goggle_time!)
            ((eq? msg 'enemy_ID) enemy_ID)
            ((eq? msg 'beweeg!) beweeg!)
            ((eq? msg 'teken!) teken!)
            ((eq? msg 'add-enemies) add-enemies)
            (else (error "not supported: " msg))))
    dispatch))