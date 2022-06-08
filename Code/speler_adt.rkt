#lang r5rs

(#%require "position_adt.rkt")
(#%require "enemy_adt.rkt")
(#%require (only racket/base error))
(#%require (only racket/base random))
(#%require (only racket/base exit))

(#%provide speler)

;speler ADT: holds the information about the player and his locations, movement, attack, score, highscore and life
(define (speler x y w h character_w/h gameboard_resolution speler_speed starting_life air_space)
  (let((positie-speler (((position) 'new) x y))
       (life starting_life)
       (score 0)
       (highscore 0)
       (direction 'left)
       (harpoen_position (((position) 'new) x y))
       (harpoen_rock 'off)
       (attack_status 'off)
       (attack_time 0)
       (attack_speed 100);speed at which the harpoon flies
       )

  (define (random_y max)
      (let ((y (random max)))
        (cond ((<= y air_space) (+ y air_space 1))
              (else y))))
    
  ;reinitializing all the players information(location, score, life) and updating the highscore
  (define (initialize_player! teken-adt)
    (if (> score  highscore)
        (set! highscore score))
    (set! score 0)
    (teken-score! teken-adt)
    (set! life starting_life) 
    (set-xy! (random gameboard_resolution)
             (random_y gameboard_resolution)))
    
  (define (teken! teken-adt)
    ((teken-adt 'teken-tunnel!) (* (dispatch 'x) speler_speed) (* (dispatch 'y) speler_speed))
    ((teken-adt 'teken-speler!) dispatch))

  (define (set-x! x)
    (((position) 'set-x!) positie-speler x))
  (define (set-y! y)
    (((position) 'set-y!) positie-speler y))

  (define (set-xy! x y)
    (set-x! x)
    (set-y! y))
  
  (define (x)
    (((position) 'x) positie-speler))
  (define (y)
    (((position) 'y) positie-speler))

    ;to change the direction of the harpoon attack
  (define (set-direction! key)
    (if (eq? attack_status 'off)
        (set! direction key)))
    
    ;a procedure that can handle updating positions of the player and the harpoon, also checks if it hits a rock or not
  (define (keyboard_updating_positions set-func! key x y field-adt)
    (let ((new_x x)
          (new_y y))
        (cond ((and (eq? key 'right))
               (if (< (+ x 1) gameboard_resolution)
                   (set! new_x (+ x 1))))
              ((eq? key 'left)
               (if (>= (- x 1) 0)
                   (set! new_x (- x 1))))
              ((eq? key 'up)
               (if (>= (- y 1) 0)
                   (set! new_y (- y 1))))
              ((eq? key 'down)
               (if (< (+ y 1) gameboard_resolution)
                   (set! new_y (+ y 1))))
              (else (error "not supported key: " key)))
      
    (if (not (eq? ((field-adt 'field_get) new_x new_y) 'rock))
        (begin (set-func! new_x new_y)
               (set! harpoen_rock 'off));in case we hit a rock with a harpoon it won't loop infinitly
        (set! harpoen_rock 'on))))

    ;updates the direction and movement of the player
  (define (beweeg! key field-adt)
    (set-direction! key)
    (keyboard_updating_positions set-xy! key (x) (y) field-adt))

  (define (enemy_hit? field-adt)
   ((field-adt 'field_enemy?) (x) (y)))

    ;checks if the player is hit by an enemy or not
  (define (check_enemy_hit field-adt)
    (if (enemy_hit? field-adt)
        (reduce_life!)))
    
  (define (reduce_life!)
    (set! life (- life 1))
    (if (<= life 0)
        'dead
        'alive))
    
  (define (add_score!)
    (let ((y (harpoen-y)))
      (set! score (+ score (+ 10 (* 5 y))))))

  (define (teken-score! teken-adt)
    ((teken-adt 'visual_update_score!) score))
    
  (define (teken-highscore! teken-adt)
    ((teken-adt 'visual_update_highscore!) highscore))

  (define (set-harpoen_position! x y)
    (((position) 'set-x!) harpoen_position x)
    (((position) 'set-y!) harpoen_position y))

  (define (harpoen-x)
    (((position) 'x) harpoen_position))
  (define (harpoen-y)
    (((position) 'y) harpoen_position))
    
  (define (switch_attack_status! key)
    (set! attack_status key))

    ;triggers the attack of the harpoon
  (define (launch_attack teken-adt)
    (set-harpoen_position! (x) (y))
    (teken-adt 'add-harpoen)
    ((teken-adt 'teken-harpoen!) (harpoen-x)
                                 (harpoen-y))
    (switch_attack_status! 'on)
    (set! attack_time attack_speed))
    
    ;handles the attack of the harpoon and removes the enemy if hit
  (define (update_attack! time teken-adt field-adt);known bug, if you shoot at the border of the game_board you harpoon will get stuck
    (set! attack_time (+ attack_time time))
    (cond ((eq? attack_status 'off))
          ((>= attack_time attack_speed)(set! attack_time 0)
                                        (keyboard_updating_positions set-harpoen_position! direction (harpoen-x) (harpoen-y) field-adt);setting the next position of the harpoon
                                        ((teken-adt 'teken-harpoen!) (harpoen-x)
                                                                     (harpoen-y))
                                        (update_attack! 0 teken-adt field-adt);this makes sure it rechecks the position of the harpoon in case it hits
                                        )
          (((field-adt 'field_enemy?) (harpoen-x) (harpoen-y))
           ((teken-adt 'remove-enemy!) ((teken-adt 'enemy_hit_by_harpoen_ID) (* (harpoen-x) speler_speed)
                                                                             (* (harpoen-y) speler_speed)
                                                                             field-adt))
           (add_score!)
           (teken-score! teken-adt)
           (stop_attack teken-adt)
           (((enemy 0 0 speler_speed field-adt 'none 0) 'add-enemies) teken-adt))
          ((or (not ((field-adt 'field_empty?) (harpoen-x) (harpoen-y))) (eq? harpoen_rock 'on))
           (set! harpoen_rock 'off)
           (stop_attack teken-adt))))
    
    ;stops the harpoon attack
  (define (stop_attack teken-adt)
    (teken-adt 'remove-harpoen)
    (switch_attack_status! 'off))
    
  (define (dispatch msg)
    (cond ((eq? msg 'set-x!) set-x!)
          ((eq? msg 'set-y!) set-y!)
          ((eq? msg 'set-xy!) set-xy!)
          ((eq? msg 'x)(x))
          ((eq? msg 'y)(y))
          ((eq? msg 'life) life)
          ((eq? msg 'check_enemy_hit) check_enemy_hit)
          ((eq? msg 'reduce_life!) (reduce_life!))
          ((eq? msg 'pos) positie-speler)
          ((eq? msg 'beweeg!) beweeg!)
          ((eq? msg 'teken!) teken!)
          ((eq? msg 'teken-highscore!) teken-highscore!)
          ((eq? msg 'launch_attack) launch_attack)
          ((eq? msg 'update_attack!) update_attack!)
          ((eq? msg 'initialize_player!) initialize_player!)
          ((eq? msg 'attack_status) attack_status)
          (else (error "not supported: " msg))))
  dispatch))