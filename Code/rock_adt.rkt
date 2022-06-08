#lang r5rs

(#%require "position_adt.rkt")
(#%require "field_adt.rkt")
(#%require (only racket/base error))
(#%require (only racket/base random))

(#%provide rock)

;rock ADT: saves the locations of the rocks and how they will move once dug under
(define (rock x y speler_speed initialized_field rock_number)
  (let* ((rock_position (((position) 'new) x y))
        (rock_ID rock_number)
        (ready_to_fall 'off))

    (define (lower_ID!)
      (set! rock_ID (- rock_ID 1)))
    
    (define (x)
      (((position) 'x) rock_position))
    (define (y)
      (((position) 'y) rock_position))
    
    (define (set-x! x)
      (((position) 'set-x!) rock_position x))
    (define (set-y! y)
      (((position) 'set-y!) rock_position y))
    
    (define (set-xy! x y)
      (set-x! x)
      (set-y! y))
    
    (define (teken! teken-adt)
      (teken-adt 'teken-rocks!))
    
    ;checks if there is no collision under the rock
    (define (no_collision?)
      ((initialized_field 'field_empty?) (/ (x) speler_speed)
                                         (/ (+ (y)  speler_speed)
                                            speler_speed)))
    ;checks if an enemy location is the same as the rocks location
    ;this is still bugged and does not work!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    (define (enemy_hit! teken-adt)
      (cond (((initialized_field 'field_enemy?) (x) (y))
             ((teken-adt 'remove-enemy!) ((teken-adt 'enemy_hit_by_harpoen_ID) (x);enemy_hit_by_harpoen gives the ID of the hit enemy
                                                                               (y);it doesn't matter if hit by a rock or a harpoon
                                                                               initialized_field)))))

    ;the position of the rock is being calculated and changed, the field vector is also updated        
    (define (new_rock_position! teken-adt)
      ((initialized_field 'field_set_character_keyword!) (/ (x) speler_speed) (/ (y) speler_speed) 'empty)
      (set-y! (+ (y) speler_speed))
      (enemy_hit! teken-adt)
      ((initialized_field 'field_set_character_keyword!) (/ (x) speler_speed) (/ (y) speler_speed) 'rock))

    (define (switch_ready_to_fall! keyword)
      (set! ready_to_fall keyword))
   
    ;moves the rock if a player created a tunnel under it and moved away from it 
    (define (beweeg! teken-adt speler-adt)
      (cond ((and (= (- (speler-adt 'y) 1) (/ (y) speler_speed));if player is under it, triggers the falling process
                  (= (speler-adt 'x) (/ (x) speler_speed))
                  (eq? ready_to_fall 'off))
             ((teken-adt 'teken-tunnel!) (x) (y))
             (switch_ready_to_fall! 'on))
            ((and (= (- (speler-adt 'y) 1) (/ (y) speler_speed));if player is still under it and the rock is ready to fall
                  (= (speler-adt 'x) (/ (x) speler_speed))))
            ((and (eq? ready_to_fall 'on) (no_collision?));the rock actually falling due to the player going away
             (new_rock_position! teken-adt))
            ((not (no_collision?)) (switch_ready_to_fall! 'off))));the rock hit the ground again and stops falling

    ;adds a rock to the game
    (define (add-rock teken-adt)
        (let ((pos (initialized_field 'get_random_rock_pos)))
                 ((initialized_field 'field_set_character_keyword!) (((position) 'x) pos) (((position) 'y) pos) 'rock)
                 ((teken-adt 'add-rock!) (* (((position) 'x) pos) speler_speed) (* (((position) 'y) pos) speler_speed))
                 (teken-adt 'teken-rocks!)))
    
    (define (dispatch msg)
      (cond ((eq? msg 'x) (x))
            ((eq? msg 'y) (y))
            ((eq? msg 'set-x!) set-x!)
            ((eq? msg 'set-y!) set-y!)
            ((eq? msg 'set-xy!) set-xy!)
            ((eq? msg 'lower_ID!) (lower_ID!))
            ((eq? msg 'rock_ID) rock_ID)
            ((eq? msg 'beweeg!) beweeg!)
            ((eq? msg 'teken!) teken!)
            ((eq? msg 'add-rock) add-rock)
            (else (error "not supported: " msg))))
    dispatch))