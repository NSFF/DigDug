#lang r5rs

(#%require "Library/Graphics2.rkt")
(#%require "enemy_adt.rkt")
(#%require "rock_adt.rkt")
(#%require (only racket/base error))
(#%require (only racket/base random))


(#%provide maak-adt-teken)

;teken ADT: Handles all the information conserning visualising(player, rock, tunnel, enemy, menu, text) and adding/removing rocks/enemies from the game
(define (maak-adt-teken w h scale_w scale_h title air_space character_w/h speler_speed initialized_field speler bg_color_string)
  (let* ((venster (make-window (+ w speler_speed) (+ h 12) title));een bug fixed-> bug caused door Graphics2.rkt gemaakt door de assistenten
;;;;;;;;;;;;;;;;;; initialising tiles and layers ;;;;;;;;;;;;;;;;;;;;;;;;;
         (background-layer (venster 'make-layer))
         (background-color (make-tile w (+ h speler_speed)))
         (tunnel-layer (venster 'make-layer))
         (tunnel-tile (make-tile w (+ h character_w/h))) ; height is higher for debugging reasons
         (speler-layer (venster 'make-layer))
         (speler-tile (make-bitmap-tile "sprites/digdug.png" "sprites/digdug-mask.png"))
         (harpoen-tile (make-bitmap-tile "sprites/harpoen.png" "sprites/harpoen-mask.png"))
         (rock-layer (venster 'make-layer))
         (enemy-layer (venster 'make-layer))
         (enemy-fygar-attack-tile (make-bitmap-tile "sprites/vuur.png" "sprites/vuur-mask.png"))
         (enemy-attack-layer (venster 'make-layer))
         (text-layer (venster 'make-layer))
         (text-tile (make-tile w h))
         (score-tile (make-tile w h))
         (highscore-tile (make-tile w h))
         (menu-layer (venster 'make-layer))
         (menu-tile (make-tile w (+ h speler_speed)))
;;;;;;;;;;;;;;;;;; ending  initialising tiles and layers ;;;;;;;;;;;;;;;;;;;
         
         (enemy_number 0)
         (rock_number 0))
    ((menu-layer 'add-drawable) menu-tile)
    ((background-layer 'add-drawable) background-color)
    ((tunnel-layer 'add-drawable) tunnel-tile)

    ;this procedure is used to copy paste a background tile to the whole screen
    (define (add-tile-to-background x y)
      (let ((new-tile (make-bitmap-tile "sprites/ground.bmp")))
        ((new-tile 'set-x!) x)
        ((new-tile 'set-y!) y)
        ((background-layer 'add-drawable) new-tile)))

    ;you can choose between a brown rectangle backround or a bitmap background by changing the keyword to 'bitmap_background or so 'brown
    (define (initialize-background y w h keyword)
      (if (eq? keyword 'bitmap_background)
          (let ((y+h (+ y h)))
            (define (iter cntr-w cntr-h)
              (cond ((<= y+h cntr-h) (display 'background_initialization_done))
                    ((<= w cntr-w) (iter 0 (+ cntr-h character_w/h)))
                    (else (add-tile-to-background cntr-w cntr-h)                    
                          (iter (+ cntr-w character_w/h) cntr-h))))
            (iter 0 y))
          ((background-color 'draw-rectangle) 0 y w (+ h 3) "brown")))
   
    (define enemies (list 'dummy))
    (define rocks (list 'dummy))
    
    (define (teken-speler! speler-adt)
        ((speler-tile 'set-x!) (* (speler-adt 'x) speler_speed))
        ((speler-tile 'set-y!) (* (speler-adt 'y) speler_speed)))

    ;adds an enemy and a bitmap-tile to a list
    (define (add-enemy! x y bitmap keyword)
      (let ((new-tile bitmap))
        ((new-tile 'set-x!) x)
        ((new-tile 'set-y!) y)
        (set! enemies (append enemies (list (vector (enemy x y speler_speed initialized_field keyword enemy_number) new-tile))))
        (set! enemy_number (+ enemy_number 1))
        ((enemy-layer 'add-drawable) new-tile)))

    ;adds a rock and a bitmap-tile to a list
    (define (add-rock! x y)
      (let ((new-tile (make-bitmap-tile "sprites/rots.png" "sprites/rots-mask.png")))
        ((new-tile 'set-x!) x)
        ((new-tile 'set-y!) y)
        (set! rocks (append rocks (list (vector (rock x y speler_speed initialized_field rock_number) new-tile))))
        (set! rock_number (+ rock_number 1))
        ((rock-layer 'add-drawable) new-tile)))

    ;changes the enemy appearance to one of the ghostly goggles and back if the monster finds a tunnel
    (define (change_enemy_appearance! x y keyword)
      (define (iter lst)
        (let ((car-lst (car lst)))
        (cond ((and (eq? x ((vector-ref car-lst 0) 'x))
                    (eq? y ((vector-ref car-lst 0) 'y)))
               (let ((bitmap_pooka (make-bitmap-tile "sprites/monster1.png" "sprites/monster1-mask.png"))
                     (bitmap_fygar (make-bitmap-tile "sprites/monster2.png" "sprites/monster2-mask.png"))
                     (bitmap_goggle (make-bitmap-tile "sprites/monster3.png" "sprites/monster3-mask.png")))
                 (cond ((eq? keyword 'fygar)((enemy-layer 'remove-drawable) (vector-ref car-lst 1))
                                            (vector-set! car-lst 1 bitmap_fygar)
                                            ((enemy-layer 'add-drawable) bitmap_fygar))
                       ((eq? keyword 'pooka)((enemy-layer 'remove-drawable) (vector-ref car-lst 1))
                                            (vector-set! car-lst 1 bitmap_pooka)
                                            ((enemy-layer 'add-drawable) bitmap_pooka))
                       ((eq? keyword 'goggle)((enemy-layer 'remove-drawable) (vector-ref car-lst 1))
                                             (vector-set! car-lst 1 bitmap_goggle)
                                             ((enemy-layer 'add-drawable) bitmap_goggle)))))
              (else (iter (cdr lst))))))
      (iter (cdr enemies)))

    ;gives the enemy_ID of the an enemy so we know which one we have to delete
    (define (enemy_hit_by_harpoen_ID x y field-adt)
      (define (iter lst)
        (let ((car-lst (car lst)))
        (cond ((and (eq? x ((vector-ref car-lst 0) 'x))
                    (eq? y ((vector-ref car-lst 0) 'y)))
               ((field-adt 'field_set!) (/ x speler_speed) (/ y speler_speed) 'empty)
               ((vector-ref car-lst 0) 'enemy_ID))
              (else (iter (cdr lst))))))
      (iter (cdr enemies)))
    
    (define (lower_enemy_number)
      (set! enemy_number (- enemy_number 1)))
    
    (define (lower_rock_number)
      (set! rock_number (- rock_number 1)))

    ;removes an enemy from the list
    (define (remove-enemy! n);list start from position 0
      (define (lower_enemy_ID_numbers lst);lowers the ID's of each enemy ADT left in the list, so we can delete it correctly if hit
        (cond ((null? lst))
              (else ((vector-ref (car lst) 0) 'lower_ID!)
                    (lower_enemy_ID_numbers (cdr lst)))))
      
      (define (iter lst n)
        (cond ((null? (cdr lst)))
              ((> n 0) (iter (cdr lst) (- n 1)))
              (else ((enemy-layer 'remove-drawable) (vector-ref (cadr lst) 1))
                    (set-cdr! lst (cddr lst))
                    (lower_enemy_number)
                    (lower_enemy_ID_numbers (cdr lst)))))

      (iter enemies n))
    
    ;removes a rock from the list
    (define (remove-rock! n)
      (define (lower_rock_ID_numbers lst)
        (cond ((null? lst))
              (else ((vector-ref (car lst) 0) 'lower_ID!)
                    (lower_rock_ID_numbers (cdr lst)))))
      
      (define (iter lst n)
        (cond ((null? (cdr lst)))
              ((> n 0) (iter (cdr lst) (- n 1)))
              (else ((rock-layer 'remove-drawable) (vector-ref (cadr lst) 1))
                    (set-cdr! lst (cddr lst))
                    (lower_rock_number)
                    (lower_rock_ID_numbers (cdr lst)))))

      (iter rocks n))

    ;updates the positions of all the enemies in the list and also erases the fygar flame if there is one
    (define (update_enemy_positions! time speler-adt)
      (define (iter lst)
        (cond ((null? lst))
              ((eq? 'dummy (car lst)) (iter (cdr lst)))
              (else (let ((lst (car lst)))
                      (((vector-ref lst 0) 'add_ghostly_goggle_time!) time)
                      (((vector-ref lst 0) 'add_attack_time!) time)
                      (((vector-ref lst 0) 'beweeg!) dispatch speler-adt)
                      (((vector-ref lst 1) 'set-x!) ((vector-ref lst 0) 'x))
                      (((vector-ref lst 1) 'set-y!) ((vector-ref lst 0) 'y)))
                    (iter (cdr lst)))))
      
      (if (>= time 350)
          (begin ((enemy-attack-layer 'remove-drawable) enemy-fygar-attack-tile)
                 (iter enemies)
                 #t)
          #f))
    ;updates all the positions of the rocks if they were falling down
    (define (update_rock_positions! speler-adt)
      (define (iter lst)
        (cond ((null? lst))
              ((eq? 'dummy (car lst)) (iter (cdr lst)))
              (else (let ((lst (car lst)))
                      (((vector-ref lst 0) 'beweeg!) dispatch speler-adt)
                      (((vector-ref lst 1) 'set-x!) ((vector-ref lst 0) 'x))
                      (((vector-ref lst 1) 'set-y!) ((vector-ref lst 0) 'y)))
                    (iter (cdr lst)))))
      (iter rocks))

    (define (teken-fygar-aanval! x y)
      ((enemy-attack-layer 'add-drawable) enemy-fygar-attack-tile)
      ((enemy-fygar-attack-tile 'set-x!) x)
      ((enemy-fygar-attack-tile 'set-y!) y))
      
    ;adds a random enemy between pooka or fygar to the list
    (define (add-enemy-random! x y keyword)
      (cond ((equal? keyword 'pooka)
             (add-enemy! x y (make-bitmap-tile "sprites/monster1.png" "sprites/monster1-mask.png") keyword))
            (else (add-enemy! x y (make-bitmap-tile "sprites/monster2.png" "sprites/monster2-mask.png") keyword))))

    ;updates all the visual positions of the enemies
    (define (teken-enemies!)
      (define (iter lst)
        (cond ((null? lst))
              ((eq? 'dummy (car lst)) (iter (cdr lst)))
              (else  (let ((lst (car lst)))
                       (((vector-ref lst 1) 'set-x!) ((vector-ref lst 0) 'x))
                       (((vector-ref lst 1) 'set-y!) ((vector-ref lst 0) 'y)))
                       (iter (cdr lst)))))
      (iter enemies))
    
    ;updates all the visual positions of the rocks
    (define (teken-rocks!)
      (define (iter lst)
        (cond ((null? lst))
              ((eq? 'dummy (car lst)) (iter (cdr lst)))
              (else  (let ((lst (car lst)))
                       (((vector-ref lst 1) 'set-x!) ((vector-ref lst 0) 'x))
                       (((vector-ref lst 1) 'set-y!) ((vector-ref lst 0) 'y)))
                       (iter (cdr lst)))))
      (iter rocks))
    
    (define (teken-tunnel! x y)
      ((tunnel-tile 'draw-rectangle) x y (* character_w/h scale_w) (* character_w/h scale_h) bg_color_string))
    
    (define (reinitialize-tunnel!)
      (tunnel-tile 'clear))

    ;updates the life of the player
    (define (visual_update_life! speler-adt)
      (text-tile 'clear)
      ((text-tile 'draw-text) (string-append "life: " (number->string (speler-adt 'life))) (* 16 scale_h) 0 0 "red"))

    (define (visual_update_score! score)
      (score-tile 'clear)
      ((score-tile 'draw-text) (string-append "score: " (number->string score)) (* 16 scale_h) (/ w 4) 0 "red"))
    
    (define (visual_update_highscore! highscore)
      (highscore-tile 'clear)
      ((highscore-tile 'draw-text) (string-append "highscore: " (number->string highscore)) (* 16 scale_h) (* w 0.6) 0 "red"))

    ;clears the game of anything visual left onto the screen
    (define (clear_game!)
      (define (iter_enemies lst)
        (cond ((null? lst))
              ((eq? 'dummy (car lst)) (iter_enemies (cdr lst)))
              (else  (remove-enemy! 0)
                    (iter_enemies (cdr lst)))))
      
      (iter_enemies enemies)
      
      (define (iter_rocks lst)
        (cond ((null? lst))
              ((eq? 'dummy (car lst)) (iter_rocks (cdr lst)))
              (else  (remove-rock! 0)
                    (iter_rocks (cdr lst)))))
      
      (iter_rocks rocks)
      ((enemy-attack-layer 'remove-drawable) enemy-fygar-attack-tile)
      ((text-layer 'remove-drawable) highscore-tile)
      ((text-layer 'remove-drawable) score-tile)
      ((text-layer 'remove-drawable) text-tile)
      ((speler-layer 'remove-drawable) speler-tile)
      (remove-harpoen)
      (background-color 'clear))
    
    ;gives a background color to the menu
    (define (initialize_menu!)
      ((text-tile 'draw-rectangle) 0 0 w (+ h 3) bg_color_string))

    ;draws the menu, this could be parameterised, with a list of options example: (start . exit)
    (define (teken-menu! menu_selection_number menu_status)
      (menu-tile 'clear)
      (cond ((eq? menu_status 'off))
            ((eq? menu_selection_number 0)
             ((menu-tile 'draw-text) "Start" (* 16 scale_h) (round (- (/ w 2) 20)) (round (- (/ h 2) 20)) "yellow")
             ((menu-tile 'draw-text) "Exit" (* 16 scale_h) (round (- (/ w 2) 20)) (+ (round (- (/ h 2) 20)) (* 30 scale_h)) "red"))
            ((eq? menu_selection_number 1)
             ((menu-tile 'draw-text) "Start" (* 16 scale_h) (round (- (/ w 2) 20)) (round (- (/ h 2) 20)) "red")
             ((menu-tile 'draw-text) "Exit" (* 16 scale_h) (round (- (/ w 2) 20)) (+ (round (- (/ h 2) 20)) (* 30 scale_h)) "yellow"))))


    (define (add-harpoen)
      ((speler-layer 'add-drawable) harpoen-tile))
    (define (remove-harpoen)
      ((speler-layer 'remove-drawable) harpoen-tile))
    
    (define (teken-harpoen! x y)
      ((harpoen-tile 'set-x!) (* x speler_speed))
      ((harpoen-tile 'set-y!) (* y speler_speed)))
      
    (define (set-spel-lus-functie! fun)
      ((venster 'set-update-callback!) fun))
    
    (define (set-toets-functie! fun)
      ((venster 'set-key-callback!) fun))
    
    
    (define (dispatch msg)
      (cond ((eq? msg 'teken-speler!) teken-speler!)
            ((eq? msg 'teken-tunnel!) teken-tunnel!)
            ((eq? msg 'reinitialize-tunnel!) (reinitialize-tunnel!))
            ((eq? msg 'teken-enemies!) (teken-enemies!))
            ((eq? msg 'teken-rocks!) (teken-rocks!))
            ((eq? msg 'change_enemy_appearance!) change_enemy_appearance!)
            ((eq? msg 'teken-menu!) teken-menu!)
            ((eq? msg 'add-enemy!) add-enemy-random!)
            ((eq? msg 'add-rock!) add-rock!)
            ((eq? msg 'teken-fygar-aanval!) teken-fygar-aanval!)
            ((eq? msg 'initialize_text_tile) ((text-layer 'add-drawable) text-tile))
            ((eq? msg 'initialize_score_tile) ((text-layer 'add-drawable) score-tile))
            ((eq? msg 'initialize_highscore_tile) ((text-layer 'add-drawable) highscore-tile))
            ((eq? msg 'initialize_speler_tile) ((speler-layer 'add-drawable) speler-tile))
            ((eq? msg 'enemy_hit_by_harpoen_ID) enemy_hit_by_harpoen_ID)
            ((eq? msg 'add-harpoen) (add-harpoen))
            ((eq? msg 'remove-harpoen) (remove-harpoen))
            ((eq? msg 'teken-harpoen!) teken-harpoen!)
            ((eq? msg 'initialize_background) (initialize-background (* air_space speler_speed) w (- h (* air_space speler_speed)) 'brown))
            ((eq? msg 'visual_update_life!) visual_update_life!)
            ((eq? msg 'visual_update_score!) visual_update_score!)
            ((eq? msg 'visual_update_highscore!) visual_update_highscore!)
            ((eq? msg 'initialize_menu!) (initialize_menu!))
            ((eq? msg 'remove-enemy!) remove-enemy!)
            ((eq? msg 'clear_game!) (clear_game!))
            ((eq? msg 'update_enemy_positions!) update_enemy_positions!)
            ((eq? msg 'update_rock_positions!) update_rock_positions!)
            ((eq? msg 'set-spel-lus-functie!) set-spel-lus-functie!)
            ((eq? msg 'set-toets-functie!) set-toets-functie!)
            (else (error "not supported: " msg))))
    dispatch))