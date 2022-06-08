(#%require "position_adt.rkt")
(#%require "field_adt.rkt")
(#%require "speler_adt.rkt")
(#%require "enemy_adt.rkt")
(#%require "teken_adt.rkt")
(#%require "Library/Graphics2.rkt")
(#%require "menu_adt.rkt")
(#%require "rock_adt.rkt")


(#%require (only racket/base random))

;Spel ADT: used to control the game/menu-loop and start the game up 
(define (spel w
              h
              standard_h
              standard_w
              air_space
              gameboard_resolution
              character_w/h
              life
              amount_of_tunnels
              min_tunnel_length
              max_tunnel_length
              amount_of_enemies
              amount_of_rocks)
  
  (let* ((speler_speed (floor (/ w gameboard_resolution)));how many pixels the each character can move at one time
         (w (- w (modulo w gameboard_resolution)))
         (h (- h (modulo h gameboard_resolution)))
         (scale_h (/ h standard_h));scale variables used to scale the "tunnels" to the window resolution. bigger window -> bigger tunnels
         (scale_w (/ w standard_w)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pre-initialisation of the game ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define initialized_field (field w h character_w/h gameboard_resolution speler_speed air_space))
    ((initialized_field 'field_set_rect_keyword!) 0 0 (- gameboard_resolution 1) air_space 'ground)
    
    (define speler-adt (speler 0 0 w h character_w/h gameboard_resolution speler_speed life air_space))
    (define teken-adt (maak-adt-teken w h scale_w scale_h "Dig_Dug " air_space character_w/h speler_speed initialized_field speler-adt "black"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end of pre-initialisation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;procedure that can be used to reset all the aspects of the game: player position, enemies, rocks positions, score...
    ;Beware: highscore won't be reset
    (define (game_initialisation)

      ;spawn_enemies and spawn_rocks can spawn a number of enemies/rocks at the beginning of the game
      (define (spawn_enemies amount_of_enemies)
        (if (not (= amount_of_enemies 0))
            (begin (((enemy 0 0 speler_speed initialized_field 'none 0) 'add-enemies) teken-adt)
                   (spawn_enemies (- amount_of_enemies 1)))))
      (define (spawn_rocks amount_of_rocks)
        (if (not (= amount_of_rocks 0))
            (begin (((rock 0 0 speler_speed initialized_field 0) 'add-rock) teken-adt)
                   (spawn_rocks (- amount_of_rocks 1)))))

      ;initialisation of the game
      (initialized_field 'reinitialize-field!);remaking the field vector
      ((speler-adt 'initialize_player!) teken-adt);repositioning the player and resetting life
            
      (teken-adt 'initialize_background);drawing the background
      (teken-adt 'reinitialize-tunnel!);drawing the tunnels
      ((initialized_field 'initialize_tunnel!) amount_of_tunnels air_space teken-adt min_tunnel_length max_tunnel_length);initializing the tunnels onto the field vector

      (teken-adt 'initialize_text_tile);drawing the text
      (teken-adt 'initialize_score_tile);drawing the score
      (teken-adt 'initialize_highscore_tile);drawing the highscore
      
      (teken-adt 'initialize_speler_tile);drawing the player
      ((speler-adt 'teken!) teken-adt);reposition the drawn player
      ((speler-adt 'teken-highscore!) teken-adt);updating the highscore value
      
      (spawn_enemies amount_of_enemies);adding enemies to the list
      (teken-adt 'teken-enemies!);drawing the enemies listed

      (spawn_rocks amount_of_rocks);adding rocks to the list
      (teken-adt 'teken-rocks!));drawing the rocks listed

    ;procedure who handles the key-input of the player during the game. It issues attack commands and movement of the player
    ;beware: the key-inputs of the menu are handled in the menu-adt
    (define (game-keyboard-input key)
      
      (cond((eq? key 'right);if you wanne use letters:example(eq? key #\a) 
            ((speler-adt 'beweeg!) 'right initialized_field))
           ((eq? key 'left)
            ((speler-adt 'beweeg!) 'left initialized_field))
           ((eq? key 'up)
            ((speler-adt 'beweeg!) 'up initialized_field))
           ((eq? key 'down)
            ((speler-adt 'beweeg!) 'down initialized_field))
           ((and (eq? key #\space) (eq? 'off (speler-adt 'attack_status)));space= attack of the player
            ((speler-adt 'launch_attack) teken-adt)))

      ;this checks if during movement of the player het got hit by an enemy or not
      (if (eq? ((speler-adt 'check_enemy_hit) initialized_field)
                            'dead)
                       (begin (teken-adt 'clear_game!)
                              (start!)))
      
      ;visual updates of life and player + applying tunnels to the field vector
      ((teken-adt 'visual_update_life!) speler-adt)
      ((initialized_field 'field_set_character_keyword!)(speler-adt 'x)(speler-adt 'y) 'empty)
      ((speler-adt 'teken!) teken-adt))

    ;this procedure is the main game-loop
    (define (spel-lus-functie)
      
      (define (game_start! delta-time)
        
        (set! speler-time (+ speler-time delta-time))
        (set! rock-falling-speed-time (+ rock-falling-speed-time delta-time))

        ;handles the position updating of the rocks
        (cond ((>= rock-falling-speed-time 200)
               ((teken-adt 'update_rock_positions!) speler-adt)
               (set! rock-falling-speed-time 0)))

        ;handles the position updating of the enemies + if life=0 triggers the menu-loop
        (if ((teken-adt 'update_enemy_positions!) speler-time speler-adt)
            (begin (if (eq? ((speler-adt 'check_enemy_hit) initialized_field)
                            'dead)
                       (begin (teken-adt 'clear_game!)
                              (start!)))
                   
                   ((teken-adt 'visual_update_life!) speler-adt)
                   (set! speler-time 0)))
        
        ;updates the flying harpoon position
        ((speler-adt 'update_attack!) delta-time teken-adt initialized_field)

        )

      ;initialising the game-loop/keyboard and starting the game
      (game_initialisation)
      ((teken-adt 'set-toets-functie!) game-keyboard-input)
      ((teken-adt 'set-spel-lus-functie!) game_start!))
    
    (define speler-time 0)
    (define rock-falling-speed-time 0)

    ;procedure that starts the game or goes back to the menu
    (define (start!)

      ;initialisation of the menu
      (define menu-adt (menu spel-lus-functie teken-adt))
      (menu-adt 'initialize_menu!)

      ;menu-loop ->can trigger the game-loop
      (define (menu_game_loop delta-time)
        ((teken-adt 'set-toets-functie!) (menu-adt 'game-keyboard-input)))
      
      ((teken-adt 'set-spel-lus-functie!) menu_game_loop))
    
    (define (dispatch msg)
      (cond ((eq? msg 'start!) (start!))
            (else (error "not supported: " msg))))
    dispatch))

(define game (spel 500;height
                   500;width
                   480;standard_height
                   480;standard_width
                   3;air_space
                   30;gameboard_resolution
                   ((make-bitmap-tile "sprites/digdug.png") 'get-w);character_w/h
                   3;life
                   3;amount_of_tunnels
                   4;min_tunnel_length
                   10;max_tunnel_length
                   2;amount_of_enemies
                   3;amount_of_rocks
                   ))
(game 'start!)
