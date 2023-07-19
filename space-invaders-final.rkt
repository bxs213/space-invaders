;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)
(define BACKGROUND (place-image (bitmap "bkg.jpg") WIDTH (/ HEIGHT 2) (empty-scene WIDTH HEIGHT)))

(define TANK-SPEED 5)
(define MISSILE-SPEED 10)

(define HIT-RANGE 20)

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "green")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "green")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "white")       ;tread center
                       (ellipse 30 10 "solid" "white"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "white")       ;gun
                     (rectangle 20 10 "solid" "white"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-Y (- HEIGHT TANK-HEIGHT/2))

(define MISSILE (ellipse 5 15 "solid" "yellow"))


;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfinvaders is one of:
;;  - empty
;;  - (cons invaders ListOfinvaders)
;; interp. a list of invaders

(define LOI1 empty)
(define LOI2 (list I1 I2 I3))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons invader ListOfInvaders)
;; - reference: (first lod) is Invader
;; - self reference: (rest lod) is ListOfInvaders


;; ListOfmissile is one of:
;;  - empty
;;  - (cons missile ListOfmissile)
;; interp. a list of missiles

(define LOM1 empty)
(define LOM2 (cons (make-missile 10 20) (cons (make-missile 3 6) empty)))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons missile ListOfMissile)
;; - reference: (first lod) is Missile
;; - self reference: (rest lod) is ListOfMissile

;; Game constants/exmaples
(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; Functions:

;; game -> game
;; start the world with (main game)

(define (main game)
  (big-bang game                             ;game
    (on-tick   advance-all)                  ;game -> game
    (to-draw   render)                       ;game -> Image
    (stop-when invaded)                      ;game -> Boolean
    (on-key    operate-tank)))               ;game KeyEvent -> game

;; game -> game
;; produce the next state of the game. State of the game includes:
;; - Next missile(s) position
;; - Next Invader(s) position
;; - Create more invaders
;; - Bounce invaders of walls
;; - Check if hit has occured
;; - Remove missiles and invaders based on the above hits

(check-random (advance-all G0) (make-game (list (make-invader (random WIDTH) -10  (random 10))) empty (make-tank 155 1)))
(check-random (advance-all G1) (make-game (list (make-invader (random WIDTH) -10  (random 10))) empty (make-tank 55 1)))
(check-random (advance-all G3) (make-game (list (make-invader (random WIDTH) -10  (random 10))
                                                (make-invader 145 (- 510 5) -10))
                                          (list (make-missile 150 290))
                                          (make-tank 55 1)))
(check-random (advance-all G3) (make-game (list (make-invader (random WIDTH) -10  (random 10))
                                                (make-invader 145 (- 510 5) -10))
                                          (list (make-missile 150 290) )
                                          (make-tank 55 1)))

(check-random (advance-all (make-game (list (make-invader 300 300 4) (make-invader 50 50 10) (make-invader 150 150 -12))
                                      (list (make-missile 21 21) (make-missile 55 45) (make-missile 140 150))
                                      T1))
              (make-game (list (make-invader (random WIDTH) -10 (random 10))
                               (make-invader 302 302 -4))
                         (list (advance-missile (make-missile 21 21)))
                         (advance-tank T1)))
                                
;(define (advance-all game) game);stub

(define (advance-all game)
  (make-game (create-invaders (collision-counter (bounce-invaders (advance-invaders (game-invaders game))) (advance-missiles (game-missiles game))))
             (filter-lom (advance-missiles (game-missiles game)) (game-invaders game))
             (advance-tank (game-tank game))))

;; Tank -> Tank
;; Advance tank to its next position based on its x and dir coordinates.
(check-expect (advance-tank T0) (make-tank (+ TANK-SPEED (/ WIDTH 2)) 1))
(check-expect (advance-tank T1) (make-tank (+ 50 TANK-SPEED) 1))
(check-expect (advance-tank T2) (make-tank (- 50 TANK-SPEED) -1))             
              
;(define advance-tank t) t);stub

;; <template from tank data definition>:

(define (advance-tank t)
  (if (> (tank-dir t) 0)
      (make-tank (+ TANK-SPEED (tank-x t)) (tank-dir t))
      (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t))))


;; ListOfMissiles -> ListOfMissiles
;; Advance all missiles in the given ListOfMissiles

(check-expect (advance-missiles empty) empty)
(check-expect (advance-missiles (list M1 M2 (make-missile 100 100))) (list (make-missile 150 290)
                                                                           (make-missile 150 100)
                                                                           (make-missile 100 90)))
;(define (advance-missiles lom) lom);stub

;; <template from lom data definition>:
(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [(empty? (first lom)) empty]
        [else
         (cons (advance-missile (first lom))
               (advance-missiles (rest lom)))]))

;; Missile -> Missile
;; Advance the given missile's position

(check-expect (advance-missile M1) (make-missile 150 (- 300 10)))
(check-expect (advance-missile (make-missile 100 100)) (make-missile 100 (- 100 10)))
(check-expect (advance-missile (make-missile 100 0)) empty)

;(define (advance-missile m) m);stub

;; <template from missile data definition>:

(define (advance-missile m)
  (if (> (missile-y m) 0)
      (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED))
      empty
      ))

;; ListOfInvaders -> ListOfInvaders
;; Advance Invaders in given ListOfInvaders.

(check-expect (advance-invaders empty) empty)
(check-expect (advance-invaders (list I1 I2 I3 (make-invader 160 300 -10)))
              (list (make-invader 156 106 12)
                    (make-invader 145 505 -10)
                    (make-invader 155 515 10)
                    (make-invader 155 305 -10)))

;(define (advance-invaders loi) loi);stub

;; <template from loi data definition>:

(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (advance-invader (first loi))
               (advance-invaders (rest loi)))]))

;; Invader -> Invader
;; Advance Invader's position
(check-expect (advance-invader I1) (make-invader 156 106 12))
(check-expect (advance-invader (make-invader 150 100 -12)) (make-invader 144 106 -12))

;(define (advance-invader i) i);stub

;; <template from invader data definition>:

(define (advance-invader invader)
  (make-invader (+ (invader-x invader) (/ (invader-dx invader) 2))
                (+ (invader-y invader) (/ (abs (invader-dx invader)) 2))
                (invader-dx invader)))

;; LoI -> LoI
;; Create more invaders based on the current amount on screen. Created invader's x-coordinate is randomly generated number
;; under WIDTH. invader face the opposite direction from the last one.

(check-random (create-invaders empty) (list (make-invader (random WIDTH) -10 (random 10))))
(check-random (create-invaders (list I1 I2)) (list (make-invader (random WIDTH) -10 (* -1 (random 10))) I1 I2 ))
(check-random (create-invaders (list I1 I2 I3 I1 I2)) (list I1 I2 I3 I1 I2))


;(define (create-invaders LoI) LoI);stub

;; <template from LoI data definition>:

(define (create-invaders loi)
  (cond [(>= (length loi) 3) loi]
        [(empty? loi) (cons (make-invader (random WIDTH) -10 (random 10)) loi)]
        [else
         (if (< (invader-dx (first loi)) 0)
             (cons (make-invader (random WIDTH) -10 (random 10)) loi)
             (cons (make-invader (random WIDTH) -10 (* -1 (random 10))) loi))]))

;; LoI -> LoI
;; Check the x coordinates of the given invaders in loi. If x > WIDTH or x< 0, reverse direction.
(check-expect (bounce-invaders empty) empty)
(check-expect (bounce-invaders (list (make-invader 300 151 12) (make-invader 0 150 -12)))
              (list (make-invader 300 151 -12) (make-invader 0 150 12)))
              
(check-expect (bounce-invaders (list (make-invader 299 151 12) (make-invader 1 150 -12)))
              (list (make-invader 299 151 12) (make-invader 1 150 -12)))

;(define (bounce-invaders loi) loi);stub

(define (bounce-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (bounce-invader (first loi))
               (bounce-invaders (rest loi)))]))

;; invader -> invader
;; Change the direction of the invader if they have hit WIDTH going left (positive dx) or if they have hit
;; 0 going right (negative dx)
(check-expect (bounce-invader (make-invader 300 150 10)) (make-invader 300 150 -10))
(check-expect (bounce-invader (make-invader 300 150 -10)) (make-invader 300 150 -10))
(check-expect (bounce-invader (make-invader 0 150 -15)) (make-invader 0 150 15))
(check-expect (bounce-invader (make-invader 0 150 15)) (make-invader 0 150 15))

;(define (bounce-invader invader) invader);stub

(define (bounce-invader invader)
  (if (or (and (>= (invader-x invader) 300) (< 0 (invader-dx invader)))
          (and (<= (invader-x invader) 0) (> 0 (invader-dx invader))))
      (make-invader (invader-x invader) (invader-y invader) (* -1 (invader-dx invader)))
      invader))
         
;; loi lom -> loi
;; check if any of the give missiles have collided with an invader. If so, remove invader and missile.
(check-expect (collision-counter (game-invaders G1) (game-missiles G1)) (game-invaders G1))
(check-expect (collision-counter (game-invaders G2) (game-missiles G2)) (game-invaders G2))
(check-expect (collision-counter (list I1 (make-invader 100 100 -10)) (list M1 M2))
              (list (make-invader 100 100 -10)))

(check-expect (collision-counter (list (make-invader 300 300 -5) (make-invader 100 150 15) (make-invader 50 50 13))
                                 (list (make-missile 305 295) (make-missile 95 100) (make-missile 45 55)))
              (list (make-invader 100 150 15)))
             
;(define (collision-counter game) game);stub

(define (collision-counter loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else
         (if (collision-lom-i? lom (first loi))
             (collision-counter (rest loi) lom)
             (cons (first loi) (collision-counter (rest loi) lom)))]))
                   

;; LOM invader -> boolean
;; check if the given invader has collided with any of the missile in the provided lom. If so, return true.

(check-expect (collision-lom-i? empty I1) false)
(check-expect (collision-lom-i? (list M1 M2) empty) false)
(check-expect (collision-lom-i? (list M1 M2 M3) I1) true)
(check-expect (collision-lom-i? (list M1 M3) I2) false)
(check-expect (collision-lom-i? (list (make-missile 150 505)) I2) true)
;(define (collision-loi-m loi m) loi);stub

;;<template from loi data definition (modified)>:

(define (collision-lom-i? lom i)
  (cond [(empty? lom) false]
        [(empty? (first lom)) false]
        [(empty? i) false]
        [else
         (if (collided? (first lom) i)
             true
             (collision-lom-i? (rest lom) i)
             )]))

;; missile invader-> boolean
;; return true if the given invader and missile collide (HIT-RANGE). Otherwise, return false.

(check-expect (collided? M2 I1 ) true)
(check-expect (collided? M3 I1 ) true)
(check-expect (collided? M2 (make-invader 510 300 12)) false)
(check-expect (collided? M1 I1 ) false)
(check-expect (collided? M1 (make-invader 100 100 -10) ) false)

;(define (collided? i m) true);stub

;;<template from invader data definition>:
(define (collided? m i)
  (and (<= (abs(-(invader-x i) (missile-x m))) HIT-RANGE)
       (<= (abs(-(invader-y i) (missile-y m))) HIT-RANGE)))

;; lom loi -> lom
;; Filter missiles based on their proximity with invaders (HIT-RANGE). Return list of filtered missiles
(check-expect (filter-lom (game-missiles G1) (game-invaders G1)) (game-missiles G1))
(check-expect (filter-lom (game-missiles G2) (game-invaders G2)) (game-missiles G2))
(check-expect (filter-lom (list M1 M2) (list I1 (make-invader 100 100 -10)))
              (list M1))

(check-expect (filter-lom (list (make-missile 305 295) (make-missile 95 100) (make-missile 45 55))
                          (list (make-invader 300 300 -5) (make-invader 100 150 15) (make-invader 50 50 13)))
              (list (make-missile 95 100)))

;(define (filter-lom lom loi) lom);stub

;;<template from lom data definition>:
(define (filter-lom lom loi)
  (cond [(empty? lom) empty]
        [(empty? loi) lom]
        [else
         (if (collision-m-loi? loi (first lom))
             (filter-lom (rest lom) loi)
             (cons (first lom) (filter-lom (rest lom) loi)))]))

;; missile loi -> boolean
;; return true if missile collides with invader in loi. Else, return false

;(define (collision-m-loi? m loi) true); stub


(check-expect (collision-m-loi? empty M1) false)
(check-expect (collision-m-loi? (list I1 I2) empty) false)
(check-expect (collision-m-loi? (list I1 I2) M2) true)
(check-expect (collision-m-loi? (list (make-invader 150 100 12)) (make-missile 140 90)) true)
(check-expect (collision-m-loi? (list I1 (make-invader 100 100 -10)) M1)
              false)
(check-expect (collision-m-loi? (list I1 (make-invader 100 100 -10)) M2)
              true)

;;<template from loi data definition (modified)>:
(define (collision-m-loi? loi m) 
  (cond [(empty? loi) false]
        [(empty? m) false]
        [else
         (if (collided? m (first loi))
             true
             (collision-m-loi? (rest loi) m)
             )]))



;; game -> Image
;; render the given game onto BACKGROUND
(check-expect (render empty) BACKGROUND)
(check-expect (render G0) (place-image TANK (tank-x T0) TANK-Y BACKGROUND))
(check-expect (render G1) (place-image TANK (tank-x T1) TANK-Y BACKGROUND))
(check-expect (render G2) (place-image INVADER (invader-x I1) (invader-y I1)
                                       (place-image MISSILE (missile-x M1) (missile-y M1)
                                                    (place-image TANK (tank-x T1) TANK-Y BACKGROUND))))
(check-expect (render G3) (place-image INVADER (invader-x I1) (invader-y I1)
                                       (place-image INVADER (invader-x I2) (invader-y I2)
                                                    (place-image MISSILE (missile-x M1) (missile-y M1)
                                                                 (place-image MISSILE (missile-x M2) (missile-y M2)
                                                                              (place-image TANK (tank-x T1) TANK-Y BACKGROUND))))))
                                                                                                                     
;(define (render game) BACKGROUND) ;stub

;;<template from game data definition>:

(define (render g)
  (cond [(empty? g) BACKGROUND]
        [else
         (render-invaders (game-invaders g)
                          (render-missiles (game-missiles g)
                                           (render-tank (game-tank g))))]))

;; loi image -> image
;; Render the list of given invaders
(check-expect (render-invaders empty BACKGROUND) BACKGROUND)
(check-expect (render-invaders (list I1) BACKGROUND) (place-image INVADER (invader-x I1) (invader-y I1) BACKGROUND))
(check-expect (render-invaders (list I1 I2 I3) BACKGROUND) (place-image INVADER (invader-x I1) (invader-y I1)
                                                                        (place-image INVADER (invader-x I2) (invader-y I2)
                                                                                     (place-image INVADER (invader-x I3) (invader-y I3) BACKGROUND))))
;(define (render-invaders loi b) b);stub

;;<template from loi data definition>:

(define (render-invaders loi b)
  (cond [(empty? loi) b]
        [else
         (place-image INVADER (invader-x (first loi)) (invader-y (first loi))
                      (render-invaders (rest loi) b))]))

;; lom image -> image
;; Render the list of given missiles

(check-expect (render-missiles empty BACKGROUND) BACKGROUND)
(check-expect (render-missiles (list M1) BACKGROUND) (place-image MISSILE (missile-x M1) (missile-y M1) BACKGROUND))
(check-expect (render-missiles (list M1 M2) (render-tank T0))
              (place-image MISSILE (missile-x M1) (missile-y M1)
                           (place-image MISSILE (missile-x M2) (missile-y M2)
                                        (render-tank T0))))         
                                             
;(define (render-missiles lom b) BACKGROUND);stub

;;<template from lom data definition>:

(define (render-missiles lom b)
  (cond [(empty? lom) b]
        [(empty? (first lom)) b]
        [else
         (place-image MISSILE (missile-x (first lom)) (missile-y (first lom))
                      (render-missiles (rest lom) b))]))

;; tank -> image
;; render tank on BACKGROUND
(check-expect (render-tank T0) (place-image TANK (tank-x T0) TANK-Y BACKGROUND))
(check-expect (render-tank T1) (place-image TANK (tank-x T1) TANK-Y BACKGROUND))
(check-expect (render-tank T2) (place-image TANK (tank-x T2) TANK-Y BACKGROUND))

;(define (render-tank t) BACKGROUND);stub

;<template from tank data definition>:
(define (render-tank t)
  (place-image TANK (tank-x t) TANK-Y BACKGROUND))

;; game KeyEvent -> game
;; Perform tank operations (move and shoot)
(check-expect (operate-tank G0 "i") G0)
(check-expect (operate-tank G0 " ") (make-game empty (cons (make-missile (tank-x T0) HEIGHT) (game-missiles G0)) T0))
(check-expect (operate-tank G0 "left") (make-game empty empty (make-tank (tank-x T0) -1)))
(check-expect (operate-tank (make-game (list I1 I2) (list M1 M2) T2) "right")
              (make-game (list I1 I2) (list M1 M2) (make-tank (tank-x T2) 1)))

;(define (operate-tank game ke) game);stub

(define (operate-tank g ke)
  (cond [(key=? ke " ")
         (make-game (game-invaders g) (cons (make-missile (tank-x (game-tank g)) HEIGHT) (game-missiles g)) (game-tank g))]
        
        [(and (key=? ke "right") (> 0 (tank-dir (game-tank g))))
         (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) (* -1 (tank-dir (game-tank g)))))]
        
        [(and (key=? ke "left") (> (tank-dir (game-tank g)) 0))
         (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) (* -1 (tank-dir (game-tank g)))))]
        [else 
         g]))

;; game -> boolean
;; Return true if an invader has reached HEIGHT (laneded) in the given game. Else, return false

(check-expect (invaded G0) false)
(check-expect (invaded G1) false)
(check-expect (invaded G2) false)
(check-expect (invaded G3) true)

;(define (invaded game) false) ;stub

;<template from game data definition>:
(define (invaded g)
  (invaded-loi (game-invaders g)))

;; loi -> boolean
;; return true if an invader in the given loi has reached HEIGHT in the given loi. Else, return false

(check-expect (invaded-loi (list I1 I2 I3)) true)
(check-expect (invaded-loi empty) false)
(check-expect (invaded-loi (list (make-invader 0 0 10) (make-invader 100 100 -10))) false)

;(define (invaded-loi loi) false);stub
       
;;<template from loi data definition>:
(define (invaded-loi loi)
  (cond [(empty? loi) false]
        [else
         (if (invaded? (first loi))
             true
             (invaded-loi (rest loi)))]))
;; invader -> boolean
;; return true if the given invader has reached HEIGHT. Otherwise, return false

(check-expect (invaded? I1) false)
(check-expect (invaded? (make-invader 100 530 10)) true)

;(define (invaded? i) false);stub

;;<template from invader data definition>:
(define (invaded? i)
  (>= (invader-y i) HEIGHT))