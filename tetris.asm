; SIZE 12 x 8


;; game state memory location
.equ T_X, 0x1000 ; falling tetromino position on x
.equ T_Y, 0x1004 ; falling tetromino position on y
.equ T_type, 0x1008 ; falling tetromino type
.equ T_orientation, 0x100C ; falling tetromino orientation
.equ SCORE, 0x1010 ; score
.equ GSA, 0x1014 ; Game State Array starting address
.equ SEVEN_SEGS, 0x1198 ; 7-segment display addresses
.equ LEDS, 0x2000 ; LED address
.equ RANDOM_NUM, 0x2010 ; Random number generator address
.equ BUTTONS, 0x2030 ; Buttons addresses

;; tetromino type enumeration
.equ C, 0x00 ; carre (square)
.equ B, 0x01 ; bar-shape
.equ T, 0x02 ; t-shape
.equ S, 0x03 ; s-shape
.equ L, 0x04 ; l-shape

;; GSA type
.equ NOTHING, 0x00 ; the array location is not occupied
.equ PLACED, 0x01 ; occupied by a nonmoving object
.equ FALLING, 0x02 ; occupied by a falling object

;; orientation enumeration
.equ N, 0x00 ; north
.equ E, 0x01 ; east
.equ So, 0x02 ; south
.equ W, 0x03 ; west

;; Rotation enumeration
.equ CLOCKWISE, 0 ; clockwise direction of rotation
.equ COUNTERCLOCKWISE, 1 ; counterclockwise direction of rotation

;; Actions over tetrominos
.equ moveL, 0x01 ; move left, horizontally
.equ rotL, 0x02 ; rotate counterclockwise
.equ reset, 0x04 ; reset the game
.equ rotR, 0x08 ; rotate clockwise
.equ moveR, 0x10 ; move right, horizontally
.equ moveD, 0x20 ; move down, vertically

;; Collision return ENUM
.equ W_COL, 0x00 ; collision on the west side of tetrominoe
.equ E_COL, 0x01 ; collision on the east side of tetrominoe
.equ So_COL, 0x02 ; collision on the south side of tetrominoe
.equ OVERLAP, 0x03 ; tetromino overlaps with something
.equ NONE, 0x04 ; tetromino does not collide with anything

;; start location
.equ START_X, 6 ; start tetromino x-axis coordinate
.equ START_Y, 1 ; start tetromino y-axis coordinate

; standard limits
.equ X_LIMIT, 12
.equ Y_LIMIT, 8

;; game rate of tetrominoes falling down (in terms of game loop iteration)
.equ RATE, 5

; BEGIN:main
main:
    call clear_leds

    addi a0, zero, 1
    addi a1, zero, 1
    call set_pixel

; END:main

; BEGIN:clear_leds
clear_leds:
    ; set LED[x] to 0
    stw zero, LEDS(0)
    stw zero, LEDS(4)
    stw zero, LEDS(8)
    ret
; END:clear_leds

; BEGIN:set_pixel
set_pixel:
    ; $a0: pixel’s x-coordinate.
    ; $a1: pixel’s y-coordinate.

    ; load y index into t1
    addi t1, zero, 1
    rol t1, t1, a1 ; Create a mask for the byte

    ldb t2, LEDS(a0)
    or t2, t2, t1
    stb t2, LEDS(a0)

    ret
; END:set_pixel

; BEGIN:wait
wait:
    ; val 2^20 for real
    ; val Smaller for simulation

    ; TODO: A simple loop from  val to 0
    
    ret
; END:wait



; BEGIN:get_input
get_input:

    ret
; END:get_input

; BEGIN:set_gsa
set_gsa:

    ret
; END:set_gsa

; BEGIN:move_gsa
move_gsa:

    ret
; END:move_gsa

; BEGIN:in_gsa
in_gsa:

    ret
; END:in_gsa

; BEGIN:display_score
display_score:

    ret
; END:display_score

; BEGIN:reset_game
reset_game:

    ret
; END:reset_game

; BEGIN:detect_full_line
detect_full_line:

    ret
; END:detect_full_line

; BEGIN:remove_full_line
remove_full_line:

    ret
; END:remove_full_line

; BEGIN:increment_score
increment_score:

    ret
; END:increment_score

; BEGIN:act
act:

    ret
; END:act

; BEGIN:rotate_tetrominoe
rotate_tetrominoe:

    ret
; END:rotate_tetrominoe

; BEGIN:detect_collision
detect_collision:

    ret
; END:detect_collision

; BEGIN:draw_gsa
draw_gsa:
    
    ret
; END:draw_gsa

; BEGIN:draw_tetromino
draw_tetromino:

    ret
; END:draw_tetromino

; BEGIN:generate_tetrominoe
generate_tetrominoe:

    ret
; END:generate_tetrominoe