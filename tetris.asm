
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
    
    addi a0, zero, 0
    addi a1, zero, 0
    call set_pixel

    addi a0, zero, 6
    addi a1, zero, 7
    call set_pixel

    addi a0, zero, 9
    addi a1, zero, 3
    call set_pixel

    addi a0, zero, 11
    addi a1, zero, 7
    call set_pixel

    call end

; END:main

; BEGIN:clear_leds
clear_leds:
    ; set LED[x] to 0
    stw zero, LEDS(zero)
    
    ; store 4 in t0 register
    addi t0, zero, 4
    stw zero, LEDS(t0)
    
    ; store 8 in t0 register
    addi t0, zero, 8
    stw zero, LEDS(t0)
    ret
; END:clear_leds

; BEGIN:set_pixel
set_pixel:
    ; $a0: pixel’s x-coordinate.
    ; $a1: pixel’s y-coordinate.

    ; load y index into t1
    addi t0, zero, 1
    rol t0, t0, a1 ; Create a mask for the byte

    ; Calculate a shit of n bytes which will be applied to the mask
    andi t3, a0, 3 ; get last two significants bits of x
    addi t2, zero, 8; Calculate shift of bytes
    mul t2, t2, t3

    rol t0, t0, t2

    ; Get third and forth bit of x
    add t4, a0, zero
    andi t4, t4, 0xFD
    
    ; Load leds, apply mask and
    ldw t1, LEDS(t4)
    or t1, t1, t0
    stw t1, LEDS(t4)

    ret
; END:set_pixel

; BEGIN:wait
wait:
    ; val 2^20 for real
    ; val Smaller for simulation

    ; TODO: A simple loop from  val to 0
    
    ret
; END:wait

; BEGIN:in_gsa
in_gsa:
    ;a0: pixel’s x-coordinate
    ;a1: pixel’s y-coordinate
    ;Return Value
    ;v0: 1 if out of GSA, 0 if in GSA
    add v0, zero, zero
    cmpgei v0, a0, X_LIMIT
    cmpgei v0, a1, Y_LIMIT
    cmplt v0, a0, zero
    cmplt v0, a1, zero
    ret

; END:in_gsa

; BEGIN:get_gsa
get_gsa:
    ;a0: pixel’s x-coordinate
    ;a1: pixel’s y-coordinate

    ret
; END:get_gsa

; BEGIN:set_gsa
set_gsa:
    ;a0: pixel’s x-coordinate
    ;a1: pixel’s y-coordinate
    ;a2: pixel’s value p

    ret
; END:set_gsa

; BEGIN:get_input
get_input:

    ret
; END:get_input

; BEGIN:move_gsa
move_gsa:

    ret
; END:move_gsa

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


; BEGIN:end
end:

; END:end
