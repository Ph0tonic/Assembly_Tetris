  ;; game state memory location
  .equ T_X, 0x1000                  ; falling tetrominoe position on x
  .equ T_Y, 0x1004                  ; falling tetrominoe position on y
  .equ T_type, 0x1008               ; falling tetrominoe type
  .equ T_orientation, 0x100C        ; falling tetrominoe orientation
  .equ SCORE,  0x1010               ; score
  .equ GSA, 0x1014                  ; Game State Array starting address
  .equ SEVEN_SEGS, 0x1198           ; 7-segment display addresses
  .equ LEDS, 0x2000                 ; LED address
  .equ RANDOM_NUM, 0x2010           ; Random number generator address
  .equ BUTTONS, 0x2030              ; Buttons addresses

  .equ STACK, 0x2000

  ;; type enumeration
  .equ C, 0x00
  .equ B, 0x01
  .equ T, 0x02
  .equ S, 0x03
  .equ L, 0x04

  ;; GSA type
  .equ NOTHING, 0x0
  .equ PLACED, 0x1
  .equ FALLING, 0x2

  ;; orientation enumeration
  .equ N, 0
  .equ E, 1
  .equ So, 2
  .equ W, 3
  .equ ORIENTATION_END, 4

  ;; collision boundaries
  .equ COL_X, 4
  .equ COL_Y, 3

  ;; Rotation enumeration
  .equ CLOCKWISE, 0
  .equ COUNTERCLOCKWISE, 1

  ;; Button enumeration
  .equ moveL, 0x01
  .equ rotL, 0x02
  .equ reset, 0x04
  .equ rotR, 0x08
  .equ moveR, 0x10
  .equ moveD, 0x20

  ;; Collision return ENUM
  .equ W_COL, 0
  .equ E_COL, 1
  .equ So_COL, 2
  .equ OVERLAP, 3
  .equ NONE, 4

  ;; start location
  .equ START_X, 6
  .equ START_Y, 1

  ;; game rate of tetrominoe falling down (in terms of game loop iteration)
  .equ RATE, 5

  ;; standard limits
  .equ X_LIMIT, 12
  .equ Y_LIMIT, 8

; BEGIN:main
main:
    addi sp, zero, STACK

    call clear_leds

    call generate_tetrominoe
;    call wait
	addi a0, zero, PLACED
    call draw_tetromino
;	call wait
;	call wait
    call draw_gsa

    ;call clear_leds

    ;call generate_tetrominoe
	;call draw_tetromino
	
    ;call draw_gsa
	call wait
	call wait
	call wait
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
    ;addi t0, zero, 1048576 ; 2^20
    addi t0, zero, 256 ; 2^10 for simulation
    add t1, zero, zero ; initialize loop variable to 0

    ; TODO: Comment for inner label
    wait_loop:
    addi t1, t1, 1 
    blt t1, t0, wait_loop
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
    cmpgei v1, a1, Y_LIMIT
    or v0, v0, v1
    cmplt v1, a0, zero
    or v0, v0, v1
    cmplt v1, a1, zero
    or v0, v0, v1
    ret

; END:in_gsa

; BEGIN:get_gsa
get_gsa:
    ;a0: pixel’s x-coordinate
    ;a1: pixel’s y-coordinate
    ;96 word of 32 bits
    slli t7, a0, 3
    add t7, t7, a1
    slli t7,t7, 2
    ldw v0, GSA(t7)
    ret
; END:get_gsa

; BEGIN:set_gsa
set_gsa:
    ;a0: pixel’s x-coordinate
    ;a1: pixel’s y-coordinate
    ;a2: pixel’s value p
    slli t7, a0, 3
    add t7, t7, a1
    slli t7,t7, 2
    stw a2, GSA(t7)
    ret
; END:set_gsa

; BEGIN:draw_gsa
draw_gsa:
    ; TODO: Comments for labels
    
    addi sp, sp, -4
    stw ra, 0(sp)
    
    addi t5, zero, X_LIMIT
    draw_gsa_outer_loop:
    addi t5, t5, -1
    
    addi t6, zero, Y_LIMIT

    draw_gsa_inner_loop:
    addi t6, t6, -1
    
    add a0, t5, zero
    add a1, t6, zero

    call get_gsa

    addi t2, zero, NOTHING
    beq v0, t2, draw_gsa_inner_loop_end

    call set_pixel

    draw_gsa_inner_loop_end:
    bne t6, zero, draw_gsa_inner_loop
    bne t5, zero, draw_gsa_outer_loop
    
    ldw ra, 0(sp)
    addi sp, sp, 4

    ret
; END:draw_gsa

; BEGIN:draw_tetromino
draw_tetromino:
    ;a0: GSA value p

    ; Saving ra register
    addi sp, sp, -4
    stw ra, 0(sp)

    ; Param for value type in gsa
    add a2, zero, a0

    ldw s0, T_X(zero)
    ldw s1, T_Y(zero)
    ldw s2, T_orientation(zero)
    ldw s3, T_type(zero)

    slli t5, s3, 2
    add t5, t5, s2
    slli t5, t5, 2
    ldw t0, DRAW_Ax(t5)
    ldw t1, DRAW_Ay(t5)

    add a0, s0, zero
    add a1, s1, zero

    ; TODO: Comment inner loop
    add t2, zero, zero ; Counter
    addi t3, zero, 4 ; Max value
    draw_tetromino_loop:
    
    call in_gsa

    bne v0, zero, draw_tetromino_loop_zap_set
    call set_gsa
    
    ; TODO: Loop comment
    draw_tetromino_loop_zap_set:

    ; Increment coordinates
    ldw s4, 0(t0)
    ldw s5, 0(t1)

    add a0, s0, s4
    add a1, s1, s5

    addi t2, t2, 1
    addi t0, t0, 4
    addi t1, t1, 4

    ; TODO: End loop comment
    bne t2, t3, draw_tetromino_loop

    ; Restore ra
    ldw ra, 0(sp)
    addi sp, sp, 4

    ret
; END:draw_tetromino

; BEGIN:generate_tetrominoe
generate_tetrominoe:
    addi t0, zero, START_X
    stw t0, T_X(zero)

    addi t0, zero, START_Y
    stw t0, T_Y(zero)

    addi t0, zero, N
    stw t0, T_orientation(zero)

    addi t1, zero, 7
    addi t2, zero, 5

    loop_generate_tetromino:

    ldw t0, RANDOM_NUM(zero)
    and t0, t0, t1

    bge t0, t2, loop_generate_tetromino
    
    stw t0, T_type(zero)

    ret
; END:generate_tetrominoe

; BEGIN:rotate_tetrominoe
rotate_tetrominoe:
    ; if a0 = 0x02(rotL value)
    addi t0, zero, 0x02
    beq a0, t0, rotL

    ;else if a0 = 0x10(rotR value)
    addi t0, zero, 0x10
    beq a0, t0, rotR
    
    ; else quit
    ret

    rotL:
    ldw t0, T_orientation(zero); get the actual orientation
    beq t0, N, rotL_reset_pos ; if t_orientation = N then assign the W value (w value is the position value before N)

    ; Else : substract one at the actual position
    subi t0, t0, 1 ; t0 -= 1
    br save_new_orientation ; jump to saving section

    rotL_reset_pos:
    addi t0, zero, W; set the w value to t0
    br save_new_orientation ; jump to saving section

    rotR:
    ldw t0, T_orientation(zero); get the actual orientation
    beq t0, W, rotR_reset_pos ; if actual position is w we need to set the new position to N (N is the next position value after w)

    ; else we add 1 at the position
    addi t0, t0, 1; t0 += 1

    br save_new_orientation ; jump to saving section

    rotR_reset_pos:
    addi t0, zero, N; assign th value of n in t0
    br save_new_orientation ; jump to saving section
    
    save_new_orientation:
    stw t0, T_orientation(zero) ; save the new position (t0) in the memory

    ret
; END:rotate_tetrominoe

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

; BEGIN:detect_collision
detect_collision:

    ret
; END:detect_collision


  ;; TODO Insert your code here
font_data:
  .word 0xFC  ; 0
  .word 0x60  ; 1
  .word 0xDA  ; 2
  .word 0xF2  ; 3
  .word 0x66  ; 4
  .word 0xB6  ; 5
  .word 0xBE  ; 6
  .word 0xE0  ; 7
  .word 0xFE  ; 8
  .word 0xF6  ; 9

C_N_X:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

C_N_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0xFFFFFFFF

C_E_X:
  .word 0x01
  .word 0x00
  .word 0x01

C_E_Y:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

C_So_X:
  .word 0x01
  .word 0x00
  .word 0x01

C_So_Y:
  .word 0x00
  .word 0x01
  .word 0x01

C_W_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0xFFFFFFFF

C_W_Y:
  .word 0x00
  .word 0x01
  .word 0x01

B_N_X:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x02

B_N_Y:
  .word 0x00
  .word 0x00
  .word 0x00

B_E_X:
  .word 0x00
  .word 0x00
  .word 0x00

B_E_Y:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x02

B_So_X:
  .word 0xFFFFFFFE
  .word 0xFFFFFFFF
  .word 0x01

B_So_Y:
  .word 0x00
  .word 0x00
  .word 0x00

B_W_X:
  .word 0x00
  .word 0x00
  .word 0x00

B_W_Y:
  .word 0xFFFFFFFE
  .word 0xFFFFFFFF
  .word 0x01

T_N_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

T_N_Y:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0x00

T_E_X:
  .word 0x00
  .word 0x01
  .word 0x00

T_E_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

T_So_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

T_So_Y:
  .word 0x00
  .word 0x01
  .word 0x00

T_W_X:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0x00

T_W_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

S_N_X:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

S_N_Y:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

S_E_X:
  .word 0x00
  .word 0x01
  .word 0x01

S_E_Y:
  .word 0xFFFFFFFF
  .word 0x00
  .word 0x01

S_So_X:
  .word 0x01
  .word 0x00
  .word 0xFFFFFFFF

S_So_Y:
  .word 0x00
  .word 0x01
  .word 0x01

S_W_X:
  .word 0x00
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

S_W_Y:
  .word 0x01
  .word 0x00
  .word 0xFFFFFFFF

L_N_X:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x01

L_N_Y:
  .word 0x00
  .word 0x00
  .word 0xFFFFFFFF

L_E_X:
  .word 0x00
  .word 0x00
  .word 0x01

L_E_Y:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0x01

L_So_X:
  .word 0xFFFFFFFF
  .word 0x01
  .word 0xFFFFFFFF

L_So_Y:
  .word 0x00
  .word 0x00
  .word 0x01

L_W_X:
  .word 0x00
  .word 0x00
  .word 0xFFFFFFFF

L_W_Y:
  .word 0x01
  .word 0xFFFFFFFF
  .word 0xFFFFFFFF

DRAW_Ax:                        ; address of shape arrays, x axis
  .word C_N_X
  .word C_E_X
  .word C_So_X
  .word C_W_X
  .word B_N_X
  .word B_E_X
  .word B_So_X
  .word B_W_X
  .word T_N_X
  .word T_E_X
  .word T_So_X
  .word T_W_X
  .word S_N_X
  .word S_E_X
  .word S_So_X
  .word S_W_X
  .word L_N_X
  .word L_E_X
  .word L_So_X
  .word L_W_X

DRAW_Ay:                        ; address of shape arrays, y_axis
  .word C_N_Y
  .word C_E_Y
  .word C_So_Y
  .word C_W_Y
  .word B_N_Y
  .word B_E_Y
  .word B_So_Y
  .word B_W_Y
  .word T_N_Y
  .word T_E_Y
  .word T_So_Y
  .word T_W_Y
  .word S_N_Y
  .word S_E_Y
  .word S_So_Y
  .word S_W_Y
  .word L_N_Y
  .word L_E_Y
  .word L_So_Y
  .word L_W_Y
; BEGIN:end
end:

; END:end