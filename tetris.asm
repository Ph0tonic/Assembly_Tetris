; game state memory location
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

; type enumeration
.equ C, 0x00
.equ B, 0x01
.equ T, 0x02
.equ S, 0x03
.equ L, 0x04

; GSA type
.equ NOTHING, 0x0
.equ PLACED, 0x1
.equ FALLING, 0x2

; orientation enumeration
.equ N, 0
.equ E, 1
.equ So, 2
.equ W, 3
.equ ORIENTATION_END, 4

; collision boundaries
.equ COL_X, 4
.equ COL_Y, 3

; Rotation enumeration
.equ CLOCKWISE, 0
.equ COUNTERCLOCKWISE, 1

; Button enumeration
.equ moveL, 0x01
.equ rotL, 0x02
.equ reset, 0x04
.equ rotR, 0x08
.equ moveR, 0x10
.equ moveD, 0x20

; Collision return ENUM
.equ W_COL, 0
.equ E_COL, 1
.equ So_COL, 2
.equ OVERLAP, 3
.equ NONE, 4

; start location
.equ START_X, 6
.equ START_Y, 1

; game rate of tetrominoe falling down (in terms of game loop iteration)
.equ RATE, 5

; standard limits
.equ X_LIMIT, 12
.equ Y_LIMIT, 8

; BEGIN:main
main:
    ; Init section
    addi sp, zero, STACK
    addi s0, s0, RATE ; RATE Store in s0
    call reset_game

    main_loop:
        main_falling_loop:
            add s1, zero, zero
            
            main_event_rate:
            ; while i < RATE do
            bge s1, s0, main_event_rate_end

                ; draw the GSA on the leds and display the score
                ; addi a0, zero, FALLING
                ; call draw_tetromino

                call draw_gsa
                call display_score

                ; remove the falling tetromino from the GSA, not from the tetromino structure
                addi a0, zero, NOTHING
                call draw_tetromino
                
                ; wait approximately 0.2 s
                call wait

                ; get the button input
                call get_input
                ; if a button has been pressed, try to do the required action, if possible (act, etc...)
                add a0, v0, zero
                call act

                ; redraw the falling tetromino on the GSA
                addi a0, zero, FALLING
                call draw_tetromino

                ; increment s1
                addi s1, s1, 1
            br main_event_rate
            main_event_rate_end:

            ; remove the falling tetromino from the GSA, not from the tetromino structure
            addi a0, zero, NOTHING
            call draw_tetromino

            ; try to move the falling tetromino down
            addi a0, zero, moveD
            call act
            add s2, v0, zero

            ; redraw the falling tetromino on the GSA
            addi a0, zero, FALLING
            call draw_tetromino
        ; until falling tetromino can’t be drawn when moving down
        beq zero, s2, main_falling_loop

        ; replace the falling tetromino by a placed tetromino
        addi a0, zero, PLACED
        call draw_tetromino
        
        addi s3, zero, Y_LIMIT

        ; while a full line do
        main_detect_full_line:
        call detect_full_line
        beq v0, s3, main_detect_full_line_end
        
            ; remove the bottommost full line
            add a0, v0, zero
            call remove_full_line
        
        br main_detect_full_line
        main_detect_full_line_end:

        ; generate a new tetromino
        call generate_tetromino

        ; detect for overlaping collisions
        addi a0, zero, OVERLAP
        call detect_collision
        
        add s2, v0, zero
        addi s4, zero, NONE
        bne s4, s2, main_draw_tetromino_zap
        
        ; if no collisions then draw the falling tetromino on the GSA end
        addi a0, zero, FALLING
        call draw_tetromino

        main_draw_tetromino_zap:

    ; until newly generated falling tetromino overlaps with something
    beq s2, s4, main_loop
    br end

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
    slli t2, t3, 3
    ;mul t2, t2, t3

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
    addi t0, zero, 1
    ;slli t0, t0, 20 ; 2^20 for real
    slli t0, t0, 10 ; 2^10 for simulation
    add t1, zero, zero ; initialize loop variable to 0

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
    
    addi sp, sp, -4
    stw ra, 0(sp)
    
    call clear_leds

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
    addi sp, sp, -28
    stw ra, 0(sp)
    stw s0, 4(sp)
    stw s1, 8(sp)
    stw s2, 12(sp)
    stw s3, 16(sp)
    stw s4, 20(sp)
    stw s5, 24(sp)

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

    add t2, zero, zero ; Counter
    addi t3, zero, 4 ; Max value
    draw_tetromino_loop:
    
    call in_gsa

    bne v0, zero, draw_tetromino_loop_zap_set
    call set_gsa
    
    draw_tetromino_loop_zap_set:

    ; Increment coordinates
    ldw s4, 0(t0)
    ldw s5, 0(t1)

    add a0, s0, s4
    add a1, s1, s5

    addi t2, t2, 1
    addi t0, t0, 4
    addi t1, t1, 4

    bne t2, t3, draw_tetromino_loop

    ; Restore ra
    ldw ra, 0(sp)
    ldw s0, 4(sp)
    ldw s1, 8(sp)
    ldw s2, 12(sp)
    ldw s3, 16(sp)
    ldw s4, 20(sp)
    ldw s5, 24(sp)
    addi sp, sp, 28

    ret
; END:draw_tetromino

; BEGIN:generate_tetromino
generate_tetromino:
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
; END:generate_tetromino

; BEGIN:detect_collision
detect_collision:
    ; a0 => value of colision
    ; v0 => return value, same as a0 if collision detected otherwise NONE

    ; Saving ra register
    addi sp, sp, -8
    stw ra, 4(sp)
    stw a0, 0(sp)

    ; Saving sa register
    addi sp, sp, -24
    stw s0, 0(sp)
    stw s1, 4(sp)
    stw s2, 8(sp)
    stw s3, 12(sp)
    stw s4, 16(sp)
    stw s5, 20(sp)

    ldw s0, T_X(zero)
    ldw s1, T_Y(zero)
    ldw s2, T_orientation(zero)
    ldw s3, T_type(zero)

    ; Config offset for collision detection
    cmpeqi s4, a0, E_COL
    add s0, s0, s4
    cmpeqi s4, a0, W_COL
    sub s0, s0, s4
    cmpeqi s4, a0, So_COL
    add s1, s1, s4
    
    ; Get coord values for tetromino
    slli t5, s3, 2
    add t5, t5, s2
    slli t5, t5, 2
    ldw t0, DRAW_Ax(t5)
    ldw t1, DRAW_Ay(t5)

    add a0, s0, zero
    add a1, s1, zero

    add t2, zero, zero ; Counter
    addi t3, zero, 4 ; Max value
    detect_collision_loop:
    
    ; Detect if is in_gsa
    add v0, zero, zero
    cmpgei v0, a0, X_LIMIT
    cmpgei v1, a1, Y_LIMIT
    or v0, v0, v1
    cmplt v1, a0, zero
    or v0, v0, v1
    bne v0, zero, detect_collision_colide
    
    ; Check if 
    blt a0, zero, detect_collision_zap

    ; Detect collision of current tetromino part
    call get_gsa
    addi t4, zero, NOTHING
    bne v0, t4, detect_collision_colide   
    
    detect_collision_zap:

    ; Increment coordinates
    ldw s4, 0(t0)
    ldw s5, 0(t1)

    add a0, s0, s4
    add a1, s1, s5

    addi t2, t2, 1
    addi t0, t0, 4
    addi t1, t1, 4

    bne t2, t3, detect_collision_loop

    detect_collision_none:
    addi v0, zero, NONE

    ; Saving sa register
    ldw s0, 0(sp)
    ldw s1, 4(sp)
    ldw s2, 8(sp)
    ldw s3, 12(sp)
    ldw s4, 16(sp)
    ldw s5, 20(sp)
    addi sp, sp, 24

    ; Restore ra
    ldw ra, 4(sp)
    ldw a0, 0(sp)
    addi sp, sp, 8
    ret

    detect_collision_colide:

    ; Saving sa register
    ldw s0, 0(sp)
    ldw s1, 4(sp)
    ldw s2, 8(sp)
    ldw s3, 12(sp)
    ldw s4, 16(sp)
    ldw s5, 20(sp)
    addi sp, sp, 24

    ; Restore ra
    ldw ra, 4(sp)
    ldw a0, 0(sp)
    addi sp, sp, 8

	add v0, a0, zero

    ret
; END:detect_collision

; BEGIN:act
act:
    ; Params: a0 -> action
    ; Return: v0 -> 0 if succeeded otherwise 1
    ; Saving ra register
    addi sp, sp, -40
    stw ra, 0(sp)
    stw s0, 4(sp)
    stw s1, 8(sp)
    stw s2, 12(sp)
    stw s3, 16(sp)
    stw s4, 20(sp)
    stw s5, 24(sp)
    
    ; Save tetromino data on stack in the case of we need to restore it if this function fails
    ldw t1, T_orientation(zero)
    stw t1, 28(sp)
    ldw t1, T_X(zero)
    stw t1, 32(sp)
    ldw t1, T_Y(zero)
    stw t1, 36(sp)

    ; Test parameter to know wich collision is tested
    cmpeqi t0, a0, moveL
    bne t0, zero, act_moveW
    cmpeqi t0, a0, moveR
    bne t0, zero, act_moveE
    cmpeqi t0, a0, moveD
    bne t0, zero, act_moveSo

    cmpeqi t0, a0, rotL
    bne t0, zero, act_rotateL
    cmpeqi t0, a0, rotR
    bne t0, zero, act_rotateR

    cmpeqi t0, a0, reset
    bne t0, zero, act_reset

    ; if the parameter doesnt match anything -> quit
    br act_end

    ; Test collision in direction choosen and if success then move tx, ty
    act_moveW:
    addi a0, zero, W_COL
    call detect_collision 
    beq v0, a0, act_fail      ; if v0 = a0 there is a collision -> v0 will be set to 1
    
    ldw t1, T_X(zero)         ; get the current value of T_X
    addi t1, t1, -1           ; decrement the x cordinate
    stw t1, T_X(zero)

    br act_success            ; else -> SUCCESS

    act_moveE:
    addi a0, zero, E_COL
    call detect_collision 
    beq v0, a0, act_fail                    ; if v0 = a0 there is a collision -> v0 will be set to 1

    ldw t1, T_X(zero)                       ; get the current value of T_X
    addi t1, t1, 1                          ; increment the x cordinate
    stw t1, T_X(zero)

    br act_success                          ; else -> SUCCESS

    act_moveSo:
    addi a0, zero, So_COL
    call detect_collision 
    beq v0, a0, act_fail                    ; if v0 = a0 there is a collision -> v0 will be set to 1

    ldw t1, T_Y(zero)                       ; get the current value of T_Y
    addi t1, t1, 1                          ; increment the y cordinate
    stw t1, T_Y(zero)

    br act_success                          ; else -> SUCCESS

    ; Try to move left or right
    act_rotateL:
    addi a0, zero, rotL
    call rotate_tetromino
    br act_overlap
    
    ; Try to move left or right
    act_rotateR:
    addi a0, zero, rotR
    call rotate_tetromino
    br act_overlap
    
    act_overlap:
    addi a0, zero, OVERLAP
    call detect_collision
    addi t0, zero, NONE

    beq v0, t0, act_success

    ; Collision detected try to fix with a shift left or right
    addi s0, zero, X_LIMIT
    addi s1, zero, 2                ; Loop counting
    
    srli t0, t0, 1                ; Size / 2
    ldw t1, T_X(zero)
    ; s2 = 1 if left part otherwise right part
    cmplt s2, t1, t2

    act_overlap_loop:
    addi s1, s1, -1

    bne s2, zero, right_part_of_table
    
    ; Left part of the table
    ldw t1, T_X(zero)         ; get the current value of T_X
    addi t1, t1, 1           ; decrement the x cordinate
    stw t1, T_X(zero)

    br act_overlap_loop_end
    
    ; Right part in the table
    right_part_of_table:
    ldw t1, T_X(zero)         ; get the current value of T_X
    addi t1, t1, -1           ; decrement the x cordinate
    stw t1, T_X(zero)

    br act_overlap_loop_end

    act_overlap_loop_end:
    addi a0, zero, OVERLAP
    call detect_collision
    addi t0, zero, NONE

    beq v0, t0, act_success
    bne s1, zero, act_overlap_loop
    br act_fail

    act_reset:
    call reset_game

    act_success:
    add v0, zero, zero
    br act_end 

    ; reset initial position values for tetromino
    act_fail:
    addi v0, zero, 1

    ldw t1, 28(sp)
    stw t1, T_orientation(zero)
    ldw t1, 32(sp)
    stw t1, T_X(zero)
    ldw t1, 36(sp)
    stw t1, T_Y(zero)

    act_end:
    ldw ra, 0(sp)
    ldw s0, 4(sp)
    ldw s1, 8(sp)
    ldw s2, 12(sp)
    ldw s3, 16(sp)
    ldw s4, 20(sp)
    ldw s5, 24(sp)
    addi sp, sp, 40
    
    ret
; END:act

; BEGIN:rotate_tetromino
rotate_tetromino:
    ; if a0 = rotL
    addi t0, zero, rotL
    beq a0, t0, rotL_label

    ;else if a0 = rotR
    addi t0, zero, rotR
    beq a0, t0, rotR_label
    
    ; else quit
    ret

    rotL_label:
    ldw t0, T_orientation(zero); get the actual orientation
	addi t1, zero, N
    beq t0, t1, rotL_reset_pos ; if t_orientation = N then assign the W value (w value is the position value before N)

    ; Else : substract one at the actual position
    addi t0, t0, -1 ; t0 -= 1
    br save_new_orientation ; jump to saving section

    rotL_reset_pos:
    addi t0, zero, W ; set the w value to t0
    br save_new_orientation ; jump to saving section

    rotR_label:
    ldw t0, T_orientation(zero); get the actual orientation
    addi t1, zero, W
	beq t0, t1, rotR_reset_pos ; if actual position is w we need to set the new position to N (N is the next position value after w)

    ; else we add 1 at the position
    addi t0, t0, 1; t0 += 1

    br save_new_orientation ; jump to saving section

    rotR_reset_pos:
    addi t0, zero, N; assign th value of n in t0
    br save_new_orientation ; jump to saving section
    
    save_new_orientation:
    stw t0, T_orientation(zero) ; save the new position (t0) in the memory

    ret
; END:rotate_tetromino

; BEGIN:get_input
get_input:
    addi t0, zero, 4
    ldw t1, BUTTONS(t0)

    addi t2, zero, 1
    addi v0, zero, moveL
    beq t2, t1, get_input_end

    addi t2, zero, 2
    addi v0, zero, rotL
    beq t2, t1, get_input_end
    
    addi t2, zero, 4
    addi v0, zero, reset
    beq t2, t1, get_input_end
    
    addi t2, zero, 8
    addi v0, zero, rotR
    beq t2, t1, get_input_end
    
    addi t2, zero, 16
    addi v0, zero, moveR
    beq t2, t1, get_input_end

    add v0, zero, zero

    get_input_end:
    stw zero, BUTTONS(t0)

    ret
; END:get_input

; BEGIN:reset_game
reset_game:
    ; Saving ra register
    addi sp, sp, -4
    stw ra, 0(sp)

    ; New tetrominoe generated
    call generate_tetromino

    ; Empty GSA
    call reset_gsa
    
    ; Reset the leds accordingly to GSA
    call draw_gsa

    ; Game score set to zero
    stw zero, SCORE(zero)
    
    ; Reset score counter leds
    call display_score

    ; Restore return address
    ldw ra, 0(sp)
    addi sp, sp, 4

    ret
; END:reset_game

; BEGIN:detect_full_line
detect_full_line:
    ; Saving ra register
    addi sp, sp, -20
    stw ra, 0(sp)
    stw s0, 4(sp)
    stw s1, 8(sp)
    stw s2, 12(sp)
    stw s3, 16(sp)

    addi s3, zero, NOTHING
    addi s0, zero, -1
    addi s2, zero, Y_LIMIT

    detect_full_line_y:
    addi s0, s0, 1
    beq s0, s2, full_line_none

    addi s1, zero, X_LIMIT
    
    detect_full_line_x:
    addi s1, s1, -1

    add a0, s1, zero
    add a1, s0, zero
    call get_gsa ; TODO: Improve without using get_gsa

    ; If empty start detection of a new line
    beq v0, s3, detect_full_line_y

    ; Iterate over the line if not empty
    bne s1, zero, detect_full_line_x

    ; Full line
    full_line_detected:
    add v0, s0, zero

    ldw ra, 0(sp)
    ldw s0, 4(sp)
    ldw s1, 8(sp)
    ldw s2, 12(sp)
    ldw s3, 16(sp)
    addi sp, sp, 20

    ret

    full_line_none:
    ; Return no full line detected
    addi v0, zero, Y_LIMIT
    
    ldw ra, 0(sp)
    ldw s0, 4(sp)
    ldw s1, 8(sp)
    ldw s2, 12(sp)
    ldw s3, 16(sp)
    addi sp, sp, 20

    ret
; END:detect_full_line

; BEGIN:remove_full_line
remove_full_line:
    ; Param a0: y-coordinate of the full line to be removed

    ; Saving ra register
    addi sp, sp, -12
    stw ra, 0(sp)
    stw s0, 4(sp)
    stw s0, 8(sp)

    add s0, a0, zero

    ; BLINKING SEQUENCE
    ; Remove line
    add a1, s0, zero
    addi a2, zero, NOTHING
    call set_line_value

    call draw_gsa
    call wait
    
    ; Display line
    add a1, s0, zero
    addi a2, zero, PLACED
    call set_line_value

    call draw_gsa
    call wait

    ; Remove line
    add a1, s0, zero
    addi a2, zero, NOTHING
    call set_line_value

    call draw_gsa
    call wait
    
    ; Display line
    add a1, s0, zero
    addi a2, zero, PLACED
    call set_line_value

    call draw_gsa
    call wait

    ; Remove line
    add a1, s0, zero
    addi a2, zero, NOTHING
    call set_line_value

    call draw_gsa
    call wait

    ; Move pixel above y line one pixel down
    ; Y index is in s0
    remove_line_loop_y:

    addi s1, zero, X_LIMIT
    remove_line_loop_x:
    addi s1, s1, -1

    ; Set value
    add a0, s1, zero
    addi a1, s0, -1
    call get_gsa
    add a1, s0, zero
    add a2, v0, zero
    call set_gsa

    bne zero, s1, remove_line_loop_x

    addi s0, s0, -1
    bne s0, zero, remove_line_loop_y

    call increment_score
    call display_score

    ; Restore registers
    ldw ra, 0(sp)
    ldw s0, 4(sp)
    ldw s1, 4(sp)
    addi sp, sp, 12

    ret
; END:remove_full_line

; BEGIN:increment_score
increment_score:
    ldw t0, SCORE(zero)
    addi t0, t0, 1

    ; Check for max value (999)
    addi t1, zero, 10000
    blt t0, t1, increment_score_set

    ; Reset score to zero
    add t0, zero, zero

    increment_score_set:

    stw t0, SCORE(zero)
    ret
; END:increment_score

; BEGIN:display_score
display_score:
    ; Counter
    ldw t0, SCORE(zero) ; Units
    add t1, zero, zero ; Decades
    add t2, zero, zero ; Hundreds
    add t3, zero, zero ; Thousands

    addi t4, zero, 1000
    addi t5, zero, 100
    addi t6, zero, 10

    display_score_1000_loop:
    sub t0, t0, t4

    blt t0, zero, end_1000_loop

    addi t3, t3, 1
    br display_score_1000_loop

    end_1000_loop:
    add t0, t0, t4
    
    display_score_100_loop:
    sub t0, t0, t5

    blt t0, zero, end_100_loop

    addi t2, t2, 1
    br display_score_100_loop

    end_100_loop:
    add t0, t0, t5

    display_score_10_loop:
    sub t0, t0, t6

    blt t0, zero, end_10_loop

    addi t1, t1, 1
    br display_score_10_loop

    end_10_loop:
    add t0, t0, t6

    ; Units left are in t0

    ; Store unites
    slli t0, t0, 2
    ldw t7, font_data(t0)
    addi t4, zero, 12
    stw t7, SEVEN_SEGS(t4)

    ; Store decades
    slli t1, t1, 2
    ldw t7, font_data(t1)
    addi t4, zero, 8
    stw t7, SEVEN_SEGS(t4)

    ; Store hundred
    slli t2, t2, 2
    ldw t7, font_data(t2)
    addi t4, zero, 4
    stw t7, SEVEN_SEGS(t4)

    ; Store thousands (always zero)
    slli t3, t3, 2
    ldw t7, font_data(t3)
    addi t4, zero, 0
    stw t7, SEVEN_SEGS(t4)

    ret
; END:display_score

; BEGIN:helper

.equ STACK, 0x2000

set_line_value:
    ; Saving ra register
    ; a1 indice y
    ; a2 value
    addi sp, sp, -4
    stw ra, 0(sp)

    addi a0, zero, X_LIMIT
    
    set_line_value_x:
    addi a0, a0, -1
    call set_gsa

    ; Iterate over the line
    bne a0, zero, set_line_value_x

    ldw ra, 0(sp)
    addi sp, sp, 4

    ret

reset_gsa:
    ; Saving ra register
    addi sp, sp, -16
    stw ra, 0(sp)
    stw a0, 4(sp)
    stw a1, 8(sp)
    stw a2, 12(sp)

    addi t0, zero, X_LIMIT
    addi t1, zero, Y_LIMIT

    addi a2, zero, NOTHING

    add a0, zero, zero
    reset_game_empty_x:
    add a1, zero, zero

    reset_game_empty_y:
    call set_gsa

    addi a1, a1, 1 
    blt a1, t1, reset_game_empty_y

    addi a0, a0, 1
    blt a0, t0, reset_game_empty_x

    ; Restore ra
    ldw ra, 0(sp)
    ldw a0, 4(sp)
    ldw a1, 8(sp)
    ldw a2, 12(sp)
    addi sp, sp, 16

    ret
; END:helper

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