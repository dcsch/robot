;
; droid2.asm
;
; An Atari 2600 VCS videogame.
;
; Copyright (C) David Schweinsberg 1997, 1999
;------------------------------------------------------------------------------
; HISTORY
;
; v0.2.2	1999/01/11	dcs
; All Supercharger code removed, due to lack of interest, and the need to
; tighten-up the project, otherwise it'll never get finished.
; New positioning scheme for droids is required.
;
; v0.2.1	1998/01/05	dcs
; Number of floors on-screen reduced to five, to make room for a status-bar
; at the bottom.  Fine-positioning included for BALL graphic to allow
; animation.  Initial BALL animation routines.  Supercharger multi-load code.
;
; v0.2.0	1997/11/28	dcs
; BALL graphics positioning oddities, due to relative pixel movement, fixed.
; NUSIZ1 processing added.
;
; v0.1.2	1997/11/10	dcs
; Binary format changed to Supercharger format (8448 bytes); we now have three
; distinct banks.  Bank 3 contains the kernel code, banks 1 & 2 are to be used
; for data.  We also now have access to more RAM!
;
; v0.1.1	1997/10/14	dcs
; VSYNC modified so VBLANK starts before the VSYNC rather than after.
; Special-case lines moved from 7->0 to 31->24 to simplify the loop branching.
; Eliminated the branching that was required for player/no-player graphics,
; this is now handled on a floor-by-floor basis, each floor having an offset
; into the P0 data (most pointing to empty space!).
; Optimised so the Y register holds the floor-line number, without the need
;   to reload from RAM.
; BALL enemy graphics incorporated (the droids).
;
; v0.1.0	1997/09/24	dcs
; Initial implementation of hybrid, asymmetric scrolling playfield.
; BIN posted on Stella mailing list.
;

	processor 6502

	include vcs.inc

;
; Timing constants
; ----------------

;	PAL             | NTSC
;   ----------------+-----------------
;	 48 VBLANK      |  40 VBLANK
;	228 KERNEL      | 192 KERNEL
;	 36 OVERSCAN    |  30 OVERSCAN
;   ---             | ---
;	312 lines/frame | 262 lines/frame

; -------------------
; NTSC

VBLANK_TIME		= $2b
KERNEL_LINES	= $c0
;KERNEL_LINES	= $bd	;-3 to allow for initial lag
PF_FLOOR_LINES	= $a0
PF_STATUS_LINES	= $20
OVERSCAN_TIME	= $1a

; -------------------


;
; Constants
; ---------

STATE_OVER		= $00		; game over (Title sequence)
STATE_PLAY		= $01		; game in session

PF_COLOUR		= $84
PF_BG_COLOUR	= $8e		; playfield background colour
PF_TB_COLOUR	= $00		; playfield top border colour
PF_SB_COLOUR	= $04		; playfield status bar colour
P0_COLOUR		= $00		; player 0 colour
ENEMY_COLOUR	= $1c

RAM				= $f000		; Supercharger RAM access

;
; Variable definitions
; --------------------
;
CONTROL_BYTE	= $80		; Supercharger control byte

BUF0			= $81
CURR_FLOOR		= $81		; shared
JUMP_POINT		= $81

BUF1			= $82
CURR_EL			= $82

BUF2			= $83


LINE			= $84		; current screen line (player at 0)
FRAME_LINE		= $85		; current frame line

THIS_FLOOR		= $86		; current floor
BALL_ADJUST		= $86		; ball movement adjust at top-of-screen

TOP_FLOOR		= $87		; the floor at the top of the screen
TOP_FLOOR_LINE	= $88		; the floor-line at the top of screen
PF_DATA_INDEX	= $89		; playfield data index into RAM

P0_X			= $8a		; player's horizontal position
P0_Y			= $8b		; player's vertical position
P0_FRAME		= $8c		; player's animation frame
P0_DATA_PTR		= $8d		; player's graphics data (address; 2 bytes)
P1_DATA_PTR		= $8f		; player's graphics data (address; 2 bytes)
COLUP1_DATA_PTR	= $92		; player 1 colour data (address; 2 bytes)
ENEMY_DATA_PTR	= $94		; enemy graphics data (address; 2 bytes)
EL_MOVE			= $96		; current elevator movement
JUMP_POS		= $97		; current jump position

PF1_L_DATA		= $a0		; playfield definition ($05)
PF2_L_DATA		= $a5		; playfield definition ($05)
PF0_R_DATA		= $aa		; playfield definition ($05)
PF1_R_DATA		= $af		; playfield definition ($05)
PF2_R_DATA		= $b4		; playfield definition ($05)

EL_STAT			= $c0		; elevator status ($20)
PLAYER_OFFSET	= $e0		; ($05)

TEMP_COLOUR		= $e6
TEMP_COUNT		= $e7

;
; Start of the ROM code and data
;

	org		$f000

ENTER
	cld
	sei
	ldx		#$ff			; Set the stack
	txs						;
	inx
	txa

clear
	sta		VSYNC,x			; Initialise zero page to zeros
	inx
	bne		clear

initplay		subroutine

	lda		#$00			; Set joystick ports for input
	sta		SWCHA

;	lda		#STATE_PLAY
;	sta		STATE

	;
	; set the floor to start at top-of-screen
	;
	lda		#$00
	sta		TOP_FLOOR

	lda		#$1f
	sta		TOP_FLOOR_LINE

	;
	; prepare player 0
	;
	lda		#<PLAYER		; player graphics data pointer
	ldx		#>PLAYER
	sta		P0_DATA_PTR
	stx		P0_DATA_PTR+1

	;
	; load MSB of GRP1 pointer
	;
	lda		#>ITEM_DATA
	sta		P1_DATA_PTR+1

	;
	; load MSB of COLUP1 pointer
	;
	lda		#>ITEM_COLOUR_DATA
	sta		COLUP1_DATA_PTR+1

	;
	; load MSB of enemy graphics data pointer
	;
	lda		#>ENEMY_DATA
	sta		ENEMY_DATA_PTR+1

	lda		#$4c			; position player at centre of screen
	sta		P0_X
	lda		#$97
	sta		P0_Y

	;
	; set some permanent colours
	;
	lda		#P0_COLOUR
	sta		COLUP0

	;
	; load level 1 status data into RAM
	;
	ldx		#$1f
.1
	lda		#LEVEL_1_EL,x
	sta		EL_STAT,x
	dex
	bpl		.1

	;
	; initialize enemy positions
	;
	; this consists of placing ememies at either their left or right
	; constraint, depending apon the starting vector - left for a negative
	; vector, right for a positive vector
	;
init_enemy_pos		subroutine
	ldy		#$1f

.1	lda		ENEMY_VECTOR,y			; determine if we're moving left or right
	bmi		.2
	ldx		ENEMY_RIGHT,y
	jmp		.3
.2	ldx		ENEMY_LEFT,y

.3	cmp		RAM,x					; Supercharger RAM access
	lda		ENEMY_POS,y				; stores X at ENEMY_POS + Y

	dey
	bpl		.1
init_enemy_pos_end

;==============================================================================
; MAIN LOOP
; ---------
; Composed of three stages:
;	* VBLANK
;		- Sync
;		- Read joystick
;		- Load playfield RAM
;	* KERNEL
;		- Render display
;	* OVERSCAN
;
; Each KERNEL line consists of:
;	* 68 color clocks of blank (22.6 cycles)
;	* 160 color clocks of display (53.3 cycles)
;==============================================================================

;
; VBLANK
;
start_vblank		subroutine
	ldy		#$02
	lda		#$00

	sty		VBLANK			; turn on VBLANK

	sty		WSYNC			; 
	sty		VSYNC			; turn on VSYNC

	sty		WSYNC			; wait for three scan lines
	sty		WSYNC
	sty		WSYNC

	sta		VSYNC			; turn off VSYNC

;	sty		VBLANK			; turn on VBLANK

	;
	; set timer for VBLANK_TIME
	;
	lda		#VBLANK_TIME
	sta		TIM64T

	;
	; if JUMP_POS > 0, then we're mid-jump, so controls are no good
	;
	ldx		JUMP_POS
	beq		fire_button

	dex
	stx		JUMP_POS

	lda		#>PLAYER_JUMP		; set animation to player JUMPING
	sta		P0_DATA_PTR+1

	jmp		move_right

	;
	; read fire-button
	;
fire_button		subroutine

	lda		INPT4
	bne		end_fire_button

	lda		#$0f
	sta		JUMP_POS

	lda		#>PLAYER_JUMP		; set animation to player JUMPING
	sta		P0_DATA_PTR+1

	jmp		move_right	
;	jmp		end_joystick		; fire-button has priority over joystick!

end_fire_button

	lda		#>PLAYER			; set animation to player RUNNING
	sta		P0_DATA_PTR+1

	;
	; Read joystick
	;  D7 - right P0
	;  D6 - left  P0
	;  D5 - down  P0
	;  D4 - up    P0
	;  D3 - right P1
	;  D2 - left  P1
	;  D1 - down  P1
	;  D0 - up    P1
	;
joystick		subroutine

	lda		SWCHA
	lsr
	lsr
	lsr
	lsr
	lsr
	bcc		.up
	lsr
	bcc		.down

	;
	; if the TOP_FLOOR_LINE isn't 1F, the player can't move left or right,
	; and the elevator continues to move up or down
	;
	ldx		TOP_FLOOR_LINE
	cpx		#$1f
	beq		.1

	lda		EL_MOVE
	lsr
	bcc		.up
	lsr
	bcc		.down
	jmp		.2

.1
	lsr
	bcc		.left
	lsr
	bcc		.right
.2
	lda		#$00
	sta		P0_FRAME
	jmp		end_joystick

.up
	ldx		#$02
	stx		EL_MOVE

	ldx		TOP_FLOOR_LINE
	inx
	cpx		#$20
	bne		.store_up

	ldx		TOP_FLOOR
	dex

	bmi		load_playfield_ram

;	txa
;	and		$1f
;	tax

	stx		TOP_FLOOR

	ldx		#$00

.store_up
	stx		TOP_FLOOR_LINE
	jmp		load_playfield_ram

.down
	ldx		#$01
	stx		EL_MOVE

	ldx		TOP_FLOOR_LINE
	dex
	cpx		#$00
	bpl		.store_down

	ldx		TOP_FLOOR
	inx

	cpx		#$1a
	bpl		load_playfield_ram

;	txa
;	and		$1f
;	tax

	stx		TOP_FLOOR

	ldx		#$1f

.store_down
	stx		TOP_FLOOR_LINE
	jmp		load_playfield_ram

.left
	inc		P0_FRAME

	lda		P0_FRAME
	and		#$01
	bne		end_joystick

move_left
	lda		#$08
	sta		REFP0				; put player's reflection
	lda		P0_X				; get player's horizontal position
	cmp		#$21
	bcc		load_playfield_ram
	dec		P0_X				; decrement player's horizontal position
	jmp		load_playfield_ram

.right
	inc		P0_FRAME

	lda		P0_FRAME
	and		#$01
	bne		end_joystick

move_right
	lda		#$00
	sta		REFP0				; put player's reflection
	lda		P0_X				; get player's horizontal position
	cmp		#$9f
	bcs		load_playfield_ram
	inc		P0_X				; increment player's horizontal position
	jmp		load_playfield_ram

end_joystick

	;
	; load playfield data into RAM
	;
	;
	;
	; OPTIMIZATION OPPORTUNITY
	; the data set-up here only changes whilst scrolling
	;
load_playfield_ram		subroutine

	clc
	lda		#$04
	adc		TOP_FLOOR
	tay

	ldx		TOP_FLOOR_LINE		; if line 30 of the top floor isn't visible
	cpx		#$1e				; then start on the next floor down
	bpl		.2
	iny

.2
	ldx		#$04
.1
	stx		CURR_FLOOR



	lda		EL_STAT,y
	sta		CURR_EL

	and		#$01
	tax
	lda		EL_DATA_L,x
	ora		LEVEL_1_2R,y
	and		#$1f				; mask-out the P1-width definition!
	ldx		CURR_FLOOR
	sta		PF2_R_DATA,x

	lda		CURR_EL
	lsr
	sta		CURR_EL

	and		#$03
	tax
	lda		EL_DATA_M,x
	ora		LEVEL_1_1R,y
	ldx		CURR_FLOOR
	sta		PF1_R_DATA,x

	lda		CURR_EL
	lsr
	lsr
	sta		CURR_EL

	and		#$01
	tax
	lda		EL_DATA_L,x
	ora		LEVEL_1_0R,y
	ldx		CURR_FLOOR
	sta		PF0_R_DATA,x

	lda		CURR_EL
	lsr
	sta		CURR_EL

	and		#$03
	tax
	lda		EL_DATA_L,x
	ora		LEVEL_1_2L,y
	ldx		CURR_FLOOR
	sta		PF2_L_DATA,x

	lda		CURR_EL
	lsr
	lsr
	sta		CURR_EL

	tax
	lda		EL_DATA_M,x
	ora		LEVEL_1_1L,y
	ldx		CURR_FLOOR
	sta		PF1_L_DATA,x

	dey
	dex
	bpl		.1

	;
	; Calculate the starting movement value for the BALL (enemy) graphic.
	;
	; This needs to be done because all movement values for the enemy
	; are relative, but at the top-of-screen we may well start half-way-
	; down an enemy, so we need to add-up all skipped the movements.
	;
	ldx		TOP_FLOOR			; load the enemy data pointer
	lda		LEVEL1_ENEMY,x
	asl
	asl
	asl
	asl
	asl
	sta		ENEMY_DATA_PTR

	ldy		#$17
	cpy		TOP_FLOOR_LINE
	bmi		.no_adjustment_needed
	beq		.no_adjustment_needed

	lda		#$00
	sta		BALL_ADJUST			; we start with no adjustment

.adjust_next_line
	lda		(ENEMY_DATA_PTR),y
	and		#$f0				; we only want the upper nybble
	clc
	adc		BALL_ADJUST
	sta		BALL_ADJUST

	dey
	cpy		TOP_FLOOR_LINE
	bne		.adjust_next_line

.no_adjustment_needed

	;
	; Prepare the player graphic offsets for each floor
	;
	lda		P0_FRAME
	and		#$0c
	asl
	asl
	asl
	clc
	adc		#$80
	sta		PLAYER_OFFSET+2


;	lda		#$3f
;	sec
;	sbc		TOP_FLOOR_LINE
;	sta		PLAYER_OFFSET+2
;	sec
;	sbc		#$20
;	sta		PLAYER_OFFSET+3

	;
	; hang-about until the end of VBLANK
	;
.wait
	lda		INTIM
	bne		.wait

;
; KERNEL
;
start_kernel		subroutine

	sta		WSYNC
	sta		VBLANK				; turn off VBLANK
	sta		HMCLR
	sta		CXCLR

	lda		#PF_TB_COLOUR
	sta		COLUBK

	;
	; set-up our data references
	;
	lda		P0_Y
	sta		LINE

	lda		#PF_FLOOR_LINES
	sta		FRAME_LINE

	ldy		#$00				; set floor-data index
	sty		PF_DATA_INDEX

	sty		P1_DATA_PTR

	;
	; positioning player 0
	;
	ldx		P0_X				; get player's horizontal position
	lda		HOR_POS,x			; decode position
	sta		BUF1				; buffer player's decoded horizontal position

	and		#$0f				; keep only coarse horizontal position
	sta		BUF2				; put player's coarse horizontal position

	;
	; set the player's horizontal position
	;
	sta		WSYNC				; wait for leading edge of horizontal blank
	ldx		BUF1				; get player's decoded horizontal position
	ldy		BUF2

horzpos
	dey
	bpl		horzpos				; delay for coarse positioning

	;
	; reset player's position, and then apply fine positioning
	;
	sta		RESP0				; reset player 0 (non-zero page sta in original!)
	stx		HMP0				; horizontal motion player 0
	sta		WSYNC				; wait for leading edge of horizontal blank
	sta		HMOVE				; apply horizontal motion

	lda		#$00				; we don't want to move player 0 anymore!
	sta		HMP0

	;
	; load floor-item data pointer for top floor
	;
	ldx		TOP_FLOOR
	lda		LEVEL_1_0R,x
	asl
	asl
	asl
	asl
	asl
	sta		P1_DATA_PTR
	sta		COLUP1_DATA_PTR

	;
	; set NUSIZ1 (floor items)
	;
	lda		LEVEL_1_2R,x
	lsr
	lsr
	lsr
	lsr
	lsr
	sta		NUSIZ1

	;
	; position player 1 for top floor
	;
	sta		WSYNC

	nop
	nop
	nop
	nop
	nop
	nop
	nop
;	nop

	ldy		LEVEL_1_P1P,x
horzpos_p1_top
	dey
	bpl		horzpos_p1_top		; delay for coarse positioning
	sta		RESP1				; reset player 1

	;
	; position the ball for top floor
	;
	sta		WSYNC

	nop
	nop
	nop
	nop
	nop
	nop
	nop
;	nop

	ldy		ENEMY_POS,x
horzpos_ball_top
	dey
	bpl		horzpos_ball_top	; delay for coarse positioning
	sta		RESBL				; reset ball

	lda		ENEMY_POS_FINE,x
	sta		HMBL

	;
	; fine adjustment for both P1 and BALL graphics
	;
	sta		WSYNC
	sta		HMOVE

	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop

	lda		#$30
	sta		HMP1

	lda		BALL_ADJUST
	clc
	adc		#$30
	sta		HMBL

;	lda		#$02
;	sta		ENAM0

	;
	; load floor-item data pointer for top floor
	; (THIS_FLOOR shares memory with BALL_ADJUST)
	;
	lda		TOP_FLOOR
	sta		THIS_FLOOR

	;
	; do the movement and then clear the movement registers
	;
	sta		WSYNC
	sta		HMOVE
	sta		HMCLR

	;
	; set the background colour
	;
	lda		#PF_BG_COLOUR
	sta		COLUBK

	;
	; determine which part of the kernel we need to start at
	;
	lda		TOP_FLOOR_LINE		; set top floor-line
	tay							; (we want it in Y when entering the main loop)

	cmp		#$18
	bpl		use_jump_table
	jmp		line_n_entry

use_jump_table
	and		#$07
	tax

	lda		JUMP_TABLE_LSB,x	;
	sta		JUMP_POINT			;
	lda		JUMP_TABLE_MSB,x	;
	sta		JUMP_POINT+1		;

	jmp		(JUMP_POINT)		;

	; ***************************************************************
	; LINE 31
	; - set the player 0 pointer for this floor
	; - buffer player 0 graphics for lines 30 & 29
	;
line_31_entry
	lda		#$00
	tax

line_31
	sta		WSYNC				;
	sta		HMOVE
	sta		GRP0
	stx		GRP1

	;
	; set the player's data pointer
	;
	lda		THIS_FLOOR
	sec
	sbc		TOP_FLOOR
	tay

	lda		PLAYER_OFFSET,y
	sta		P0_DATA_PTR

	;
	; load the player (and player buffers)
	;
	ldy		#$1e
	lda		(P0_DATA_PTR),y
	sta		BUF0

	ldy		#$1d
	lda		(P0_DATA_PTR),y
	sta		BUF1

	ldy		#$1f
	lda		(P0_DATA_PTR),y

	ldx		#$00				; to clear GRP1 next line

	ldy		#PF_COLOUR
	sty		COLUPF

	;
	; is this the end?
	;
	dec		FRAME_LINE
	bne		line_30				;139
	jmp		start_statusbar

	; ***************************************************************
	; LINE 30
	; first (top) line of floor
	;
line_30_entry
	lda		#PF_COLOUR
	sta		COLUPF
	lda		#$00
	tax
	clc

line_30
	sta		WSYNC				;
	sta		HMOVE
	sta		GRP0				;-68
	stx		GRP1

	ldy		PF_DATA_INDEX		;-59

	;
	; load X with the floor index for line_29
	;
;	tya							;-38
;	adc		TOP_FLOOR			;-32
;	tax							;-23

	ldx		THIS_FLOOR
	nop
	nop



	lda		PF1_L_DATA,y		;-17
	sta		PF1					;- 5

	lda		PF2_L_DATA,y		;  4
	sta		PF2					; 16

	lda		PF0_R_DATA,y		; 25
	sta		PF0					; 37

	lda		PF1_R_DATA,y		; 46
	sta		PF1					; 58

	nop							; 67

	lda		PF2_R_DATA,y		; 73
	sta		PF2					; 85

	;
	; load player graphics into A for next line
	;
	lda		BUF0				; 94

	inc		PF_DATA_INDEX		;

	;
	; is this the end?
	;
	dec		FRAME_LINE
	bne		line_29				;
	jmp		start_statusbar

	; ***************************************************************
	; LINE 29
	; second (bottom) line of floor
	; - X must be already loaded with the floor-index
	;
line_29_entry
	lda		#PF_COLOUR
	sta		COLUPF
	lda		#$00
	ldx		TOP_FLOOR

line_29
	sta		WSYNC				;139 (from line_30), 142 (from jump table)
	sta		HMOVE
	sta		GRP0				;-68

	nop							;-59
	nop							;-53
	nop							;-47
	nop							;-41

	lda		#$00				;-29
	sta		PF0					;-23

	lda		LEVEL_1_1L,x		;-14
	sta		PF1					;- 2

	lda		LEVEL_1_2L,x		;  7
	sta		PF2					; 19

	lda		LEVEL_1_0R,x		; 28
	sta		PF0					; 40

	nop							;
	nop							; 49

	lda		LEVEL_1_1R,x		; 55
	sta		PF1					; 67

;	nop							; 76

;	lda		LEVEL_1_2R,x		; 82
;	sta		PF2					; 94


	lda		#$1f				; 	mask-out the P1-width definition!
	and		LEVEL_1_2R,x		; 
	sta		PF2					; 

	;
	; load player graphics into A for next line
	;
	lda		BUF1				;

	;
	; is this the end?
	;
	dec		FRAME_LINE
	bne		line_28				;
	jmp		start_statusbar

	; ***************************************************************
	; LINE 28
	; - clear the playfield graphic registers
	; - load the GRP1 data pointer
	;
line_28_entry
	lda		#$00
	ldx		TOP_FLOOR

line_28
	sta		WSYNC				;
	sta		HMOVE
	sta		GRP0				;-68

	;
	; clear playfield
	;
	lda		#$00				;
	sta		PF0					;
	sta		PF1
	sta		PF2

	sta		ENABL
;	sta		HMBL				; disabled to shorten line-time slightly

	;
	; load GRP1 data pointer
	;
	lda		LEVEL_1_0R,x
	asl
	asl
	asl
	asl
	asl
	sta		P1_DATA_PTR
	sta		COLUP1_DATA_PTR

	;
	; set NUSIZ1 (floor items)
	;
	lda		LEVEL_1_2R,x
	lsr
	lsr
	lsr
	lsr
	lsr
	sta		NUSIZ1

	;
	; load the player
	;
	ldy		#$1c
	lda		(P0_DATA_PTR),y

	;
	; is this the end?
	;
	dec		FRAME_LINE
	bne		line_27				;
	jmp		start_statusbar

	; ***************************************************************
	; LINE 27
	; - position player 1 graphic for this floor
	;
line_27_entry
	lda		#$00
	ldx		TOP_FLOOR

line_27
	sta		WSYNC				;
	sta		HMOVE
	sta		GRP0				;

	;
	; load the player
	;
	ldy		#$1b
	lda		(P0_DATA_PTR),y

	;
	; position player 1
	;
	ldy		LEVEL_1_P1P,x
horzpos_27
	dey
	bpl		horzpos_27			; delay for gross positioning
	sta		RESP1				; reset player 1

	;
	; is this the end?
	;
	dec		FRAME_LINE
	bne		line_26				;
	jmp		start_statusbar

	; ***************************************************************
	; LINE 26
	; - gross positioning of ball graphic
	;
line_26_entry
	lda		#$00
	ldx		TOP_FLOOR

line_26
	sta		WSYNC				;
	sta		HMOVE
	sta		GRP0				;

	;
	; load the player
	;
	ldy		#$1a
	lda		(P0_DATA_PTR),y

	;
	; position the ball
	;
	ldy		ENEMY_POS,x
horzpos_26
	dey
	bpl		horzpos_26			; delay for gross positioning
	sta		RESBL				; reset ball

	;
	; is this the end?
	;
	dec		FRAME_LINE
	bne		line_25				;
	jmp		start_statusbar

	; ***************************************************************
	; LINE 25
	; - load enemy graphics pointer (ball)
	; - set enemy colour (ball)
	; - fine positioning of ball graphic
	;
line_25_entry
	lda		#$00
	ldx		TOP_FLOOR

line_25
	sta		WSYNC				;
	sta		HMOVE
	sta		GRP0				;

	;
	; load the enemy graphics pointer
	;
	lda		LEVEL1_ENEMY,x
	asl
	asl
	asl
	asl
	asl
	sta		ENEMY_DATA_PTR

	;
	; set the enemy colour
	;
	lda		#ENEMY_COLOUR
	sta		COLUPF

	;
	; set enemy fine-positioning (movement)
	;
	lda		ENEMY_POS_FINE,x
	sta		HMBL

	;
	; load the player
	;
	ldy		#$19
	lda		(P0_DATA_PTR),y

	;
	; is this the end?
	;
	dec		FRAME_LINE
	bne		line_24				;
	jmp		start_statusbar

	; ***************************************************************
	; LINE 24
	;
line_24_entry
	lda		#$00

line_24
	sta		WSYNC				;
	sta		HMOVE
	sta		GRP0				;

	ldx		#$00				; to clear GRP1 next line

	;
	; clear enemy fine-positioning
	;
	stx		HMBL

	;
	; load the player
	;
	ldy		#$18
	lda		(P0_DATA_PTR),y

	ldy		#$17

	;
	; is this the end?
	;
	dec		FRAME_LINE
	bne		line_n				;
	jmp		start_statusbar

	; ***************************************************************
	; LINE n
	; - Y must hold the current floor-line number
	;
line_n_entry
	lda		#ENEMY_COLOUR
	sta		COLUPF
	lda		#$00
	tax

line_n
	sta		WSYNC				;
	sta		HMOVE				;-68
	sta		GRP0				;-59
	stx		GRP1				;-50

	lda		(COLUP1_DATA_PTR),y	;
	sta		COLUP1				;

	;
	; load the enemy
	;
	lda		(ENEMY_DATA_PTR),y	;-41
	sta		ENABL				;-26
	sta		HMBL				;-17
	asl							;- 8
	asl							;- 2
	sta		BUF0

	;
	; load the floor object
	;
	lda		(P1_DATA_PTR),y		; 13
	tax							; 28

	;
	; is this the end? (we're checking this earlier so CTRLPF can be set
	; further along the scan line - weirdness with a delay setting the
	; width of the ball!)
	;
	dec		FRAME_LINE			;
	beq		start_statusbar

	lda		#$04				; playfield must have priority, so the enemy
	ora		BUF0				;   droids pass infront of the objects
	sta		CTRLPF				;

	;
	; load the player
	;
	lda		(P0_DATA_PTR),y		;

	;
	; continue with our "end of line" duties
	;
	dey							;
	bpl		line_n				;

	;
	; if we run out of time in this section, we can shift "inc THIS_FLOOR"
	; to "line_31", after the WSYNC!
	;
	inc		THIS_FLOOR			;
	jmp		line_31

;
; STATUS BAR
;
start_statusbar		subroutine
	sta		WSYNC

	;
	; turn everything off
	;
	lda		#$00
	sta		PF0
	sta		COLUBK
	sta		GRP0
	sta		GRP1
	sta		PF1
	sta		ENABL
	sta		PF2

	sta		WSYNC
;	lda		#PF_SB_COLOUR
	lda		TEMP_COLOUR
	sta		COLUBK

	ldx		#$1d
.wsync
	sta		WSYNC
	dex
	bpl		.wsync

;
; OVERSCAN
;
start_overscan

	;
	; set timer for OVERSCAN
	;
	lda		#OVERSCAN_TIME
	sta		TIM64T

	ldy		#$02
	sty		VBLANK			; turn on VBLANK

	lda		#$00
	sta		COLUBK

	lda		#PF_SB_COLOUR
	sta		TEMP_COLOUR

	;
	; check for collisions
	;
collision_check		subroutine
	lda		#$40					; check P0/BL bit (D6)
	bit		CXP0FB					; P0/PF & P0/BL
	beq		.1

	lda		#$0e
	sta		TEMP_COLOUR
	jmp		collision_check_end
.1
	lda		#$80					; check P0/P1 bit (D7)
	bit		CXPPMM					; P0/P1 & M0/M1
	beq		collision_check_end

	lda		#$3e
	sta		TEMP_COLOUR
collision_check_end

	;
	; update enemy positions
	;
update_enemy		subroutine
	ldy		#$1f
.1
	lda		ENEMY_POS_FINE,y
	clc
	adc		ENEMY_VECTOR,y
	tax

	cmp		RAM,x					; Supercharger RAM access
	lda		ENEMY_POS_FINE,y		; stores X at ENEMY_POS_FINE + Y

	;
	; if fine position is $8x, then we've wrapped, so we
	; need to modify the gross position
	;
	txa
	and		#$f0					; check if fine position = $8x
	cmp		#$80
	bne		.3

	lda		ENEMY_VECTOR,y			; determine if we're moving left or right
	bmi		enemy_move_right

	ldx		ENEMY_POS,y				; okay, so we need to decrement
	dex
	bpl		.2

	ldx		#$08
.2
	cmp		RAM,x					; Supercharger RAM access
	lda		ENEMY_POS,y				; stores X at ENEMY_POS + Y


	; -------------------------------------------------------------------
	; adjust fine positioning
	; we want to change this so RAM is only written ONCE!
	;
	lda		ENEMY_POS_FINE,y
	clc
	adc		#$10
	tax
	cmp		RAM,x					; Supercharger RAM access
	lda		ENEMY_POS_FINE,y		; stores X at ENEMY_POS_FINE + Y
	; -------------------------------------------------------------------

	;
	; check position with left extent; if equal, then invert vector
	;
	lda		ENEMY_LEFT,y
	cmp		ENEMY_POS,y
	bne		.21

	lda		#$00					; 0 - vector = -vector
	sec
	sbc		ENEMY_VECTOR,y
	tax

	cmp		RAM,x					; Supercharger RAM access
	lda		ENEMY_VECTOR,y			; stores X at ENEMY_VECTOR + Y

.21
	jmp		.3

enemy_move_right

	ldx		ENEMY_POS,y				; okay, so we need to increment
	inx
	cpx		#$09
	bne		.4

	ldx		#$00
.4
	cmp		RAM,x					; Supercharger RAM access
	lda		ENEMY_POS,y				; stores X at ENEMY_POS + Y


	; -------------------------------------------------------------------
	; adjust fine positioning
	; we want to change this so RAM is only written ONCE!
	;
	lda		ENEMY_POS_FINE,y
	sec
	sbc		#$10
	tax
	cmp		RAM,x					; Supercharger RAM access
	lda		ENEMY_POS_FINE,y		; stores X at ENEMY_POS_FINE + Y
	; -------------------------------------------------------------------
	
	;
	; check position with right extent; if equal, then invert vector
	;
	lda		ENEMY_RIGHT,y
	cmp		ENEMY_POS,y
	bne		.3

	lda		#$00					; 0 - vector = -vector
	sec
	sbc		ENEMY_VECTOR,y
	tax

	cmp		RAM,x					; Supercharger RAM access
	lda		ENEMY_VECTOR,y			; stores X at ENEMY_VECTOR + Y

.3

	dey
	bmi		update_enemy_end
	jmp		.1
update_enemy_end

	;
	; read console switches
	;
switch_read
	lda		SWCHB
	lsr
	bcc		switch_reset
;	lsr
;	bcc		switch_select
	jmp		switch_read_end

switch_reset
	jmp		ENTER

switch_select
;	jmp		supercharger_load

switch_read_end

	;
	; wait until the end of overscan
	;
overscan_wait
	lda		INTIM
	bne		overscan_wait

	jmp		start_vblank






	org		$f800

LEVEL_1_1L
	.byte	$8F,$0F,$8F,$F8,$00,$18,$00,$00
	.byte	$08,$00,$80,$88,$08,$8F,$0F,$8F
	.byte	$F8,$00,$18,$00,$00,$08,$00,$80
	.byte	$88,$08,$F8,$00,$18,$00,$00,$F8

LEVEL_1_2L
	.byte	$11,$01,$FF,$FF,$80,$11,$02,$04
	.byte	$11,$00,$00,$00,$FF,$11,$01,$FF
	.byte	$FF,$80,$11,$00,$00,$11,$00,$00
	.byte	$00,$FF,$FF,$80,$11,$00,$00,$FF

	;
	; D7...D4 - PF0 data
	; D3...D0 - floor-item definition
	;
LEVEL_1_0R
	.byte	$01,$06,$F3,$14,$F5,$10,$01,$02
	.byte	$13,$04,$05,$00,$F1,$02,$03,$F4
	.byte	$15,$F0,$11,$02,$03,$14,$05,$00
	.byte	$01,$F2,$13,$F4,$15,$00,$01,$F2

LEVEL_1_1R
	.byte	$F8,$88,$80,$8F,$80,$88,$00,$00
	.byte	$88,$00,$00,$88,$8F,$F8,$88,$80
	.byte	$8F,$80,$88,$00,$00,$88,$00,$00
	.byte	$88,$8F,$8F,$80,$8F,$00,$00,$8F

	;
	; D7...D5 - NUSIZ0 data (just PF0)
	; D4...D0 - PF2 data
	;
LEVEL_1_2R
;	.byte	$11,$1F,$00,$1F,$00,$1F,$00,$00
;	.byte	$11,$00,$1F,$1F,$1F,$1F,$0B,$14
;	.byte	$0B,$00,$1F,$00,$00,$1F,$00,$1F
;	.byte	$1F,$1F,$1F,$00,$11,$00,$00,$11

	.byte	$31,$9F,$00,$5F,$00,$3F,$e0,$00
	.byte	$11,$00,$1F,$1F,$1F,$1F,$0B,$14
	.byte	$0B,$00,$1F,$00,$00,$1F,$00,$1F
	.byte	$1F,$1F,$1F,$00,$11,$00,$00,$11

LEVEL_1_EL
	.byte	$80,$02,$04,$40,$00,$04,$00,$00
	.byte	$02,$00,$00,$82,$40,$00,$02,$04
	.byte	$40,$00,$04,$00,$00,$02,$00,$00
	.byte	$02,$40,$40,$00,$04,$00,$00,$02

LEVEL_1_P1P
;	.byte	$01,$04,$02,$04,$07,$00,$00,$00
;	.byte	$00,$07,$07,$02,$01,$01,$03,$06
;	.byte	$04,$07,$00,$00,$07,$00,$07,$07
;	.byte	$04,$02,$04,$06,$00,$00,$02,$00

	.byte	$00,$01,$02,$03,$04,$05,$06,$08
	.byte	$00,$01,$02,$03,$04,$05,$06,$08
	.byte	$00,$01,$02,$03,$04,$05,$06,$08
	.byte	$00,$01,$02,$03,$04,$05,$06,$08

	;
	; Floor's enemy specification
	;
	; D7 D6 D5 D4 D3 D2 D1 D0
	;  x  x  x  x              Handler index
	;              x  x  x  x  Graphic data
	;
LEVEL1_ENEMY
	.byte	$01,$11,$22,$01,$11,$22,$01,$11
	.byte	$01,$11,$22,$01,$11,$22,$01,$11
	.byte	$01,$11,$22,$01,$11,$22,$01,$11
	.byte	$01,$11,$22,$01,$11,$22,$01,$11

	org		$f900

ITEM_DATA
	;
	; nothing
	;
	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00

	;
	; computer console
	;
	.byte	%10000001
	.byte	%10000001
	.byte	%10000001
	.byte	%10000001
	.byte	%11111111
	.byte	%11111111
	.byte	%11111111
	.byte	%01111110

	.byte	%00011000
	.byte	%00111100
	.byte	%00100100
	.byte	%00100100
	.byte	%00100100
	.byte	%00111100
	.byte	%00000000
	.byte	%00000000

	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

	;
	; bookshelf
	;
	.byte	%10000001
	.byte	%11111111
	.byte	%10000001
	.byte	%10000001
	.byte	%10000001
	.byte	%11111111
	.byte	%10000001
	.byte	%10000001

	.byte	%10000001
	.byte	%11111111
	.byte	%10000001
	.byte	%10000001
	.byte	%10000001
	.byte	%11111111
	.byte	%10000001
	.byte	%10000001

	.byte	%10000001
	.byte	%11111111
	.byte	%10000001
	.byte	%10000001
	.byte	%10000001
	.byte	%11111111
	.byte	%00000000
	.byte	%00000000

	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

	;
	; wall-mounted telephone
	;
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

	.byte	%00000000
	.byte	%00010000
	.byte	%00010000
	.byte	%00010000
	.byte	%00010000
	.byte	%00010000
	.byte	%00010000
	.byte	%11111110

	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

	;
	; clock
	;
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

	.byte	%00000000
	.byte	%00111000
	.byte	%01000100
	.byte	%01110100
	.byte	%01010100
	.byte	%01010100
	.byte	%00111000
	.byte	%00000000

	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

	;
	; painting
	;
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

	.byte	%11111111
	.byte	%10000001
	.byte	%10000001
	.byte	%10000001
	.byte	%10000001
	.byte	%10000001
	.byte	%10000001
	.byte	%11111111

	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

	;
	; globe
	;
	.byte	%00111000
	.byte	%00010000
	.byte	%00011000
	.byte	%00111000
	.byte	%01111100
	.byte	%01111100
	.byte	%11111110
	.byte	%11111110

	.byte	%11111110
	.byte	%11111110
	.byte	%01111100
	.byte	%01111100
	.byte	%00111000
	.byte	%01000000
	.byte	%00100000
	.byte	%00000000

	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

	org		$fa00

ITEM_COLOUR_DATA
	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00

	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$04,$06,$06,$06,$08,$00,$00,$00
	.byte	$0e,$0e,$0e,$0e,$0e,$1e,$2e,$3e
	.byte	$00,$00,$00,$00,$00,$00,$00,$00

	.byte	$4e,$5e,$6e,$7e,$8e,$9e,$ae,$be
	.byte	$ce,$de,$ee,$fe,$0e,$1e,$2e,$3e
	.byte	$4e,$5e,$6e,$7e,$8e,$9e,$ae,$be
	.byte	$00,$00,$00,$00,$00,$00,$00,$00

	.byte	$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
	.byte	$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
	.byte	$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
	.byte	$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e

	.byte	$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
	.byte	$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
	.byte	$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
	.byte	$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e

	;
	; painting
	;
	.byte	$0e,$0e,$0e,$0e,$0e,$0e,$0e,$17
	.byte	$17,$17,$17,$17,$17,$17,$17,$0e
	.byte	$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
	.byte	$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e

	;
	; globe
	;
	.byte	$00,$00,$84,$84,$86,$86,$88,$88
	.byte	$8a,$8a,$8c,$8c,$00,$00,$00,$0e
	.byte	$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
	.byte	$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e

	.byte	$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
	.byte	$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
	.byte	$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
	.byte	$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e

	org		$fb00
	;
	; Enemy definitions
	;
	; Enemies are constructed from the ball graphic, by controlling the width,
	; position and enabled status.  These are packed into a single byte per
	; line, for 32 lines (not all lines are used).
	;
	; The definitions are read in reverse order.  The movement is effective
	; on the *following* line (all movements need to be offset by 1 line).
	; So is the width
	;
	; D7 D6 D5 D4 D3 D2 D1 D0
	;  x  x  x  x              Movement (HMBL)
	;              x  x        Width    (CTRLPF)
	;                    x     Enable   (ENABL)
	;
ENEMY_DATA
	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00

	.byte	$02,$12,$12,$02,$02,$f2,$f2,$02
	.byte	$00,$00,$02,$f6,$ea,$2e,$1a,$04
	.byte	$00,$00,$02,$f6,$ea,$2e,$1a,$c4
	.byte	$00,$00,$00,$00,$00,$00,$00,$00

	.byte	$02,$12,$12,$02,$02,$f2,$f2,$02
	.byte	$00,$00,$02,$f6,$ea,$2e,$1a,$04
	.byte	$00,$00,$00,$00,$00,$00,$00,$c0
	.byte	$00,$00,$00,$00,$00,$00,$00,$00

ENEMY_POS
	.byte	$04,$05,$06,$08,$00,$01,$02,$03
	.byte	$04,$05,$06,$08,$00,$01,$02,$03
	.byte	$04,$04,$04,$04,$04,$04,$04,$04
	.byte	$04,$04,$04,$04,$04,$04,$04,$04

ENEMY_POS_FINE
	.byte	$74,$75,$76,$78,$70,$71,$72,$73
	.byte	$84,$85,$86,$88,$80,$81,$82,$83
	.byte	$74,$64,$54,$44,$34,$24,$14,$04
	.byte	$f4,$e4,$d4,$c4,$b4,$a4,$94,$84

ENEMY_VECTOR
	.byte	$04,$05,$06,$08,$00,$01,$02,$03
	.byte	$ff,$fe,$fd,$fc,$fb,$fa,$f9,$f8
	.byte	$04,$04,$04,$04,$04,$04,$04,$04
	.byte	$04,$04,$04,$04,$04,$04,$04,$04

ENEMY_LEFT
	.byte	$04,$01,$00,$02,$01,$01,$02,$03
	.byte	$04,$05,$04,$03,$01,$01,$02,$03
	.byte	$04,$05,$06,$02,$01,$01,$02,$03
	.byte	$04,$05,$05,$01,$01,$01,$02,$03

ENEMY_RIGHT
	.byte	$08,$07,$03,$07,$04,$05,$06,$07
	.byte	$07,$08,$07,$08,$04,$05,$06,$07
	.byte	$08,$07,$08,$07,$04,$05,$06,$07
	.byte	$07,$08,$07,$08,$04,$05,$06,$07

	org		$fc00

NEXT_LOAD_ID
	.byte	$01

	org		$fd00

PLAYER
;	.byte	$1f,$1e,$1d,$1c,$1b,$1a,$19,$18
;	.byte	$17,$16,$15,$14,$13,$12,$11,$10
;	.byte	$0f,$0e,$0d,$0c,$0b,$0a,$09,$08
;	.byte	$07,$06,$05,$04,$03,$02,$01,$00

	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00

	.byte	%11111111
	.byte	%00100010
	.byte	%00100010
	.byte	%00100010
	.byte	%00010100
	.byte	%00011100
	.byte	%01011101
	.byte	%01001001
	.byte	%00101010
	.byte	%00111110
	.byte	%00011100
	.byte	%00001000
	.byte	%00001000
	.byte	%00001000
	.byte	%00001000
	.byte	%00000000
	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00

;	.byte	$1f,$1e,$1d,$1c,$1b,$1a,$19,$18
;	.byte	$17,$16,$15,$14,$13,$12,$11,$10
;	.byte	$0f,$0e,$0d,$0c,$0b,$0a,$09,$08
;	.byte	$07,$06,$05,$04,$03,$02,$01,$00

	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00

	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00

	;
	; Running 1
	;
	.byte	$04,$04,$04,$44,$22,$12,$12,$14
	.byte	$1c,$08,$08,$18,$1c,$08,$08,$08
	.byte	$0c,$0c,$0c,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00

	;
	; Running 2
	;
	.byte	$10,$10,$10,$18,$08,$0c,$0c,$08
	.byte	$08,$08,$0c,$0c,$08,$08,$08,$08
	.byte	$0c,$0c,$0c,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00

	;
	; Running 3
	;
	.byte	$40,$40,$24,$24,$22,$12,$14,$1c
	.byte	$08,$08,$18,$1c,$08,$08,$08,$08
	.byte	$0c,$0c,$0c,$00,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00

	;
	; Running 4
	;
	.byte	$00,$02,$02,$82,$42,$22,$24,$14
	.byte	$1c,$0c,$08,$18,$2c,$1a,$18,$08
	.byte	$08,$0c,$0c,$0c,$00,$00,$00,$00
	.byte	$00,$00,$00,$00,$00,$00,$00,$00

	org		$fe00

PLAYER_JUMP
	;
	; Player jumping for eight frames (256 bytes)
	;
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000
	.byte	%00000000

;
; Sprite positioning LUT
;
	org		$ff00

HOR_POS
	.byte	$A0,$90,$71,$61,$51,$41,$31,$21
	.byte	$11,$01,$F1,$E1,$D1,$C1,$B1,$A1
	.byte	$91,$72,$62,$52,$42,$32,$22,$12
	.byte	$02,$F2,$E2,$D2,$C2,$B2,$A2,$92
	.byte	$73,$63,$53,$43,$33,$23,$13,$03
	.byte	$F3,$E3,$D3,$C3,$B3,$A3,$93,$74
	.byte	$64,$54,$44,$34,$24,$14,$04,$F4
	.byte	$E4,$D4,$C4,$B4,$A4,$94,$75,$65
	.byte	$55,$45,$35,$25,$15,$05,$F5,$E5
	.byte	$D5,$C5,$B5,$A5,$95,$76,$66,$56
	.byte	$46,$36,$26,$16,$06,$F6,$E6,$D6
	.byte	$C6,$B6,$A6,$96,$77,$67,$57,$47
	.byte	$37,$27,$17,$07,$F7,$E7,$D7,$C7
	.byte	$B7,$A7,$97,$78,$68,$58,$48,$38
	.byte	$28,$18,$08,$F8,$E8,$D8,$C8,$B8
	.byte	$A8,$98,$79,$69,$59,$49,$39,$29
	.byte	$19,$F9,$E9,$D9,$C9,$B9,$A9,$99
	.byte	$7A,$6A,$5A,$4A,$3A,$2A,$1A,$0A
	.byte	$FA,$EA,$DA,$CA,$BA,$AA,$9A,$7B
	.byte	$6B,$5B,$4B,$3B,$2B,$1B,$0B,$FB
	.byte	$EB,$DB,$CB,$AB,$9B,$7C,$6C,$5C

;
; elevator platform PF data
;
EL_DATA_M									; most-significant-bit first
	.byte	$00,$07,$70,$77

EL_DATA_L									; least-significant-bit first
	.byte	$00,$e0,$0e,$ee



JUMP_TABLE_LSB
	.byte	<line_24_entry
	.byte	<line_25_entry
	.byte	<line_26_entry
	.byte	<line_27_entry
	.byte	<line_28_entry
	.byte	<line_29_entry
	.byte	<line_30_entry
	.byte	<line_31_entry

JUMP_TABLE_MSB
	.byte	>line_24_entry
	.byte	>line_25_entry
	.byte	>line_26_entry
	.byte	>line_27_entry
	.byte	>line_28_entry
	.byte	>line_29_entry
	.byte	>line_30_entry
	.byte	>line_31_entry

;
; RESTART VECTOR
;
	org	$FFFC

	.word	ENTER		; Cold start & reset vector
	.word	ENTER		; IRQ vector (and BRK vector) (not used)
