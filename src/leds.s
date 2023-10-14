.segment "LEDS"

; ----------------------------------------------------------------------------------------------------

.enum LEDS_LED
		led_drive_r_lefthalf	= 128+0
		led_drive_g_lefthalf	= 128+1
		led_drive_b_lefthalf	= 128+2

		led_drive_r_righthalf	= 128+3
		led_drive_g_righthalf	= 128+4
		led_drive_b_righthalf	= 128+5

		led_power_r_lefthalf	= 128+6
		led_power_g_lefthalf	= 128+7
		led_power_b_lefthalf	= 128+8

		led_power_r_righthalf	= 128+9
		led_power_g_righthalf	= 128+10
		led_power_b_righthalf	= 128+11
.endenum

.macro LEDS_SETINDEXANDLEVEL
		stx $d61d ; x = index
		sta $d61e ; a = level
.endmacro

leds_init

		ldx #%11111111										; enable software control of leds and make sure no led is selected
		stx $d61d

		sta $d61e											; set level

		lda #LEDS_LED::led_drive_r_lefthalf
		sta $d61d
		jsr leds_wait
		lda #LEDS_LED::led_drive_g_lefthalf
		sta $d61d
		jsr leds_wait
		lda #LEDS_LED::led_drive_b_lefthalf
		sta $d61d
		jsr leds_wait
		lda #LEDS_LED::led_drive_r_righthalf
		sta $d61d
		jsr leds_wait
		lda #LEDS_LED::led_drive_g_righthalf
		sta $d61d
		jsr leds_wait
		lda #LEDS_LED::led_drive_b_righthalf
		sta $d61d
		jsr leds_wait

		lda #LEDS_LED::led_power_r_lefthalf
		sta $d61d
		jsr leds_wait
		lda #LEDS_LED::led_power_g_lefthalf
		sta $d61d
		jsr leds_wait
		lda #LEDS_LED::led_power_b_lefthalf
		sta $d61d
		jsr leds_wait
		lda #LEDS_LED::led_power_r_righthalf
		sta $d61d
		jsr leds_wait
		lda #LEDS_LED::led_power_g_righthalf
		sta $d61d
		jsr leds_wait
		lda #LEDS_LED::led_power_b_righthalf
		sta $d61d
		jsr leds_wait

		rts

; ----------------------------------------------------------------------------------------------------

leds_wait
		ldy #$00
		ldx #$00
:		inx
		bne :-
		iny
		bne :-
		rts

; ----------------------------------------------------------------------------------------------------
