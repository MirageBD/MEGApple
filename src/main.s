.define screen1			$9c00							; 80*30*2 =  4800 = $12c0

.define screenchars1	$b000							; buffer where image gets built/filled - 80*30*8 = 19200 = $4b00
.define screenchars0	$10000							; final backbuffer - 80*30*8 = 19200 = $4b00

.define zpcol			$90
.define zpchars			$94

; ----------------------------------------------------------------------------------------------------

.segment "INTROCHARS"
		.incbin "../bin/bitmap_chars0.bin"

.segment "PALETTE"
palette
		.incbin "../bin/bitmap_pal0.bin"

.segment "SPRITES"
sprites
		.incbin "../bin/m65sprites_sprites0.bin"

.segment "SPRITEPAL"
spritepal
		.incbin "../bin/m65sprites_pal0.bin"

.segment "MAIN"

entry_main

		sei

		lda #$35
		sta $01

		lda #$00
		sta $d020
		sta $d021
		sta $d022
		sta $d023

		lda #%10000000									; Clear bit 7 - HOTREG
		trb $d05d

		lda #$00										; unmap
		tax
		tay
		taz
		map
		eom

		lda #$47										; enable C65GS/VIC-IV IO registers
		sta $d02f
		lda #$53
		sta $d02f
		eom

		lda #65											; enable 40MHz
		sta $00

		lda #%11111000									; unmap c65 roms $d030 by clearing bits 3-7
		trb $d030

		lda #%10000000									; force NTSC mode, because we want to be running at 30FPS
		tsb $d06f										; set bit 7 for NTSC  ; tsb $d06f

		lda #$05										; enable Super-Extended Attribute Mode by asserting the FCLRHI and CHR16 signals - set bits 2 and 0 of $D054.
		sta $d054

		lda #%10100000									; Clear bit7=40 column, bit5=disable ...?
		trb $d031

		lda #80											; set to 80 for etherload
		sta $d05e

		lda #$01										; Y Position Where Character Display Starts ($D04E LSB, 0–3 of $D04F MSB). CAN'T BE 0!!!
		sta $d04e

		lda #30											; set number of rows
		sta $d07b

		lda #0*8+2										; reposition start of top border to what's juuuuust visible on my monitor
		sta $d048
		lda #$08										; reposition start of bottom border to what's juuuuust visible on my monitor
		sta $d04a
		lda #$02
		sta $d04b

		lda #$50										; set TEXTXPOS to same as SDBDRWDLSB
		sta $d04c

		lda #40*2										; logical chars per row
		sta $d058
		lda #$00
		sta $d059

		lda #<.loword(SAFE_COLOR_RAM)
		sta zpcol+0
		lda #>.loword(SAFE_COLOR_RAM)
		sta zpcol+1
		lda #<.hiword(SAFE_COLOR_RAM)
		sta zpcol+2
		lda #>.hiword(SAFE_COLOR_RAM)
		sta zpcol+3

		ldz #$00										; fill first two bytes of colour ram
		lda #%00000000
		sta [zpcol],z
		lda #%00000000									; 00001 for multicolour, 001 for white (dark gray in new palette)
		inz
		sta [zpcol],z

		DMA_RUN_JOB clearcolorramjob					; then copy source to dest+2 to repeat the two bytes over and over

		lda $d070										; select mapped bank with the upper 2 bits of $d070
		and #%00111111
		sta $d070

		ldx #$00										; set bitmap palette
:		lda palette+$0000,x
		sta $d100,x
		lda palette+$0100,x
		sta $d200,x
		lda palette+$0200,x
		sta $d300,x
		inx
		bne :-

		lda $d070
		and #%11001111									; clear bits 4 and 5 (BTPALSEL) so bitmap uses palette 0
		sta $d070

		lda #<$0800										; set (offset!) pointer to colour ram
		sta $d064
		lda #>$0800
		sta $d065

		lda #<.loword(screen1)							; set pointer to screen ram
		sta $d060
		lda #>.loword(screen1)
		sta $d061
		lda #<.hiword(screen1)
		sta $d062
		lda #>.hiword(screen1)
		sta $d063

		lda #$00										; fill screenmem linearly vertically
		sta screenrow
		sta screencolumn

		ldx #<(screenchars0 / 64)
		ldy #>(screenchars0 / 64)

put10	stx screen1+0
put11	sty screen1+1

		clc
		txa
		adc #$01
		tax
		tya
		adc #$00
		tay

		clc
		lda put10+1
		adc #40*2
		sta put10+1
		lda put10+2
		adc #0
		sta put10+2

		clc
		lda put11+1
		adc #40*2
		sta put11+1
		lda put11+2
		adc #0
		sta put11+2

		inc screenrow
		lda screenrow
		cmp #30
		bne put10

		lda #0
		sta screenrow
		inc screencolumn
		inc screencolumn
		lda screencolumn
		cmp #80
		beq endscreenplot1

		lda #>screen1
		sta put10+2
		sta put11+2
		clc
		lda screencolumn
		sta put10+1
		adc #$01
		sta put11+1

		jmp put10

endscreenplot1

		lda #$7f										; disable CIA interrupts
		sta $dc0d
		sta $dd0d
		lda $dc0d
		lda $dd0d

		lda #$00										; disable IRQ raster interrupts because C65 uses raster interrupts in the ROM
		sta $d01a

		lda #$fe										; setup IRQ interrupt
		sta $d012
		lda #<introirq
		sta $fffe
		lda #>introirq
		sta $ffff

		lda #$01										; ACK
		sta $d01a

		cli

		ldx #$00										; set filename for music sample, open file and get all sectors and copy to attic ram
:		lda musicfile,x
		beq :+
		sta sdc_transferbuffer,x
		inx
		bra :-
:		sta sdc_transferbuffer,x

		jsr sdc_openfile
:		jsr sdc_readsector
		bcc :+
		jsr sdc_copytoattic
		inc sectorcount+0
		bne :-
		inc sectorcount+1
		bra :-
:		lda #$35
		sta $01
		jsr sdc_closefile

		ldx #$00										; set filename for video frames, open file and get first sector
:		lda framefile,x
		beq :+
		sta sdc_transferbuffer,x
		inx
		bra :-
:		sta sdc_transferbuffer,x

		jsr sdc_openfile
		jsr sdc_readsector

		sei

		lda #$35
		sta $01

:		bit $d011
		bpl :-

		lda #$f0
:		cmp $d012
		bne :-

		lda #$00										; blank screen while setting up
		sta $d011

		lda #$01										; enable 16 bit char ptrs (bit 0), but leave full colour off (bit 2)
		sta $d054

		lda #%10000000									; Set bit7=80 column
		tsb $d031

		lda #80*2										; logical chars per row
		sta $d058
		lda #$00
		sta $d059

		lda #$18										; enable multicolour (yes, this is needed, don't remove!!!)
		sta $d016

		ldz #$00										; fill first two bytes of colour ram
		lda #%00000000
		sta [zpcol],z
		lda #%00001011									; 00001 for multicolour, 001 for white (dark gray in new palette)
		inz
		sta [zpcol],z

		DMA_RUN_JOB clearcolorramjob					; then copy source to dest+2 to repeat the two bytes over and over
		DMA_RUN_JOB clearcharmemjob

		lda #<.loword(screenchars0)						; set pointer to chargen
		sta $d068
		lda #>.loword(screenchars0)
		sta $d069
		lda #<.hiword(screenchars0)
		sta $d06a

		lda #$00										; fill screenmem linearly vertically
		sta screenrow
		sta screencolumn

		ldx #0
		ldy #0

put20	stx screen1+0
put21	sty screen1+1

		clc
		txa
		adc #$01
		tax
		tya
		adc #$00
		tay

		clc
		lda put20+1
		adc #80*2
		sta put20+1
		lda put20+2
		adc #0
		sta put20+2

		clc
		lda put21+1
		adc #80*2
		sta put21+1
		lda put21+2
		adc #0
		sta put21+2

		inc screenrow
		lda screenrow
		cmp #30
		bne put20

		lda #0
		sta screenrow
		inc screencolumn
		inc screencolumn
		lda screencolumn
		cmp #160
		beq endscreenplot2

		lda #>screen1
		sta put20+2
		sta put21+2
		clc
		lda screencolumn
		sta put20+1
		adc #$01
		sta put21+1

		jmp put20

endscreenplot2

		jsr mpInit

		; ---------------------------------------------- sprite setup


		lda $d070										; select mapped bank with the upper 2 bits of $d070
		and #%00111111
		ora #%01000000									; select palette 01
		sta $d070

		ldx #$00										; set sprite palette - each sprite has a 16 colour palette
:		lda spritepal+0*$0100,x
		sta $d100,x
		sta $d110,x
		sta $d120,x
		sta $d130,x
		lda spritepal+1*$0100,x
		sta $d200,x
		sta $d210,x
		sta $d220,x
		sta $d230,x
		lda spritepal+2*$0100,x
		sta $d300,x
		sta $d310,x
		sta $d320,x
		sta $d330,x
		inx
		cpx #$10
		bne :-

		lda #$00										; set transparent colours
		sta $d027
		sta $d028
		sta $d029
		sta $d02a

		lda $d070
		and #%11110011									; set sprite palette to 01
		ora #%00000100
		sta $d070

		lda #<sprptrs									; tell VIC-IV where the sprite pointers are (SPRPTRADR)
		sta $d06c
		lda #>sprptrs
		sta $d06d
		lda #(sprptrs & $ff0000) >> 16					; SPRPTRBNK
		sta $d06e

		lda #%10000000									; tell VIC-IV to expect two bytes per sprite pointer instead of one
		tsb $d06e										; do this after setting sprite pointer address, because that uses $d06e as well

		lda #%00001111
		sta $d015
		lda #%11111111
		sta $d05f										; Sprite H640 X Super-MSBs
		lda #$00
		sta $d010

		lda #0
		sta $d01b										; sprite background priority

		lda #%11111111
		sta $d076										; enable SPRENVV400 for this sprite
		lda #%11111111
		sta $d057										; enable 64 pixel wide sprites. 16 pixels if in Full Colour Mode
		lda #%11111111
		sta $d06b										; enable Full Colour Mode (SPR16EN)
		lda #%11111111
		sta $d055										; sprite height enable (SPRHGTEN)
		lda #%00010000
		tsb $d054										; enable SPR640 for all sprites

		lda #128										; set sprite height to 128 pixels (SPRHGHT) for sprites that have SPRHGTEN enabled (sample sprites)
		sta $d056

		lda #%00000000									; disable stretch
		sta $d017
		sta $d01d

		lda #$a8
		sta $d000
		sta $d004
		lda #$b8
		sta $d002
		sta $d006
		lda #$0e
		sta $d001
		sta $d003
		lda #$0e+128
		sta $d005
		sta $d007

		; ---------------------------------------------- sprite setup end

		lda #$7f										; disable CIA interrupts
		sta $dc0d
		sta $dd0d
		lda $dc0d
		lda $dd0d

		lda #$00										; disable IRQ raster interrupts because C65 uses raster interrupts in the ROM
		sta $d01a

		lda #$fe										; setup IRQ interrupt
		sta $d012
		lda #<irq1
		sta $fffe
		lda #>irq1
		sta $ffff

		lda #$01										; ACK
		sta $d01a

		cli
		
loop
		lda $d020
		jmp loop

; ----------------------------------------------------------------------------------------------------

.align 256

introirq
		pha
		phx
		phy

		lda sectorcount+1
		lsr
		clc
		adc #$01
		cmp #19
		bmi :+
		lda #18
:		sta loadbar

		ldx #$00

:		txa
		asl
		tay
		lda #<(screenchars0 / 64 + 20*30 + 7)
		sta screen1+2*(25*40+11)+0,y
		lda #>(screenchars0 / 64 + 20*30 + 7)
		sta screen1+2*(25*40+11)+1,y

		inx
		cpx loadbar
		bne :-

		ply
		plx
		pla
		asl $d019
		rti

; ----------------------------------------------------------------------------------------------------

.macro DEBUGTIME color
.scope
		lda showrastertime
		beq :+
		lda #color
		sta $d020
:		
.endscope
.endmacro

.align 256

irq1
		pha

		lda #$00
		sta $d020
		lda #$01
		sta $d022
		lda #$02
		sta $d023

		lda #$1b
		sta $d011

		;lda #%111111111
		;sta $d61d											; bit 7 = KEYLEDENA, bit 0-6 = KEYLEDREG
		;lda framelo
		;sta $d61e											; KEYLEDVAL

		lda endofframes
		beq playframes

		inc framelo											; when we've reached the end wait a while before restarting
		bne :+
		inc framehi
		lda framehi
		cmp #$a0
		bne :+
		lda #$00
		sta endofframes
		sta framelo
		sta framehi
:		jmp irqfinalize

playframes

:		lda framelo
		and #1
		beq evenframe

oddframe

		DEBUGTIME $6f
		lda xorfill
		beq :+
		jsr eorfill

:		jmp endirq

evenframe

		lda #$35
		sta $01

		DEBUGTIME $c0
		DMA_RUN_JOB copycharmemjob

		DEBUGTIME $2d ; yellow
		DMA_RUN_JOB clearcharmemjob

		DEBUGTIME $60 ; pink
		jsr ploteorchars

endirq

		DEBUGTIME $d6 ; dark blue

		lda sampletrigger
		beq :+
		jsr rbGetSample
		jmp :++

:		lda #$00
		sta rbChannel
		jsr rbUpdate
:		lda sampletrigger
		beq :+
		lda #$00
		sta sampletrigger

		lda #$00
		sta mpChannel
		jsr mpPlaySample
		lda #$01
		sta mpChannel
		jsr mpPlaySample
		lda #$02
		sta mpChannel
		jsr mpPlaySample
		lda #$03
		sta mpChannel
		jsr mpPlaySample
:

		inc framelo
		lda framelo
		bne :+
		inc framehi
:		
		lda framehi
		cmp #>(2*6550)
		;cmp #>(2*200)
		bne irqfinalize
		lda framelo
		cmp #<(2*6550)
		;cmp #<(2*200)
		bne irqfinalize

		; --------------------------------------------

		; END OF FRAMES!!!
		lda #$01
		sta endofframes

		lda #$00
		sta framehi
		lda #$00
		sta framelo

		lda #$35
		sta $01

		jsr sdc_closefile

		ldx #$00										; set filename for video frames, open file and get first sector
:		lda framefile,x
		beq :+
		sta sdc_transferbuffer,x
		inx
		bra :-
:		sta sdc_transferbuffer,x

		jsr sdc_openfile
		jsr sdc_readsector

		jsr ringbuffer_init
		lda #$01
		sta sampletrigger
		lda #$00
		sta cnt3

		lda #>sdc_sectorbuffer
		sta hrmpf1+2
		sta hrmpf2+2
		sta hrmpf3+2

		; --------------------------------------------

irqfinalize

		jsr keyboard_update

		lda keyboard_shouldsendreleaseevent
		beq irqkeyboardend

		lda keyboard_prevpressed
		cmp #KEYBOARD_KEY1
		bne :+
		lda showrastertime
		eor #$01
		sta showrastertime
		lda #$00
		sta $d020
		jmp irqkeyboardend
:
		cmp #KEYBOARD_KEY2
		bne :+
		lda xorfill
		eor #$01
		sta xorfill
		jmp irqkeyboardend
:
		cmp #KEYBOARD_KEY3
		bne :+
		lda showlogo
		eor #%00001111
		sta showlogo
		sta $d015
		jmp irqkeyboardend
:
		cmp #KEYBOARD_CURSORDOWN
		bne :++
		sec
		lda mpVolume
		sbc #$08
		bcs :+
		lda #$00
:		sta mpVolume
		jsr mpUpdateVolume
		jmp irqkeyboardend
:
		cmp #KEYBOARD_CURSORUP
		bne :++
		clc
		lda mpVolume
		adc #$08
		cmp #$40
		bmi :+
		lda #$40
:		sta mpVolume
		jsr mpUpdateVolume
		jmp irqkeyboardend
:

irqkeyboardend
		DEBUGTIME $00 ; black

		pla
		asl $d019
		rti

; ----------------------------------------------------------------------------------------------------

ploteorchars

		lda #$34
		sta $01

		ldx cnt3

plotloop

hrmpf1	lda sdc_sectorbuffer,x
		sta storechar+1
		inx
		bne hrmpf2
		lda hrmpf1+2
		cmp #>sdc_sectorbuffer
		bne wrapme1
		inc hrmpf1+2
		inc hrmpf2+2
		inc hrmpf3+2
		bra hrmpf2
wrapme1	jsr sdc_readsector
		lda #>sdc_sectorbuffer
		sta hrmpf1+2
		sta hrmpf2+2
		sta hrmpf3+2
hrmpf2	lda sdc_sectorbuffer,x
		beq plotloopend
		sta storechar+2
		inx
		bne hrmpf3
		lda hrmpf1+2
		cmp #>sdc_sectorbuffer
		bne wrapme2
		inc hrmpf1+2
		inc hrmpf2+2
		inc hrmpf3+2
		bra hrmpf3
wrapme2	jsr sdc_readsector
		lda #>sdc_sectorbuffer
		sta hrmpf1+2
		sta hrmpf2+2
		sta hrmpf3+2
hrmpf3	lda sdc_sectorbuffer,x
		sta loadchar+1
		inx
		bne hrmpf4
		lda hrmpf1+2
		cmp #>sdc_sectorbuffer
		bne wrapme3
		inc hrmpf1+2
		inc hrmpf2+2
		inc hrmpf3+2
		bra hrmpf4
wrapme3	jsr sdc_readsector
		lda #>sdc_sectorbuffer
		sta hrmpf1+2
		sta hrmpf2+2
		sta hrmpf3+2
hrmpf4

loadchar	lda #$00
storechar	sta $b00b

;		lda endofframes
;		beq :+
;		inc $d020
;		jmp *-3
;:

		jmp plotloop

plotloopend

		inx
		bne :+
		lda hrmpf1+2
		cmp #>sdc_sectorbuffer
		bne wrapme4
		inc hrmpf1+2
		inc hrmpf2+2
		inc hrmpf3+2
		bra :+
wrapme4	jsr sdc_readsector
		lda #>sdc_sectorbuffer
		sta hrmpf1+2
		sta hrmpf2+2
		sta hrmpf3+2
:		inx
		bne :+
		lda hrmpf1+2
		cmp #>sdc_sectorbuffer
		bne wrapme5
		inc hrmpf1+2
		inc hrmpf2+2
		inc hrmpf3+2
		bra :+
wrapme5	jsr sdc_readsector
		lda #>sdc_sectorbuffer
		sta hrmpf1+2
		sta hrmpf2+2
		sta hrmpf3+2
:		

		stx cnt3

		lda #$35
		sta $01

		rts

; ----------------------------------------------------------------------------------------------------

eorfill

		lda #$34
		sta $01

		lda #<.loword(screenchars1)
		sta zpchars+0
		lda #>.loword(screenchars1)
		sta zpchars+1

		ldx #$4b										; $4b, enough to eor fill bitmap that is 80*30*8 = $4b00

		lda #$00										; initialize eor filler

		ldy #$00
:		eor (zpchars),y
		sta (zpchars),y
		iny
		eor (zpchars),y
		sta (zpchars),y
		iny
		eor (zpchars),y
		sta (zpchars),y
		iny
		eor (zpchars),y
		sta (zpchars),y
		iny
		eor (zpchars),y
		sta (zpchars),y
		iny
		eor (zpchars),y
		sta (zpchars),y
		iny
		eor (zpchars),y
		sta (zpchars),y
		iny
		eor (zpchars),y
		sta (zpchars),y
		iny
		bne :-
		inc zpchars+1
		dex
		bpl :-

		lda #$35
		sta $01

		rts

; ----------------------------------------------------------------------------------------------------

clearcolorramjob
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80,((SAFE_COLOR_RAM) >> 20)				; source megabyte
				.byte $81, ((SAFE_COLOR_RAM) >> 20)				; dest megabyte
				.byte $82, 0									; Source skip rate (256ths of bytes)
				.byte $83, 1									; Source skip rate (whole bytes)
				.byte $84, 0									; Destination skip rate (256ths of bytes)
				.byte $85, 1									; Destination skip rate (whole bytes)
				.byte $00										; No more options
				.byte %00000000									; copy and last request
				.word 80*30*2-2									; Count LSB + Count MSB
				.word ((SAFE_COLOR_RAM) & $ffff)				; Destination Address LSB + Destination Address MSB
				.byte (((SAFE_COLOR_RAM) >> 16) & $0f)			; Destination Address BANK and FLAGS (copy to rbBaseMem)
				.word ((SAFE_COLOR_RAM+2) & $ffff)				; Destination Address LSB + Destination Address MSB
				.byte (((SAFE_COLOR_RAM+2) >> 16) & $0f)		; Destination Address BANK and FLAGS (copy to rbBaseMem)
				.word $0000

; -------------------------------------------------------------------------------------------------

clearcharmemjob
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $81, (screenchars1 >> 20)					; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $00										; No more options
				.byte %00000011									; fill and don't chain
				.word 80*30*8									; Count LSB + Count MSB
				.word $0000										; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)
				.word ((screenchars1) & $ffff)					; Destination Address LSB + Destination Address MSB
				.byte ((screenchars1 >> 16) & $0f)				; Destination Address BANK and FLAGS (copy to rbBaseMem)
				.word $0000

; -------------------------------------------------------------------------------------------------

copycharmemjob
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80, (screenchars1 >> 20)					; sourcebank
				.byte $81, (screenchars0 >> 20)					; destbank
				.byte $00										; No more options
				.byte $00										; Copy and don't chain
				.word 80*30*8									; Count LSB + Count MSB
				.word ((screenchars1) & $ffff)					; Source Address LSB + Destination Address MSB
				.byte ((screenchars1 >> 16) & $0f)				; Source Address BANK and FLAGS (copy to rbBaseMem)
				.word ((screenchars0) & $ffff)					; Destination Address LSB + Destination Address MSB
				.byte ((screenchars0 >> 16) & $0f)				; Destination Address BANK and FLAGS (copy to rbBaseMem)
				.word $0000

; -------------------------------------------------------------------------------------------------

musicfile				.byte "BAMUSIC.BIN", 0
framefile				.byte "BAFRAMES.BIN", 0
screenrow				.byte 0
screencolumn			.byte 0
framelo					.byte 0
framehi					.byte 0
sampletrigger			.byte 1
cnt3					.byte 0
endofframes				.byte 0
showrastertime			.byte 0
loadbar					.byte $00
sectorcount				.word 0
xorfill					.byte 1
showlogo				.byte %00001111

; -------------------------------------------------------------------------------------------------

.align 256
sprptrs
	.byte <($6000/64)
	.byte >($6000/64)
	.byte <(($6000)/64 + 32)
	.byte >(($6000)/64 + 32)
	.byte <(($6000)/64 + 16)
	.byte >(($6000)/64 + 16)
	.byte <(($6000)/64 + 48)
	.byte >(($6000)/64 + 48)
