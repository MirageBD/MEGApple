.define palette			$9c00							;                   $0300
.define screen1			$a000							; 80*25*2 =  4000 = $0fa0
.define screenchars1	$b000							; 80*25*8 = 16000 = $3e80

.define zpcol			$90
.define zpscr			$94
.define zpchars			$98

; ----------------------------------------------------------------------------------------------------

.segment "MAIN"

entry_main

		SD_LOAD_ATTICRAM $000000, "samples.bin"

		sei

		lda #$35
		sta $01

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

		lda #%10000000									; force PAL mode, because I can't be bothered with fixing it for NTSC
		trb $d06f										; clear bit 7 for PAL ; trb $d06f 
		;tsb $d06f										; set bit 7 for NTSC  ; tsb $d06f

		lda #65											; enable 40MHz
		sta $00

		lda #%11111000									; unmap c65 roms $d030 by clearing bits 3-7
		trb $d030

		lda #$01										; enable 16 bit char ptrs (bit 0), but leave full colour off (bit 2)
		sta $d054

		lda #%00100000									; turn on bit 5 (enables extended attributes needed for > 256 chars)
		trb $d031

		lda #80*2										; logical chars per row
		sta $d058
		lda #$00
		sta $d059

		lda #$50										; set TEXTXPOS to same as SDBDRWDLSB
		sta $d04c

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
		lda #%00001011									; 00001 for multicolour, 001 for white (dark gray in new palette)
		inz
		sta [zpcol],z

		DMA_RUN_JOB clearcolorramjob					; then copy source to dest+2 to repeat the two bytes over and over

		DMA_RUN_JOB clearcharmemjob

		lda #$00										; fill screenmem linearly vertically
		sta screenrow
		sta screencolumn

		ldx #0
		ldy #0

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
		adc #80*2
		sta put10+1
		lda put10+2
		adc #0
		sta put10+2

		clc
		lda put11+1
		adc #80*2
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
		cmp #160
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

		lda #<.loword(screenchars1)						; set pointer to chargen
		sta $d068
		lda #>.loword(screenchars1)
		sta $d069
		lda #<.hiword(screenchars1)
		sta $d06a

		lda #$00
		sta $d020
		sta $d021

		lda #$01
		sta $d022
		lda #$02
		sta $d023

		lda #$18										; enable multicolour (yes, this is needed, don't remove!!!)
		sta $d016

		lda #$00										; set grayscale palette values
		sta palette+0*$0100+0
		sta palette+1*$0100+0
		sta palette+2*$0100+0
		lda #$04
		sta palette+0*$0100+1
		sta palette+1*$0100+1
		sta palette+2*$0100+1
		lda #$08
		sta palette+0*$0100+2
		sta palette+1*$0100+2
		sta palette+2*$0100+2
		lda #$ff
		sta palette+0*$0100+3
		sta palette+1*$0100+3
		sta palette+2*$0100+3

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

		lda #$7f										; disable CIA interrupts
		sta $dc0d
		sta $dd0d
		lda $dc0d
		lda $dd0d

		lda #$00										; disable IRQ raster interrupts because C65 uses raster interrupts in the ROM
		sta $d01a

		lda #$ff										; setup IRQ interrupt
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

irq1
		pha

		inc $d020
		DMA_RUN_JOB clearcharmemjob
		dec $d020

		lda #$02
		sta $d020

		jsr ploteorchars

		lda #$03
		sta $d020

		jsr eorfill

		lda #$00
		sta $d020

		;lda #$00
		;sta samplecount

/*
		lda sampletrigger
		beq :+
		jsr rbGetSample
		jmp :++

:		lda #$00
		sta rbChannel
		jsr rbUpdate
:

		lda sampletrigger
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
*/

		pla
		asl $d019
		rti

; ----------------------------------------------------------------------------------------------------

ploteorchars

		lda #$34
		sta $01

		lda #<(framedata+0)
		sta hrmpf1+1
		lda #>(framedata+0)
		sta hrmpf1+2
		lda #<(framedata+1)
		sta hrmpf2+1
		lda #>(framedata+1)
		sta hrmpf2+2
		lda #<(framedata+2)
		sta hrmpf3+1
		lda #>(framedata+2)
		sta hrmpf3+2

plotloop
hrmpf1	lda framedata+0
		sta storechar+1
hrmpf2	lda framedata+1
		beq plotloopend
		sta storechar+2
hrmpf3	lda framedata+2
		sta loadchar+1
loadchar	lda #$00
storechar	sta $b00b
		clc
		lda hrmpf1+1
		adc #$03
		sta hrmpf1+1
		lda hrmpf1+2
		adc #$00
		sta hrmpf1+2
		clc
		lda hrmpf2+1
		adc #$03
		sta hrmpf2+1
		lda hrmpf2+2
		adc #$00
		sta hrmpf2+2
		clc
		lda hrmpf3+1
		adc #$03
		sta hrmpf3+1
		lda hrmpf3+2
		adc #$00
		sta hrmpf3+2
		jmp plotloop

plotloopend

		lda #$35
		sta $01

		rts

; ----------------------------------------------------------------------------------------------------

eorfill

		lda #<.loword(screenchars1)
		sta zpchars+0
		lda #>.loword(screenchars1)
		sta zpchars+1
		lda #<.hiword(screenchars1)
		sta zpchars+2
		lda #>.hiword(screenchars1)
		sta zpchars+3

		ldx #$00
		lda #$00										; initialize eor filler

		ldz #$00
:		eor [zpchars],z
		sta [zpchars],z
		inz
		bne :-
		inc zpchars+1
		inx
		cpx #$4b										; $4b, enough to eor fill bitmap that is 80*30*8 = $4b00
		bne :-

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

														; 11 byte DMA List structure starts here
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
				;.byte $84, $00									; Destination skip rate (256ths of bytes)
				;.byte $85, $01									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 11 byte DMA List structure starts here
				.byte %00000011									; fill and don't chain

				.word 80*30*8									; Count LSB + Count MSB

				.word $0000										; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word ((screenchars1) & $ffff)					; Destination Address LSB + Destination Address MSB
				.byte ((screenchars1 >> 16) & $0f)				; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

; -------------------------------------------------------------------------------------------------

screenrow				.byte 0
screencolumn			.byte 0
framelo					.byte 0
framehi					.byte 0

sampletrigger			.byte $01

framedata

.byte $2F, $B2, $40
.byte $30, $B2, $10
.byte $31, $B2, $44
.byte $32, $B2, $14
.byte $CA, $B3, $09
.byte $CB, $B3, $03
.byte $CC, $B3, $0F
.byte $CD, $B3, $05
.byte $81, $B4, $10
.byte $82, $B4, $10
.byte $BB, $B4, $40
.byte $BC, $B4, $10
.byte $BD, $B4, $04
.byte $BE, $B4, $41
.byte $BF, $B4, $10
.byte $C0, $B4, $05
.byte $39, $B5, $01
.byte $3A, $B5, $03
.byte $3C, $B5, $03
.byte $40, $B5, $03
.byte $41, $B5, $01
.byte $43, $B5, $01
.byte $44, $B5, $03
.byte $45, $B5, $01
.byte $25, $B6, $01
.byte $26, $B6, $03
.byte $27, $B6, $09
.byte $28, $B6, $74
.byte $29, $B6, $80
.byte $35, $B6, $40
.byte $36, $B6, $90
.byte $37, $B6, $24
.byte $38, $B6, $0A
.byte $39, $B6, $01
.byte $B1, $B6, $05
.byte $B3, $B6, $05
.byte $EB, $B6, $10
.byte $EC, $B6, $34
.byte $ED, $B6, $30
.byte $EE, $B6, $40
.byte $EF, $B6, $44
.byte $F0, $B6, $10
.byte $0A, $B7, $0B
.byte $0B, $B7, $34
.byte $0C, $B7, $40
.byte $0E, $B7, $40
.byte $11, $B7, $40
.byte $13, $B7, $C0
.byte $14, $B7, $40
.byte $28, $B7, $40
.byte $29, $B7, $80
.byte $2A, $B7, $10
.byte $2E, $B7, $30
.byte $2F, $B7, $10
.byte $30, $B7, $04
.byte $31, $B7, $08
.byte $32, $B7, $01
.byte $33, $B7, $01
.byte $35, $B7, $01
.byte $36, $B7, $02
.byte $78, $B7, $05
.byte $7A, $B7, $05
.byte $F7, $B7, $03
.byte $F8, $B7, $1C
.byte $F9, $B7, $A0
.byte $FA, $B7, $40
.byte $27, $B8, $80
.byte $28, $B8, $40
.byte $29, $B8, $20
.byte $2A, $B8, $10
.byte $2B, $B8, $08
.byte $2C, $B8, $04
.byte $2D, $B8, $02
.byte $2E, $B8, $01
.byte $84, $B8, $60
.byte $85, $B8, $30
.byte $86, $B8, $50
.byte $CC, $B8, $50
.byte $CD, $B8, $34
.byte $CE, $B8, $61
.byte $CF, $B8, $05
.byte $D1, $B8, $50
.byte $D2, $B8, $40
.byte $D3, $B8, $04
.byte $D4, $B8, $14
.byte $E3, $B8, $02
.byte $E4, $B8, $09
.byte $E5, $B8, $34
.byte $E6, $B8, $C0
.byte $1E, $B9, $80
.byte $1F, $B9, $60
.byte $20, $B9, $18
.byte $21, $B9, $06
.byte $22, $B9, $01
.byte $D0, $B9, $02
.byte $D1, $B9, $09
.byte $D2, $B9, $74
.byte $D3, $B9, $80
.byte $12, $BA, $C0
.byte $13, $BA, $30
.byte $14, $BA, $0C
.byte $15, $BA, $02
.byte $16, $BA, $01
.byte $A9, $BA, $03
.byte $AA, $BA, $0C
.byte $AF, $BA, $10
.byte $B0, $BA, $10
.byte $B1, $BA, $04
.byte $B2, $BA, $0C
.byte $B3, $BA, $04
.byte $B4, $BA, $01
.byte $B5, $BA, $02
.byte $BE, $BA, $01
.byte $BF, $BA, $6E
.byte $C0, $BA, $90
.byte $06, $BB, $80
.byte $07, $BB, $60
.byte $08, $BB, $1E
.byte $09, $BB, $01
.byte $55, $BB, $01
.byte $56, $BB, $04
.byte $57, $BB, $04
.byte $58, $BB, $01
.byte $8D, $BB, $02
.byte $8E, $BB, $01
.byte $8F, $BB, $08
.byte $91, $BB, $24
.byte $92, $BB, $10
.byte $93, $BB, $10
.byte $97, $BB, $50
.byte $98, $BB, $80
.byte $A6, $BB, $80
.byte $A8, $BB, $C0
.byte $AC, $BB, $C0
.byte $AD, $BB, $64
.byte $AE, $BB, $B4
.byte $AF, $BB, $50
.byte $F8, $BB, $80
.byte $F9, $BB, $74
.byte $FA, $BB, $0B
.byte $0C, $BC, $01
.byte $0D, $BC, $01
.byte $79, $BC, $07
.byte $7A, $BC, $18
.byte $7B, $BC, $60
.byte $7C, $BC, $80
.byte $EA, $BC, $E9
.byte $EB, $BC, $16
.byte $FB, $BC, $50
.byte $FD, $BC, $50
.byte $66, $BD, $01
.byte $67, $BD, $0A
.byte $68, $BD, $74
.byte $69, $BD, $80
.byte $DA, $BD, $40
.byte $DB, $BD, $BE
.byte $DC, $BD, $01
.byte $2D, $BE, $54
.byte $2F, $BE, $44
.byte $30, $BE, $10
.byte $52, $BE, $02
.byte $53, $BE, $09
.byte $54, $BE, $14
.byte $55, $BE, $20
.byte $56, $BE, $C0
.byte $CB, $BE, $40
.byte $CC, $BE, $BF
.byte $3F, $BF, $01
.byte $40, $BF, $1A
.byte $41, $BF, $A4
.byte $42, $BF, $40
.byte $BB, $BF, $1F
.byte $BC, $BF, $E0
.byte $2A, $C0, $01
.byte $2B, $C0, $06
.byte $2C, $C0, $08
.byte $2D, $C0, $20
.byte $2E, $C0, $50
.byte $2F, $C0, $80
.byte $AB, $C0, $40
.byte $AC, $C0, $A0
.byte $AD, $C0, $1E
.byte $AE, $C0, $01
.byte $F1, $C0, $15
.byte $F2, $C0, $0F
.byte $F3, $C0, $1F
.byte $F5, $C0, $05
.byte $10, $C1, $01
.byte $11, $C1, $03
.byte $14, $C1, $03
.byte $15, $C1, $01
.byte $17, $C1, $02
.byte $18, $C1, $09
.byte $19, $C1, $74
.byte $1A, $C1, $80
.byte $9D, $C1, $40
.byte $9E, $C1, $90
.byte $9F, $C1, $24
.byte $A0, $C1, $0A
.byte $A1, $C1, $01
.byte $FC, $C1, $01
.byte $FD, $C1, $0A
.byte $FE, $C1, $24
.byte $FF, $C1, $90
.byte $00, $C2, $40
.byte $05, $C2, $40
.byte $06, $C2, $40
.byte $90, $C2, $40
.byte $91, $C2, $90
.byte $92, $C2, $30
.byte $93, $C2, $1A
.byte $94, $C2, $05
.byte $E2, $C2, $01
.byte $E3, $C2, $02
.byte $E4, $C2, $04
.byte $E5, $C2, $08
.byte $E6, $C2, $10
.byte $E8, $C2, $30
.byte $E9, $C2, $10
.byte $EA, $C2, $80
.byte $EC, $C2, $40
.byte $82, $C3, $01
.byte $83, $C3, $BE
.byte $84, $C3, $40
.byte $CC, $C3, $01
.byte $CD, $C3, $02
.byte $CE, $C3, $08
.byte $CF, $C3, $14
.byte $D0, $C3, $20
.byte $D1, $C3, $80
.byte $D2, $C3, $40
.byte $72, $C4, $6F
.byte $73, $C4, $90
.byte $B9, $C4, $06
.byte $BA, $C4, $19
.byte $BB, $C4, $60
.byte $BC, $C4, $80
.byte $62, $C5, $F4
.byte $63, $C5, $0A
.byte $64, $C5, $01
.byte $A5, $C5, $01
.byte $A6, $C5, $03
.byte $A7, $C5, $09
.byte $A8, $C5, $74
.byte $A9, $C5, $80
.byte $53, $C6, $5B
.byte $54, $C6, $A4
.byte $90, $C6, $02
.byte $91, $C6, $09
.byte $92, $C6, $14
.byte $93, $C6, $20
.byte $94, $C6, $80
.byte $95, $C6, $40
.byte $40, $C7, $03
.byte $41, $C7, $1C
.byte $42, $C7, $A0
.byte $43, $C7, $40
.byte $7C, $C7, $01
.byte $7D, $C7, $03
.byte $7E, $C7, $1D
.byte $7F, $C7, $E0
.byte $2F, $C8, $6B
.byte $30, $C8, $94
.byte $69, $C8, $01
.byte $6A, $C8, $0A
.byte $6B, $C8, $B4
.byte $6C, $C8, $40
.byte $1E, $C9, $05
.byte $1F, $C9, $FA
.byte $52, $C9, $0B
.byte $53, $C9, $14
.byte $54, $C9, $30
.byte $55, $C9, $30
.byte $57, $C9, $30
.byte $58, $C9, $50
.byte $59, $C9, $80
.byte $0E, $CA, $40
.byte $0F, $CA, $BE
.byte $10, $CA, $01
.byte $41, $CA, $50
.byte $42, $CA, $AF
.byte $FF, $CA, $50
.byte $00, $CB, $AE
.byte $01, $CB, $01
.byte $31, $CB, $5A
.byte $32, $CB, $A5
.byte $F0, $CB, $40
.byte $F1, $CB, $B9
.byte $F2, $CB, $06
.byte $20, $CC, $01
.byte $21, $CC, $BE
.byte $22, $CC, $40
.byte $E2, $CC, $E4
.byte $E3, $CC, $1B
.byte $10, $CD, $BF
.byte $11, $CD, $40
.byte $D3, $CD, $FF
.byte $00, $CE, $FF
.byte $C1, $CE, $01
.byte $C2, $CE, $6E
.byte $C3, $CE, $90
.byte $F0, $CE, $FF
.byte $B0, $CF, $29
.byte $B1, $CF, $D6
.byte $E0, $CF, $FF
.byte $9F, $D0, $06
.byte $A0, $D0, $B9
.byte $A1, $D0, $40
.byte $D0, $D0, $FF
.byte $8E, $D1, $05
.byte $8F, $D1, $FA
.byte $C0, $D1, $FF
.byte $7E, $D2, $55
.byte $7F, $D2, $AA
.byte $B0, $D2, $FF
.byte $6E, $D3, $50
.byte $6F, $D3, $AF
.byte $A0, $D3, $FF
.byte $5F, $D4, $AA
.byte $60, $D4, $55
.byte $90, $D4, $FF
.byte $4C, $D5, $01
.byte $4D, $D5, $06
.byte $4E, $D5, $18
.byte $4F, $D5, $E0
.byte $80, $D5, $F0
.byte $81, $D5, $08
.byte $82, $D5, $04
.byte $83, $D5, $02
.byte $84, $D5, $01
.byte $D8, $D5, $01
.byte $D9, $D5, $01
.byte $E3, $D5, $01
.byte $E4, $D5, $03
.byte $E5, $D5, $02
.byte $34, $D6, $01
.byte $35, $D6, $06
.byte $36, $D6, $0C
.byte $37, $D6, $04
.byte $38, $D6, $10
.byte $39, $D6, $30
.byte $3A, $D6, $50
.byte $3B, $D6, $80
.byte $69, $D6, $05
.byte $6A, $D6, $03
.byte $6B, $D6, $07
.byte $6C, $D6, $01
.byte $75, $D6, $80
.byte $76, $D6, $74
.byte $77, $D6, $09
.byte $78, $D6, $02
.byte $C4, $D6, $01
.byte $C5, $D6, $06
.byte $C6, $D6, $28
.byte $C7, $D6, $90
.byte $C8, $D6, $40
.byte $CB, $D6, $40
.byte $CF, $D6, $10
.byte $D0, $D6, $04
.byte $D1, $D6, $02
.byte $D2, $D6, $40
.byte $D3, $D6, $13
.byte $D4, $D6, $04
.byte $D5, $D6, $41
.byte $D6, $D6, $90
.byte $D7, $D6, $24
.byte $D8, $D6, $08
.byte $D9, $D6, $03
.byte $21, $D7, $05
.byte $22, $D7, $1A
.byte $23, $D7, $60
.byte $24, $D7, $80
.byte $59, $D7, $40
.byte $5B, $D7, $40
.byte $68, $D7, $94
.byte $69, $D7, $6B
.byte $AD, $D7, $01
.byte $AE, $D7, $01
.byte $B3, $D7, $50
.byte $B4, $D7, $B0
.byte $B5, $D7, $4A
.byte $B6, $D7, $55
.byte $C2, $D7, $80
.byte $C3, $D7, $10
.byte $C4, $D7, $F4
.byte $C5, $D7, $0D
.byte $C6, $D7, $7F
.byte $C7, $D7, $03
.byte $C8, $D7, $10
.byte $C9, $D7, $95
.byte $CA, $D7, $64
.byte $CB, $D7, $09
.byte $CC, $D7, $02
.byte $0C, $D8, $01
.byte $0D, $D8, $01
.byte $11, $D8, $50
.byte $12, $D8, $A9
.byte $13, $D8, $06
.byte $57, $D8, $01
.byte $58, $D8, $02
.byte $59, $D8, $FC
.byte $99, $D8, $02
.byte $9A, $D8, $05
.byte $9B, $D8, $19
.byte $9C, $D8, $A6
.byte $9D, $D8, $48
.byte $9E, $D8, $70
.byte $9F, $D8, $80
.byte $A5, $D8, $40
.byte $A6, $D8, $D0
.byte $A7, $D8, $70
.byte $A9, $D8, $10
.byte $AA, $D8, $10
.byte $AF, $D8, $14
.byte $B1, $D8, $0C
.byte $B2, $D8, $04
.byte $B3, $D8, $01
.byte $B5, $D8, $03
.byte $B6, $D8, $50
.byte $B7, $D8, $3B
.byte $B8, $D8, $3C
.byte $BA, $D8, $54
.byte $BD, $D8, $40
.byte $BE, $D8, $A5
.byte $BF, $D8, $18
.byte $FC, $D8, $80
.byte $FD, $D8, $D0
.byte $FE, $D8, $50
.byte $02, $D9, $40
.byte $03, $D9, $B9
.byte $04, $D9, $06
.byte $39, $D9, $01
.byte $3C, $D9, $03
.byte $40, $D9, $02
.byte $47, $D9, $A8
.byte $48, $D9, $57
.byte $52, $D9, $01
.byte $56, $D9, $01
.byte $81, $D9, $01
.byte $82, $D9, $03
.byte $83, $D9, $07
.byte $84, $D9, $01
.byte $86, $D9, $14
.byte $87, $D9, $30
.byte $88, $D9, $80
.byte $89, $D9, $70
.byte $8A, $D9, $90
.byte $8B, $D9, $40
.byte $AC, $D9, $10
.byte $AD, $D9, $10
.byte $B1, $D9, $04
.byte $B3, $D9, $01
.byte $B4, $D9, $05
.byte $E1, $D9, $01
.byte $E2, $D9, $03
.byte $E7, $D9, $03
.byte $EB, $D9, $03
.byte $EC, $D9, $09
.byte $ED, $D9, $74
.byte $EE, $D9, $69
.byte $EF, $D9, $16
.byte $F4, $D9, $90
.byte $F5, $D9, $6E
.byte $F6, $D9, $01
.byte $12, $DA, $02
.byte $13, $DA, $01
.byte $14, $DA, $08
.byte $16, $DA, $04
.byte $18, $DA, $10
.byte $1F, $DA, $30
.byte $21, $DA, $10
.byte $23, $DA, $40
.byte $25, $DA, $C0
.byte $29, $DA, $40
.byte $30, $DA, $FF
.byte $38, $DA, $40
.byte $3C, $DA, $C0
.byte $3D, $DA, $40
.byte $3F, $DA, $40
.byte $40, $DA, $C4
.byte $41, $DA, $04
.byte $42, $DA, $40
.byte $46, $DA, $44
.byte $47, $DA, $AE
.byte $48, $DA, $15
.byte $63, $DA, $01
.byte $64, $DA, $03
.byte $65, $DA, $01
.byte $66, $DA, $04
.byte $67, $DA, $08
.byte $69, $DA, $10
.byte $6B, $DA, $30
.byte $6D, $DA, $20
.byte $6E, $DA, $0A
.byte $6F, $DA, $05
.byte $70, $DA, $40
.byte $72, $DA, $40
.byte $7D, $DA, $01
.byte $7E, $DA, $0B
.byte $7F, $DA, $25
.byte $81, $DA, $30
.byte $82, $DA, $12
.byte $83, $DA, $09
.byte $84, $DA, $04
.byte $B1, $DA, $04
.byte $B3, $DA, $01
.byte $B4, $DA, $04
.byte $B5, $DA, $01
.byte $B8, $DA, $01
.byte $B9, $DA, $01
.byte $D0, $DA, $7F
.byte $D1, $DA, $80
.byte $DF, $DA, $E4
.byte $E0, $DA, $1B
.byte $E5, $DA, $40
.byte $E6, $DA, $B9
.byte $E7, $DA, $06
.byte $FB, $DA, $01
.byte $FC, $DA, $03
.byte $FD, $DA, $05
.byte $FE, $DA, $0C
.byte $FF, $DA, $24
.byte $00, $DB, $50
.byte $01, $DB, $C0
.byte $02, $DB, $40
.byte $20, $DB, $FF
.byte $37, $DB, $40
.byte $38, $DB, $C1
.byte $39, $DB, $53
.byte $3A, $DB, $2B
.byte $3B, $DB, $01
.byte $3C, $DB, $0C
.byte $3D, $DB, $0C
.byte $3E, $DB, $04
.byte $3F, $DB, $01
.byte $40, $DB, $02
.byte $41, $DB, $01
.byte $43, $DB, $01
.byte $4E, $DB, $01
.byte $4F, $DB, $04
.byte $50, $DB, $1D
.byte $51, $DB, $20
.byte $52, $DB, $8C
.byte $53, $DB, $14
.byte $54, $DB, $40
.byte $55, $DB, $30
.byte $57, $DB, $30
.byte $5A, $DB, $30
.byte $5B, $DB, $10
.byte $5C, $DB, $80
.byte $5D, $DB, $40
.byte $64, $DB, $01
.byte $65, $DB, $06
.byte $66, $DB, $0D
.byte $67, $DB, $14
.byte $68, $DB, $21
.byte $69, $DB, $46
.byte $6A, $DB, $39
.byte $6B, $DB, $40
.byte $6C, $DB, $06
.byte $6D, $DB, $69
.byte $6E, $DB, $90
.byte $71, $DB, $1A
.byte $72, $DB, $E5
.byte $80, $DB, $01
.byte $81, $DB, $03
.byte $82, $DB, $01
.byte $83, $DB, $04
.byte $85, $DB, $04
.byte $86, $DB, $02
.byte $87, $DB, $01
.byte $AC, $DB, $40
.byte $AE, $DB, $55
.byte $AF, $DB, $2B
.byte $B0, $DB, $21
.byte $B1, $DB, $18
.byte $B2, $DB, $05
.byte $B3, $DB, $03
.byte $B4, $DB, $01
.byte $C0, $DB, $80
.byte $C1, $DB, $60
.byte $C3, $DB, $04
.byte $C4, $DB, $10
.byte $C5, $DB, $0D
.byte $C6, $DB, $04
.byte $C7, $DB, $03
.byte $C9, $DB, $01
.byte $CD, $DB, $10
.byte $CE, $DB, $10
.byte $D0, $DB, $90
.byte $D1, $DB, $69
.byte $D2, $DB, $06
.byte $D6, $DB, $40
.byte $D7, $DB, $A4
.byte $D8, $DB, $1C
.byte $DA, $DB, $0C
.byte $DC, $DB, $14
.byte $DF, $DB, $30
.byte $E7, $DB, $10
.byte $E8, $DB, $40
.byte $EA, $DB, $C0
.byte $EB, $DB, $40
.byte $10, $DC, $FF
.byte $15, $DC, $24
.byte $16, $DC, $5C
.byte $17, $DC, $1C
.byte $18, $DC, $34
.byte $19, $DC, $40
.byte $1D, $DC, $10
.byte $23, $DC, $01
.byte $24, $DC, $05
.byte $25, $DC, $10
.byte $26, $DC, $44
.byte $28, $DC, $10
.byte $29, $DC, $40
.byte $2F, $DC, $40
.byte $30, $DC, $40
.byte $34, $DC, $40
.byte $35, $DC, $C0
.byte $36, $DC, $42
.byte $37, $DC, $01
.byte $38, $DC, $49
.byte $39, $DC, $22
.byte $3A, $DC, $5C
.byte $3B, $DC, $1D
.byte $3C, $DC, $16
.byte $4A, $DC, $01
.byte $4B, $DC, $07
.byte $4D, $DC, $1C
.byte $4E, $DC, $33
.byte $4F, $DC, $50
.byte $50, $DC, $50
.byte $51, $DC, $3D
.byte $52, $DC, $70
.byte $53, $DC, $30
.byte $54, $DC, $04
.byte $55, $DC, $C0
.byte $57, $DC, $10
.byte $58, $DC, $C0
.byte $59, $DC, $40
.byte $5B, $DC, $69
.byte $5C, $DC, $C3
.byte $5D, $DC, $55
.byte $5F, $DC, $56
.byte $60, $DC, $39
.byte $61, $DC, $90
.byte $67, $DC, $01
.byte $68, $DC, $02
.byte $69, $DC, $05
.byte $6A, $DC, $12
.byte $6B, $DC, $3D
.byte $6C, $DC, $53
.byte $6D, $DC, $C0
.byte $6E, $DC, $05
.byte $6F, $DC, $40
.byte $77, $DC, $01
.byte $78, $DC, $82
.byte $79, $DC, $5C
.byte $7A, $DC, $20
.byte $9B, $DC, $14
.byte $9C, $DC, $10
.byte $9D, $DC, $04
.byte $9E, $DC, $50
.byte $9F, $DC, $04
.byte $A0, $DC, $AD
.byte $A1, $DC, $07
.byte $A2, $DC, $03
.byte $A3, $DC, $19
.byte $A4, $DC, $E5
.byte $A5, $DC, $04
.byte $A6, $DC, $10
.byte $A7, $DC, $15
.byte $A9, $DC, $6A
.byte $AA, $DC, $54
.byte $AB, $DC, $35
.byte $AC, $DC, $0A
.byte $AD, $DC, $01
.byte $B8, $DC, $40
.byte $BA, $DC, $90
.byte $BB, $DC, $34
.byte $BC, $DC, $10
.byte $BE, $DC, $14
.byte $BF, $DC, $30
.byte $C0, $DC, $10
.byte $C1, $DC, $40
.byte $C2, $DC, $80
.byte $00, $DD, $FF
.byte $22, $DD, $05
.byte $23, $DD, $0D
.byte $24, $DD, $2C
.byte $25, $DD, $B4
.byte $26, $DD, $10
.byte $27, $DD, $C0
.byte $28, $DD, $40
.byte $29, $DD, $40
.byte $2A, $DD, $C2
.byte $2B, $DD, $19
.byte $2C, $DD, $34
.byte $2D, $DD, $12
.byte $2E, $DD, $41
.byte $2F, $DD, $08
.byte $30, $DD, $14
.byte $31, $DD, $20
.byte $32, $DD, $80
.byte $33, $DD, $05
.byte $34, $DD, $C0
.byte $35, $DD, $01
.byte $36, $DD, $10
.byte $37, $DD, $CC
.byte $38, $DD, $F0
.byte $3B, $DD, $01
.byte $3C, $DD, $03
.byte $3D, $DD, $17
.byte $3F, $DD, $40
.byte $40, $DD, $04
.byte $41, $DD, $06
.byte $48, $DD, $02
.byte $49, $DD, $02
.byte $4B, $DD, $10
.byte $4C, $DD, $80
.byte $4D, $DD, $10
.byte $4E, $DD, $C0
.byte $4F, $DD, $40
.byte $53, $DD, $02
.byte $54, $DD, $05
.byte $55, $DD, $18
.byte $56, $DD, $60
.byte $57, $DD, $80
.byte $5A, $DD, $80
.byte $5B, $DD, $C0
.byte $5C, $DD, $40
.byte $65, $DD, $01
.byte $66, $DD, $2E
.byte $67, $DD, $D0
.byte $6D, $DD, $07
.byte $6E, $DD, $18
.byte $6F, $DD, $60
.byte $71, $DD, $50
.byte $72, $DD, $20
.byte $73, $DD, $08
.byte $74, $DD, $05
.byte $75, $DD, $02
.byte $93, $DD, $40
.byte $95, $DD, $40
.byte $99, $DD, $40
.byte $9B, $DD, $C0
.byte $9C, $DD, $D0
.byte $9D, $DD, $50
.byte $F0, $DD, $FF
.byte $11, $DE, $01
.byte $15, $DE, $03
.byte $16, $DE, $09
.byte $17, $DE, $24
.byte $18, $DE, $91
.byte $19, $DE, $4B
.byte $1A, $DE, $20
.byte $1B, $DE, $55
.byte $1C, $DE, $80
.byte $2A, $DE, $05
.byte $2B, $DE, $6F
.byte $2C, $DE, $3F
.byte $2D, $DE, $04
.byte $2E, $DE, $52
.byte $2F, $DE, $18
.byte $30, $DE, $B0
.byte $31, $DE, $55
.byte $32, $DE, $45
.byte $33, $DE, $40
.byte $34, $DE, $04
.byte $37, $DE, $40
.byte $39, $DE, $40
.byte $3D, $DE, $01
.byte $3E, $DE, $07
.byte $40, $DE, $0C
.byte $41, $DE, $10
.byte $42, $DE, $A0
.byte $43, $DE, $45
.byte $53, $DE, $06
.byte $54, $DE, $29
.byte $55, $DE, $D0
.byte $5A, $DE, $02
.byte $5B, $DE, $1D
.byte $5C, $DE, $A0
.byte $5D, $DE, $40
.byte $66, $DE, $C0
.byte $67, $DE, $20
.byte $68, $DE, $14
.byte $69, $DE, $08
.byte $6A, $DE, $03
.byte $E0, $DE, $FF
.byte $FE, $DE, $40
.byte $FF, $DE, $D1
.byte $00, $DF, $03
.byte $01, $DF, $34
.byte $02, $DF, $0C
.byte $03, $DF, $D6
.byte $04, $DF, $C8
.byte $05, $DF, $44
.byte $06, $DF, $1A
.byte $07, $DF, $0F
.byte $08, $DF, $B0
.byte $09, $DF, $01
.byte $0A, $DF, $14
.byte $0B, $DF, $40
.byte $10, $DF, $14
.byte $11, $DF, $14
.byte $18, $DF, $05
.byte $19, $DF, $11
.byte $1A, $DF, $84
.byte $1B, $DF, $D0
.byte $1C, $DF, $40
.byte $1D, $DF, $64
.byte $1E, $DF, $84
.byte $1F, $DF, $32
.byte $20, $DF, $48
.byte $21, $DF, $74
.byte $22, $DF, $11
.byte $29, $DF, $01
.byte $2A, $DF, $1B
.byte $2B, $DF, $4E
.byte $2C, $DF, $54
.byte $2D, $DF, $50
.byte $2E, $DF, $F5
.byte $2F, $DF, $5C
.byte $30, $DF, $53
.byte $31, $DF, $33
.byte $32, $DF, $33
.byte $33, $DF, $50
.byte $34, $DF, $01
.byte $35, $DF, $01
.byte $36, $DF, $04
.byte $39, $DF, $01
.byte $3C, $DF, $01
.byte $3D, $DF, $03
.byte $3E, $DF, $01
.byte $3F, $DF, $04
.byte $40, $DF, $0C
.byte $41, $DF, $14
.byte $42, $DF, $A0
.byte $43, $DF, $40
.byte $46, $DF, $01
.byte $47, $DF, $06
.byte $48, $DF, $18
.byte $49, $DF, $A0
.byte $4A, $DF, $40
.byte $57, $DF, $01
.byte $58, $DF, $01
.byte $5A, $DF, $02
.byte $5B, $DF, $D9
.byte $5C, $DF, $24
.byte $63, $DF, $01
.byte $65, $DF, $01
.byte $98, $DF, $01
.byte $99, $DF, $03
.byte $9B, $DF, $01
.byte $9C, $DF, $04
.byte $A5, $DF, $04
.byte $A7, $DF, $01
.byte $AA, $DF, $03
.byte $AB, $DF, $01
.byte $C6, $DF, $01
.byte $C7, $DF, $03
.byte $C8, $DF, $01
.byte $C9, $DF, $04
.byte $CD, $DF, $04
.byte $CE, $DF, $01
.byte $D0, $DF, $FD
.byte $E8, $DF, $01
.byte $E9, $DF, $02
.byte $EA, $DF, $04
.byte $EB, $DF, $09
.byte $EC, $DF, $26
.byte $ED, $DF, $1C
.byte $EE, $DF, $94
.byte $EF, $DF, $60
.byte $F0, $DF, $40
.byte $F1, $DF, $80
.byte $F4, $DF, $01
.byte $F5, $DF, $18
.byte $F6, $DF, $8C
.byte $F7, $DF, $C0
.byte $F8, $DF, $05
.byte $F9, $DF, $50
.byte $0D, $E0, $64
.byte $0E, $E0, $0C
.byte $0F, $E0, $80
.byte $10, $E0, $44
.byte $11, $E0, $03
.byte $12, $E0, $50
.byte $17, $E0, $01
.byte $18, $E0, $14
.byte $19, $E0, $B0
.byte $1A, $E0, $E3
.byte $1B, $E0, $10
.byte $1C, $E0, $3F
.byte $1D, $E0, $0C
.byte $1E, $E0, $DC
.byte $1F, $E0, $01
.byte $20, $E0, $10
.byte $22, $E0, $1D
.byte $23, $E0, $CC
.byte $24, $E0, $11
.byte $25, $E0, $FC
.byte $26, $E0, $04
.byte $27, $E0, $30
.byte $28, $E0, $30
.byte $29, $E0, $10
.byte $2B, $E0, $C0
.byte $2C, $E0, $41
.byte $2D, $E0, $02
.byte $2E, $E0, $02
.byte $2F, $E0, $01
.byte $34, $E0, $0B
.byte $35, $E0, $24
.byte $36, $E0, $D0
.byte $44, $E0, $01
.byte $45, $E0, $51
.byte $46, $E0, $48
.byte $47, $E0, $5E
.byte $48, $E0, $69
.byte $49, $E0, $90
.byte $4A, $E0, $40
.byte $4E, $E0, $02
.byte $4F, $E0, $01
.byte $50, $E0, $08
.byte $51, $E0, $2E
.byte $52, $E0, $8F
.byte $53, $E0, $40
.byte $54, $E0, $14
.byte $55, $E0, $01
.byte $56, $E0, $40
.byte $57, $E0, $C0
.byte $58, $E0, $51
.byte $59, $E0, $2E
.byte $5B, $E0, $06
.byte $5C, $E0, $01
.byte $5D, $E0, $04
.byte $5E, $E0, $01
.byte $5F, $E0, $03
.byte $61, $E0, $01
.byte $7D, $E0, $01
.byte $7E, $E0, $02
.byte $7F, $E0, $04
.byte $80, $E0, $0C
.byte $81, $E0, $04
.byte $83, $E0, $20
.byte $84, $E0, $10
.byte $85, $E0, $40
.byte $86, $E0, $C0
.byte $87, $E0, $40
.byte $9C, $E0, $40
.byte $9D, $E0, $C0
.byte $A1, $E0, $40
.byte $B1, $E0, $40
.byte $B3, $E0, $C0
.byte $B5, $E0, $40
.byte $D5, $E0, $10
.byte $D6, $E0, $74
.byte $D7, $E0, $C0
.byte $D8, $E0, $74
.byte $D9, $E0, $10
.byte $DA, $E0, $80
.byte $DB, $E0, $40
.byte $E3, $E0, $15
.byte $E4, $E0, $B0
.byte $E5, $E0, $F5
.byte $E7, $E0, $50
.byte $F7, $E0, $50
.byte $F8, $E0, $E4
.byte $F9, $E0, $10
.byte $FA, $E0, $F0
.byte $FC, $E0, $05
.byte $FD, $E0, $0A
.byte $FE, $E0, $44
.byte $FF, $E0, $20
.byte $00, $E1, $80
.byte $01, $E1, $40
.byte $02, $E1, $01
.byte $04, $E1, $04
.byte $05, $E1, $05
.byte $06, $E1, $14
.byte $07, $E1, $70
.byte $08, $E1, $75
.byte $09, $E1, $43
.byte $0A, $E1, $10
.byte $0B, $E1, $03
.byte $0C, $E1, $07
.byte $0D, $E1, $4C
.byte $0E, $E1, $15
.byte $12, $E1, $50
.byte $13, $E1, $4B
.byte $14, $E1, $05
.byte $15, $E1, $0A
.byte $16, $E1, $14
.byte $17, $E1, $31
.byte $18, $E1, $56
.byte $19, $E1, $2A
.byte $1A, $E1, $59
.byte $1B, $E1, $64
.byte $1C, $E1, $97
.byte $1D, $E1, $28
.byte $1E, $E1, $D0
.byte $22, $E1, $06
.byte $23, $E1, $6D
.byte $24, $E1, $F0
.byte $25, $E1, $30
.byte $26, $E1, $14
.byte $27, $E1, $40
.byte $32, $E1, $20
.byte $33, $E1, $44
.byte $34, $E1, $0C
.byte $35, $E1, $36
.byte $36, $E1, $E1
.byte $37, $E1, $40
.byte $3C, $E1, $64
.byte $3D, $E1, $CD
.byte $3E, $E1, $50
.byte $40, $E1, $07
.byte $41, $E1, $01
.byte $42, $E1, $90
.byte $43, $E1, $F8
.byte $44, $E1, $33
.byte $46, $E1, $40
.byte $47, $E1, $75
.byte $48, $E1, $90
.byte $49, $E1, $07
.byte $4A, $E1, $6C
.byte $4B, $E1, $95
.byte $52, $E1, $46
.byte $53, $E1, $E9
.byte $54, $E1, $50
.byte $59, $E1, $06
.byte $5A, $E1, $0F
.byte $5B, $E1, $01
.byte $5C, $E1, $0C
.byte $5E, $E1, $04
.byte $5F, $E1, $01
.byte $61, $E1, $01
.byte $6B, $E1, $0A
.byte $6C, $E1, $75
.byte $6D, $E1, $80
.byte $D1, $E1, $05
.byte $D2, $E1, $55
.byte $D3, $E1, $10
.byte $D4, $E1, $40
.byte $E9, $E1, $03
.byte $EA, $E1, $1C
.byte $EB, $E1, $60
.byte $EC, $E1, $80
.byte $ED, $E1, $03
.byte $EE, $E1, $08
.byte $EF, $E1, $14
.byte $F0, $E1, $20
.byte $F1, $E1, $80
.byte $F2, $E1, $D6
.byte $F3, $E1, $39
.byte $F4, $E1, $50
.byte $F5, $E1, $01
.byte $F6, $E1, $1A
.byte $F7, $E1, $74
.byte $F8, $E1, $90
.byte $F9, $E1, $05
.byte $FA, $E1, $AF
.byte $FB, $E1, $3F
.byte $FC, $E1, $C5
.byte $FD, $E1, $10
.byte $FE, $E1, $40
.byte $01, $E2, $04
.byte $02, $E2, $54
.byte $03, $E2, $D0
.byte $04, $E2, $80
.byte $05, $E2, $06
.byte $06, $E2, $2B
.byte $07, $E2, $CD
.byte $08, $E2, $60
.byte $09, $E2, $86
.byte $0A, $E2, $29
.byte $0B, $E2, $D0
.byte $0F, $E2, $01
.byte $10, $E2, $0A
.byte $11, $E2, $64
.byte $12, $E2, $90
.byte $24, $E2, $06
.byte $25, $E2, $B9
.byte $26, $E2, $40
.byte $2A, $E2, $0A
.byte $2B, $E2, $15
.byte $2C, $E2, $10
.byte $2D, $E2, $08
.byte $2E, $E2, $04
.byte $2F, $E2, $52
.byte $30, $E2, $05
.byte $31, $E2, $C1
.byte $32, $E2, $70
.byte $34, $E2, $B0
.byte $35, $E2, $57
.byte $36, $E2, $16
.byte $37, $E2, $4D
.byte $38, $E2, $FC
.byte $3A, $E2, $0A
.byte $3B, $E2, $50
.byte $40, $E2, $01
.byte $41, $E2, $2E
.byte $42, $E2, $D0
.byte $46, $E2, $0B
.byte $47, $E2, $74
.byte $48, $E2, $80
.byte $4B, $E2, $54
.byte $4C, $E2, $FB
.byte $4D, $E2, $50
.byte $4E, $E2, $1A
.byte $4F, $E2, $65
.byte $50, $E2, $C0
.byte $51, $E2, $C0
.byte $52, $E2, $1B
.byte $53, $E2, $34
.byte $54, $E2, $50
.byte $56, $E2, $01
.byte $57, $E2, $03
.byte $58, $E2, $02
.byte $5C, $E2, $40
.byte $5D, $E2, $A4
.byte $5E, $E2, $1A
.byte $5F, $E2, $01
.byte $62, $E2, $01
.byte $63, $E2, $01
.byte $C5, $E2, $04
.byte $C6, $E2, $01
.byte $C7, $E2, $04
.byte $C8, $E2, $01
.byte $C9, $E2, $04
.byte $CB, $E2, $05
.byte $CD, $E2, $01
.byte $D0, $E2, $02
.byte $D1, $E2, $01
.byte $D2, $E2, $04
.byte $D3, $E2, $0C
.byte $D5, $E2, $14
.byte $D6, $E2, $30
.byte $D7, $E2, $50
.byte $D8, $E2, $82
.byte $D9, $E2, $05
.byte $DA, $E2, $18
.byte $DB, $E2, $60
.byte $DC, $E2, $80
.byte $DF, $E2, $01
.byte $E0, $E2, $1B
.byte $E1, $E2, $B5
.byte $E2, $E2, $51
.byte $E3, $E2, $17
.byte $E4, $E2, $78
.byte $E5, $E2, $D1
.byte $E6, $E2, $40
.byte $E7, $E2, $14
.byte $E8, $E2, $70
.byte $E9, $E2, $F4
.byte $EA, $E2, $D0
.byte $EB, $E2, $40
.byte $F4, $E2, $40
.byte $F6, $E2, $40
.byte $F8, $E2, $1B
.byte $F9, $E2, $E4
.byte $FD, $E2, $06
.byte $FE, $E2, $29
.byte $FF, $E2, $D0
.byte $12, $E3, $06
.byte $13, $E3, $69
.byte $14, $E3, $90
.byte $16, $E3, $01
.byte $17, $E3, $01
.byte $1A, $E3, $55
.byte $1B, $E3, $AB
.byte $1C, $E3, $01
.byte $1D, $E3, $01
.byte $20, $E3, $47
.byte $21, $E3, $90
.byte $22, $E3, $6C
.byte $23, $E3, $15
.byte $27, $E3, $50
.byte $28, $E3, $40
.byte $29, $E3, $10
.byte $2A, $E3, $80
.byte $2B, $E3, $34
.byte $2C, $E3, $10
.byte $2D, $E3, $01
.byte $2E, $E3, $0D
.byte $2F, $E3, $28
.byte $30, $E3, $D0
.byte $34, $E3, $05
.byte $35, $E3, $BC
.byte $36, $E3, $40
.byte $3B, $E3, $69
.byte $3C, $E3, $90
.byte $3E, $E3, $B9
.byte $3F, $E3, $4D
.byte $40, $E3, $24
.byte $41, $E3, $90
.byte $42, $E3, $40
.byte $44, $E3, $0B
.byte $45, $E3, $64
.byte $46, $E3, $84
.byte $47, $E3, $01
.byte $49, $E3, $80
.byte $4A, $E3, $6A
.byte $4E, $E3, $40
.byte $4F, $E3, $D4
.byte $50, $E3, $60
.byte $51, $E3, $54
.byte $52, $E3, $0E
.byte $53, $E3, $51
.byte $A5, $E3, $14
.byte $A6, $E3, $1D
.byte $A7, $E3, $0C
.byte $A8, $E3, $04
.byte $A9, $E3, $10
.byte $AA, $E3, $05
.byte $AB, $E3, $10
.byte $AC, $E3, $01
.byte $AD, $E3, $04
.byte $B0, $E3, $01
.byte $B6, $E3, $01
.byte $B7, $E3, $43
.byte $B8, $E3, $D1
.byte $B9, $E3, $88
.byte $BA, $E3, $04
.byte $BB, $E3, $40
.byte $BC, $E3, $30
.byte $BD, $E3, $11
.byte $BE, $E3, $C3
.byte $BF, $E3, $45
.byte $C0, $E3, $0E
.byte $C1, $E3, $09
.byte $C4, $E3, $01
.byte $C5, $E3, $07
.byte $C6, $E3, $28
.byte $C7, $E3, $93
.byte $C8, $E3, $44
.byte $CA, $E3, $0C
.byte $CB, $E3, $03
.byte $CC, $E3, $11
.byte $CD, $E3, $31
.byte $CE, $E3, $97
.byte $CF, $E3, $47
.byte $D0, $E3, $1D
.byte $D1, $E3, $64
.byte $D2, $E3, $C0
.byte $D3, $E3, $40
.byte $D4, $E3, $40
.byte $D6, $E3, $6A
.byte $D7, $E3, $03
.byte $D8, $E3, $28
.byte $D9, $E3, $01
.byte $E5, $E3, $06
.byte $E6, $E3, $29
.byte $E7, $E3, $D0
.byte $EA, $E3, $02
.byte $EB, $E3, $1D
.byte $EC, $E3, $60
.byte $ED, $E3, $80
.byte $00, $E4, $01
.byte $01, $E4, $2E
.byte $02, $E4, $D0
.byte $05, $E4, $15
.byte $06, $E4, $F1
.byte $07, $E4, $E4
.byte $0A, $E4, $05
.byte $0B, $E4, $5A
.byte $0F, $E4, $30
.byte $10, $E4, $40
.byte $12, $E4, $10
.byte $17, $E4, $20
.byte $18, $E4, $14
.byte $19, $E4, $08
.byte $1A, $E4, $01
.byte $1B, $E4, $03
.byte $1C, $E4, $01
.byte $23, $E4, $01
.byte $25, $E4, $03
.byte $28, $E4, $03
.byte $2A, $E4, $01
.byte $31, $E4, $01
.byte $32, $E4, $1A
.byte $33, $E4, $A5
.byte $34, $E4, $41
.byte $35, $E4, $40
.byte $36, $E4, $D0
.byte $37, $E4, $30
.byte $38, $E4, $C5
.byte $39, $E4, $0A
.byte $3A, $E4, $90
.byte $3D, $E4, $07
.byte $3E, $E4, $04
.byte $3F, $E4, $02
.byte $40, $E4, $01
.byte $42, $E4, $54
.byte $44, $E4, $C0
.byte $45, $E4, $3C
.byte $46, $E4, $01
.byte $47, $E4, $40
.byte $48, $E4, $03
.byte $49, $E4, $01
.byte $4A, $E4, $14
.byte $98, $E4, $40
.byte $99, $E4, $C0
.byte $9A, $E4, $10
.byte $9B, $E4, $F0
.byte $9C, $E4, $40
.byte $9D, $E4, $04
.byte $9E, $E4, $7D
.byte $9F, $E4, $02
.byte $A2, $E4, $04
.byte $A3, $E4, $70
.byte $A4, $E4, $51
.byte $A5, $E4, $82
.byte $A6, $E4, $04
.byte $A7, $E4, $08
.byte $A8, $E4, $20
.byte $A9, $E4, $10
.byte $AA, $E4, $40
.byte $AB, $E4, $C6
.byte $AC, $E4, $D3
.byte $AD, $E4, $C3
.byte $AE, $E4, $0D
.byte $AF, $E4, $24
.byte $B0, $E4, $80
.byte $B1, $E4, $01
.byte $B2, $E4, $07
.byte $B3, $E4, $1C
.byte $B4, $E4, $A5
.byte $B9, $E4, $40
.byte $BB, $E4, $06
.byte $BC, $E4, $2C
.byte $BD, $E4, $83
.byte $BE, $E4, $3D
.byte $BF, $E4, $D4
.byte $C0, $E4, $40
.byte $C5, $E4, $14
.byte $C6, $E4, $84
.byte $C7, $E4, $C0
.byte $C8, $E4, $50
.byte $D4, $E4, $05
.byte $D5, $E4, $BA
.byte $D6, $E4, $40
.byte $D9, $E4, $16
.byte $DA, $E4, $E9
.byte $EE, $E4, $02
.byte $EF, $E4, $2D
.byte $F0, $E4, $D0
.byte $F2, $E4, $07
.byte $F3, $E4, $18
.byte $F4, $E4, $30
.byte $F5, $E4, $50
.byte $F7, $E4, $6B
.byte $F8, $E4, $14
.byte $F9, $E4, $05
.byte $FA, $E4, $AA
.byte $FB, $E4, $50
.byte $0A, $E5, $01
.byte $0B, $E5, $1A
.byte $0C, $E5, $A4
.byte $0D, $E5, $40
.byte $10, $E5, $0B
.byte $11, $E5, $74
.byte $12, $E5, $80
.byte $15, $E5, $06
.byte $16, $E5, $1F
.byte $17, $E5, $71
.byte $18, $E5, $DC
.byte $19, $E5, $12
.byte $1A, $E5, $49
.byte $1B, $E5, $10
.byte $20, $E5, $0B
.byte $21, $E5, $A4
.byte $22, $E5, $04
.byte $23, $E5, $14
.byte $24, $E5, $40
.byte $26, $E5, $05
.byte $27, $E5, $13
.byte $28, $E5, $7B
.byte $29, $E5, $93
.byte $2A, $E5, $01
.byte $2C, $E5, $45
.byte $2D, $E5, $AF
.byte $2E, $E5, $05
.byte $2F, $E5, $10
.byte $30, $E5, $96
.byte $31, $E5, $6C
.byte $32, $E5, $2E
.byte $33, $E5, $20
.byte $34, $E5, $0B
.byte $35, $E5, $10
.byte $36, $E5, $01
.byte $37, $E5, $07
.byte $38, $E5, $7C
.byte $39, $E5, $D5
.byte $3A, $E5, $40
.byte $84, $E5, $05
.byte $85, $E5, $0C
.byte $86, $E5, $01
.byte $87, $E5, $10
.byte $88, $E5, $0C
.byte $89, $E5, $30
.byte $8A, $E5, $05
.byte $8B, $E5, $47
.byte $8C, $E5, $CF
.byte $8D, $E5, $40
.byte $8E, $E5, $0D
.byte $8F, $E5, $04
.byte $90, $E5, $30
.byte $91, $E5, $10
.byte $92, $E5, $80
.byte $93, $E5, $40
.byte $99, $E5, $05
.byte $9A, $E5, $60
.byte $9B, $E5, $F2
.byte $9C, $E5, $F8
.byte $9D, $E5, $90
.byte $A0, $E5, $05
.byte $A1, $E5, $50
.byte $A2, $E5, $FF
.byte $A3, $E5, $05
.byte $A4, $E5, $50
.byte $A8, $E5, $0A
.byte $A9, $E5, $20
.byte $AA, $E5, $80
.byte $AB, $E5, $7F
.byte $AC, $E5, $84
.byte $AD, $E5, $11
.byte $AE, $E5, $40
.byte $C3, $E5, $06
.byte $C4, $E5, $B9
.byte $C5, $E5, $40
.byte $C8, $E5, $15
.byte $C9, $E5, $70
.byte $CA, $E5, $8F
.byte $CB, $E5, $15
.byte $DC, $E5, $01
.byte $DD, $E5, $6E
.byte $DE, $E5, $90
.byte $E0, $E5, $01
.byte $E1, $E5, $6E
.byte $E2, $E5, $90
.byte $E6, $E5, $6B
.byte $E7, $E5, $94
.byte $E8, $E5, $05
.byte $E9, $E5, $BB
.byte $EA, $E5, $41
.byte $F8, $E5, $02
.byte $F9, $E5, $1D
.byte $FA, $E5, $A0
.byte $FB, $E5, $40
.byte $FE, $E5, $07
.byte $FF, $E5, $B8
.byte $00, $E6, $40
.byte $03, $E6, $15
.byte $04, $E6, $45
.byte $05, $E6, $11
.byte $06, $E6, $42
.byte $07, $E6, $0C
.byte $08, $E6, $70
.byte $09, $E6, $80
.byte $0D, $E6, $01
.byte $0E, $E6, $0A
.byte $0F, $E6, $B4
.byte $10, $E6, $40
.byte $16, $E6, $40
.byte $17, $E6, $40
.byte $18, $E6, $1A
.byte $19, $E6, $BE
.byte $1A, $E6, $5E
.byte $1B, $E6, $05
.byte $1C, $E6, $40
.byte $1D, $E6, $1A
.byte $1E, $E6, $E5
.byte $1F, $E6, $40
.byte $20, $E6, $94
.byte $21, $E6, $C4
.byte $22, $E6, $50
.byte $23, $E6, $40
.byte $24, $E6, $E0
.byte $25, $E6, $49
.byte $26, $E6, $50
.byte $27, $E6, $D0
.byte $28, $E6, $3C
.byte $29, $E6, $55
.byte $76, $E6, $01
.byte $77, $E6, $04
.byte $78, $E6, $1C
.byte $79, $E6, $3D
.byte $7A, $E6, $B4
.byte $7B, $E6, $D0
.byte $7C, $E6, $10
.byte $7D, $E6, $74
.byte $7E, $E6, $0C
.byte $7F, $E6, $30
.byte $80, $E6, $40
.byte $82, $E6, $01
.byte $83, $E6, $03
.byte $84, $E6, $0D
.byte $85, $E6, $30
.byte $86, $E6, $30
.byte $87, $E6, $41
.byte $88, $E6, $7C
.byte $89, $E6, $05
.byte $8A, $E6, $D0
.byte $8B, $E6, $41
.byte $8C, $E6, $14
.byte $8D, $E6, $05
.byte $8E, $E6, $10
.byte $8F, $E6, $05
.byte $90, $E6, $53
.byte $91, $E6, $0C
.byte $92, $E6, $F5
.byte $93, $E6, $50
.byte $96, $E6, $05
.byte $97, $E6, $61
.byte $98, $E6, $F4
.byte $99, $E6, $C5
.byte $9A, $E6, $C0
.byte $9B, $E6, $85
.byte $9C, $E6, $10
.byte $B1, $E6, $07
.byte $B2, $E6, $78
.byte $B3, $E6, $80
.byte $B5, $E6, $06
.byte $B6, $E6, $16
.byte $B7, $E6, $10
.byte $B8, $E6, $55
.byte $B9, $E6, $0F
.byte $BA, $E6, $A5
.byte $CB, $E6, $01
.byte $CC, $E6, $BE
.byte $CD, $E6, $40
.byte $D0, $E6, $50
.byte $D1, $E6, $C5
.byte $D2, $E6, $6F
.byte $D3, $E6, $05
.byte $D4, $E6, $02
.byte $D5, $E6, $6D
.byte $D6, $E6, $90
.byte $D8, $E6, $54
.byte $D9, $E6, $3E
.byte $DA, $E6, $C4
.byte $DB, $E6, $51
.byte $E6, $E6, $02
.byte $E7, $E6, $6D
.byte $E8, $E6, $90
.byte $EB, $E6, $01
.byte $EC, $E6, $0B
.byte $ED, $E6, $B4
.byte $EE, $E6, $41
.byte $F1, $E6, $01
.byte $F2, $E6, $46
.byte $F3, $E6, $58
.byte $F4, $E6, $60
.byte $F5, $E6, $80
.byte $FA, $E6, $01
.byte $FB, $E6, $07
.byte $FC, $E6, $19
.byte $FD, $E6, $E0
.byte $0A, $E7, $46
.byte $0B, $E7, $B9
.byte $0C, $E7, $05
.byte $0D, $E7, $53
.byte $0E, $E7, $E8
.byte $0F, $E7, $41
.byte $16, $E7, $90
.byte $17, $E7, $D0
.byte $18, $E7, $40
.byte $63, $E7, $01
.byte $64, $E7, $15
.byte $65, $E7, $44
.byte $66, $E7, $10
.byte $67, $E7, $40
.byte $75, $E7, $80
.byte $76, $E7, $5B
.byte $77, $E7, $24
.byte $78, $E7, $01
.byte $79, $E7, $07
.byte $7A, $E7, $01
.byte $7B, $E7, $5C
.byte $7C, $E7, $42
.byte $7D, $E7, $18
.byte $7E, $E7, $5B
.byte $7F, $E7, $30
.byte $80, $E7, $C0
.byte $81, $E7, $05
.byte $82, $E7, $50
.byte $85, $E7, $40
.byte $87, $E7, $40
.byte $88, $E7, $10
.byte $89, $E7, $44
.byte $8A, $E7, $51
.byte $8B, $E7, $40
.byte $8C, $E7, $40
.byte $8D, $E7, $04
.byte $8F, $E7, $01
.byte $9E, $E7, $02
.byte $9F, $E7, $0D
.byte $A0, $E7, $B0
.byte $A1, $E7, $40
.byte $A2, $E7, $01
.byte $A3, $E7, $0A
.byte $A4, $E7, $75
.byte $A5, $E7, $DA
.byte $A6, $E7, $A4
.byte $A7, $E7, $05
.byte $A8, $E7, $53
.byte $A9, $E7, $E9
.byte $AA, $E7, $40
.byte $B8, $E7, $04
.byte $B9, $E7, $02
.byte $BA, $E7, $04
.byte $BB, $E7, $59
.byte $BC, $E7, $A4
.byte $C1, $E7, $50
.byte $C3, $E7, $C0
.byte $C4, $E7, $91
.byte $C5, $E7, $01
.byte $C7, $E7, $15
.byte $C8, $E7, $4F
.byte $C9, $E7, $F5
.byte $CA, $E7, $10
.byte $CB, $E7, $40
.byte $D3, $E7, $02
.byte $D4, $E7, $1D
.byte $D5, $E7, $A0
.byte $D6, $E7, $40
.byte $D9, $E7, $01
.byte $DA, $E7, $0A
.byte $DB, $E7, $64
.byte $DC, $E7, $C0
.byte $DD, $E7, $10
.byte $DE, $E7, $47
.byte $DF, $E7, $18
.byte $E0, $E7, $60
.byte $E1, $E7, $80
.byte $E5, $E7, $01
.byte $E6, $E7, $04
.byte $E7, $E7, $0D
.byte $E8, $E7, $2C
.byte $E9, $E7, $84
.byte $EA, $E7, $40
.byte $EB, $E7, $30
.byte $EF, $E7, $30
.byte $F0, $E7, $10
.byte $F1, $E7, $08
.byte $F2, $E7, $07
.byte $F7, $E7, $15
.byte $F8, $E7, $4F
.byte $F9, $E7, $02
.byte $FA, $E7, $C8
.byte $FB, $E7, $10
.byte $FC, $E7, $69
.byte $FD, $E7, $43
.byte $FE, $E7, $CC
.byte $FF, $E7, $67
.byte $00, $E8, $01
.byte $06, $E8, $40
.byte $08, $E8, $40
.byte $61, $E8, $01
.byte $62, $E8, $02
.byte $63, $E8, $08
.byte $64, $E8, $25
.byte $65, $E8, $D7
.byte $66, $E8, $1D
.byte $67, $E8, $74
.byte $68, $E8, $D0
.byte $69, $E8, $40
.byte $6B, $E8, $07
.byte $6C, $E8, $B8
.byte $6D, $E8, $15
.byte $6E, $E8, $C3
.byte $6F, $E8, $3C
.byte $70, $E8, $15
.byte $71, $E8, $40
.byte $77, $E8, $01
.byte $78, $E8, $A0
.byte $79, $E8, $C5
.byte $7A, $E8, $40
.byte $7B, $E8, $7C
.byte $7C, $E8, $01
.byte $7D, $E8, $13
.byte $7E, $E8, $1C
.byte $80, $E8, $04
.byte $82, $E8, $47
.byte $85, $E8, $0C
.byte $86, $E8, $0C
.byte $87, $E8, $13
.byte $88, $E8, $01
.byte $89, $E8, $0C
.byte $8A, $E8, $14
.byte $8B, $E8, $30
.byte $8D, $E8, $90
.byte $8E, $E8, $40
.byte $8F, $E8, $01
.byte $90, $E8, $06
.byte $91, $E8, $18
.byte $92, $E8, $A1
.byte $93, $E8, $5A
.byte $94, $E8, $A4
.byte $95, $E8, $40
.byte $97, $E8, $51
.byte $98, $E8, $FB
.byte $99, $E8, $55
.byte $AB, $E8, $D0
.byte $AC, $E8, $2E
.byte $AD, $E8, $01
.byte $B2, $E8, $54
.byte $B3, $E8, $FE
.byte $B5, $E8, $BF
.byte $B6, $E8, $05
.byte $B7, $E8, $45
.byte $B8, $E8, $F0
.byte $B9, $E8, $5F
.byte $BB, $E8, $1C
.byte $BE, $E8, $1F
.byte $BF, $E8, $04
.byte $C0, $E8, $05
.byte $C1, $E8, $18
.byte $C2, $E8, $60
.byte $C3, $E8, $81
.byte $C4, $E8, $01
.byte $C8, $E8, $14
.byte $C9, $E8, $A0
.byte $CA, $E8, $50
.byte $CB, $E8, $04
.byte $CC, $E8, $60
.byte $CD, $E8, $80
.byte $CF, $E8, $01
.byte $D0, $E8, $02
.byte $D1, $E8, $09
.byte $D2, $E8, $1A
.byte $D3, $E8, $50
.byte $D5, $E8, $40
.byte $D6, $E8, $01
.byte $D9, $E8, $01
.byte $E1, $E8, $01
.byte $E2, $E8, $5A
.byte $E3, $E8, $A4
.byte $E6, $E8, $50
.byte $E7, $E8, $10
.byte $E8, $E8, $40
.byte $EC, $E8, $01
.byte $ED, $E8, $A4
.byte $EE, $E8, $33
.byte $EF, $E8, $D3
.byte $F0, $E8, $45
.byte $4C, $E9, $01
.byte $4D, $E9, $06
.byte $4E, $E9, $0D
.byte $4F, $E9, $26
.byte $50, $E9, $58
.byte $51, $E9, $92
.byte $52, $E9, $0F
.byte $53, $E9, $7C
.byte $54, $E9, $C1
.byte $55, $E9, $04
.byte $56, $E9, $50
.byte $59, $E9, $07
.byte $5A, $E9, $68
.byte $5B, $E9, $90
.byte $5C, $E9, $56
.byte $5D, $E9, $0D
.byte $5E, $E9, $F4
.byte $5F, $E9, $50
.byte $64, $E9, $01
.byte $65, $E9, $03
.byte $66, $E9, $07
.byte $67, $E9, $10
.byte $68, $E9, $40
.byte $69, $E9, $73
.byte $6A, $E9, $03
.byte $6B, $E9, $70
.byte $6C, $E9, $40
.byte $6F, $E9, $13
.byte $70, $E9, $40
.byte $71, $E9, $81
.byte $72, $E9, $1C
.byte $73, $E9, $74
.byte $74, $E9, $50
.byte $75, $E9, $40
.byte $76, $E9, $40
.byte $7A, $E9, $01
.byte $7B, $E9, $07
.byte $7C, $E9, $09
.byte $7D, $E9, $20
.byte $7E, $E9, $50
.byte $7F, $E9, $80
.byte $80, $E9, $02
.byte $81, $E9, $2D
.byte $82, $E9, $90
.byte $83, $E9, $42
.byte $85, $E9, $02
.byte $86, $E9, $55
.byte $87, $E9, $0F
.byte $88, $E9, $F0
.byte $89, $E9, $55
.byte $9B, $E9, $1A
.byte $9C, $E9, $A5
.byte $9D, $E9, $40
.byte $A0, $E9, $55
.byte $A1, $E9, $3C
.byte $A2, $E9, $7D
.byte $A3, $E9, $80
.byte $A5, $E9, $94
.byte $A6, $E9, $01
.byte $A7, $E9, $04
.byte $A8, $E9, $5F
.byte $A9, $E9, $43
.byte $AA, $E9, $01
.byte $AB, $E9, $18
.byte $AC, $E9, $40
.byte $AE, $E9, $01
.byte $AF, $E9, $40
.byte $B0, $E9, $01
.byte $B1, $E9, $01
.byte $B2, $E9, $55
.byte $B3, $E9, $14
.byte $B4, $E9, $40
.byte $BC, $E9, $01
.byte $BD, $E9, $04
.byte $BE, $E9, $61
.byte $BF, $E9, $80
.byte $C1, $E9, $74
.byte $C2, $E9, $90
.byte $C6, $E9, $40
.byte $C7, $E9, $40
.byte $CF, $E9, $02
.byte $D0, $E9, $1D
.byte $D1, $E9, $A0
.byte $D2, $E9, $40
.byte $DC, $E9, $24
.byte $DD, $E9, $24
.byte $DE, $E9, $40
.byte $DF, $E9, $59
.byte $E0, $E9, $08
.byte $E1, $E9, $11
.byte $37, $EA, $01
.byte $38, $EA, $04
.byte $39, $EA, $0D
.byte $3A, $EA, $1C
.byte $3B, $EA, $74
.byte $3C, $EA, $E0
.byte $3D, $EA, $C0
.byte $3E, $EA, $40
.byte $3F, $EA, $05
.byte $40, $EA, $60
.byte $41, $EA, $30
.byte $42, $EA, $14
.byte $43, $EA, $03
.byte $44, $EA, $44
.byte $45, $EA, $0F
.byte $46, $EA, $10
.byte $47, $EA, $26
.byte $48, $EA, $C5
.byte $49, $EA, $10
.byte $4A, $EA, $70
.byte $4B, $EA, $33
.byte $4C, $EA, $FC
.byte $4D, $EA, $40
.byte $4E, $EA, $01
.byte $4F, $EA, $10
.byte $50, $EA, $04
.byte $51, $EA, $05
.byte $53, $EA, $40
.byte $54, $EA, $C5
.byte $55, $EA, $C1
.byte $56, $EA, $43
.byte $57, $EA, $05
.byte $58, $EA, $8C
.byte $59, $EA, $50
.byte $5A, $EA, $34
.byte $5B, $EA, $40
.byte $5C, $EA, $10
.byte $5F, $EA, $40
.byte $68, $EA, $01
.byte $69, $EA, $04
.byte $6A, $EA, $5F
.byte $6B, $EA, $F0
.byte $6C, $EA, $55
.byte $6E, $EA, $10
.byte $6F, $EA, $80
.byte $70, $EA, $10
.byte $71, $EA, $C0
.byte $72, $EA, $40
.byte $75, $EA, $EA
.byte $76, $EA, $BD
.byte $77, $EA, $0D
.byte $78, $EA, $30
.byte $79, $EA, $7F
.byte $7A, $EA, $10
.byte $7B, $EA, $04
.byte $7C, $EA, $01
.byte $89, $EA, $01
.byte $8A, $EA, $5A
.byte $8B, $EA, $A4
.byte $8E, $EA, $01
.byte $8F, $EA, $1E
.byte $90, $EA, $40
.byte $91, $EA, $44
.byte $92, $EA, $70
.byte $93, $EA, $C2
.byte $94, $EA, $A9
.byte $95, $EA, $14
.byte $96, $EA, $E0
.byte $97, $EA, $B4
.byte $98, $EA, $01
.byte $99, $EA, $47
.byte $9A, $EA, $1C
.byte $9B, $EA, $25
.byte $9C, $EA, $40
.byte $9D, $EA, $52
.byte $9E, $EA, $68
.byte $9F, $EA, $41
.byte $A0, $EA, $44
.byte $A1, $EA, $40
.byte $A8, $EA, $01
.byte $A9, $EA, $04
.byte $AA, $EA, $15
.byte $AB, $EA, $50
.byte $AD, $EA, $40
.byte $BE, $EA, $40
.byte $BF, $EA, $A4
.byte $C0, $EA, $1B
.byte $CF, $EA, $50
.byte $D0, $EA, $05
.byte $D1, $EA, $55
.byte $E9, $EA, $01
.byte $EA, $EA, $03
.byte $EC, $EA, $05
.byte $ED, $EA, $0C
.byte $EF, $EA, $14
.byte $F2, $EA, $30
.byte $F7, $EA, $40
.byte $F8, $EA, $10
.byte $FA, $EA, $C0
.byte $FB, $EA, $10
.byte $FC, $EA, $C0
.byte $FE, $EA, $74
.byte $FF, $EA, $1E
.byte $00, $EB, $05
.byte $2B, $EB, $01
.byte $2C, $EB, $07
.byte $2D, $EB, $0C
.byte $2E, $EB, $5F
.byte $2F, $EB, $01
.byte $30, $EB, $30
.byte $31, $EB, $C4
.byte $32, $EB, $30
.byte $33, $EB, $D0
.byte $34, $EB, $46
.byte $35, $EB, $2C
.byte $36, $EB, $D0
.byte $37, $EB, $1F
.byte $38, $EB, $70
.byte $39, $EB, $C3
.byte $3A, $EB, $C2
.byte $3B, $EB, $14
.byte $3C, $EB, $C0
.byte $3D, $EB, $D0
.byte $3E, $EB, $C0
.byte $41, $EB, $40
.byte $42, $EB, $30
.byte $43, $EB, $44
.byte $44, $EB, $C0
.byte $46, $EB, $0C
.byte $47, $EB, $06
.byte $48, $EB, $10
.byte $49, $EB, $41
.byte $4B, $EB, $05
.byte $4C, $EB, $0C
.byte $4D, $EB, $13
.byte $4E, $EB, $60
.byte $4F, $EB, $04
.byte $50, $EB, $53
.byte $51, $EB, $99
.byte $52, $EB, $34
.byte $53, $EB, $82
.byte $54, $EB, $1D
.byte $55, $EB, $60
.byte $56, $EB, $C0
.byte $57, $EB, $A9
.byte $58, $EB, $43
.byte $59, $EB, $0F
.byte $5B, $EB, $F0
.byte $5C, $EB, $50
.byte $5D, $EB, $05
.byte $64, $EB, $10
.byte $65, $EB, $A9
.byte $66, $EB, $56
.byte $67, $EB, $40
.byte $69, $EB, $C4
.byte $6A, $EB, $31
.byte $6B, $EB, $0F
.byte $6C, $EB, $40
.byte $6D, $EB, $10
.byte $6E, $EB, $04
.byte $6F, $EB, $01
.byte $76, $EB, $01
.byte $77, $EB, $1A
.byte $78, $EB, $24
.byte $79, $EB, $C1
.byte $7A, $EB, $04
.byte $7B, $EB, $05
.byte $7D, $EB, $10
.byte $7E, $EB, $84
.byte $7F, $EB, $70
.byte $80, $EB, $34
.byte $82, $EB, $50
.byte $83, $EB, $80
.byte $84, $EB, $01
.byte $86, $EB, $11
.byte $87, $EB, $45
.byte $89, $EB, $FC
.byte $8A, $EB, $03
.byte $8B, $EB, $42
.byte $8C, $EB, $78
.byte $8D, $EB, $90
.byte $94, $EB, $01
.byte $95, $EB, $07
.byte $96, $EB, $13
.byte $97, $EB, $41
.byte $98, $EB, $14
.byte $99, $EB, $40
.byte $B0, $EB, $50
.byte $B1, $EB, $80
.byte $B2, $EB, $40
.byte $B4, $EB, $10
.byte $B5, $EB, $C0
.byte $BB, $EB, $C0
.byte $BE, $EB, $40
.byte $BF, $EB, $50
.byte $C1, $EB, $40
.byte $C2, $EB, $30
.byte $C3, $EB, $14
.byte $C4, $EB, $0C
.byte $C5, $EB, $0C
.byte $CF, $EB, $10
.byte $D0, $EB, $04
.byte $D3, $EB, $30
.byte $D5, $EB, $50
.byte $D7, $EB, $C0
.byte $D9, $EB, $40
.byte $EF, $EB, $AB
.byte $F0, $EB, $54
.byte $1C, $EC, $50
.byte $1D, $EC, $10
.byte $1E, $EC, $40
.byte $21, $EC, $01
.byte $22, $EC, $0B
.byte $23, $EC, $A0
.byte $25, $EC, $0F
.byte $26, $EC, $F1
.byte $27, $EC, $14
.byte $2A, $EC, $40
.byte $34, $EC, $01
.byte $35, $EC, $0B
.byte $36, $EC, $63
.byte $37, $EC, $88
.byte $38, $EC, $A1
.byte $39, $EC, $E5
.byte $3A, $EC, $5A
.byte $3B, $EC, $94
.byte $3C, $EC, $30
.byte $3D, $EC, $35
.byte $3E, $EC, $11
.byte $3F, $EC, $C0
.byte $40, $EC, $41
.byte $41, $EC, $14
.byte $42, $EC, $7F
.byte $43, $EC, $94
.byte $45, $EC, $01
.byte $48, $EC, $EA
.byte $49, $EC, $81
.byte $4A, $EC, $3D
.byte $4B, $EC, $0F
.byte $4C, $EC, $0C
.byte $4D, $EC, $55
.byte $51, $EC, $02
.byte $52, $EC, $08
.byte $53, $EC, $16
.byte $54, $EC, $24
.byte $55, $EC, $8C
.byte $56, $EC, $4E
.byte $57, $EC, $15
.byte $58, $EC, $55
.byte $59, $EC, $47
.byte $5A, $EC, $43
.byte $5B, $EC, $D1
.byte $5C, $EC, $34
.byte $5D, $EC, $3E
.byte $5E, $EC, $0C
.byte $5F, $EC, $73
.byte $60, $EC, $1C
.byte $62, $EC, $04
.byte $64, $EC, $09
.byte $65, $EC, $64
.byte $66, $EC, $90
.byte $67, $EC, $06
.byte $68, $EC, $79
.byte $69, $EC, $80
.byte $6A, $EC, $A6
.byte $6B, $EC, $59
.byte $6F, $EC, $14
.byte $70, $EC, $04
.byte $71, $EC, $15
.byte $72, $EC, $1F
.byte $73, $EC, $33
.byte $74, $EC, $20
.byte $75, $EC, $09
.byte $77, $EC, $40
.byte $78, $EC, $40
.byte $82, $EC, $07
.byte $83, $EC, $68
.byte $84, $EC, $C5
.byte $85, $EC, $FF
.byte $87, $EC, $55
.byte $DE, $EC, $40
.byte $DF, $EC, $90
.byte $E2, $EC, $30
.byte $E4, $EC, $04
.byte $E6, $EC, $11
.byte $E7, $EC, $0C
.byte $E8, $EC, $04
.byte $E9, $EC, $03
.byte $EB, $EC, $01
.byte $0E, $ED, $01
.byte $0F, $ED, $07
.byte $10, $ED, $1C
.byte $11, $ED, $B0
.byte $12, $ED, $0E
.byte $13, $ED, $34
.byte $14, $ED, $D0
.byte $15, $ED, $40
.byte $21, $ED, $01
.byte $22, $ED, $0A
.byte $23, $ED, $61
.byte $24, $ED, $C3
.byte $25, $ED, $30
.byte $26, $ED, $C0
.byte $28, $ED, $F1
.byte $29, $ED, $70
.byte $2A, $ED, $75
.byte $2B, $ED, $37
.byte $2C, $ED, $05
.byte $2D, $ED, $65
.byte $30, $ED, $5F
.byte $31, $ED, $CD
.byte $33, $ED, $35
.byte $34, $ED, $07
.byte $35, $ED, $01
.byte $37, $ED, $30
.byte $38, $ED, $D4
.byte $39, $ED, $40
.byte $3A, $ED, $44
.byte $3B, $ED, $11
.byte $3C, $ED, $F1
.byte $3D, $ED, $40
.byte $3E, $ED, $05
.byte $3F, $ED, $63
.byte $40, $ED, $97
.byte $41, $ED, $6C
.byte $42, $ED, $80
.byte $43, $ED, $07
.byte $45, $ED, $6B
.byte $46, $ED, $CF
.byte $47, $ED, $30
.byte $49, $ED, $C3
.byte $4A, $ED, $56
.byte $4B, $ED, $40
.byte $4C, $ED, $90
.byte $4D, $ED, $25
.byte $4E, $ED, $4C
.byte $4F, $ED, $04
.byte $50, $ED, $12
.byte $53, $ED, $50
.byte $55, $ED, $01
.byte $56, $ED, $6F
.byte $57, $ED, $91
.byte $5A, $ED, $95
.byte $5B, $ED, $6A
.byte $60, $ED, $05
.byte $61, $ED, $13
.byte $62, $ED, $03
.byte $63, $ED, $30
.byte $64, $ED, $5C
.byte $65, $ED, $04
.byte $66, $ED, $C1
.byte $67, $ED, $48
.byte $68, $ED, $B4
.byte $69, $ED, $40
.byte $70, $ED, $05
.byte $71, $ED, $6A
.byte $72, $ED, $90
.byte $74, $ED, $55
.byte $75, $ED, $FF
.byte $76, $ED, $01
.byte $77, $ED, $54
.byte $D9, $ED, $40
.byte $DB, $ED, $10
.byte $DC, $ED, $C0
.byte $DE, $ED, $70
.byte $DF, $ED, $04
.byte $E0, $ED, $1B
.byte $FC, $ED, $04
.byte $FD, $ED, $64
.byte $FE, $ED, $E0
.byte $FF, $ED, $C1
.byte $00, $EE, $D0
.byte $01, $EE, $D1
.byte $02, $EE, $40
.byte $0F, $EE, $50
.byte $10, $EE, $C0
.byte $12, $EE, $90
.byte $19, $EE, $01
.byte $1A, $EE, $46
.byte $1B, $EE, $E8
.byte $1C, $EE, $05
.byte $1D, $EE, $03
.byte $1E, $EE, $FF
.byte $1F, $EE, $C0
.byte $20, $EE, $CC
.byte $21, $EE, $5F
.byte $22, $EE, $03
.byte $23, $EE, $03
.byte $24, $EE, $04
.byte $25, $EE, $40
.byte $26, $EE, $C1
.byte $27, $EE, $60
.byte $28, $EE, $18
.byte $29, $EE, $56
.byte $2A, $EE, $25
.byte $2B, $EE, $5C
.byte $2C, $EE, $31
.byte $2D, $EE, $13
.byte $2E, $EE, $0C
.byte $2F, $EE, $47
.byte $30, $EE, $C1
.byte $31, $EE, $10
.byte $32, $EE, $C0
.byte $33, $EE, $34
.byte $34, $EE, $30
.byte $35, $EE, $14
.byte $36, $EE, $D4
.byte $38, $EE, $71
.byte $3A, $EE, $40
.byte $3B, $EE, $5C
.byte $3C, $EE, $04
.byte $3D, $EE, $05
.byte $3E, $EE, $80
.byte $3F, $EE, $E8
.byte $40, $EE, $D0
.byte $41, $EE, $40
.byte $45, $EE, $40
.byte $46, $EE, $D4
.byte $47, $EE, $30
.byte $48, $EE, $74
.byte $49, $EE, $40
.byte $4A, $EE, $C0
.byte $4B, $EE, $50
.byte $4D, $EE, $1A
.byte $4E, $EE, $65
.byte $4F, $EE, $50
.byte $50, $EE, $20
.byte $51, $EE, $48
.byte $52, $EE, $07
.byte $55, $EE, $40
.byte $5E, $EE, $02
.byte $5F, $EE, $1D
.byte $60, $EE, $E0
.byte $64, $EE, $54
.byte $65, $EE, $FD
.byte $66, $EE, $54
.byte $67, $EE, $03
.byte $68, $EE, $01
.byte $7B, $EE, $01
.byte $7C, $EE, $01
.byte $BF, $EE, $01
.byte $C2, $EE, $01
.byte $D0, $EE, $FF
.byte $EF, $EE, $10
.byte $F0, $EE, $10
.byte $07, $EF, $06
.byte $08, $EF, $2C
.byte $09, $EF, $83
.byte $0A, $EF, $43
.byte $0B, $EF, $17
.byte $0C, $EF, $49
.byte $0D, $EF, $11
.byte $0E, $EF, $C0
.byte $0F, $EF, $0D
.byte $10, $EF, $D4
.byte $11, $EF, $D2
.byte $12, $EF, $33
.byte $13, $EF, $C0
.byte $14, $EF, $07
.byte $16, $EF, $C1
.byte $18, $EF, $30
.byte $19, $EF, $C1
.byte $1A, $EF, $04
.byte $1B, $EF, $40
.byte $1C, $EF, $10
.byte $1D, $EF, $03
.byte $1F, $EF, $01
.byte $20, $EF, $01
.byte $21, $EF, $43
.byte $22, $EF, $40
.byte $26, $EF, $03
.byte $27, $EF, $03
.byte $29, $EF, $01
.byte $30, $EF, $01
.byte $31, $EF, $05
.byte $32, $EF, $04
.byte $3C, $EF, $40
.byte $3E, $EF, $10
.byte $3F, $EF, $C0
.byte $40, $EF, $10
.byte $41, $EF, $80
.byte $46, $EF, $40
.byte $47, $EF, $40
.byte $4B, $EF, $01
.byte $4C, $EF, $1E
.byte $4D, $EF, $A0
.byte $4E, $EF, $40
.byte $59, $EF, $40
.byte $5A, $EF, $D0
.byte $5B, $EF, $74
.byte $5C, $EF, $10
.byte $5D, $EF, $08
.byte $5E, $EF, $01
.byte $5F, $EF, $01
.byte $60, $EF, $08
.byte $61, $EF, $04
.byte $62, $EF, $08
.byte $63, $EF, $04
.byte $65, $EF, $01
.byte $66, $EF, $05
.byte $68, $EF, $04
.byte $69, $EF, $18
.byte $6A, $EF, $64
.byte $6B, $EF, $80
.byte $6D, $EF, $80
.byte $6E, $EF, $40
.byte $6F, $EF, $20
.byte $70, $EF, $18
.byte $71, $EF, $05
.byte $72, $EF, $03
.byte $73, $EF, $01
.byte $7E, $EF, $04
.byte $7F, $EF, $10
.byte $80, $EF, $14
.byte $AF, $EF, $A5
.byte $B0, $EF, $5A
.byte $B3, $EF, $40
.byte $B4, $EF, $C0
.byte $B5, $EF, $40
.byte $B6, $EF, $10
.byte $B8, $EF, $30
.byte $B9, $EF, $10
.byte $BB, $EF, $04
.byte $BC, $EF, $0C
.byte $BD, $EF, $04
.byte $C0, $EF, $FC
.byte $F5, $EF, $05
.byte $F6, $EF, $2C
.byte $F7, $EF, $BD
.byte $F8, $EF, $C0
.byte $F9, $EF, $54
.byte $FA, $EF, $51
.byte $FB, $EF, $3B
.byte $FC, $EF, $73
.byte $FD, $EF, $59
.byte $FF, $EF, $40
.byte $01, $F0, $50
.byte $02, $F0, $45
.byte $03, $F0, $15
.byte $04, $F0, $01
.byte $05, $F0, $01
.byte $18, $F0, $10
.byte $19, $F0, $B1
.byte $1A, $F0, $46
.byte $1B, $F0, $58
.byte $1C, $F0, $40
.byte $1E, $F0, $0A
.byte $1F, $F0, $17
.byte $20, $F0, $08
.byte $21, $F0, $13
.byte $22, $F0, $0C
.byte $23, $F0, $10
.byte $24, $F0, $30
.byte $26, $F0, $03
.byte $27, $F0, $0D
.byte $28, $F0, $24
.byte $2A, $F0, $01
.byte $2B, $F0, $03
.byte $2F, $F0, $02
.byte $3A, $F0, $1B
.byte $3B, $F0, $A4
.byte $3C, $F0, $40
.byte $63, $F0, $40
.byte $64, $F0, $80
.byte $65, $F0, $20
.byte $67, $F0, $14
.byte $68, $F0, $08
.byte $69, $F0, $03
.byte $81, $F0, $30
.byte $82, $F0, $08
.byte $83, $F0, $15
.byte $84, $F0, $02
.byte $85, $F0, $30
.byte $87, $F0, $10
.byte $89, $F0, $04
.byte $8A, $F0, $0C
.byte $8B, $F0, $04
.byte $8C, $F0, $01
.byte $8D, $F0, $03
.byte $8E, $F0, $01
.byte $9F, $F0, $40
.byte $A0, $F0, $D5
.byte $A1, $F0, $6B
.byte $A2, $F0, $01
.byte $E4, $F0, $10
.byte $E5, $F0, $90
.byte $E6, $F0, $80
.byte $EA, $F0, $40
.byte $EB, $F0, $10
.byte $EC, $F0, $50
.byte $F2, $F0, $54
.byte $F3, $F0, $14
.byte $F4, $F0, $40
.byte $06, $F1, $14
.byte $07, $F1, $1B
.byte $08, $F1, $20
.byte $09, $F1, $90
.byte $0A, $F1, $40
.byte $0C, $F1, $15
.byte $0D, $F1, $43
.byte $0E, $F1, $10
.byte $0F, $F1, $5C
.byte $10, $F1, $5F
.byte $11, $F1, $D0
.byte $12, $F1, $07
.byte $13, $F1, $44
.byte $14, $F1, $5C
.byte $15, $F1, $97
.byte $16, $F1, $18
.byte $17, $F1, $0F
.byte $18, $F1, $65
.byte $19, $F1, $01
.byte $1A, $F1, $37
.byte $1B, $F1, $F0
.byte $1C, $F1, $5D
.byte $1D, $F1, $14
.byte $1E, $F1, $A0
.byte $1F, $F1, $40
.byte $28, $F1, $05
.byte $29, $F1, $6A
.byte $2A, $F1, $90
.byte $5A, $F1, $80
.byte $5B, $F1, $69
.byte $5C, $F1, $16
.byte $5F, $F1, $01
.byte $60, $F1, $03
.byte $62, $F1, $01
.byte $63, $F1, $08
.byte $64, $F1, $04
.byte $65, $F1, $10
.byte $67, $F1, $10
.byte $6A, $F1, $20
.byte $6B, $F1, $10
.byte $6D, $F1, $10
.byte $6F, $F1, $30
.byte $70, $F1, $14
.byte $71, $F1, $08
.byte $72, $F1, $02
.byte $73, $F1, $01
.byte $74, $F1, $40
.byte $75, $F1, $C0
.byte $76, $F1, $60
.byte $77, $F1, $14
.byte $78, $F1, $09
.byte $79, $F1, $02
.byte $7F, $F1, $40
.byte $80, $F1, $C0
.byte $81, $F1, $40
.byte $83, $F1, $10
.byte $84, $F1, $30
.byte $85, $F1, $14
.byte $86, $F1, $0C
.byte $87, $F1, $04
.byte $88, $F1, $01
.byte $89, $F1, $03
.byte $8A, $F1, $01
.byte $90, $F1, $02
.byte $91, $F1, $A9
.byte $92, $F1, $54
.byte $EE, $F1, $01
.byte $F1, $F1, $04
.byte $F2, $F1, $04
.byte $F4, $F1, $06
.byte $F5, $F1, $19
.byte $F6, $F1, $AA
.byte $F7, $F1, $62
.byte $F8, $F1, $B9
.byte $F9, $F1, $96
.byte $FA, $F1, $10
.byte $FB, $F1, $0D
.byte $FC, $F1, $34
.byte $FD, $F1, $70
.byte $FE, $F1, $D4
.byte $FF, $F1, $09
.byte $01, $F2, $23
.byte $02, $F2, $01
.byte $03, $F2, $C0
.byte $04, $F2, $30
.byte $05, $F2, $50
.byte $06, $F2, $50
.byte $08, $F2, $10
.byte $0A, $F2, $40
.byte $11, $F2, $01
.byte $12, $F2, $01
.byte $15, $F2, $01
.byte $16, $F2, $11
.byte $17, $F2, $46
.byte $18, $F2, $E9
.byte $19, $F2, $40
.byte $27, $F2, $01
.byte $28, $F2, $04
.byte $29, $F2, $05
.byte $4C, $F2, $80
.byte $4E, $F2, $C0
.byte $4F, $F2, $40
.byte $63, $F2, $80
.byte $64, $F2, $67
.byte $65, $F2, $18
.byte $6A, $F2, $90
.byte $6B, $F2, $70
.byte $6C, $F2, $14
.byte $6D, $F2, $1E
.byte $6E, $F2, $01
.byte $6F, $F2, $01
.byte $70, $F2, $01
.byte $71, $F2, $14
.byte $7B, $F2, $40
.byte $7C, $F2, $C0
.byte $7E, $F2, $C0
.byte $7F, $F2, $40
.byte $B1, $F2, $01
.byte $B2, $F2, $07
.byte $B4, $F2, $03
.byte $B5, $F2, $01
.byte $B6, $F2, $14
.byte $B7, $F2, $10
.byte $D2, $F2, $10
.byte $D3, $F2, $10
.byte $DB, $F2, $19
.byte $DC, $F2, $03
.byte $DD, $F2, $4F
.byte $DE, $F2, $05
.byte $E1, $F2, $D7
.byte $E2, $F2, $E9
.byte $E3, $F2, $CB
.byte $E4, $F2, $30
.byte $E5, $F2, $FC
.byte $E6, $F2, $C0
.byte $E7, $F2, $08
.byte $E8, $F2, $FB
.byte $E9, $F2, $FF
.byte $EA, $F2, $F5
.byte $EB, $F2, $50
.byte $F0, $F2, $02
.byte $F1, $F2, $09
.byte $F2, $F2, $B4
.byte $F3, $F2, $41
.byte $F4, $F2, $06
.byte $F5, $F2, $02
.byte $F6, $F2, $05
.byte $FC, $F2, $10
.byte $FD, $F2, $50
.byte $FE, $F2, $40
.byte $FF, $F2, $04
.byte $00, $F3, $54
.byte $01, $F3, $50
.byte $04, $F3, $05
.byte $05, $F3, $50
.byte $06, $F3, $3F
.byte $07, $F3, $D5
.byte $08, $F3, $40
.byte $54, $F3, $F8
.byte $55, $F3, $05
.byte $56, $F3, $05
.byte $57, $F3, $68
.byte $58, $F3, $40
.byte $59, $F3, $20
.byte $5A, $F3, $0C
.byte $5B, $F3, $02
.byte $5C, $F3, $01
.byte $5D, $F3, $02
.byte $5E, $F3, $99
.byte $5F, $F3, $64
.byte $A1, $F3, $40
.byte $A2, $F3, $D0
.byte $A4, $F3, $90
.byte $CE, $F3, $06
.byte $CF, $F3, $1F
.byte $D0, $F3, $BD
.byte $D1, $F3, $34
.byte $D2, $F3, $D0
.byte $D6, $F3, $45
.byte $D7, $F3, $AF
.byte $D8, $F3, $3E
.byte $D9, $F3, $94
.byte $DB, $F3, $01
.byte $DC, $F3, $06
.byte $DD, $F3, $08
.byte $DE, $F3, $20
.byte $DF, $F3, $91
.byte $E0, $F3, $44
.byte $E2, $F3, $21
.byte $E3, $F3, $F4
.byte $E4, $F3, $90
.byte $E5, $F3, $40
.byte $E7, $F3, $01
.byte $E9, $F3, $01
.byte $ED, $F3, $02
.byte $EE, $F3, $09
.byte $EF, $F3, $22
.byte $F0, $F3, $29
.byte $F3, $F3, $04
.byte $F4, $F3, $52
.byte $F5, $F3, $F9
.byte $F6, $F3, $50
.byte $BD, $F4, $90
.byte $BE, $F4, $D0
.byte $BF, $F4, $40
.byte $C3, $F4, $01
.byte $C4, $F4, $03
.byte $C5, $F4, $19
.byte $C6, $F4, $70
.byte $C7, $F4, $30
.byte $C8, $F4, $74
.byte $C9, $F4, $50
.byte $CA, $F4, $C2
.byte $CB, $F4, $46
.byte $CC, $F4, $14
.byte $CD, $F4, $15
.byte $CE, $F4, $10
.byte $CF, $F4, $B5
.byte $D0, $F4, $E0
.byte $D1, $F4, $41
.byte $D2, $F4, $03
.byte $D3, $F4, $53
.byte $D4, $F4, $10
.byte $D5, $F4, $46
.byte $D7, $F4, $0E
.byte $D8, $F4, $09
.byte $D9, $F4, $01
.byte $DA, $F4, $07
.byte $DB, $F4, $13
.byte $DC, $F4, $4F
.byte $DD, $F4, $F2
.byte $DE, $F4, $78
.byte $DF, $F4, $D0
.byte $E2, $F4, $01
.byte $E3, $F4, $1B
.byte $E4, $F4, $E5
.byte $96, $F5, $01
.byte $98, $F5, $01
.byte $AE, $F5, $02
.byte $AF, $F5, $05
.byte $B0, $F5, $18
.byte $B1, $F5, $31
.byte $B2, $F5, $55
.byte $B3, $F5, $C0
.byte $B4, $F5, $44
.byte $B5, $F5, $02
.byte $B6, $F5, $01
.byte $B7, $F5, $08
.byte $B8, $F5, $15
.byte $B9, $F5, $AB
.byte $BA, $F5, $22
.byte $BB, $F5, $B8
.byte $BC, $F5, $91
.byte $BD, $F5, $47
.byte $BE, $F5, $2E
.byte $BF, $F5, $C8
.byte $C0, $F5, $A0
.byte $C1, $F5, $41
.byte $C2, $F5, $1B
.byte $C3, $F5, $BF
.byte $C4, $F5, $E7
.byte $C5, $F5, $4D
.byte $C6, $F5, $71
.byte $C7, $F5, $86
.byte $C8, $F5, $18
.byte $C9, $F5, $B5
.byte $CA, $F5, $FE
.byte $CB, $F5, $04
.byte $CC, $F5, $10
.byte $CD, $F5, $47
.byte $CE, $F5, $13
.byte $CF, $F5, $14
.byte $D1, $F5, $05
.byte $D2, $F5, $6F
.byte $D3, $F5, $80
.byte $D4, $F5, $10
.byte $D5, $F5, $05
.byte $86, $F6, $40
.byte $87, $F6, $41
.byte $89, $F6, $01
.byte $93, $F6, $01
.byte $98, $F6, $04
.byte $9B, $F6, $03
.byte $9C, $F6, $5D
.byte $9D, $F6, $F5
.byte $9E, $F6, $11
.byte $9F, $F6, $40
.byte $A0, $F6, $02
.byte $A1, $F6, $08
.byte $A2, $F6, $25
.byte $A3, $F6, $50
.byte $A4, $F6, $80
.byte $A5, $F6, $02
.byte $A6, $F6, $08
.byte $A7, $F6, $63
.byte $A8, $F6, $B9
.byte $A9, $F6, $D0
.byte $AA, $F6, $06
.byte $AB, $F6, $2E
.byte $AC, $F6, $B8
.byte $AD, $F6, $D0
.byte $AE, $F6, $40
.byte $B0, $F6, $16
.byte $B1, $F6, $8D
.byte $B2, $F6, $F4
.byte $B3, $F6, $D1
.byte $B4, $F6, $4A
.byte $B5, $F6, $20
.byte $B6, $F6, $C1
.byte $B7, $F6, $13
.byte $B8, $F6, $1D
.byte $B9, $F6, $A4
.byte $BA, $F6, $41
.byte $BB, $F6, $1A
.byte $BC, $F6, $A4
.byte $BD, $F6, $5A
.byte $BE, $F6, $E5
.byte $BF, $F6, $55
.byte $C0, $F6, $50
.byte $C1, $F6, $45
.byte $C2, $F6, $E9
.byte $C3, $F6, $FC
.byte $C4, $F6, $F0
.byte $C5, $F6, $40
.byte $C6, $F6, $0E
.byte $C7, $F6, $14
.byte $76, $F7, $50
.byte $77, $F7, $10
.byte $78, $F7, $40
.byte $7F, $F7, $04
.byte $81, $F7, $10
.byte $85, $F7, $0C
.byte $86, $F7, $30
.byte $87, $F7, $40
.byte $89, $F7, $01
.byte $8A, $F7, $D3
.byte $8B, $F7, $4F
.byte $8C, $F7, $15
.byte $8D, $F7, $61
.byte $8E, $F7, $1B
.byte $8F, $F7, $03
.byte $90, $F7, $CD
.byte $91, $F7, $04
.byte $92, $F7, $50
.byte $93, $F7, $06
.byte $94, $F7, $68
.byte $95, $F7, $8B
.byte $96, $F7, $71
.byte $97, $F7, $D4
.byte $98, $F7, $40
.byte $9D, $F7, $02
.byte $9E, $F7, $0D
.byte $9F, $F7, $70
.byte $A0, $F7, $80
.byte $A1, $F7, $01
.byte $A2, $F7, $18
.byte $A3, $F7, $AD
.byte $A4, $F7, $64
.byte $A5, $F7, $50
.byte $A6, $F7, $C0
.byte $A7, $F7, $41
.byte $A8, $F7, $06
.byte $A9, $F7, $38
.byte $AA, $F7, $C0
.byte $AC, $F7, $11
.byte $AD, $F7, $EE
.byte $AE, $F7, $01
.byte $AF, $F7, $57
.byte $B0, $F7, $FD
.byte $B1, $F7, $AA
.byte $B2, $F7, $41
.byte $B4, $F7, $45
.byte $B5, $F7, $BA
.byte $B6, $F7, $40
.byte $C0, $F7, $14
.byte $C2, $F7, $14
.byte $F1, $F7, $06
.byte $F2, $F7, $03
.byte $F3, $F7, $05
.byte $38, $F8, $06
.byte $39, $F8, $0F
.byte $3A, $F8, $0D
.byte $3B, $F8, $04
.byte $77, $F8, $02
.byte $78, $F8, $1B
.byte $79, $F8, $BD
.byte $7A, $F8, $34
.byte $7B, $F8, $D0
.byte $7C, $F8, $55
.byte $7D, $F8, $B0
.byte $7E, $F8, $F1
.byte $7F, $F8, $04
.byte $80, $F8, $50
.byte $81, $F8, $01
.byte $82, $F8, $6B
.byte $83, $F8, $8F
.byte $84, $F8, $B1
.byte $85, $F8, $14
.byte $86, $F8, $40
.byte $8A, $F8, $06
.byte $8B, $F8, $19
.byte $8C, $F8, $A0
.byte $8D, $F8, $40
.byte $90, $F8, $10
.byte $91, $F8, $90
.byte $92, $F8, $80
.byte $95, $F8, $06
.byte $96, $F8, $79
.byte $97, $F8, $80
.byte $9B, $F8, $06
.byte $9C, $F8, $B9
.byte $9D, $F8, $40
.byte $9E, $F8, $56
.byte $9F, $F8, $FC
.byte $A1, $F8, $FE
.byte $A2, $F8, $54
.byte $A3, $F8, $55
.byte $A4, $F8, $F0
.byte $A6, $F8, $40
.byte $A7, $F8, $0F
.byte $A8, $F8, $10
.byte $A9, $F8, $05
.byte $E0, $F8, $64
.byte $E1, $F8, $F4
.byte $E2, $F8, $D0
.byte $E3, $F8, $40
.byte $28, $F9, $40
.byte $29, $F9, $40
.byte $64, $F9, $10
.byte $65, $F9, $40
.byte $66, $F9, $D0
.byte $67, $F9, $C0
.byte $68, $F9, $40
.byte $6A, $F9, $01
.byte $6B, $F9, $68
.byte $6C, $F9, $FC
.byte $6D, $F9, $C5
.byte $6E, $F9, $50
.byte $70, $F9, $16
.byte $71, $F9, $B2
.byte $72, $F9, $F4
.byte $73, $F9, $50
.byte $76, $F9, $01
.byte $77, $F9, $06
.byte $78, $F9, $29
.byte $79, $F9, $85
.byte $7A, $F9, $04
.byte $7B, $F9, $10
.byte $7C, $F9, $41
.byte $7E, $F9, $01
.byte $7F, $F9, $42
.byte $80, $F9, $49
.byte $81, $F9, $34
.byte $82, $F9, $5B
.byte $83, $F9, $65
.byte $84, $F9, $6B
.byte $85, $F9, $D4
.byte $86, $F9, $41
.byte $87, $F9, $01
.byte $89, $F9, $04
.byte $8A, $F9, $6E
.byte $8B, $F9, $84
.byte $8C, $F9, $10
.byte $8D, $F9, $02
.byte $8E, $F9, $96
.byte $8F, $F9, $3D
.byte $90, $F9, $FD
.byte $91, $F9, $40
.byte $92, $F9, $14
.byte $93, $F9, $40
.byte $94, $F9, $10
.byte $96, $F9, $C4
.byte $97, $F9, $31
.byte $98, $F9, $1B
.byte $99, $F9, $41
.byte $B5, $F9, $01
.byte $B7, $F9, $04
.byte $B8, $F9, $11
.byte $B9, $F9, $14
.byte $58, $FA, $05
.byte $59, $FA, $11
.byte $5A, $FA, $84
.byte $5B, $FA, $D0
.byte $5C, $FA, $40
.byte $5E, $FA, $14
.byte $5F, $FA, $84
.byte $60, $FA, $D0
.byte $61, $FA, $40
.byte $64, $FA, $15
.byte $65, $FA, $38
.byte $66, $FA, $99
.byte $67, $FA, $11
.byte $68, $FA, $0F
.byte $69, $FA, $14
.byte $6A, $FA, $40
.byte $6B, $FA, $02
.byte $6C, $FA, $48
.byte $6D, $FA, $64
.byte $6E, $FA, $94
.byte $6F, $FA, $50
.byte $70, $FA, $01
.byte $71, $FA, $7C
.byte $72, $FA, $8D
.byte $73, $FA, $B1
.byte $74, $FA, $14
.byte $75, $FA, $01
.byte $76, $FA, $15
.byte $78, $FA, $0F
.byte $79, $FA, $F4
.byte $7A, $FA, $50
.byte $7B, $FA, $07
.byte $7C, $FA, $69
.byte $7D, $FA, $90
.byte $7E, $FA, $A8
.byte $7F, $FA, $01
.byte $80, $FA, $4D
.byte $81, $FA, $64
.byte $82, $FA, $29
.byte $83, $FA, $ED
.byte $84, $FA, $AD
.byte $85, $FA, $40
.byte $86, $FA, $3D
.byte $87, $FA, $04
.byte $88, $FA, $C0
.byte $89, $FA, $74
.byte $8A, $FA, $31
.byte $8B, $FA, $15
.byte $93, $FA, $01
.byte $95, $FA, $01
.byte $A5, $FA, $90
.byte $A6, $FA, $34
.byte $A7, $FA, $F4
.byte $A8, $FA, $50
.byte $00, $00, $00
