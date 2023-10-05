.segment "RINGBUFFER"

; ----------------------------------------------------------------------------------------------------

.define rbRingBufferSize							$0800
.define rbSliceSize									$0400
.define rbBaseMem									$050000

.struct RBDATA
	start		.dword
.endstruct

rbGetSample

		lda rbChannel									; 0, 1, 2, 3



														; calculate what the start point in ringbuffer should be if
														; we want the endpoint to be at the end of the ringbuffer
		lda samplelength+0								; remainder = sample length & (rbRingBufferSize-1)
		and #<(rbRingBufferSize - 1)
		sta rbgs0+1
		lda samplelength+1
		and #>(rbRingBufferSize - 1)
		sta rbgs1+1

		sec												; start in ringbuffer = rbRingBufferSize - remainder
		lda #<rbRingBufferSize
rbgs0	sbc #$00
		sta rbSliceStartLo+0
		lda #>rbRingBufferSize
rbgs1	sbc #$00
		sta rbSliceStartHi+0

		lda rbSliceStartLo+0							; calculate second slice start
		sta rbSliceStartLo+1
		clc
		lda rbSliceStartHi+0
		adc #>(rbSliceSize)
		and #>(rbRingBufferSize-1)
		sta rbSliceStartHi+1

		lda rbSliceStartLo+0							; set up initial pointers to ringbuffer and attic
		sta rbSliceStart+0
		lda rbSliceStartHi+0
		sta rbSliceStart+1

		lda samplestart+0
		sta rbDMAsrc1+0
		lda samplestart+1
		sta rbDMAsrc1+1

														; fall through to doDMA

; ----------------------------------------------------------------------------------------------------

doDMA	lda #$00
		sta $d020

		lda rbSliceStart+0								; copy first slice start to dst1
		sta rbDMAdst1+0
		lda rbSliceStart+1
		sta rbDMAdst1+1

		lda #<($0000)									; assume that second slice starts at $0000. we'll correct this later if not
		sta rbDMAdst2+0
		lda #>($0000)
		sta rbDMAdst2+1

		sec
		lda #<rbRingBufferSize
		sbc rbSliceStart+0
		sta rbDMAsiz1+0
		lda #>rbRingBufferSize
		sbc rbSliceStart+1
		sta rbDMAsiz1+1

		lda #%00000100									; turn on chain
		sta rbDMAcmd1

		cmp #>rbSliceSize								; is calculated slice size bigger than $0400?
		bmi :+

		lda #<rbSliceSize								; yes, clamp to $0400
		sta rbDMAsiz1+0
		lda #>rbSliceSize
		sta rbDMAsiz1+1

		lda #%00000000									; we now know that the second slice has no size, so turn off chain
		sta rbDMAcmd1

:		sec												; size of second slice is $0400-firstslicesize
		lda #<rbSliceSize
		sbc rbDMAsiz1+0
		sta rbDMAsiz2+0
		lda #>rbSliceSize
		sbc rbDMAsiz1+1
		sta rbDMAsiz2+1

		clc
		lda rbDMAsrc1+0
		adc rbDMAsiz1+0
		sta rbDMAsrc2+0
		lda rbDMAsrc1+1
		adc rbDMAsiz1+1
		sta rbDMAsrc2+1
		lda rbDMAsrc1+2
		adc #$00
		sta rbDMAsrc2+2

		;jsr debugtoc000

		DMA_RUN_JOB rbDMACopy

		lda #$00
		sta $d020

		rts

; ----------------------------------------------------------------------------------------------------

debugtoc000

		lda rbDMAdst1+0
		sta $c000
		lda rbDMAdst1+1
		sta $c001
		lda rbDMAsiz1+0
		sta $c002
		lda rbDMAsiz1+1
		sta $c003

		lda rbDMAdst2+0
		sta $c004
		lda rbDMAdst2+1
		sta $c005
		lda rbDMAsiz2+0
		sta $c006
		lda rbDMAsiz2+1
		sta $c007

		rts

; ----------------------------------------------------------------------------------------------------

rbUpdate

		lda rbChannel
		asl
		asl
		asl
		asl
		tax

		lda #%00000000									; disable audio DMA, so registers can latch
		sta $d711

		lda $d72a,x
		sta rbAudioPos+0
		lda $d72b,x
		sta rbAudioPos+1

		lda #%10000000									; enable audio DMA
		sta $d711

		lda #$00
		sta rbus1+1
		sta rbus2+1
		
		lda rbAudioPos+1
		cmp rbSliceStartHi+0
		bcc :++
		bne :+
		lda rbAudioPos+0
		cmp rbSliceStartLo+0
		bcc :++
:		jmp :++
:		lda #$01
		sta rbus1+1
:		lda rbAudioPos+1
		cmp rbSliceStartHi+1
		bcc :++
		bne :+
		lda rbAudioPos+0
		cmp rbSliceStartLo+1
		bcc :++
:		jmp :++
:		lda #$01
		sta rbus2+1
:
rbus1	lda #$00
rbus2	eor #$00
		sta	rbSliceIndex

		lda rbSliceIndex
		cmp rbPrevSliceIndex
		bne rbFetchNextSlice
		rts

rbFetchNextSlice

		ldx rbSliceIndex
		lda rbSliceStartLo,x
		sta rbSliceStart+0
		lda rbSliceStartHi,x
		sta rbSliceStart+1

		clc
		lda rbDMAsrc1+0
		adc #<rbSliceSize
		sta rbDMAsrc1+0
		lda rbDMAsrc1+1
		adc #>rbSliceSize
		sta rbDMAsrc1+1
		lda rbDMAsrc1+2
		adc #$00
		sta rbDMAsrc1+2

		lda rbSliceIndex
		sta rbPrevSliceIndex

		jsr doDMA

		rts

; ----------------------------------------------------------------------------------------------------

rbDMACopy		.byte $0b										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80, $80									; source megabyte ($8000000 >> 20) ($80 is attic ram)
				.byte $81, $00									; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $82, $00									; Source skip rate (256ths of bytes)
				.byte $83, $01									; Source skip rate (whole bytes)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, $01									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 12 byte DMA List structure starts here
rbDMAcmd1		.byte %00000100									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

rbDMAsiz1		.word $0400										; Count LSB + Count MSB

rbDMAsrc1		.word $0000										; Source Address LSB + Source Address MSB
				.byte ($000000 >> 16) | (%0000 << 4)			; Source Address BANK and FLAGS
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

rbDMAdst1		.word $0400										; Destination Address LSB + Destination Address MSB
				.byte (rbBaseMem >> 16) | (%0000 << 4)			; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.byte %00000000									; Command MSB
																;     0–1 Addressing mode of source (Only Linear, Modulo and Hold implemented at time of writing)
																;         %00 (0) Linear (normal) addressing
																;         %01 (1) Modulo (rectangular) addressing
																;         %10 (2) Hold (constant address)
																;         %11 (3) XY MOD (bitmap rectangular) addressing
																;     2–3 Addressing mode of destination (Only Linear, Modulo and Hold implemented at time of writing)
																;         %00 (0) Linear (normal) addressing
																;         %01 (1) Modulo (rectangular) addressing
																;         %10 (2) Hold (constant address)
																;         %11 (3) XY MOD (bitmap rectangular) addressing
																;     4-7 RESESRVED. Always set to 0’s
				.word $0000										; Modulo LSB / Mode + Modulo MSB / Mode

				.byte $0b										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80, $80									; source megabyte ($8000000 >> 20) ($80 is attic ram)
				.byte $81, $00									; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $82, $00									; Source skip rate (256ths of bytes)
				.byte $83, $01									; Source skip rate (whole bytes)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, $01									; Destination skip rate (whole bytes)

				.byte $00										; No more options

rbDMAcmd2		.byte %00000000									; second part of slice copy
rbDMAsiz2		.word $0010
rbDMAsrc2		.word $0000
				.byte ($000000 >> 16) | (%0000 << 4)
rbDMAdst2		.word $0000
				.byte (rbBaseMem >> 16) | (%0000 << 4)
				.byte %00000000
				.word $0000

				.byte %00000000

; ----------------------------------------------------------------------------------------------------

rbSliceStartLo
.byte $00
.byte $00
rbSliceStartHi
.byte $00
.byte $00

rbAudioPos
.word $0675

rbSliceStart
.word $0000

rbSliceIndex
.byte $00

rbPrevSliceIndex
.byte $00

rbChannel
.byte $00

/*
Instruments:                  start    end      length   repeat   fint volm
                              -------- -------- -------- -------- ---- ----
0x01 [   song composed by:  ] 0x000000 0x008E16 0x008E16 0x007834 0x00 0x40
0x02 [#### romeo knight ### ] 0x008ED8 0x00E462 0x00558A 0x00D0F8 0x00 0x40
0x03 [                      ] 0x00E51C 0x015638 0x00711C 0x013CA4 0x00 0x40
0x04 [                      ] 0x015638 0x01A44A 0x004E12 0x018C1E 0x00 0x40
0x05 [                      ] 0x01A492 0x01CE28 0x002996 0x01C83C 0x00 0x40
0x06 [                      ] 0x01CE56 0x01F194 0x00233E 0x000000 0x00 0x40
0x07 [                      ] 0x01F194 0x025E08 0x006C74 0x0240F4 0x00 0x40
0x08 [                      ] 0x025E82 0x02AF9A 0x005118 0x029C40 0x00 0x40
0x09 [                      ] 0x02B086 0x02FEF8 0x004E72 0x02EB4A 0x00 0x40
0x0A [                      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
0x0B [                      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
0x0C [                      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
0x0D [                      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
0x0E [                      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
0x0F [                      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
0x10 [                      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
0x11 [                      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
0x12 [                      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
0x13 [                      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
0x14 [                      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
0x15 [                      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
0x16 [                      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
0x17 [                      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
0x18 [                      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
0x19 [                      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
0x1A [                      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
0x1B [                      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
0x1C [                      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
0x1D [                      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
0x1E [                      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
0x1F [    end of line.      ] 0x02FEFE 0x02FEFE 0x000000 0x000000 0x00 0x00
*/

.define SSTART $000000
samplestart
.byte <.loword(SSTART)
.byte >.loword(SSTART)
.byte <.hiword(SSTART)

.define SEND $007f00
sampleend
.byte <.loword(SSTART)
.byte >.loword(SSTART)
.byte <.hiword(SSTART)

.define SLEN $007fff
samplelength
.byte <.loword(SLEN)
.byte >.loword(SLEN)
.byte <.hiword(SLEN)

.define SREP $00D0F8
samplerepeat
.byte <.loword(SREP)
.byte >.loword(SREP)
.byte <.hiword(SREP)

.align 256
sampleremap
.repeat 256, I
.byte ((128 + I) .MOD 256)
.endrepeat