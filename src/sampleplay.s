.segment "SAMPLEPLAY"

mpChannel
.byte $00

mpVolume
.byte $40

mpInit

		lda #$40
		sta mpVolume
		rts

mpUpdateVolume

		lda mpVolume									; set volume
		sta $d729+0*16									; CHXVOLUME
		sta $d729+1*16									; CHXVOLUME
		sta $d729+2*16									; CHXVOLUME
		sta $d729+3*16									; CHXVOLUME
		rts

mpPlaySample

		lda mpChannel
		asl
		asl
		asl
		asl
		tax

		; TODO - we should have 4 ringbuffers instead of one

		lda rbSliceStart+0								; set start in ringbuffer
		sta $d72a,x										; CHXCURADDRL
		lda rbSliceStart+1
		sta $d72b,x										; CHXCURADDRC
		lda #<.hiword(rbBaseMem)
		sta $d72c,x										; CHXCURADDRM

		lda #<.loword(rbBaseMem)						; set repeat in ringbuffer to 0
		sta $d721,x										; CHXBADDRL
		lda #>.loword(rbBaseMem)
		sta $d722,x										; CHXBADDRC
		lda #<.hiword(rbBaseMem)
		sta $d723,x										; CHXBADDRM

		lda #<(rbRingBufferSize-1)						; set end address to end of ringbuffer
		sta $d727,x										; CHXTADDRL
		lda #>(rbRingBufferSize-1)
		sta $d728,x										; CHXTADDRM

		; 8272 samples per second?

		; 0x1000000 = 40.5Mhz ( 2 ^ 24)
		; 0x1000000 / $1ad1 = 2443Hz

														; 23c8 seems ok for xemu
														; 23a8 seems ok for real HW

		lda #$a8										; d1
		sta $d724,x										; CHXFREQL    Audio DMA channel X frequency LSB
		lda #$23
		sta $d725,x										; CHXFREQC    Audio DMA channel X frequency middle byte
		lda #$00
		sta $d726,x										; CHXFREQM    Audio DMA channel X frequency MSB

		lda mpVolume									; set volume
		sta $d729,x										; CHXVOLUME

		lda #%10000000									; enable audio DMA
		sta $d711										; D711                      AUDEN BLKD AUDWRBLK NOMIX – AUDBLKTO

		lda #%11000010									; play DMA (CHXEN)  with loop enabled (CHXLOOP), 8 bit samples (CHXSBITS) (11=16, 10=8, 01=upper nybl, 00=lower nybl)
		sta $d720,x										; D720      CHXEN CHXLOOP CHXSGN CHXSINE CHXSTP – CHXSBITS

		rts

; ----------------------------------------------------------------------------------------------------