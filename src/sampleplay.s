mpChannel
.byte $00

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

		lda #$d1
		sta $d724,x										; CHXFREQL    Audio DMA channel X frequency LSB
		lda #$22
		sta $d725,x										; CHXFREQC    Audio DMA channel X frequency middle byte
		lda #$00
		sta $d726,x										; CHXFREQM    Audio DMA channel X frequency MSB

		lda #$40										; set volume
		sta $d729,x										; CHXVOLUME

		lda #%10000000									; enable audio DMA
		sta $d711										; D711                      AUDEN BLKD AUDWRBLK NOMIX – AUDBLKTO

		lda #%11000010									; play DMA (CHXEN)  with loop enabled (CHXLOOP), 8 bit samples (CHXSBITS) (11=16, 10=8, 01=upper nybl, 00=lower nybl)
		sta $d720,x										; D720      CHXEN CHXLOOP CHXSGN CHXSINE CHXSTP – CHXSBITS

		rts

; ----------------------------------------------------------------------------------------------------

mpDebugSample

		lda mpChannel
		asl
		asl
		asl
		asl
		tax

		lda #%00000000									; disable audio DMA
		sta $d711										; D711                      AUDEN BLKD AUDWRBLK NOMIX – AUDBLKTO

		lda $d72a,x
		sta $12
		lda $d72b,x
		sta $13
		lda $d72c,x
		sta $14
		lda #$00
		sta $15

		lda #%10000000									; enable audio DMA
		sta $d711										; D711                      AUDEN BLKD AUDWRBLK NOMIX – AUDBLKTO

		DMA_RUN_JOB mpClearScreenRamJob

		lda #$00
		sta dss+1
		lda #$30
		sta dss+2

		ldx #$00
		ldz #00
:		lda [$12],z
		tay
		lda sampleremap,y
		lsr
		lsr
		lsr
		tay
		lda #$5d
dss		sta $3000,y
		clc
		lda dss+1
		adc #80
		sta dss+1
		lda dss+2
		adc #$00
		sta dss+2

		inz
		inz
		inz
		inz
		inz
		inz
		inz
		inz
		inx
		cpx #25
		bne :-

		rts

; ----------------------------------------------------------------------------------------------------

mpClearScreenRamJob
		DMA_HEADER $00, $3000 >> 20
		DMA_FILL_JOB $20, $3000, 80*25, 0

; ----------------------------------------------------------------------------------------------------