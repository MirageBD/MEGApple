.segment "SDC"

; ----------------------------------------------------------------------------------------------------

.define sdc_transferbuffer	$0200
.define sdc_sectorbuffer	$0400

.define sdc_inadvancebuffer	$4000

.define sdc_sectorstobuffer	8

sdc_bytecounterlo		.byte 0
sdc_bytecounterhi		.byte 0
sdc_sectorcount			.word 0

sdc_filedescriptor		.byte 0

sdc_sectorsbuffered		.byte 0

; ----------------------------------------------------------------------------------------------------

sdc_openfile

		lda #$00
		sta sdc_bytecounterlo
		sta sdc_bytecounterhi

		lda #$ff
		sta sdc_sectorcount+0
		lda #$ff
		sta sdc_sectorcount+1

		ldy #>sdc_transferbuffer						; set the hyppo filename from transferbuffer
		lda #$2e
		sta $d640
		clv
		bcc :+

		lda #$34
		sta $D640
		clv
		bcc sdc_openfile_error

		lda #$18
		sta $d640
		clv
		bcc sdc_openfile_error
		rts

sdc_openfile_error
:		lda #$04
		sta $d020
		lda #$05
		sta $d020
		jmp :-				

; ----------------------------------------------------------------------------------------------------

sdc_closefile

		ldx sdc_filedescriptor
		lda #$20										; Preconditions: The file descriptor given in the X register was opened using hyppo_openfile.
		sta $d640
		clv
		rts

; ----------------------------------------------------------------------------------------------------

sdc_readsector

		lda #$35
		sta $01

		inc sdc_sectorcount+0
		bne :+
		inc sdc_sectorcount+1

														; assume the file is already open.		
:		lda $d030										; unmap the colour RAM from $dc00 because that will prevent us from mapping in the sector buffer
		pha
		and #%11111110
		sta $d030

sdc_readsector_loop

		lda #$1a										; read the next sector (hyppo_readfile)
		sta $d640
		clv

		cpx #$00										; WHY DO I HAVE TO DO THIS??? ERROR HANDLING SHOULD TAKE CARE OF THIS!!!
		bne :+
		cpy #$00
		bne :+
		clc
		jmp sdc_readsector_done
:		
		bcs :+
		jmp sdc_readsector_error

:		lda #$81										; map the sector buffer to $de00
		sta $d680

		ldx #$00										; copy sector to sectorbuffer
:		lda $de00,x
		sta sdc_sectorbuffer+$0000,x
		lda $df00,x
		sta sdc_sectorbuffer+$0100,x
		inx
		lda $de00,x
		sta sdc_sectorbuffer+$0000,x
		lda $df00,x
		sta sdc_sectorbuffer+$0100,x
		inx
		lda $de00,x
		sta sdc_sectorbuffer+$0000,x
		lda $df00,x
		sta sdc_sectorbuffer+$0100,x
		inx
		lda $de00,x
		sta sdc_sectorbuffer+$0000,x
		lda $df00,x
		sta sdc_sectorbuffer+$0100,x
		inx
		lda $de00,x
		sta sdc_sectorbuffer+$0000,x
		lda $df00,x
		sta sdc_sectorbuffer+$0100,x
		inx
		lda $de00,x
		sta sdc_sectorbuffer+$0000,x
		lda $df00,x
		sta sdc_sectorbuffer+$0100,x
		inx
		lda $de00,x
		sta sdc_sectorbuffer+$0000,x
		lda $df00,x
		sta sdc_sectorbuffer+$0100,x
		inx
		lda $de00,x
		sta sdc_sectorbuffer+$0000,x
		lda $df00,x
		sta sdc_sectorbuffer+$0100,x
		inx
		bne :-

		lda #$82										; unmap the sector buffer from $de00
		sta $d680

sdc_readsector_done

		pla												; map the colour RAM at $dc00 if it was previously mapped
		sta $d030

		lda #$34
		sta $01

		rts

sdc_readsector_error

		cmp #$ff										; if the error code in A is $ff we have reached the end of the file otherwise there’s been an error
		bne sdc_readsector_fatalerror

		pla												; map the colour RAM at $dc00 if it was previously mapped
		sta $d030

		lda #$34
		sta $01

		rts

sdc_readsector_fatalerror

:		inc $d020
		jmp :-

; ----------------------------------------------------------------------------------------------------

sdc_readbufferedsector_init

		lda #>sdc_inadvancebuffer
		sta sdcrbs1+2
		sta sdcrbs2+2
		inc sdcrbs2+2
		rts

; ----------------------------------------------------------------------------------------------------

sdc_readbufferedsector

		lda #$35
		sta $01

		lda sdc_sectorsbuffered							; are there any buffered sectors left?
		bne :+											; yes, read from buffered sectors
		jmp sdc_readsector								; no, do normal sector read

:		ldx #$00										; copy sector to sectorbuffer
:
sdcrbs1	lda sdc_inadvancebuffer+$0000,x
		sta sdc_sectorbuffer+$0000,x
sdcrbs2	lda sdc_inadvancebuffer+$0100,x
		sta sdc_sectorbuffer+$0100,x
		inx
		bne :-

		dec sdc_sectorsbuffered							; decrease number of buffered sectors

		inc sdcrbs1+2
		inc sdcrbs1+2
		inc sdcrbs2+2
		inc sdcrbs2+2

		lda sdcrbs1+2
		cmp #>(sdc_inadvancebuffer+sdc_sectorstobuffer*$0200)
		bne :+
		lda #>sdc_inadvancebuffer
		sta sdcrbs1+2
		sta sdcrbs2+2
		inc sdcrbs2+2

:		lda #$34
		sta $01

		rts

; ----------------------------------------------------------------------------------------------------

sdc_readsectorsinadvance_init

		lda #>sdc_inadvancebuffer
		sta sdcrsi1+2
		sta sdcrsi2+2
		inc sdcrsi2+2

		lda #$00
		sta sdc_sectorsbuffered

		jmp sdc_readsectorsinadvance

; ----------------------------------------------------------------------------------------------------

sdc_readsectorsinadvance

		;lda #$35
		;sta $01

														; assume the file is already open.		
		lda $d030										; unmap the colour RAM from $dc00 because that will prevent us from mapping in the sector buffer
		pha
		and #%11111110
		sta $d030

sdc_readsectorsinadvance_loop

		lda sdc_sectorsbuffered
		cmp #sdc_sectorstobuffer
		bne :+

		jmp sdc_readsectorsinadvance_done

:		inc sdc_sectorcount+0
		bne :+
		inc sdc_sectorcount+1

:		lda #$1a										; read the next sector (hyppo_readfile)
		sta $d640
		clv

		cpx #$00										; WHY DO I HAVE TO DO THIS??? ERROR HANDLING SHOULD TAKE CARE OF THIS!!!
		bne :+
		cpy #$00
		bne :+
		clc
		jmp sdc_readsectorsinadvance_done

:		bcs :+
		jmp sdc_readsectorsinadvance_error

:		lda #$81										; map the sector buffer to $de00
		sta $d680

		ldx #$00										; copy sector to inadvance buffer
:		lda $de00,x
sdcrsi1	sta sdc_inadvancebuffer+$0000,x
		lda $df00,x
sdcrsi2	sta sdc_inadvancebuffer+$0100,x
		inx
		bne :-

		lda #$82										; unmap the sector buffer from $de00
		sta $d680

		inc sdcrsi1+2
		inc sdcrsi1+2
		inc sdcrsi2+2
		inc sdcrsi2+2

		lda sdcrsi1+2
		cmp #>(sdc_inadvancebuffer+sdc_sectorstobuffer*$0200)
		bne :+
		lda #>sdc_inadvancebuffer
		sta sdcrsi1+2
		sta sdcrsi2+2
		inc sdcrsi2+2

:		inc sdc_sectorsbuffered

		jmp sdc_readsectorsinadvance_loop

sdc_readsectorsinadvance_done

		pla												; map the colour RAM at $dc00 if it was previously mapped
		sta $d030

		lda #$00
		sta $d020

		;lda #$34
		;sta $01

		rts

sdc_readsectorsinadvance_error

		cmp #$ff										; if the error code in A is $ff we have reached the end of the file otherwise there’s been an error
		bne sdc_readsectorsinadvance_fatalerror

		pla												; map the colour RAM at $dc00 if it was previously mapped
		sta $d030

		lda #$34
		sta $01

		rts

sdc_readsectorsinadvance_fatalerror

:		inc $d020
		jmp :-

; ----------------------------------------------------------------------------------------------------

sdc_copytoattic

				lda #$35
				sta $01

				DMA_RUN_JOB sdc_copytoatticjob

				lda #$34
				sta $01

				clc
				lda sdc_ctaj+1
				adc #$02
				sta sdc_ctaj+1
				lda sdc_ctaj+2
				adc #$00
				sta sdc_ctaj+2
				cmp #$10
				bne :+
				inc sdc_ctajb+1
:				lda sdc_ctaj+2
				and #$0f
				sta sdc_ctaj+2
				rts

sdc_copytoatticjob
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80, (sdc_sectorbuffer >> 20)				; sourcebank
sdc_ctajb		.byte $81, ($8000000 >> 20)						; destbank ($80 = attic ram)
				.byte $00										; No more options
				.byte $00										; Copy and don't chain
				.word 512										; Count LSB + Count MSB
				.word ((sdc_sectorbuffer) & $ffff)				; Source Address LSB + Destination Address MSB
				.byte ((sdc_sectorbuffer >> 16) & $0f)			; Source Address BANK and FLAGS (copy to rbBaseMem)
sdc_ctaj		.word (($8000000) & $ffff)						; Destination Address LSB + Destination Address MSB
				.byte (($8000000 >> 16) & $0f)					; Destination Address BANK and FLAGS (copy to rbBaseMem)
				.word $0000

; ----------------------------------------------------------------------------------------------------
