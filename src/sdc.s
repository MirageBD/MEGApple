.segment "SDC"

; ----------------------------------------------------------------------------------------------------

.define sdc_transferbuffer	$0200
.define sdc_sectorbuffer	$0400

sdc_bytecounterlo	.byte 0
sdc_bytecounterhi	.byte 0
sdc_sectorcount		.byte 0

sdc_filedescriptor	.byte 0

; ----------------------------------------------------------------------------------------------------

sdc_openfile

		lda #$00
		sta sdc_bytecounterlo
		sta sdc_bytecounterhi

		lda #$ff
		sta sdc_sectorcount

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

		inc sdc_sectorcount
														; assume the file is already open.		
		lda $d030										; unmap the colour RAM from $dc00 because that will prevent us from mapping in the sector buffer
		pha
		and #%11111110
		sta $d030

sdc_readsector_loop

		lda #$1a										; read the next sector (hyppo_readfile)
		sta $d640
		clv
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
