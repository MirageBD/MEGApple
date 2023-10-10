.feature pc_assignment
.feature labels_without_colons
.feature c_comments
; .feature org_per_seg

filebuffer = $0200

.include "macros.s"
.include "mathmacros.s"
.include "sdc.s"
.include "keyboard.s"
.include "leds.s"
.include "main.s"
.include "ringbuffer.s"
.include "sampleplay.s"

; ----------------------------------------------------------------------------------------------------
