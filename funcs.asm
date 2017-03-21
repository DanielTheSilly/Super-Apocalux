;	LoadPalette - Macro that loads palette information into CGRAM
;	In: SRC_ADDR = 24 bit address of source data, 
;	START = Color index to start on, 
;	SIZE = Number of COLORS to copy
;	requires rep #$10 sep #$20
;	Returns: A=#$01, Y=SRC_ADDR
;	Modifies: A,Y

.MACRO LoadPalette

    stz $4300       ; Set DMA Mode (byte, normal increment).
    lda #$22        ; Register to write ($2122 - CGRAM Write).
    sta $4301
    lda #\2
    sta $2121       ; Start at START color.
    ldy #:\1        ; Using : before the parameter gets its bank.
    sty $4304       ; Store data bank into DMA source bank.
    ldy #\1         ; Not using : gets the offset address.
    sty $4302       ; Store data offset into DMA source offset.
    lda #(\3 * 2)   ; 2 bytes for every color.
    sta $4305       ; Store size of data block.
    lda #$01        ; Initiate DMA transfer - Must be done at end.
    sta $420B

.ENDM


;	LoadBlockToVRAM -- Macro that simplifies calling LoadVRAM to copy data to VRAM
;	In: SRC_ADDR = 24 bit address of source data, 
;	DEST = VRAM address to write to (WORD address!!), 
;	SIZE = number of BYTEs to copy
;	requires rep #$10 sep #$20 and $4300 to be #$01
;	Returns: A=#$01 Y=SRC_ADDR
;	Modifies: A, Y

.MACRO LoadBlockToVRAM

    lda #$18        ; Set the destination register (VRAM write register)
    sta $4301
    lda #$80
    sta $2115       ; Set incriment after writing 2119 / read 213A bit.
    ldy #\2         ; DEST
    sty $2116       ; $2116: Word address for accessing VRAM.
    ldy #\1         ; SRCOFFSET
    sty $4302       ; Store Data offset into DMA source offset.
    ldy #:\1        ; SRCBANK
    sty $4304       ; Store data Bank into DMA source bank.
    ldy #\3         ; SIZE
    sty $4305       ; Store size of data block.
    lda #$01        ; Initiate DMA transfer (channel 1).
    sta $420B

.ENDM
