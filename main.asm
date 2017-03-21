.INCLUDE "header.inc"
.INCLUDE "init.inc"

.INCLUDE "funcs.asm"
.INCLUDE "tiles.inc"

;TODO thoses numbers are taken by sprites. Move this all before uploading monster graphics
.EQU bghi $0100		; Background color (highbit) 0bbbbbgg
.EQU bglo $0101		; Background color (lowbit)  gggrrrrr
.EQU rng $0102		; Pseudo-random number generated.
.EQU aired $0104	; Character is on the ground. 0 = yes else no.
.EQU fall $0105		; Addition to Y value. Ranges from 248 to 8 exclusive. Grows 1 up to 8 each frame.
.EQU charx $0000	; Character X location.
.EQU chary (charx + 1)	; Character Y location.
.EQU chart (charx + 2)	; Character starting tile #
.EQU chars (charx + 3)	; Character settings vhoopppc    v: vertical flip h: horizontal flip  o: priority bits
                        ; p: palette # c = starting tile bit.
.EQU score $1700	; Low end of score. Each bit is ordered as: bbbbaaaa
                        ; a = first digit of score b= next number (tens place).
                        ; Each number is one lower than tilemap index so $0A should be carried to $10 ect.
;.EQU scoreHigh $170F	; High end of score. Indexing will deal with reaching these numbers. Free after this.
.EQU scroll $1710	; Scroll value of screen.
;.EQU scroll+1 $1711	; High scroll value of screen.
.EQU tmp $1712		; used for values that are not held long.
;.EQU tmp2 $1713	; high bit
.EQU Ground $1714	; hight of the ground in pixles
                        ; 0 = top of screen 8 = one tile under top.
.EQU Ground2 $1715	; Building that is stood on.
.EQU Ground3 $1717	; Location to draw hero. Forgive my sins making this $1717. I lazy.
.EQU LoadBG $1716	; A flag set to !#$00 if the game needs the next building generated.
.EQU Speed $1718	; Gamespeed.
.EQU Push $1719		; Amount to scroll the screen by.
.EQU gclock $171B	; Global clock.

; TODO Change EVERYTHING to have either a .w or a .b for propper dp useage.

;	SpriteUpdate
;	Useage:		SpriteUpdate after the char has moved.
;	Returns:	$01 in A and #$0220 in Y
;	Uses:		A and Y
;	Modifies:	A and Y

.MACRO SpriteUpdate
    stz $4302
    stz $4303
    rep #$10
    ldy #$0220
    sty $4305
    sep #$10
    lda #$01
    sta $420B
.ENDM

.BANK 0 SLOT 0
.ORG 0
.SECTION "MainCode"

Start:
    InitSNES    ; Clear registers, etc.

    rep #$10
    sep #$20

    lda #%00001001
    sta $2105
    sta rng ; It doesn't really matter what I put in here as long as it's not zero.
    ; TODO the rng will be cicled based on player imput later.

    ; Load Palette for our tiles
    LoadPalette SprPal, 128, 16     ; Sprite Palettes start at color 128
    LoadPalette BGPal, 0, 16
    LoadPalette BuildPal, 16, 16
    LoadPalette NumPal, 32, 4 ; TODO Get this working and also change the colors.
    ; Frankly this came out better than what I intended and I don't think I can make it better.

    lda #$01
    sta $4300       ; Set DMA mode (word, normal increment)
    LoadBlockToVRAM Sprite, charx, $0800
    LoadBlockToVRAM BG, $1000, $2F40
    LoadBlockToVRAM BGMap, $0800, $0800
    LoadBlockToVRAM Building1, $4000, $0C60
    LoadBlockToVRAM Numbers, $6000, $0800
    LoadBlockToVRAM NumMap, $6800, $0020

    LoadBlockToVRAM BuildMapH, $5100, $0140
    LoadBlockToVRAM WindowL1, $51A0, $0140
    LoadBlockToVRAM WindowL2, $5240, $0140
    LoadBlockToVRAM WindowL3, $52E0, $0140

    LoadBlockToVRAM BuildMapH, $5500, $0140
    LoadBlockToVRAM WindowL3, $55A0, $0140
    LoadBlockToVRAM WindowL4, $5640, $0140
    LoadBlockToVRAM WindowL3, $56E0, $0140

    lda #%01010001 ; aaaaaass BG1 a: Tile map address  s: SC size word address leftsift by 10 bits
    sta $2107      ; Incriments of $400
    lda #%00001000
    sta $2108
    lda #%01101000
    sta $2109

    lda #%00010100 ; bbbbaaaa a=BG1 b=BG2
    ;lda #$00 ; 4 MSBs >> 12 (word address)  Incriments of $1000
    sta $210B
    lda #%00000110 ; bbbbaaaa a=BG3 b=BG4
    sta $210C

    ldx #$0000
    stx $2116 ; Initial address for a VRAM upload or download.

    stz $2100 ; Turn off screen.
    stz $2106 ; mosaic off.

    jsr SpriteInit

    lda #$40
    sta Ground
    sta Ground2
    lda #$38
    sta Ground3
    sta chary

    lda #($80-16)
    sta charx

    stz aired ; Set main char on the ground.
    lda.b $08
    sta fall ; Fall acts oddly if it's not intalized to 8
    stz scroll
    stz scroll+1

    stz chart
    lda #%01110000
    ; vhoopppc    v: vertical flip   h: horizontal flip  o: priority bits   p: palette number
    sta chars
   
    lda #%01010100
    sta $0200

    ; Setup Video modes and other stuff, then turn on the screen
    jsr SetupVideo

    lda #$81
    sta $4200

    lda #$06
    sta Speed
    stz Push

    ; Zero score:
    stz score
    stz score+1
    stz score+2
    stz score+3
    stz score+4
    stz score+5
    stz score+6
    stz score+7
    stz score+8
    stz score+9
    stz score+10
    stz score+11
    stz score+12
    stz score+13
    stz score+14
    stz score+15
    ; If you can get one point per second, this score will roll over in only
    ; about 31709791983764590100000000000000000 years. Good luck.

    lda #%01000000
    sta bghi
    sep #$10

GameLoop:

    stz Push

    SetGround3:
    lda charx
    clc
    adc scroll
    bcs SetG2OnG3
	cmp #$C6
	bcs NullGround
	    lda Ground
	    jmp EndSetG3
    NullGround:
    cmp #$F2
    bcs SetG2OnG3
	lda #$00
	jmp EndSetG3
	SetG2OnG3:
	    lda Ground2
    EndSetG3:
    adc #$F8
    sta Ground3

    lda $4219 ; Load joy data1 - axlr0000
    ;lda $4218 ; Load joy data2 - byetUDLR

    ;Joypad reading and gravity.
    ;TestLeft:
    ora #%00000010 ; Set the left bit.
    cmp $4219 ; See if it's the same or if we have change the bit.
    bne TestRight
	lda gclock
	asl
	and #$04
	sta chart

	ldx charx ; The bit was already set so we can change the sprite X now.
	dex
	dex
	dex
	dex
	lda.b #%01000000 ; Left facing sprite bit.
	ora chars
	sta chars
	cpx.b #$00
	bmi TestUp ; No to-do The screen will not ever shift more left.
	stx.b charx
	jmp TestUp ; On a regular joypad there in no way to press up and down.
	; Warning: Emulators may altar inputs making it look like the game fuctions differently.
	; This means pressing left and right may cause nothing at all to happen, but the
	; expected result given the code would be to move left only.
    TestRight:
    and.b #%11111101 ; Removing the changed bit is cheaper than loading the value again.
    ; That is safe bcause we know it's changed.
    ora #%00000001
    cmp $4219
    bne TestUp
	lda gclock
	asl
	and #$04
	sta chart

	ldx charx
	inx
	inx
	inx
	inx
	lda.b #%10111111
	and chars
	sta chars
	cpx.b #$FF
	bpl NoScroller ; This scrolls the screen.
	    lda #$04
	    sta Push
	    jmp TestUp
	NoScroller:
	stx charx
    TestUp:
    lda $4219 ; At this point we don't know if it's chaged anymore, so and is not fesable to OR data back in.
    ora #%00001000
    cmp $4219
    bne TestDone ; Jump to end because down is no longer tested.
	lda aired
	cmp.b #$00
	bne TestDone
	    ina
	    sta aired
	    lda.b #$F7
	    sta fall
	jmp TestDone
    ;TestDown:
    ;and #%11110111 ; This code was for testing. Down does nothing now.
    ;ora #%00000100
    ;cmp $4219
    ;bne TestDone
	;ldx chary
	;inx
	;stx chary
    TestDone:

    lda #$07
    ;lda Speed
    clc
    sbc Speed
    clc
    adc Push
    sta Push
    jmp Scroller

    Gravity:

    lda fall
    cmp.b #$09 ; #$09 = max fall speed
    bcs GravityJump ; goto Jump if fall >= #$09
	; fall is possitive (< 9) falling.
	cmp.b #$08
	bne ContinueG
	    ldx aired
	    cpx #$00
	    bne ContinueFF
		ldx chary
		cpx Ground3
		bne ContinueFF
		    ;ldx chary
		    ;cpx #$F0
		    ;bcc GravityClean
		    ;jmp Start
		    jmp GravityClean
	ContinueG:
	ina
	sta fall
	ContinueFF: ; Free fall.
	sta aired
	adc chary
	cmp.b #$BF ; tests for bottom of screen
	bcc NoDed
	    stz $2100 ; Turn off screen.
	    jmp Start
	NoDed:
	cmp.w Ground3
	bcc GravityDo
	    adc.b #$F6 ; 10 is needed to account for errors.
	    cmp.w Ground3
	    bcc ColidedGround
		lda #$09
		; This code is reached if they failed to hit the ground.
		sta Ground3 ; This will make it imposible to get up ending the game.
		sta Ground2
		sta Ground
		sta aired
		adc chary
		jmp GravityDo
		;stz score
	    ColidedGround: ; This code is reached if the player is inside the ground but only
	    ; By a little. They have just managed to hit the edge of the ground.
	    ; Or gravity has pelled them too far.
	    lda.b #$08
	    sta fall
	    lda.w Ground3
	    stz aired
	GravityDo:
	sta chary
    GravityClean:

    SpriteUpdate

    lda Speed ; Waits for as many times as speed is. The less speed is the faster.
    SpeedLoop:
	dea
	wai
	cmp.b #$00
    bne SpeedLoop

    lda gclock ; Counts up the clock. Only used for walking animation now.
    ina ; Double increment makes it easy for the walking frames, if gclock is used in the
    ina ; future, this should be removed and the 'sta chart' should have a left shift before.
    sta gclock

    jmp GameLoop
; End of game code.

GravityJump:
; This code is the jumping code. Somehow it works better to fit it out here.
; TODO Jam this back in the main code. -1 score for spaghetticode

    ina
    sta fall
    cmp #$00
    beq GravityClean
	adc chary
	bcc DoneJump ; If after adding this y is large than we have jumped off the screen.
	    sta chary
	    jmp GravityClean
	DoneJump:
	lda #$FF
	sta chary

jmp GravityClean

Scroller: ; Scrolls the screen if needed.
lda Push
cmp #$00
beq ScrollDone

lda gclock
;asl
and #$04
sta chart ; Flip the walking frame if the clock is set right.

    lda scroll
    clc
    adc Push
    sta $210D
    sta scroll
    lda scroll + 1
    bcc samescr2 ; We should only count up the second scroll bit if the carry flag is set.
    adc #$00
    sta scroll + 1
    lda #$01 ; This command *can* be removed, but it will mess up after 256*256 scrolls.
    sta LoadBG
    samescr2:
    sta $210D ; We need to write twice either way.
    ; It may be better to stz the second bit every other time because sta take longer.
    ;lda walkBonus ; This is needed if the score for walking is to be cut into smaller amounts.
    ;dea ; Frankly, I'm fine with it for now because the score max is so high anyway.
    ;cmp.b #$??
    ;bmi AddScore
    phx
    ldx.b #$00
    jmp AddScore
    ScoreDone:
    plx
    ScrollDone:
jmp Gravity


; GetRNG returns a pseudo-random number in A and stores the number in rng.
; Fibonacci LFSR for a m-sequence function.
; Taps: 8 6 5 4

GetRNG:
    lda rng
    lsr
    lsr
    eor rng ; xor 8th bit with 6th. LSB is the feedback.
    sta tmp
    lda rng
    lsr
    lsr
    lsr
    eor tmp ; 5th xor with 6 xor 8.
    sta tmp
    lda rng
    lsr
    lsr
    lsr
    lsr
    eor tmp ; 4th xor with 5 xor 6 xor 8.
    ror ; Move feedback (LSB) to the carry flag.
    lda rng
    ror ; Feedback is in the MSB now.
    sta rng
rts

; GetBRNG Same as GetRNG but it maps backwards for fun.

GetBRNG:
    lda rng
    asl
    asl
    eor rng
    sta tmp
    lda rng
    asl
    asl
    asl
    eor tmp
    sta tmp
    lda rng
    asl
    asl
    asl
    asl
    eor tmp
    rol
    lda rng
    rol
    sta rng
rts

SpriteInit:
    php
    rep #$30

    ldx.w #$0000
    lda #$0001
    setoffscr:
	sta charx, x
	inx
	inx
	inx
	inx
	cpx #$0200
    bne setoffscr

    ldx #$0000
    lda #$5555
    clr:
	sta $0200, x
	inx
	inx
	cpx #$0020
    bne clr

    plp
rts

SetupVideo:

    sep #$20

    ;lda charx
    stz $2102
    stz $2103

    ldy #$0400
    sty $4300		; CPU -> PPU, auto increment, write 1 reg, $2104 (OAM Write)
    stz $4302
    stz $4303		; Zero offset
    ldy #$0220
    sty $4305		; Number of bytes to transfer
    lda #$7E
    sta $4304		; Bank address = $7E  (work RAM)
    lda #$01
    sta $420B		; Start DMA transfer

    lda #%10100000
    sta $2101		; Set OAM Size to 32x32

    lda #%00010111	; Enable BG1&2&3 and sprites.
    sta $212C
    
    lda #$0F
    sta $2100		; Turn on screen, full Brightness.

rts

VBlank:
    rep #$30        ; A/mem=16 bits, X/Y=16 bits (to push all 16 bits)
	phb
	pha
	phx
	phy
	phd
    sep #$20        ; A/mem=8 bit    

    ldx.w #$6800 ; Warning: wla can't read this as 6800 without ".w". It reads 68 and causes
    ; lots of errors. This is a bug in wla.
    stx.w $2116
    sep #$10
    ldy #$10
    ldx #$08
    ScoreLoop: ; Looks at the score and draws the correct number to screen.
	dey
	lda score, y
	lsr
	lsr
	lsr
	lsr
	ina
	sta $2118
	stx $2119
	lda score, y
	and #%00001111
	ina
	sta $2118
	stx $2119
    cpy #$00
    bne ScoreLoop

    rep #$10

    lda LoadBG
    cmp.b #$00
    bne GenBG ; Generate a new building if needed.
    DoneGenBG:

    stz $2121 ; TODO Later this code will be used for lightning.
    lda bghi
    sta $2122
    lda bglo
    sta $2122

    lda $4210       ; Clear NMI flag
    rep #$30        ; A/Mem=16 bits, X/Y=16 bits 
	pld
	ply
	plx
	pla
	plb
    sep #$20
rti

; This is called by GenBG. It sets A to $30
; The point being a branch is needed.
SetTo48:
lda #$30
jmp SetGround

; This loads the next building into vram
; must be called in vblank, but it would be nice if I didn't have to do that.
; It should only happen the moment the building is off screen so it would be ok.
GenBG: ; TODO clean out some clc's because I think most of them are not needed.
    lda #$01
    sta $4310
    stz LoadBG
    lda #$18
    sta $4311
    lda #$80
    sta $2115

    lda Ground2
    ina ; TODO this is a workaround for some odd bug. Probably a missing clc, I guess.
    sta Ground

    ;jmp GetBRNG ; This causes errors because of rti?
	lda rng ; Copy of GetBRNG as workaround.
	asl ; TODO Make GetBRNG a macro. That would probably fit better.
	asl
	eor rng
	sta tmp
	lda rng
	asl
	asl
	asl
	eor tmp
	sta tmp
	lda rng
	asl
	asl
	asl
	asl
	eor tmp
	rol
	lda rng
	rol
	sta rng

    and #%00011000 ; Value is now randomly 0, 8, 16 or 24
    clc
    adc #$F0 ; 240. Value is now 240, 248, 0 or 8
    clc
    cmp #$00
    bne NoAdd16
    clc
    adc #$10
    NoAdd16: ; Value is now 240, 248, 8 or 16 (add for -16, -8, 8, 16)
    clc
    adc Ground2 ; A (Ground2) is now 1-2 tiles higher or lower.
    cmp #$30 ; Min of Ground is 48
    bcc SetTo48
	cmp #$B0 ; Max is 160.
	bcc SetGround
	    lda #$B0
    SetGround:
    sta Ground2

    rep #$20
    and #$00FF ; Clear junk data in B.
    lsr
    lsr
    lsr
    tax ; x is now number of rows of zero tiles to put in.
    lda scroll+1
    and #$0001 ; Loading high bite of scroll and treating it as the low bite.
    cmp #$00
    bne SkipBuild2a
        lda #$5400
	jmp SkipBuild2b
    SkipBuild2a:
    lda #$5000
    SkipBuild2b:
    sta tmp
    sep #$20

    ;rep #$10 ; Already set.
    ; $#0020 = one line of zeros.

    ZeroLineLoop:
	ldy.w tmp
	sty.w $2116
	ldy.w #DeadSpace
	sty.w $4312
	ldy.w #:DeadSpace
	sty.w $4314
	ldy.w #$0040
	sty.w $4315
	lda #$02
	sta $420B
	lda tmp
	clc
	adc #$20
	sta tmp
	bcc NoAddTmp2
	    lda tmp+1
	    ina
	    sta tmp+1
	NoAddTmp2:
	dex
    cpx #$00
    bne ZeroLineLoop

    ;LoadBlockToVRAM BuildMapH, $5000, $0140
    ; That causes errors because of channel 1 for some reason.
    ; Here's the workaround:
    ldy tmp
    sty $2116
    ldy #BuildMapH
    sty $4312
    ldy #:BuildMapH
    sty $4314
    ldy #$0140
    sty $4315
    lda #$02
    sta $420B

    rep #$20 ; This should set the highbit of a to junkdata from rng+1
    ; This shouldn't be a problem because we wipe everything with bit-and

    lda scroll+1
    and #$0001 ; Loading high bite of scroll and treating it as the low bite. x_x
    cmp #$00
    bne SkipBuild2Wa
	lda Ground2
	asl
	asl
	clc
	adc #$54A0
	jmp SkipBuild2Wb
    SkipBuild2Wa:
	lda Ground2
	asl
	asl
	clc
	adc #$50A0
    SkipBuild2Wb:
    ;adc #$50A0 ; 5000 for the building ram location and $A0 for the
    ; Uncounted building top. Ground2 is the empty space
    tax ; so X can be used for looping

    lda rng ; Reuse first rng because I only want a few bits.
    ; I can use the bits I tossed in the last rng.
    ; I could use other bits and save some computation time,
    ; but It's just not tha important now.

    WindowLoop:
	stx $2116

	and.w #%0000000000000011
	sta.w tmp ; and tmp + 1
	asl
	asl
	clc
	adc.w tmp
	asl
	asl
	asl
	asl
	asl
	asl ; This multiplies rng 0-3 with 320
	adc.w #WindowL1 ; A= WindowL(1 to 4 via rng)
	;;;LoadBlockToVRAM WindowL?, (Ground2*4+50A0), $0140

	sta $4312       ; Store Data offset into DMA source offset.
	ldy #:WindowL1  ; SRCBANK WindowL1 is fine because it's all the same bank.
	sty $4314       ; Store data Bank into DMA source bank.
	ldy #$0140         ; SIZE
	sty $4315       ; Store size of data block.
	sep #$20
	lda #$02        ; Initiate DMA transfer (channel 1).
	sta $420B

    rep #$20
    txa
    adc.w #$00A0
    tax
    lda scroll+1
    and #$0001
    cmp #$00
    bne SkipBuild2L
	cpx.w #$5780
	jmp TestResult
    SkipBuild2L:
    cpx.w #$5340
    TestResult:
    bcs DoneGenBG1

    sep #$20
	lda rng ; Copy of GetBRNG as workaround.
	asl
	asl
	eor rng
	sta tmp
	lda rng
	asl
	asl
	asl
	eor tmp
	sta tmp
	lda rng
	asl
	asl
	asl
	asl
	eor tmp
	rol
	lda rng
        rol
        sta rng

    rep #$20
jmp WindowLoop

DoneGenBG1: ; Stupid two step jump to get out. I don't like this.
sep #$20
jmp DoneGenBG

;	AddScore -- Function that adds "Push" to the game score
;	In: X must be set to $00 score is added by x*100+1.
;	requires sep #$20
;	Returns: A=#$01 X=$00-$10
;	Modifies: A and X

AddScore:
    lda score, x
    and #$0F ; Get the ones place of the score.
    clc
    adc Push
    ;ina
    cmp #$0A
    bcs ScoreCarry
    lda score, x
    clc
    adc Push
    ;ina
    sta score, x
jml ScoreDone
ScoreCarry:
    cpx.b #$01
    bne SkipSpeed
	lda Speed
	cmp.b #$02
	beq SkipSpeed
	    dea
	    sta Speed
    SkipSpeed:
    lda score, x
    clc
    adc #$06 ; Value was $?9
    clc
    adc Push ; adc #$06 for 1 point
    cmp #$A0
    bcs ScoreCarryNext ; The score value has passed 100 and must go to the next score index.
    sta score, x
jml ScoreDone
ScoreCarryNext:
    lda #$01
    sta Push
    stz score, x
    inx
    cpx.b #$10
    bne AddScore
    lda $4218 ; Please do not add any comments for what this code does.
    ldx $4219
    sta 0, x
jml ScoreDone

.ENDS

; Character Data

.BANK 1 SLOT 0
.ORG 0
.SECTION "CharacterData"

;Sprites:
Sprite:
    .INCBIN "Ninja3.pic" ; Note this is a mash of two sprites.
Numbers:
    .INCBIN "numbers.pic"

;Palettes:
SprPal:
    .INCBIN "Ninja.clr"
BGPal:
    .INCBIN "CityBack.clr"
BuildPal:
    .INCBIN "Building3.clr"
NumPal:
    .INCBIN "numbers.clr"

;Tile maps:
BGMap:
    .INCBIN "CityBack.map"
;NumMap:
;    .INCBIN "numbers.map"
;BuildMap:
;    .INCBIN "Building2.map"
.ENDS

.BANK 2 SLOT 0
.ORG 0
.SECTION "BuildingsData"
BG:
    .INCBIN "CityBack.pic"
.ENDS

.BANK 3 SLOT 0
.ORG 0
.SECTION "BGData"
Building1:
    .INCBIN "Building2.pic"
.ENDS
