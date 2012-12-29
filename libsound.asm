*
* disk streaming sound driver library for ORCA/M Assembler on Apple IIgs
* Christopher Shepherd <chris@southernmost.us>, July 2007 - May 2012
*
* 1 infinite-length streaming sound from disk
* up to 8 sounds swapped into DOC RAM on-demand and played simultaneously
* DOC oscillators and DOC RAM are all managed/allocated for you
* good stuff eh?
*

		case on

		keep libsound2
		mcopy libsound.macros

*DUMMY999		start libsound
*		end

BGSoundStart	start
		using SGlobals

		tsc
		tcs
		phd
		phb
		phk
		plb
		tcd

		pushword #0000
		pushword #0000
		pushword #429
		pushlong #FilePath
		jsl soundPlayStart

		plb
		pld



		rtl
		end

*
* int soundInit( int MMID );
* get DOC ready and install MIRQ vector
* carry set if error
*
soundInit	start
		using SGlobals

begin		equ 1
* 4-byte temporary variable: for handle deref
HndlRef		equ begin
* 4-byte temporary variable: deref'd handle in DP
BuffPtr		equ begin+4

rtn		equ begin+8
mmid		equ rtn+3
finish		equ mmid+2

stkAdj		equ finish-(rtn+3)
varspace		equ rtn-begin

		tsc
		sec
		sbc #varspace
		tcs
		phd
		phb
		phk
		plb
		tcd

		stz errorFlag

		lda mmid
		sta ourMMID

		ldx #$0100
		ldy #0000
		jsr AllocDP
		jsr CheckError
		bcs siret
		stx HndlRef
		sty HndlRef+2
		ldy #0002
		lda [HndlRef],y
		pha
		_SoundStartUp
*		bcs siret

 		ldx #$2000
 		ldy #0000
 		jsr AllocDP
* 		jsr CheckError
		bcs siret
 		stx HndlRef
 		sty HndlRef+2
 		lda [HndlRef]
 		sta BuffPtr
		sta BuffPtrSave
 		ldy #0002
 		lda [HndlRef],y
 		sta BuffPtr+2
		sta BuffPtrSave+2
 		jsr DOCHalt
 		pushLong #SoundMIRQV
 		_SetSoundMIRQV
		bcs siret

 		ldx #$00E1 	; $E1 - oscillator enable?
 		lda #$00FF	; all registers?
 		jsr SetDOCReg

siret		tay

		lda rtn+1
		sta rtn+stkAdj+1
		lda rtn
		sta rtn+stkAdj
		plb
		pld
		tsc
		clc
		adc #stkAdj+varspace
		tcs
		clc
		lda errorFlag
		beq siret2
		sec
siret2		tya
 		rtl
		end

*
* int loadOneSound( char *pathname );
* pathname is gs/os pathname (16-bit length followed by data)
* returns carry set if (GS/OS or MM) error
* otherwise returns sound ID number for use in soundPlayShort
*
loadOneSound	start
		using SGlobals

begin		equ 1
hndlref		equ begin
buffptr		equ hndlref+4

rtn		equ buffptr+4
path		equ rtn+3
finish		equ path+4
stkAdj		equ finish-path
varspace		equ rtn-begin

		tsc
		sec
		sbc #varspace
		tcs
		phd
		phb
		phk
		plb
		tcd
		stz errorFlag
* OpenGS
		lda path
		sta LOSOPath
		lda path+2
		sta LOSOPath+2
		_OpenGS LOSOpenParams
		bcc geteof
		inc errorFlag
		brl losend
* GetEOFGS
geteof		lda LOSOHndl
		sta LOSEOFHndl
		sta LOSReadHndl
		sta LOSCloseHndl
		_GetEOFGS LOSEOFParams
		bcc lalloc
		inc errorFlag
		brl losend
* Alloc
lalloc		ldx LOSEOF
		stx LOSReadReq
		ldy LOSEOF+2
		sty LOSReadReq+2
		jsr Alloc
		bcc lalloc2
		inc errorFlag
		jmp losend
lalloc2 		stx hndlref
 		sty hndlref+2
 		lda [hndlref]
 		sta buffptr
		sta LOSReadData
 		ldy #0002
 		lda [hndlref],y
 		sta buffptr+2
		sta LOSReadData+2
* ReadGS
		_ReadGS LOSReadParams
		bcc lclose
		inc errorFlag
		bra losend
* CloseGS
lclose		_CloseGS LOSCloseParams
		bcc lentry
		inc errorFlag
		bra losend
* put entry in table
lentry		ldx #0000
		txy
lentry2		lda SoundTable,x
		bne next
		lda SoundTable+2,x
		bne next
* found good entry
		lda LOSReadData
		sta SoundTable,x
		lda LOSReadData+2
		sta SoundTable+2,x
		lda LOSEOF
		sta SoundTable+4,x
		lda LOSEOF+2
		sta SoundTable+6,x

* compute length of sound in pages of $100, and round up
* then store it at the right offset in LengthTbl
		phx
		tya
		asl a
		tax
		lda LOSEOF
		and #$FF00
		xba
		inc a
		sta LengthTbl,x
               plx

		tya
		bra losend
next		inx
		inx
		inx
		inx
		inx
		inx
		inx
		inx
		iny
		bra lentry2
* return
losend		tay
		lda rtn+1
		sta rtn+stkAdj+1
		lda rtn
		sta rtn+stkAdj
		plb
		pld
		tsc
		clc
		adc #stkAdj+varspace
		tcs
		clc
               lda errorFlag
		beq losend2
		sec
losend2		tya
		rtl
		end


*
* int soundPlayShort( int soundid, int rate );
* swap a SHORT (32KB or less) sound into DOC RAM and play it
* int soundid - sound id number from loadOneSound
* int rate - sound playback rate
* int length - length of sound in bytes
*
soundPlayShort	start
		using SGlobals

begin		equ 1
length2		equ begin
dataAddr		equ begin+2
oscnum		equ dataAddr+4
docpage	      equ oscnum+2
genoffset	equ docpage+2

rtn		equ genoffset+2
soundid		equ rtn+3
rate		equ soundid+2
finish		equ rate+2
stkAdj		equ finish-(rtn+3)
varspace		equ rtn-begin

		tsc
		sec
		sbc #varspace
		tcs
		phd
		phb
		phk
		plb
		tcd
		stz errorFlag

* find an unused generator
		ldx #00
genck		lda genmap+2,x
		beq found
		cmp #$FFFF
		beq failed
		inx
		inx
		inx
		inx
		inx
		inx
		inx
		inx
		bra genck
failed		inc errorFlag
		lda #0900
		brl gh
* don't map it off as used yet, in case there's no ram
* just save the table offset
found		stx genoffset
		lda genmap,x
		sta oscnum

* find enough free pages in DOC ram to play
		lda soundid
		asl a
		tax
		lda LengthTbl,x
               tay
		ldx #0000
ramsrch		jsr canfit
		bcc found2
		inx
		cpx #$0080
		bcc ramsrch
* failed
		inc errorFlag
		lda #0800
		brl gh
* found it
* x = doc page number
found2		stx docpage
		jsr markused

* map generator as used and store info about what's for dinner
		ldx genoffset	
		lda #0001
		sta genmap+2,x
		lda docpage
		sta genmap+6,x
		lda soundid
		sta genmap+4,x

* transfer sound into DOC
		asl a
		asl a
		asl a
		tax
		lda SoundTable,x
		sta dataAddr
		lda SoundTable+2,x
		sta dataAddr+2
		lda SoundTable+4,x
		tay
		lda docpage
		clc
		adc #$80
		xba
		jsr PutDOCRAM
		
* play it for one shot
		lda #00	; freq low
		clc
		adc oscnum
		tax
		lda rate
		and #$00FF
		jsr SetDOCReg
		lda #$20	; freq hi
		clc
		adc oscnum
		tax
		lda rate
		xba
		and #$00FF
		jsr SetDOCReg
		lda #$40	; volume
		clc
		adc oscnum
		tax
		lda #$00FF
		jsr SetDOCReg
		lda #$80	; addr
		clc
		adc oscnum
		tax
		lda #$0080	; $8000
		clc
		adc docpage	; doc page we ended up getting
		jsr SetDOCReg
		lda #$C0	; wavetbl
		clc
		adc oscnum
		tax
		lda #$003F	; play 32k sound
		jsr SetDOCReg
		lda #$A0	; control
		clc
		adc oscnum
		tax
		lda #$000A               ; one-shot, int enable
		jsr SetDOCReg

* go home
gh		tay
		lda rtn+1
		sta rtn+stkAdj+1
		lda rtn
		sta rtn+stkAdj
		plb
		pld
		tsc
		clc
		adc #stkAdj+varspace
		tcs
		clc
               lda errorFlag
		beq spsend2
		sec
spsend2		tya
		rtl
		end

*
* int soundPlayStart( string *path, int rate, int oversample, int echoDelay );
* begin playing long sound at *path
* oversample = 0 - no oversampling
* oversample = 1 - 2X oversample
* oversample = 2 - 4X oversample
* echoDelay - number of VBLs to desync L/R channels for faux stereo
* carry set if (typically GSOS) error
*
soundPlayStart start
		using SGlobals

begin		equ 1
* 4-byte variable: handle
HndlRef		equ begin
* 4-byte variable: deref'd buffer in DP
BuffPtr		equ begin+4
* note that HndlRef isn't used here but the stack frames have to
* match because the same DP vars are used in different internal calls
* and i don't want to ask for DP pages

rtn		equ begin+8
path		equ rtn+3
rate		equ path+4
osx		equ rate+2
echo		equ osx+2
finish		equ echo+2
stkAdj		equ finish-(rtn+3)
varspace		equ rtn-begin

		tsc
		sec
		sbc #varspace
		tcs
		phd
		phb
		phk
		plb
		tcd

		stz errorFlag

* restore saved buffptr into dp
		lda BuffPtrSave
		sta BuffPtr
		lda BuffPtrSave+2
		sta BuffPtr+2

* set rate in DOC params
		lda rate

		xba
		and #$FF00
 		sta RegTbl2
		ora #$0010
		sta RegTbl2+4
		and #$FF00
		ora #$0008
		sta RegTbl2+8

		lda rate
		and #$FF00
		ora #$0020
		sta RegTbl2+2
		and #$FF00
		ora #$0030
		sta RegTbl2+6
		and #$FF00
		ora #$0028
		sta RegTbl2+10

* set path in GS/OS params
		lda path
		sta OPath
		lda path+2
		sta OPath+2

* set oversampling
		lda osx
		sta OverSample

* set echo delay
		lda echo
		sta EchoDelay

 		jsr DOCInit
		jsr FileOpen
		bcs end1
		jsr FirstDiskRead
		bcs end1
		ldy #0000
pl1		lda RegTbl2,y
		cmp #$FFFF
		beq pl2
		and #$00FF
		tax
		lda RegTbl2,y
		xba
		and #$00FF
		jsr SetDOCReg
		iny
		iny
		bra pl1
pl2		ldx EchoDelay
		beq noecho
vbl		sep #$20
		longa off
vbl1		lda $E0C019
		bpl vbl1
vbl2		lda $E0C019
		bpl vbl2
		rep #$20
		longa on
		dex
		bne vbl
noecho		ldx #$00A0	; A0 - Control 0
 		lda #0000	; 00 - free run, no int, no halt (playing)
 		jsr SetDOCReg

 		stz BadIntCount
 		stz SndBank
 		stz EndFlag

 		lda #$FFFF
 		sta IntFlag
 		lda EndFlag
 		beq end1
		jsr DOCHalt
end1		tay
		lda rtn+1
		sta rtn+stkAdj+1
		lda rtn
		sta rtn+stkAdj
		plb
		pld
		tsc
		clc
		adc #stkAdj+varspace
		tcs
		clc
               lda errorFlag
		beq end12
		sec
end12		tya
		rtl
		end

*
* int soundMaint( void );
* call often to keep sound playing
* carry set if error
*
soundMaint	start
		using SGlobals

begin		equ 1
HndlRef		equ begin
BuffPtr		equ HndlRef+4

rtn		equ begin+8
finish		equ rtn+3
stkAdj		equ 0
varspace		equ rtn-begin

		tsc
		sec
		sbc #varspace
		tcs
		phd
		phb
		phk
		plb
		tcd

		stz errorFlag
		lda BuffPtrSave
		sta BuffPtr
		lda BuffPtrSave+2
		sta BuffPtr+2

 		lda IntFlag
 		beq NextChunk
mret		tay
		lda rtn+1
		sta rtn+stkAdj+1
		lda rtn
		sta rtn+stkAdj
		plb
		pld
		tsc
		clc
		adc #stkAdj+varspace
		tcs
		clc
		lda errorFlag
		beq mret2
		sec
mret2		tya
		rtl

NextChunk	anop
		dec IntFlag
		jsr DiskRead
		bcs end2
		ldx SndBank
		jsr PutSndData
		lda SndBank
		inc a
		cmp #0004
		bne notFour
		lda #0000
notFour		sta SndBank
		lda IntFlag
		bne mret

* interrupt happened during refilling (too slow!)
		inc BadIntCount
		lda BadIntCount
		cmp #0003
 		beq end2
		bra mret

end2		anop
		pha
		jsr DOCHalt
		jsr CloseFile
		pla
		bra mret
		end

*
* int soundStop( void );
* call to stop sound from playing
* carry set, A=errno if error
*
soundStop	start
		using SGlobals

rtn		equ 1
stkAdj		equ 0

		tsc
		phd
		phb
		phk
		plb
		tcd

		stz errorFlag
		jsr DOCHalt
		jsr CloseFile

		lda rtn+1
		sta rtn+stkAdj+1
		lda rtn
		sta rtn+stkAdj
		plb
		pld
		tsc
* don't perform adc #0000
*		clc
*		adc #stkAdj
		tcs
               clc
		lda #0000
		rtl
		end

*
* internal subroutines
*

* halt all DOC voices
DOCHalt		start
		using SGlobals

		ldx #$00A0
		lda #$0001
dh 		jsr SetDOCReg
 		inx
 		cpx #$00C0
 		bne dh
 		rts
		end

* read a DOC register
* a = data
* x = register
GetDOCReg	start
		using SGlobals

 		sep #$20
		longa off
busy 		lda $E1C03C
 		bmi busy
 		lda $E100CA
 		and $0F
 		sta $E1C03C
 		txa
 		sta $E1C03E
 		lda $E1C03D
 		lda $E1C03D
 		rep #$20
		longa on
 		rts
		end

* set a DOC register
* a = data
* x = register
SetDOCReg	start
		using SGlobals

 		sep #$20
		longa off
 		pha
busy2 		lda $E1C03C
 		bmi busy2
 		lda $E100CA
 		and $0F
 		sta $E1C03C
 		txa
 		sta $E1C03E
 		pla
 		sta $E1C03D
 		rep #$20
		longa on
 		rts
		end

* init DOC registers to defaults
DOCInit		start
		using SGlobals

		sep #$20
		longa off
 		lda $E100CA
 		and #$0F
 		ora #$60
 		sta $E1C03C	; DOC RAM, auto-increment
		lda #00
		sta $E1C03E
		sta $E1C03F	; address $0000
		ldy #00
		lda #$80
base		sta $E1C03D	; put $80 (baseline) into DOC RAM
		iny
		bne base
		rep #$20
		longa on
		ldy #0000
init		lda RegTbl1,y
		cmp #$FFFF
		beq ret
		and #$00FF
		tax
		lda RegTbl1,y
		xba
		and #$00FF
		jsr SetDOCReg
		iny
		iny
		bra init
ret		rts
               end

* slam data from buffer into DOC ram
* for short sounds
* $5 - pointer to the data
* a - DOC RAM address (8000,9000,A000, etc)
* x - number of pages perhaps?
* y - length of sample
PutDOCRAM	start
		using SGlobals

length		equ 1
BuffPtr		equ 3

		sty length
		xba	; $8000 -> $0080
		tax
		sep #$20
		longa off
		lda $E100CA
		and #$0F
		ora #$60	; DOC RAM, auto increment
		sta $E1C03C
		lda #00
		sta $E1C03E	; address low byte: 00
		txa
		sta $E1C03F	; set DOC page
		ldy #0000
sloop2		lda [BuffPtr],y
		bne nozero2
		inc a
nozero2		sta $E1C03D
		iny
		cpy length
		bcc sloop2
		lda #00
		sta $E1C03D	; 00 byte for end of sound
ret2		rep #$20
		longa on
		rts
		end

* slam data from buffer into DOC ram
* for long sounds / oversampler engine
* 2012/05/05 - New stack-based version, DOESN'T SUPPORT OVERSAMPLING
* a, y - trashed
* $5 - pointer to the data
* x = DOC 4K page number (0,1,2,3)
PutSndData	start
		using SGlobals

* you'd better have DP that supports this if you call me
BuffPtr		equ 5

		phb
		lda >$000002	; Save $00/0002 - $00/0000B
		pha                      ; Because it's about to get stomped on
		lda >$000004
		pha
		lda >$000006
		pha
		lda >$000008
		pha
		lda BuffPtr
		sta >$000002	; $00/0002 has low word of BuffPtr
		lda RTrans
		sta >$000004              ; $00/0004 has low word of RTrans
		lda RReq
		sta >$000006	; $00/0006 has low word of RReq
		tdc
		pha	; Keep old DPage Register too
		lda #$C000
		tcd      	; Direct Page register is now $C000
		tsc
		sta >$000008	; $00/0008 has old stack pointer
		sep #$20
		longa off
		lda #00
		pha
		plb	; Data Bank $00
		rep #$30
		longa on
		lda >$000002
		tcs	; Stack pointer at top of sound
		sep #$20
		longa off

		lda $E100CA
		and #$0F
		ora #$60	; DOC RAM, auto increment
		sta $3C
		lda #00
               sta $3E	; address low byte: 00

		txa	; x=0, a=$00
		asl a	; x=1, a=$20
		asl a	; x=2, a=$40
		asl a	; x=3, a=$60
		asl a
		asl a
		sta $3F	; set DOC page

		ldy |$000004
sloop		pla	; Use the stack, luke!
		bne noover
		inc a
noover		sta $3D
		dey
		bne sloop

		rep #$20
		longa on
		lda >$000004
		cmp >$000006
		beq ret2
		sep #$20
		longa off
		stz $3D	; 00 byte for end of sound
		rep #$20
		longa on
ret2		lda >$000008
		tcs	; Restore Stack Pointer
		pla
		tcd	; Restore Direct Page Register
		pla
		sta >$000008
		pla	; Restore the $00/000x bytes
		sta >$000006                ;  that we trashed
		pla
		sta >$000004
		pla
		sta >$000002
		plb                      ; Restore Data Bank
		rts

OverSample2x	anop
		longa off
		cpx #02
 		beq OverSample4x
		pha
		rep #$20
		longa on
		clc
		adc OSByte2
		lsr a
		sep #$20
		longa off
		sta $E1C03D
		pla
		sta OSByte2
		rts

OverSample4x	anop
		longa off
		pha
		rep #$20
		longa on
		clc
		adc OSByte2
		lsr a
		sta OSByte1
		clc
		adc OSByte2
		lsr a
		sep #$20
		longa off
		sta $E1C03D
		lda OSByte1
		sta $E1C03D
		pla
		sta OSByte2
		rep #$20
		longa on
		clc
		adc OSByte1
		lsr a
		sep #$20
		longa off
		sta $E1C03D
		lda OSByte2
		rts
		end

* Sound MIRQ handler
SoundMIRQV	start
		using SGlobals

 		php
 		phb
 		phd
 		rep #$30
		longa on
 		phk
 		plb
 		ldx #$00E0	; oscillator interrupt register
 		jsr GetDOCReg
 		bit #$0080	; interrupt?
 		beq smi1
 		and #$003E	; get osc number
		lsr a
		cmp #0008	; oscillator8?
 		bne not8
		stz IntFlag	; clear interrupt flag to show this happened
not8		cmp #0004
		bne not4
		lda #0000
               jsr clearreg
		bra smi1
not4		cmp #12
		bne not12
		lda #0001
		jsr clearreg
		bra smi1
not12		cmp #20
		bne not20
		lda #0002
		jsr clearreg
		bra smi1
not20		cmp #28
		bne smi1
		lda #0003
		jsr clearreg
smi1		ldx #$00A0	; Control Register 0
		jsr GetDOCReg
		and #0001	; halted?
		beq smi2

		dec EndFlag

		jsr DOCHalt
smi2		pld
		plb
		plp
		clc
		rtl

* mark generator as unused again, and free its managed pages
* a = register offset in genmap table
clearreg		anop
		asl a
		asl a
		asl a
		tax
		phx
		lda #0000
		sta genmap+2,x	; mark generator unused
               lda genmap+4,x	; sound number currently playing
		asl a
		tax
		lda LengthTbl,x	; length in pages in y
               tay
		plx
		lda genmap+6,x	; offset in x
		tax
		jsr markfree
		rts
		end

* read a chunk from disk
* first time - fill low and high $4000's
FirstDiskRead	start
		using SGlobals

		lda #0000
		sta SDisp
		sta SDisp+2	; init position to 0
		_SetMarkGS SeekParams

		jsr CheckError

		ldx BuffPtrSave
		ldy BuffPtrSave+2
		stx RData
		sty RData+2	; set GSOS to read to our allocated buffer
		lda #$2000
		ldx OverSample
		beq noOS
OS		lsr a	; reduce to $2000 or $1000 if 2x or 4x oversample
		dex
		bne OS
noOS		sta RReq
		stz RReq+2
		_ReadGS ReadParams

		jsr CheckError

		ldx #0000
		jsr PutSndData
		_ReadGS ReadParams

		jsr CheckError

		ldx #0001
		jsr PutSndData
		_ReadGS ReadParams

		jsr CheckError

		ldx #0002
		jsr PutSndData
		_ReadGS ReadParams

		jsr CheckError

		ldx #0003
		jsr PutSndData

		clc
		rts
		end

* close the soundfile
CloseFile	start
		using SGlobals

		_CloseGS CloseParams
		rts
		end

* just get one chunk from disk
DiskRead	      start
		using SGlobals

		_ReadGS ReadParams
		rts
		end

* open the file for reading
FileOpen	      start
		using SGlobals

		_OpenGS OpenParams

		jsr CheckError

		lda OHndl
		sta RHndl
		sta CHndl
		sta GHndl
 		sta SHndl
		_GetEOFGS GetEOFParams

		jsr CheckError

		rts
		end

* Get Memory from the Memory Manager
* x = size lo
* y = size hi
Alloc 		start
		using SGlobals

		pha
		pha
		phy
		phx
		pushword ourMMID
		pushword #$C000
		pushlong #$000000	
		_NewHandle
		jsr CheckError
		plx
		ply
		rts

		end
AllocDP		start
		using SGlobals

		pha
		pha
		phy
		phx
		pushword ourMMID
		pushword #$C005
		pushlong #$000000
		_NewHandle
		jsr CheckError
		plx
		ply
		rts
		end
* Check to see if there's room in DOC ram to fix X pages at X offset
* x = page # offset to start searching
* y = length in pages
canfit		start
		using SGlobals

		phx
		phy
		txa
		asl a
		tax
check		lda docrammap,x
		bne notOK
		dey
		beq OK
		inx
		inx
		bra check
notOK		ply
		plx
		sec
		lda #0700
		rts
OK		ply
		plx
		clc		
		rts
		end

* mark pages free
* x = offset
* y = pages
markfree		start
		using SGlobals

		txa
		asl a
		tax
		lda #0000
mark3		sta docrammap,x
		dey
		beq done2
		inx
		inx
		bra mark3
done2		rts
		end

* mark pages used
* x = offset
* y = pages
markused		start
		using SGlobals

		txa
		asl a
		tax
		lda #0001
mark2		sta docrammap,x
		dey
		beq done
		inx
		inx
		bra mark2
done		rts
		end

*
* Check for toolbox and GS/OS errors
*
CheckError	start
		using SGlobals

 		bcs setFlag
 		rts

setFlag		inc errorFlag
		rts

		end

*
* Global Variables and such
*

SGlobals		data

ourMMID		dc i2'$00'
BuffPtrSave	dc i4'$00'

errorFlag	dc i2'$00'

RegTbl2		anop
 		dc i1'$00,$01'	; freq low0   = E3
		dc i1'$20,$02'	; freq high0  = 01 (freq0 = 1E3 or d483)
		dc i1'$10,$01'	; freq low16  = E3
		dc i1'$30,$02'	; freq high16 = 01 (freq16 = 1E3 or d483)
		dc i1'$08,$01'	; freq low8   = E3
		dc i1'$28,$02'	; freq high8  = 01 (freq8 = 1E3 or d483)
		dc i1'$40,$FF'	; volume0     = FF
		dc i1'$50,$FF'	; volume16    = FF
		dc i1'$48,$00'	; volume8     = 00
		dc i1'$80,$00'	; addr0       = 00
		dc i1'$88,$00' 	; addr8       = 00 (formerly 80)
		dc i1'$90,$00'	; addr16      = 00
		dc i1'$C0,$3F'	; multi0      = 3F (play 32k sound)
		dc i1'$C8,$3D'	; multi8      = 3D (play 32k sound, 1/4 resolution)
		dc i1'$D0,$3F'	; multi16     = 3F (play 32k sound)
		dc i1'$A0,$01'	; control0    = 01 (free run, halt, no int)
		dc i1'$A8,$08'	; control8    = 08 (free run, no halt, int)
		dc i1'$B0,$10'	; control16   = 10 (free run, no halt, no int, stereo?)
		dc i1'$FF,$FF'	; end of table

RegTbl1	      anop
* 0, 8, and 16 for long sound / oversampler engine
		dc i1'$00,$00' 	; freq low0     = 00
		dc i1'$20,$08' 	; freq hi0      = 08
		dc i1'$10,$00' 	; freq low16    = 00
		dc i1'$30,$08'	; freq hi16     = 08
		dc i1'$08,$00'	; freq low8     = 00
		dc i1'$28,$08'	; freq hi8      = 00
		dc i1'$40,$00'	; volume 0      = 00
		dc i1'$50,$00'	; volume 16     = 00
		dc i1'$48,$00'	; volume 8      = 00
		dc i1'$80,$00'	; address low0  = 00
		dc i1'$88,$00' 	; address low8  = 00
		dc i1'$90,$00'	; address low16 = 00
		dc i1'$C0,$00'	; wavtbl0       = 00
		dc i1'$C8,$00'	; wavtbl8       = 00
		dc i1'$D0,$00'	; wavtbl16      = 00
		dc i1'$A0,$02'	; control0      = 02 (one shot)
		dc i1'$A8,$02'	; control8      = 02 (one shot)
		dc i1'$B0,$02'	; control16     = 02 (one shot)
* we use 4, 12, 20, and 28 for the one-shot sounds
		dc i1'$04,$00'	; freq low4	= 00
		dc i1'$24,$08'	; freq hi4	= 08
		dc i1'$0C,$00'	; freq low12	= 00
		dc i1'$2C,$08'	; freq hi12	= 08
		dc i1'$14,$00'	; freq low20	= 00
		dc i1'$34,$08'	; freq hi20	= 08
		dc i1'$18,$00'	; freq low28	= 00
		dc i1'$38,$08'	; freq hi28	= 08
		dc i1'$44,$00'	; volume4	= 00
		dc i1'$4C,$00'	; volume12	= 00
		dc i1'$54,$00'	; volume20	= 00
		dc i1'$58,$00'	; volume28	= 00
		dc i1'$84,$00'	; address low4	= 00
		dc i1'$8C,$00'	; address low12 = 00
		dc i1'$94,$00'	; address low20 = 00
		dc i1'$98,$00'	; address low24 = 00
		dc i1'$C4,$00'	; wavtbl4	= 00
		dc i1'$CC,$00'	; wavtbl12	= 00
		dc i1'$D4,$00'	; wavtbl20	= 00
		dc i1'$D8,$00'	; wavtbl28	= 00
		dc i1'$A4,$02'	; control4	= 02 (one shot)
		dc i1'$AC,$02'	; control12	= 02 (one shot)
		dc i1'$B4,$02'	; control20	= 02 (one shot)
		dc i1'$B8,$02'	; control28	= 02 (one shot)
* 6,14,22,30 - 4 more voices for one-shotters
		dc i1'$06,$00'	; freq low6	= 00
		dc i1'$26,$08'	; freq hi6	= 08
		dc i1'$0E,$00'	; freq low14	= 00
		dc i1'$2E,$08'	; freq hi14	= 08
		dc i1'$16,$00'	; freq low22	= 00
		dc i1'$36,$08'	; freq hi22	= 08
		dc i1'$1A,$00'	; freq low30	= 00
		dc i1'$3A,$08'	; freq hi30	= 08
		dc i1'$46,$00'	; volume6	= 00
		dc i1'$4E,$00'	; volume14	= 00
		dc i1'$56,$00'	; volume22	= 00
		dc i1'$5A,$00'	; volume30	= 00
		dc i1'$86,$00'	; address low6	= 00
		dc i1'$8E,$00'	; address low14 = 00
		dc i1'$96,$00'	; address low22 = 00
		dc i1'$9A,$00'	; address low30 = 00
		dc i1'$C6,$00'	; wavtbl6	= 00
		dc i1'$CE,$00'	; wavtbl14	= 00
		dc i1'$D6,$00'	; wavtbl22	= 00
		dc i1'$DA,$00'	; wavtbl30	= 00
		dc i1'$A6,$02'	; control6	= 02 (one shot)
		dc i1'$AE,$02'	; control14	= 02 (one shot)
		dc i1'$B6,$02'	; control22	= 02 (one shot)
		dc i1'$BA,$02'	; control30	= 02 (one shot)
		dc i1'$FF,$FF'	; end of table

IntFlag		dc i2'$FFFF'
EndFlag		dc i2'0000'
EchoDelay	dc i2'0000'
SndBank		dc i2'0000'
BadIntCount	dc i2'0000'
OverSample	dc i2'0000'
Length		dc i2'0000'
Length2		dc i2'0000'
OSByte1		dc i2'0000'
OSByte2		dc i2'0000'

OpenParams	dc i2'$0002'
OHndl		dc i2'0000'
OPath		dc a4'FilePath'

FilePath		dosin ':music:thewire.wav'

ReadParams	dc i2'$0004'
RHndl		dc i2'0000'
RData		dc i2'0000'
		dc i2'0000'
RReq		dc i2'0000'
		dc i2'0000'
RTrans		dc i2'0000'
		dc i2'0000'

CloseParams	dc i2'$0001'
CHndl		dc i2'0000'

GetEOFParams	dc i2'$0002'
GHndl		dc i2'0000'
GEOF		dc i2'0000'
		dc i2'0000'

SeekParams	dc i2'$0003'
SHndl		dc i2'0000'
SBase		dc i2'0000'
SDisp		dc i2'0000'
		dc i2'0000'

LOSOpenParams	dc i2'$0002'
LOSOHndl		dc i2'0000'
LOSOPath		dc i4'0000'

LOSEOFParams	dc i2'$0002'
LOSEOFHndl	dc i2'0000'
LOSEOF		dc i4'0000'

LOSReadParams	dc i2'$0004'
LOSReadHndl	dc i2'0000'
LOSReadData	dc i4'0000'
LOSReadReq	dc i4'0000'
LOSReadTrans	dc i4'0000'

LOSCloseParams	dc i2'$0001'
LOSCloseHndl	dc i2'0000'

* room to hold the pointers to up to 32 sounds loaded into RAM, and their
* lengths in bytes
* hopefully this is more than you ever need

SoundTable	anop
addr0		dc i4'0000,0000'
addr1		dc i4'0000,0000'
addr2		dc i4'0000,0000'
addr3		dc i4'0000,0000'
addr4		dc i4'0000,0000'
addr5		dc i4'0000,0000'
addr6		dc i4'0000,0000'
addr7		dc i4'0000,0000'
addr8		dc i4'0000,0000'
addr9		dc i4'0000,0000'
addr10		dc i4'0000,0000'
addr11		dc i4'0000,0000'
addr12		dc i4'0000,0000'
addr13		dc i4'0000,0000'
addr14		dc i4'0000,0000'
addr15		dc i4'0000,0000'
addr16		dc i4'0000,0000'
addr17		dc i4'0000,0000'
addr18		dc i4'0000,0000'
addr19		dc i4'0000,0000'
addr20		dc i4'0000,0000'
addr21		dc i4'0000,0000'
addr22		dc i4'0000,0000'
addr23		dc i4'0000,0000'
addr24		dc i4'0000,0000'
addr25		dc i4'0000,0000'
addr26		dc i4'0000,0000'
addr27		dc i4'0000,0000'
addr28		dc i4'0000,0000'
addr29		dc i4'0000,0000'
addr30		dc i4'0000,0000'
addr31		dc i4'0000,0000'

* and now their lengths, in pages
LengthTbl	anop
length0		dc i2'0000'
length1		dc i2'0000'
lengthx		dc i2'0000'
length3		dc i2'0000'
length4		dc i2'0000'
length5		dc i2'0000'
length6		dc i2'0000'
length7		dc i2'0000'
length8		dc i2'0000'
length9		dc i2'0000'
length10		dc i2'0000'
length11		dc i2'0000'
length12		dc i2'0000'
length13		dc i2'0000'
length14		dc i2'0000'
length15		dc i2'0000'
length16		dc i2'0000'
length17		dc i2'0000'
length18		dc i2'0000'
length19		dc i2'0000'
length20		dc i2'0000'
length21		dc i2'0000'
length22		dc i2'0000'
length23		dc i2'0000'
length24		dc i2'0000'
length25		dc i2'0000'
length26		dc i2'0000'
length27		dc i2'0000'
length28		dc i2'0000'
length29		dc i2'0000'
length30		dc i2'0000'
length31		dc i2'0000'

* map of DOC RAM in pages of $100
* 2 bytes each, 32K of ram = $80*2 = $FF
docrammap	ds 256
		dc i2'$FFFF,$FFFF'	

* map of DOC generators we're going to manage, and if they're in use
* 2 bytes - generator number
* 2 bytes - in use? 0001 if yet
* 2 bytes - sound currently playing
* 2 bytes - doc ram offset
genmap		anop		
gen1		dc i2'4,0000,0000,0000'
gen2		dc i2'12,0000,0000,0000'
gen3		dc i2'20,0000,0000,0000'
gen4		dc i2'28,0000,0000,0000'
gen5		dc i2'6,0000,0000,0000'
gen6		dc i2'14,0000,0000,0000'
gen7		dc i2'22,0000,0000,0000'
gen8		dc i2'30,0000,0000,0000'
		dc i2'$FFFF,$FFFF'

		end
