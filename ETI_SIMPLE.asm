; SIMPLE - ETI January 1979
; This version for Southern Cross Z80 Computer 
; and Talking Electronics TEC-1F

;originally converted from 8080 assembler to Z80 assembler
;for the RC2014 Micro board by Ken Boak September 2021

;comments added from original source code and ported to
;the above-mentioned Z80 boards by Craig Jones October 2021
;assembled using TASM 3.2

;uncomment when using the TEC-1F
;#define TEC-1F

;Small Interpretive microprocessor language experiment
;###################     SIMPLE     ##################
;written by Tim Hendtlass
;October 1978
;Version 2??  25/10/78


;first tell the assembler where the program is to start

            .ORG $2000
            
;the program proper starts here
;this is the master text edit routine

start:      ld hl,uprog     ;point hl to the start of the user program area
			ld sp,hl        ;stack will start just below the user program

tloop:		ld de,tloop     ;tloop address to de
			push de         ;and onto the top of the stack
                            ;this lets us return to tloop
                            ;with a return instruction
			call ci         ;get and echo a character
			dec hl
			cp $5f          ;was it backspace (5f)?
			ret z           ;yes, we have already done the
			                ;necessary correction, so back
			                ;to tloop for the next character
			inc hl          ;it wasn't so restore hl

			sub '&'         ;was it & (ampersand)
			jp z,type       ;yes,display next line
			inc a           ;was it % (percent)
			jp z,pad        ;yes,go pad and return to tloop
                            ;from the pad routine
			inc a           ;was it $ (dollar)
			jp z,exec0      ;yes,execute the user program
			inc a           ;was it # (hash)
			jp z,start      ;yes, back to start-stack will be
                            ;cleaned up automatically by the 
                            ;first two instructions!
                            
;if it was none of these it must have been a character
;to put into the user program.before we can do this we
;must restore the character the way it was when we first
;got it from the routine ci

			add a,$23       ;restore it as it was
			ld (hl),a       ;put it in the user program 
			inc hl          ;and point to the next sequential
                            ;location in the user program area
                            
;if that was a carriage return (cr) we just put away we
;now send a line feed (lf) to keep the terminal happy

			cp $0d          ;was it cr?
			call z,plf      ;if so type a lf
                            ;note-this must be a call
			ret             ;in either case back to tloop
                            ;for the next character
                            
;that is the end of the master text edit routine

;now for the main subroutines that the master
;routine calls  in case you wish to modify the 
;above note that the interpreter (exec) also
;uses the com routine. the type subroutine will
;be found as part of the interpreter

;the pad routine-it must precede the com routine
;as it 'falls through to it'

pad:		ld d,$80     ;set common routine flag = pad

;the common (com) routine if entered with D => 80h it
;pads the user program with nulls. it starts at the 
;the location pointed to by hl and pads up to but not
;including the first location in which it finds a cr. a
;cr and lf are then sent to the terminal. if on entry d < 80h
;it types the contents of the user program area
;from the location pointed to by hl. it types the text
;until a cr is found. this cr is typed and then a lf is
;also typed in all cases there is a safety count of 64
;in force-if 64 characters have been typed or padded
;without a cr being found a '?' is typed and the 
;routine aborts to start

com:		push bc        ;save bc on the stack to make some room
			ld b,$40       ;set a safety count of 64
com1:		ld a,(hl)      ;get a character to a
			cp $0d         ;is it cr?
			jp z,com2      ;job finished when we find a cr
                           ;so clean up by typing both a cr and lf
			ld c,a         ;copy a into c in case we are to type
			ld a,d         ;get the flag
			rlca           ;move bit 7 into the carry flag
			jp nc,nopad    ;no carry means don't pad
			ld (hl),$00    ;if there is a carry we must
                           ;pad so put in a null
                           
;note- the last instruction does not affect the state 
;of the carry flag so if the last instruction
;was done the next won't be

nopad:		call nc,co     ;no carry,so type character
			inc hl         ;point to next location to treat
			dec b          ;64 characters yet?
			jp nz,com1     ;no,go and get more
			ld c,'?'       ;yes ,we have a problem
			call co        ;print a '?'
			jp start       ;and abort to start
            
com2:		ld c,(hl)      ;put the cr in c
			inc hl         ;move over the cr
			call co        ;print it
plf:		ld c,$0a       ;load c with a lf
			call co        ;print it
			pop bc         ;restore bc
			ret            ;and back to whoever called us

; end of text editor and it's main support routines

;this is the start of the interpreter main routine

;when we arrive at exec we have the address of tloop
;on the top of the stack and the address at which we 
;wish to start execution of the user program in HL
;all the other registers are as yet undefined but
;will have the following uses -
;d = marker counter or print/pad flag
;e= user counter
;b= result of last match (af=yes,0=no)
;c= last character that was input in response to an accept command

;first we lose the address of tloop from the top 
;of the stack as we no longer need it

exec0:		pop de     ;there, lost it
;now we put the address of exec onto the top of the stack
;without altering any registers  this lets us
;return to exec by a simple return instruction
;of course we must do this again each time we return to exec
;as we 'use up' the address getting there

exec:       push hl            ;put hl on stack
			ld hl,exec         ;put address of exec in hl
			ex (sp),hl         ;swap hl with top of stack
			ld a,(hl)          ;get a character from the user program area
			inc hl             ;point to next character for next time round
			cp 'Z'             ;no statement starts with 'Z' or anything beyond that
			jp nc,error        ;so if it's > or = 'Z' its an error!
			sub 'A'            ;subtract ascii 'A'
			ret c              ;ignore it if it was < 'A'
            
;note it could have been a CR, a marker (*)
;an optional statement delineator or even
;a number put in by some other line oriented text
;editor. in any case we don't want to know about it
;at the moment so we just jump over it

;we now have the identifying key letter from which
;ascii 'A' has been subtracted in register a, we look up in a table
;starting at tbase to find the least
;significant byte of the address of the subroutine
;which performs the actual statement as the whole 
;interpreter fits in 256 bytes we already know the 
;most significant byte

			push hl            ;we need a little room
			ld hl,tbase        ;address of first entry in table to hl
			add a,l            ;add l to the key letter in a
                               ;which is in the range from 
                               ;a=0 to a=25
			ld l,a             ;now hl has the address
                               ;of the entry we want
			ld l,(hl)          ;now hl has the address of
                               ;the subroutine we want
			ex (sp),hl         ;put this on the top of the 
                               ;stack and restore the
                               ;original hl all at once
			ret                ;and off to the address which
                               ;we just put on the stack

;the next 25 bytes contain the least significant
;byte of the subroutines that actually perform
;the action required to do the statement, they are
;in order, a (accept) first to y (yes) last
;the form mod 256 (%256) which appears below is a way 
;of telling my assembler to only use the  least
;significant 8 bits of the address

tbase:		.DB ci %256           ; A
			.DB error %256        ; B
			.DB skip %256         ; C
			.DB dec_cnt %256      ; D
			.DB end1 %256         ; E
			.DB error %256        ; F
			.DB get %256          ; G
			.DB error %256        ; H
			.DB inc_cnt %256      ; I
			.DB jump %256         ; J
			.DB keep %256         ; K
			.DB ld_cnt %256       ; L
			.DB match %256        ; M
			.DB testn %256        ; N
			.DB error %256        ; O
			.DB co %256           ; P
			.DB error %256        ; Q
			.DB retn %256         ; R
			.DB subr %256         ; S
			.DB type %256         ; T
			.DB user %256         ; U
			.DB error %256        ; V
			.DB error %256        ; W
			.DB exch %256         ; X
			.DB testy %256        ; Y
;end of lookup table
;now for the subroutines called by exec. these are -
;end1,error,exch,get,inc,jump,keep,lcntr,match,
;retn,subr,testn,testy and type
;note the order of these next three subroutines as
;they 'fall through' from one to another
;if we arrive at either error or end1 we have finished
;executing the user program and must go back to the 
;start in this case the contents of de are no longer 
;of any importance

error:		ld c,$3f        ;load a question mark
			call co         ;print it as we have an error
end1:		dec hl          ;back up to show the character
                            ;which has caused the trouble
			ld de, start    ;get ready to go back to start
			push de         ;start address to top of stack
type:		ld d,0          ;set com flag to type
			jp com          ;go and print line, the return
                            ;at the end of com sends us
                            ;back to exec unless we came
                            ;here by way of error or end1
                            ;when it sends us to the start
                            
subr:		ld (radr),hl    ;save address of this marker
                            ;for use later as the return
                            ;address.as it is a number
                            ;exec will skip over it for
                            ;us when we come back
;once the return address has been saved, the rest of the 
;subroutine operation is identical to a jump
;so "subr" falls through to "jump" at this point

;first we use vcom to get the marker number to a  to
;change it from an ascii number to binary and then
;to subtract one  ------ the result from all this
;is the marker counter which vcom puts in d for us
;vcom checks to see that the marker number is not
;<1 or >9 and goes to error if it is
;vcom does more than this as it is also used by the
;get and keep routines,but the fact that it corrupts
;hl is of no concern to us as we are about to reload
;them anyway
jump:		call vcom       ;get the marker counter to d
			ld hl,uprog     ;start looking for markers from
                            ;beginning of user program
			ld a,$2a        ;* this is what we are looking for
jloop:		cp (hl)         ;found one at this address?
			inc hl          ;point to next user address
			jp nz,jloop     ;no we haven't,keep looking
            
;ah,we have found one - but is it the correct one?
            dec d           ;if it is d will now be negative
			jp p,jloop      ;bother,wrong one - keep looking
			ret             ;got it,now carry on execution
                            ;from where we found it
			
			
retn:		ld hl,(radr)    ;where were we in the user's
                            ;program when we came to do this
                            ;subroutine?
			ret             ;carry on execution from there
			
match:      ld a,(hl)       ;get correct answer to a
            inc hl          ;point to next user command
            sub c           ;zero result if they matched
            ld b,$af        ;set "yes" flag in case they did
            ret z           ;they did - carry on
            ld b,$00        ;they did not  - set "no" flag
            ret             ;and now carry on
			
			
testy:		.DB $3E         ;if we come in here this, plus 
                            ;the next byte will be read by
                            ;the processor as a single
                            ;instruction to load af (hex) to
                            ;register a, i.e. as ld a,0afh
testn:		xor a           ;if we come in here we just
                            ;clear to a zero
			cp b            ;test result of last match ??
                            ;b? with a if we are looking for
                            ;"yes" a will be af if we are
                            ;looking for "no" a will be 0
			ret z           ;0 means test passed so carry on
                            ;with the rest of this user line
                            
;if the test was failed we must skip to the end
;of this user line and carry on execution from there. we use
;the jloop routine to search for the cr at the end of 
;this user line, first setting up to find only 
;one cr
skip:		ld a,$0d              ;$0d is ascii for cr-what we want
			ld d,$00              ;so we stop at the first one
			jp jloop              ;jloop does the rest and we
                                  ;return from the end of it to
                                  ;exec and carry on
exch:		ld a,e                ;put e into a
			ld e,c                ;put c into e
			ld c,a                ;and a into c
			ret                   ;and carry on
            
ld_cnt:		ld e,(hl)             ;load the counter
			inc hl                ;move over that character
			ret                   ;back to exec for more
			
dec_cnt:	dec e                 ;decrement counter
			ret                   ;back to exec for more
			
inc_cnt:	inc e                 ;increment counter
			ret                   ;back to exec for more
			
keep:		push hl               ;we need some room
			call vcom             ;find address where to 
			ld (hl),c             ;save c
			pop hl                ;restore hl
			ret                   ;and back to exec for more
			
get:		push hl               ;we need some room
			call vcom             ;find address where to
			ld c,(hl)             ;load c from
			pop hl                ;restore hl
			ret                   ;and back to exec for more
			
vcom:		ld a,(hl)             ;which variable or marker?

;note we don't skip over variable or marker as
;exec will do it for us if we did the error routine
;would not show us the whole of the offending 
;statement!

			sub $31                ;convert to binary -1
;if variable or marker * was <1 it will now be 256-*
;and the check in the next line will pick it up too

			cp $09                 ;>9 or <1? impossible!
			jp nc,error            ;so go and complain

;the next instruction is only of use if we are
;working out the marker counter - but it does not
;do any harm if we are working out a variable 
;address

			ld d,a                 ;put it in d in case
;now to work out the address of the particular
;storage location identified by the binary number in a
;this is of no use (or harm) if we are working out
;the marker counter!
			ld hl,vbase            ;point to storage location hl
			add a,l                ;add the index from a to the 
                                   ;least significant byte of the
                                   ;base address which is in l
			ld l,a                 ;and put the answer into l so
                                   ;hl now points to the wanted
                                   ;storage location
			ret                    ;return to whoever called us

;the next line provides a linkage out of this
;page for the user statement. at present it is
;flagged as an error. when you use it change the value
;of the address to jump to
user:		jp error               ;'U' is not implemented yet

;the next few lines handle the single character
;input and output as written here they use two of 
;my monitor's subroutines and so will need altering
;to suit your system MI gets a single character
;from the terminal to a, does not echo it and 
;does not strip off the parity bit (bit 7)  MO
;prints the single character in c on the terminal
;you will probably have to rewrite all the code
;between the lines of dashes

            ;---------------------------------------------------
;MI         .equ   RXDATA      ;address of my monitor
                               ;input routine
;MO         .equ   TXDATA      ;address of my monitor
                               ;output routine
                               
ci:         call RXDATA        ;get a character
			and $7f            ;strip off parity bit
            ld  c,a            ;put in c ready to echo
co:			call TXDATA        ;print character in c
			ld  a,c            ;copy it back to a
			ret                ;and return to whoever called us
            ;---------------------------------------------------

;--------------------------------------------------
; SOUTHERN CROSS MONITOR  BIT BANGED SERIAL ROUTINES
;---------------------------------------------------
;
; I/O PORT ADDRESS
;
#IFDEF TEC-1F
KEYBUF:      .EQU 00H             ;MM74C923N KEYBOARD ENCODER
SCAN:        .EQU 01H             ;DISPLAY SCAN LATCH
DISPLY:      .EQU 02H             ;DISPLAY LATCH
PORT3:       .EQU 03H             ;ST3 (8X8), STROBE (RELAY BOARD) DATLATCH (DAT BOARD)
PORT4:       .EQU 04H             ;ST4 (8X8), LCD 'E' (DAT BOARD)
PORT5:       .EQU 05H
PORT6:       .EQU 06H
PORT7:       .EQU 07H             ;ENABLE/DISABLE SINGLE STEPPER (IF INSTALLED)
#ELSE
IO0:         .EQU 80H             ;IO PORT 0
IO1:         .EQU 81H             ;IO PORT 1
IO2:         .EQU 82H             ;IO PORT 2
IO3:         .EQU 83H             ;IO PORT 3
DISPLY:      .EQU 84H             ;DISPLAY LATCH
SCAN:        .EQU 85H             ;DISPLAY SCAN LATCH
KEYBUF:      .EQU 86H             ;KEYBOARD BUFFER
IO7:         .EQU 87H             ;ENABLE/DISABLE SINGLE STEPPER (IF INSTALLED)
#ENDIF
;
; BAUD RATE CONSTANTS
;
B300:	.EQU	0220H	;300 BAUD
B1200:	.EQU	0080H	;1200 BAUD
B2400:	.EQU	003FH	;2400 BAUD
B4800:	.EQU	001BH	;4800 BAUD
B9600:	.EQU	000BH	;9600 BAUD
;------------------------
; SERIAL TRANSMIT ROUTINE
;------------------------
;TRANSMIT BYTE SERIALLY ON DOUT
;
; ENTRY : C = BYTE TO TRANSMIT
;  EXIT : NO REGISTERS MODIFIED
;
TXDATA:	PUSH	AF
	PUSH	BC
	PUSH	HL
    LD	HL,B4800
    LD	(BAUD),HL	;DEFAULT SERIAL=4800 BAUD
	LD	HL,(BAUD)
;
; TRANSMIT START BIT
;
	XOR	A
	OUT	(SCAN),A
	CALL	BITIME
;
; TRANSMIT DATA
;
	LD	B,08H
	RRC	C
NXTBIT:	RRC	C	;SHIFT BITS TO D6,
	LD	A,C	;LSB FIRST AND OUTPUT
	AND	40H	;THEM FOR ONE BIT TIME.
	OUT	(SCAN),A
	CALL	BITIME
	DJNZ	NXTBIT
;
; SEND STOP BITS
;
	LD	A,40H
	OUT	(SCAN),A
	CALL	BITIME
	CALL	BITIME
	POP	HL
	POP	BC
	POP	AF
	RET
;-----------------------
; SERIAL RECEIVE ROUTINE
;-----------------------
;RECEIVE SERIAL BYTE FROM DIN
;
; ENTRY : NONE
;  EXIT : A= RECEIVED BYTE IF CARRY CLEAR
;
; REGISTERS MODIFIED A AND F
;
RXDATA:	PUSH	BC
	PUSH	HL
;
; WAIT FOR START BIT 
;
        LD	HL,B4800
        LD	(BAUD),HL	;DEFAULT SERIAL=4800 BAUD

RXDAT1: IN	A,(KEYBUF)
	   BIT	7,A
	   JR	NZ,RXDAT1	;NO START BIT
;
; DETECTED START BIT
;
	LD	HL,(BAUD)
	SRL	H
	RR	L 	;DELAY FOR HALF BIT TIME
	CALL 	BITIME
	IN	A,(KEYBUF)
	BIT	7,A
	JR	NZ,RXDAT1	;START BIT NOT VALID
;
; DETECTED VALID START BIT,READ IN DATA
;
	LD	B,08H
RXDAT2:	LD	HL,(BAUD)
	CALL	BITIME	;DELAY ONE BIT TIME
	IN	A,(KEYBUF)
	RL	A
	RR	C	;SHIFT BIT INTO DATA REG
	DJNZ	RXDAT2
	LD	A,C
	OR	A	;CLEAR CARRY FLAG
    POP	HL
	POP	BC
	RET
;---------------
; BIT TIME DELAY
;---------------
;DELAY FOR ONE SERIAL BIT TIME
;ENTRY : HL = DELAY TIME
; NO REGISTERS MODIFIED
;
BITIME:	PUSH	HL
	PUSH	DE
	LD	DE,0001H
BITIM1:	SBC	HL,DE
	JP	NC,BITIM1
	POP	DE
	POP	HL
	RET
    

			.ORG $2200
radr:       .block 2
BAUD:       .block 2
vbase:      .block 9         ; 9 bytes for user variables
            .block 32        ; allow some room for the stack
uprog:      .equ  $          ; User program Starts Here
    .end
