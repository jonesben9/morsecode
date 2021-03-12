;***********************************************************
;*
;*	lab 8 letter select 
;*
;*	
;*
;*	This is the sourcecode for Lab 8 of ECE 375
;*
;***********************************************************
;*
;*	 Author: Garrett Hallquist and Ben Jones
;*	   Date: 3/8/21
;*
;***********************************************************

.include "m128def.inc"			; Include definition file
;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register 
.def	mpr2 = r24				; Multipurpose register #2
;loop counters
.def	i = r5					
.def	j = r6
.def	length = r4				;word length				
.def	letter = r25			;holds a letter

.def	Lswitch = r7			;used to flip LEDS
.def	count1 = r8				;used for incrementing
;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

		; Set up interrupt vectors for any interrupts being used


.org	$0046					; End of Interrupt Vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:							; The initialization routine
		; Initialize the Stack Pointer (VERY IMPORTANT!!!!)
		ldi		mpr, low(RAMEND)
		out		SPL, mpr		; Load SPL with low byte of RAMEND
		ldi		mpr, high(RAMEND)
		out		SPH, mpr		; Load SPH with high byte of RAMEND

		;initialize LCD
		rcall	LCDInit	
		rcall LCDClr
		clr j
		ldi mpr, $08
		mov i, mpr
		ldi ZL, low(STRING1_BEG<<1) ; Load 'welcome!' to Data Memory where lcd reads it
		ldi ZH, high(STRING1_BEG<<1)
		ldi		XL, $00			
		ldi		XH, $01
LOOP:	inc		j	;loops to add all chars, increments a counter, then compares
		lpm mpr, Z+
		st	X+, mpr
		cp		j, i
		brne	LOOP

		clr j
		ldi mpr, $10
		mov i, mpr
		ldi ZL, low(STRING2_BEG<<1) ; Load 'please press pd0' to Data Memory where lcd reads it
		ldi ZH, high(STRING2_BEG<<1)
		ldi		XL, $10			
		ldi		XH, $01
LOOP2:	inc		j	;loops to add all chars, increments a counter, then compares
		lpm mpr, Z+
		st	X+, mpr
		cp		j, i
		brne	LOOP2

		rcall LCDWrite

		; Initialize Port B for output
		ldi		mpr, $FF		; Set Port B Data Direction Register
		out		DDRB, mpr		; for output
		ldi		mpr, $00		; Initialize Port B Data Register
		out		PORTB, mpr		; so all Port B outputs are low		

		; Initialize Port D for input
		ldi		mpr, $2E		; Set Port D Data Direction Register
		out		DDRD, mpr		; for input on pins 7,6,4,0
		ldi		mpr, $D1		; Initialize Port D Data Register
		out		PORTD, mpr		; pins 7,6,4,0 tristate input, pins 5,3,2,1 output low

		; Configure 16-bit Timer/Counters. TCCR1A: normal operation (00), normal wavegen (00)
		;								   TCCR1B: no noise cancel (0), falling edge ICE (0), (0), normal wavegen (00), 1024 prescaler (101)
		ldi		mpr, 0b00000000
		ldi		mpr2, 0b00000101
		out		TCCR1A, mpr
		out		TCCR1B, mpr2
		; Load compare with 15625, or about 1 second
		ldi		mpr, $3D
		ldi		mpr2, $09
		out		OCR1AH, mpr
		out		OCR1AL, mpr2		
		;set timer to 0 initially
		ldi		mpr, $00
		ldi		mpr2, $00
		out		TCNT1H, mpr
		out		TCNT1L, mpr2
		
		; Configure 8-bit Timer/Counter1. TCCR0 = 0b
		ldi		mpr, 0b00000111
		out		TCCR0, mpr
		ldi		mpr, $9C
		out		OCR0, mpr

		ldi		mpr, 0b11100000
		mov		Lswitch, mpr
		ldi		mpr, $00
		mov		count, mpr
;***********************************************************
;*	Main Program
;***********************************************************
MAIN:	;waits for user to press pd0					
		in mpr, PIND
		andi mpr, (1<<0)	
		cpi mpr, (1<<0)
		brne START				
		rjmp	MAIN			; Continue through main
START:
		rcall LCDClr
		clr length
		inc length
		clr j
		ldi mpr, $0A
		mov i, mpr
		ldi ZL, low(STRING3_BEG<<1) ; Load 'enter word' to Data Memory where lcd reads it
		ldi ZH, high(STRING3_BEG<<1)
		ldi		XL, $00			
		ldi		XH, $01
LOOP3:	inc		j	;loops to add all chars, increments a counter, then compares
		lpm mpr, Z+
		st	X+, mpr
		cp		j, i
		brne	LOOP3
		rcall LCDWrite

		ldi		XL, $10			
		ldi		XH, $01
		ldi letter, 'A'
		st X, letter
		rcall	LCDWrLn2
		
MAINLOOP:;polls the inputs, then if an input is recieved executes that routine
		rcall Busy
		;checks for pd4
		in mpr, PIND
		andi mpr, (1<<4)	
		cpi mpr, (1<<4)
		brne TRANSMIT

		;checks for pd0
		in mpr, PIND
		andi mpr, (1<<0)	
		cpi mpr, (1<<0)
		brne NEXTLET
		;checks for pd6
		in mpr, PIND
		andi mpr, (1<<6)	
		cpi mpr, (1<<6)
		brne INCLET
		;checks for pd7
		in mpr, PIND
		andi mpr, (1<<7)	
		cpi mpr, (1<<7)
		brne DECLET

		rjmp MAINLOOP

NEXTLET:;moves to the next letter
		;rcall Busy
		mov mpr, length
		CPI mpr, $10
		breq TRANSMIT;make sure word isnt too long
		inc length
		st X+, letter
		ldi letter, 'A'
		st X, letter
		rcall	LCDWrLn2
		rjmp MAINLOOP
INCLET:;increments the letter
		CPI letter, 'Z'
		breq OVER
		inc letter
		st X, letter
		rcall	LCDWrLn2
		rjmp MAINLOOP
OVER:;deals with Z->A
		ldi letter, 'A'
		st X, letter
		rcall	LCDWrLn2
		rjmp MAINLOOP
DECLET:;decrementsa the letter
		CPI letter, 'A'
		breq UNDER
		dec letter
		st X, letter
		rcall	LCDWrLn2
		rjmp MAINLOOP
UNDER:;deals with A->Z
		ldi letter, 'Z'
		st X, letter
		rcall	LCDWrLn2
		rjmp MAINLOOP
TRANSMIT:;transmits the word
		in mpr, PORTB
		ldi mpr2, (1<<4)
		eor mpr, mpr2
		out PORTB, mpr
		ldi		XL, $10			
		ldi		XH, $01
		clr j
TLOOP:;loop through each letter
		inc j
		ld letter, X+
		rcall TransmitL;call transmission function
		cp j, length
		brne TLOOP
		in mpr, PORTB
		
		eor mpr, mpr2
		out PORTB, mpr
		rjmp START;begin again
;***********************************************************
;*	Functions and Subroutines
;***********************************************************
;-----------------------------------------------------------
; func:	 Timer1
; Desc:	flips LEDs after 1 time Unit
;-----------------------------------------------------------
Timer1:
		push	mpr
		push	mpr2

		
		ldi		mpr, $00		; Reset Timer
		ldi		mpr2, $00
		out		TCNT1H, mpr
		out		TCNT1L, mpr2

T1Loop:	in		mpr, TIFR		; Loop until the overflow flag is set
		andi	mpr, (1<<4)
		cpi		mpr, (1<<4)
		brne	T1LOOP
		ldi		mpr, $00		; Reset timer
		ldi		mpr2, $00
		out		TCNT1H, mpr
		out		TCNT1L, mpr2
		ldi		mpr, (1<<4)		; clear overflow flag
		out		TIFR, mpr

		; Flip LEDs
		in		mpr, PORTB
		eor		mpr, Lswitch
		out		PORTB, mpr

		pop		mpr2
		pop		mpr
		ret

;-----------------------------------------------------------
; func: Busy
; Desc:		avoid button bounce with timer/counter0
;-----------------------------------------------------------
Busy:
		push	mpr

		ldi		mpr, $00		; Reset Timer
		out		TCNT0, mpr

BLoop:	in		mpr, TIFR		; Loop until the overflow flag is set
		andi	mpr, (1<<0)
		cpi		mpr, (1<<0)
		brne	BLOOP
		ldi		mpr, $00		; Reset Timer
		out		TCNT0, mpr
		ldi		mpr, (1<<0)		; clear overflow flag
		out		TIFR, mpr

		mov		mpr, count1		; Loop 7 times for 10ms = 70 ms
		inc		mpr
		cpi		mpr, $07
		mov		count1, mpr
		brne	BLoop

		ldi		mpr, $00		; clear count
		mov		count1, mpr

		pop		mpr
		ret
	
;-----------------------------------------------------------
; func: Timer3	
; Desc:	flips LEDs after 3 time Units
;-----------------------------------------------------------
Timer3:
		push	mpr
		push	mpr2

		
		ldi		mpr, $00		; Reset Timer
		ldi		mpr2, $00
		out		TCNT1H, mpr
		out		TCNT1L, mpr2

T3Loop:	in		mpr, TIFR		; Loop until the overflow flag is set
		andi	mpr, (1<<4)
		cpi		mpr, (1<<4)
		brne	T3LOOP
		ldi		mpr, $00		; Reset Timer
		ldi		mpr2, $00
		out		TCNT1H, mpr
		out		TCNT1L, mpr2
		ldi		mpr, (1<<4)		; Clear overflow Flag
		out		TIFR, mpr

		mov		mpr, count1		; Loop 3 times for 1 seconds = 3 seconds
		inc		mpr
		cpi		mpr, $03
		mov		count1, mpr
		brne	T3LOOP

		ldi		mpr, $00		; clear count
		mov		count1, mpr

		; Flip LEDs
		in		mpr, PORTB
		eor		mpr, Lswitch
		out		PORTB, mpr

		pop		mpr2
		pop		mpr
		ret
;-----------------------------------------------------------
; func:	Timer2
; Desc:	waits for 2 time Units
;-----------------------------------------------------------
Timer2:
		push	mpr
		push	mpr2

		
		ldi		mpr, $00		; Reset Timer
		ldi		mpr2, $00
		out		TCNT1H, mpr
		out		TCNT1L, mpr2

T2Loop:	in		mpr, TIFR		; Loop until the overflow flag is set
		andi	mpr, (1<<4)
		cpi		mpr, (1<<4)
		brne	T2LOOP
		ldi		mpr, $00		; Reset Timer
		ldi		mpr2, $00
		out		TCNT1H, mpr
		out		TCNT1L, mpr2
		ldi		mpr, (1<<4)		; Clear overflow Flag
		out		TIFR, mpr

		mov		mpr, count1		; Loop 2 times for 2 seconds
		inc		mpr
		cpi		mpr, $02
		mov		count1, mpr
		brne	T2LOOP

		ldi		mpr, $00		; clear count
		mov		count1, mpr

		

		pop		mpr2
		pop		mpr
		ret
;-----------------------------------------------------------
; funcs:	DOT, DASH, DOTE, DASHE
; Desc:	A series of routines representing morse code DOTS and DASHES
;-----------------------------------------------------------
DOT:	rcall	Timer1
		rcall	Timer1
		ret
DASH:	rcall	Timer1
		rcall	Timer3
		ret
DOTE:	rcall	Timer1
		rcall	Timer1
		rcall	Timer2
		ret
DASHE:	rcall	Timer1
		rcall	Timer3
		rcall	Timer2
		ret
;----------------------------------------------------------------
; func:	transmitL
; Desc: transmits  a letter
;----------------------------------------------------------------
TransmitL:
;checks to see what the letter is
		cpi letter, 'A'
		breq transA
		cpi letter, 'B'
		breq transB
		cpi letter, 'C'
		breq transD
		cpi letter, 'E'
		breq transE
		cpi letter, 'F'
		breq transF
		cpi letter, 'G'
		breq transG
		cpi letter, 'H'
		breq transH
		cpi letter, 'I'
		breq transI
		cpi letter, 'J'
		breq transJ
		cpi letter, 'K'
		breq transK
		cpi letter, 'L'
		breq transL
		cpi letter, 'M'
		breq transM
		cpi letter, 'N'
		breq transN
		cpi letter, 'O'
		breq transO
		cpi letter, 'P'
		breq transP
		cpi letter, 'Q'
		breq transQ
		cpi letter, 'R'
		breq transR
		cpi letter, 'S'
		breq transS
		cpi letter, 'T'
		breq transT
		cpi letter, 'U'
		breq transU
		cpi letter, 'V'
		breq transV
		cpi letter, 'W'
		breq transW
		cpi letter, 'X'
		breq transX
		cpi letter, 'Y'
		breq transY
		cpi letter, 'Z'
		breq transZ
		;calls the corresponding morse code function
transA:
		rcall MorseA
		ret
transB:
		rcall MorseB
		ret
transC:
		rcall MorseC
		ret
transD:
		rcall MorseD
		ret
transE:
		rcall MorseE
		ret
transF:
		rcall MorseF
		ret
transG:
		rcall MorseG
		ret
transH:
		rcall MorseH
		ret
transI:
		rcall MorseI
		ret
transJ:
		rcall MorseJ
		ret
transK:
		rcall MorseK
		ret
transL:
		rcall MorseL
		ret
transM:
		rcall MorseM
		ret
transN:
		rcall MorseN
		ret
transO:
		rcall MorseO
		ret
transP:
		rcall MorseP
		ret
transQ:
		rcall MorseQ
		ret
transR:
		rcall MorseR
		ret
transS:
		rcall MorseS
		ret
transT:
		rcall MorseT
		ret
transU:
		rcall MorseU
		ret
transV:
		rcall MorseV
		ret
transW:
		rcall MorseW
		ret
transX:
		rcall MorseX
		ret
transY:
		rcall MorseY
		ret
transZ:
		rcall MorseZ
		ret

;-----------------------------------------------------------
; funcs:	MorseA -> MorseZ
; Desc:	A series of routines using DOT and DASH to represent the 26 Letters in morse code
;-----------------------------------------------------------
MorseA:	rcall	DOT
		rcall	DASHE
		ret
MorseB:	rcall	DASH
		rcall	DOT
		rcall	DOT
		rcall	DOTE
		ret
MorseC:	rcall	DASH
		rcall	DOT
		rcall	DASH
		rcall	DOTE
		ret
MorseD:	rcall	DASH
		rcall	DOT
		rcall	DOTE
		ret
MorseE:	rcall	DOTE
		ret
MorseF:	rcall	DOT
		rcall	DOT
		rcall	DASH
		rcall	DOTE
		ret
MorseG:	rcall	DASH
		rcall	DASH
		rcall	DOTE
		ret
MorseH:	rcall	DOT
		rcall	DOT
		rcall	DOT
		rcall	DOTE
		ret
MorseI:	rcall	DOT
		rcall	DOTE
		ret
MorseJ:	rcall	DOT
		rcall	DASH
		rcall	DASH
		rcall	DASHE
		ret
MorseK:	rcall	DASH
		rcall	DOT
		rcall	DASHE
		ret
MorseL:	rcall	DOT
		rcall	DASH
		rcall	DOT
		rcall	DOTE
		ret
MorseM:	rcall	DASH
		rcall	DASHE
		ret
MorseN:	rcall	DASH
		rcall	DOTE
		ret
MorseO:	rcall	DASH
		rcall	DASH
		rcall	DASHE
		ret
MorseP:	rcall	DOT
		rcall	DASH
		rcall	DASH
		rcall	DOTE
		ret
MorseQ:	rcall	DASH
		rcall	DASH
		rcall	DOT
		rcall	DASHE
		ret
MorseR:	rcall	DOT
		rcall	DASH
		rcall	DOTE
		ret
MorseS:	rcall	DOT
		rcall	DOT
		rcall	DOTE
		ret
MorseT:	rcall	DASHE
		ret
MorseU:	rcall	DOT
		rcall	DOT
		rcall	DASHE
		ret
MorseV:	rcall	DOT
		rcall	DOT
		rcall	DOT
		rcall	DASHE
		ret
MorseW:	rcall	DOT
		rcall	DASH
		rcall	DASHE
		ret
MorseX:	rcall	DASH
		rcall	DOT
		rcall	DOT
		rcall	DASHE
		ret
MorseY:	rcall	DASH
		rcall	DOT
		rcall	DASH
		rcall	DASHE
		ret
MorseZ:	rcall	DASH
		rcall	DASH
		rcall	DOT
		rcall	DOTE
		ret
;***********************************************************
;*	Stored Program Data
;***********************************************************

; Enter any stored data you might need here

STRING1_BEG:
.DB		"Welcome!"		; Declaring data in ProgMem
STRING1_END:
STRING2_BEG:
.DB		"Please press PD0"		; Declaring data in ProgMem
STRING2_END:
STRING3_BEG:
.DB		"Enter word"		; Declaring data in ProgMem
STRING3_END:


;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include	"LCDDriver.asm"		;include LCD driver
