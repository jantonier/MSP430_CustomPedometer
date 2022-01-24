;--------------------------------------------------------
; 			CharacterHERO
;		Program by; Joshua Bonilla
;--------------------------------------------------------	
	#include	<msp430.h>
;--------------------------------------------------------	
	ORG 0C000h				
;--------------------------------------------------------
wmes1		DW 'WELCOME to'
wmes2		DW 'Joshuas Pedometer '
mainmenu1	DW 'Entrar ZPM min'
mainmenu2	DW 'Entrar ZPM max'
mainmenu3	DW 'Entrar Tolerancia '
mainmenu4	DW 'Presione Start'
Lives 		DW '8,7,6,5,4,3,2,1,0,'
;--------------------------------------------------------
	ORG 0C080h					
;--------------------------------------------------------	
easyrut	DW ' , , , , , , , , , , , , , , ,A, ,B, ,C, ,B, ,A, , ,B, ,C, ,A, , ,C,A, ,C, ,B, ,A, , , , , , , , , , , ,'

mediumrut DW ' , , , , , , , , , , , , , , ,C, ,A, ,B, ,B, ,A, ,C, ,A, ,B, ,C,B, ,B, ,C, ,A, ,A, ,B, ,C, ,C, ,B, ,A, ,C, ,B, , ,A, , ,B, ,C, ,A, ,B, ,C, , ,A, , , , , , , , , , , ,'

highrut	DW ' , , , , , , , , , , , , , , ,A,C,C,A,B,C,C,A,B,A,B,A,C,A,C,B,A,C,B,C,A,B,C,A,C,C,A,C,B,A,A,C,A,C, , , , , , , , , , , ,'
;--------------------------------------------------------
	ORG 0F800h 				; Program Start   ; Progam Start (16K Flash device)
;--------------------------------------------------------
;--------------------------------------------------------
; Macros
;--------------------------------------------------------
moveCursor	MACRO	ddramValue     ;READY!
		;Moves cursor
		bic.b	#11111111b,&P2OUT
		bic.b	#11111111b,&P1OUT
		bis.b	ddramValue,&P1OUT ;Move 
		call #enableLCD		
		bic.b	#11111111b,&P1OUT 
		ENDM
;--------------------------------------------------------

RESET	mov.w	#0280h,SP			; initialize SP   ;(512B RAM device)
StopWDT mov.w	#WDTPW+WDTHOLD,&WDTCTL 		; stop WDT
	bis.b	#11111111b,&P1DIR 		; all P1 pins as output
	bic.b	#11111111b,&P1OUT 		; all P1 pins are zero's 
;	bic.b	#11000000b,&P2SEL		; enable Xin and Xout as I/O-------------------(NOT)
	bis.b	#00110000b,&P2DIR		; P2.4 and P2.5 are output
	bic.b	#00110000b,&P2OUT		; P2.4 and P2.5 are zero's
	bic.b	#00001111b,&P2DIR		; P2.0, P2.1, P2.2 and P2.3 are input bottons
	bis.b	#00001111b,&P2REN		; internal resistors
	bic.b	#00001111b,&P2OUT		; make it pull-up
	bis.b   #00001111b,&P2IES 		; Edge sensitivity now H->L
	bic.b	#00111111b,&P2IFG 		; clear int. flag
	bis.b	#00001111b,&P2IE 		; enable int.
	eint 					; global interrupt enable 
	
	call	#initLCD 
	call	#Start
	;call	#Level
	
	bic.b	#00111111b,&P2IFG
HERE:	
	;call #buttonSelector
	jmp	HERE

;--------------------------------------------------------
; Delays Sub
;--------------------------------------------------------

Delay: 
	mov	#0x0500, R15
wait	dec 	R15 	
	jnz 	wait 	
	ret
	  
DelayDouble: 
	 mov #0x002, R14
wait3	 mov #0xFFFF, R15
wait2	 dec R15 		
	 jnz wait2 		
	 dec R14		 
	 jnz wait3		
	 ret 	
	   
DelayLED:
	mov	#0x0300, R15
wait4	dec 	R15 		
	jnz 	wait4 		
	ret 			

DelayE:
	 mov #0x0005, R14
wait6	 mov #0xFFFF, R15
wait5	 dec R15 		
	 jnz wait5 		
	 dec R14		 
	 jnz wait6		
	 ret 
	 
DelayM:
	 mov #0x0003, R14
wait7	 mov #0xFFFF, R15
wait8	 dec R15 		
	 jnz wait8 		
	 dec R14		 
	 jnz wait7		
	 ret 
	   
DelayH:
	 mov #0x0002, R14
wait9	 mov #0xFFFF, R15
wait10	 dec R15 		
	 jnz wait10 		
	 dec R14		 
	 jnz wait9		
	 ret 
;--------------------------------------------------------
; SubRoutines
;-------------------------------------------------------- 

initLCD:	
	bis.b	#30h,&P1OUT	;Wake up command 
	call 	#enableLCD
	call 	#enableLCD
	call 	#enableLCD
	bis.b	#38h,&P1OUT	;Function-set: 8bits
	call 	#enableLCD
	bic.b	#38h,&P1OUT
	bis.b	#10h,&P1OUT	;Set Cursor
	call 	#enableLCD
	bic.b	#10h,&P1OUT
	bis.b	#0Fh,&P1OUT	;Display ON/Cursor ON
	call 	#enableLCD
	bic.b	#0Fh,&P1OUT
	bis.b	#06h,&P1OUT	;Entry Mode Set
	call 	#enableLCD
	bic.b	#06h,&P1OUT
	call #Clear
	call #Clear
	ret
	
buttonSelector:
	cmp	#1,R8
	jz	easylvl
	cmp	#2,R8
	jz	mediumlvl
	cmp	#3,R8
	jz	highlvl
	jnz lvlSend
easylvl		call	#EasyD
mediumlvl	call	#MediumD
highlvl		call	#HighD
lvlSend	ret

enableLCD:
	bis.b	#00010000b,&P2OUT
	call	#Delay
	bic.b	#00010000b,&P2OUT
	ret

enableLCDRS:
	bis.b	#00100000b,&P2OUT
	call	#enableLCD
	bic.b	#00100000b,&P2OUT
	bic.b	#11111111b,&P1OUT
	ret
                                       
Start:
		;call 	#allLEDon
		mov	#0,R8	;Store lvl value, default 0
		moveCursor #83h
		;mov.b	#83h,&P1OUT ;Set Cursor position "02" which is 3 in LCD
		;call	#enableLCD
		bic.b	#11111111b,&P1OUT
		
		;Writes firt line of welcome message
		mov	#wmes1,R10 ;Stores initial value of first welcome message
		mov	#5,R14 ;Lenght of message div by 2, because a reg holds 2 letters (length 10)
		
writeNextChar:	mov	@R10+,R11 ;Put what is in R10 (char) in R11
		mov.b	R11,&P1OUT ;Write R11 char into display,  move R10 up
		call	#enableLCDRS
		swpb	R11 ;Swaps reg value, in oder to write the other char (Reg holds 2 char)
		mov.b	R11,&P1OUT ;Writes whats in R11 into the LCD
		call	#enableLCDRS
		dec	R14 ;Decrement lenght of message to be written
		jnz	writeNextChar
			
		;Move to second line of LCD
		mov.b	#11000001b,&P1OUT 
		call	#enableLCD
		bic.b	#11000001b,&P1OUT
		
		;Write second line of welcome message
		mov	#9,R14 ;Lenght of message div by 2, because a reg holds 2 letters (length 14)
		mov	#wmes2,R10 ;Stores initial value of second welcome message
		
writeNextChar2:	mov	@R10+,R11 ;Put what is in R10 (char) in R11,  move R10 up
		mov.b	R11,&P1OUT ;Write R11 char into display
		call	#enableLCDRS
		swpb	R11 ;Swaps reg value, in oder to write the other char (Reg holds 2 char)
		mov.b	R11,&P1OUT ;Swaps reg value, in oder to write the other char (Reg holds 2 char)
		call	#enableLCDRS
		dec	R14 ;Decrement lenght of message to be written
		jnz	writeNextChar2
		ret

Level:		;Main title delay
		call	#DelayE
		call	#DelayE
		call	#DelayE
		;call	#allLEDoff	
	
		;Move cursor
		call #Clear
		mov.b	#10000010b,&P1OUT
		call	#enableLCD
		bic.b	#10000010b,&P1OUT
		
		;Start writting first part message
		mov	#mainmenu1,R10 ;Store first value of main message
		mov	#6,R14 ;Lenght of message div by 2, because a reg holds 2 letters (length 12)
writeNextChar3:	mov	@R10+,R11 ;Put what is in R10 (char) in R11,  move R10 up
		mov.b	R11,&P1OUT ;Write R11 char into display
		call	#enableLCDRS
		swpb	R11 ;Swaps reg value, in oder to write the other char (Reg holds 2 char)
		mov.b	R11,&P1OUT  ;Writes whats in R11 into the LCD
		call	#enableLCDRS
		dec	R14 ;Decrement lenght of message to be written
		jnz	writeNextChar3
			
		;Move cursor
		mov.b	#11000000b,&P1OUT
		call	#enableLCD
		bic.b	#11000000b,&P1OUT
		
		;Start writting second part message
		mov	#mainmenu2,R10 ;Store first value of second main message
		mov	#8,R14 ;Lenght of message div by 2, because a reg holds 2 letters (length 16)
writeNextChar4:	mov	@R10+,R11 ;Put what is in R10 (char) in R11, move R10 up
		mov.b	R11,&P1OUT ;Write R11 char into display
		call	#enableLCDRS
		swpb	R11 ;Swaps reg value, in oder to write the other char (Reg holds 2 char)
		mov.b	R11,&P1OUT ;Writes whats in R11 into the LCD
		call	#enableLCDRS
		dec	R14
		jnz	writeNextChar4
		ret	;End
	
Clear:				;Clear LCD
	call	#DelayDouble
	bis.b	#1,&P1OUT
	call	#enableLCD
	bic.b	#1,&P1OUT
	ret

allLEDon:
	bis.b	#00111000b,P2OUT	
	ret
allLEDoff:
	bic.b	#00111000b,P2OUT	
	ret
Score:
	mov.b	#11000000b,&P1OUT
	call	#enableLCD
	bic.b	#11000000b,&P1OUT
	
	;mov.b	#53h,&P1OUT	;Write Char S
	;call	#enableLCDRS
	;mov.b	#63h,&P1OUT	;Write Char c
	;call	#enableLCDRS
	;mov.b	#6Fh,&P1OUT	;Write Char o
	;call	#enableLCDRS
	;mov.b	#72h,&P1OUT	;Write Char r
	;call	#enableLCDRS
	;mov.b	#65h,&P1OUT	;Write Char e
	;call	#enableLCDRS
	
	mov.b	#4Ch,&P1OUT	;Write Char L
	call	#enableLCDRS
	mov.b	#69h,&P1OUT	;Write Char i
	call	#enableLCDRS
	mov.b	#76h,&P1OUT	;Write Char v
	call	#enableLCDRS
	mov.b	#65h,&P1OUT	;Write Char e
	call	#enableLCDRS
	mov.b	#73h,&P1OUT	;Write Char s
	call	#enableLCDRS	
	
	mov.b	#3Ah,&P1OUT	;Write Char :
	call	#enableLCDRS
	
	mov.b	@R7,&P1OUT	;Write current score value
	call	#enableLCDRS
	mov.b	#10000111b,&P1OUT
	call	#enableLCD
	bic.b	#10000111b,&P1OUT 
	ret
	
checkButValue:	
	cmp 	R5,R6 ;Compare R5(button value, defaul 20h *space*) with R6 middle LCD char
	jz 	cmpEnd	;Compare input button value with lcd middle char
	call 	#allLEDon
	call 	#Delay
	call 	#Delay
	call	#allLEDoff
	add	#2h,R7	;Change score value
	cmp	#0xC046,R7 ;Checks if is the last score pos, "0"
	jnz	cmpEnd
	call	#gameover ;IF R7 is 0 game over
cmpEnd	ret

win:
	call	#Clear
	mov.b	#10000100b,&P1OUT
	call	#enableLCD
	bic.b	#10000100b,&P1OUT
	mov.b	#59h,&P1OUT	;Write char Y
	call	#enableLCDRS
	mov.b	#51h,&P1OUT	;Write char O
	call	#enableLCDRS
	mov.b	#55h,&P1OUT	;Write char U
	call	#enableLCDRS
	mov.b	#20h,&P1OUT	;Write char <space>
	call	#enableLCDRS
	mov.b	#57h,&P1OUT	;Write char W
	call	#enableLCDRS
	mov.b	#49h,&P1OUT	;Write char I
	call	#enableLCDRS
	mov.b	#4Eh,&P1OUT	;Write char N
	call	#enableLCDRS
	mov.b	#21h,&P1OUT	;Write char !
	call	#enableLCDRS
	call	#Score
	call	#DelayDouble
	call	#DelayDouble
	jmp	RESET
	ret

gameover:				
	call	#Clear
	call	#allLEDon
	mov.b	#10000100b,&P1OUT
	call	#enableLCD
	bic.b	#10000100b,&P1OUT
	mov.b	#47h,&P1OUT	;Write char G
	call	#enableLCDRS
	mov.b	#41h,&P1OUT	;Write char A
	call	#enableLCDRS
	mov.b	#4Dh,&P1OUT	;Write char M
	call	#enableLCDRS
	mov.b	#45h,&P1OUT	;Write char E
	call	#enableLCDRS
	mov.b	#20h,&P1OUT	;Write char <space>
	call	#enableLCDRS
	mov.b	#4Fh,&P1OUT	;Write char O
	call	#enableLCDRS
	mov.b	#56h,&P1OUT	;Write char v
	call	#enableLCDRS
	mov.b	#45h,&P1OUT	;Write char e
	call	#enableLCDRS
	mov.b	#52h,&P1OUT	;Write char r
	call	#enableLCDRS
	mov.b	#21h,&P1OUT	;Write char !
	call	#enableLCDRS
	call	#Score
	call	#DelayDouble
	call	#DelayDouble
	jmp	RESET
	ret
EasyD:
		call	#Delay
		call 	#Clear
		mov	#36,R13 ;num of char
		mov	#easyrut,R9 ;store first value of rut
		mov	#easyrut,R10 ;store first value of rut
writeE16:	mov	#16,R12 ;Stores the value 16,  values to be written in lcd
		mov	#20h,R5 ;reset input
writeEChar:	mov	@R10+,R11 ;Store R10 value, which is char in R11
		cmp	#9,R12 ;check if R12 is middle position, 9h is middle value
		jnz	notMiddlePosE
		mov.b	R11,R6 ;Store middle pos
notMiddlePosE	mov.b	R11,&P1OUT ;Store middle position in R6
		call	#enableLCDRS
		dec	R12 ;Store middle position in R6
		jnz	writeEChar
		call	#Score
		call	#DelayE ;Easy delay
		call 	#Clear	
		call	#checkButValue
		add	#0x2,R9 ;Alter first position of the list
		mov	R9,R10 ;Alter pointer runing through list
		dec	R13  ;Decrement end position, counter to complete routine (num of char left)
		jnz	writeE16
		call 	#win
		ret
	
MediumD:
		call	#Delay
		call 	#Clear
		mov	#56,R13	;num of char
		mov	#mediumrut,R9  ;store first value of rut
		mov	#mediumrut,R10 ;store first value of rut
writem16:	mov	#16,R12 ;Stores the value 16,  values to be written in lcd
		mov	#20h,R5 ;reset input
writemChar:	mov	@R10+,R11 ;Store R10 value, which is char in R11
		cmp	#9,R12 ;check if R12 is middle position, 9h is middle value
		jnz	notMiddlePosM 
		mov.b	R11,R6 ;Store middle pos
notMiddlePosM	mov.b	R11,&P1OUT ;Store middle position in R6
		call	#enableLCDRS
		dec	R12 ;Spaces left to write in the LCD, orgiginal value 16
		jnz	writemChar
		call	#Score
		call	#DelayM	;Medium delay
		call 	#Clear	
		call	#checkButValue
		add	#0x2,R9 ;Alter first position of the list
		mov	R9,R10 ;Alter pointer runing through list
		dec	R13 ;Decrement end position, counter to complete routine (num of char left)
		jnz	writem16
		call 	#win
		ret

HighD:
		call	#DelayH
		call 	#Clear
		mov	#44,R13	;num of char
		mov	#highrut,R9 ;store first value of rut
		mov	#highrut,R10 ;store first value of rut
writeh16:	mov	#16,R12 ;Stores the value 16,  values to be written in lcd
		mov	#20h,R5	;reset input
writehChar	mov	@R10+,R11 ;Store R10 value, which is char in R11
		cmp	#9,R12 ;check if R12 is middle position, 9h is middle value
		jnz	notMiddlePosD	;its not middle position
		mov.b	R11,R6	;Store middle position in R6
notMiddlePosD	mov.b	R11,&P1OUT ;LCD writter, R11 current value of R10
		call	#enableLCDRS
		dec	R12 ;Spaces left to write in the LCD, orgiginal value 16
		jnz	writehChar
		call	#Score
		call	#DelayH	;High delay
		call 	#Clear	
		call	#checkButValue
		add	#0x2,R9 ;Alter first position of the list
		mov	R9,R10 ;Alter pointer runing through list
		dec	R13 ;Decrement end position, counter to complete routine (num of char left)
		jnz	writeh16
		call 	#win
		ret

;--------------------------------------------------------
; P2.0 Interrupt Routine
;--------------------------------------------------------
PBISR:	
	;Check which button was pressed
	call	#Delay
	;cmp.b 	#11000100b,&P2IFG	;Bug fix
	;jz 	Port20
	cmp.b 	#00000001b,&P2IFG
	jz 	Port20
	;cmp.b 	#11000010b,&P2IFG	;Bug fix
	;jz 	Port21
	cmp.b 	#00000010b,&P2IFG
	jz 	Port21
	;cmp.b 	#11000001b,&P2IFG	;Bug fix
	;jz 	Port22
	cmp.b 	#00000100b,&P2IFG
	jz 	Port22
	;cmp.b 	#11000001b,&P2IFG	;Bug fix
	;jz 	Port23
	cmp.b 	#00001000b,&P2IFG
	jz 	Port23
;-------------------------------
	;Left Button was pressed
Port20:	
	cmp	#0,R8
	jnz	charA
	bic.b	#11111111b,&P2IFG
	mov	#1,R8
	mov	#Lives,R7
	jmp Port20end
	
charA:	
	mov	#41h,R5
	bic.b	#11111111b,&P2IFG
Port20end	reti
;-------------------------------
	;Middle-left Button was pressed
Port21:
	cmp	#0,R8
	jnz	charB
	bic.b	#11111111b,&P2IFG
	mov	#2,R8
	mov	#Lives,R7
	jmp Port21end
	
charB:	
	mov	#42h,R5
	bic.b	#11111111b,&P2IFG
Port21end	reti
;-------------------------------
	;Middle-right Button was pressed
Port22:
	cmp	#0,R8
	jnz	charC
	bic.b	#11111111b,&P2IFG
	mov	#3,R8
	mov	#Lives,R7
	jmp Port22end
	
charC:	
	mov	#43h,R5
	bic.b	#11111111b,&P2IFG
Port22end	reti
;-------------------------------
	;Right Button was pressed
Port23:
	cmp	#0,R8
	jnz	charD
	bic.b	#11111111b,&P2IFG
	mov	#4,R8
	mov	#Lives,R7
	jmp Port23end
	
charD:	
	mov	#43h,R5
	bic.b	#11111111b,&P2IFG
Port23end	reti

;POR AQUI ME QUEDE EDITANDOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
;--------------------------------------------------------
; Interrupt Vectors
;--------------------------------------------------------
	ORG	0FFFEh ; MSP430 RESET Vector
	DW	RESET ; address of label RESET
	
	ORG	0FFE6h ; interrupt vector Port2
	DW 	PBISR  ; addressoflabel PBISR
	END

