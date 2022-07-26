%include "in_out.asm"
%include "sys-equal.asm"
%include "fileOperation.asm"

    %macro testing 1
        push rax
        mov rax, %1
        call writeNum
        pop rax
    %endmacro


section .data
    IFD		dq	0 		;Input File Descriptor
	OFD		dq	0 		;Output File Descriptor
    ; testinst  db "mov rax,rbx",0
    bufferSize equ 10000

section .bss
    buffer resb bufferSize
    obuffer resb bufferSize

    line resb 100
    
    linePointer resq 1
    obPointer resq 1


    fileName resb 100
    fileSave resb 100

    output resb 100
    input resb 100
    
;------------------------
    Prefix 	resb 5	;Hex		|					|
	REX 	resb 5	;Binary	|
	RexW 	resb 2	;Boolean	|
	RexR 	resb 2	;Boolean	|
	RexX 	resb 2	;Boolean	|
	RexB 	resb 2	;Boolean	|
	;						|
	Opcode	resb 20	;Binary	|
	codeD	resb 2	;Boolean	|
	codeW	resb 2	;Boolean	|
	Mod	    resb 5	;Binary	|
	Reg	    resb 5	;Binary	|
	RM		resb 5	;Binary	|
	Scale	resb 5	;Binary	|
	Index   resb 5	;Binary	|
	Base	resb 5	;Binary	|	
	Displace resb 10	;Hex		|
	Data	resb 20	;Hex		|
	;-----------------------------------|

	opsize	resb 1	;Operand size
	dispsize resb 1	;Displacement size

	needRex	resb	1	;Boolean. Does it need Rex
	pre67	resb	1	;Boolean. Does it need prefix 67
	pre66	resb	1	;Boolean. Does it need prefix 66


section .text
    global _start

_start:

File:
    mov rax,obuffer
    mov [obPointer],rax

    ; ; file name
    ; mov	rax,	3
	; mov	rbx,	2
	; mov	rcx,	fileName
	; mov	rdx,	50
	; int 80h

    ; ; remove 0xA from file name
    ; mov	rax,	-1
	; nlwhile:
	; inc	rax
	; mov	bl,	[fileName+rax]
	; cmp	bl,	0xA
	; jne	nlwhile
	; mov	bl,	0
	; mov	[fileName+rax],	bl

    mov Qword[fileName],	"test"


    ; open file
    mov	rdi,	fileName
	mov	rax,	sys_open
	mov	rsi,	O_RDWR
	syscall
	mov	[IFD],	rax



    ; read file to buffer
	mov	rdi,	[IFD]
	mov	rsi,	buffer
	mov	rdx,	10000
    mov	rax,	sys_read
	syscall
    mov	byte[rsi+rax],	0	
    mov	rsi,	buffer
	mov	[linePointer],	rsi


    LOOP:
    ; call setDefault
    call getLine
    mov rsi, line

    call printString
    call length


    cmp	al,	3
	jl	NLPASS
	call	assembler
 
    mov	rsi, output
	call	newLine
	call	printString
	call	newLine


	mov	rdi,[obPointer]
	call	copy
	mov	[obPointer], rdi


	NLPASS:

        mov	rdi,[obPointer]
        mov	al,	10
        mov	[rdi], al
        inc	rdi
        mov	[obPointer], rdi
        mov	al,	0
        mov	[rdi], al

	jmp	LOOP


 
assembler:
    ; call breakIns
    ret



; breakIns:

copy:
;Copies the content of the memory which rsi is
;pointing, to the memory pointed by rdi.
;Copies till reached Null or ',' or ' ' or tab
;Changes RSI, RDI
;Puts NULL at the end of the rdi
    push rax

    xor	rax, rax
    copyWhile:
        mov	al,	[rsi]
        cmp	al,	0
        je	copyRet
        cmp	al,	' '
        je	copyRet
        cmp	al,	','
        je	copyRet
        cmp	al,	9	;Tab
        je	copyRet
        cmp	al,	10	;NL
        je	copyRet

        mov	[rdi], al
        inc	rsi
        inc	rdi
        jmp	copyWhile

    copyRet:
        mov	al,	0	;Put NULL at the end
        mov	[rdi], al
        ; testing 1
        pop	rax
        ret



length:
        push	rbx
        push	rsi

        xor	rax,	rax
    lenWhile:
        cmp	byte[rsi], 0
        je	lenRet
        inc	rax
        inc	rsi
        jmp	lenWhile

    lenRet:
        pop	rsi
        pop	rbx
        ret


getLine:
    push	rsi
    push	rdi
    push	rax

    mov	rsi,	[linePointer]
    mov	rdi,	line

    cmp	byte[rsi], 0 ;EOF

    jne	glWhile

    pop	rax
    pop	rdi
    pop	rsi
    ; jmp	writeInFile
    
    jmp Exit

    glWhile:
        xor	rax,	rax
        mov	al,	[rsi]
        cmp	al,	0xA
        je	glPutNull
        cmp	al,	0
        je	glPutNull2
        mov	[rdi], al
        inc	rsi
        inc	rdi

        jmp	glWhile

    glPutNull:
        inc	rsi
        mov	[linePointer], rsi
        mov	byte[rdi],	0
        jmp	glRet

    glPutNull2:	
        mov	[linePointer],	rsi
        mov	byte[rdi],	0
        jmp	glRet

    glRet:
        pop	rax
        pop	rdi
        pop	rsi
        ret

Exit:
	mov	rax,	1
	mov	rbx,	0
	int	0x80