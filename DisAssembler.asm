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
    reg64 dq "rax",0,"rdx",0,"rcx",0,"rbx", 0,"rsp",0,"rbp",0,"rsi",0,"rdi", 0,"r8", 0,"r9", 0,"r10", 0,"r11", 0,"r12", 0,"r13", 0,"r14", 0,"r15", 0
    reg32 dq "eax",0,"edx",0,"ecx",0,"ebx",0,"esp",0,"ebp",0,"esi",0,"edi",0,"r8d",0,"r9d",0,"r10d",0,"r11d",0,"r12d",0,"r13d",0,"r14d",0,"r15d",0
    reg16 dq "ax",0,"dx",0,"cx",0,"bx",0,"sp",0,"bp",0,"si",0,"di",0,"r8w",0,"r9w",0,"r10w",0,"r11w",0,"r12w",0,"r13w",0,"r14w",0,"r15w",0
    reg8  dq "al",0,"dl",0,"cl",0,"bl",0,"spl",0,"bpl",0,"sil",0,"dil",0,"r8b",0,"r9b",0,"r10b",0,"r11b",0,"r12b",0,"r13b",0,"r14b",0,"r15b",0
    space db " ",0
    commo db ",",0
    x     db "0x",0

section .bss
    buffer resb bufferSize
    obuffer resb bufferSize
    
    code resb 100

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

	opsize	resq 1	;Operand size
	dispsize resq 1	;Displacement size

	needRex	resb	1	;Boolean. Does it need Rex
	pre67	resb	1	;Boolean. Does it need prefix 67
	pre66	resb	1	;Boolean. Does it need prefix 66
    NumPre  resb    1   ;how many prefixes
    
    reg1    resq    1
    reg2    resq    1
    OpcodeSize resb 1
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

    mov Qword[fileName], "dest"


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


    cmp	al,	2
	jl	NLPASS
	call	dassembler
 
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


 
dassembler:
    call setDefult

    call checkNoneOperand
    cmp rax,0
    jne finished
    
    xor rdi,rdi

    call checkPrefix
    call checkPrefix

    call checkRex

    call handlePopPushRet

    call checkUsualUnary
    


    ; call newLine
    ; xor rax,rax
    ; mov al,byte[NumPre]
    ; call writeNum
    
    finished:
    ; mov [output],rax
    ret

checkUsualUnary:
    push rdi
    mov al,byte[line+rdi]      ;al = next char
    mov bl,byte[line+rdi+1]    ;bl = Dnext char
    cmp al,'f'
    je UUyes
    cmp al,'d'
    je UUyes
    cmp al,'8'
    je UU
    cmp al,'c'
    je UU
    pop rdi
    ret

    UU:
    cmp bl,'f'
    je UUyes
    cmp bl,'4'
    je UUyes
    pop rdi
    ret

    UUyes:
        lea rsi,[line+rdi]     
        mov rdx, 2
        call HexNumtoIntBounded    ;rax
        shr rax,2
        mov qword[Opcode], rax          ;Opcode

        lea rsi,[line+rdi+2]    ;al = next char
        mov rdx, 2
        call HexNumtoIntBounded    ;rax
        shr rax,3
        and rax,0b111
        mov qword[Reg], rax          ;rCode

        mov byte[OpcodeSize],1
        mov byte[opsize],1

        mov byte[codeD],1
        

        call newLine
        mov rax,qword[Reg]
        call writeNum        



    pop rdi
    ret



handlePopPushRet:
    push rdi 
    lea rsi,[line + rdi]
    xor rdx,rdx
    mov rdx,2
    call HexNumtoIntBounded
    cmp rax, 0xc2
    je handleRet

   
    shr rax,3

    cmp rax, 0b01010
    je handlePush
    cmp rax, 0b01011
    je handlePop

    mov rax,-1
    pop rdi
    ret
    handleRet:
        mov qword[Opcode],'ret'
        jmp gdis
    handlePush:
        mov qword[Opcode],'push'
        jmp greg
    handlePop:
        mov qword[Opcode],'pop'
        jmp greg

        


    gdis:
        lea rsi,[line + rdi+2]
        call HexNumtoInt
        mov qword[Displace],rbx

        call newLine
        mov rsi, Opcode
        call printString
        mov rsi, space
        call printString
        mov rsi, x
        call printString
        mov rax, qword[Displace]
        call writeNumhex


        xor rax,rax
        pop rdi
        ret
    greg:
        mov al, byte[line + rdi+1]
        call HextoInt     ;rax = reg code
        mov rbx,rax
        call getReg64    ;rbx <-code

        mov qword[reg1],rbx

        call newLine
        mov rsi, Opcode
        call printString
        mov rsi, space
        call printString
        mov rsi, reg1
        call printString
    ; call writeNum

        xor rax,rax
        pop rdi
        ret 

setDefult:
    xor rdi,rdi
    mov byte[RexB], 0
    mov byte[RexR], 0
    mov byte[RexX], 0
    mov byte[RexW], 0
    mov byte[needRex], 0
    mov byte[pre67], 0
    mov byte[pre66], 0
    mov byte[NumPre], 0
    mov qword[Opcode], 0
    mov qword[reg2], 0
    mov qword[reg1], 0
    mov qword[Displace], 0
    mov qword[Data], 0
    mov qword[opsize], 0
    mov qword[dispsize], 0

    ret


HexNumtoIntBounded:
;rsi <- start of num
;rdx <- end of num
    xor rbx,rbx
    xor r9,r9
    HtIwhileb:
        cmp r9,rdx
        je HtIendb
        xor rax,rax
        mov	al,	[rsi]
        call HextoInt
        add	rbx,rax
        inc	rsi
        inc r9
        shl rbx,4
        jmp HtIwhileb
    HtIendb:
        shr rbx,4
        mov rax,rbx
        xor rbx,rbx
        ret
checkRex:
    cmp byte[line+rdi],"4"
    jne noRex
    mov byte[needRex],1
    inc rdi
    mov al,byte[line+rdi]
    inc rdi
    call HextoInt
    
    xor rbx,rbx
    mov bl,2
    xor rdx,rdx
    div bx
    cmp dx,0

    je noRexB
    mov byte[RexB],1

    noRexB:
    xor rdx,rdx
    div bx

    cmp dx,0
    je noRexX
    mov byte[RexX],1
    noRexX:
    xor rdx,rdx
    div bx
    cmp dx,0
    je noRexR
    mov byte[RexR],1
    noRexR:
    xor rdx,rdx
    div bx
    cmp dx,0
    je noRexW
    mov byte[RexW],1
    noRexW:
    noRex:
    ret


checkPrefix:
    push rax
    push rsi

    mov rsi, line
    mov al,byte[rsi+rdi]
    cmp al,"6"
    jne noPrefix

    mov al,byte[rsi+rdi+1]
    cmp al,"7"
    je Pre67
    cmp al,"6"
    je Pre66
    jmp noPrefix

    noPrefix:
    mov byte[pre66],0
    mov byte[pre67],0
    pop rsi
    pop rax
    ret
    Pre66:
        mov byte[pre66],1
        mov byte[pre67],0
        add rdi,2
        inc byte[NumPre]
        pop rsi
        pop rax
        ret
    Pre67:
        mov byte[pre66],0
        mov byte[pre67],1
        add rdi,2
        inc byte[NumPre]
        pop rsi
        pop rax
        ret

checkNoneOperand:     
    mov rsi, line
    call HexNumtoInt
    mov [code],rbx

    cmp	rbx,0b11000011
    mov rax, "ret"
    je	done
    cmp	rbx,0b11111101
    mov rax, "std"
    je	done
    cmp	rbx,0b11111001
    mov rax, "stc"
    je	done
    cmp	rbx,0b11111000
    mov rax, "clc"
    je	done
    cmp	rbx,0b11111100
    mov rax, "cld"
    je	done
    cmp  rbx,0b0000111100000101
    mov rax, "syscall"
    je	done
    mov rax, 0
    jmp	done
    done:
        ret

HexNumtoInt:
;rsi <- start of num
    xor rbx,rbx
    HtIwhile:
        cmp byte[rsi], 0
        je HtIend
        xor rax,rax
        mov	al,	[rsi]
        call HextoInt
        add	rbx,rax
        inc	rsi
        shl rbx,4
        jmp HtIwhile
    HtIend:
    shr rbx,4
    ret

HextoInt:
;al = hex
    xor r8,r8
    cmp al,"0"
    je finishHextoInt
    inc r8
    cmp al,"1"
    je finishHextoInt
    inc r8
    cmp al,"2"
    je finishHextoInt
    inc r8
    cmp al,"3"
    je finishHextoInt
    inc r8
    cmp al,"4"
    je finishHextoInt
    inc r8
    cmp al,"5"
    je finishHextoInt
    inc r8
    cmp al,"6"
    je finishHextoInt
    inc r8
    cmp al,"7"
    je finishHextoInt
    inc r8
    cmp al,"8"
    je finishHextoInt
    inc r8
    cmp al,"9"
    je finishHextoInt
    inc r8
    cmp al,"a"
    je finishHextoInt
    inc r8
    cmp al,"b"
    je finishHextoInt
    inc r8
    cmp al,"c"
    je finishHextoInt
    inc r8
    cmp al,"d"
    je finishHextoInt
    inc r8
    cmp al,"e"
    je finishHextoInt
    inc r8
    cmp al,"f"
    je finishHextoInt
    finishHextoInt:
        mov rax,r8
        ret





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

getReg64:
;rbx <- regcode
    push rdi
    xor rdi,rdi

    lea rdi, [rbx*2]
    mov rbx, qword[reg64+rdi*8]
    
    pop rdi
    ret

getReg32:
;rbx <- regcode
    push rdi
    xor rdi,rdi
    lea rdi, [rbx*2]
    mov rbx, qword[reg32+rdi*8]
    pop rdi
    ret

getReg16:
;rbx <- regcode
    push rdi
    xor rdi,rdi
    lea rdi, [rbx*2]
    mov rbx, qword[reg16+rdi*8]
    pop rdi
    ret
getReg8:
;rbx <- regcode
    push rdi
    xor rdi,rdi
    lea rdi, [rbx*2]
    mov rbx, qword[reg8+rdi*8]
    pop rdi
    ret

Exit:
	mov	rax,	1
	mov	rbx,	0
	int	0x80