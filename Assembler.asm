%include "in_out.asm"
%include "sys-equal.asm"
; %include "fileOperation.asm"

    %macro testing 1
        push rax
        call newLine
        mov rsi,t
        call newLine
        mov rax, %1
        call writeNum
        pop rax
    %endmacro

    %macro copyMem 1
        push r11
        push r12
        push r13
        push rsi
        push rdi
        push rcx
        push rax

        mov rsi, %1
        call length

        mov r11, r10
        mov r12, %1
        mov r13, rax
        cld
        mov rsi, r12
        mov rdi, r11
        mov rcx, r13
        shr rcx,3
        rep movsq
        mov rcx, r13
        and rcx, 7
        rep movsb
        add r10, rax
        
        pop rax
        pop rcx
        pop rdi
        pop rsi
        pop r13
        pop r12
        pop r11        
    %endmacro
   %macro copyBin 1
        push r11
        push r12
        push r13
        push rsi
        push rdi
        push rcx
        push rax
        push rbx

        mov rbx, %1
        call HexToBin

        mov rsi, BinTemp
        call length

        mov r11, r15
        mov r12, BinTemp
        mov r13, rax
        cld
        mov rsi, r12
        mov rdi, r11
        mov rcx, r13
        shr rcx,3
        rep movsq
        mov rcx, r13
        and rcx, 7
        rep movsb
        add r15, rax
        
        pop rbx
        pop rax
        pop rcx
        pop rdi
        pop rsi
        pop r13
        pop r12
        pop r11        
    %endmacro
section .data
    IFD		dq	0 		;Input File Descriptor
	OFD		dq	0 		;Output File Descriptor
    ; testinst  db "mov rax,rbx",0
    bufferSize equ 10000
    t db "t   ",0
	enterSave	db	"Enter the name of list file: ", 0
	enterSaveBin	db	"Enter the name of list file Bin: ", 0

    unaryOpcode dq  "inc",0,0b111111000,0,"dec",0,0b111111001,0,"call",0,0b111111010,0,"jmp",0,0b111111100,0,"push",0,0b111111110,0,"not",0,0b111101010,0,"neg",0,0b111101011,0,"imul",0,0b111101101,0,"idiv",0,0b111101111,0,"pop",0,0b100011000,0,"shl",0,0b110100100,0,"shr",0,0b110100101,0
    binaryOpcode dq "mov",0, 0b100010,0,"add",0, 0b000000,0,"adc",0, 0b000100,0,"sub",0, 0b001010,0,"sbb",0, 0b000110,0,"or",0, 0b000010,0,"xor",0, 0b001100,0,"and",0, 0b001000,0,"cmp",0, 0b001110,0,"shl",0, 0b000000,0,"shr",0, 0b000000,0,"test",0, 0b0000100001,0,"xchg",0, 0b0000100001,0
    ibinOpcode dq   "mov", 0,0b110001000,0,"add", 0,0b100000000,0,"adc", 0,0b100000010,0,"sub", 0,0b100000101,0,"sbb", 0,0b100000011,0,"or",  0,0b100000001,0,"xor", 0,0b100000110,0,"and", 0,0b100000100,0,"cmp", 0,0b100000111,0,"shl", 0,0b110000100,0,"shr", 0,0b110000101,0,"test",0,0b111101000,0

    reg64 dq "rax",0,"rcx",0,"rdx",0,"rbx",0,"rsp",0,"rbp",0,"rsi",0,"rdi",0,"r8", 0,"r9", 0,"r10", 0,"r11", 0,"r12", 0,"r13", 0,"r14", 0,"r15", 0
    reg32 dq "eax",0,"ecx",0,"edx",0,"ebx",0,"esp",0,"ebp",0,"esi",0,"edi",0,"r8d",0,"r9d",0,"r10d",0,"r11d",0,"r12d",0,"r13d",0,"r14d",0,"r15d",0
    reg16 dq "ax",0,"cx",0,"dx",0,"bx",0,"sp",0,"bp",0,"si",0,"di",0,"r8w",0,"r9w",0,"r10w",0,"r11w",0,"r12w",0,"r13w",0,"r14w",0,"r15w",0
    reg8  dq "al",0,"cl",0,"dl",0,"bl",0,"spl",0,"bpl",0,"sil",0,"dil",0,"r8b",0,"r9b",0,"r10b",0,"r11b",0,"r12b",0,"r13b",0,"r14b",0,"r15b",0

    newline db 0xA,0


section .bss
    buffer resb bufferSize
    obuffer resb bufferSize

    line resb 100
    
    linePointer resq 1
    obPointer resq 1

    BinTemp resb 50

    fileName resb 100
    fileSave resb 100
    fileSaveBin resb 100

    output resb 1000
    outputBin resb 10000
    input resb 100
    
    Prefix 	resq 5	;Hex		|					|
	REX 	resq 5	;Binary	|
	RexW 	resb 2	;Boolean	|
	RexR 	resb 2	;Boolean	|
	RexX 	resb 2	;Boolean	|
	RexB 	resb 2	;Boolean	|
	;						|
	Opcode	resq 20	;Binary	|
	codeD	resb 2	;Boolean	|
	codeW	resb 2	;Boolean	|
	Mod	    resb 5	;Binary	|
	Reg	    resb 5	;Binary	|
	RM		resb 5	;Binary	|
	Scale	resb 5	;Binary	|
	Index   resq 5	;Binary	|
	Base	resq 5	;Binary	|	
	Displace resb 10	;Hex		|
	Data	resb 20	;Hex		|
    BaseRegName resb 50
    IndexRegName resb 50
    DispStr resq 1
    neededSIB resb 1
    hasBase resb 1
    hasIndex resb 1
    hasScale resb 1
    hasDisplace resb 1

    firstOprTemp resb 50

	opsize	resb 1	;Operand size
	dispsize resb 1	;Displacement size

	needRex	resb	1	;Boolean. Does it need Rex
	pre67	resq	1	;Boolean. Does it need prefix 67
	pre66	resq	1	;Boolean. Does it need prefix 66
    InsSize resq 1
    Operation resq 1
    Operand1 resq 1
    Operand2 resq 1
    isfinished resb 1
    isOperandOneReg resb 1
    operandSize resq 1
    addresSize resq 1
    result resq 1
    resultHexTemp resb 50
    operand1Adress resq 1
    operand2Adress resq 1

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

    mov r10,output
    mov r15,outputBin

    LOOP:
    call setDefault
    call getLine
    mov rsi, line

    call printString
    call length
    mov qword[InsSize], rax


    cmp	al,	3
	jl	NLPASS
	call	assembler
 
    ; mov	rsi, output
	call	newLine
	; call	printString
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
    call breakIns

    cmp byte[opsize], 0
    je noneOperand
    cmp byte[isfinished],1
    je finished


    cmp byte[opsize], 1
    je UnaryOperand
    cmp byte[isfinished],1
    je finished


    ; call newLine
    ; mov rsi,Operand1
    ; call printString 
    
    finished:
    ret

UnaryOperand:
    call getOperator 

    mov al, byte[Operand1]
    call checkRegOrMem
    cmp byte[isOperandOneReg], 1
    je UnaryReg
    jne UnaryMem
    ; mov  rax,qword[Opcode]
    ; call newLine
    ; call writeNumhex

    ret

UnaryMem:
    mov rdi,qword[operand1Adress]
    lea rsi, [line+rdi]

    call breakMemIns
    call setCodes  
    call setcodeD
    
    ;Reg
    mov rax,qword[Opcode]
    and rax,0b111
    mov byte[Reg],al

    ret

setcodeD:
    mov rax,qword[Opcode]
    cmp rax,0b110100100
    je setD0
    cmp rax,0b110100101
    je setD0
    mov byte[codeD],1
    ret
    setD0:
    mov byte[codeD],0
    ret
setCodes:
    cmp byte[hasBase],1
    jne checkIndexset
    mov r14,qword[operandSize]
    mov rax,qword[BaseRegName]
    call getOperand

    mov qword[Base],rax
    mov rax,qword[operandSize]
    mov qword[addresSize], rax


    checkIndexset:
    cmp byte[hasIndex],1
    jne NOSIB
    mov rax,qword[IndexRegName]
    call getOperand
    mov qword[Index],rax
    mov rax,qword[operandSize]
    mov qword[addresSize], rax
    mov byte[neededSIB],1
    mov qword[operandSize],r14
    ret


    NOSIB:
    mov byte[neededSIB],0
    mov qword[operandSize],r14
        ret
breakMemIns:
    call setOpMsize
    whileskipPTR:
        cmp byte[rsi],'['
        je skipPTR
        inc rsi
        jmp whileskipPTR
    skipPTR:
    inc rsi
    
    ;[disp]
    cmp byte[rsi],'0'
    je justDisp
    ;get Base Index

    xor rdi,rdi
    gbiwhile:
        cmp byte[rsi],'*'
        je noBase
        cmp byte[rsi],'+'
        je hasBasee
        cmp byte[rsi],']'
        je JustBase
        mov al, byte[rsi]
        mov byte[firstOprTemp+rdi], al
        inc rsi
        inc rdi
        jmp gbiwhile


    noBase:
        mov byte[hasBase], 0
        mov byte[hasIndex], 1
        jmp indexGet


    hasBasee:
        mov byte[hasBase], 1
        inc rsi
        cmp byte[rsi],'0'
        je noIndex
        ;Base + Index
        mov byte[hasBase], 1
        mov byte[hasIndex], 1
        mov rax,qword[firstOprTemp]
        mov qword[BaseRegName], rax
        indexGet:
        xor rdi,rdi
        gIndexWhile:
            cmp byte[rsi],'*'
            je getScale
            mov al,byte[rsi]
            mov byte[firstOprTemp+rdi], al
            inc rsi
            inc rdi
            jmp gIndexWhile
        getScale:
        mov rax,qword[firstOprTemp]
        mov qword[IndexRegName], rax
        inc rsi
        ;scale
        cmp byte[rsi],'1'
        mov byte[Scale],0b00
        je endScale
        cmp byte[rsi],'2'
        mov byte[Scale],0b01
        je endScale
        cmp byte[rsi],'4'
        mov byte[Scale],0b10
        je endScale
        cmp byte[rsi],'8'
        mov byte[Scale],0b11
        endScale:
        inc rsi
        cmp byte[rsi],']'
        je BaseIndexNoDisp
        mov byte[hasDisplace], 1
        inc rsi
        inc rsi
        inc rsi
        xor rdi,rdi
        gDispWhile:
            cmp byte[rsi],']'
            je finishedgetDisp

            mov al,byte[rsi]
            mov byte[DispStr+rdi],al
            inc rsi
            inc rdi
            jmp gDispWhile
        finishedgetDisp:
            jmp finishedBreakIns

        BaseIndexNoDisp:
            mov byte[hasDisplace],0
            mov byte[hasIndex],1
            mov byte[hasBase],1


            jmp finishedBreakIns
    noIndex:
        mov byte[hasBase], 1
        mov byte[hasIndex], 0
        mov rax,qword[firstOprTemp]
        mov qword[BaseRegName], rax
        inc rsi
        inc rsi
        xor rdi,rdi
        gDispWhile2:
            cmp byte[rsi],']'
            je finishedBreakIns

            mov al,byte[rsi]
            mov byte[DispStr+rdi],al
            inc rsi
            inc rdi
            jmp gDispWhile2

    JustBase:
        mov byte[hasBase], 1
        mov byte[hasIndex], 0
        mov byte[hasDisplace], 0
        mov rax,qword[firstOprTemp]
        mov qword[BaseRegName], rax
        jmp finishedBreakIns


    justDisp:
        mov byte[hasBase],0
        mov byte[hasIndex],0
        mov byte[hasDisplace],1
        inc rsi
        inc rsi
        xor rdi,rdi
        gDispWhile3:
            cmp byte[rsi],']'
            je finishedBreakIns

            mov al,byte[rsi]
            mov byte[DispStr+rdi],al
            inc rsi
            inc rdi
            jmp gDispWhile3

    

    finishedBreakIns:
    ; mov rsi,BaseRegName
    ; call newLine
    ; call printString    
    ; mov rsi,IndexRegName
    ; call newLine
    ; call printString
    ; mov rsi,DispStr
    ; call newLine
    ; call printString
    ret

setOpMsize:
    mov al, byte[rsi]
    cmp al,"Q"
    jne not64opm
    mov byte[opsize], 64
    not64opm:
    cmp al,"D"
    jne not32opm
    mov byte[opsize], 32
    not32opm:
    cmp al,"W"
    jne not16opm
    mov byte[opsize], 16
    not16opm:
    mov byte[opsize], 8
    ret
HexToBin:
    ;rbx adrrs of HEX     56f0
    xor rdi,rdi
    mov rsi,BinTemp
    HTBwhile:
        cmp byte[rbx+rdi],0
        ; testing 6
        je HTBend
        cmp byte[rbx+rdi],'0'
        je HTB0
        cmp byte[rbx+rdi],'1'
        je HTB1
        cmp byte[rbx+rdi],'2'
        je HTB2
        cmp byte[rbx+rdi],'3'
        je HTB3
        cmp byte[rbx+rdi],'4'
        je HTB4
        cmp byte[rbx+rdi],'5'
        je HTB5
        cmp byte[rbx+rdi],'6'
        je HTB6
        cmp byte[rbx+rdi],'7'
        je HTB7
        cmp byte[rbx+rdi],'8'
        je HTB8
        cmp byte[rbx+rdi],'9'
        je HTB9
        cmp byte[rbx+rdi],'a'
        je HTBA
        cmp byte[rbx+rdi],'b'
        je HTBB
        cmp byte[rbx+rdi],'c'
        je HTBC
        cmp byte[rbx+rdi],'d'
        je HTBD
        cmp byte[rbx+rdi],'e'
        je HTBE
        cmp byte[rbx+rdi],'f'
        je HTBF

        jmp HTBend

        HTBcon:
            inc rdi
            jmp HTBwhile        

    HTBend:
        ret
    HTB0:
        mov dword[rsi],'0000'
        add rsi,4
        jmp HTBcon
    HTB1:
        mov dword[rsi],'0001'
        add rsi,4
        jmp HTBcon
    HTB2:
        mov dword[rsi],'0010'
        add rsi,4
        jmp HTBcon
    HTB3:
        mov dword[rsi],'0011'
        add rsi,4
        jmp HTBcon
    HTB4:
        mov dword[rsi],'0100'
        add rsi,4
        jmp HTBcon
    HTB5:
        mov dword[rsi],'0101'
        add rsi,4
        jmp HTBcon
    HTB6:
        mov dword[rsi],'0110'
        add rsi,4
        jmp HTBcon
    HTB7:
        mov dword[rsi],'0111'
        add rsi,4
        jmp HTBcon
    HTB8:
        mov dword[rsi],'1000'
        add rsi,4
        jmp HTBcon

    HTB9:
        mov dword[rsi],'1001'
        add rsi,4
        jmp HTBcon

    HTBA:
        mov dword[rsi],'1010'
        add rsi,4
        jmp HTBcon

    HTBB:
        mov dword[rsi],'1011'
        add rsi,4
        jmp HTBcon

    HTBC:
        mov dword[rsi],'1100'
        add rsi,4
        jmp HTBcon

    HTBD:
        mov dword[rsi],'1101'
        add rsi,4
        jmp HTBcon

    HTBE:
        mov dword[rsi],'1110'
        add rsi,4
        jmp HTBcon

    HTBF:
        mov dword[rsi],'1111'
        add rsi,4
        jmp HTBcon



UnaryReg:
    mov rax,qword[Operand1]
    call getOperand
    call setUnaryRegD
    ;reg
    mov rax,qword[Opcode]
    and rax,0b111
    mov byte[Reg],al
    ;Mod
    mov byte[Mod],0b11
    ;RM
    mov rax,qword[Operand1]
    and rax,0b111
    mov byte[RM],al
    ;rexB
    mov rax,qword[Operand1]
    shr rax,3
    mov byte[RexB],al
    
    call setOperandSize


    cmp qword[Opcode],0b111111110
    je handlePushPop
    cmp qword[Opcode],0b100011000
    je handlePushPop
    cmp qword[Opcode],0b111111100
    je handleJmp
    cmp qword[Opcode],0b111111010
    je handleCall

    cmp qword[REX],0
    je unaryhandleNoREX
    xor rax,rax
    mov rbx,qword[pre66]
    shl rbx,24
    add rax,rbx
    mov rbx,qword[REX]
    shl rbx,20
    add rax,rbx
    xor rbx,rbx
    mov bl,byte[RexW]
    shl rbx,19
    add rax,rbx
    xor rbx,rbx
    mov bl,byte[RexR]
    shl rbx,18
    add rax,rbx
    xor rbx,rbx
    mov bl,byte[RexX]
    shl rbx,17
    add rax,rbx
    xor rbx,rbx
    mov bl,byte[RexB]
    shl rbx,16
    add rax,rbx
    mov rbx,qword[Opcode]
    shr rbx,3
    shl rbx,10
    add rax,rbx
    xor rbx,rbx
    mov bl,byte[codeW]
    shl rbx,8
    add rax,rbx
    xor rbx,rbx
    mov bl,byte[codeD]
    shl rbx,9
    add rax,rbx
    xor rbx,rbx
    mov bl,byte[Mod]
    shl rbx,6
    add rax,rbx
    xor rbx,rbx
    mov bl,byte[Reg]
    shl rbx,3
    add rax,rbx
    xor rbx,rbx
    mov bl,byte[RM]
    add rax,rbx

    mov qword[result],rax
    call newLine
    call IntToHexNum
    mov rsi,resultHexTemp
    call printString

    copyMem resultHexTemp
    copyMem newline
    copyBin resultHexTemp
    
    ret   
    unaryhandleNoREX:
    xor rax,rax
    mov rbx,qword[pre66]
    shl rbx,16
    add rax,rbx

    mov rbx,qword[Opcode]
    shr rbx,3
    shl rbx,10
    add rax,rbx
    xor rbx,rbx
    mov bl,byte[codeW]
    shl rbx,8
    add rax,rbx
    xor rbx,rbx
    mov bl,byte[codeD]
    shl rbx,9
    add rax,rbx
    xor rbx,rbx
    mov bl,byte[Mod]
    shl rbx,6
    add rax,rbx
    xor rbx,rbx
    mov bl,byte[Reg]
    shl rbx,3
    add rax,rbx
    xor rbx,rbx
    mov bl,byte[RM]
    add rax,rbx

    mov qword[result],rax
    call newLine
    call IntToHexNum
    mov rsi,resultHexTemp
    call printString
    copyMem resultHexTemp
    copyMem newline
    copyBin resultHexTemp
    
    ret   

    ; call newLine
    ; mov rax,qword[REX]
    ; call writeNum


    ret
handlePushPop:
    xor rax,rax
    cmp byte[RexB],1
    je pushpopRex
    xor rbx,rbx
    mov bl,byte[RM]
    add rax,rbx
    mov rbx,0b01010
    shl rbx,3
    add rax,rbx
    mov rbx,qword[pre66]
    shl rbx,8
    add rax,rbx
    mov qword[result],rax
    
    
    call newLine
    call IntToHexNum
    mov rsi,resultHexTemp
    call printString
    copyMem resultHexTemp
    copyMem newline
    copyBin resultHexTemp
    
    ret
    pushpopRex:
    xor rbx,rbx
    mov bl,byte[RM]
    add rax,rbx
    mov rbx,0b01010
    shl rbx,3
    add rax,rbx
    mov rbx,qword[pre66]
    shl rbx,16
    add rax,rbx
    mov rbx,qword[REX]
    shl rbx,12
    add rax,rbx
    mov rbx,1
    shl rbx,8
    add rax,rbx

    mov qword[result],rax
    call newLine
    call IntToHexNum
    mov rsi,resultHexTemp
    call printString
    copyMem resultHexTemp
    copyMem newline
    copyBin resultHexTemp
    
    ret

handleJmp:
    xor rax,rax
    cmp byte[RexB],1
    je jmpRex
    xor rbx,rbx
    mov bl,byte[RM]
    add rax,rbx
    mov rbx,0b1111111111100
    shl rbx,3
    add rax,rbx
    mov rbx,qword[pre66]
    shl rbx,15
    add rax,rbx

    mov qword[result],rax
    
    call newLine
    call IntToHexNum
    mov rsi,resultHexTemp
    call printString
    copyMem resultHexTemp
    copyMem newline
    copyBin resultHexTemp
    
    ret
    
    jmpRex:
    xor rbx,rbx
    mov bl,byte[RM]
    add rax,rbx
    mov rbx,0b1111111111100
    shl rbx,3
    add rax,rbx
    mov rbx,qword[pre66]
    shl rbx,24
    add rax,rbx
    mov rbx,qword[REX]
    shl rbx,20
    add rax,rbx
    mov rbx,1
    shl rbx,16
    add rax,rbx
    mov qword[result],rax
    
    call newLine
    call IntToHexNum
    mov rsi,resultHexTemp
    call printString
    copyMem resultHexTemp
    copyMem newline
    copyBin resultHexTemp
    
    ; call writeNumhex
    ret 
    
handleCall:
    xor rax,rax
    cmp byte[RexB],1
    je callRex
    xor rbx,rbx
    mov bl,byte[RM]
    add rax,rbx
    mov rbx,0b1111111111010
    shl rbx,3
    add rax,rbx
    mov rbx,qword[pre66]
    shl rbx,15
    add rax,rbx

    mov qword[result],rax
    
    call newLine
    call IntToHexNum
    mov rsi,resultHexTemp
    call printString
    copyMem resultHexTemp
    copyMem newline
    copyBin resultHexTemp

    ret
    
    callRex:
    xor rbx,rbx
    mov bl,byte[RM]
    add rax,rbx
    mov rbx,0b1111111111010
    shl rbx,3
    add rax,rbx
    mov rbx,qword[pre66]
    shl rbx,24
    add rax,rbx
    mov rbx,qword[REX]
    shl rbx,20
    add rax,rbx
    mov rbx,1
    shl rbx,16
    add rax,rbx
    mov qword[result],rax
    
    call newLine
    call IntToHexNum
    mov rsi,resultHexTemp
    call printString
     copyMem resultHexTemp
    copyMem newline
    copyBin resultHexTemp
       ; call writeNumhex
    ret 
    


IntToHexNum:
    ;rax = number
    ;return Hex in rax
    xor rdx,rbx
    xor rbx,rbx
    mov rcx,16
    xor rdi,rdi

    ItHNumwhile:
        xor rdx,rdx
        cmp rax, 0
        jle ItHfinished
        div ecx
        push rax
        push rcx
        xor rax,rax
        mov al,dl
        call IntToHex
        mov byte[resultHexTemp+rdi],cl

        pop rcx
        pop rax
        inc rdi
        jmp ItHNumwhile


    ItHfinished:
    lea rsi,[resultHexTemp+rdi]
    dec rsi          ;pointer of last
    xor rdx,rdx      ;temp 1
    xor rbx,rbx      ;temp 2
    mov rdi,resultHexTemp  ;pointer of first
    ithReverse:
        cmp rsi, rdi
        jle rend
        mov dl, byte[rsi]
        mov bl, byte[rdi]
        mov byte[rdi], dl
        mov byte[rsi], bl
        sub rsi, 1
        add rdi, 1
        jmp ithReverse
    rend:
        ret
setOperandSize:
    mov rax,qword[operandSize]
    cmp rax,8
    jne Operand16
    mov byte[codeW],0
    ret    
    Operand16:
    cmp rax, 16
    jne Operand32
    mov byte[codeW],1
    mov qword[pre66],0x66
    ret
    Operand32:
    cmp rax,32
    jne Operand64
    mov byte[codeW],1
    ret
    Operand64:
    mov byte[codeW],1
    mov qword[REX], 0b100
    mov byte[RexW], 0b1
    ret




setUnaryRegD:
    cmp qword[Operand1],0b110100100
    je setUnaryRegD_0
    cmp qword[Operand1],0b110100101
    je setUnaryRegD_0

    mov byte[codeD],1
    ret
    setUnaryRegD_0:
    mov byte[codeD],0
    ret



getOperand:
    ;rax =  Operand1 register name
    push rdi
    xor rdi,rdi
    mov qword[operandSize], 64
    gtOpwhile64:
        cmp rdi, 256
        jge gtOpwhile32
        cmp rax,qword[reg64 + rdi]
        je findReg
        add rdi,16
        jmp gtOpwhile64
    gtOpwhile32:
        mov qword[operandSize], 32
        xor rdi,rdi
    gtOpwhile32t:
        cmp rdi, 256
        jge gtOpwhile16
        cmp rax,qword[reg32 + rdi]
        je findReg
        add rdi,16
        jmp gtOpwhile32t
    gtOpwhile16:
    mov qword[operandSize],16
    xor rdi,rdi
    gtOpwhile16t:
        cmp rdi, 256
        jge gtOpwhile8
        cmp rax,qword[reg16 + rdi]
        je findReg
        add rdi,16
        jmp gtOpwhile16t
    gtOpwhile8:
        mov qword[operandSize],8
        xor rdi,rdi
    gtOpwhile8t:
        cmp rax,qword[reg8+ rdi]
        je findReg
        add rdi,16
        jmp gtOpwhile8t
    
    findReg:
        mov rax,rdi
        xor rdx,rdx
        mov rdi,16
        div rdi
        mov qword[Operand1], rax
    pop rdi
    ret
checkRegOrMem:
    ;al = Op1[0]
    cmp al,'Q'
    je isMem
    cmp al,'D'
    je isMem
    cmp al,'W'
    je isMem
    cmp al,'B'
    je isMem

    jmp isReg

    isMem:
        mov byte[isOperandOneReg], 0
        ret
    isReg:
        mov byte[isOperandOneReg], 1
        ret



getOperator:
    push rdi
    push rsi
    push rbx
    push rdx

    xor rax,rax
    xor rbx,rbx
    xor rdi,rdi
    xor rsi,rsi

    mov rax, qword[Operation]

    gowhile:
        mov rbx,qword[unaryOpcode+rsi]
        cmp rax,rbx
        je gofounded
        add rsi,32
        jmp gowhile

    gofounded:
        add rsi,16
        mov rax,qword[unaryOpcode+rsi]
        mov qword[Opcode], rax

    pop rdx
    pop rbx
    pop rsi
    pop rdi

    ret





noneOperand:
    call newLine
    cmp qword[Operation],"ret"
    jne notRet
    mov rax,0b11000011
    call writeNumhex
    jmp noneOpEnd
    notRet:
    cmp qword[Operation],"std"
    jne notStd
    mov rax,0b11111101
    call writeNumhex
    jmp noneOpEnd
    notStd:
    cmp qword[Operation],"stc"
    jne notStc
    mov rax,0b11111001
    call writeNumhex
    jmp noneOpEnd
    notStc:
    cmp qword[Operation],"clc"
    jne notClc
    mov rax,0b11111000
    call writeNumhex
    jmp noneOpEnd
    notClc:
    cmp qword[Operation],"cld"
    jne notCld
    mov rax,0b11111100
    call writeNumhex
    jmp noneOpEnd
    notCld:
    mov rax,0b0000111100000101
    call writeNumhex

    noneOpEnd:
    mov byte[isfinished],1
    ret


IntToHex:
    push rax
    push rbx

    xor rbx,rbx
    
    ;al = int
    ;cl = hex
    cmp al,0
    mov bl,"0"
    je ItHEnd
    cmp al,1
    mov bl,"1"
    je ItHEnd
    cmp al,2
    mov bl,"2"
    je ItHEnd
    cmp al,3
    mov bl,"3"
    je ItHEnd
    cmp al,4
    mov bl,"4"
    je ItHEnd
    cmp al,5
    mov bl,"5"
    je ItHEnd
    cmp al,6
    mov bl,"6"
    je ItHEnd
    cmp al,7
    mov bl,"7"
    je ItHEnd
    cmp al,8
    mov bl,"8"
    je ItHEnd
    cmp al,9
    mov bl,"9"
    je ItHEnd
    cmp al,10
    mov bl,"a"
    je ItHEnd
    cmp al,11
    mov bl,"b"
    je ItHEnd
    cmp al,12
    mov bl,"c"
    je ItHEnd
    cmp al,13
    mov bl,"d"
    je ItHEnd
    cmp al,14
    mov bl,"e"
    je ItHEnd
    cmp al,15
    mov bl,"f"
    je ItHEnd   


    ItHEnd:
    xor rcx,rcx
    mov cl,bl
    pop rbx
    pop rax
    ret


setDefault:
    mov qword[needRex], 0
    mov qword[pre67], 0
    mov qword[pre66], 0
    mov qword[opsize], 0
    mov qword[dispsize], 0
    mov qword[InsSize], 0
    mov qword[Operation], 0
    mov qword[Operand1], 0
    mov qword[Operand2], 0
    mov byte[isfinished],0
    mov byte[RexB],0
    mov byte[RexX],0
    mov byte[RexR],0
    mov byte[RexW],0
    mov byte[isOperandOneReg],0
    mov qword[REX],0
    mov byte[hasBase],0
    mov byte[hasIndex],0
    mov byte[hasScale],0
    mov byte[hasDisplace],0
    mov qword[BaseRegName],0
    mov qword[IndexRegName],0
    mov qword[Scale],0
    mov qword[Displace],0
    mov qword[Base],0
    mov qword[Index],0


    call setresultHexDefult
    ret

setresultHexDefult:
    xor rdi,rdi
    srhdwhile:
        cmp rdi,51
        jge setresultHexDefultEnd
        mov qword[resultHexTemp+rdi],0
        mov byte[BaseRegName+rdi],0
        mov byte[IndexRegName+rdi],0
        mov byte[firstOprTemp+rdi],0
        mov byte[BinTemp+rdi], 0
        inc rdi
        jmp srhdwhile

    setresultHexDefultEnd:
    ret
breakIns:
    push rdi
    push rbx
    push rax
    push rcx
    xor rdi,rdi
    xor rbx,rbx
    xor rcx,rcx

    lea rbx, [line]
    brWhile:
        ; cmp rdi, qword[InsSize]
        ; je InsEnd
        cmp byte[line+rdi],' '   
        je findOp
        mov al,byte[line+rdi]
        mov byte[Operation+rdi],al
        inc rdi
        jmp brWhile
    
    findOp:
        inc rdi
        cmp rdi, qword[InsSize]
        jge NoOp
        xor rcx,rcx
        mov qword[operand1Adress],rdi
        jmp findOperand1
    findOperand1:

        cmp byte[line+rdi],','
        je findOperand2
        cmp byte[line+rdi],0
        je OneOp

        mov al,byte[line+rdi]
        mov byte[Operand1+rcx],al
        inc rdi
        inc rcx
        jmp findOperand1

    findOperand2:
        xor rcx,rcx
        inc rdi
        mov qword[operand2Adress],rdi
        jmp findOperand2temp
    findOperand2temp:
        cmp byte[line+rdi],0
        je TwoOp
        mov al,byte[line+rdi]
        mov byte[Operand2+rcx],al
        inc rdi
        inc rcx
        jmp findOperand2temp
    NoOp:
        mov qword[opsize],0
        pop rcx
        pop rax
        pop rbx
        pop rdi
        ret

    OneOp:
        mov qword[opsize],1
        pop rcx
        pop rax
        pop rbx
        pop rdi
        ret

    TwoOp:
        mov qword[opsize],2
        pop rcx
        pop rax
        pop rbx
        pop rdi
        ret    

copy:
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
    ; mov rsi,output
    ; call newLine
    ; call newLine
    ; call printString

    ; mov byte[r14],0
	mov	rsi,	enterSave
	call	printString
	mov	rax,	3
	mov	rbx,	2
	mov	rcx,	fileSave
	mov	rdx,	50
	int	80h

	;Replace the 0xA at the end of the name by NULL
	mov	rax,	-1
	wiwhile:
	inc	rax
	mov	bl,	[fileSave+rax]
	cmp	bl,	0xA
	jne	wiwhile
	mov	bl,	0
	mov	[fileSave+rax],	bl


    mov	rdi,	fileSave
    call	createFile

    mov	[OFD], rax
    mov	rdi,	[OFD]
    mov	rsi,	output
    call	length
    mov	rdx,	rax
    call	writeFile

    mov	rdi,	[OFD]
    call	closeFile



	mov	rsi,	enterSaveBin
	call	printString
	mov	rax,	3
	mov	rbx,	2
	mov	rcx,	fileSaveBin
	mov	rdx,	50
	int	80h

	;Replace the 0xA at the end of the name by NULL
	mov	rax,	-1
	wiwhilee:
	inc	rax
	mov	bl,	[fileSaveBin+rax]
	cmp	bl,	0xA
	jne	wiwhilee
	mov	bl,	0
	mov	[fileSaveBin+rax],	bl


    mov	rdi,	fileSaveBin
    call	createFile

    mov	[OFD], rax
    mov	rdi,	[OFD]
    mov	rsi,	outputBin
    call	length
    mov	rdx,	rax
    call	writeFile

    mov	rdi,	[OFD]
    call	closeFile




	mov	rax,	1
	mov	rbx,	0
	int	0x80




openFile:
;rdi <- fileName
	mov	rax,	sys_open
	mov	rsi,	O_RDWR
	syscall
    ret
createFile:
;rdi <- fileName
	mov	rax,	sys_create
	mov	rsi,	sys_IRUSR | sys_IWUSR
	syscall
    ret
writeFile:
;rdi <- fileDescriptor
;rsi <- buffer
;rdx <- length
	mov	rax,	sys_write
	syscall
    ret
readFile:
;rdi <- fileDescriptor
;rsi <- buffer
;rdx <- length
	mov	rax,	sys_read
	syscall
    mov	byte[rsi+rax],	0
    ret
closeFile:
;rdi <- fileDescriptor
    mov	rax,	sys_close
    syscall
    ret