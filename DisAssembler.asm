%include "in_out.asm"
%include "sys-equal.asm"
%include "fileOperation.asm"

    %macro testing 1
        push rax
        push rsi
        call newLine
        mov rsi,hah
        call printString
        mov rax, %1
        call writeNum
        pop rsi
        pop rax
    %endmacro


section .data
    IFD		dq	0 		;Input File Descriptor
	OFD		dq	0 		;Output File Descriptor
    ; testinst  db "mov rax,rbx",0,0
    bufferSize equ 10000
    reg64 dq "rax",0,"rcx",0,"rdx",0,"rbx",0,"rsp",0,"rbp",0,"rsi",0,"rdi",0,"r8", 0,"r9", 0,"r10", 0,"r11", 0,"r12", 0,"r13", 0,"r14", 0,"r15", 0
    reg32 dq "eax",0,"ecx",0,"edx",0,"ebx",0,"esp",0,"ebp",0,"esi",0,"edi",0,"r8d",0,"r9d",0,"r10d",0,"r11d",0,"r12d",0,"r13d",0,"r14d",0,"r15d",0
    reg16 dq "ax",0,"cx",0,"dx",0,"bx",0,"sp",0,"bp",0,"si",0,"di",0,"r8w",0,"r9w",0,"r10w",0,"r11w",0,"r12w",0,"r13w",0,"r14w",0,"r15w",0
    reg8  dq "al",0,"cl",0,"dl",0,"bl",0,"spl",0,"bpl",0,"sil",0,"dil",0,"r8b",0,"r9b",0,"r10b",0,"r11b",0,"r12b",0,"r13b",0,"r14b",0,"r15b",0
    space db " ",0
    commo db ",",0
    x     db "0x",0
    bytee db "BYTE",0
    wordd db "WORD",0
    dwordd db "DWORD",0
    qwordd db "QWORD",0
    ptr db "PTR",0
    braleft db "[",0
    braright db "]",0
    sum db "+",0
    mull db "*",0
    
    hah db "t ",0
    one db "1",0
    unaryOpcode dq  "inc",0,0b111111000,0,"dec",0,0b111111001,0,"call",0,0b111111010,0,"jmp",0,0b111111100,0,"push",0,0b111111110,0,"not",0,0b111101010,0,"neg",0,0b111101011,0,"imul",0,0b111101101,0,"idiv",0,0b111101111,0,"pop",0,0b100011000,0,"shl",0,0b110100100,0,"shr",0,0b110100101,0
    binaryOpcode dq "mov",0, 0b100010,0,"add",0, 0b000000,0,"adc",0, 0b000100,0,"sub",0, 0b001010,0,"sbb",0, 0b000110,0,"or",0, 0b000010,0,"xor",0, 0b001100,0,"and",0, 0b001000,0,"cmp",0, 0b001110,0,"shl",0, 0b000000,0,"shr",0, 0b000000,0,"test",0, 0b0000100001,0,"xchg",0, 0b0000100001,0
    ibinOpcode dq   "mov", 0,0b110001000,0,"add", 0,0b100000000,0,"adc", 0,0b100000010,0,"sub", 0,0b100000101,0,"sbb", 0,0b100000011,0,"or",  0,0b100000001,0,"xor", 0,0b100000110,0,"and", 0,0b100000100,0,"cmp", 0,0b100000111,0,"shl", 0,0b110000100,0,"shr", 0,0b110000101,0,"test",0,0b111101000,0


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
	Reg	    resQ 5	;Binary	|
	RM		resQ 5	;Binary	|
	Scale	resQ 5	;Binary	|
	Index   resQ 5	;Binary	|
	Base	resQ 5	;Binary	|	
	Displace resq 10	;Hex		|
	Data	resb 20	;Hex		|
    hasbase resb 1
    hasindex resb 1
    hasscale resb 1
    hasdisplace resb 1
    hexDisp resb 20

	;-----------------------------------|
	opsize	resq 1	;Operand size
    isfinished resb 0
    oprSize resq 1
    adrSize resq 1
	dispsize resq 1	;Displacement size
    memAdress resb  1
    needSIB resb    1
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
    cmp byte[isfinished], 1
    je finished
    
    xor rdi,rdi

    call checkPrefix
    call checkPrefix

    call checkRex

    call handlePopPushRet
    call checkUsualUnary
    cmp byte[isfinished],1
    je finished
    
    call checkUsualBinary
    ; testing 5
    cmp byte[isfinished],1
    je finished
    
    call checkImdRegBinary
    cmp byte[isfinished],1
    je finished

    finished:
    ; mov [output],rax
    ret


checkImdRegBinary:
    push rdi
    
    ; testing 4
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


    mov byte[codeD],1
    ; lea rsi,[line+rdi]    ;al = next char
    ; call printString
    call getRopCode           ;rax
    ; testing 1
    mov qword[Opcode],rax       ;Opcode name

    lea rsi,[line+rdi+1]    ;al = next char
    mov rdx, 1
    call HexNumtoIntBounded    ;rax
    and rax, 0b1
    mov byte[codeW],al          ;W
    mov byte[codeD],1
    
    add rdi,2
    call SetSize

    ;mod
    lea rsi,[line+rdi]
    mov rdx, 1
    call HexNumtoIntBounded    ;rax
    shr rax,2
    mov qword[Mod], rax          ;Mod
    
    ;rm
    lea rsi,[line+rdi+1]
    mov rdx, 1
    call HexNumtoIntBounded    ;rax
    and rax,0b111
    mov qword[RM], rax          ;RM

    cmp byte[Mod],3
    jne notMode3Imd
    
    mov rcx, qword[oprSize]
    mov rbx,rax
    call getREG         ;reg  rbx
    mov qword[reg1], rbx
    call setImd

    call newLine
    mov rsi,Opcode
    call printString
    mov rsi,space
    call printString
    mov rsi,reg1
    call printString
    mov rsi,commo
    call printString
    mov rsi,x
    call printString
    mov rsi,hexDisp
    call printString
    pop rdi
    mov byte[isfinished],1
    ret

    notMode3Imd:
        cmp byte[RM],0b100
        jne notRM4imd
        
        mov byte[memAdress],1
        mov byte[needSIB],1

        lea rsi,[line+rdi+2]
        mov rdx, 2
        call HexNumtoIntBounded    ;rax
        and rax,0b111
        mov qword[Base],rax             ;base

        lea rsi,[line+rdi+2]
        mov rdx, 2
        call HexNumtoIntBounded    ;rax
        shr rax,3
        and rax,0b111
        mov qword[Index],rax             ;index

        lea rsi,[line+rdi+2]
        mov rdx, 2
        call HexNumtoIntBounded    ;rax
        shr rax,6
        call SetScale                ;scale 

        add rdi,4

        call setImd
    
        mov qword[Displace], rax    ;disp

        cmp qword[Base],0b101
        jne Baseeimd
        cmp qword[dispsize],2
        je Baseeimd
        
        mov byte[hasbase],0
        jmp noBaseimd
    Baseeimd:
        mov byte[hasbase],1
        xor rbx,rbx
        mov bl,byte[RexB]
        shl bl,3
        mov rax,qword[Base]
        add rax,rbx
        mov rcx, qword[adrSize]
        mov rbx,rax
        call getREG         ;reg  rbx
        mov qword[Base], rbx
    noBaseimd:
        cmp qword[Index],0b100
        jne Indexeimd
        mov byte[hasindex],0
        mov byte[Index],0
        jmp PrintRegImd        
    Indexeimd:
        mov byte[hasindex],1
        mov rcx, qword[adrSize]
        xor rbx,rbx
        xor rax,rax
        mov bl, byte[Index]        
        mov al , byte[RexX]
        shl al,3
        add rbx,rax
        call getREG
        mov qword[Index], rbx  

    PrintRegImd:
        call newLine
        mov rsi,Opcode
        call printString
        mov rsi,space
        call printString
        call memorySizeMap
        mov rsi, space
        call printString
        mov rsi,ptr
        call printString
        mov rsi,braleft
        call printString


            cmp byte[hasbase],0
            je noBaseprintbin2imd
            mov rsi,Base
            call printString
            mov rsi, sum
            call printString
            noBaseprintbin2imd:
            cmp byte[hasindex],0
            je noIndexprintbin2imd
            mov rsi, Index
            call printString
            mov rsi, mull
            call printString
            mov rax, qword[Scale]
            call writeNum
            noIndexprintbin2imd:
            cmp byte[hasbase],1
            je printsumbin2imd
            cmp byte[hasindex],1
            je printsumbin2imd
            jmp notprintsumbin2imd
            printsumbin2imd:
            mov rsi, sum
            call printString
            notprintsumbin2imd:
            mov rsi, x
            call printString
            mov rsi, hexDisp
            call printString
            mov rsi, braright
            call printString

        mov rsi,commo
        call printString
        mov rsi,x
        call printString
        mov rsi,hexDisp
        call printString



        pop rdi
        mov byte[isfinished],1
        ret
    notRM4imd:

        mov byte[hasbase],1
        xor rbx,rbx
        mov bl,byte[RexB]
        shl bl,3
        xor rax,rax
        mov al,byte[RM]
        add rax,rbx
        mov rcx, qword[adrSize]
        mov rbx,rax
        call getREG         ;reg  rbx
        mov qword[Base], rbx

        add rdi,2
        

        call setImd

        call newLine
        mov rsi,Opcode
        call printString
        mov rsi, space
        call printString

                call memorySizeMap
        mov rsi, space
        call printString
        mov rsi, ptr
        call printString
        mov rsi, braleft
        call printString
        mov rsi, Base
        call printString

        cmp qword[Displace],0
        je disp0imdd
        mov rsi, sum
        call printString
        mov rsi,x
        call printString
        mov rsi, hexDisp
        call printString

        disp0imdd:
        mov rsi, braright
        call printString

        mov rsi, commo
        call printString
        mov rsi, reg1
        call printString



    pop rdi
    ret

setImd:
    push rdi

    add rdi,2


    lea rsi,[line+rdi]
    call length
    mov qword[dispsize] , rax
    lea rsi,[line+rdi]
    call setDisp

    pop rdi
    ret
    

; inc r9d
; inc Qword ptr [r9+rbx*8]
; dec qword ptr[0x12]
; inc qword ptr[rbx*4]
; inc qword ptr[rsi+rbx*4]
; neg dword ptr[r8d+0x12]
; inc qword ptr[rbx*4+0x2]
; inc dword ptr[rax*4]
; inc qword ptr[rax*4]
; inc qword ptr[rsp+rbp*2]
; inc qword ptr[rbp+rax*2]
; inc qword ptr[rax*2]
; inc qword ptr[rax+rbp*2+0x0000000000]
; inc qword ptr[rax+0x0]
; inc qword ptr[rax+0x12]
; inc qword ptr[rax+0x123]
; inc qword ptr[rax+0xab]
; cmp rbx,qword ptr[rax+rbx*4+0x12]
; mov qword ptr[rax+rbx*4+0x12],rcx
; add rax, qword ptr[r9d+0x12]
; add qword ptr[r9d+0x12],rax
; mov rax,0x12
; mov dword ptr[r8+r9*1+0x12],0x15
; mov dword ptr[r8+0x12],0x15




checkUsualBinary:
    push rdi
    mov al,byte[line+rdi]      ;al = next char
    mov bl,byte[line+rdi+1]    ;bl = Dnext char

        ; testing 7
    cmp al,'8'
    jl BByes
    je BB
    pop rdi
    ret
    BB: 
        cmp bl,'3'
        jg BByes
        pop rdi
        ret
    BByes:
        lea rsi,[line+rdi]     
        mov rdx, 2
        call HexNumtoIntBounded    ;rax
        shr rax,2
        mov qword[Opcode], rax                  

        mov byte[opsize],2

        lea rsi,[line+rdi+1]
        mov rdx, 1
        call HexNumtoIntBounded    ;rax
        shr rax,1
        and rax,0b1                 
        mov byte[codeD], al         ;d

        ; testing 7
        call getOperand2
        mov qword[Opcode], rax
        ; testing 1

        ; call newLine
        ; mov rsi,Opcode
        ; call printString

        lea rsi,[line+rdi+1]
        mov rdx, 1
        call HexNumtoIntBounded    ;rax
        and rax,0b1
        ; testing 5
        mov byte[codeW], al         ;w

        add rdi,2
        call SetSize                ;set Size 

        ;handle reg
        
        lea rsi,[line+rdi]
        mov rdx, 2
        call HexNumtoIntBounded    ;rax
        shr rax,3
        and rax,0b111
        ; mov rbx,rax
        xor rbx,rbx
        mov bl,byte[RexR]
        shl bl,3
        add rax,rbx

        mov rcx, qword[oprSize]
        mov rbx,rax
        call getREG         ;reg  rbx
        mov qword[reg1], rbx

        ; testing 5
        ; mov rsi,reg1
        ; call printString
        
        lea rsi,[line+rdi]
        mov rdx, 1
        call HexNumtoIntBounded    ;rax
        shr rax,2
        mov byte[Mod], al         ;mod

        lea rsi,[line+rdi+1]
        mov rdx, 1
        call HexNumtoIntBounded    ;rax
        and rax,0b111
        mov byte[RM], al         ;rm

        cmp byte[Mod],0b11                
        jne notMod3bin

        xor rax,rax
        mov al,byte[RexB]
        shl al,3
        add al,byte[RM]

        mov rcx, qword[oprSize]
        mov rbx,rax
        call getREG         ;reg  rbx
        mov qword[reg2], rbx

        ; mov rsi,reg2
        ; call printString
        cmp byte[codeD],0
        je revereceRegPrint
        cmp qword[Opcode],"xchg"
        je revereceRegPrint
        call newLine
        mov rsi,Opcode
        call printString
        mov rsi, space
        call printString
        mov rsi,reg1
        call printString
        mov rsi,commo
        call printString
        mov rsi,reg2
        call printString
        pop rdi
        mov byte[isfinished],1
        ret


        revereceRegPrint:
        call newLine
        mov rsi,Opcode
        call printString
        mov rsi, space
        call printString
        mov rsi,reg2
        call printString
        mov rsi,commo
        call printString
        mov rsi,reg1
        call printString
        pop rdi
        mov byte[isfinished],1
        ret
        notMod3bin:
        cmp byte[RM],0b100
        jne notRM4bin
        
        mov byte[memAdress],1
        mov byte[needSIB],1

        lea rsi,[line+rdi+2]
        mov rdx, 2
        call HexNumtoIntBounded    ;rax
        and rax,0b111
        mov qword[Base],rax             ;base

        lea rsi,[line+rdi+2]
        mov rdx, 2
        call HexNumtoIntBounded    ;rax
        shr rax,3
        and rax,0b111
        mov qword[Index],rax             ;index

        lea rsi,[line+rdi+2]
        mov rdx, 2
        call HexNumtoIntBounded    ;rax
        shr rax,6
        call SetScale                ;scale 

        add rdi,4

        lea rsi,[line+rdi]
        call length
        mov qword[dispsize] , rax
        lea rsi,[line+rdi]

        call setDisp
    
        mov qword[Displace], rax    ;disp

        cmp qword[Base],0b101
        jne BaseeBin
        cmp qword[dispsize],2
        je BaseeBin
        
        mov byte[hasbase],0
        jmp noBaseBin

        BaseeBin:
        mov byte[hasbase],1
        xor rbx,rbx
        mov bl,byte[RexB]
        shl bl,3
        mov rax,qword[Base]
        add rax,rbx
        mov rcx, qword[adrSize]
        mov rbx,rax
        call getREG         ;reg  rbx
        mov qword[Base], rbx
        noBaseBin:
        cmp qword[Index],0b100
        jne IndexeBin
        mov byte[hasindex],0
        mov byte[Index],0
        jmp PrintRegMem

        IndexeBin:
        mov byte[hasindex],1
        mov rcx, qword[adrSize]
        xor rbx,rbx
        xor rax,rax
        mov bl, byte[Index]        
        mov al , byte[RexX]
        shl al,3
        add rbx,rax
        call getREG
        mov qword[Index], rbx        ;index name
        PrintRegMem:
        call newLine
        cmp byte[codeD],0
        jne printRegSIBfirst

        mov rsi,Opcode
        call printString
        mov rsi, space
        call printString

            call memorySizeMap
                    mov rsi, space
        call printString
        mov rsi,ptr
        call printString
        mov rsi,braleft
        call printString


            cmp byte[hasbase],0
            je noBaseprintbin2
            mov rsi,Base
            call printString
            mov rsi, sum
            call printString
            noBaseprintbin2:
            cmp byte[hasindex],0
            je noIndexprintbin2
            mov rsi, Index
            call printString
            mov rsi, mull
            call printString
            mov rax, qword[Scale]
            call writeNum
            noIndexprintbin2:
            cmp byte[hasbase],1
            je printsumbin2
            cmp byte[hasindex],1
            je printsumbin2
            jmp notprintsumbin2
            printsumbin2:
            mov rsi, sum
            call printString
            notprintsumbin2:
            mov rsi, x
            call printString
            mov rsi, hexDisp
            call printString
            mov rsi, braright
            call printString
    
        mov rsi,commo
        call printString
        mov rsi,reg1
        call printString
        pop rdi
        mov byte[isfinished],1
        ret
        printRegSIBfirst:

        mov rsi,Opcode
        call printString
        mov rsi, space
        call printString
        mov rsi,reg1
        call printString
        mov rsi,commo
        call printString
        call memorySizeMap
        mov rsi, space
        call printString
        mov rsi, ptr
        call printString
        mov rsi, braleft
        call printString


            cmp byte[hasbase],0
            je noBaseprintbin
            mov rsi,Base
            call printString
            mov rsi, sum
            call printString
            noBaseprintbin:
            cmp byte[hasindex],0
            je noIndexprintbin
            mov rsi, Index
            call printString
            mov rsi, mull
            call printString
            mov rax, qword[Scale]
            call writeNum
            noIndexprintbin:
            cmp byte[hasbase],1
            je printsumbin
            cmp byte[hasindex],1
            je printsumbin
            jmp notprintsumbin
            printsumbin:
            mov rsi, sum
            call printString
            notprintsumbin:
            mov rsi, x
            call printString
            mov rsi, hexDisp
            call printString
            mov rsi, braright
            call printString

        mov byte[isfinished],1
        pop rdi
        ret
        notRM4bin:
        mov byte[needSIB],0
        mov byte[memAdress],1

        mov byte[hasbase],1
        xor rbx,rbx
        mov bl,byte[RexB]
        shl bl,3
        xor rax,rax
        mov al,byte[RM]
        add rax,rbx
        mov rcx, qword[adrSize]
        mov rbx,rax
        call getREG         ;reg  rbx
        mov qword[Base], rbx

        add rdi,2
        
        lea rsi,[line+rdi]
        call length
        mov qword[dispsize] , rax
        lea rsi,[line+rdi]

        call setDisp

        call newLine
        mov rsi,Opcode
        call printString
        mov rsi, space
        call printString

        cmp byte[codeD],0
        jne printRegfirst

        call memorySizeMap
        mov rsi, space
        call printString
        mov rsi, ptr
        call printString
        mov rsi, braleft
        call printString
        mov rsi, Base
        call printString

        cmp qword[Displace],0
        je disp0binn
        mov rsi, sum
        call printString
        mov rsi,x
        call printString
        mov rsi, hexDisp
        call printString
        disp0binn:
        mov rsi, braright
        call printString

        mov rsi, commo
        call printString
        mov rsi, reg1
        call printString

        mov byte[isfinished],1
        pop rdi
        ret
        printRegfirst:
        mov rsi,reg1
        call printString
        mov rsi, commo
        call printString

        call memorySizeMap
        mov rsi, space
        call printString
        mov rsi, ptr
        call printString
        mov rsi, braleft
        call printString
        mov rsi, Base
        call printString

        cmp qword[Displace],0
        je disp0bin
        mov rsi, sum
        call printString
        mov rsi,x
        call printString
        mov rsi, hexDisp
        call printString
        disp0bin:
        mov rsi, braright
        call printString

        mov byte[isfinished],1
        pop rdi
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
    cmp al,'4'
    
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
        
        call getRopCode           ;rax
        mov qword[Opcode],rax       ;Opcode name

        lea rsi,[line+rdi+1]
        mov rdx, 1
        call HexNumtoIntBounded    ;rax <-
        and rax, 0b1
        mov byte[codeW], al          ;W

        add rdi ,2
        call SetSize
        
        lea  rsi,[line+rdi]      ;al = next char
        mov rdx, 1
        call HexNumtoIntBounded    ;rax <-  
        shr rax, 2                  ;mode    
        mov qword[Mod], rax          
       
        lea rsi, [line+rdi+1]     ;al = next char
        mov rdx, 1
        call HexNumtoIntBounded    ;rax <-
        and rax, 0b111
        mov qword[RM], rax          ;rm

        call handlePushPopOprSize

        cmp byte[Mod], 0b11
        jne notMod3
        mov byte[needSIB],0
        mov byte[memAdress],0
        

        xor r9,r9
        xor rbx,rbx
        mov rcx,qword[oprSize]
        mov bl,Byte[RM]
        mov r9b , byte[RexB]    
        shl r9b,3
        add rbx,r9
        mov rax,rbx
        ; call newLine 
        ; call writeNum
        call getREG
        ; testing 5
        mov qword[Reg], rbx


        call newLine
        mov rsi, Opcode
        call printString
        mov rsi, space
        call printString
        mov rsi, Reg
        call printString
        cmp qword[Opcode],"shr"
        je handleshrshl
        cmp qword[Opcode],"shl"
        je handleshrshl


        pop rdi
        mov byte[isfinished],1
        ret        
        notMod3:
        ; testing 2

        cmp byte[RM], 0b100
        jne notRM4
        mov byte[needSIB],1
        mov byte[memAdress],1

        mov byte[hasbase],1
        mov byte[hasindex],1


        lea rsi,[line+rdi+2]
        mov rdx, 2
        call HexNumtoIntBounded    ;rax <-
        and rax, 0b111              ;Base
        mov byte[Base], al          ;Base
        ; xor rax,rax
        ; mov al,byte[Base]
        ; call newLine
        ; call writeNum




        lea rsi,[line+rdi+2]
        mov rdx, 2
        call HexNumtoIntBounded    ;rax <-
        shr rax, 3
        and rax, 0b111              ;Index
        mov byte[Index], al          ;Index

        lea rsi,[line+rdi+2]
        mov rdx, 2
        call HexNumtoIntBounded    ;rax <-
        shr rax,6

        call SetScale               ;scale

        ;pointe += 4
        add rdi,4

        ;rsi  pointer
        ;rax return len
        lea rsi, [line+rdi]      
        call length
        mov qword[dispsize], rax
        lea rsi, [line+rdi]      


        call setDisp                ;disp
        mov qword[Displace],rax

        cmp byte[Base], 0b101
        jne Basee
        cmp byte[Mod],0
        jne Basee
        cmp qword[dispsize],8
        jne Basee

        mov byte[hasbase],0
        mov byte[Base],0 
        jmp indexx

        Basee:
        mov rcx,qword[adrSize]
        xor rbx,rbx
        xor rax,rax
        mov bl, byte[Base]        
        mov al , byte[RexB]
        shl al,3
        add rbx,rax
        
        call getREG
        mov qword[Base], rbx        ;Base name

        indexx:
        
        cmp qword[Index], 0b100
        jne indexxx
        mov byte[Index],0
        mov byte[hasindex],0
        jmp printtt 
        indexxx:
        mov rcx,qword[adrSize]
        xor rbx,rbx
        xor rax,rax
        mov bl, byte[Index]        
        mov al , byte[RexX]
        shl al,3
        add rbx,rax
        
        ; mov rax, rbx
        ; call newLine
        ; call writeNum

        call getREG
        mov qword[Index], rbx        ;index name
        printtt:
        call PrintMem
        cmp qword[Opcode],"shr"
        je handleshrshl
        cmp qword[Opcode],"shl"
        je handleshrshl


        pop rdi
        mov byte[isfinished],1
        ret
        notRM4:
        mov byte[needSIB],0
        mov byte[memAdress],1


        mov byte[hasindex],0
        mov byte[hasbase],1
        mov rcx,qword[adrSize]
        xor rbx,rbx
        xor rax,rax
        mov bl, byte[RM]        
        mov al , byte[RexB]
        shl al,3
        add rbx,rax
        call getREG
        mov qword[RM], rbx    
        mov qword[Base], rbx
        
        add rdi,2

        lea rsi, [line+rdi]      
        call length
        mov qword[dispsize], rax
        lea rsi, [line+rdi]
        ; call newLine
        ; call writeNum
        call setDisp                ;disp
        mov qword[Displace],rax
        call PrintMem
        cmp qword[Opcode],"shr"
        je handleshrshl
        cmp qword[Opcode],"shl"
        je handleshrshl


        pop rdi
        mov byte[isfinished],1
        ret


handleshrshl:
    mov rsi,commo
    call printString
    mov rsi,one
    call printString
    pop rdi
    mov byte[isfinished],1
    ret
InttoHex:
;rax = int
    push rsi
    push rdi
    push rbx
    push rcx
    push rdx

    xor rdi,rdi ;counter
    mov rsi, hexDisp  ;result str
    xor rdx,rdx ;reminder
    xor rbx,rbx  ;16
    
    mov ebx, 16
    ithWhile:
        cmp rax, 0
        je reverce
        xor rdx,rdx
        div ebx     ;reminder rdx

        cmp rdx, 9
        jle plus30
        add rdx, 87
        mov byte[rsi], dl
        add rsi, 1
        jmp ithWhile

        plus30:
        add rdx, 0x30
        mov byte[rsi], dl
        add rsi, 1
        jmp ithWhile
    reverce:
    dec rsi          ;pointer of last
    xor rdx,rdx      ;temp 1
    xor rbx,rbx      ;temp 2
    mov rdi,hexDisp  ;pointer of first
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
    pop rdx
    pop rcx
    pop rbx
    pop rdi
    pop rsi
    ret
PrintMem:
    call newLine
    mov rsi, Opcode
    call printString
    mov rsi, space
    call printString

    call memorySizeMap
    mov rsi, space
    call printString
    mov rsi, ptr
    call printString
    mov rsi, braleft
    call printString


    cmp byte[needSIB],0
    je noSIBprint
    cmp byte[hasbase],0
    je noBaseprint
    mov rsi,Base
    call printString
    mov rsi, sum
    call printString
    noBaseprint:
    cmp byte[hasindex],0
    je noIndexprint
    mov rsi, Index
    call printString
    mov rsi, mull
    call printString
    mov rax, qword[Scale]
    call writeNum
    noIndexprint:
    cmp byte[hasbase],1
    je printsum
    cmp byte[hasindex],1
    je printsum
    jmp notprintsum
    printsum:
    mov rsi, sum
    call printString
    notprintsum:
    mov rsi, x
    call printString
    mov rsi, hexDisp
    call printString
    mov rsi, braright
    call printString
    ret
    noSIBprint:
    mov rsi, Base
    call printString

    cmp qword[Displace],0
    je disp0
    mov rsi, sum
    call printString
    mov rsi,x
    call printString
    mov rsi, hexDisp
    call printString
    disp0:
    mov rsi, braright
    call printString

    ret

memorySizeMap:
    
    cmp qword[oprSize],8
    jne worddd
    mov rsi, bytee
    call printString
    ret
    worddd:
    cmp qword[oprSize],16
    jne dworddd
    mov rsi, wordd
    call printString
    ret
    dworddd:
    cmp qword[oprSize],32
    jne qworddd
    mov rsi, dwordd
    call printString
    ret
    qworddd:
    mov rsi, qwordd
    call printString
    ret
setDisp:
    push rdi   
    ;rdi = counter
    ;r10 = len
    ;rsi = pointer
    ;rbx = disp
    ;rsi = local counter
    xor rdi,rdi
    xor rbx,rbx
    mov r10,rax
    inc rdi
    xor r11,r11
    inc r10
    
    dispwhile1:
        cmp rdi,r10
        jge disdone
        mov rdx,1
        call HexNumtoIntBounded    ;rax <-
        ; call newLine
        ; call writeNum
        ; call printString
        xor r11,r11
        localwhile1:
            cmp r11,rdi
            je ldone1
            shl rax,4
            inc r11
            jmp localwhile1
        ldone1:
        ; call newLine
        ; call writeNum
        add rbx,rax
        inc rdi
        inc rsi
        jmp dispwhile2

    dispwhile2:
        cmp rdi,r10
        jge disdone
        mov rdx,1
        call HexNumtoIntBounded    ;rax <-
        ; mov rax ,rdi
        ; call newLine
        ; call writeNum
        ; call printString
        dec rdi
        dec rdi
        xor r11,r11
        localwhile2:
            cmp r11,rdi
            je ldone2
            shl rax,4
            inc r11
            jmp localwhile2
        ldone2:       

        inc rdi
        inc rdi
        add rbx,rax
        inc rdi
        inc rsi
        jmp dispwhile1
    
    disdone:
    mov qword[Displace],rbx
    mov rax,rbx
    ; call newLine
    ; call writeNum
    call InttoHex
    cmp rbx,0
    ; call writeNum
    jne notZeroDisp
    mov byte[hexDisp],"0"

    ; call newLine
    ; lea rsi, [line+rdi]
    ; call printString

    notZeroDisp:
    mov rax,rbx
    pop rdi
    ret
SetScale:
    cmp rax, 0
    jne scale1
    mov byte[Scale],1
    ret
    scale1:
    cmp rax, 1
    jne scale2
    mov byte[Scale],2
    ret
    scale2:
    cmp rax, 2
    jne scale3
    mov byte[Scale],4
    ret
    scale3:
    mov byte[Scale],8
    ret





getREG:
;rcx = oprSize
;rbx = reg code

    cmp rcx, 8
    jne greg16
    call getReg8
    jmp regEnd
    greg16:
    cmp rcx, 16
    jne greg32
    call getReg16
    jmp regEnd
    greg32:
    cmp rcx, 32
    jne greg64
    call getReg32
    ; testing 2
    jmp regEnd
    greg64:
    call getReg64
    regEnd:
    ret

handlePushPopOprSize:
    cmp qword[Opcode],"push"
    je ppyes
    cmp qword[Opcode],"pop"
    je ppyes
    ret
    ppyes:
        mov qword[oprSize],64
        ret

SetSize:
    ; call newLine
    ; mov al, byte[pre67]
    ; call writeNum


    cmp byte[pre67],1
    jne addres64
    mov qword[adrSize],32
    jmp pre66han
    addres64:
    mov qword[adrSize],64
    pre66han:
    cmp byte[pre66],0
    je oprsizeNot16
    mov qword[oprSize],16
    jmp finishSizing
    oprsizeNot16:
    cmp byte[RexW],0
    je oprsizeNot64
    mov qword[oprSize],64
    jmp finishSizing
    oprsizeNot64:
    cmp byte[codeW],0
    je oprsizeNot32
    ; testing 5
    mov qword[oprSize],32
    jmp finishSizing
    oprsizeNot32:
    mov qword[oprSize],8
    finishSizing:
    ; call newLine
    ; mov rax,qword[oprSize]
    ; call writeNum
    ret

getRopCode:
    push rdi
    push rsi
    push rbx
    push rdx

    xor rax,rax
    xor rbx,rbx
    xor rdi,rdi
    xor rsi,rsi

    mov rax,qword[Opcode]
    shl rax,3
    add rax,qword[Reg]
    ; call newLine
    ; call writeNumhex
    
    add rsi,16
    
    gowhile:
        mov rbx,qword[unaryOpcode+rsi]
        cmp rax,rbx
        je gofounded

        add rsi,32
        jmp gowhile
    
    gofounded:
        sub rsi, 16
        mov rax,qword[unaryOpcode+rsi]
        
    pop rdx
    pop rbx
    pop rsi
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
        mov byte[isfinished],1
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
        mov byte[isfinished],1
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
    mov byte[codeD], 0
    mov byte[codeW], 0
    mov byte[hasdisplace], 0
    mov byte[hasbase], 0
    mov byte[hasindex],0
    call sethexdispDefult
    mov byte[isfinished],0

    ret
sethexdispDefult:
    push rsi
    push rdi
    push rax
    xor rdi,rdi
    mov rsi, hexDisp
    mov rax,20
    dwhile:
        cmp rdi,rax
        je ddone
        mov byte[rsi+rdi],0
        add rdi,1
        jmp dwhile
    ddone:
        pop rax
        pop rdi
        pop rsi
        ret

HexNumtoIntBounded:
;rsi <- start of num
    push rsi
    push rbx
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
        pop rbx
        pop rsi
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

    pop rsi
    pop rax
    ret
    Pre66:
        mov byte[pre66],1
        add rdi,2
        inc byte[NumPre]
        pop rsi
        pop rax
        ret
    Pre67:
        mov byte[pre67],1
        add rdi,2
        inc byte[NumPre]
        pop rsi
        pop rax
        ret

checkNoneOperand:     
    mov rsi, line
    call HexNumtoInt
    mov qword[code],rbx
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
    ret
    done:
        mov byte[isfinished],1
        call newLine
        mov qword[Opcode],rax
        mov rsi, Opcode
        call printString
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


getOperand2:
    push rdi
    push rsi
    push rbx
    push rdx

    xor rax,rax
    xor rbx,rbx
    xor rdi,rdi
    xor rsi,rsi
    xor rdx,rdx

    mov rax,qword[Opcode]
    ; call newLine
    ; call writeNumhex
    mov rsi,16
    gowhileB:
        mov rbx,qword[binaryOpcode+rsi]
        cmp rax,rbx
        je gofoundedB

        add rsi,32
        jmp gowhileB
    
    gofoundedB:
        sub rsi, 16
    
        cmp qword[binaryOpcode+rsi],"test"
        jne gofoundedB2
        cmp byte[codeD],1
        jne gofoundedB2
        ; testing 9  
        add rsi,32
        je gowhileB

        gofoundedB2:
        mov rax,qword[binaryOpcode+rsi]
        
    pop rdx
    pop rbx
    pop rsi
    pop rdi
    ret    
