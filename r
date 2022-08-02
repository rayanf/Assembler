shr rcx,1
shr rdx,0x10
add eax,DWORD PTR[esi+0x35]
add edx,DWORD PTR[ebx+ecx*1+0x32345467]
neg bx
not BYTE PTR[r11]
shl rbx,1
and cx,WORD PTR[rcx]
xor edx,DWORD PTR[ebp+r9d*4+0x55]
xor rdx,r8
xor rbx,rcx
mov ecx,DWORD PTR[ebx]
mov edx,DWORD PTR[ebp+0x12]
idiv r12
idiv DWORD PTR[rax+rbx*1+0x0]
add BYTE PTR[rdi+0xf],cl
and r14d,ebx
and r15b,r9b
or WORD PTR[edx*8+0x0],ax
inc r15
dec DWORD PTR[rcx+rbx*4+0x0]
add eax,DWORD PTR[esi+0x35]
syscall
