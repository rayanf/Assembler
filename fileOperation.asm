%include "sys-equal.asm"


openFile:
;rdi <- fileName
	mov	rax,	sys_open
	mov	rsi,	O_RDWR
	syscall

createFile:
;rdi <- fileName
	mov	rax,	sys_create
	mov	rsi,	sys_IRUSR | sys_IWUSR
	syscall

writeFile:
;rdi <- fileDescriptor
;rsi <- buffer
;rdx <- length
	mov	rax,	sys_write
	syscall

readFile:
;rdi <- fileDescriptor
;rsi <- buffer
;rdx <- length
	mov	rax,	sys_read
	syscall
    mov	byte[rsi+rax],	0

closeFile:
;rdi <- fileDescriptor
    mov	rax,	sys_close
    syscall