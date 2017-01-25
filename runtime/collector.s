.global switchme
.extern gc_collect
.text
switchme:
	pushl %eax
	pushl %ebx
	pushl %ecx
	movl 20(%esp), %eax
	movl 16(%esp), %ebx
	movl %esp, %ecx
	movl %ebx, %esp
	pushl %ecx
	pushl %eax
	call gc_collect
	popl %eax
	popl %esp
	popl %ecx
	popl %ebx
	popl %eax
	ret
