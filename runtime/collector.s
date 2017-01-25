.global switchme
.extern gc_collect
.text
switchme:
	pushl %eax
	movl %esp, %eax
	movl %edi, %esp
	pushl %eax
	movl %esi, %edi
	call gc_collect
	popl %esp
	popl %eax
	ret
