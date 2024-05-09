.align 4
.global _main
_main:
	sub sp, sp, #16
	stp x29, x30, [sp, #0]
	mov x29, sp
	bl main
	bl _exit
.align 4
.global main
main:
	sub sp, sp, #16; (1:43..13)
	stp x29, x30, [sp, #0]; (1:43..13)
	add x29, sp, #0; (1:43..13)
0:
	mov x0, xzr; (1:54..1)
	ldp x29, x30, [sp, #0]; (1:43..13)
	add sp, sp, #16; (1:43..13)
	ret x30; (1:43..13)
