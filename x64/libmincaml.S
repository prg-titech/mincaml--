#if defined(__CYGWIN__) || defined(__MACH__)
#define U(x) _##x
#else
#define U(x) x
#endif
#if defined(__MACH__)
#define ALIGNSTACK0 andl $0xfffffff0, %esp
#define ALIGNSTACK1 andl $0xfffffff0, %esp; pushq %rax; pushq %rax; pushq %rax
#define ALIGNSTACK2 andl $0xfffffff0, %esp; pushq %rax; pushq %rax
#define ALIGNSTACK3 andl $0xfffffff0, %esp; pushq %rax
#else
#define ALIGNSTACK0
#define ALIGNSTACK1
#define ALIGNSTACK2
#define ALIGNSTACK3
#endif
.text
.globl min_caml_print_newline
min_caml_print_newline:
	pushq	%rbp
	movq	%rsp, %rbp
  pushq %rdi
	ALIGNSTACK1
  movq  $10, %rdi
	call	U(putchar)
  popq  %rdi
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_print_int
min_caml_print_int:
	pushq	%rbp
	movq	%rsp, %rbp
	ALIGNSTACK2
  pushq  %rsi #save
  pushq  %rdi #save
  movq  %rax, %rsi
  movq  $format_int, %rdi
  movq  $0, %rax
	call	U(printf)
  popq  %rdi
  popq  %rsi
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_print_byte
min_caml_print_byte:
	pushq	%rbp
	movq	%rsp, %rbp
  pushq  %rdi
	ALIGNSTACK1
	movq	%rax, %rdi
	call	U(putchar)
  popq  %rdi
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_prerr_int
min_caml_prerr_int:
	pushq	%rbp
	movq	%rsp, %rbp
  pushq %rsi
  pushq %rdi
  pushq %rdx
	ALIGNSTACK3
	movq	%rax, %rdx
	movq	$format_int, %rsi
	movq	U(min_caml_stderr), %rdi
  movq  $0, %rax
	call	U(fprintf)
  movq  $0, %rax
  popq  %rdx
  popq  %rdi
  popq  %rsi
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_prerr_byte
min_caml_prerr_byte:
	pushq	%rbp
	movq	%rsp, %rbp
	ALIGNSTACK2
	pushq	U(min_caml_stderr)
	pushq	%rax
	call	U(fputc)
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_print_float
min_caml_print_float:
	pushq	%rbp
	movq	%rsp, %rbp
  pushq  %rsi #save
  pushq  %rdi #save
	ALIGNSTACK0
  movq  $format_float, %rsi
  movq  U(min_caml_stdout), %rdi
  movq  $1, %rax
	call	U(fprintf)
  movq  $0, %rax
  popq  %rdi
  popq  %rsi
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_prerr_float
min_caml_prerr_float:
	pushq	%rbp
	movq	%rsp, %rbp
  pushq  %rsi #save
  pushq  %rdi #save
	ALIGNSTACK0
  movq  $format_float, %rsi
  movq  U(min_caml_stderr), %rdi
  movq  $1, %rax
	call	U(fprintf)
  movq  $0, %rax
  popq  %rdi
  popq  %rsi
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_read_int
min_caml_read_int:
	pushq	%rbp
	movq	%rsp, %rbp
  pushq  %rsi #save
  pushq  %rdi #save
	ALIGNSTACK3
	subq	$8, %rsp
	leaq	-8(%rbp), %rax
  movq  %rax, %rsi
  movq  $format_int, %rdi
  movq  $0, %rax
	call	U(scanf)
	movq	-8(%rbp), %rax
  popq  %rdi
  popq  %rsi
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_read_float
min_caml_read_float:
	pushq	%rbp
	movq	%rsp, %rbp
  pushq  %rsi #save
  pushq  %rdi #save
	ALIGNSTACK0
	subq	$16, %rsp
  movsd %xmm0, -16(%rbp)
	leaq	-16(%rbp), %rax
  movq  %rax, %rsi
  movq  $format_read_float, %rdi
  movq  $0, %rax
	call	U(scanf)
  movsd -16(%rbp), %xmm0
  popq  %rdi
  popq  %rsi
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_create_array
min_caml_create_array:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%rcx
	pushq	%rdx
	movq	%rax, %rcx
	movq	min_caml_hp, %rax
	movq	%rcx, %rdx
	shlq	$3, %rdx
	addq	%rdx, min_caml_hp
create_array_loop:
	cmpq	$0, %rcx
	jne	create_array_cont
create_array_exit:
	popq	%rdx
	popq	%rcx
	movq	%rbp, %rsp
	popq	%rbp
	ret
create_array_cont:
	decq	%rcx
	movq	%rbx, (%rax,%rcx,8)
	jmp	create_array_loop
.globl min_caml_create_float_array
min_caml_create_float_array:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%rcx
	pushq	%rdx
	movq	%rax, %rcx
	movq	min_caml_hp, %rax
	movq	%rcx, %rdx
	shlq	$3, %rdx
	addq	%rdx, min_caml_hp
create_float_array_loop:
	cmpq	$0, %rcx
	jne	create_float_array_cont
create_float_array_exit:
	popq	%rdx
	popq	%rcx
	movq	%rbp, %rsp
	popq	%rbp
	ret
create_float_array_cont:
	decq	%rcx
	movsd	%xmm0, (%rax,%rcx,8)
	jmp	create_float_array_loop
.globl min_caml_abs_float
min_caml_abs_float:
	pushq	%rbp
	movq	%rsp, %rbp
	ALIGNSTACK2
	call	U(fabs)
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_sqrt
min_caml_sqrt:
	sqrtsd	%xmm0, %xmm0
	ret
.globl min_caml_floor
min_caml_floor:
	pushq	%rbp
	movq	%rsp, %rbp
	ALIGNSTACK2
	#subq	$8, %rsp
	#movsd	%xmm0, (%rsp)
	call	U(floor)
	#fstpl	(%rsp)
	#movsd	(%rsp), %xmm0
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_int_of_float
min_caml_int_of_float:
.globl min_caml_truncate
min_caml_truncate:
	cvttsd2si	%xmm0, %rax
	ret
.globl min_caml_float_of_int
min_caml_float_of_int:
	cvtsi2sd	%rax, %xmm0
	ret
.globl min_caml_cos
min_caml_cos:
	pushq	%rbp
	movq	%rsp, %rbp
	ALIGNSTACK2
	#subq	$8, %rsp
	#movsd	%xmm0, (%rsp)
	call	U(cos)
	#fstpl	(%rsp)
	#movsd	(%rsp), %xmm0
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_sin
min_caml_sin:
	pushq	%rbp
	movq	%rsp, %rbp
	ALIGNSTACK2
	#subq	$8, %rsp
	#movsd	%xmm0, (%rsp)
	call	U(sin)
	#fstpl	(%rsp)
	#movsd	(%rsp), %xmm0
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_atan
min_caml_atan:
	pushq	%rbp
	movq	%rsp, %rbp
	ALIGNSTACK2
	#subq	$8, %rsp
	#movsd	%xmm0, (%rsp)
	call	U(atan)
	#fstpl	(%rsp)
	#movsd	(%rsp), %xmm0
	movq	%rbp, %rsp
	popq	%rbp
	ret
.data
format_int:
	.asciz	"%d"
format_float:
	.asciz	"%g"
format_read_float:
	.asciz	"%lf"
.balign 8
float_0:
	.long	0x0
	.long	0x0
float_1:
	.long	0x0
	.long	0x3ff00000
.balign 16
.globl min_caml_fnegd
min_caml_fnegd:
	.long	0
	.long	0x80000000
	.long	0
	.long	0
.globl min_caml_hp
min_caml_hp:	
	.long	0x0
