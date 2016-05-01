
	.text
# PROCEDURE tigermain
	.globl	tigermain
	.func	tigermain
	.type	tigermain, @function
tigermain:
	# FRAME tigermain(1 formals, 7 locals)
	pushl %ebp
	movl %esp, %ebp
	subl $28, %esp
	# SP, FP, calleesaves, argregs have values
L2_blocks:                                        # x86gen:122
	movl %ebp, -8(%ebp)                       # x86gen:246 x86frame:575
	movl -8(%ebp), %ebx                       # x86gen:251 x86frame:367
	addl $-4, %ebx                            # x86gen:251 x86frame:372
	movl %ebx, -8(%ebp)                       # x86gen:251 x86frame:377
	movl -8(%ebp), %ebx                       # x86gen:117 x86frame:589
	movl %ebx, -20(%ebp)                      # x86gen:117 x86frame:594
	movl -12(%ebp), %ebx                      # x86gen:438 x86frame:320
	movl $0, %ebx                             # x86gen:438 x86frame:325
	movl %ebx, -12(%ebp)                      # x86gen:438 x86frame:330
	movl -12(%ebp), %ebx                      # x86gen:206 x86frame:265
	pushl %ebx                                # x86gen:206 x86frame:270
	movl -16(%ebp), %ebx                      # x86gen:438 x86frame:320
	movl $10, %ebx                            # x86gen:438 x86frame:325
	movl %ebx, -16(%ebp)                      # x86gen:438 x86frame:330
	movl -16(%ebp), %ebx                      # x86gen:206 x86frame:265
	pushl %ebx                                # x86gen:206 x86frame:270
	call initArray                            # x86gen:68
	addl $8, %esp                             # x86gen:54
	movl %eax, -24(%ebp)                      # x86gen:70 x86frame:575
	movl -20(%ebp), %ebx                      # x86gen:114 x86frame:302
	movl -24(%ebp), %ecx                      # x86gen:114 x86frame:307
	movl %ecx, (%ebx)                         # x86gen:114 x86frame:312
	movl -4(%ebp), %ebx                       # x86gen:218 x86frame:340
	movl %ebx, -28(%ebp)                      # x86gen:218 x86frame:345
	movl -28(%ebp), %eax                      # x86gen:117 x86frame:582
	jmp L1_block_done                         # x86gen:172
L1_block_done:                                    # x86gen:122
	# FP, SP, RV, calleesaves still live
	leave
	ret
	.size	tigermain, .-tigermain
	.endfunc
# END tigermain


	.data
L0_string:
	.long 13
	.asciz "DefaultString"
