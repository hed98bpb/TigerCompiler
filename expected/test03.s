
	.text
# PROCEDURE tigermain
	.globl	tigermain
	.func	tigermain
	.type	tigermain, @function
tigermain:
	# FRAME tigermain(1 formals, 15 locals)
	pushl %ebp
	movl %esp, %ebp
	subl $60, %esp
	# SP, FP, calleesaves, argregs have values
L6_blocks:                                        # x86gen:122
	movl %ebp, -8(%ebp)                       # x86gen:246 x86frame:575
	movl -8(%ebp), %ebx                       # x86gen:251 x86frame:367
	addl $-4, %ebx                            # x86gen:251 x86frame:372
	movl %ebx, -8(%ebp)                       # x86gen:251 x86frame:377
	movl -8(%ebp), %ebx                       # x86gen:117 x86frame:589
	movl %ebx, -24(%ebp)                      # x86gen:117 x86frame:594
	movl -12(%ebp), %ebx                      # x86gen:438 x86frame:320
	movl $2, %ebx                             # x86gen:438 x86frame:325
	movl %ebx, -12(%ebp)                      # x86gen:438 x86frame:330
	movl -12(%ebp), %ebx                      # x86gen:206 x86frame:265
	pushl %ebx                                # x86gen:206 x86frame:270
	call allocRecord                          # x86gen:68
	addl $4, %esp                             # x86gen:54
	movl %eax, -28(%ebp)                      # x86gen:70 x86frame:575
	movl -16(%ebp), %ebx                      # x86gen:431 x86frame:320
	movl $L1_string, %ebx                     # x86gen:431 x86frame:325
	movl %ebx, -16(%ebp)                      # x86gen:431 x86frame:330
	movl -28(%ebp), %ebx                      # x86gen:93 x86frame:302
	movl -16(%ebp), %ecx                      # x86gen:93 x86frame:307
	movl %ecx, 0(%ebx)                        # x86gen:93 x86frame:312
	movl -20(%ebp), %ebx                      # x86gen:438 x86frame:320
	movl $1000, %ebx                          # x86gen:438 x86frame:325
	movl %ebx, -20(%ebp)                      # x86gen:438 x86frame:330
	movl -28(%ebp), %ebx                      # x86gen:93 x86frame:302
	movl -20(%ebp), %ecx                      # x86gen:93 x86frame:307
	movl %ecx, 4(%ebx)                        # x86gen:93 x86frame:312
	movl -24(%ebp), %ebx                      # x86gen:114 x86frame:302
	movl -28(%ebp), %ecx                      # x86gen:114 x86frame:307
	movl %ecx, (%ebx)                         # x86gen:114 x86frame:312
	movl -4(%ebp), %ebx                       # x86gen:218 x86frame:340
	movl %ebx, -32(%ebp)                      # x86gen:218 x86frame:345
	movl -32(%ebp), %ebx                      # x86gen:117 x86frame:589
	movl %ebx, -36(%ebp)                      # x86gen:117 x86frame:594
	movl -36(%ebp), %ebx                      # x86gen:144 x86frame:265
	cmpl $0, %ebx                             # x86gen:144 x86frame:270
	je L2_fvar_nil
	jmp L3_fvar_nnil                          # x86gen:151
L3_fvar_nnil:                                     # x86gen:122
	movl -36(%ebp), %ebx                      # x86gen:265 x86frame:589
	movl %ebx, -52(%ebp)                      # x86gen:265 x86frame:594
	movl -40(%ebp), %ebx                      # x86gen:438 x86frame:320
	movl $4, %ebx                             # x86gen:438 x86frame:325
	movl %ebx, -40(%ebp)                      # x86gen:438 x86frame:330
	movl -40(%ebp), %ebx                      # x86gen:316 x86frame:589
	movl %ebx, -48(%ebp)                      # x86gen:316 x86frame:594
	movl -44(%ebp), %ebx                      # x86gen:438 x86frame:320
	movl $0, %ebx                             # x86gen:438 x86frame:325
	movl %ebx, -44(%ebp)                      # x86gen:438 x86frame:330
	movl -44(%ebp), %eax                      # x86gen:317 x86frame:582
	movl -48(%ebp), %ebx                      # x86gen:322 x86frame:418
	imull %ebx                                # x86gen:322 x86frame:423
	movl %eax, -48(%ebp)                      # x86gen:323 x86frame:575
	movl -52(%ebp), %ebx                      # x86gen:270 x86frame:522
	movl -48(%ebp), %ecx                      # x86gen:270 x86frame:528
	addl %ecx, %ebx                           # x86gen:270 x86frame:533
	movl %ebx, -52(%ebp)                      # x86gen:270 x86frame:539
	movl -56(%ebp), %ebx                      # x86gen:431 x86frame:320
	movl $L4_string, %ebx                     # x86gen:431 x86frame:325
	movl %ebx, -56(%ebp)                      # x86gen:431 x86frame:330
	movl -52(%ebp), %ebx                      # x86gen:114 x86frame:302
	movl -56(%ebp), %ecx                      # x86gen:114 x86frame:307
	movl %ecx, (%ebx)                         # x86gen:114 x86frame:312
	movl -4(%ebp), %ebx                       # x86gen:218 x86frame:340
	movl %ebx, -60(%ebp)                      # x86gen:218 x86frame:345
	movl -60(%ebp), %eax                      # x86gen:117 x86frame:582
	jmp L5_block_done                         # x86gen:172
L2_fvar_nil:                                      # x86gen:122
	call recFieldError                        # x86gen:179
	addl $0, %esp                             # x86gen:54
	jmp L3_fvar_nnil                          # x86gen:172
L5_block_done:                                    # x86gen:122
	# FP, SP, RV, calleesaves still live
	leave
	ret
	.size	tigermain, .-tigermain
	.endfunc
# END tigermain


	.data
L4_string:
	.long 8
	.asciz "Somebody"
	.data
L1_string:
	.long 6
	.asciz "Nobody"
	.data
L0_string:
	.long 13
	.asciz "DefaultString"
