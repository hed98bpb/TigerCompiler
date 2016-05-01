
	.text
# PROCEDURE tigermain
	.globl	tigermain
	.func	tigermain
	.type	tigermain, @function
tigermain:
	# FRAME tigermain(1 formals, 1 locals)
	pushl %ebp
	movl %esp, %ebp
	subl $4, %esp
	# SP, FP, calleesaves, argregs have values
L6_blocks:                                        # x86gen:122
	movl -4(%ebp), %ebx                       # x86gen:438 x86frame:320
	movl $10, %ebx                            # x86gen:438 x86frame:325
	movl %ebx, -4(%ebp)                       # x86gen:438 x86frame:330
	movl -4(%ebp), %ebx                       # x86gen:206 x86frame:265
	pushl %ebx                                # x86gen:206 x86frame:270
	pushl %ebp                                # x86gen:206
	call L1_nfactor                           # x86gen:68
	addl $8, %esp                             # x86gen:54
	movl %eax, %eax                           # x86gen:70
	jmp L5_block_done                         # x86gen:172
L5_block_done:                                    # x86gen:122
	# FP, SP, RV, calleesaves still live
	leave
	ret
	.size	tigermain, .-tigermain
	.endfunc
# END tigermain



	.text
# PROCEDURE L1_nfactor
	.globl	L1_nfactor
	.func	L1_nfactor
	.type	L1_nfactor, @function
L1_nfactor:
	# FRAME L1_nfactor(2 formals, 10 locals)
	pushl %ebp
	movl %esp, %ebp
	subl $40, %esp
	# SP, FP, calleesaves, argregs have values
L8_blocks:                                        # x86gen:122
	movl 12(%ebp), %ebx                       # x86gen:218 x86frame:340
	movl %ebx, -4(%ebp)                       # x86gen:218 x86frame:345
	movl -4(%ebp), %ebx                       # x86gen:144 x86frame:265
	cmpl $0, %ebx                             # x86gen:144 x86frame:270
	je L2_if_then
	jmp L3_if_else                            # x86gen:151
L3_if_else:                                       # x86gen:122
	movl 12(%ebp), %ebx                       # x86gen:218 x86frame:340
	movl %ebx, -8(%ebp)                       # x86gen:218 x86frame:345
	movl -8(%ebp), %ebx                       # x86gen:117 x86frame:589
	movl %ebx, -28(%ebp)                      # x86gen:117 x86frame:594
	movl 12(%ebp), %ebx                       # x86gen:218 x86frame:340
	movl %ebx, -12(%ebp)                      # x86gen:218 x86frame:345
	movl -12(%ebp), %ebx                      # x86gen:275 x86frame:589
	movl %ebx, -16(%ebp)                      # x86gen:275 x86frame:594
	movl -16(%ebp), %ebx                      # x86gen:280 x86frame:367
	subl $1, %ebx                             # x86gen:280 x86frame:372
	movl %ebx, -16(%ebp)                      # x86gen:280 x86frame:377
	movl -16(%ebp), %ebx                      # x86gen:206 x86frame:265
	pushl %ebx                                # x86gen:206 x86frame:270
	movl 8(%ebp), %ebx                        # x86gen:218 x86frame:340
	movl %ebx, -20(%ebp)                      # x86gen:218 x86frame:345
	movl -20(%ebp), %ebx                      # x86gen:206 x86frame:265
	pushl %ebx                                # x86gen:206 x86frame:270
	call L1_nfactor                           # x86gen:68
	addl $8, %esp                             # x86gen:54
	movl %eax, -24(%ebp)                      # x86gen:70 x86frame:575
	movl -24(%ebp), %ebx                      # x86gen:316 x86frame:589
	movl %ebx, -32(%ebp)                      # x86gen:316 x86frame:594
	movl -28(%ebp), %eax                      # x86gen:317 x86frame:582
	movl -32(%ebp), %ebx                      # x86gen:322 x86frame:418
	imull %ebx                                # x86gen:322 x86frame:423
	movl %eax, -32(%ebp)                      # x86gen:323 x86frame:575
	movl -32(%ebp), %ebx                      # x86gen:117 x86frame:589
	movl %ebx, -36(%ebp)                      # x86gen:117 x86frame:594
L4_if_join:                                       # x86gen:122
	movl -36(%ebp), %eax                      # x86gen:117 x86frame:582
	jmp L7_block_done                         # x86gen:172
L2_if_then:                                       # x86gen:122
	movl -40(%ebp), %ebx                      # x86gen:438 x86frame:320
	movl $1, %ebx                             # x86gen:438 x86frame:325
	movl %ebx, -40(%ebp)                      # x86gen:438 x86frame:330
	movl -40(%ebp), %ebx                      # x86gen:117 x86frame:589
	movl %ebx, -36(%ebp)                      # x86gen:117 x86frame:594
	jmp L4_if_join                            # x86gen:172
L7_block_done:                                    # x86gen:122
	# FP, SP, RV, calleesaves still live
	leave
	ret
	.size	L1_nfactor, .-L1_nfactor
	.endfunc
# END L1_nfactor


	.data
L0_string:
	.long 13
	.asciz "DefaultString"
