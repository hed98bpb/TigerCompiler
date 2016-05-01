
	.text
# PROCEDURE tigermain
	.globl	tigermain
	.func	tigermain
	.type	tigermain, @function
tigermain:
	# FRAME tigermain(1 formals, 12 locals)
	pushl %ebp
	movl %esp, %ebp
	subl $48, %esp
	# SP, FP, calleesaves, argregs have values
L7_blocks:                                        # x86gen:131
	movl -12(%ebp), %ebx                      # x86gen:456 x86frame:323
	movl $0, %ebx                             # x86gen:456 x86frame:328
	movl %ebx, -12(%ebp)                      # x86gen:456 x86frame:333
	movl -12(%ebp), %ebx                      # x86gen:95 x86frame:279
	movl %ebx, -4(%ebp)                       # x86gen:95 x86frame:284
	movl -16(%ebp), %ebx                      # x86gen:456 x86frame:323
	movl $0, %ebx                             # x86gen:456 x86frame:328
	movl %ebx, -16(%ebp)                      # x86gen:456 x86frame:333
	movl -16(%ebp), %ebx                      # x86gen:127 x86frame:616
	movl %ebx, -24(%ebp)                      # x86gen:127 x86frame:620
	movl -20(%ebp), %ebx                      # x86gen:456 x86frame:323
	movl $100, %ebx                           # x86gen:456 x86frame:328
	movl %ebx, -20(%ebp)                      # x86gen:456 x86frame:333
	movl -20(%ebp), %ebx                      # x86gen:127 x86frame:616
	movl %ebx, -32(%ebp)                      # x86gen:127 x86frame:620
	movl -24(%ebp), %ebx                      # x86gen:174 x86frame:304
	movl -32(%ebp), %ecx                      # x86gen:174 x86frame:309
	cmpl %ebx, %ecx                           # x86gen:174 x86frame:314
	jg L1_done                                # x86gen:179
L8_fallthrough:                                   # x86gen:171
	jmp L5_for_begin                          # x86gen:184
L5_for_begin:                                     # x86gen:131
	movl -24(%ebp), %ebx                      # x86gen:95 x86frame:279
	movl %ebx, -8(%ebp)                       # x86gen:95 x86frame:284
L4_for_next:                                      # x86gen:131
	movl -28(%ebp), %ebx                      # x86gen:239 x86frame:343
	movl -8(%ebp), %ebx                       # x86gen:239 x86frame:348
	movl %ebx, -28(%ebp)                      # x86gen:239 x86frame:353
	movl -28(%ebp), %ebx                      # x86gen:174 x86frame:304
	movl -32(%ebp), %ecx                      # x86gen:174 x86frame:309
	cmpl %ebx, %ecx                           # x86gen:174 x86frame:314
	jle L3_for_body                           # x86gen:179
L9_fallthrough:                                   # x86gen:171
	jmp L1_done                               # x86gen:184
L1_done:                                          # x86gen:131
	jmp L6_block_done                         # x86gen:191
L3_for_body:                                      # x86gen:131
	movl -36(%ebp), %ebx                      # x86gen:239 x86frame:343
	movl -4(%ebp), %ebx                       # x86gen:239 x86frame:348
	movl %ebx, -36(%ebp)                      # x86gen:239 x86frame:353
	movl -36(%ebp), %ebx                      # x86gen:263 x86frame:616
	movl %ebx, -40(%ebp)                      # x86gen:263 x86frame:620
	movl -40(%ebp), %ebx                      # x86gen:267 x86frame:377
	addl $1, %ebx                             # x86gen:267 x86frame:382
	movl %ebx, -40(%ebp)                      # x86gen:267 x86frame:387
	movl -40(%ebp), %ebx                      # x86gen:95 x86frame:279
	movl %ebx, -4(%ebp)                       # x86gen:95 x86frame:284
L2_mtseq:                                         # x86gen:131
	movl -44(%ebp), %ebx                      # x86gen:239 x86frame:343
	movl -8(%ebp), %ebx                       # x86gen:239 x86frame:348
	movl %ebx, -44(%ebp)                      # x86gen:239 x86frame:353
	movl -44(%ebp), %ebx                      # x86gen:263 x86frame:616
	movl %ebx, -48(%ebp)                      # x86gen:263 x86frame:620
	movl -48(%ebp), %ebx                      # x86gen:267 x86frame:377
	addl $1, %ebx                             # x86gen:267 x86frame:382
	movl %ebx, -48(%ebp)                      # x86gen:267 x86frame:387
	movl -48(%ebp), %ebx                      # x86gen:95 x86frame:279
	movl %ebx, -8(%ebp)                       # x86gen:95 x86frame:284
	jmp L4_for_next                           # x86gen:191
L6_block_done:                                    # x86gen:131
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
