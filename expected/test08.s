
	.text
# PROCEDURE tigermain
	.globl	tigermain
	.func	tigermain
	.type	tigermain, @function
tigermain:
	# FRAME tigermain(1 formals, 8 locals)
	pushl %ebp
	movl %esp, %ebp
	subl $32, %esp
	# SP, FP, calleesaves, argregs have values
L7_blocks:                                        # x86gen:131
	movl -4(%ebp), %ebx                       # x86gen:456 x86frame:323
	movl $1, %ebx                             # x86gen:456 x86frame:328
	movl %ebx, -4(%ebp)                       # x86gen:456 x86frame:333
	movl -4(%ebp), %ebx                       # x86gen:127 x86frame:616
	movl %ebx, -20(%ebp)                      # x86gen:127 x86frame:620
	movl -12(%ebp), %ebx                      # x86gen:143 x86frame:323
	movl $10, %ebx                            # x86gen:143 x86frame:328
	movl %ebx, -12(%ebp)                      # x86gen:143 x86frame:333
	movl -8(%ebp), %ebx                       # x86gen:456 x86frame:323
	movl $20, %ebx                            # x86gen:456 x86frame:328
	movl %ebx, -8(%ebp)                       # x86gen:456 x86frame:333
	movl -8(%ebp), %ebx                       # x86gen:148 x86frame:304
	movl -12(%ebp), %ecx                      # x86gen:148 x86frame:309
	cmpl %ebx, %ecx                           # x86gen:148 x86frame:314
	jg L1_unEx_t                              # x86gen:153
L8_fallthrough:                                   # x86gen:156
	jmp L2_unEx_f                             # x86gen:161
L2_unEx_f:                                        # x86gen:131
	movl -16(%ebp), %ebx                      # x86gen:456 x86frame:323
	movl $0, %ebx                             # x86gen:456 x86frame:328
	movl %ebx, -16(%ebp)                      # x86gen:456 x86frame:333
	movl -16(%ebp), %ebx                      # x86gen:127 x86frame:616
	movl %ebx, -20(%ebp)                      # x86gen:127 x86frame:620
L1_unEx_t:                                        # x86gen:131
	movl -20(%ebp), %ebx                      # x86gen:157 x86frame:264
	cmpl $0, %ebx                             # x86gen:157 x86frame:269
	je L4_if_else                             # x86gen:162
L9_fallthrough:                                   # x86gen:171
	jmp L3_if_then                            # x86gen:179
L3_if_then:                                       # x86gen:131
	movl -24(%ebp), %ebx                      # x86gen:456 x86frame:323
	movl $30, %ebx                            # x86gen:456 x86frame:328
	movl %ebx, -24(%ebp)                      # x86gen:456 x86frame:333
	movl -24(%ebp), %ebx                      # x86gen:127 x86frame:616
	movl %ebx, -28(%ebp)                      # x86gen:127 x86frame:620
L5_if_join:                                       # x86gen:131
	movl -28(%ebp), %eax                      # x86gen:127 x86frame:609
	jmp L6_block_done                         # x86gen:191
L4_if_else:                                       # x86gen:131
	movl -32(%ebp), %ebx                      # x86gen:456 x86frame:323
	movl $40, %ebx                            # x86gen:456 x86frame:328
	movl %ebx, -32(%ebp)                      # x86gen:456 x86frame:333
	movl -32(%ebp), %ebx                      # x86gen:127 x86frame:616
	movl %ebx, -28(%ebp)                      # x86gen:127 x86frame:620
	jmp L5_if_join                            # x86gen:191
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
