sys_write:	equ		1
stdout:		equ		1
long_length:	equ		128

                section         .text

                global          _start
_start:

                sub             rsp, 2 * long_length * 8
                lea             rdi, [rsp + long_length * 8]
                mov             rcx, long_length
                call            read_long
                mov             rdi, rsp
                call            read_long
		mov		rsi, rdi
                lea             rdi, [rsp + long_length * 8]
                call            sub_long_long

                call            write_long

                jmp             exit

; sub two long number
;    rdi -- address of #1 (long number)
;    rsi -- address of #2 (long number)
;    rcx -- length of long numbers in qwords
; result:
;    sub is written to rdi
sub_long_long:
                push            rdi
                push            rsi
                push            rcx

                clc
.loop:
                mov             rax, [rsi]
                lea             rsi, [rsi + 8]
                sbb             [rdi], rax
                lea             rdi, [rdi + 8]
                dec             rcx
                jnz             .loop

                pop             rcx
                pop             rsi
                pop             rdi
                ret

; adds 64-bit number to long number
;    rdi -- address of summand #1 (long number)
;    rax -- summand #2 (64-bit unsigned)
;    rcx -- length of long number in qwords
; result:
;    sum is written to rdi
add_long_short:
                push            rdi
                push            rcx
                push            rdx

                xor             edx,edx
.loop:
                add             [rdi], rax
                adc             rdx, 0
                mov             rax, rdx
                xor             edx, edx
                add             rdi, 8
                dec             rcx
                jnz             .loop

                pop             rdx
                pop             rcx
                pop             rdi
                ret

; multiplies long number by a short
;    rdi -- address of multiplier #1 (long number)
;    rbx -- multiplier #2 (64-bit unsigned)
;    rcx -- length of long number in qwords
; result:
;    product is written to rdi
mul_long_short:
                push            rax
                push            rdi
                push            rcx

                xor             esi, esi
.loop:
                mov             rax, [rdi]
                mul             rbx
                add             rax, rsi
                adc             rdx, 0
                mov             [rdi], rax
                add             rdi, 8
                mov             rsi, rdx
                dec             rcx
                jnz             .loop

                pop             rcx
                pop             rdi
                pop             rax
                ret

; divides long number by a short
;    rdi -- address of dividend (long number)
;    rbx -- divisor (64-bit unsigned)
;    rcx -- length of long number in qwords
; result:
;    quotient is written to rdi
;    rdx -- remainder
div_long_short:
                push            rdi
                push            rax
                push            rcx

                lea             rdi, [rdi + 8 * rcx - 8]
                xor             edx, edx

.loop:
                mov             rax, [rdi]
                div             rbx
                mov             [rdi], rax
                sub             rdi, 8
                dec             rcx
                jnz             .loop

                pop             rcx
                pop             rax
                pop             rdi
                ret

; assigns a zero to long number
;    rdi -- argument (long number)
;    rcx -- length of long number in qwords
set_zero:
                push            rax
                push            rdi
                push            rcx

                xor             eax, eax
                rep stosq

                pop             rcx
                pop             rdi
                pop             rax
                ret

; checks if a long number is a zero
;    rdi -- argument (long number)
;    rcx -- length of long number in qwords
; result:
;    ZF=1 if zero
is_zero:
                push            rax
                push            rdi
                push            rcx

                xor             eax, eax
                rep scasq

                pop             rcx
                pop             rdi
                pop             rax
                ret

; read long number from stdin
;    rdi -- location for output (long number)
;    rcx -- length of long number in qwords
read_long:
                push            rcx
                push            rdi

                call            set_zero
.loop:
                call            read_char
                or              rax, rax
                js              exit
                cmp             rax, 0x0a
                je              .done
                cmp             rax, '0'
                jb              .invalid_char
                cmp             rax, '9'
                ja              .invalid_char

                sub             rax, '0'
                mov             rbx, 10
                call            mul_long_short
                call            add_long_short
                jmp             .loop

.done:
                pop             rdi
                pop             rcx
                ret

.invalid_char:
		mov		[invalid_char_msg + invalid_char_msg_size - 3], al	; put char in error message
                mov             rsi, invalid_char_msg
                mov             rdx, invalid_char_msg_size
                call            print_string

.skip_loop:
                call            read_char
                or              rax, rax
                js              exit
                cmp             rax, 0x0a
                je              exit
                jmp             .skip_loop

; write long number to stdout
;    rdi -- argument (long number)
;    rcx -- length of long number in qwords
write_long:
                push            rax
                push            rcx

                mov             rax, 21			; add byte for '\n'
                mul             rcx
                mov             rbp, rsp
                sub             rsp, rax

                mov             rsi, rbp
		dec		rsi
		mov		byte [rsi], 0x0a	; put '\n'

.loop:
                mov             rbx, 10
                call            div_long_short
                add             rdx, '0'
                dec             rsi
                mov             [rsi], dl
                call            is_zero
                jnz             .loop

                mov             rdx, rbp
                sub             rdx, rsi
                call            print_string

                mov             rsp, rbp
                pop             rcx
                pop             rax
                ret

; read one char from stdin
; result:
;    rax == -1 if error occurs
;    rax \in [0; 255] if OK
read_char:
                push            rcx
                push            rdi

		mov		rcx, qword [buffer_position]
		cmp		rcx, qword [buffer_readed]
		jb		.read					; buffer isn't ended

		xor		eax, eax				; sys read
		xor		edi, edi				; stdin
		mov		rsi, buffer
		mov		rdx, buffer_size
		syscall

		mov		qword [buffer_readed], rax		; count of readed chars
		xor		ecx, ecx				; start of buffer
		test		rax, rax				; check reading error
		jbe		.error
.read:
		xor		eax, eax
		mov		al, byte [buffer + rcx]
		inc		rcx
		mov		qword [buffer_position], rcx

                pop             rdi
                pop             rcx
                ret
.error:
                mov             rax, -1
                pop             rdi
                pop             rcx
                ret

; print string to stdout
;    rsi -- string
;    rdx -- size
print_string:
                push            rax

                mov             rax, sys_write
                mov             rdi, stdout
                syscall

                pop             rax
                ret

exit:
		mov		rax, 60			; sys exit
		xor		edi, edi
		syscall


		section		.bss
; global buffer of input
buffer:		resb		4096
buffer_size:	equ		$ - buffer
buffer_position:
		resb		8		; qword
buffer_readed:	resb		8		; qword


                section         .data
invalid_char_msg:
                db              "Invalid character: '", 0, "'", 0x0a		; 0 - place for char
invalid_char_msg_size: equ             $ - invalid_char_msg
