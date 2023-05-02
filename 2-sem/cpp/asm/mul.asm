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
                lea             rsi, [rsp + long_length * 8]
		
		sub		rsp, 2 * long_length * 8		; result
		mov		r9, rsp
		call		mul_long_long

		mov		rdi, r9
                call            write_long

                jmp             exit

; mul 2 long number:
;	rdi -- address of 1
;	rsi -- address of 2
;	rcx -- length of arguments
;	r9 -- address of result
; result:
;	number in [r9]
;	rcx' = 2 * rcx
mul_long_long:
		push		rsi
		push		r9
		push		rbx
		push		rdi

		mov		rdi, r9				; swap r9 and rdi for set_zero
		mov		r9, [rsp]

		mov		rbx, rcx			; old length
		mov		r12, rcx			; counter
		shl		rcx, 1				; new length

		mov		r13, rdi			; shifted result

		call		set_zero			; result = 0
.loop:
		mov		r11, [rsi]			; short
		call		mul_long_short_add

		mov		r11, rdx
		xor		edx, edx
		add		[r13 + rbx * 8], r11		; add overflow

		adc		rdx, 0

		lea		r13, [r13 + 8]			; next part
		lea		rsi, [rsi + 8]
		
		dec		r12
		jnz		.loop

		pop		rdi
		pop		rbx
		pop		r9
		pop		rsi

		ret

; multiplies long number by a short
;    r9 -- address of multiplier #1 (long number)
;    r11 -- multiplier #2 (64-bit unsigned)
;    rbx -- length of long number in qwords
;    r13 -- address of long number where add result
; result:
;    result add to r13
;    overflow in rdx
mul_long_short_add:
		push		rbx
		push		r9
		push		r13
		push		rax
		push		rsi

		xor		esi, esi
.loop
		mov		rax, [r9]
		mul		r11
		add		rax, rsi
		adc		rdx, 0
		add		[r13], rax
		adc		rdx, 0
		add		r9, 8
		add		r13, 8
		mov		rsi, rdx
		dec		rbx
		jnz		.loop

		pop		rsi
		pop		rax
		pop		r13
		pop		r9
		pop		rbx

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

                xor             rdx,rdx
.loop:
                add             [rdi], rax
                adc             rdx, 0
                mov             rax, rdx
                xor             rdx, rdx
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
mul_long_short_replace:
                push            rax
                push            rdi
                push            rcx

                xor             esi, esi
.loop:
                mov             rax, [rdi]
                mul             rbx
                add             rax, rsi
                adc             rdx, 0
		mov		[rdi], rax
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

                call            mul_long_short_replace
                call            add_long_short
                jmp             .loop

.done:
                pop             rdi
                pop             rcx
                ret

.invalid_char:

		mov		[invalid_char_msg + invalid_char_msg_size - 3], al	; put char to empty place
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

                mov             rax, 21
                mul             rcx
                mov             rbp, rsp
                sub             rsp, rax

                mov             rsi, rbp
		dec		rsi
		mov		byte [rsi], 0x0a
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

mov		rcx, qword [ibuffer_position]
		cmp		rcx, qword [ibuffer_readed]
		jb		.read

		xor		eax, eax			; sys read
		xor		edi, edi			; stdin
		mov		rsi, ibuffer
		mov		rdx, ibuffer_size
		syscall
		mov		qword [ibuffer_readed], rax
		xor		ecx, ecx			; buffer position = 0
		test		rax, rax			; check reading error
		jbe		.error
.read:
		xor		eax, eax
		mov		al, byte [ibuffer + rcx]
		inc		rcx
		mov		qword [ibuffer_position], rcx

                pop             rdi
                pop             rcx
                ret
.error:
                mov             rax, -1
                add             rsp, 1
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
		mov		rax, 60				; sys exit
		xor		edi, edi
		syscall

		section .bss
; buffer of input
ibuffer:	resb		4096
ibuffer_size:	equ		$ - ibuffer
ibuffer_position:
		resb		8				; qword
ibuffer_readed:
		resb		8				; qword


                section         .data
invalid_char_msg:
                db              "Invalid character: '", 0, "'", 0x0a 
invalid_char_msg_size: equ             $ - invalid_char_msg
