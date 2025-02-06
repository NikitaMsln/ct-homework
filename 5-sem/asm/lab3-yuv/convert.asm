global RGB2YUV
global YUV2RGB

section .rdata
R2Y_CONST: times 16 dw (298839 * 256 * 256 / 1000000)
G2Y_CONST: times 16 dw (586811 * 256 * 128 / 1000000)
B2Y_CONST: times 16 dw (114350 * 256 * 256 / 1000000)
B2U_CONST: times 16 dw (256 * 64 * 1000000 / (1000000 - 114350))
R2V_CONST: times 16 dw (256 * 64 * 1000000 / (1000000 - 298839))
HALFBYTE_CONST: times 16 dw 128
V2R_CONST: times 16 dw (256 * 128 * (1000000 - 298839) / 1000000)
U2B_CONST: times 16 dw (256 * 128 * (1000000 - 114350) / 1000000)
U2G_CONST: times 16 dw (256 * 64 * 2 * 114350 * (1000000 - 114350) / (586811 * 1000000))
V2G_CONST: times 16 dw (256 * 64 * 2 * 298839 * (1000000 - 298839) / (586811 * 1000000))
ROUND6: times 16 dw (1 << 5)
ROUND5: times 16 dw (1 << 4)


FST_R_EXTRACT_MASK: db 0, 3, 6, 9, 12, 15, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128
FST_G_EXTRACT_MASK: db 1, 4, 7, 10, 13, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128
FST_B_EXTRACT_MASK: db 2, 5, 8, 11, 14, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128
SND_R_EXTRACT_MASK: db 128, 128, 128, 128, 128, 128, 2, 5, 8, 11, 14, 128, 128, 128, 128, 128
SND_G_EXTRACT_MASK: db 128, 128, 128, 128, 128, 0, 3, 6, 9, 12, 15, 128, 128, 128, 128, 128
SND_B_EXTRACT_MASK: db 128, 128, 128, 128, 128, 1, 4, 7, 10, 13, 128, 128, 128, 128, 128, 128
TRD_R_EXTRACT_MASK: db 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 1, 4, 7, 10, 13
TRD_G_EXTRACT_MASK: db 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 2, 5, 8, 11, 14
TRD_B_EXTRACT_MASK: db 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 0, 3, 6, 9, 12, 15

FST_R_INSERT_MASK: db 0, 128, 128, 1, 128, 128, 2, 128, 128, 3, 128, 128, 4, 128, 128, 5
FST_G_INSERT_MASK: db 128, 0, 128, 128, 1, 128, 128, 2, 128, 128, 3, 128, 128, 4, 128, 128
FST_B_INSERT_MASK: db 128, 128, 0, 128, 128, 1, 128, 128, 2, 128, 128, 3, 128, 128, 4, 128
SND_R_INSERT_MASK: db 128, 128, 6, 128, 128, 7, 128, 128, 8, 128, 128, 9, 128, 128, 10, 128
SND_G_INSERT_MASK: db 5, 128, 128, 6, 128, 128, 7, 128, 128, 8, 128, 128, 9, 128, 128, 10
SND_B_INSERT_MASK: db 128, 5, 128, 128, 6, 128, 128, 7, 128, 128, 8, 128, 128, 9, 128, 128
TRD_R_INSERT_MASK: db 128, 11, 128, 128, 12, 128, 128, 13, 128, 128, 14, 128, 128, 15, 128, 128
TRD_G_INSERT_MASK: db 128, 128, 11, 128, 128, 12, 128, 128, 13, 128, 128, 14, 128, 128, 15, 128
TRD_B_INSERT_MASK: db 10, 128, 128, 11, 128, 128, 12, 128, 128, 13, 128, 128, 14, 128, 128, 15

section .text
; void YUV2RGB(const uint8_t *in, uint8_t *restrict out, size_t width, size_t height, ptrdiff_t in_stride, ptrdiff_t out_stride);
YUV2RGB:
    push rsi
    push rdi
    push rbx
    push r10
    push r11
    push r12
    mov r11, rcx        ; in
    mov r12, rdx        ; out
    mov rcx, r8         ; width
    mov rdx, r9         ; height
    mov r8, [rsp + 88]  ; in_stride
    mov r9, [rsp + 96]  ; out_stride
    sub rsp, 32 * 3

    test rdx, rdx
    jz .end_func
    test rcx, rcx
    jz .end_func                ; if width == 0 || height == 0 do nothing

    vpxor ymm15, ymm15                      ; set zero
    vmovdqu ymm14, [rel HALFBYTE_CONST]
    vmovdqu ymm13, [rel V2R_CONST]
    vmovdqu ymm12, [rel U2B_CONST]
    vmovdqu ymm11, [rel U2G_CONST]
    vmovdqu ymm10, [rel V2G_CONST]
    vmovdqu xmm9, [rel FST_R_INSERT_MASK]
    vmovdqu xmm8, [rel FST_G_INSERT_MASK]
    vmovdqu xmm7, [rel FST_B_INSERT_MASK]
    vmovdqu xmm6, [rel SND_R_INSERT_MASK]
    vmovdqu ymm5, [rel ROUND6]

.lines_loop:
    mov r10, rcx
    mov rsi, r11
    mov rdi, r12

    mov rax, r10
    shr rax, 4
    jz .last_line

.packed_eval_loop:
    vmovdqu ymm0, [rsi]         ; Y U V X Y U V X ...
    vmovdqu ymm1, [rsi + 32]    ; Y U V X Y U V X ...

    vextracti128 xmm3, ymm0, 1
    vextracti128 xmm4, ymm1, 0
    vinserti128 ymm1, ymm1, xmm3, 0
    vinserti128 ymm0, ymm0, xmm4, 1     ; ymm0 = 1 2, ymm1 = 3 4 -> ymm0 = 1 3, ymm1 = 2 4

    vmovdqu ymm2, ymm0
    vmovdqu ymm3, ymm1

    vpsrlw ymm0, 8                      ; ymm0 = U 0 X 0 U 0 X 0 ...
    vpsrlw ymm1, 8                      ; ymm1 = U 0 X 0 U 0 X 0 ...
    vpackuswb ymm1, ymm0, ymm1          ; ymm1 = U X U X U X U X ...
    vpsllw ymm1, 8
    vpsrlw ymm1, 8                      ; ymm1 = U 0 U 0 U 0 U 0 ...

    vpsllw ymm2, 8
    vpsrlw ymm2, 8                      ; ymm2 = Y 0 V 0 Y 0 V 0 ...
    vpsllw ymm3, 8
    vpsrlw ymm3, 8                      ; ymm3 = Y 0 V 0 Y 0 V 0 ...
    vpackuswb ymm0, ymm2, ymm3          ; ymm0 = Y V Y V Y V Y V ...

    vmovdqu ymm2, ymm0
    vpsrlw ymm2, 8                      ; ymm2 = V 0 V 0 V 0 V 0 ...
    vpsllw ymm0, 8
    vpsrlw ymm0, 8                      ; ymm0 = Y 0 Y 0 Y 0 Y 0 ...

    add rsi, 4 * 16                     ; end read

    vpsubw ymm1, ymm14
    vpsubw ymm2, ymm14

    vpsllw ymm0, 6
    vpsllw ymm1, 8
    vpsllw ymm2, 8

    vmovdqu ymm3, ymm1
    vmovdqu ymm4, ymm2

    vpmulhw ymm2, ymm13
    vpaddw ymm2, ymm0
    vpaddw ymm2, ymm5
    vpsraw ymm2, 6                  ; ymm2 = R 0 R 0 R 0 R 0 ...

    vpmulhw ymm1, ymm12
    vpaddw ymm1, ymm0
    vpaddw ymm1, ymm5
    vpsraw ymm1, 6                  ; ymm1 = B 0 B 0 B 0 B 0 ...

    vpmulhw ymm3, ymm11
    vpmulhw ymm4, ymm10
    vpsubw ymm0, ymm3
    vpsubw ymm0, ymm4
    vpaddw ymm0, ymm5
    vpsraw ymm0, 6                  ; ymm0 = G 0 G 0 G 0 G 0 ...

    vextracti128 xmm3, ymm2, 1
    vpackuswb xmm2, xmm2, xmm3      ; xmm2 = R R R R R R R R ...

    vextracti128 xmm3, ymm0, 1
    vpackuswb xmm0, xmm0, xmm3      ; xmm0 = G G G G G G G G ...

    vextracti128 xmm3, ymm1, 1
    vpackuswb xmm1, xmm1, xmm3      ; xmm0 = B B B B B B B B ...

    vpshufb xmm3, xmm2, xmm9        ; xmm3 = R 0 0 R 0 0 R 0 ...
    vpshufb xmm4, xmm0, xmm8        ; xmm4 = 0 G 0 0 G 0 0 G ...
    vpaddb xmm3, xmm4               ; xmm3 = R G 0 R G 0 R G ...
    vpshufb xmm4, xmm1, xmm7        ; xmm4 = 0 0 B 0 0 B 0 0
    vpaddb xmm3, xmm4               ; xmm3 = R G B R G B R G ...
    vmovdqu [rdi], xmm3

    vpshufb xmm3, xmm2, xmm6
    vpshufb xmm4, xmm0, [rel SND_G_INSERT_MASK]
    vpaddb xmm3, xmm4
    vpshufb xmm4, xmm1, [rel SND_B_INSERT_MASK]
    vpaddb xmm3, xmm4
    vmovdqu [rdi + 16], xmm3

    vpshufb xmm3, xmm2, [rel TRD_R_INSERT_MASK]
    vpshufb xmm4, xmm0, [rel TRD_G_INSERT_MASK]
    vpaddb xmm3, xmm4
    vpshufb xmm4, xmm1, [rel TRD_B_INSERT_MASK]
    vpaddb xmm3, xmm4
    vmovdqu [rdi + 32], xmm3

    add rdi, 3 * 16

    dec rax
    jnz .packed_eval_loop

.last_line:
    mov rax, r10
    and rax, 15
    jz .end_write
    dec rax
    mov r10, rax

    xor ebx, ebx
.read_yuv:
    mov bl, [rsi]
    mov [rsp + rax * 2], bx
    mov bl, [rsi + 1]
    mov [rsp + rax * 2 + 32 * 1], bx
    mov bl, [rsi + 2]
    mov [rsp + rax * 2 + 32 * 2], bx
    add rsi, 4
    dec al
    jnl .read_yuv

    vmovdqu ymm0, [rsp]
    vmovdqu ymm1, [rsp + 32 * 1]
    vmovdqu ymm2, [rsp + 32 * 2]

    vpsubw ymm1, ymm14
    vpsubw ymm2, ymm14

    vpsllw ymm0, 6
    vpsllw ymm1, 8
    vpsllw ymm2, 8

    vmovdqu ymm3, ymm1
    vmovdqu ymm4, ymm2

    vpmulhw ymm2, ymm13
    vpaddw ymm2, ymm0
    vpaddw ymm2, ymm5
    vpsraw ymm2, 6                  ; ymm2 = R 0 R 0 R 0 R 0 ...

    vpmulhw ymm1, ymm12
    vpaddw ymm1, ymm0
    vpaddw ymm1, ymm5
    vpsraw ymm1, 6                  ; ymm1 = B 0 B 0 B 0 B 0 ...

    vpmulhw ymm3, ymm11
    vpmulhw ymm4, ymm10
    vpsubw ymm0, ymm3
    vpsubw ymm0, ymm4
    vpaddw ymm0, ymm5
    vpsraw ymm0, 6                  ; ymm0 = G 0 G 0 G 0 G 0 ...

    vmovdqu [rsp], ymm2
    vmovdqu [rsp + 32 * 1], ymm0
    vmovdqu [rsp + 32 * 2], ymm1

    mov eax, 16 - 1
    
.write_rgb:
    mov bx, [rsp + r10 * 2]
    mov [rdi], bl
    mov bx, [rsp + r10 * 2 + 32 * 1]
    mov [rdi + 1], bl
    mov bx, [rsp + r10 * 2 + 32 * 2]
    mov [rdi + 2], bl
    add rdi, 3
    dec r10
    jnl .write_rgb

.end_write:

    add r11, r8         ; in += in_stride
    add r12, r9         ; out += out_stride

    dec rdx             ; height--
    jnz .lines_loop

.end_func:
    vzeroupper
    add rsp, 32 * 3
    pop r12
    pop r11
    pop r10
    pop rbx
    pop rdi
    pop rsi
    ret

; void RGB2YUV(const uint8_t *in, uint8_t *restrict out, size_t width, size_t height, ptrdiff_t in_stride, ptrdiff_t out_stride);
RGB2YUV:
    push rsi
    push rdi
    push rbx
    push r10
    push r11
    push r12
    mov r11, rcx        ; in
    mov r12, rdx        ; out
    mov rcx, r8         ; width
    mov rdx, r9         ; height
    mov r8, [rsp + 88]  ; in_stride
    mov r9, [rsp + 96]  ; out_stride
    sub rsp, 32 * 3

    test rdx, rdx
    jz .end_func
    test rcx, rcx
    jz .end_func                        ; if width == 0 || height == 0 do nothing

    vmovdqu ymm15, [rel HALFBYTE_CONST]
    vpxor ymm14, ymm14
    vmovdqu ymm13, [rel R2Y_CONST]
    vmovdqu ymm12, [rel G2Y_CONST]
    vmovdqu ymm11, [rel B2Y_CONST]
    vmovdqu ymm10, [rel B2U_CONST]
    vmovdqu ymm9, [rel R2V_CONST]
    vmovdqu xmm8, [rel FST_R_EXTRACT_MASK]
    vmovdqu xmm7, [rel FST_G_EXTRACT_MASK]
    vmovdqu ymm6, [rel ROUND5]
    vmovdqu ymm5, [rel ROUND6]

.lines_loop:
    mov r10, rcx
    mov rsi, r11
    mov rdi, r12

    mov rax, r10
    shr rax, 4
    jz .last_line

.packed_eval_loop:
    vmovdqu xmm3, [rsi]                             ; xmm3 = R G B R G B R G ...
    vpshufb xmm0, xmm3, xmm8                        ; xmm0 = R R R R R R 0 0 ...
    vpshufb xmm1, xmm3, xmm7                        ; xmm1 = G G G G G G 0 0 ...
    vpshufb xmm2, xmm3, [rel FST_B_EXTRACT_MASK]    ; xmm2 = B B B B B B 0 0 ...

    vmovdqu xmm3, [rsi + 16]
    vpshufb xmm4, xmm3, [rel SND_R_EXTRACT_MASK]
    vpaddb xmm0, xmm4
    vpshufb xmm4, xmm3, [rel SND_G_EXTRACT_MASK]
    vpaddb xmm1, xmm4
    vpshufb xmm4, xmm3, [rel SND_B_EXTRACT_MASK]
    vpaddb xmm2, xmm4

    vmovdqu xmm3, [rsi + 32]
    vpshufb xmm4, xmm3, [rel TRD_R_EXTRACT_MASK]
    vpaddb xmm0, xmm4
    vpshufb xmm4, xmm3, [rel TRD_G_EXTRACT_MASK]
    vpaddb xmm1, xmm4
    vpshufb xmm4, xmm3, [rel TRD_B_EXTRACT_MASK]
    vpaddb xmm2, xmm4

    vpmovzxbw ymm0, xmm0                            ; ymm0 = R 0 R 0 R 0 R 0 ...
    vpmovzxbw ymm1, xmm1                            ; ymm1 = G 0 G 0 G 0 G 0 ...
    vpmovzxbw ymm2, xmm2                            ; ymm2 = B 0 B 0 B 0 B 0 ...

    add rsi, 16 * 3

    vpsllw ymm0, 6
    vpsllw ymm1, 7
    vpsllw ymm2, 6
    vmovdqu ymm3, ymm2
    vmovdqu ymm4, ymm0

    vpmulhuw ymm0, ymm13
    vpmulhuw ymm1, ymm12
    vpmulhuw ymm2, ymm11

    vpaddw ymm0, ymm1
    vpaddw ymm0, ymm2

    vpsubw ymm3, ymm0
    vpmulhw ymm3, ymm10
    vpaddw ymm3, ymm6
    vpsraw ymm3, 5
    vpaddw ymm3, ymm15                              ; ymm3 = U * U * U * U * ...

    vpsubw ymm4, ymm0
    vpmulhw ymm4, ymm9
    vpaddw ymm4, ymm6
    vpsraw ymm4, 5
    vpaddw ymm4, ymm15                              ; ymm4 = V * V * V * V * ...

    vpaddw ymm0, ymm5
    vpsraw ymm0, 6                                  ; ymm0 = Y * Y * Y * Y * ...

    vextracti128 xmm1, ymm0, 1
    vpackuswb xmm0, xmm0, xmm1                      ; xmm0 = Y Y Y Y Y Y Y Y ...

    vextracti128 xmm1, ymm3, 1
    vpackuswb xmm3, xmm3, xmm1                      ; xmm3 = U U U U U U U U ...

    vextracti128 xmm1, ymm4, 1
    vpackuswb xmm4, xmm4, xmm1                      ; xmm4 = V V V V V V V V ...

    vpunpckhbw xmm1, xmm0, xmm3
    vpunpcklbw xmm0, xmm0, xmm3
    vinserti128 ymm0, ymm0, xmm1, 1                 ; ymm0 = Y U Y U Y U Y U ...
    vpunpckhbw xmm1, xmm4, xmm14
    vpunpcklbw ymm4, ymm4, ymm14
    vinserti128 ymm4, ymm4, xmm1, 1                 ; ymm4 = V 0 V 0 V 0 V 0 ...

    vpunpcklwd ymm1, ymm0, ymm4                     ; ymm1 = Y U V 0 Y U V 0 ...
    vpunpckhwd ymm2, ymm0, ymm4                     ; ymm2 = Y U V 0 Y U V 0 ...

    vextracti128 xmm3, ymm1, 1
    vextracti128 xmm4, ymm2, 0
    vinserti128 ymm2, ymm2, xmm3, 0
    vinserti128 ymm1, ymm1, xmm4, 1                 ; ymm1 = 1 3, ymm2 = 2 4 -> ymm1 = 1 2, ymm2 = 3 4

    vmovdqu [rdi], ymm1
    vmovdqu [rdi + 32], ymm2

    add rdi, 16 * 4

    dec rax
    jnz .packed_eval_loop

.last_line:
    mov rax, r10
    and rax, 15
    jz .end_write
    dec rax
    mov r10, rax

    xor ebx, ebx
.read_rgb:
    mov bl, [rsi]
    mov [rsp + rax * 2], bx
    mov bl, [rsi + 1]
    mov [rsp + rax * 2 + 32], bx
    mov bl, [rsi + 2]
    mov [rsp + rax * 2 + 64], bx
    add rsi, 3
    dec al
    jnl .read_rgb

    vmovdqu ymm0, [rsp]
    vmovdqu ymm1, [rsp + 32]
    vmovdqu ymm2, [rsp + 64]

    vpsllw ymm0, 6
    vpsllw ymm1, 7
    vpsllw ymm2, 6
    vmovdqu ymm3, ymm2
    vmovdqu ymm4, ymm0

    vpmulhuw ymm0, ymm13
    vpmulhuw ymm1, ymm12
    vpmulhuw ymm2, ymm11

    vpaddw ymm0, ymm1
    vpaddw ymm0, ymm2

    vpsubw ymm3, ymm0
    vpmulhw ymm3, ymm10
    vpaddw ymm3, ymm6
    vpsraw ymm3, 5
    vpaddw ymm3, ymm15                              ; ymm3 = U * U * U * U * ...

    vpsubw ymm4, ymm0
    vpmulhw ymm4, ymm9
    vpaddw ymm4, ymm6
    vpsraw ymm4, 5
    vpaddw ymm4, ymm15                              ; ymm4 = V * V * V * V * ...

    vpaddw ymm0, ymm5
    vpsraw ymm0, 6                                  ; ymm0 = Y * Y * Y * Y * ...
    
    vmovdqu [rsp], ymm0
    vmovdqu [rsp + 32], ymm3
    vmovdqu [rsp + 64], ymm4

.write_yuv:
    mov bx, [rsp + r10 * 2]
    mov [rdi], bl
    mov bx, [rsp + r10 * 2 + 32]
    mov [rdi + 1], bl
    mov bx, [rsp + r10 * 2 + 64]
    mov [rdi + 2], bl
    mov [rdi + 3], byte 0
    add rdi, 4
    dec r10
    jnl .write_yuv
.end_write:

    add r11, r8     ; in += in_stride
    add r12, r9     ; out += out_stride

    dec rdx         ; height--
    jnz .lines_loop

.end_func:
    vzeroupper
    add rsp, 32 * 3
    pop r12
    pop r11
    pop r10
    pop rbx
    pop rdi
    pop rsi
    ret
