global  print

section .text

print:
    push ebp
    lea ebp, [esp + 4]
    
    push ebx
    push esi
    push edi

    mov ecx, -1
    mov esi, [ebp + 12]
    mov ebx, esi
    cmp byte [esi], '-'
    jne .count_of_digits
    inc esi

.count_of_digits:           ; count of 16b chars
    inc ecx
    cmp byte [esi + ecx], 0
    jne .count_of_digits

    push 0                  ; push (int128)0
    push 0
    push 0
    push 0

    test ecx, ecx
    jz .read_number_exit

    mov edi, esp
.read_number:
    dec ecx

    mov al, [esi + ecx]
    or al, 32               ; to lower case in ascII
    sub al, 'a'
    cmp al, 'f' - 'a'
    ja .less_letter_1
    add al, 10
    jmp .great_letter_1
.less_letter_1:
    add al, 'a' - '0'
.great_letter_1:

    mov [edi], al
    test ecx, ecx
    jz .read_number_exit

    dec ecx

    mov al, [esi + ecx]
    or al, 32               ; to lower case in ascII
    sub al, 'a'
    cmp al, 'f' - 'a'
    ja .less_letter_2
    add al, 10
    jmp .great_letter_2
.less_letter_2:
    add al, 'a' - '0'
.great_letter_2:
    shl al, 4

    add [edi], al
    
    inc edi

    test ecx, ecx
    jnz .read_number

.read_number_exit:

    cmp ebx, esi            ; check begin '-'
    je .not_minus
    xor [esp], dword -1
    xor [esp + 4], dword -1
    xor [esp + 8], dword -1
    xor [esp + 12], dword -1
    add [esp], dword 1
    adc [esp + 4], dword 0
    adc [esp + 8], dword 0
    adc [esp + 12], dword 0

.not_minus:
    push 0
    mov esi, esp            ; save sign and begin of number
    lea edi, [esi + 16]     ; biggest int in number

    test [edi], dword 1 << 31   ; check sign
    jz .not_negative
    mov [esi], dword '-'
    xor [esp + 4], dword -1
    xor [esp + 8], dword -1
    xor [esp + 12], dword -1
    xor [esp + 16], dword -1
    add [esp + 4], dword 1
    adc [esp + 8], dword 0
    adc [esp + 12], dword 0
    adc [esp + 16], dword 0

.not_negative:

    add edi, 4
    mov ebx, 10
    mov ecx, 5
.remove_zeros:              ; remove oldest 0
    sub edi, 4
    dec ecx
    cmp ecx, 1
    je .remove_zeros_exit
    mov eax, [edi]
    test eax, eax
    jz .remove_zeros
.remove_zeros_exit:

.to_dec:
    push edi
    push ecx                ; save ecx, edi
    xor edx, edx            ; big bits in division
.division_10:
    mov eax, [edi]
    div ebx
    mov [edi], eax
    sub edi, 4
    dec ecx
    jnz .division_10

    pop ecx
    pop edi

    add dl, '0'
    push edx

    mov eax, [edi]
    test eax, eax
    jnz .to_dec
    sub edi, 4
    dec ecx
    jnz .to_dec

    xor eax, eax
    xor ecx, ecx
    mov dl, ' '
    mov edi, [ebp + 8]
.read_format_flags:
    xor ebx, ebx
    mov bl, [edi]
    sub bl, '1'
    cmp bl, '9' - '1'
    jbe .read_format_size
    inc edi
    add bl, '1'
    test bl, bl
    jz .read_format_end
    cmp bl, 44              ; bl < 44 => bl = ' ' or '+', bl > 44 => bl = '0' or '-'
    ja .zero_minus
.space_plus:
    or cl, bl               ; ' ' or '+' = '+'
    jmp .read_format_flags
.zero_minus:
    or bl, 10000b
    or dl, bl
    jmp .read_format_flags

.read_format_size:
    push ecx
    push edx
    mov ecx, 10
    inc ebx
.read_format_size_loop:
    mul ecx
    add eax, ebx
    inc edi
    mov bl, [edi]
    sub bl, '0'
    cmp bl, '9' - '0'
    jbe .read_format_size_loop
    
    pop edx
    pop ecx

.read_format_end:           ; eax - number, cl - first char ('+'/' ' or 0), dl - 61 = '-' or '0'/' '

    mov edi, [ebp + 4]

    cmp [esi], byte 0
    je .nonegative
    mov cl, [esi]
.nonegative:

    cmp dl, '0'
    je .zero_ws
    test cl, cl
    jz .not_sign
    push ecx
.not_sign:
    mov ebx, esi
    sub ebx, esp
    shr ebx, 2
    sub eax, ebx
    jae .number_greater
    xor eax, eax
.number_greater:
    cmp dl, 61
    je .write_num
    test eax, eax
    jz .end_first_ws
.first_ws:
    mov [edi], byte ' '
    inc edi
    dec eax
    jnz .first_ws
.end_first_ws:
    jmp .write_num

.zero_ws:
    mov ebx, esi
    sub ebx, esp
    shr ebx, 2
    test cl, cl
    jz .not_sign1
    mov [edi], cl
    inc edi
    test eax, eax
    jz .number_greater1
    dec eax
.not_sign1:
    sub eax, ebx
    jbe .number_greater1

.write_zero:
    mov [edi], byte '0'
    inc edi
    dec eax
    jnz .write_zero
.number_greater1:
    xor eax, eax

.write_num:
    pop ecx
    mov [edi], cl
    inc edi
    cmp esi, esp
    jne .write_num

    test eax, eax
    jz .not_last_ws
.last_ws:
    mov [edi], byte ' '
    inc edi
    dec eax
    jnz .last_ws
.not_last_ws:
    mov [edi], byte 0

    add esp, 20
    pop edi
    pop esi
    pop ebx
    pop ebp
    ret
