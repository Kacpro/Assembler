data segment
        arguments db 128 dup('$')  ;A001
        number_of_args db 0
		crc_dont_match db "CRC codes don't match $"
		crc_match db "CRC codes are the same $"
		handler1 dw ?
		handler2 dw ?
		result db 4 dup('?')
		result2 db 4 dup('?') 
		buffer db 256 dup(0),'$'
		file_error db "File error$"
		too_few_args db "Too few arguments$"
		too_many_args db "Too many arguments$"
		sec_arg_err db "Second argument has a wrong value$"
		sec_arg_len db "Second argument has a wrong lenght$"
		
		
		code_table  dw 00000h, 0C0C1h, 0C181h, 00140h, 0C301h, 003C0h, 00280h, 0C241h
							dw 0C601h, 006C0h, 00780h, 0C741h, 00500h, 0C5C1h, 0C481h, 00440h
							dw 0CC01h, 00CC0h, 00D80h, 0CD41h, 00F00h, 0CFC1h, 0CE81h, 00E40h
							dw 00A00h, 0CAC1h, 0CB81h, 00B40h, 0C901h, 009C0h, 00880h, 0C841h
							dw 0D801h, 018C0h, 01980h, 0D941h, 01B00h, 0DBC1h, 0DA81h, 01A40h
							dw 01E00h, 0DEC1h, 0DF81h, 01F40h, 0DD01h, 01DC0h, 01C80h, 0DC41h
							dw 01400h, 0D4C1h, 0D581h, 01540h, 0D701h, 017C0h, 01680h, 0D641h
							dw 0D201h, 012C0h, 01380h, 0D341h, 01100h, 0D1C1h, 0D081h, 01040h
							dw 0F001h, 030C0h, 03180h, 0F141h, 03300h, 0F3C1h, 0F281h, 03240h
							dw 03600h, 0F6C1h, 0F781h, 03740h, 0F501h, 035C0h, 03480h, 0F441h
							dw 03C00h, 0FCC1h, 0FD81h, 03D40h, 0FF01h, 03FC0h, 03E80h, 0FE41h
							dw 0FA01h, 03AC0h, 03B80h, 0FB41h, 03900h, 0F9C1h, 0F881h, 03840h
							dw 02800h, 0E8C1h, 0E981h, 02940h, 0EB01h, 02BC0h, 02A80h, 0EA41h
							dw 0EE01h, 02EC0h, 02F80h, 0EF41h, 02D00h, 0EDC1h, 0EC81h, 02C40h
							dw 0E401h, 024C0h, 02580h, 0E541h, 02700h, 0E7C1h, 0E681h, 02640h
							dw 02200h, 0E2C1h, 0E381h, 02340h, 0E101h, 021C0h, 02080h, 0E041h
							dw 0A001h, 060C0h, 06180h, 0A141h, 06300h, 0A3C1h, 0A281h, 06240h
							dw 06600h, 0A6C1h, 0A781h, 06740h, 0A501h, 065C0h, 06480h, 0A441h
							dw 06C00h, 0ACC1h, 0AD81h, 06D40h, 0AF01h, 06FC0h, 06E80h, 0AE41h
							dw 0AA01h, 06AC0h, 06B80h, 0AB41h, 06900h, 0A9C1h, 0A881h, 06840h
							dw 07800h, 0B8C1h, 0B981h, 07940h, 0BB01h, 07BC0h, 07A80h, 0BA41h
							dw 0BE01h, 07EC0h, 07F80h, 0BF41h, 07D00h, 0BDC1h, 0BC81h, 07C40h
							dw 0B401h, 074C0h, 07580h, 0B541h, 07700h, 0B7C1h, 0B681h, 07640h
							dw 07200h, 0B2C1h, 0B381h, 07340h, 0B101h, 071C0h, 07080h, 0B041h
							dw 05000h, 090C1h, 09181h, 05140h, 09301h, 053C0h, 05280h, 09241h
							dw 09601h, 056C0h, 05780h, 09741h, 05500h, 095C1h, 09481h, 05440h
							dw 09C01h, 05CC0h, 05D80h, 09D41h, 05F00h, 09FC1h, 09E81h, 05E40h
							dw 05A00h, 09AC1h, 09B81h, 05B40h, 09901h, 059C0h, 05880h, 09841h
							dw 08801h, 048C0h, 04980h, 08941h, 04B00h, 08BC1h, 08A81h, 04A40h
							dw 04E00h, 08EC1h, 08F81h, 04F40h, 08D01h, 04DC0h, 04C80h, 08C41h
							dw 04400h, 084C1h, 08581h, 04540h, 08701h, 047C0h, 04680h, 08641h
							dw 08201h, 042C0h, 04380h, 08341h, 04100h, 081C1h, 08081h, 04040h
data ends

code segment
start:
		.286
		mov ax, seg arguments
		mov ds, ax
		mov ax, seg top
		mov ss, ax
		mov sp, offset top
		
		call Parser
	
		call CheckArgs

		call Logic
		
		mov ah, 4ch
		int 21h


Parser:      
	
		pusha																		; Odkładanie na stos rejestrów używanych w funkcji
		
		xor ax, ax
		xor bx, bx
		xor cx, cx																; Zerowanie rejestrów
		xor dx, dx
		xor di, di
		xor si, si
		
		call GetCmdArgs														; Ładuje listę argumentów,  OUT: si - offset początku argumentów, ds - segment argumntów, cl - ilość znaków
		
		mov ax, seg arguments
		mov es, ax																; Wysłanie do es segmentu zmiennej do przechowywania argumentów
		lea di, arguments														; Ustawienie di na początek tej zmiennej
		
		mov bx, di																; bx będzie pamiętał offset początku bufora
		
		cmp cl, 2																	; W cl jest ilość znaków ( wliczając pierwszą spację )
		jb NoArgs																; Jak nie ma argumentów można zakończyć funkcję

		call SignCheckingLoop												; Pętla przechodząca po znakach w liście argumentów i wykonująca odpowiednie działania
																						; IN: si - offset początku argumentów (PSP) , ds - segment PSP, es - segment zmiennej, di - offset początku 	zmiennej, cl - ilość znaków
		NoArgs:
		
		popa																			; Zdejmujemy ze stosu rejestry odłożone na początku funkcji
											
		ret																			; Wracamy do miejsca wywołania		
		
		
GetCmdArgs: 																; OUT: si - offset początku argumentów, ds - segment argumntów, cl - ilość znaków
		push ax
		push bx
		xor bx, bx
		mov ah, 62h				
		int 21h																		; Załadowanie segmentu listy argumentów do bx
		
		mov si, 82h																; Ustawienie si na początek listy  argumentów (PSP)
		mov ds, bx																; Segment z argumentami jest teraz w ds
		mov cl, ds:[80h]														; Wpisanie do cl ilości znaków w liście argumentów
		
		pop bx
		pop ax
		ret

		
SignCheckingLoop:														; IN: si - offset początku argumentów, ds - segment argumentów, es - segment zmiennej do przechowywania, di - offset początku zmiennej, cl - ilość znaków
	
		push ax
		push dx
		
		xor dx, dx																; Jak dx = 1 i napotkamy biały znak to będzie znaczyć że jest to pierwszy znak po agrumencie i można zrobić odstęp w liście argumentów
		mov ax, seg arguments
		mov es, ax
		Check:									
			mov ah, ds:[si]														; Wrzucenie do ah kodu znaku z PSP
			
			cmp ah, 32
			jz WhiteSign
			cmp ah, 9																; Sprawdzenie czy obecny znak jest biały ( spacja, tab, enter )
			jz WhiteSign
			cmp ah, 13
			jz WhiteSign
			mov dx, 1																; Następny biały znak będzie pierwszym białym po argumencie
			mov es:[di], ah														; Jak znak nie jest biały to wpisujemy go do zmiennej....			
			inc di																	; ...i zwiększamy di aby pokazywał on na kolejne "wolne miejsce"
			
			jmp NextSign														; Przechodzimy do kolejnego znaku ( trzeba jeszcze zwiększyć si )
			
			WhiteSign:															; Gdy znak jest biały	
			cmp dx, 0																; Sprawdzamy czy to pierwszy znak po argumencie
			jz NotFirstWhite				
			push si
			mov dx, 0
			mov si, offset number_of_args
			mov ah, es:[si]
			inc ah																		; Jak tak to zwiększamy licznik argumentów i przechodzimy na kolejne miejsce w zmiennej ( jednen $ odstępu )
			mov es:[si], ah					
			pop si
			inc di
			NotFirstWhite:
			NextSign:
			inc si																		; Zwiększamy si aby wskazywał na kolejny znak w linii argumentów
		loop Check																	; Wracamy na początek aby sprawdzać kolejny znak ( jeśli jeszcze jakieś są ( wartość w cl ))
		
		pop dx
		pop ax
		ret
		
CheckArgs:
		mov ax, seg arguments
		mov ds ,ax
		mov si, offset number_of_args								      ; Sprawdzanie ilości argumentów
		mov ah, ds:[si]
		cmp ah, 2
		jnb OK1
		mov dx, offset too_few_args
		call PrintErrorAndExit
		OK1:
		cmp ah, 3
		jna OK2
		mov dx, offset too_many_args
		call PrintErrorAndExit
		OK2:
		cmp ah, 2
		jz TwoArgs                                                                    
		mov ax, 2
		
		call GetArgLength                                                          ; Sprawdzanie czy w przypadku 3 argumentów drugi ma 2 znaki
		cmp dx, 2
		jz OK3
		mov dx, offset sec_arg_len
		call PrintErrorAndExit
		OK3:
		
		
		call GetArgOffset
		mov si, dx
		mov al, ds:[si]																		; Sprawdzanie czy drugi argument wynosi -v
		cmp al, '-'
		jnz WrongArg
		mov al, ds:[si+1]
		cmp al, 'v'
		jnz WrongArg
		jmp Correct
		WrongArg:
		mov dx, offset sec_arg_err
		call PrintErrorAndExit
		Correct:
		mov al , 1                                                                                ; argument nr 1
		mov ah, 1                                                                                ; tryb do odczytu
		call OpenFile
		mov al , 3                                                                                ; argument nr 3
		mov ah, 1                                                                                ; tryb do odczytu
		call OpenFile
		jmp ThreeArgs
		TwoArgs:                                                                                ; Przypadek z 2 argumentami
		mov al , 1                                                                                ; argument nr 1
		mov ah, 1                                                                                ; tryb do odczytu
		call OpenFile
		mov al , 2                                                                                ; argument nr 2
		mov ah, 0                                                                                ; tryb do zapisu
		call OpenFile
		ThreeArgs:
		
		ret
		

OpenFile:					; al - numer,  ah- tryb (1 - odczyt, 0 - zapis)
		push bx
		push dx
		push si
		push cx
		
		mov bx, ax
		
		mov ax, seg arguments
		mov ds, ax
		mov ax, bx
		xor ah, ah
		call GetArgOffset
		push dx
		
		call GetArgLength                                                                                ; przygotowanie nazwy pliku - załadowanie odpowiedniego offsetu i dodanie \0 na końcu
		
		mov si,dx                                                                                             ; dlugość argumentu do si
		pop dx                                                                                                 ;  offset listy argumentów do dx
		mov cl, 0
		push bx
		mov bx, dx
		
		mov ds:[bx+si], cl                                                                                ; wstawienie \0 za nazwą pliku
		pop bx
		cmp bh, 1                                                                                				; sprawdzamy tryb otwarcia
		jz Read
		
		mov ah, 3ch
		mov cx, 0
		int 21h                                                                                                  ; tworzenie nowego pliku (cx - atrybuty), jak istnieje to nadpisz
		jnc OK4
		mov dx, offset file_error
		call PrintErrorAndExit
		OK4:
		jmp NotRead
		Read:
		mov ah, 3dh                                                                                ; otworzenie istniejącego pliku w trybie read-only (al=0)
		mov al, 0
		int 21h
		jnc OK5
		mov dx, offset file_error                                                                    ; jak plik nie istnieje to błąd 
		call PrintErrorAndExit
		OK5:                                                                                ; w ax mamy uchwyt otwartego pliku
		NotRead:
		push si
		cmp bl, 1
		jz First
		mov si, offset handler2
		mov ds:[si], ax
		jmp Second
		First:                                                                                ; zapisujemy uchwyt do odpowiedniej zmiennej
		mov si, offset handler1
		mov ds:[si], ax
		Second:
		pop si
		mov cl, '$'                                                                                ; zamieniamy \0 na $
		mov bx, dx
		mov ds:[bx+si], cl
		

		pop cx
		pop si
		pop dx
		pop bx
		ret

		
GetArgLength:  																; IN : ax - numer argumentu , ds - segment z agrumentami OUT: dx - dlugość
			push si
			push cx
			push ax
			mov dx, ax
			call GetArgOffset
			mov si, dx																	; Do si wrzucamy offset 
			xor cx, cx																; Zerujemy licznik - ilość znaków
			Next:
			mov ah, ds:[si]
			inc si
			cmp ah, '$'
			jz EndOfArg															; Przechodzimy po argumencie zwiększając cx aż do $
			inc cx
			jmp Next
			EndOfArg:
			mov dx, cx																; W dx zwracamy długość
			
			pop ax
			pop cx
			pop si
			ret
		
		
GetArgOffset: 																	; IN ax - numer arg OUT - dx - offset
			push cx																	; Zabezpieczanie rejestrów używanych w funkcji 
			push si
			push ax
			
			mov cx, ax																; Wrzucenie do cx numeru argumentu
			dec cx																		; Przejście do numeracji argumentów od zera
			mov ax, seg arguments
			mov ds, ax																; ds:si  - adres zmiennej z argumentami
			mov si, offset arguments
			cmp cx, 0																	; Sprawdzenie czy chodzi o obecny argument
			jz ItIsFirst
			Search:
				inc si
				mov ah, ds:[si]														; Wrzucamy do ah kolejny znak z listy argumentów
				cmp ah, '$'															; Sprawdzanie czy przeszli=śmy już po całym argumencie
				jnz Search
				dec cx																	; Sprawdzamy czy szukanym argumentem jest następny na liście
				cmp cx, 0
			jnz Search	
			inc si																		; Jak tak to do si wrzucamy początek następnego argumentu
			ItIsFirst:
			mov dx, si																	; Adres zwracamy w dx
			pop ax
			pop si
			pop cx
			ret
		
FirstArgCRC:
		push ax
		push cx
		push bx
		mov ax, seg arguments
		mov ds, ax
		mov dx, 0                                                                                ; w dx przechowujemy CRC, inicjujemy zerem
		LoadBuffer:
		mov bx, ds:handler1
		push dx
		mov dx, offset buffer                                                           ; do bufora czytamy 256 znaków z pliku
		mov cx, 256
		mov ah, 3Fh
		int 21h
		jnc OK6
		mov dx, offset file_error
		call PrintErrorAndExit
		OK6:
		pop dx
		call CRCFromBuf                                                                     ; liczymy CRC z bufora
		cmp ax, cx                                                                               ; sprawdzamy czy nie skończył się plik (ax - ilość przeczytanych znaków)
		jb Eof
		cmp ax, 0
		jz Eof
		jmp LoadBuffer
		Eof:
		pop bx
		pop cx
		pop ax
		ret
		
CRCFromBuf:
		push ax
		push cx
		push bx
		push si
		mov cx, ax
		mov ax, seg buffer
		mov ds, ax
		mov si, offset buffer 
		NextLetter:
		xor bx, bx
		mov bl, ds:[si]                                                                                ; do bl wrzucamy kod kolejnego znaku
		mov ax, dx                                                                                	 ; do ax wrzucamy CRC
		
		shr ax, 8
		and dx, 255
		xor bx, dx
		shl bx, 1                                                                                        ; algorytm obliczania CRC
		push si
		mov si, offset code_table		
		mov dx, ds:[si+bx]
		pop si
		xor dx, ax
		
		inc si
		loop NextLetter                                                                            ; ładujemy kolejną literę
		pop si
		pop bx
		pop cx
		pop ax
		ret
		
Logic:
		push ax
		push si
		push cx
		push bx
		call FirstArgCRC                                                                                    ;do dx wrzucamy CRC pierwszego pliku
		
		mov ax, seg arguments
		mov ds ,ax
		
		call DecToHex                                                                                      ; do result wrzucamy CRC zamienione na hexa
		
		mov si, offset number_of_args
		mov ah, ds:[si]
		cmp ah, 2
		jz TwoArguments
		
		jmp ThreeArguments
		TwoArguments:
		mov bx, ds:handler2
		mov cx, 4                                                                                					   ; zapisujemy 4 bajty
		mov dx, offset result                                                                                ; jak mamy 2 argumenty to w drugim zapisujemy CRC
		mov ah, 40h
		int 21h
		jnc OK7
		mov dx, offset file_error
		call PrintErrorAndExit
		OK7:
		jmp Finish
		ThreeArguments:
		mov cx, 4
		mov bx, ds:handler2
		mov dx, offset result2                                                                                ; jak mamy 3 argumenty to czytamy wartość CRC z drugiego i zapisujemy w result2
		mov ah, 3fh
		int 21h
		mov cx, 4
		CmpNextNum:
		mov bx, cx
		dec bx
		mov al, ds:[result2 + bx]
		cmp ds:[result + bx], al                                                                                ; sprawdzamy czy wortość obliczona i odczytana są równe
		jnz NotCorrect
		loop CmpNextNum
		mov ah, 9
		mov dx, offset crc_match
		int 21h
		jmp AllCorrect
		NotCorrect:                                                                                                ; wypisujemy odpowiedni komunikat
		mov ah, 9
		mov dx, offset crc_dont_match
		int 21h
		AllCorrect:
		Finish:
		mov dx, offset file_error
		
		mov ah, 3eh
		mov bx, ds:handler1
		int 21h
		jc PrintErrorAndExit
		mov bx, ds:handler2                                                                                     ; zamkamy otwarte pliki
		int 21h
		jc PrintErrorAndExit
		pop bx
		pop cx
		pop si
		pop ax
		ret
		
DecToHex:
		push ax
		push si
		push bx
		
		mov ax, seg result
		mov ds, ax
		mov si, offset result
		mov cx , 4
		
		Divide:
		mov bx, dx
		push cx
		dec cx
		cmp cx, 0
		jz CX_is_zero                                                                                     ; przesuwany w lewo i w prawo aby otrzymać jeden znak w hexie
		LPush:
		shl bx, 4
		loop LPush
		CX_is_zero:
		shr bx, 12
		pop cx
		
		mov ax, bx
		cmp ax, 10
		jb ItIsNumber
		add ax, 87
		jmp ItIsLetter                                                                                     ; zamieniamy kod na znak
		ItIsNumber:
		add ax, 48
		ItIsLetter:
		mov bx, cx
		dec bx
		mov ds:[si+bx], al
		
		loop Divide
		pop bx
		pop si
		pop ax
		ret
		
PrintErrorAndExit:
		mov ah, 9
		int 21h
		mov ah, 4ch
		int 21h
		ret
code ends

stack1 segment stack
              dw 200 dup('?')
		top dw '?'
stack1 ends
end start