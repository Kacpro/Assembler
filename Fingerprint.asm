data segment  
	arguments db 128 dup('$')
	number_of_args db 0
	bin_number db 128 dup('?') , '$'
	screen db '+',17 dup('-'),'+',10, 9 dup('|',17 dup('a') ,'|',10), '+',17 dup('-'),'+',10,'$'
	chars db ' ', '.','o', '+', '=', '*','B', 'O', 'X', '@', '%', '&', '#', '/', '^'
	too_few_args db "Too few arugments $"
	too_many_args db "Too many arguments $"
	first_too_long db "First argument is too long $"
	second_too_short db "Second argument is too short $"
	second_too_long db "Second argument is too long $"
	first_not_correct db "First argument has a wrong value $"
	second_not_correct db "Second argument has a wrong value $"
	no_args db "No arguments $"
 
data ends

code segment
start:
	.286
	assume ds: data
	
	mov ax, seg arguments
	mov ds, ax
	mov ax, seg top
	mov ss, ax
	lea sp, top
	
	call Parser
	call CheckArguments
	call ConvertHexToBin
	call MoveLogic
	call VisitsToSigns
	call PrintScreen
	
	mov ah, 4ch
	int 21h
	
	;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////;
	
	Parser:      
	
		pusha																		; Odkładanie na stos rejestrów używanych w funkcji
		
		xor ax, ax
		xor bx, bx
		xor cx, cx																    ; Zerowanie rejestrów
		xor dx, dx
		xor di, di
		xor si, si
		
		call GetCmdArgs														; Ładuje listę argumentów,  OUT: si - offset początku PSP, ds - segment PSP, cl - ilość znaków
		
		mov ax, seg arguments
		mov es, ax																    ; Wysłanie do es segmentu zmiennej do przechowywania PSP
		lea di, arguments														; Ustawienie di na początek tej zmiennej
		
	;	mov bx, di																    ; bx będzie pamiętał offset początku bufora
		
		cmp cl, 2																	; W cl jest ilość znaków 
		jb NoArgs																    ; Jak nie ma argumentów można zakończyć funkcję

		call SignCheckingLoop												; Pętla przechodząca po znakach w liście argumentów i wykonująca odpowiednie działania
																						; IN: si - offset początku argumentów (PSP) , ds - segment PSP, es - segment zmiennej, di - offset początku 	zmiennej, cl - ilość znaków
		NoArgs:
		
		popa																			; Zdejmujemy ze stosu rejestry odłożone na początku funkcji
											
		ret																			; Wracamy do miejsca wywołania
	
	;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////;
	
	GetCmdArgs: 																; OUT: si - offset początku argumentów, ds - segment argumntów, cl - ilość znaków
		push ax
		push bx
		
		mov ah, 62h				
		int 21h																		; Załadowanie segmentu listy argumentów do bx
		
		mov si, 82h																; Ustawienie si na początek listy  argumentów (PSP)
		mov ds, bx																    ; Segment z argumentami jest teraz w ds
		mov cl, ds:[80h]														    ; Wpisanie do cl ilości znaków w liście argumentów
		
		pop bx
		pop ax
		ret

	;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////;

	SignCheckingLoop:														    ; IN: si - offset początku PSP, ds - segment PSP, es - segment zmiennej do przechowywania, di - offset początku zmiennej, cl - ilość znaków
	
		push ax
		push dx
		
		xor dx, dx																    ; Jak dx = 1 i napotkamy biały znak to będzie znaczyć że jest to pierwszy znak po agrumencie i można zrobić odstęp w liście argumentów
													
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
			xor dx, dx
			mov si, offset number_of_args
			mov ah, es:[si]
			inc ah																	; Jak tak to zwiększamy licznik argumentów i przechodzimy na kolejne miejsce w zmiennej ( jeden $ odstępu )
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

	;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////;

	GetArgOffset: 															    ; IN ax - numer arg OUT - dx - offset
			push cx
			push ds																	; Zabezpieczanie rejestrów używanych w funkcji 
			push si
			push ax
			
			mov cx, ax																; Wrzucenie do cx numeru argumentu
			dec cx																	; Przejście do numeracji argumentów od zera
			mov ax, seg arguments
			mov ds, ax																; ds:si  - adres zmiennej z argumentami
			mov si, offset arguments
			cmp cx, 0																; Sprawdzenie czy chodzi o obecny argument
			jz ItIsFirst
			Search:
				inc si
				mov ah, ds:[si]													; Wrzucamy do ah kolejny znak z listy argumentów
				cmp ah, '$'														; Sprawdzanie czy przeszli=śmy już po całym argumencie
				jnz Search
				dec cx																; Sprawdzamy czy szukanym argumentem jest następny na liście
				cmp cx, 0
			jnz Search	
			inc si																		; Jak tak to do si wrzucamy początek następnego argumentu
			ItIsFirst:
			mov dx, si																; Adres zwracamy w dx
			
			pop ax
			pop si
			pop ds
			pop cx
			ret
			
	;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////;

	GetArgLength:  																; IN : ax - numer argumentu , ds - segment z argumentami OUT: dx - dlugość
			push si
			push cx
			push ax
			
			call GetArgOffset
			mov si, dx																; Do si wrzucamy offset 
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
			
	;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////;

	CheckArgLetters:  															; IN : ax - numer argumentu, dl - dolne ograniczenie, dh - górne ograniczenie, ds - segment argumentów, OUT: dx - 0/1
			push si
			push cx
			push dx
			
			call GetArgOffset
			mov si, dx																	; Do si wrzucamy offset argumentu na którym będziemy pracować
			call GetArgLength
			mov cx, dx																	; Ilość znaków do cx
			pop dx																		; Ograniczenia wracają do dx
			NextNumber:
			mov ah, ds:[si]
			sub ah, 48
			cmp ah, dl																	; Sprawdzenie czy znak nie jest poniżej zakresu
			jb RangeError
			
			cmp ah, dh																	;  Sprawdzenie czy znak nie jest powyżej zakresu
			ja MaybeHex															; Jak jest to może być znakiem hex
			jmp InRange						
			MaybeHex:
			cmp ah, 48
			jb RangeError				 											; Sprawdzamy czy znak jest między 'a' i 'f'
			cmp ah, 54
			ja RangeError
			sub ah, 39
			cmp ah, dl																	; Przechodzimy na dec i sprawdzamy czy należy do przedziału
			jb RangeError
			cmp ah, dh
			ja RangeError
			
			InRange:							
			
			inc si																			; Jak wszystko jest ok to przechodzimy do kolejnego znaku
			loop NextNumber
			mov dx, 1																	; Jak wszystkie znaki są poprawne to w dx zwracamy 1
			jmp AllCorrect		
			RangeError:
			mov dx, 0																	; W przeciwnym wypadku zwracamy 0
			AllCorrect:
			
			pop cx
			pop si
			ret
	
	;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////;

	CheckArguments:
	
			push ax
			push si
			push dx
			
			
			mov ax, seg number_of_args										; Załadowanie odpowiedniego segmentu
			mov ds, ax
			
			mov si, offset number_of_args														
			mov ah, ds:[si]
			cmp ah, 2
			jz ArgNumOK
				cmp ah, 2
				ja TooMany															; Sprawdzenie ilości argumentów
				mov dx, offset too_few_args
				call PrintErrorAndExit
				TooMany:
				mov dx, offset too_many_args
				call PrintErrorAndExit
			ArgNumOK:
			
			mov ax, 1
			call GetArgLength
			cmp dx, 1
			jz FirstLenOK													 		; Sprawdzenie długości pierwszego argumentu
				mov dx, offset first_too_long
				call PrintErrorAndExit
			FirstLenOK:
			
			mov ax, 2
			call GetArgLength
			cmp dx, 32
			jz SecondLenOK
				cmp dx,32																;  Sprawdzenie długości drugiego argumentu
				ja TooLong
				mov dx, offset second_too_short
				call PrintErrorAndExit
				TooLong:
				mov dx, offset second_too_long
				call PrintErrorAndExit
			SecondLenOK:
			
			mov dh, 1
			mov dl, 0
			mov ax, 1
			call CheckArgLetters													; Sprawdzenie znaków w pierwszym argumencieie
			cmp dx, 1
			jz FirstSignsOK
				mov dx, offset first_not_correct
				call PrintErrorAndExit
			FirstSignsOK:
			
			mov dh, 16
			mov dl, 0
			mov ax, 2
			call CheckArgLetters
			cmp dx, 1																	; Sprawdzenie znaków w drugim argumencieie
			jz SecondSignsOK
				mov dx, offset second_not_correct
				call PrintErrorAndExit
			SecondSignsOK:
			
			pop dx
			pop si
			pop ax
			ret
	
	;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////;

	ConvertHexToBin:	
	
			pusha
			mov cx, 32 
			mov ax, 2
			call GetArgOffset													; Wrzucenie do si offsetu 2 argumentu
			mov si, dx
			mov di, offset bin_number										; Wrzycenie do di offsetu zmiennej docelowej
			NextNum:																; 32 razy czytamy znak
				mov ah, ds:[si] 													; Wrzucamy znak do ah
				sub ah, 48											
				cmp ah, 10											
				jb CharToIntDone												; Odejmujemy odpowiednią liczbę aby przejść z hex na dec
				sub ah, 39			
				CharToIntDone:
				
				mov al, ah
				xor ah, ah
				
				push cx
				mov cx, 4
				BinStack:
					mov bl, 2
					div bl
					xor bx, bx														; Dzielenie przez 2 i wrzucanie na stos
					mov bl, ah
					push bx
					xor ah, ah
				loop BinStack
				mov cx, 4
				BinSave:
					pop bx
					mov ah, bl														; Ściąganie ze stosu i zapisywanie w odwrotnej kolejności
					mov ds:[di], ah
					inc di
				loop BinSave
				pop cx
				inc si																	; Przechodzimy do kolejnego miejsca w zmiennej
			loop NextNum
			
			popa
			ret
	
	;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////;

	PrintScreen:
		
		push ax
		push dx
		mov dx, offset screen
		mov ah, 9																		; Wypisaniw wyniku na ekran
		int 21h
		pop dx
		pop ax
		ret
	
	;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////;
	
	AddOne:   																		;IN: dh, dl - współrzędne
		push dx
		push si
		push ax
		
		xor ax, ax
		mov si, offset screen													   ; Wrzucamy offset planszy do si
		mov al, 20																	   ; Wpisanie do al długości linijki
		mul dh
		xor ah, ah																		; Obliczenie obecnej pozycji
		add al, dl	
		add si, ax
		mov ah, ds:[si]												
		cmp ah, 'o'																	; Sprawdzamy czy na danym polu nie byliśmy już co najmniej 14 razy
		jae DontAdd
		inc ah																			; Jak nie to zwiększamy ilość odwiedzin o 1
		mov ds:[si], ah
		DontAdd:
		pop ax
		pop si
		pop dx
		ret
	
	;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////;
	
	AddSE:   																			; IN: dh, dl - współrzędne					
		push dx
		push si
		push ax
		xor ax, ax
		mov si, offset screen
		mov al, 20																		; Wpisanie do al długości linijki
		mul dh
		xor ah, ah
		add al, dl																		; Obliczenie odpowiedniego pola
		add si, ax
		mov ah, ds:[si]
		mov ah, 69																	; Przejście na pole i wstawienie 'E'
		mov ds:[si], ah
		
		mov si, offset screen[109]
		mov ah, 83																	; Przejście na środek planszy i wpisanie 'S'
		mov ds:[si], ah
		
		pop ax
		pop si
		pop dx
		ret
	
	;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////;
																							; IN: dh, dl - współrzędne
	GoLeft:
		cmp dl, 1
		jz NoLeft
		dec dl
		NoLeft:
		ret
		
	GoRight:
		cmp dl, 17
		jz NoRight
		inc dl
		NoRight:
		ret
																							; Sprawdzenie czy można i przesunięcie w odpowiednim kierunku
	GoUp:
		cmp dh, 1
		jz NoUp
		dec dh
		NoUp:
		ret
		
	GoDown:
		cmp dh, 9
		jz NoDown
		inc dh
		NoDown:
		ret
		
	;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////;
	
	MoveLogic:																		;IN: dl, dh - współrzędne
		push ax
		push cx
		mov cx, 16																	    ; Będziemy pobierać 16 bajtów
		push si
		push dx
		mov si, offset bin_number
		mov dl, 9																		; Ustawiemy się na środku planszy
		mov dh, 5
		push di																			; di - dodatkowa zmienna pomocnicza
		JumpToNextByte:
			xor bx, bx
			xor ax, ax
			mov di, cx
			
			mov cx, 8
			PushByte:
				xor ax, ax
				mov al, ds:[si]														; Odkładamy 8 bitów na stos
				push ax
				inc si
			loop PushByte
	
			mov cx, 4
			PopByteRev:
				xor bx, bx
				xor ax, ax
				pop bx
												
				pop ax																	; 4 razy ściągamy po 2 bity i obliczamy ich wartość binarną
								
				add al, al
				add al, bl                                                              ; W al jest rodzaj ruchu
				
			call SelectVersion													 ; Do ah wrzucamy wartość 1 argumentu
			
			cmp al, 0
			jnz NotA
				cmp ah, 0
				jz NotKnight1
					call GoLeft														  ; Lewo, Góra / Lewo, Lewo, Góra
				NotKnight1:
				call GoLeft
				call GoUp
				call AddOne
			NotA:
			
			cmp al, 1
			jnz NotB
				cmp ah, 0
				jz NotKnight2
					call GoRight														  ; Prawo, Góra / Prawo, Prawo, Góra
				NotKnight2:
				call GoRight
				call GoUp
				call AddOne
			NotB:
			
			cmp al, 2
			jnz NotC
				cmp ah, 0
				jz NotKnight3
					call GoLeft														    ; Lewo, Dół / Lewo, Lewo, Dół
				NotKnight3:
				call GoLeft
				call GoDown
				call AddOne
			NotC:
			
			cmp al, 3
			jnz NotD
				cmp ah, 0
				jz NotKnight4
					call GoRight														    ; Prawo, Dół / Prawo, Prawo, Dół
				NotKnight4:
				call GoRight
				call GoDown
				call AddOne
			NotD:
			
		loop PopByteRev															    ; Przechodzimy do kolejnej pary bitów
		
		mov cx, di
		loop JumpToNextByte													    ; Przychodzimy do kolejnego bajtu
		
		call AddSE																	    ; Dodajemy literki 'S' i 'E'
		
		pop di
		pop dx
		pop si
		pop cx
		pop ax
		ret
	
	;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////;
	
	SelectVersion: 																	    ; OUT: ah - 1 - goniec, 2 - skoczek
			push dx
			push si
			push bx
			
			mov bx, ax
			
			mov ax, 1
			call GetArgOffset
			mov si, dx																	    ; Do bh wrzucamy wartość pierwszego argumentu
			mov bh, ds:[si]
			sub bh, 48
			
			mov ax, bx																    ; W ah zwracamy wartość pierwszego argumentu zachowując al
			
			
			pop bx
			pop si
			pop dx
			ret
	
	;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////;
	
	VisitsToSigns:
			push ax
			push si
			push di
			push cx
			mov di, offset screen
			mov si, offset chars
			mov cx, 220																    ; Ilość znaków w planszy
			Convert:
				mov al, ds:[di]														    ; Do al wrzucamy znak z planszy i zamieniamy go na liczbę
				sub al , 97
				cmp al, 0
				jb Edge
				cmp al, 14																    ; Sprawdzamy czy wartość jest z zakresu [0, 14] jak nie to jest to ramka
				ja Edge
				
				push ax
				xor ah, ah
				push si
				add si, ax														
				mov al, ds:[si]														    ; Do al wrzucamy znak odpowiadający ilości wizyt na tym polu
				mov ds:[di], al														    ; Zapisujemy na danym polu ten znak
				pop si
				pop ax
				
				Edge:
				inc di																	    ; Przechodzimy na kolejne pole w planszy
			loop Convert
			
			pop cx
			pop di
			pop si
			pop ax
			ret
	
	;//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////;
	
	PrintErrorAndExit:															    ;IN: dx - offset komunikatu
		mov ax, seg arguments
		mov ds, ax
		mov ah, 9
		int 21h																			    ; Wypisujemy odpowiedni komunikat i kończymy program
		mov ah, 4ch
		int 21h
	ret
	
code ends

stack1 segment stack
	dw 128 dup('?')
	top dw ?
stack1 ends
end start