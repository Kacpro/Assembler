data segment
	arguments db 128 dup('$') 
	 number_of_args db 0
	 nope db "NOPE $"
	 minx dq ?
	 miny dq ?
	 maxx dq ?
	 maxy dq ?
	 buf1 dw ?, '$' 
	 buf2 dw ?, '$'
	 ten dw 10
	 max dq 4.0
	 y_display dq 200
	 x_display dq 320
	 buf2Length dw ?, '$'
	 
	 too_few_args db "Too few arguments $"
	 too_many_args db "Too many arguments $"
	 plus_or_minus db "All arguments must start with a '+' or a '-' $"
	 one_dot db "Wrong argument, more than one '.' $"
	 wrong_signs db "Wrong argument, invalid signs $"
	 no_dot db "Wrong argument, no dot $"
	 no_after db "Wrong argument, no signs after a dot $"
	 no_before db "Wrong argument, no signs before a dot $"
data ends

code segment
start:
	.286
	finit
	mov ax, seg arguments
	mov ds, ax
	mov ax, seg top
	mov ss, ax
	mov sp, offset top
	
	call Parser
	
	call CheckArgs
	
	call SaveMinMax
	
	call PrintResult

	mov ah, 4ch
	int 21h
	
	;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	
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
		
		
		
		CheckArgs:
		
				pusha
				mov ax, seg arguments
				mov ds, ax
				mov ah, ds:[number_of_args]					; Sprawdzmy czy są 4 argumenty
				cmp ah, 4
				jna OK1
				mov dx, offset too_many_args
				call PrintErrorAndExit
				OK1:
				cmp ah, 4
				jnb OK2
				mov dx, offset too_few_args
				call PrintErrorAndExit
				OK2:
				
				mov cx, 4
				CheckOneArg:											; Pętla przechodząca po wszystkich argumentach
				xor bx, bx													; bl=1 - są cyfry przek kropką, bh=1 - są cyfry za kropką
				xor di, di													; Jak di=1 to znaczy że już była kropka
				mov ax, cx
				call GetArgOffset									
				mov si, dx													; Do si wrzucamy offset obecnego argumentu	
				call GetArgLength
				push cx
				mov cx, dx													; Do cx wrzucamy długość obecnego argumentu	
				CheckOneNumber:									; Pętla sprawdzahąca znaki pojedynczego argumentu
				mov al, ds:[si]
				
				cmp dx, cx													; Sprawdzamy czy jesteśmy na pierwszym znaku
				jne NotFirst
				cmp al, 43													; Sprawdzamy czy to jest '+'
				jne NotPlus
				jmp OK
				NotPlus:
				cmp al, 45													; Sprawdzamy czy to jest '-'
				je OK3
				mov dx, offset plus_or_minus
				call PrintErrorAndExit
				OK3:
				jmp OK
				NotFirst:
				
				cmp al, 46
				jne NotDot
				cmp di, 1														; Sprawdzamy czy to jest kropka i czy jeszcze jej nie było
				jne OK4 
				mov dx, offset one_dot
				call PrintErrorAndExit
				OK4:
				mov di, 1
				jmp OK
				NotDot:
				
				cmp al, 48
				jnb OK5 
				mov dx, offset wrong_signs
				call PrintErrorAndExit								; Sprawdzamy czy znak jest cyfrą
				OK5:
				cmp al, 57
				jna OK6
				mov dx, offset wrong_signs
				call PrintErrorAndExit
				OK6:
				
				cmp di, 1														; Sprawdzamy czy jesteśmy przed czy za kropką
				je AfterDot
				mov bl, 1														; Zapisujemy, że cą jakieś cyfry przed / za kropką
				jmp BeforeDot
				AfterDot:
				mov bh, 1 
				BeforeDot:
				
				OK:
				inc si															; Przechodzimy na kolejny znak
				loop CheckOneNumber
				
				cmp di, 0													; Sprawdzamy czy była kropka
				jne OK7
				mov dx, offset no_dot
				call PrintErrorAndExit
				OK7:
				cmp bh, 0													; Sprawdzamy czy były jakieś cyfry za kropką
				jne OK8
				mov dx, offset no_after
				call PrintErrorAndExit
				OK8:
				cmp bl, 0													; Sprawdzamy czy były jakieś cyfry przed kropką
				jne OK9
				mov dx, offset no_before
				call PrintErrorAndExit
				OK9:
				
				pop cx
				dec cx
				cmp cx, 0
				jnz CheckOneArg									; Przechodzimy do kolejnego argumentu
				popa
		ret
		
		SaveMinMax:
				pusha
				mov ax, seg arguments
				mov ds, ax
				mov cx, 4
				CountMore:												; Pętla przechodząca po wszystkich argumentach
				
				mov ax, cx
				call GetArgOffset									; Do si wrzucamy offset obecnego argumentu i pomijamy znak ( plus lub minus )
				mov si, dx
				inc si
				
				xor dx, dx
				inc dx															; W dx trzymamy długość argumentu od początku do kropki
				xor ax, ax													
				
				Mulloop:
				inc dx
				xor bx, bx
				mov bl, ds:[si]
				cmp bl, 46													; Sprawdzamy czy doszliśmy do kropki
				je ItsDot
				push cx
				push dx
				
				mov cx, 10
				mul cx															; W ax trzymamy wynik
				sub bx, 48													; Mnożymy aktualną wartość ax przez 10 i dodajemy obecny znak
				add ax, bx
				
				pop dx
				pop cx
				inc si															; Przechodzimy do kolejnego znaku
				jmp Mulloop
				ItsDot:
				
				mov ds:[buf1], ax										; Zapisujemy część przed kropką do pamięci
				inc si
				mov bx, dx
				mov ax, cx
				call GetArgLength
				sub dx, bx													; Do dx wrzucamy długość części po kropce
				mov ds:[buf2Length], dx							; Zapisujemy tę wartość do pamięci
				
				push cx
				mov cx, dx													; Do cx wrzucamy długość części po kropce
				xor ax, ax
				xor bx, bx
				AfterDotPart:
				push cx
				push dx
				xor bx, bx
				
				mov cx, 10
				mul cx
				mov bl, ds:[si]											; Wyliczamy wartość części ułamkowej
				sub bl, 48
				add ax, bx
				
				pop dx
				pop cx
				inc si															; Przechodzimy do kolejnego znaku
				loop AfterDotPart
					
				
				mov ds:[buf2],ax										; Zapisujemy część ułamkową do pamięci
				pop cx
				
				push dx
				push si
				push bx
				push ax
				mov ax, cx
				call GetArgOffset
				mov si, dx														; Do si wrzucamy offset obecnego argumentu
				pop ax
				mov bl, ds:[si]
				cmp bl, '-'														; Sprawdzamy czy pierwszy znak to minus
				jnz DontNegate
				
				mov ax, ds:[buf1]
				neg ax
				mov ds:[buf1], ax											; Jak tak to negujemy zapisane wartości
				mov ax, ds:[buf2]
				neg ax
				mov ds:[buf2], ax
				
				DontNegate:
				pop bx
				pop si
				pop dx
				
				mov ax, cx
				call SaveFraction												; Zapisujemy wartości z buforów do odpowiednich zmiennych - argument w ax
				dec cx
				cmp cx, 0
				jnz CountMore													; Przechodzimy do kolejmego argumentu
				popa
		ret
		
		SaveFraction:															; Ax nr argumentu
				push cx
				
				fild ds:buf1														; Wrzucamy na stos część całkowitą
				fild ds:ten															; Wrzucamy na stos liczbę 10
				fild ds:buf2														; Wrzucamy na stos część ułamkową
				
				mov cx, ds:[buf2Length]									; Do cx wrzucamy długość części ułamkowej
				
				Divide:
				fdiv st, st(1)														; Dzielimy część ułamkową przez 10 aż dostaniemy wartość mniejszą od 1
				loop Divide
				
				fxch
				fsubp st, st														; Usuwamy liczbę 10 ze stosu
				
				fadd st, st(1)														; Dodajemy do siebie część całkowitą i ułamkową
				
				cmp ax, 1
				je Xmin
				cmp ax, 2
				je Xmax
				cmp ax, 3
				je Ymin
				cmp ax, 4
				je Ymax
				Xmin:
				fstp ds:minx														; Zapisujemy wynik do odpowiedniej zmiennej i usuwamy wierzchołek stosu
				jmp Done
				Ymin:
				fstp ds:miny
				jmp Done
				Xmax:
				fstp ds:maxx
				jmp Done
				Ymax:
				fstp ds:maxy
				
				Done:
				fsubp st, st															; Czyścimy stos
				pop cx
		ret
		
		Logic:
				
				pushf
				xor si, si
				xor di, di
				YLoop:
				cmp di, 200
				je YLoopExit
				
				XLoop:
				cmp si,  320
				je XLoopExit
																									; Wyliczanie P
				fld ds:[minx]																; minx
				fld ds:[maxx]															; maxx			minx
				fsub st, st(1)																; maxx-minx			minx
				mov bx, si																	; Wrzucamy współrzędną X do bx
				mov ds:[buf1], bx
				fild ds:[buf1]    															; N			maxx-minx			min
				fmul st, st(1)																; N*(maxx-minx)			maxx-minx			minx
				fxch																			; maxx-minx			N*(maxx-minx)			minx
				fsubp st, st																; N*(maxx-minx)			minx
								
				fild ds:[x_display]													; x_rozdz			N*(maxx-minx)			minx
				fdiv st(1), st
				fsubp st, st																; N*(maxx-minx)/x_rozdz		minx
				fadd st, st(1)																; minx + N*(maxx-minx)/x_rozdz		minx
				fxch
				fsubp st, st																; minx + N*(maxx-minx)/x_rozdz			= P
																					
																									; Wyliczanie Q
				fld ds:[miny]																; miny			p
				fld ds:[maxy]																; maxy				miny			p
				fsub st, st(1)																; maxy-miny			miny			p
				mov bx, di																	; Wrzucamy współrzędną Y do bx												
				mov ds:[buf1], bx
				fild ds:[buf1]    															;  M			maxy-miny			miny			p
				fmul st, st(1)																; M*(maxy-miny)			maxy-miny			miny			p
				fxch
				fsubp st, st																; M*(maxy-miny)			miny			p
										
				fild ds:[y_display]													; y_rozdz			M*(maxy-miny)			miny			p
				fdiv st(1), st																; y_rozdz			M*(maxy-miny)/y_rozdz			miny			p	
				fsubp st, st																; M*(maxy-miny)/y_rozdz			miny			p
				fadd st, st(1)			
				fxch
				fsubp st, st																; miny + M*(maxy-miny)/y_rozdz			p				= q		p
				
				fldz
				fldz																				; x=0			y=0			q			p
				
				mov cx, 1000
				LogicLoop:
				fldz																				; 0			x			y			q			p
				fadd st, st(1)
				fmul st, st
				fldz																				; 0			x*x			x			y			q			p
				fadd st, st(3)
				fmul st, st																	; y*y			x*x 		x			y			q			p
				fsub st(1), st																; y*y			x*x - y*y			x			y			q			p
				fsubp st, st																; x*x - y*y			x			y			q			p
				fadd st, st(4)															; x*x - y*y + p			x			y			q			p
				fld1																			; 1			x*x - y*y + p			x			y			q			p
				fld1																			; 1			1			x*x - y*y + p			x			y			q			p
				fadd st(1), st
				fsubp st, st																; 2			x*x - y*y + p			x			y			q			p
				fmul st, st(2)
				fmul st, st(3)																; 2*x*y			x*x - y*y + p			x			y			q			p
				fadd st, st(4)															; 2*x*y +q			x*x - y*y + p			x			y			q			p
				fxch st(3)
				fsubp st, st
				fxch  st(1)
				fsubp st, st																; x*x - y*y + p			2*x*y +q			q			p
				
																									; x*x - y*y + p = x			2*x*y+q  = y			q			p
				fldz																				; 0			x			y			q			p
				fadd st, st(1)
				fmul st, st																	; x*x			x			y			q			p
				fldz																				; 0			x*x			x			y			q			p
				fadd st, st(3)
				fmul st, st																	; y*y			x*x			x			y			q			p
				fadd st(1), st
				fsubp st, st																; x*x + y*y		 x			y			q			p
				
				fcomp ds:[max]															; Porównanie wierzchołka z wartością 4.0 i ustawienie flag koprocesora
				fstsw ax
				sahf																			; Wrzucenie flag koprocesora do zwykłego rejestru flag
				ja Break																		; Jak wartość jest większa niż 4 to wychodzimy z pętli
				
				loop LogicLoop															; Kolejna iteracja	
				mov al, 15
				jmp White																	; Jak przejdziemy przez wszystkie iteracje to do al wrzucamy kod koloru białego
				Break:
				
				mov al, 0
				White:																		; Jak przerwiemy pętle to do al wrzucamy kod koloru czarnego
				
				mov ah, 0ch
				mov dx, di
				mov cx, si																	; Kolorujemy piksel o współrzędnych x = cx, y = dx o kolorze, którego kod jest w al
				int 10h
				
				fsubp st, st
				fsubp st, st
				fsubp st, st																; Czyścimy stos koprocesora
				fsubp st, st
				
				inc si																			; Zwiększamy współrzędną X
				jmp XLoop
				XLoopExit:
				xor si, si
				inc di																			; Zwiększamy współrzędną Y
				jmp YLoop
				YLoopExit:
				popf
		ret
		
		
		
		PrintResult:
				xor ah, ah
				mov al, 13																	; Przechodzimy w tryb graficzny 320x200
				int 10h
				
	
				call Logic
				
				xor ax, ax
				int 16h																		; Czekamy na naciśnięcie klawisza
				
				xor ax, ax
				mov al, 3																		; Powracamy do trybu tekstowego
				int 10h
		
		ret
		
		PrintErrorAndExit:
				mov ah, 9
				int 21h
				mov ah, 4ch
				int 21h
				ret
		
		
	
code ends

stack1 segment stack
			dw 200 dup (?)
	top	dw ?
stack1 ends
end start