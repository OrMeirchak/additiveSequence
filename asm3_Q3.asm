;name: or kampler meirchak, id: 313432965   , guy hassin 312253255
TITLE asm3_Q3
INCLUDE irvine32.inc
INCLUDE asm3_q3_data.inc

.data
myName BYTE "or kampler meirchak 313432965 , guy hassin 312253255",0
string1 byte "Input : s = ",0
string2 byte "Output : ",0
string3 byte "Explanation : ",0
string4 byte "false",0
.code
main proc

mov EDX,OFFSET myName
call WriteString
call CRLF

mov EAX,OFFSET res
push EAX

mov EAX,OFFSET num
push EAX

mov EAX,N
push EAX

call IsAddSeq

mov EDX,OFFSET string1
call WriteString

mov EDX,OFFSET num
call WriteString
call CRLF


mov EDX,OFFSET string2
call WriteString

cmp AL,1
JNE FALSECASEEE

mov EDX,OFFSET res
call WriteString
call CRLF

mov EDX,OFFSET string3
call WriteString

mov EAX,OFFSET res
push eax
call PrintSeq

JMP ENDOFMAIN

FALSECASEEE:mov EDX,OFFSET string4
call WriteString

ENDOFMAIN:exit
main endp
;______________________Print seq_________________________
;the function input
;res array

PrintSeq proc uses EAX EBX

USES=8
ResAddr=USES+8
nextResAddr=-4

    push ebp
    mov ebp,esp
	sub esp,4

	mov EBX,[EBP+resAddr]

	     ;print the first num
	loop1:mov AL,[EBX]
		  call WriteChar
		  inc EBX
		  cmp AL,32
		  JNE loop1
		  mov [EBP+nextResAddr],EBX

		  ;print the '+ '
		  mov AL,43
		  call WriteChar
		  mov AL,32
		  call WriteChar

         ;print the second num
	loop2:mov AL,[EBX]
		  call WriteChar
		  inc EBX
		  cmp AL,32
		  JNE loop2

		  ;print the '= '
		  mov AL,61
		  call WriteChar
		  mov AL,32
		  call WriteChar

		  ;print the third num
	 loop3:mov AL,[EBX]
		  call WriteChar
		  inc EBX
		  cmp AL,0
		  JE ENDOFPRINT
		  cmp AL,32
		  JNE loop3 

		  ;print ','
		  mov AL,44
		  call WriteChar

		  call PrintSeq

ENDOFPRINT:mov esp,ebp
	       pop ebp
	       ret 4
PrintSeq endp


;______________________IsAddSeq__________________________
;the function input:
;res Array
;num
;length of num

IsAddSeq proc USES EBX ECX EDX

USES = 12
StrSize = USES + 8
StrNum = StrSize + 4
ResArray = StrNum + 4
dynAll1 = -N
dynAll2 = dynAll1-N
dynAll3 = dynAll2-N

    push ebp
    mov ebp,esp
    add ESP,dynAll3
	
	;EDX=StrLength/2
	mov EDX,[EBP+StrSize]
	shr EDX,1

	       
	        mov ECX,1
outerloop:  
                     
					
					 ;EDX=StrLength-ECX/2
					 push EDX
	                 mov EDX,[EBP+StrSize]
					 sub EDX,ECX
	                 shr EDX,1

					 mov EBX,1
          innerloop:

		            ;dynAll1=subString(o,ECX)
					lea EAX,[EBP+dynAll1]
					push EAX
					push ECX;------LEN
					push 0
			        push [EBP+StrSize]
					push [EBP+StrNum]
					call SubString

					;dynAll2=subString(ECX,EDX)
					lea EAX,[EBP+dynAll2]
					push EAX
					push EBX;--------LEN
					push ECX
					push [EBP+StrSize]
					push [EBP+StrNum]
					call SubString

					;dynAll3=subString(ECX+EDX)
					lea EAX,[EBP+dynAll3]
					push EAX
					mov EAX,[EBP+StrSize]
					sub EAX,ECX
					sub EAX,EBX
					dec EAX
					push EAX;--------LEN
					mov EAX,ECX
					add EAX,EBX
					push EAX
					push [EBP+StrSize]
					push [EBP+StrNum]
		            call SubString

				   push [EBP+ResArray]
				   lea EAX,[EBP+dynAll1]
                   push EAX
				   mov EAX,ECX
				   inc EAX
				   push EAX
				   lea EAX,[EBP+dynAll2]
                   push EAX
				   mov EAX,EBX
				   inc EAX
				   push EAX
				   lea EAX,[EBP+dynAll3]
                   push EAX
				    mov EAX,[EBP+StrSize]
					sub EAX,ECX
					sub EAX,EBX
					push EAX;
					call ChkAddition
					cmp AL,1
					JE TRUECASE

		            ;innerloop end
                    inc EBX
			        cmp EBX,EDX
			        JBE innerloop
					pop EDX

	        ;outerloop end
	        inc ECX
			cmp ECX,EDX
			JBE outerloop
		
	    mov al,0
		jmp ENDOFPROC


		TRUECASE:
		push [EBP+ResArray]
		lea EAX, [EBP+dynAll2]
		push EAX
		call PushFront

		push [EBP+ResArray]
		lea EAX, [EBP+dynAll1]
		push EAX
		call PushFront
		mov al,1

 ENDOFPROC:mov esp,ebp
	       pop ebp
	        ret 12

IsAddSeq endp


;______________________ChkAddition__________________________
;the function input:
;res Array
;strA
;str A size
;str B
;str B size
;str C
;str C size
;AL=1 
;EBX=new end of res 




ChkAddition proc USES EBX ECX EDX
USES = 12
Csize = USES +8
Cstr = Csize + 4
Bsize = Cstr + 4
Bstr = Bsize + 4
Asize = Bstr + 4
Astr = Asize + 4
RESARRAY = Astr + 4
SUM = -N
TEMPC = SUM - Csize


    push ebp
    mov ebp,esp
	add ESP,tempC

	;clear sum
	mov ecx,N
	lea eax,[EBP+SUM]
	mov bl,0
	loopy:mov [eax],bl
	inc eax
	loop loopy

	;cheak A is valid
	mov EAX,[EBP+Asize]
	push EAX
	mov EAX,[EBP+Astr]
	push EAX
    call isVaild
	cmp AL,0
	JE FALSECASE

	;cheak B is valid
	mov EAX,[EBP+Bsize]
	push EAX
	mov EAX,[EBP+Bstr]
	push EAX
    call isVaild
	cmp AL,0
	JE FALSECASE
	
	;SUM=A+B
	mov EAX,[EBP+Astr]
	push EAX
	mov EAX,[EBP+Asize]
	dec EAX
	push EAX
	mov EAX,[EBP+Bstr]
	push EAX
	mov EAX,[EBP+Bsize]
	dec EAX
	push EAX
	lea EAX,[EBP+SUM]
	push EAX
	call addString

	lea EAX,[EBP+SUM]

	;if (sum!=c) --> jump to case2
	lea EAX,[EBP+SUM];
    push EAX
	mov EAX,[EBP+Cstr]
	 push EAX
	
	call CmpStr
	cmp AL,1
	JNE CASE2

	;if (sum==c) --> add sum to Res and return true
	mov EAX,[EBP+RESARRAY]
	push EAX
	lea EAX,[EBP+SUM]
	push EAX
	call PushBack
	JMP TRUECASE

	   ;if Csize<=sumSize --> return false
CASE2: mov EAX,[EBP+Csize]
       cmp EAX,ECX
	   JBE FALSECASE


	   ;tempC = c.substr(0,sumSize)
	   lea EAX,[ebp+TEMPC]
	   push EAX
	   mov EAX,ECX
	   DEC EAX
	   push EAX
	   xor EAX,EAX
	   push EAX
	   mov EAX,[EBP+Csize]
	   push EAX
	   mov EAX,[EBP+Cstr]
       push EAX
	   call SubString


	   ;if sum!=c.substr(0,sumSize) --> return false
	   lea EAX,[EBP+TEMPC]
	   push EAX
	   lea EAX,[EBP+SUM]
	   push EAX
	   call CmpStr
	   cmp AL,1
	   JNE FALSECASE

	   ;add SUM to res
	   mov EAX,[EBP+RESARRAY]
	   push EAX
	   lea EAX,[EBP+SUM]
	   push EAX
	   call PushBack

	   ;tempC= c.subStr(sum.Size)
	   lea EAX,[EBP+TEMPC]
	   push EAX
	   mov EDX,[EBP+Csize]
	   sub EDX,ECX;          
	   inc EDX  ;    EDX=tempC size
	   push EDX
	   mov EBX,ECX
	   dec EBX
	   push ebx
       mov EAX,[EBP+Csize]
	   push EAX
	   mov EAX,[EBP+Cstr]
	   push EAX
	   call subString

	   ;rexursive
	   mov EAX,[EBP+RESARRAY]
	   push EAX
       mov EAX,[EBP+Bstr]
	   push EAX
	   mov EAX,[EBP+Bsize]
	   push EAX
	   lea EAX,[EBP+SUM]
	   push EAX
	   push ECX
	   lea EAX,[EBP+TEMPC]
	   push EAX
	  push EDX
	   call ChkAddition
	   jmp ENDOFPROC

FALSECASE: mov AL,0
           jmp ENDOFPROC


TRUECASE: mov AL,1
ENDOFPROC: mov esp,ebp
	 pop ebp
	 ret 28

ChkAddition endp
;______________________addString__________________________
;the function input:
;strA
;size A
;str B
;size B
;str sum

AddString PROC

push eax
push ebx
push edx
push ebp
mov ebp, esp
push esi


sum = 20
sizeB = sum+ 4
addrB = sizeB + 4
sizeA = addrB + 4
addrA = sizeA + 4
counter = -36
carry = -32

mov esi, [ebp+sum]
xor eax,eax
xor ebx,ebx
xor ecx,ecx
xor edx,edx
	
mov dword ptr carry[ebp], 0
mov dword ptr counter[ebp], 1
mov ebx,[ebp + sizeA]
mov edx,[ebp + sizeB]
mov ecx,edx
cmp edx,ebx
jge continue
mov ecx,ebx



continue:
add ecx,1
xor ebx,ebx
xor edx,edx

loopy:
    mov ebx,[ebp + addrA]
    push ebx
    xor ebx,ebx
    mov ebx,[ebp + sizeA]
    push ebx
    sub ebx,[ebp+counter]
    push ebx
    xor ebx,ebx
    call val
    xor edx,edx
    add dl, al

    mov ebx,[ebp + addrB]
    push ebx
    xor ebx,ebx
    mov ebx,[ebp + sizeB]
    push ebx
    sub ebx,[ebp+counter]
    push ebx
    xor ebx,ebx
    call val
    add dl, al
    add dl,[ebp+carry] 


  xor eax,eax
  xor ebx,ebx
    mov al, dl
    xor dx,dx
    xor bx,bx
	add bx,10
    div bx
    add dx,48
    mov [esi], dl
    add esi,1
    mov [ebp+carry],ax

    inc dword ptr [ebp+counter]
    cmp ecx,[ebp+counter]
    je finAddStr
    jmp loopy

finAddStr:

cmp dword ptr carry[ebp],0
je endd
add dword ptr carry[ebp],48
push eax
mov eax, [ebp+carry]
add [esi], al
pop eax
inc ecx


endd:
mov ebx, [ebp+sum]
mov esi, [ebp+sum]
push [ebp+sum]
push ecx
call ReverseString

xor esi,esi
add esi, [ebp+sum]
add esi, ecx
sub esi,1
mov byte ptr [esi],0


pop esi
mov esp, ebp
pop ebp
pop edx
pop ebx
pop eax
ret 20

AddString endp
;______________________SubString__________________________
;the function input:
;1.*res
;2.len
;3.pos
;4.String size without 0 
;5.*str
;the function return:
;1.al = 1 if valid
;2.al = 0 if not valid

SubString proc USES EBX ECX EDX

    push ebp
    mov ebp,esp
	
	;if len<=0 return true
	mov EAX,[EBP+32]
	cmp eax,0
	jbe TRUECASE


	;if pos>strSize return false
	mov EBX,[EBP+24]
	mov ECX,[EBP+28]
	cmp ECX,EBX
	jae FALSECASE

	;[res]=[str+pos]
	mov EDX,[EBP+36]
	push EAX
	push EBX
	mov EAX,[EBP+20]
	add EAX,ECX
	mov bl,[EAX]
	mov [edx],bl
    pop EBX
	pop EAX

	dec EAX
	;dec EBX
	inc ECX
	inc EDX
	mov byte ptr [edx],0
    
	push EDX
	push EAX
	push ECX
	push EBX
    push [EBP+20]

    call SubString

	cmp AL,0
	JE FALSECASE

TRUECASE:
mov al,1
	     jmp FINISH

FALSECASE:mov al,0

FINISH:


mov esp,ebp
	    pop ebp
	    ret 20

SubString endp


;______________________isVaild__________________________
;the function input:
;1.size
;2.offset
;the function return:
;1.al = 1 if valid
;2.al = 0 if not valid
isVaild proc USES EBP EBX EDX

    push ebp
    mov ebp,esp
	
	;eax=0
	xor eax,eax

	;ebx=0
	xor ebx,ebx

	;if (size<1)  jump to false
	mov ebx,[ebp+24]
	cmp ebx,1
	JBE falseCase
	
	;ebx=0
	xor ebx,ebx

	;edx=0
	xor edx,edx

	;if ([offset]=='0') jump to false
	mov ebx,[ebp+20]
	mov dl,[ebx]
	cmp edx,'0'
	JE falseCase

	;al=1
	inc al

falseCase:  mov esp,ebp
	pop ebp
	ret 8 
isVaild endp

	
	;____________________CmpStr___________________
;the function input:
;1.offset of str1
;2.offset of str2
;the function return
;if str1==str2  AL=1
;if str1!=str2 AL=0
CmpStr proc USES EBP EBX EDX


    push ebp
    mov ebp,esp

	;EAX=offset str1
	mov eax,[ebp+20]

	;EBX=offset str2
	mov ebx,[ebp+24]
    
	       ;check if str1 finish
   LOOP1:  mov dl,[eax]
	       cmp dl,0
           JE EndOfStr1

           ;check if str2 finish
           mov dl,[ebx]
           cmp dl,0
           JE FalseCase

          JMP CHECK_IF_EQUAL;str1 and str2 not finish

              ;check if str1 and str2 finish
               
   EndOfStr1:  mov dl,[ebx]
               cmp dl,0
               JE TrueCase

                 ;if [eax]!=[ebx] return false
   CHECK_IF_EQUAL:mov dl,[eax]
                  mov dh,[ebx]
                  cmp dl,dh
                  JNE FalseCase
                  inc eax
                  inc ebx
                  jmp LOOP1

FalseCase: xor eax,eax
jmp FINISH 

TrueCase:mov eax,1

FINISH: mov esp,ebp
	pop ebp
	ret 8
CmpStr endp


;____________________PushBack___________________
;the function input:
;starting address of string1
;starting address of string2
;the fucntin add string string2 to the back of string1 (string1=string1+string2)
PushBack proc USES EBP EAX EBX EDX


    push ebp
    mov ebp,esp

	
	;EBX=offset string1
	mov ebx,[ebp+28]

	;EDX=offset string2
	mov edx,[ebp+24]

	loop1:  mov al,[ebx]
	        cmp al,0
			JE NEXTSTEP
			inc ebx
			jmp loop1
			
			NEXTSTEP:
			cmp ebx,[ebp+28]
			JE loop2
	        push ecx
			mov cl,32
			mov [ebx],cl
			pop ecx
			inc ebx
	loop2:   mov al,[edx]
			 mov [ebx],al
			 cmp al,0
	       	 JE FINISH
	    	 inc edx
			 inc ebx
		     jmp loop2
	        
FINISH:mov esp,ebp
	     pop ebp
	     ret 8

PushBack endp

;____________________Val___________________
val proc

N = 12  ;index
N1 = 16 ;size
N2 = 20 ;address

push ebx
push ebp
mov ebp, esp
mov ebx, [ebp +N]
cmp ebx, 0
jl isFalse

mov eax, [ebp+N1]
cmp eax, [ebp+N]
jge true1 ; if size >= index => jump to label "true"

isFalse:
mov al,0
jmp fin


true1: 
mov ebx, [ebp+N2]
add ebx, [ebp+N]
mov eax,0
mov al, byte ptr[ebx]
sub al, '0'


fin:
mov esp, ebp
pop ebp
pop ebx
ret 12
val endp
;__________________ReverseString______
;the function input:
;1.offset
;2.size
;the function reverse the string
ReverseString proc USES EBP EAX EBX ECX


    push ebp
    mov ebp,esp


;ECX=size
xor ecx,ecx
mov cl, [ebp+24]
sub cl,2

;EAX point to begin the string
mov eax, [ebp+28]

;EBX point to end of the string
mov ebx,eax
add ebx,ecx

;ECX=0
xor ecx,ecx



  ;while (eax!=ebx)
  REVERSE:  cmp eax,ebx
  JA FIN
  
  mov cl,[eax]
  push ecx

  mov cl,[ebx]
  mov [eax],cl

  pop ecx
  mov [ebx],cl

  ;EAX++
  add eax,1

  ;EBX--
  sub ebx,1
  
  jmp REVERSE

FIN:

  mov esp,ebp
	pop ebp
	ret 8
ReverseString endp

;__________________PushFront______
;the function input:
;starting address of string1
;starting address of string2
;the fucntin add string string2 to the front of string1 (string1=string2+string1)
PushFront proc USES EAX EBX EDX
STR2=20
STR1=STR2+4
DYNAMICALLOC=-N

    push ebp
    mov ebp,esp
	SUB esp ,N

	
         ;move str1 to the stack
         mov eax,[EBP+STR1]
         lea ebx,[EBP+DYNAMICALLOC]
loop1:   mov dl,[eax]
         mov [ebx],dl
		 inc eax
		 inc ebx
		 cmp dl,0
		 JNE loop1
	
	     ;move str2 to str1
		 mov eax,[EBP+STR2]
         mov ebx,[EBP+STR1]
loop2:   mov dl,[eax]
         mov [ebx],dl
		  inc eax
		 inc ebx
		 cmp dl,0
		 JNE loop2
		 dec ebx
		 mov dl,32
         mov [ebx],dl
		 inc ebx

		 ;move str1 on back of str2
		 lea eax,[EBP+DYNAMICALLOC]
loop3:   mov dl,[eax]
         mov [ebx],dl
		 inc eax
		 inc ebx
		 cmp dl,0
		 JNE loop3
     
FINISH:  mov esp,ebp
	     pop ebp
	     ret 8

PushFront endp

	end main
