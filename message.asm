; «««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««
; This file manages messages displayed to the user
; Copyright David B. Sher 2011
; «««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««
; ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
    include \masm32\include\masm32rt.inc
    include calcDisplay.inc
; ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

.data
messageBuffer DWORD ?
messageInitialized DWORD FALSE
EXTERN hDlg : DWORD  ; get window from main file

.code

; clears the message window
clearMessages PROC USES eax
    cmp messageInitialized,FALSE
    je  notInitialized
    ;free$(messageBuffer)  ; free up the curren memory used in the message buffer
notInitialized: ; don't free if not initialized
    mov messageBuffer,chr$(13,10) ; move an empty string into the message buffer
    ;invoke SetDlgItemText,hDlg,MESSAGE,messageBuffer
clearMessages ENDP

addMessage PROC USES eax ebx, toAdd :PTR BYTE
    
    mov messageBuffer,cat$(cat$(messageBuffer,toAdd),chr$(13,10))
    invoke SetDlgItemText,hDlg,MESSAGE,messageBuffer
addMessage ENDP
end