{Incluido\pdfglobal.i "NEW SHARED"}

/* PEKI - Find the handle if on the air */
&IF "{1}" = "THIS-PROCEDURE" &THEN
    h_PDFinc = THIS-PROCEDURE. 

    DO WHILE VALID-HANDLE(h_PDFinc)
    AND h_PDFinc:PRIVATE-DATA <> 'Persistent PDFinc':
       h_PDFinc = h_PDFinc:NEXT-SIBLING.
    END.

&ELSEIF "{1}" = "" &THEN
     h_PDFinc = SESSION:FIRST-PROCEDURE.

     DO WHILE VALID-HANDLE(h_PDFinc)
     AND h_PDFinc:PRIVATE-DATA <> 'Persistent PDFinc':
         h_PDFinc = h_PDFinc:NEXT-SIBLING.
     END.
&ENDIF

IF NOT VALID-HANDLE(h_PDFinc) THEN DO:

  /* Call pdf_inc.p Persistenly */
  RUN Incluido\pdf_inc.p PERSISTENT 
                SET h_PDFinc.

  ASSIGN h_PDFinc:PRIVATE-DATA = 'Persistent PDFinc'.

  &IF NOT PROVERSION BEGINS "8" &THEN
    &IF "{1}" = "THIS-PROCEDURE" &THEN
      IF VALID-HANDLE(THIS-PROCEDURE) THEN THIS-PROCEDURE:ADD-SUPER-PROCEDURE(h_PDFinc).
    &ELSEIF "{1}" = "" &THEN
      IF VALID-HANDLE(SESSION) THEN SESSION:ADD-SUPER-PROCEDURE(h_PDFinc).
    &ENDIF
  &ENDIF
END. /* If Not a valid Handle to PDFinclude */

/* ------------------------ Pre-Define Functions -------------------------- */

{Incluido\pdf_func.i h_PDFinc}

/* --------------------- End of Pre-Define Functions ---------------------- */
