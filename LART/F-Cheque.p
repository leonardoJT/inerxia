/*******************************************************************
 PROCEDIMIENTO: cheque
 *******************************************************************/
DEFINE INPUT PARAMETER P_Monto1  AS CHAR FORMAT "X(70)".
DEFINE INPUT PARAMETER P_Monto2  AS CHAR FORMAT "X(90)".
DEFINE INPUT PARAMETER P_Nombre  AS CHAR FORMAT "X(90)".
DEFINE INPUT PARAMETER P_Ciudad  AS CHAR FORMAT "X(12)".
DEFINE INPUT PARAMETER P_Valor   AS DECIMAL FORMAT "*,***,***,***.99".
DEFINE INPUT PARAMETER P_Coment  AS CHAR FORMAT "X(60)".
/*DEFINE INPUT PARAMETER wxNit     LIKE taquilla.nit.*/
DEFINE INPUT PARAMETER wxNroDoc  LIKE taquilla.num_documento.
/*DEFINE INPUT PARAMETER wxCta     LIKE taquilla.cuenta.*/
DEFINE INPUT PARAMETER wxNat     LIKE taquilla.naturaleza.
/*DEFINE INPUT PARAMETER wxCtaCon  LIKE taquilla.cuenta.*/
DEFINE INPUT PARAMETER wxDocRet  LIKE taquilla.num_documento.

/*DEFINE INPUT PARAMETER P_cedula  AS CHAR FORMAT "X(12)".*/

DEFINE VARIABLE Ano AS character FORMAT "XX".
DEFINE VARIABLE P_Nom2 LIKE P_Nombre.
DEFINE VARIABLE P_Val2 LIKE P_Valor.
DEFINE VARIABLE P_Val3 LIKE P_Valor EXTENT 8.
DEFINE VARIABLE P_Val4 LIKE P_Valor.
/* DEFINE VARIABLE wxnit1 LIKE wxnit. */
/* DEFINE VARIABLE wxnit2 LIKE wxnit. */
DEFINE VARIABLE wxDC1 AS CHARACTER FORMAT "X" EXTENT 8 INITIAL "1" .
DEFI   VAR      Ctas  LIKE Cuentas.Cuenta     EXTENT 8.
DEFI   VAR      Ceds  LIKE Mov_Contable.Nit   EXTENT 8.
DEFI   VAR      DocS  LIKE Mov_Contable.Doc_Referen EXTENT 8.
DEFI   VAR      NN      AS INTEG FORM "99".
DEFI   VAR      JJ      AS INTEG FORM "99".
DEFI   VAR      W_VlrS0 AS CHAR FORM "X(12)".
DEFI   VAR      W_Aster AS CHAR FORM "X(14)".
DEFI   VAR      NChar   AS INTEG FORM "999".

DEFINE VARIABLE wxdc2 AS CHARACTER FORMAT "X" INITIAL "2".

DEFINE VAR P_Tit1 AS CHARACTER FORMAT "X(18)" INITIAL " ".
DEFINE VAR P_Tit2 AS CHARACTER FORMAT "X(18)" INITIAL " ".
DEFINE VAR procname  AS CHARACTER FORMAT "X(132)" INITIAL "temporal.txt".
DEFINE VAR W_Rpta             AS   LOGICAL. 

 {Incluido\VARIABLE.I "SHARED"}

ASSIGN Ano     = substring(string(year(today)),1,4)
       P_Val2  = P_Valor
       P_Val3  = P_Valor
       P_Val4  = P_Valor
       P_Nom2  = P_Nombre.
      /*wxnit1  = wxnit
       wxnit2  = wxnit.*/
 IF WXNAT EQ "CR" THEN ASSIGN WXDC1 = "2"
                              WXDC2 = "1".
 
ASSIGN P_Nombre = TRIM(P_Nombre) + " **************************"
       W_VlrS0  = TRIM(STRING(P_Valor,">>>>>>>>>9.99")) .
      /* W_Aster  = "**" + TRIM(W_VlrS0).*/

IF TRIM(P_Monto2)  = "" THEN DO:
   ASSIGN P_Monto1 = W_Aster + " " + TRIM(P_Monto1) + " ****"
          P_Monto2 = "********************************************************".
          NChar    = LENGTH(P_Monto1).
   
   IF NChar LT 60 THEN DO:
      NChar = 60 - NChar.

      DO JJ = 1 TO NChar:
         P_Monto1 = P_Monto1 + "*".
      END.
   END.
END.
ELSE
   ASSIGN P_Monto1 = W_Aster + " " + TRIM(P_Monto1)
          P_Monto2 = TRIM(P_Monto2) + "*****".

NN = 1.
FOR EACH Mov_Contable WHERE Mov_Contable.Agencia         EQ 1
                            AND Mov_Contable.Comprobante EQ INTEG(wxNroDoc)
                            AND Mov_Contable.Num_Docum   EQ INTEG(wxDocRet)
                            AND Mov_Contable.Fec_Contab  EQ W_Fecha NO-LOCK
                             BY Mov_Contable.Cuenta:
    IF SUBSTRING(Mov_Contable.Cuenta,1,4) EQ "1110" THEN
       NEXT.

    ASSIGN WXDC1[NN]  = "1"
           P_Val3[NN] = Mov_Contable.Db
           Ctas[NN]   = Mov_Contable.Cuenta
           Ceds[NN]   = Mov_Contable.Nit
           Docs[NN]   = Mov_Contable.Doc_Referen.

    IF Mov_Contable.Cr GT 0 THEN 
       ASSIGN WXDC1[NN]  = "2"
              P_Val3[NN] = Mov_Contable.Cr.

    NN = NN + 1.
    IF NN GT 6 THEN
       LEAVE.
END.

FIND FIRST Mov_Contable WHERE Mov_Contable.Agencia       EQ 1
                            AND Mov_Contable.Comprobante EQ INTEG(wxNroDoc)
                            AND Mov_Contable.Num_Docum   EQ INTEG(wxDocRet)
                            AND Mov_Contable.Fec_Contab  EQ W_Fecha
                            AND SUBSTRING(Mov_Contable.Cuenta,1,4) EQ "1110" NO-LOCK NO-ERROR.
IF AVAIL(Mov_Contable) THEN
   ASSIGN WXDC1[NN]  = "2"
          P_Val3[NN] = P_Valor
          Ctas[NN]   = Mov_Contable.Cuenta
          Ceds[NN]   = Mov_Contable.Nit
          Docs[NN]   = Mov_Contable.Doc_Referen.

/*FOR EACH Mov_Contable WHERE Mov_Contable.Agencia         EQ 1
                            AND Mov_Contable.Comprobante EQ INTEG(wxNroDoc)
                            AND Mov_Contable.Num_Docum   EQ INTEG(wxDocRet)
                            AND Mov_Contable.Fec_Contab  EQ W_Fecha NO-LOCK
                             BY Mov_Contable.Cuenta:
    IF SUBSTRING(Mov_Contable.Cuenta,1,4) NE "1110" THEN
       NEXT.

    ASSIGN WXDC1[NN]  = "2"
           P_Val3[NN] = P_Val3[NN] + Mov_Contable.Cr
           Ctas[NN]   = Mov_Contable.Cuenta
           Ceds[NN]   = Mov_Contable.Nit
           Docs[NN]   = Mov_Contable.Doc_Referen.
END.*/

  DEFINE VARIABLE Cheque AS CHARACTER FORMAT "X(50)".
  Cheque = W_PathSpl + "F_Cheque.Lst".

  DEFINE VAR W_Dispositivo AS CHARACTER INITIAL "P".  
  RUN P-DisPos IN W_Manija (INPUT-OUTPUT Cheque, INPUT-OUTPUT W_Dispositivo).
  IF W_Dispositivo = "" THEN
     RETURN.
  IF W_Dispositivo = "I" THEN
   DO:
     Cheque = "Printer".
     SYSTEM-DIALOG PRINTER-SETUP.
   END.





  OUTPUT TO VALUE(Cheque) NO-ECHO PAGED PAGE-SIZE 42.
  /*PUT CONTROL CHR(27) + "T" + CHR(40).*/
  IF P_Valor GT 0 THEN DO:
     DISPLAY 
      Ano                          AT ROW 5 COL 34 FORMAT "X(4)" 
      STRING(MONTH(TODAY),">9")    AT ROW 5 COL 40 FORMAT "X(2)"
      STRING(DAY(TODAY),">9")      AT ROW 5 COL 44 FORMAT "X(2)"
      P_Valor                      AT ROW 5 COL 50              
      P_Nombre                     AT ROW 7 COL 7               
      P_Monto1                     AT ROW 9 COL 7              
      P_Monto2                     AT ROW 11 COL 1              
     /* TRIM(W_Aster) + TRIM(W_Aster) + TRIM(W_Aster)  AT ROW 15 COL 26 FORM "X(45)"*/
       "*"                       AT ROW 16 COL 1
      P_Coment                   AT ROW 25 COL 2
      P_Nom2                     AT ROW 26 COL 7
/*    wxnit                      AT ROW 26 COL 68 */
/*    P_Val2                     AT ROW 27 COL 7   */
      TODAY                      AT ROW 27 COL 32
      wxDocRet                   AT ROW 28 COL 7
      Ctas[1]                    AT ROW 30 COL 2
      Ceds[1]                    AT ROW 30 COL 26
      Docs[1]                    AT ROW 30 COL 38
      wxdc1[1]                   AT ROW 30 COL 46
      P_Val3[1]                  AT ROW 30 COL 48
      Ctas[2]                    AT ROW 31 COL 2 
      Ceds[2]                    AT ROW 31 COL 26
      Docs[2]                    AT ROW 31 COL 38
      wxdc1[2]                   AT ROW 31 COL 46
      P_Val3[2]                  AT ROW 31 COL 48
      Ctas[3]                    AT ROW 32 COL 2          WHEN Ctas[3] NE ?
      Ceds[3]                    AT ROW 32 COL 26         WHEN Ctas[3] NE ?
      Docs[3]                    AT ROW 32 COL 38         WHEN Ctas[3] NE ?
      wxdc1[3]                   AT ROW 32 COL 46         WHEN Ctas[3] NE ?
      P_Val3[3]                  AT ROW 32 COL 48         WHEN Ctas[3] NE ?
      Ctas[4]                    AT ROW 33 COL 2          WHEN Ctas[4] NE ?
      Ceds[4]                    AT ROW 33 COL 26         WHEN Ctas[4] NE ?
      Docs[4]                    AT ROW 33 COL 38         WHEN Ctas[4] NE ?
      wxdc1[4]                   AT ROW 33 COL 46         WHEN Ctas[4] NE ?
      P_Val3[4]                  AT ROW 33 COL 48         WHEN Ctas[4] NE ?
      Ctas[5]                    AT ROW 34 COL 2          WHEN Ctas[5] NE ?
      Ceds[5]                    AT ROW 34 COL 26         WHEN Ctas[5] NE ?
      Docs[5]                    AT ROW 34 COL 38         WHEN Ctas[5] NE ?
      wxdc1[5]                   AT ROW 34 COL 46         WHEN Ctas[5] NE ?
      P_Val3[5]                  AT ROW 34 COL 48         WHEN Ctas[5] NE ?
      Ctas[6]                    AT ROW 35 COL 2          WHEN Ctas[6] NE ?
      Ceds[6]                    AT ROW 35 COL 26         WHEN Ctas[6] NE ?
      Docs[6]                    AT ROW 35 COL 38         WHEN Ctas[6] NE ?
      wxdc1[6]                   AT ROW 35 COL 46         WHEN Ctas[6] NE ?
      P_Val3[6]                  AT ROW 35 COL 48         WHEN Ctas[6] NE ?
      Ctas[7]                    AT ROW 36 COL 2          WHEN Ctas[7] NE ?
      Ceds[7]                    AT ROW 36 COL 26         WHEN Ctas[7] NE ?
      Docs[7]                    AT ROW 36 COL 38         WHEN Ctas[7] NE ?
      wxdc1[7]                   AT ROW 36 COL 46         WHEN Ctas[7] NE ?
      P_Val3[7]                  AT ROW 36 COL 48         WHEN Ctas[7] NE ?
      Ctas[8]                    AT ROW 36 COL 2          WHEN Ctas[8] NE ?
      Ceds[8]                    AT ROW 36 COL 26         WHEN Ctas[8] NE ?
      Docs[8]                    AT ROW 36 COL 38         WHEN Ctas[8] NE ?
      wxdc1[8]                   AT ROW 36 COL 46         WHEN Ctas[8] NE ?
      P_Val3[8]                  AT ROW 36 COL 48         WHEN Ctas[8] NE ?

    WITH PAGE-TOP FRAME  F-cheq width 120 NO-LABELS NO-BOX USE-TEXT.
   PAGE.
     /*WITH DOWN FRAME  F-cheq width 120 NO-LABELS NO-BOX USE-TEXT STREAM-IO.*/

     /* wxCta                      AT ROW 30 COL 2
      wxnit1                     AT ROW 30 COL 26
      wxDocRet                   AT ROW 30 COL 38
      wxdc1                      AT ROW 30 COL 46 
      P_Val3                     AT ROW 30 COL 48

      wxCtaCon                   AT ROW 31 COL 2
      wxnit2                     AT ROW 31 COL 26
      wxNroDoc                   AT ROW 31 COL 38
      wxdc2                      AT ROW 31 COL 46 
      P_Val4                     AT ROW 31 COL 48
      WITH DOWN FRAME  F-cheq width 120 NO-LABELS NO-BOX USE-TEXT STREAM-IO.*/
  END.
  ELSE DO:
    DISPLAY 
       "*"                        AT ROW 16 COL 1
    WITH PAGE-TOP FRAME  F-cheq width 120 NO-LABELS NO-BOX USE-TEXT.
   END.

OUTPUT CLOSE.
  IF W_Dispositivo = "P" THEN  
    RUN Pantalla IN W_Manija (INPUT Cheque).
  IF W_Dispositivo <> "A" THEN
    OS-DELETE VALUE(Cheque).

/*output to c:\INFO_cooprudea\cheque1.txt.
PUT CONTROL chr(27) + "@". 
PUT CONTROL chr(27) + "f" + chr(1) + chr(37) + chr(10). 
PUT CONTROL chr(27) + "!" + chr(1). 
PUT CONTROL chr(27) + "c" + "0" + chr(4).
PUT CONTROL chr(27) + "L".
PUT CONTROL chr(27) + "V" + chr(1).
PUT CONTROL chr(27) + "T" + chr(51).

PUT SKIP(1) STRING(YEAR(TODAY) - 1900)   AT 46 FORMAT "X(2)"
            STRING(MONTH(TODAY),">9")  AT 50 FORMAT "X(2)"
            STRING(DAY(TODAY),">9")  AT 54 FORMAT "X(2)"
            P_Valor AT 62 "***" AT 77.
PUT SKIP P_Nombre AT 8.
PUT SKIP(1) P_Monto1 AT 10 SKIP(1) P_Monto2 AT 3.

PUT CONTROL chr(12) + chr(12) + chr(0) + chr(13) + chr(0).
 */
