DEFINE VARIABLE vdSaldo   AS DECIMAL  INITIAL 0 NO-UNDO.
DEFINE VARIABLE vdDebito  AS DECIMAL  INITIAL 0 NO-UNDO.
DEFINE VARIABLE vdCredito AS DECIMAL  INITIAL 0 NO-UNDO.
DEFINE VARIABLE vcNomCta  LIKE Cuentas.Nombre INITIAL "".
DEFINE VARIABLE vcNatura  LIKE Cuentas.Naturaleza INITIAL "".

DEFINE VARIABLE viagen    LIKE mov_contable.agencia       INITIAL 0.
DEFINE VARIABLE vifecha   LIKE mov_contable.fec_contable  INITIAL ?.
DEFINE VARIABLE vicpte    LIKE mov_contable.comprobante   INITIAL 0.
DEFINE VARIABLE vinume    LIKE mov_contable.num_documento INITIAL 0.
DEFINE VARIABLE vibandera AS INTEGER INITIAL 0.

DEFINE TEMP-TABLE tempmov
 FIELD agencia        LIKE mov_contable.agencia
 FIELD comprobante    LIKE mov_contable.comprobante
 FIELD fec_contable   LIKE mov_contable.fec_contable
 FIELD num_documento  LIKE mov_contable.num_documento
 FIELD db             LIKE mov_contable.db
 FIELD cr             LIKE mov_contable.cr
 INDEX idxtodo agencia fec_contable comprobante num_documento.

DEFI TEMP-TABLE TempCtas
     FIELD Agen    LIKE mov_contable.Agencia
     FIELD Fecha   LIKE mov_contable.fec_contable
     FIELD Cpte    LIKE mov_contable.Comprobante
     FIELD Dcto    LIKE mov_contable.num_documento
     FIELD Debito  AS DECIMAL
     FIELD Credito AS DECIMAL
     FIELD Saldo   AS DECIMAL
     INDEX x3 agen fecha Cpte Dcto.

FOR EACH Mov_Contable field(agencia comprobante fec_contable num_documento db cr) WHERE
/*     mov_contable.agencia = 1 AND */
    Mov_Contable.Fec_Contable >= date("01/01/2007") AND Mov_Contable.Fec_Contable <= date("31/12/2007") 
     NO-LOCK BY mov_contable.agencia: /*BY mov_contable.comprobante BY mov_contable.num_documento /*BY mov_contable.comprobante*/ :*/
    IF vibandera = 0  THEN DO:
       ASSIGN viagen  = mov_contable.agencia.
       ASSIGN vibandera = 1.
    END.
    IF viagen NE mov_contable.agencia THEN DO:
       DISPLAY viagen.
       ASSIGN viagen  = mov_contable.agencia.
    END.

    CREATE tempmov.
    UPDATE tempmov.agencia       =  mov_contable.agencia      
           tempmov.comprobante   =  mov_contable.comprobante  
           tempmov.fec_contable  =  mov_contable.fec_contable 
           tempmov.num_documento =  mov_contable.num_documento
           tempmov.db            =  mov_contable.db           
           tempmov.cr              =  mov_contable.cr.

/*     GROUP BY Mov_Contable.agencia     BY Mov_Contable.fec_contable   */
/*           BY Mov_Contable.Comprobante BY Mov_Contable.Num_Documento: */
/*    DISPLAY mov_contable.fec_contable.*/
END.

MESSAGE "Va a Empezar.."
     VIEW-AS ALERT-BOX INFO BUTTONS OK.
FOR EACH tempmov BREAK BY tempmov.agencia  BY tempmov.fec_contable
                 BY tempmov.Comprobante    BY tempmov.Num_Documento:

    IF FIRST-OF(tempmov.Agencia) OR FIRST-OF(tempmov.fec_contable) OR 
       FIRST-OF(tempmov.Comprobante) OR FIRST-OF(tempmov.num_documento) THEN
       ASSIGN vdSaldo   = 0
              vdDebito  = 0
              vdCredito = 0.
    
    ASSIGN /*vdSaldo   = Mov_Contable.Cr - Mov_Contable.Db*/
           vdDebito  = vdDebito  + tempmov.Db
           vdCredito = vdCredito + tempmov.Cr.
    
    IF LAST-OF(tempmov.Agencia) OR LAST-OF(tempmov.fec_contable) OR
       LAST-OF(tempmov.Comprobante) OR LAST-OF(tempmov.num_documento) THEN DO:
          ASSIGN vdSaldo = vdCredito - vdDebito.
       IF vdSaldo NE 0 THEN DO:
          CREATE TempCtas.
          UPDATE TempCtas.Agen    = tempmov.agencia
                 TempCtas.Fecha   = tempmov.fec_contable
                 TempCtas.Cpte    = tempmov.Comprobante
                 TempCtas.Dcto    = tempmov.num_documento
                 TempCtas.Debito  = vdDebito
                 TempCtas.Credito = vdCredito
                 TempCtas.Saldo   = vdSaldo.
       END.
    END.
END.
RUN PValida_Salida NO-ERROR.

PROCEDURE PValida_Salida:
    
/*     IF P_NomArchivo EQ "DEFAULT" THEN                                 */
/*         ASSIGN P_NomArchivo = W_PathSpl + "CptesDescuadrados.txt".    */
/*                                                                       */
/*     OUTPUT TO VALUE(P_NomArchivo) PAGED PAGE-SIZE VALUE(P_NumLineas). */
OUTPUT TO "c:\info_juriscoop\cptedescuadre2.txt".
FOR EACH TempCtas: /* BY Agen BY Fecha BY Cpte BY Dcto:*/
    FORM
        TempCtas.Agen    COLUMN-LABEL "Age"         FORMAT "999"
        TempCtas.Fecha   COLUMN-LABEL "Fecha"       FORMAT "99/99/9999"
        TempCtas.Cpte    COLUMN-LABEL "Comprobante" FORMAT "999"
        TempCtas.Dcto    COLUMN-LABEL "Documento"   FORMAT "999999999"
        TempCtas.Debito  COLUMN-LABEL "Debito"      FORMAT "->>>,>>>,>>>,>>9.99"
        TempCtas.Credito COLUMN-LABEL "Credito"     FORMAT "->>>,>>>,>>>,>>9.99"
        TempCtas.Saldo   COLUMN-LABEL "Diferencia"  FORMAT "->>>,>>>,>>>,>>9.99"
        WITH FRAME FCptDesc DOWN COLUMN 1 WIDTH 160
            NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
    DISPLAY 
        TempCtas.Agen   
        TempCtas.Fecha 
        TempCtas.Cpte
        TempCtas.Dcto  
        TempCtas.Debito 
        TempCtas.Credito
        TempCtas.Saldo  
      WITH FRAME FCptDesc.
    DOWN WITH FRAME FCptDesc.
END.
VIEW FRAME F-Ftr.
/*END.*/
OUTPUT CLOSE.
END PROCEDURE.
RETURN.
