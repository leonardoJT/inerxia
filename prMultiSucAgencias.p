/*
Proposito   : Listar Conciliación Sucursales y Agencias, 
Date        : Feb/15/2008
By          : Félix Vargas
*/

/* {\\172.16.31.149\sfgbancapruebas\prg\Incluido\iprmt_rpt.i} */
{Incluido\iprmt_rpt.i}
{Incluido\Variable.i "SHARED"}

DEFINE VARIABLE qbf-count    AS INTEGER NO-UNDO.
DEFINE VARIABLE qbf-governor AS INTEGER NO-UNDO.
 
DEFINE VARIABLE qbf-govcnt AS INTEGER NO-UNDO.
DEFINE VARIABLE qbf-loop   AS INTEGER NO-UNDO.
DEFINE VARIABLE qbf-time   AS INTEGER NO-UNDO.

DEFINE VARIABLE TT_Db  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
DEFINE VARIABLE TT_Cr  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
DEFINE VARIABLE TC_Db  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
DEFINE VARIABLE TC_Cr  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
DEFINE VARIABLE TTT_Db AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
DEFINE VARIABLE TTT_Cr AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.

DEFINE VARIABLE viAge1   AS INTEGER NO-UNDO.
DEFINE VARIABLE viAge2   AS INTEGER NO-UNDO.
DEFINE VARIABLE viCen1   AS INTEGER NO-UNDO.
DEFINE VARIABLE viCen2   AS INTEGER NO-UNDO.
DEFINE VARIABLE viCpt1   AS INTEGER NO-UNDO.
DEFINE VARIABLE viCpt2   AS INTEGER NO-UNDO.
DEFINE VARIABLE visi     AS INTEGER INITIAL 0 NO-UNDO.
DEFINE VARIABLE vccuenta AS CHARACTER NO-UNDO.

DEFINE BUFFER Mov_Contable FOR Mov_Contable.

ASSIGN
  qbf-count    = 0
  qbf-governor = 0
  qbf-time     = TIME.

ASSIGN 
    viAge1 = INTEGER(pc01)
    viAge2 = INTEGER(pc02)
    viCen1 = INTEGER(pc03)
    viCen2 = INTEGER(pc04)
    viCpt1 = INTEGER(pc05)
    viCpt2 = INTEGER(pc06).

DEFINE VARIABLE vdSaldo   AS DECIMAL  INITIAL 0 NO-UNDO.
DEFINE VARIABLE vdDebito  AS DECIMAL  INITIAL 0 NO-UNDO.
DEFINE VARIABLE vdCredito AS DECIMAL  INITIAL 0 NO-UNDO.
DEFINE VARIABLE vcNomCta  LIKE Cuentas.Nombre INITIAL "".
DEFINE VARIABLE vcNatura  LIKE Cuentas.Naturaleza INITIAL "".
DEFINE VARIABLE vcNomNit  AS CHARACTER FORMAT "X(40)".
DEFINE VARIABLE viconta   AS INTEGER NO-UNDO.
DEFINE VARIABLE viageini  AS INTEGER INITIAL 1  NO-UNDO.
DEFINE VARIABLE viagefin  AS INTEGER INITIAL 55 NO-UNDO.

DEFINE VAR vdeb AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VAR vcre AS DECIMAL INITIAL 0 NO-UNDO.

DEFI TEMP-TABLE tempmov
     FIELD Nit     LIKE mov_contable.Nit
     FIELD Cuenta  LIKE mov_contable.Cuenta
     FIELD Fecha   LIKE mov_contable.fec_contable
     FIELD Agencia LIKE mov_contable.Agencia
     FIELD Centro  LIKE mov_contable.cen_costos
     FIELD Cpte    LIKE mov_contable.Comprobante
     FIELD Usuario LIKE mov_contable.Usuario
     FIELD Dcto    LIKE mov_contable.num_documento
     FIELD Destino LIKE mov_contable.destino
     FIELD DocRefe LIKE mov_contable.doc_referencia
     FIELD Comenta LIKE mov_contable.comentario
     FIELD Debito  AS DECIMAL
     FIELD Credito AS DECIMAL
     INDEX x3 nit cuenta fecha agencia.

/* viAge1           */      
/* viAge2           */      
/* viCen1           */      
/* viCen2           */      
/* viCpt1           */      
/* viCpt2           */      
/* pc07   vcCtaIni  */      
/* pc08   vcCtaFin  */      
/* pc09   vcNitIni  */      
/* pc10   vcNitFin  */      
/* pc11   vcUsuIni  */      
/* pc12   vcUsuFin  */      


EMPTY TEMP-TABLE tempmov.

MESSAGE "Desea Sacar solo los totales de las cuentas?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice AS LOGICAL.

IF (viAge1 EQ 0 AND viAge2 EQ 999) THEN DO:
    FIND LAST agencias NO-LOCK  NO-ERROR.
    IF AVAILABLE(agencias) THEN 
       ASSIGN viageini = 1
              viagefin = agencias.agencia.
END.
ELSE 
  ASSIGN viageini = viAge1
         viagefin = viAge1.


MESSAGE "Agencia : " STRING(viageini,"999") + " - " + STRING(viagefin,"999") SKIP
       "C.Costos: " STRING(viCen1,"999") + " - " + STRING(viCen2,"999") SKIP
       "Cptes.  : " STRING(viCpt1) + " - " + STRING(viCpt2) SKIP
       "Cuentas : " pc07   + " - " + pc08   SKIP
       "Nits    : " pc09   + " - " + pc10   SKIP
       "Usuarios: " pc11   + " - " + pc12   SKIP
       "Fechas  : " STRING(pdt01,"99/99/9999")  + " - " + STRING(pdt02,"99/99/9999")     
   VIEW-AS ALERT-BOX INFO BUTTONS OK.

DO viconta = viageini TO viagefin:
    MESSAGE "Agencia: " viconta
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    FOR EACH Mov_Contable
        WHERE Mov_Contable.Agencia      EQ viconta AND
              Mov_Contable.Fec_Contable GE pdt01 AND Mov_Contable.Fec_Contable LE pdt02 NO-LOCK:
        CREATE tempmov.
        UPDATE tempmov.Nit      =  mov_contable.nit
               tempmov.Cuenta   =  mov_contable.cuenta
               tempmov.Fecha    =  mov_contable.fec_contable
               tempmov.Agencia  =  mov_contable.agencia
               tempmov.Centro   =  mov_contable.cen_costos
               tempmov.Cpte     =  mov_contable.comprobante
               tempmov.Usuario  =  mov_contable.usuario
               tempmov.Dcto     =  mov_contable.num_documento
               tempmov.Destino  =  mov_contable.Destino
               tempmov.DocRefe  =  Mov_Contable.Doc_Referencia
               tempmov.Comenta  =  mov_contable.comentario
               tempmov.Debito   =  mov_contable.db
               tempmov.Credito  =  mov_contable.cr.

        IF tempmov.cuenta = "25301001" AND tempmov.nit = "001"THEN
           ASSIGN vdeb = vdeb + tempmov.Debito
                  vcre = vcre + tempmov.Credito.
    END.
END.
MESSAGE "Debitos : " vdeb
        "Creditos: " vcre
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

RUN ImpReporte.                                                                            

PROCEDURE ImpReporte:
IF P_NomArchivo EQ "DEFAULT" THEN
    ASSIGN P_NomArchivo = W_PathSpl + "ConcilSucAgencias.lst".
OUTPUT TO VALUE(P_NomArchivo) PAGED PAGE-SIZE VALUE(P_NumLineas).

FOR EACH tempmov 
    WHERE tempmov.Cuenta   GE pc07     AND tempmov.Cuenta  LE pc08     AND 
          tempmov.Cpte     GE viCpt1   AND tempmov.Cpte    LE viCpt2   AND 
          tempmov.Agencia  GE viageini AND tempmov.Agencia LE viagefin AND
          tempmov.Fecha    GE pdt01    AND tempmov.Fecha   LE pdt02    AND
          tempmov.Centro   GE viCen1   AND tempmov.Centro  LE viCen2   AND
/*           tempmov.Centro   EQ viCen1   AND */
          tempmov.Usuario  GE pc11     AND tempmov.Usuario LE pc12     AND 
          tempmov.Nit      GE pc09     AND tempmov.Nit     LE pc10       
    BREAK BY tempmov.nit 
          BY tempmov.cuenta
          BY tempmov.fecha 
          BY tempmov.agencia:

/*     IF tempmov.cuenta = "25301001" AND tempmov.nit = "001"THEN  */
/*        ASSIGN vdeb = vdeb + tempmov.Debito                      */
/*               vcre = vcre + tempmov.Credito.                    */

    IF FIRST-OF(tempmov.Nit) THEN DO:
       FIND Clientes WHERE Clientes.Nit EQ tempmov.Nit NO-LOCK NO-ERROR.
       ASSIGN vcNomNit = "No Existe Cliente: " + STRING(tempmov.Nit).
       IF AVAILABLE Clientes THEN 
          ASSIGN vcNomNit = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
       DISPLAY tempmov.Nit AT 1 FORMAT "X(14)" 
               vcNomNit         AT 20
               "------------------------------------------------------------------------------------------------------------------------" AT 1
       WITH FRAME F_Nit WIDTH 132 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS STREAM-IO.
    END.
    IF FIRST-OF(tempmov.Cuenta) THEN DO:
       ASSIGN vcNomCta = "Cta No Existe".
       FIND Cuentas WHERE Cuentas.Cuenta EQ tempmov.Cuenta NO-LOCK NO-ERROR.
        IF AVAILABLE Cuentas THEN 
          ASSIGN vcNomCta = Cuentas.Nombre.
    END.
    IF NOT choice THEN
      DISPLAY tempmov.Fecha          AT 2   FORMAT "99/99/9999"
              tempmov.Cpte           AT 14  FORMAT "99"
              tempmov.Dcto           AT 18  FORMAT "9999999"
              tempmov.Destino        AT 28  FORMAT "999"
              tempmov.DocRefe        AT 32  FORMAT "X(10)"
              tempmov.Comenta        AT 45  FORMAT "X(35)"
              tempmov.Debito         AT 84  FORMAT ">>,>>>,>>>,>>9"
              tempmov.Credito        AT 100 FORMAT ">>,>>>,>>>,>>9"
              tempmov.Usuario        AT 117 FORMAT "X(4)"
      WITH DOWN FRAME F_Mov WIDTH 132 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.
    ASSIGN TT_Db = TT_Db   + tempmov.Debito
           TT_Cr = TT_Cr   + tempmov.Credito
           TC_Db = TC_Db   + tempmov.Debito
           TC_Cr = TC_Cr   + tempmov.Credito
           TTT_Db = TTT_Db + tempmov.Debito
           TTT_Cr = TTT_Cr + tempmov.Credito.
    IF LAST-OF(tempmov.Cuenta) THEN DO:
        IF NOT choice THEN
           DISPLAY SKIP(1)
                  /*"__________________________________________________________________________________________________________" AT 14*/
                  "TotCuenta:"        AT 14 
                  tempmov.Cuenta      AT 25 FORMAT "X(14)" 
                  vcNomCta            AT 45 FORMAT "X(35)"
                  TC_Db               AT 84  FORMAT ">>,>>>,>>>,>>9"
                  TC_Cr               AT 100 FORMAT ">>,>>>,>>>,>>9"
                  "----------------------------------------------------------------------------------------------------------" AT 14
          WITH FRAME F_TCta WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
        ELSE
           DISPLAY SKIP(1)
                  "TotCuenta:"        AT 14 
                  tempmov.Cuenta      AT 25 FORMAT "X(14)" 
                  vcNomCta            AT 45 FORMAT "X(35)"
                  TC_Db               AT 84  FORMAT ">>,>>>,>>>,>>9"
                  TC_Cr               AT 100 FORMAT ">>,>>>,>>>,>>9"
          WITH FRAME F_TCta2 WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
        ASSIGN TC_Cr = 0 TC_Db = 0.
    END.
    IF LAST-OF(tempmov.Nit) THEN DO:
        DISPLAY SKIP(1)
                "__________________________________________________________________________________________________________" AT 14
                "TotNit:"           AT 14 
                tempmov.Nit         AT 25  FORMAT "X(14)" 
                vcNomNit            AT 45  FORMAT "X(35)"
                TT_Db               AT 84  FORMAT ">>,>>>,>>>,>>9"
                TT_Cr               AT 100 FORMAT ">>,>>>,>>>,>>9" SKIP(2)
        WITH FRAME F_TNit WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
        ASSIGN TT_Cr = 0 TT_Db = 0.
    END.
END.   /* Fin FOR */
DISPLAY SKIP(1)
        "__________________________________________________________________________________________________________" AT 14
        "Total :"        AT 14 
        TTT_Db               AT 84  FORMAT ">>,>>>,>>>,>>9"
        TTT_Cr               AT 100   FORMAT ">>,>>>,>>>,>>9" SKIP(2)
WITH FRAME F_TTT WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
ASSIGN TTT_Cr = 0 TTT_Db = 0.
PAGE.
OUTPUT CLOSE.
/* VIEW FRAME F-Ftr. */

MESSAGE "Debitos : " vdeb
        "Creditos: " vcre
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.
