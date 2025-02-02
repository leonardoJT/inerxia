DEFINE INPUT PARAMETER P_FtoCheq AS INTEGER FORMAT "9". /*0 Formato, 1 Cheque*/
DEFINE INPUT PARAMETER P_Agencia AS CHARACTER FORMAT "X(3)".
DEFINE INPUT PARAMETER P_TipPro AS INTEGER FORMAT "9".
DEFINE INPUT PARAMETER P_AhoVta AS INTEGER FORMAT "9".       /*0 = A la Vista Valida, 1 = El resto Tirilla Auditoria*/
DEFINE INPUT PARAMETER P_Nit AS CHARACTER FORMAT "X(12)".
DEFINE INPUT PARAMETER P_SAnt AS DECIMAL FORMAT "****,***,**9.99".
DEFINE INPUT PARAMETER P_SAct AS DECIMAL FORMAT "****,***,**9.99".
DEFINE INPUT PARAMETER P_AboK AS DECIMAL FORMAT "****,***,**9.99".
DEFINE INPUT PARAMETER P_AboI AS DECIMAL FORMAT "****,***,**9.99".
DEFINE INPUT PARAMETER P_Cuenta AS CHARACTER FORMAT "X(14)".
DEFINE INPUT PARAMETER P_Descrip AS CHARACTER FORMAT "X(30)".
DEFINE INPUT PARAMETER P_Benef AS CHARACTER FORMAT "X(30)".
DEFINE INPUT PARAMETER P_NomPto AS CHARACTER FORMAT "X(26)".
DEFINE INPUT PARAMETER P_NroTx AS INTEGER FORMAT "99999999".
DEFINE INPUT PARAMETER P_Valor AS DECIMAL FORMAT "****,***,**9.99".
DEFINE INPUT PARAMETER P_VrCheq AS DECIMAL FORMAT "****,***,**9.99".
DEFINE INPUT PARAMETER P_ICheque AS CHARACTER FORMAT "X(30)".
DEFINE INPUT PARAMETER P_OtrosC AS DECIMAL FORMAT "****,***,**9.99".  /*Prog.Ejecutable para el Cheque*/

{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I "SHARED"}

DEFINE VAR SwImp AS LOGICAL.
DEFINE VAR fechaHora AS CHARACTER FORMAT "X(38)".
DEFINE VAR idUsuario AS CHARACTER FORMAT "X(30)" INITIAL " ".
DEFINE VAR xx AS CHARACTER FORMAT "X(8)".
DEFINE VAR salida AS CHARACTER FORMAT "X(50)".
DEFINE VAR vcAsociado AS CHARACTER FORMAT "X(80)".

fechaHora = "Fecha y Hora: " + STRING(W_Fecha,"99/99/9999") + " - " + STRING(TIME,"HH:MM:SS AM").
idUsuario = "Cajero: " + STRING(W_Usuario).

PROCEDURE ImpFto:
/*IF P_AhoVta NE 0 THEN DO:    /*Menos A la Vista, El resto son en esta Tirilla Auditoria*/
       OUTPUT TO PRINTER LPT1.  /* Com1. */               /*PRINTER.*/

       PUT CONTROL CHR(27) + "@". 
       PUT CONTROL CHR(27) + "SP".
       PUT CONTROL CHR(27) + "!" + CHR(1). 
       PUT CONTROL CHR(27) + "T".
      
       /*PUT "COOPERATIVA DE PROFESORES DE LA UNIVERSIDAD DE ANTIOQUIA" SKIP.
       PUT "                 NIT Nro.890985032-1"           SKIP(1).*/
       PUT "Asociado :"  P_Nit                       SKIP.
       PUT P_Benef                                   SKIP.
       PUT "Pcto     :"  P_NomPto                    SKIP.
       PUT "No.Cuenta: "  P_Agencia "-" P_Cuenta     SKIP.
       PUT "Saldo Anterior : " P_SAnt                SKIP(1).
       PUT "Desc." P_Descrip                         SKIP.

       IF P_TipPro EQ 2 THEN DO:
          PUT "Abono Intereses: " P_AboI             SKIP.
          PUT "Abono Capital  : " P_AboK             SKIP.
          IF P_OtrosC GT 0 THEN
             PUT "Seg.Ca/OtrosCar: " P_OtrosC        SKIP.
          P_Valor = P_AboI + P_AboK + P_OtrosC.
       END.
       ELSE PUT SKIP(2).

       PUT "Saldo Actual   : " P_SAct                SKIP(1).

       IF P_TipPro EQ 0 OR P_TipPro EQ 4 THEN DO:
          PUT "Valor : " P_Valor SKIP.
          PUT "Id_Cajero : " W_Usuario.
       END.
       ELSE IF P_TipPro EQ 2 OR (P_TipPro EQ 1 AND P_ICheque EQ "ConsigAho") THEN DO:
          PUT "Vlr.Pagado:" P_Valor SKIP.
          PUT idUsuario.
       END.
       ELSE IF P_TipPro EQ 1 THEN DO:
          PUT "Valor     : " P_Valor    SKIP.
          PUT "Id_Cajero : " W_Usuario.          
       END.

       PUT SKIP(1).
       PUT " No.Trans." P_NroTx fechaHora        SKIP.
       PUT "NOTA: EL SALDO SOLO SE ENTREGA"     SKIP.
       PUT "AL TITULAR O PERSONA DEBIDAMENTE"   SKIP.
       PUT "AUTORIZADA POR ESCRITO." SKIP.

       PUT SKIP(6).
       PUT SKIP(6).
       
       PUT " Firma y Sello:" SKIP.
       PUT SKIP(6).
       PUT SKIP(3).
       
       
       PUT CONTROL CHR(12).  
       PUT CONTROL CHR(27) + "q".  

       OUTPUT CLOSE.

       RETURN.   /*Retorna si fu� por tirilla*/
    END.*/

  /*Continua solo por Validadora para A la Vista*/
    /*
    MESSAGE "Inserte Documento Para Validar la Operaci�n " SKIP
            "Del Producto : " P_NomPto                     SKIP
            "Si el Documento SI Visualiza SALDO teclee.........SI" SKIP
            "Para NO Visualizar SALDO teclee..........NO"
             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "CONFIRMAR IMPRESI�N"
             UPDATE W_SiNos AS LOGICAL.
    IF NOT W_SiNos THEN P_SAct = 0.    */
            
            xx = TRIM(STRING(INT(w_usuario))) + "_" + STRING(TIME).
         salida = W_PathSpl + "tq" + xx + ".txt".
          /* MESSAGE  salida VIEW-AS ALERT-BOX INFO BUTTON OK. */
       OUTPUT TO  value(salida).
 
    REPEAT:

    /* OUTPUT TO PRINTER LPT1. /*COM1.*/ /*PRINTER. */
       PUT CONTROL CHR(27) + "@". 
       PUT CONTROL CHR(27) + "f" + CHR(1) + CHR(57) + CHR(10).
       PUT CONTROL CHR(27) + "!" + CHR(1). 
       PUT CONTROL CHR(27) + "c" + "0" + CHR(4).
    /* PUT CONTROL CHR(27) + "L".*/
       PUT CONTROL CHR(27) + "V" + CHR(1).
    /* PUT CONTROL CHR(27) + "T" + CHR(12).*/   /*tenia CHR(48), Tama�o p�gina*/
    /* PUT CONTROL CHR(27) + "c" + CHR(52).*/*/
       PUT SKIP(4).
     /*PUT "COOPERATIVA DE PROFESORES DE LA UNIVERSIDAD DE ANTIOQUIA" SKIP.
       PUT "                 NIT Nro.890985032-1"           SKIP(1).*/
        
        /*GCamacho - Feb/26/2011 - MOdificar formato para impresora*/
        
/*        PUT "No.Trans. "  P_NroTx fechaHora         SKIP.                           */
/*        PUT "Nit:      "  P_Nit                    SKIP.                           */
/*        PUT "Asociado: "  P_Benef                  SKIP.                           */
/*        PUT "Pcto    : "  P_NomPto                 SKIP.                           */
/*        PUT "No.Cuenta:"  P_Agencia " - " P_Cuenta SKIP.                           */
/*        PUT "Desc.: " P_Descrip                    SKIP.                           */
/*        IF P_TipPro EQ 0 OR P_TipPro EQ 4 THEN DO:                                 */
/*           PUT "Valor : " P_Valor SKIP.                                    */
/*           PUT "Id_Cajero : " W_Usuario   SKIP.                                    */
/*        END.                                                                       */
/*        ELSE                                                                       */
/*        IF P_TipPro EQ 2 OR (P_TipPro EQ 1 AND P_ICheque EQ "ConsigAho") THEN DO:  */
/*           PUT "Vlr.Pagado:" P_Valor  SKIP.                                */
/*           PUT idUsuario                        SKIP.                                */
/*        END.                                                                       */
/*        ELSE                                                                       */
/*        IF P_TipPro EQ 1 THEN DO:                                                  */
/*           PUT "Valor     : " P_Valor SKIP.                                */
/*           PUT "Id_Cajero : " W_Usuario       SKIP.                                */
/*        END.                                                                       */
/*        PUT "Saldo Actual : " P_SAct  SKIP.                                        */
/*        PUT "NOTA: EL SALDO SOLO SE ENTREGA"     SKIP.                             */
/*        PUT "AL TITULAR O PERSONA DEBIDAMENTE"   SKIP.                             */
/*        PUT "AUTORIZADA POR ESCRITO." SKIP.                                        */
       
       ASSIGN vcAsociado = P_Nit + " - " + P_Benef.

        PUT "FONDO DE EMPLEADOS DOCENTES DE LA UNIVERSIDAD NACIONAL" SKIP.
        PUT "                 NIT: 800.112.808-7"           SKIP(1).
        PUT "Num Doc.    : "  P_NroTx fechaHora      SKIP.
        PUT vcAsociado                          SKIP(1).
        
/*        PUT "Nit:      "  P_Nit                    SKIP. */
/*        PUT "Asociado: "  P_Benef                  SKIP. */
       PUT "Prod.        : "  P_NomPto          SKIP.
       PUT "No.Cuenta    : "  STRING(P_Agencia,"999") " - " P_Cuenta SKIP.
       PUT "Descr.       : "  P_Descrip                    SKIP.
       IF P_TipPro EQ 0 OR P_TipPro EQ 4 THEN DO:
          PUT "Valor         : " P_Valor SKIP.
          PUT "Cajero        : " W_Usuario   SKIP.
       END.
       ELSE
       IF P_TipPro EQ 2 OR (P_TipPro EQ 1 AND P_ICheque EQ "ConsigAho") THEN DO:
          PUT "Vlr.Pagado    :" P_Valor  SKIP.
          PUT idUsuario                        SKIP.
       END.
       ELSE
       IF P_TipPro EQ 1 THEN DO:
          PUT "Valor         : " P_Valor SKIP.
          PUT "Id_Cajero     : " W_Usuario       SKIP.          
       END.
       PUT "Saldo Actual     : " P_SAct  SKIP(1). 
       PUT "NOTA: EL SALDO SOLO SE ENTREGA"     SKIP.
       PUT "AL TITULAR O PERSONA DEBIDAMENTE"   SKIP.
       PUT "AUTORIZADA POR ESCRITO." SKIP.
        
        
        
        /* PUT " " SKIP(1).    */
       PUT CONTROL CHR(12).  
      /* PUT CONTROL CHR(27) + "q".  */
       OUTPUT CLOSE.
       RUN _osprint.r (INPUT ?,INPUT trim(salida),INPUT 8,INPUT 1,INPUT 1,INPUT 99999,OUTPUT SwImp).
       /*RUN MostrarMensaje IN W_Manija(INPUT 262,OUTPUT W_Rpta).*/
       W_Rpta = NO.
       IF NOT W_Rpta THEN LEAVE.
  END.
END PROCEDURE.

PROCEDURE ImpCheque:
    MESSAGE "Inserte Cheque a Imprimir" SKIP "Del Producto : " P_NomPto
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "Imp-Cheque en Validadora".
    DEF VAR W_Cadena AS CHA FORMAT "X(150)".
    DEF VAR W_Monto1 AS CHA FORMAT "X(70)".
    DEF VAR W_Monto2 AS CHA FORMAT "X(70)".
    DEF VAR W_Monto3 AS CHA FORMAT "X(70)".
    DEF VAR W_Rpta   AS LOG.
    RUN MontoEsc.r (INPUT P_Valor, INPUT 0, OUTPUT W_Cadena).
    RUN PartirValor IN W_Manija (INPUT W_Cadena,INPUT 70,OUTPUT W_Monto1,OUTPUT W_Monto2,OUTPUT W_Monto3).
    RUN VALUE(P_ICheque) (W_Monto1, W_Monto2, P_Benef, W_Ciudad, P_Valor, " ", 1).
END PROCEDURE.
