DEFINE VARIABLE W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Libro Caja Diario".
DEFINE VARIABLE L_CC AS LOGICAL INITIAL YES.

{Incluido/Variable.I "SHARED"}.
{Incluido/Varcon.I "SHARED"}.
{Incluido/Pantalla_Validacion.I}

PROCEDURE Proceso_Imprimir:
    DEFINE VAR Listado AS CHARACTER INITIAL "".

    Listado = W_Pathspl + W_Usuario + "L-CajaDia.lst".
    {Incluido\IMPRIMIR_carta_CamCcio.I "listado"}.
END PROCEDURE.


PROCEDURE ProcesoImprimir:
    {Incluido\RepEncabezado.i}

    DEFINE VARIABLE W_EstadoInf AS CHARACTER FORMAT "X(8)" INITIAL "".
    DEFINE VARIABLE Nom_Cencosto AS CHARACTER FORMAT "X(2)" INITIAL "".

    W_Reporte = STRING(W_nom_Entidad,"X(54)") + " - " + STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(20)").
    W_EncColumna = "CUENTA DESCRIPCION CUENTA      COMPROBANTE                             DEBITOS           CREDITOS".

    DEFINE VAR CtaAnt AS CHARACTER FORMAT "X".
    DEFINE VAR T_Cr AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
    DEFINE VAR T_Db AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
    DEFINE VAR TT_Cr AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
    DEFINE VAR TT_Db AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
    DEFINE VAR TTT_Cr AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
    DEFINE VAR TTT_Db AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
    DEFI VAR NomCta AS CHAR FORM "X(35)".

    DISPLAY "ENTIDAD         : FODUN                  CONSOLIDADO"  SKIP
            /*"NIT             : 860.075.780-9"         SKIP*/
            "REPORTE         : LIBRO CAJA DIARIO"     SKIP
            "FECHA DE CORTE  : " STRING(W_Fec1)       SKIP
            "--------------------------------------------------------------------------------------------------" SKIP
            "CUENTA DESCRIPCION CUENTA      COMPROBANTE                             DEBITOS           CREDITOS"  SKIP
            "--------------------------------------------------------------------------------------------------"
        WITH FRAME F-Encabezado2 WIDTH 132 PAGE-TOP NO-LABELS USE-TEXT NO-BOX STREAM-IO.

    VIEW FRAME F-Encabezado2.
    /*VIEW FRAME F-Ftr.*/

    FOR EACH Mov_Contable WHERE Mov_Contable.Agencia GE W_Ag1
                            AND Mov_Contable.Agencia LE W_Ag2
                            AND Mov_Contable.Cuenta GT " "
                            AND MONTH(Mov_Contable.Fec_Contable) EQ MONTH(W_Fec1)
                            AND YEAR(Mov_Contable.Fec_Contable) EQ YEAR(W_Fec1) NO-LOCK BREAK BY SUBSTRING(Mov_Contable.Cuenta,1,4)
                                                                                              BY Mov_Contable.Comprobante:
        IF FIRST-OF(SUBSTRING(Mov_Contable.Cuenta,1,4)) THEN DO:
            NomCta = "No-Existe en PUC".

            FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ SUBSTRING(Mov_Contable.Cuenta,1,4) NO-LOCK NO-ERROR.
            IF AVAIL(Cuentas) THEN
                NomCta = Cuentas.Nombre.

            DISPL SKIP(1)
                  SUBSTRING(Mov_Contable.Cuenta,1,4) FORM "X(4)"
                  " "
                  NomCta SKIP(0)
                WITH FRAME F-Cta WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
        END.

        ASSIGN T_Db   = T_Db + Mov_Contable.Db
                 T_Cr   = T_Cr + Mov_Contable.Cr
                 TT_Cr  = TT_Cr  + Mov_Contable.Cr
                 TT_Db  = TT_Db  + Mov_Contable.Db
                 TTT_Cr = TTT_Cr + Mov_Contable.Cr
                 TTT_Db = TTT_Db + Mov_Contable.Db.

          IF LAST-OF(Mov_Contable.Comprobante) THEN DO:
             FIND FIRST Comprobantes WHERE Comprobantes.Comprobante EQ Mov_Contable.Comprobante NO-LOCK NO-ERROR.
             DISPL "                              "
                   Mov_Contable.Comprobante FORM "999"
                   Comprobantes.Nombre      FORM "X(25)" WHEN AVAIL(Comprobantes)
                   T_Db 
                   T_Cr       SKIP(0)
                WITH DOWN FRAME F-Cpte WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO. 

             ASSIGN T_Db = 0
                    T_Cr = 0.
          END.

          IF LAST-OF(SUBSTRING(Mov_Contable.Cuenta,1,4)) THEN DO:   
             ASSIGN TT_Db = ROUND(TT_Db,0)
                    TT_Cr = ROUND(TT_Cr,0).
             DISPL "                                                           ------------------- ------------------" SKIP
                   "TOTAL CUENTA :"                                                   
                   SUBSTRING(Mov_Contable.Cuenta,1,4)  FORM "X(4)"                                                         
                   NomCta  
                   "   "
                   TT_Db                                                                                          
                   TT_Cr       SKIP(0)                                                                            
                WITH FRAME F-TCta WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.                            
                                                                                                                 
             ASSIGN TT_Db = 0                                                                                     
                    TT_Cr = 0.                                                                                    
          END.                                                                                                   
      END.

      ASSIGN TTT_Db = ROUND(TTT_Db,0)
             TTT_Cr = ROUND(TTT_Cr,0).
      DISPL SKIP(1)
            "                                                         -------------------- -------------------" SKIP
            "TOTAL GENERAL ------------------------------------------>"                                                                                             
            TTT_Db                                                                                                        
            TTT_Cr       SKIP(0)                                                                                          
         WITH FRAME F-TGen WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.                                                
      PAGE.
      OUTPUT CLOSE.

      ASSIGN TTT_Db = 0
             TTT_Cr = 0.
 END PROCE.

 PROCEDURE Habilita_Deshabilita:
  /* En este procedimiento se habilitan o deshabilitan las variables
     a pedir en pantalla segun el informe que se vaya a ejecutar. */
      ENABLE ALL WITH FRAME F_Valida IN WINDOW W_Pantalla.
            DISABLE Cmb_Comprob 
              W_Fec2
              W_NomUsuario1  W_NomUsuario2          
              W_NomCuenta1   W_NomCuenta2         
              W_Nit1         W_NomNit1            
              W_Nit2         W_NomNit2   
              Cmb_Nivel
              W_Base         W_Porcentaje WITH FRAME F_Valida.
      Cmb_Nivel:SCREEN-VALUE = "3".
      IF NOT L_CC THEN
         DISABLE Cmb_CenCost WITH FRAME F_Valida.
  END PROCEDURE.
