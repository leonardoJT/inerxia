  /*Programa W-Rp_CajaDia.P*/
    
  DEFINE VARIABLE W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Comprobante Resumen".
  DEFINE VARIABLE L_CC         AS LOGICAL INITIAL YES.

 {Incluido/Variable.I "SHARED"}.
 {Incluido/Varcon.I "SHARED"}.

 {Incluido/Pantalla_Validacion.I}  

 PROCEDURE Proceso_Imprimir:
      DEFINE VAR Listado AS CHARACTER INITIAL "".
      
      Listado = W_Pathspl + W_Usuario + "L-CpteResum.lst".
     {Incluido\IMPRIMIR_carta.I "listado"}.
 END PROCEDURE.


 PROCEDURE ProcesoImprimir:
     {Incluido\RepEncabezado.i}
      DEFINE VARIABLE W_EstadoInf   AS CHARACTER FORMAT "X(8)" INITIAL "".
      DEFINE VARIABLE Nom_Cencosto  AS CHARACTER FORMAT "X(2)" INITIAL "".
      W_Reporte   = "".                              
      W_EncColumna = "".

      DEFINE VAR CtaAnt AS CHARACTER FORMAT "X".

      DEFINE VAR  T_Cr   AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      DEFINE VAR  T_Db   AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      DEFINE VAR  TT_Cr  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      DEFINE VAR  TT_Db  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      DEFINE VAR  TTT_Cr AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99". 
      DEFINE VAR  TTT_Db AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".
      DEFINE VAR  TTTT_Cr AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99". 
      DEFINE VAR  TTTT_Db AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".

      DEFI   VAR  NomCta AS CHAR FORM "X(35)".
      DEFI   VAR  NomAge AS CHAR FORM "X(25)" INIT "CONSOLIDADO".

      IF W_Ag1 EQ W_Ag2 THEN DO:
         FIND FIRST Agencias WHERE Agencias.Agencia EQ W_Ag1 NO-LOCK NO-ERROR.
         NomAge = Agencias.Nombre.
      END.

      DISPLAY "ENTIDAD         : FONDO EMPLEADOS DOCENTES UNIVERSIDAD NACIONAL"  SKIP /**Cambio entidad Alan Gordon 02-10-2007**/
              "AGENCIA         : " + STRING(NomAge,"X(35)")         FORM "X(130)"         SKIP
              "REPORTE         : COMPROBANTE RESUMEN"                                     SKIP
              "FECHA DE CORTE  : " + STRING(W_Fec1,"99/99/9999")  + " HASTA " + STRING(W_Fec2,"99/99/9999") 
                                                                    FORM "X(130)"         SKIP
              "------------------------------------------------------------------------------------------------------" SKIP
              "CUENTA       DESCRIPCION CUENTA       COMPROBANTE                           DEBITOS           CREDITOS"  SKIP
              "------------------------------------------------------------------------------------------------------" 
          WITH FRAME F-Encabezado2 WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.

      VIEW FRAME F-Encabezado2.
      VIEW FRAME F-Ftr.

      FOR EACH Mov_Contable WHERE Mov_Contable.Agencia             GE W_Ag1      AND
                                  Mov_Contable.Agencia             LE W_Ag2      AND
                                  Mov_Contable.Comprobante         GE W_CB1      AND
                                  Mov_Contable.Comprobante         LE W_CB2      AND
                                  Mov_Contable.Fec_Contable        GE W_Fec1     AND
                                  Mov_Contable.Fec_Contable        LE W_Fec2  NO-LOCK
               BREAK BY Mov_Contable.Comprobante BY Mov_Contable.Cuenta:
          IF FIRST-OF(Mov_Contable.Comprobante) THEN DO:
              FIND FIRST Comprobantes WHERE Comprobantes.Comprobante EQ Mov_Contable.Comprobante NO-LOCK NO-ERROR.
              DISPL "                                     "
                    Mov_Contable.Comprobante FORM "99"
                    Comprobantes.Nombre      FORM "X(25)" WHEN AVAIL(Comprobantes) SKIP
                    "                                     ------------------------------"
                 WITH DOWN FRAME F-Cpte WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO. 
          END.

          ASSIGN T_Db   = T_Db + Mov_Contable.Db
                 T_Cr   = T_Cr + Mov_Contable.Cr
                 TT_Cr  = TT_Cr  + Mov_Contable.Cr
                 TT_Db  = TT_Db  + Mov_Contable.Db
                 TTT_Cr = TTT_Cr + Mov_Contable.Cr
                 TTT_Db = TTT_Db + Mov_Contable.Db
                 TTTT_Cr = TTTT_Cr + Mov_Contable.Cr
                 TTTT_Db = TTTT_Db + Mov_Contable.Db.

          IF LAST-OF(Mov_Contable.Cuenta) THEN DO:  
             NomCta = "No-Existe en PUC".
             FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Mov_Contable.Cuenta NO-LOCK NO-ERROR.
             IF AVAIL(Cuentas) THEN
                NomCta = Cuentas.Nombre.
             DISPL Mov_Contable.Cuenta   FORM "X(12)"                                                         
                   NomCta  
                   "               "
                   T_Db                                                                                          
                   T_Cr       SKIP(0)                                                                            
                WITH FRAME F-TCta WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.                            
                                                                                                                 
             ASSIGN T_Db = 0                                                                                     
                    T_Cr = 0.                                                                                    
          END. 


          IF LAST-OF(Mov_Contable.Comprobante) THEN DO:
             DISPLAY
                 "                                                                  ------------------- ------------------" SKIP
                 "TOTAL COMPROBANTE "
                 Comprobantes.Nombre      FORM "X(25)" WHEN AVAIL(Comprobantes)
                 "                   "
                 TT_Db 
                 TT_Cr       SKIP(1)
               WITH FRAME F-TCpte WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.

             ASSIGN TT_Db = 0
                    TT_Cr = 0.
          END.
      END.

      DISPL SKIP(1)
            "                                                                -------------------- -------------------" SKIP
            "TOTAL GENERAL ------------------------------------------------>"                                                                                             
            TTT_Db                                                                                                        
            TTT_Cr       SKIP(0)                                                                                          
         WITH FRAME F-TGen WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.                                                
      PAGE.

      ASSIGN TTT_Cr =  0
             TTT_Db =  0
             TTTT_Cr = 0
             TTTT_Db = 0.

      DISPL SKIP(2) WITH NO-LABEL.

      FOR EACH Mov_Contable WHERE Mov_Contable.Agencia             GE W_Ag1      AND
                                  Mov_Contable.Agencia             LE W_Ag2      AND
                                  Mov_Contable.Comprobante         GE W_CB1      AND
                                  Mov_Contable.Comprobante         LE W_CB2      AND
                                  Mov_Contable.Fec_Contable        GE W_Fec1     AND
                                  Mov_Contable.Fec_Contable        LE W_Fec2  NO-LOCK
               BREAK BY Mov_Contable.Cuenta:
          ASSIGN TTT_Cr = TTT_Cr + Mov_Contable.Cr
                 TTT_Db = TTT_Db + Mov_Contable.Db
                 TTTT_Cr = TTTT_Cr + Mov_Contable.Cr
                 TTTT_Db = TTTT_Db + Mov_Contable.Db.

          IF LAST-OF(Mov_Contable.Cuenta) THEN DO:  
             NomCta = "No-Existe en PUC".
             FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Mov_Contable.Cuenta NO-LOCK NO-ERROR.
             IF AVAIL(Cuentas) THEN
                NomCta = Cuentas.Nombre.
             DISPL Mov_Contable.Cuenta   FORM "X(12)"                                                         
                   NomCta  
                   "               "
                   TTT_Db                                                                                          
                   TTT_Cr       SKIP(0)                                                                            
                WITH FRAME F-TCtaTT WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.                            
                                                                                                                 
             ASSIGN TTT_Db = 0                                                                                     
                    TTT_Cr = 0.                                                                                    
          END. 
      END.

      DISPL SKIP(1)
            "                                                                -------------------- -------------------" SKIP
            "TOTAL GENERAL ------------------------------------------------>"                                                                                             
            TTTT_Db                                                                                                        
            TTTT_Cr       SKIP(0)                                                                                          
         WITH FRAME F-TGenTT WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.                                                

      OUTPUT CLOSE.

      ASSIGN TTTT_Db = 0
             TTTT_Cr = 0.
 END PROCE.

 PROCEDURE Habilita_Deshabilita:
  /* En este procedimiento se habilitan o deshabilitan las variables
     a pedir en pantalla segun el informe que se vaya a ejecutar. */
      ENABLE ALL WITH FRAME F_Valida IN WINDOW W_Pantalla.
            DISABLE /*Cmb_Comprob */
              W_NomUsuario1  W_NomUsuario2  
              W_Usuario1     W_Usuario2
              W_Cuenta1      W_Cuenta2
              W_NomCuenta1   W_NomCuenta2         
              W_Nit1         W_NomNit1            
              W_Nit2         W_NomNit2   
              Cmb_Nivel
              W_Base         W_Porcentaje WITH FRAME F_Valida.
      Cmb_Nivel:SCREEN-VALUE = "3".
      IF NOT L_CC THEN
         DISABLE Cmb_CenCost WITH FRAME F_Valida.
  END PROCEDURE.
