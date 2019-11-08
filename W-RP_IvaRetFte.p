DEFINE VARIABLE W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "INFORME IVA-RETEFUENTE".

{incluido/Variable.i "SHARED"}.
{incluido/Varcon.i "SHARED"}.

  DEFINE VARIABLE W_Naturaleza LIKE Cuentas.Naturaleza.
  DEFINE VARIABLE W_CtrNat     LIKE Cuentas.Ctr_Naturaleza.
  DEFINE VARIABLE L_CC         AS LOGICAL INITIAL YES.
  DEFINE VARIABLE W_Com        AS LOGICAL INITIAL NO.
  DEFI   VAR      BaseP        LIKE Base_Ret.Porcentaje.

  DEFINE TEMP-TABLE TSCuentas LIKE Mov_Contable.
  

/* incluido de Pantalla con parametros */
{incluido/Pantalla_Validacion.i}

  PROCEDURE Busca_Cuenta:
    DEFINE INPUT  PARAMETER T_ConsCtai LIKE Cuentas.Cuenta. 
    DEFINE OUTPUT PARAMETER T_ConsCta  LIKE Cuentas.Cuenta.
    DEFINE OUTPUT PARAMETER T_ConsNom  LIKE Cuentas.Nombre.
    IF T_ConsCtai NE "" THEN DO:
       FIND Cuentas WHERE Cuentas.Cuenta EQ T_ConsCtai AND
                          Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
       IF AVAILABLE(Cuentas) THEN
          ASSIGN T_ConsCta = Cuentas.Cuenta
                 T_ConsNom = Cuentas.Nombre.
    END.
    IF T_ConsCta NE "" THEN DO:
       RUN C-Cuentas.r (OUTPUT T_ConsCta, OUTPUT T_ConsNom, OUTPUT W_Naturaleza, OUTPUT W_CtrNat, INPUT "T").
       IF T_ConsCta EQ ? THEN DO:
          FIND Cuentas WHERE Cuentas.Cuenta EQ T_ConsCta
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR NO-WAIT.
          IF NOT AVAILABLE(Cuentas) THEN
            ASSIGN T_ConsCta  = ""
                   T_ConsNom  = "".           
       END.
    END.
  END PROCEDURE.

/* fin incluido Pantalla parametros */
  

  PROCEDURE Habilita_Deshabilita:
  /* En este procedimiento se habilitan o deshabilitan las variables
     a pedir en pantalla segun el informe que se vaya a ejecutar. */
      ENABLE ALL WITH FRAME F_Valida IN WINDOW W_Pantalla.
      DISABLE Cmb_Comprob 
/*              W_Fec2*/
              W_NomUsuario1  W_NomUsuario2          
              W_NomCuenta1   W_NomCuenta2         
              W_Nit1         W_NomNit1            
              W_Nit2         W_NomNit2            
              W_Base         W_Porcentaje WITH FRAME F_Valida.
      IF NOT L_CC THEN /* valida si la entidad maneja centros de costos */
         DISABLE Cmb_CenCost WITH FRAME F_Valida.
  END PROCEDURE.

PROCEDURE Tabla_Temporal:
    DEFINE VAR W_FecIniMes AS DATE.

    IF W_Cuenta1 = "" THEN
        W_Cuenta1 = "0".
    
    IF W_Cuenta2 = "" THEN
        W_Cuenta2 = "99999999999999".

    W_FecIniMes = DATE("01/" + STRING(MONTH(W_Fec1)) + "/" + STRING(YEAR(W_Fec1))).

    FOR EACH Mov_Contable WHERE Mov_Contable.Agencia >= W_Ag1
                            AND Mov_Contable.Agencia <= W_Ag2
                            AND Mov_Contable.Cen_Costos >= W_CC1
                            AND Mov_Contable.Cen_Costos <= W_CC2
                            AND Mov_Contable.Cuenta >= W_Cuenta1
                            AND Mov_Contable.Cuenta <= W_Cuenta2
                            AND Mov_Contable.Fec_Contable >= W_Fec1
                            AND Mov_Contable.Fec_Contable <= W_Fec2 NO-LOCK:
        CREATE TSCuentas.
        BUFFER-COPY Mov_Contable TO TsCuentas.
    END.

    FOR EACH Mov_Contable2 WHERE Mov_Contable2.Agencia >= W_Ag1
                             AND Mov_Contable2.Agencia <= W_Ag2
                             AND mov_contable2.comprobante >= 0
                             AND mov_contable2.comprobante <= 999
                             AND Mov_Contable2.Cen_Costos >= W_CC1
                             AND Mov_Contable2.Cen_Costos <= W_CC2
                             AND Mov_Contable.Fec_Contable >= W_Fec1
                             AND Mov_Contable.Fec_Contable <= W_Fec2
                             AND Mov_Contable.Cuenta >= W_Cuenta1
                             AND Mov_Contable.Cuenta <= W_Cuenta2 NO-LOCK:
        CREATE TSCuentas.
        TSCuentas.agencia = mov_contable2.agencia.
        TSCuentas.cen_costos = mov_contable2.cen_costos.
        TSCuentas.nit = mov_contable2.cliente_id.
        TSCuentas.comentario = mov_contable2.comentario.
        TSCuentas.comprobante = mov_contable2.comprobante.
        TSCuentas.cr = mov_contable2.cr.
        TSCuentas.cuenta = mov_contable2.cuenta.
        TSCuentas.db = mov_contable2.db.
        TSCuentas.doc_referencia = mov_contable2.doc_referencia.
        TSCuentas.fec_contable = mov_contable2.fec_contable.
        TSCuentas.num_documento = mov_contable2.num_documento.
        TSCuentas.usuario = mov_contable2.usuario.
    END.

END PROCEDURE.

  PROCEDURE Proceso_Imprimir:
      DEFINE VAR Listado AS CHARACTER INITIAL "".
      
      FOR EACH TSCuentas: DELETE TSCuentas. END.

      RUN Tabla_Temporal.                        

      Listado = W_Pathspl + "-IvaREtFte" + STRING(TIME) + ".lst".
     {incluido\IMPRIMIR.I "listado"}.
  END PROCEDURE.

  PROCEDURE ProcesoImprimir:
     {Incluido\RepEncabezado.i}
      DEFINE VARIABLE W_EstadoInf   AS CHARACTER FORMAT "X(8)" INITIAL "".
      DEFINE VARIABLE Nom_Cencosto  AS CHARACTER FORMAT "X(2)" INITIAL "".
      W_Reporte   = "REPORTE   : IVA Y RETEFUENTE - Agencia: " + STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(25)")
                    + " - "+ STRING(TIME,"hh:mm am").
/*                             1         2         3         4         5         6         7         8         9         10
                      12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
      W_EncColumna = "AG. CED./NIT     FECHA    CPTE-DOCTO           VLR.BASE            VLR.DEBITO        VLR.CREDITO".
      DEFINE VAR  TT_Db  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      DEFINE VAR  TT_Cr  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      DEFINE VAR  To_Db  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      DEFINE VAR  To_Cr  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      DEFINE VAR  TTT_Db  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      DEFINE VAR  TTT_Cr  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      DEFI   VAR  Tbas    AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      DEFI   VAR  TTbas   AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      DEFI   VAR  TTTbas  AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".
      DEFI   VAR  Vr_Base AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99".

      
      DEFINE VAR  W_Cta AS CHARACTER FORMAT "X(14)".
      DEFINE VAR  W_Nom AS CHARACTER FORMAT "X(20)".
      DEFINE VAR  SFin LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR  SFTo LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR  Sini  LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR  SAnt  LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR  TSini LIKE Sal_Cuenta.Sal_Inicial.
      DEFINE VAR  TSFin LIKE Sal_Cuenta.Sal_Inicial.

      VIEW FRAME F-Encabezado.
      VIEW FRAME f-ftr.

      DISPLAY "                               De Fecha : "
              W_Fec1
              " Hasta : "
              W_Fec2
           WITH WIDTH 132 FRAME F-tit NO-BOX NO-LABELS USE-TEXT.

      FOR EACH TSCuentas BREAK BY TSCuentas.Agencia BY TSCuentas.Cuenta BY TSCuentas.Fec_Contab
                               BY TSCuentas.Comprob BY TSCuentas.Num_Docum:
          IF FIRST-OF(TSCuentas.Cuenta) THEN DO:
             ASSIGN W_Nom = ""
                    Base  = 0.

             FIND Cuentas WHERE Cuentas.Cuenta EQ TSCuentas.Cuenta NO-LOCK NO-ERROR.
             IF AVAIL(Cuentas) THEN DO:
                ASSIGN Base  = 0
                       W_Nom = Cuentas.Nombre.
                IF Cuentas.Id_Base AND Cuentas.Cod_base GT " " THEN DO:
                   FIND FIRST Base_Ret WHERE Base_Ret.Cod_Base EQ Cuentas.Cod_base
                                         AND Base_Ret.Estado   EQ 1 NO-LOCK NO-ERROR.
                   IF AVAIL(Base_Ret) AND Base_Ret.Porcentaje NE ? AND Base_Ret.Porcentaje NE 0 THEN
                      Base = Base_Ret.Porcentaje.
                END.
             END.

             DISPLAY "CUENTA : "
                     TSCuentas.Cuenta     FORM "X(14)"
                     W_Nom                FORM "X(30)"        
                     "Retenc.%"
                     Base                 FORM "->>9.99"
                  WITH WIDTH 132 NO-LABELS NO-BOX USE-TEXT.
          END.

          IF TSCuentas.Db GT 0 THEN
             ASSIGN TT_Db = TT_Db + TSCuentas.DB.
          ELSE 
             ASSIGN TT_Cr   = TT_Cr + TSCuentas.Cr.

          IF Base NE 0 AND TSCuentas.Cr GT 0 THEN
             ASSIGN Vr_Base = ROUND(TSCuentas.Cr / (Base / 100),0)
                    Tbas    = TBas + Vr_Base.
          ELSE
             ASSIGN Vr_Base = 0.

          DISPLAY TSCuentas.Agenc     
                  TSCuentas.Nit
                  TSCuentas.Fec_Contab FORM "99/99/99"
                  TSCuentas.Comprob  FORMAT "9999"   
                  TSCuentas.Num_Docum FORMAT "99999999999"
                  "     "
                  Vr_Base              FORMAT "->>,>>>,>>>,>>9.99"
                  TSCuentas.Db    
                  TSCuentas.Cr               
              WITH DOWN FRAME F_Mov WIDTH 132 NO-BOX USE-TEXT STREAM-IO NO-LABELS.
             
          IF LAST-OF(TSCuentas.Cuenta) THEN DO:
             DISPLAY "---------------------------------------------------------------------------------------" SKIP
                      "TOT.CTA:" 
                      TSCuentas.Cuenta     FORM "X(12)"
                      W_Nom                FORM "X(18)"
                      Tbas
                      TT_Db   
                      TT_Cr   
                     "---------------------------------------------------------------------------------------" 
                      SKIP(1)
                 WITH WIDTH 132 FRAME F_Tot NO-LABELS NO-BOX USE-TEXT.
              ASSIGN TT_db = 0 TT_Cr = 0 TBas.
          END.
      END.  
      
      PAGE.
    OUTPUT CLOSE.
    FOR EACH TSCuentas: DELETE TSCuentas. END.
  END PROCEDURE.
                                           
