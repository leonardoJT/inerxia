&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Prc_RevAportes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Prc_RevAportes 
CREATE WIDGET-POOL.

ON RETURN TAB.

{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I "SHARED"}

DEFI VAR W_SiContab AS LOG INIT FALSE.

DEFI TEMP-TABLE TempLiq
    FIELD Age AS INTEGER
    FIELD Nit AS CHARACTER
    FIELD Cod AS INTEGER
    FIELD Cta AS CHARACTER
    FIELD VrRev AS DECIMAL
    FIELD PorcR AS DECIMAL FORMAT "999.999999"
    FIELD SdoPro AS DECIMAL
    FIELD SdoMes AS DECIMAL EXTENT 12.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Proc

/* Standard List Definitions                                            */

&Scoped-Define ENABLED-OBJECTS RECT-314 RECT-315 BUTTON-5 W_CmbOfi Btn_Proc ~
Btn_Contab Btn_Done Rs_Op W_Porc W_Val 
&Scoped-Define DISPLAYED-OBJECTS W_CmbOfi W_FecMes Msaje W_Cont Rs_Op ~
W_Porc W_Val 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 Msaje 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

    /* oakley */


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Prc_RevAportes AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Contab 
     LABEL "&Contabiliza" 
     SIZE 10.43 BY 1.46 TOOLTIP "Contabiliza la revalorizaciòn".

DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Salir" 
     SIZE 10.43 BY 1.42
     BGCOLOR 8 .

DEFINE BUTTON Btn_Proc 
     LABEL "&Procesar" 
     SIZE 10.43 BY 1.46 TOOLTIP "Procesa la Revalorizaciòn".

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 5" 
     SIZE 10.43 BY 1.62.

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(30)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 30.29 BY 1 TOOLTIP "Agencias Activas"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Msaje AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 43.29 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Cont AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.86 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FecMes AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Proceso" 
     VIEW-AS FILL-IN 
     SIZE 10.72 BY .81 TOOLTIP "Fecha Corte para el Proceso"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_Porc AS DECIMAL FORMAT "99.999":U INITIAL 0 
     LABEL "Porcentaje %" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .85 TOOLTIP "Porcentaje %"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_Val AS DECIMAL FORMAT ">>>>>>,>>>,>>9":U INITIAL 0 
     LABEL "Vlr.a Distribuir" 
     VIEW-AS FILL-IN 
     SIZE 16.86 BY .85 TOOLTIP "Valor a Distribuir"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Rs_Op AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Sobre Saldos Fin-Año", 1,
"Sobre Saldos Promedios", 2,
"Sobre cada Saldo-Mes", 3
     SIZE 26.57 BY 2.81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-314
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15.14 BY 7.73.

DEFINE RECTANGLE RECT-315
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59.14 BY 3.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Proc
     BUTTON-5 AT ROW 2.54 COL 68
     W_CmbOfi AT ROW 3.42 COL 19 COLON-ALIGNED
     Btn_Proc AT ROW 4.5 COL 68
     W_FecMes AT ROW 6.15 COL 19 COLON-ALIGNED
     Btn_Contab AT ROW 6.38 COL 68
     Btn_Done AT ROW 8.15 COL 68 HELP
          "Sale del proceso de Depreciación y Ajustes"
     Msaje AT ROW 8.62 COL 5 COLON-ALIGNED NO-LABEL
     W_Cont AT ROW 8.65 COL 49 COLON-ALIGNED NO-LABEL
     Rs_Op AT ROW 10.69 COL 8 NO-LABEL
     W_Porc AT ROW 11.15 COL 46.72 COLON-ALIGNED
     W_Val AT ROW 12.35 COL 46.57 COLON-ALIGNED
     "Opciones para la Revalorizaciòn   l            Porcentaje    O     Valor" VIEW-AS TEXT
          SIZE 59.14 BY .5 AT ROW 10 COL 7.14
          BGCOLOR 1 
     "Regist.Proceso" VIEW-AS TEXT
          SIZE 13.72 BY .5 AT ROW 8.12 COL 51
          BGCOLOR 1 
     RECT-314 AT ROW 2.15 COL 65.86
     RECT-315 AT ROW 10.54 COL 7.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 86.72 BY 13.88
         BGCOLOR 17 FGCOLOR 0 FONT 5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Prc_RevAportes ASSIGN
         HIDDEN             = YES
         TITLE              = "Aportes - Revalorizaciòn, Programa W-Prc_RevAportes.W"
         HEIGHT             = 13.88
         WIDTH              = 86.72
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 86.72
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 86.72
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Prc_RevAportes
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Proc
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN Msaje IN FRAME F_Proc
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN W_Cont IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_FecMes IN FRAME F_Proc
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Prc_RevAportes)
THEN W-Prc_RevAportes:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Proc
/* Query rebuild information for FRAME F_Proc
     _Query            is NOT OPENED
*/  /* FRAME F_Proc */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Prc_RevAportes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Prc_RevAportes W-Prc_RevAportes
ON END-ERROR OF W-Prc_RevAportes /* Aportes - Revalorizaciòn, Programa W-Prc_RevAportes.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Prc_RevAportes W-Prc_RevAportes
ON WINDOW-CLOSE OF W-Prc_RevAportes /* Aportes - Revalorizaciòn, Programa W-Prc_RevAportes.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Contab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Contab W-Prc_RevAportes
ON CHOOSE OF Btn_Contab IN FRAME F_Proc /* Contabiliza */
DO:
  DEFI VAR Listado  AS CHAR FORM "X(40)".

  IF W_Porc LE 0 AND W_Val LE 0 THEN DO:
     MESSAGE "Falta Porcentaje o Valor para la Distribuciòn...Rectifique por favor."
             VIEW-AS ALERT-BOX ERROR.
        
     RETURN.       
  END.        

  FOR EACH TempLiq: DELETE TempLiq. END.

  ASSIGN Rs_Op.
    
  FOR EACH Agencias WHERE Agencias.Estado  NE 3 
                      AND Agencias.Agencia GE W_OfiIni 
                      AND Agencias.Agencia LE W_OfiFin NO-LOCK:
      FIND FIRST ProcDia WHERE ProcDia.Agencia     EQ Agencias.Agencia
                     AND ProcDia.Cod_Proceso       EQ 4
                     AND YEAR(ProcDia.Fecha_Proc)  EQ YEAR (W_FecMes)
                     AND ProcDia.Estado            EQ 1  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(ProcDia) THEN DO:
         MESSAGE "Este proceso Ya Fué Ejecutado para este Día en la Agencia: " Agencias.Agencia SKIP
                 "O no està Matriculado...Revise por favor. No se permite la operación." SKIP
                 VIEW-AS ALERT-BOX ERROR TITLE "Confirmar Proceso".     
         RETURN. 
      END.
  END. 

  FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Id_Revaloriz
                           AND Pro_Ahorros.Estado       EQ 1 NO-LOCK NO-ERROR.
  IF AVAIL(Pro_Ahorros) THEN DO:
     FIND Cuentas WHERE Cuentas.Cuenta EQ Pro_Ahorros.Cta_Revaloriz
                    AND Cuentas.Estado EQ 1
                    AND Cuentas.Tipo   EQ 2 NO-LOCK NO-ERROR.
     IF NOT AVAIL(Cuentas) THEN DO:
        MESSAGE "La Cuenta Contrapartida de Pro_Ahorros.Cta_Revaloriz: " Pro_Ahorros.Cta_Revaloriz SKIP
                "                           No existe...Rectifique por favor"
               VIEW-AS ALERT-BOX ERROR.
        RETURN.
     END.
     
     FIND FIRST CortoLargo WHERE CortoLargo.Agencia    GE W_OfiIni
                         AND CortoLargo.Agencia        LE W_OfiFin
                         AND CortoLargo.Clase_Producto EQ 1                                           
                         AND CortoLargo.Cod_Producto   EQ Pro_Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
     IF AVAIL(CortoLargo) THEN FOR EACH CortoLargo 
                                  WHERE CortoLargo.Agencia        GE W_OfiIni
                                    AND CortoLargo.Agencia        LE W_OfiFin
                                    AND CortoLargo.Clase_Producto EQ 1                                           
                                    AND CortoLargo.Cod_Producto   EQ Pro_Ahorros.Cod_Ahorro NO-LOCK:
        FIND Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_AsoAd
                       AND Cuentas.Estado EQ 1
                       AND Cuentas.Tipo   EQ 2 NO-LOCK NO-ERROR.
        IF NOT AVAIL(Cuentas) THEN DO:
           MESSAGE "La Cuenta del Pdcto de Aportes : " Pro_Ahorros.Cod_Ahorro ", de la Agencia: "
                   CortoLargo.Agencia SKIP
                   "En CortoLargo.Cta_AsoAd no existe...Rectifique por favor"
               VIEW-AS ALERT-BOX ERROR.
           RETURN.
        END.
     END.
     ELSE DO:
        MESSAGE "Debe Existir CortoLargo con el Pdcto de aportes: " Pro_Ahorros.Cod_Ahorro SKIP
                "                             Proceso Rechazado..." 
                VIEW-AS ALERT-BOX ERROR.
        RETURN.
     END.
  END.
  ELSE DO: 
     MESSAGE "Debe Existir Pro_Ahorros con Id_Revaloriza y CortoLargo con el Pdcto de aportes." SKIP
             "                             Proceso Rechazado..." 
         VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.
    
  FIND FIRST Varios WHERE Varios.Tipo   EQ 8
                      AND Varios.Codigo EQ 4 NO-LOCK NO-ERROR.
  IF AVAIL (varios) THEN 
     FIND FIRST Comprobantes WHERE Comprobantes.Agencia EQ W_Agencia
                               AND Comprobantes.Comprob EQ Varios.Comprobante
                               AND Comprobantes.Estado  EQ 1 NO-ERROR.

  IF NOT AVAIL(Comprobantes) OR NOT AVAIL(varios) THEN DO:
     MESSAGE "El Comprobante-Fuente Contable para el proceso debe existir en Tipos y en Comprobantes" SKIP
             "                          No se acepta la Operación." VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.
      
  ASSIGN Comprobantes.Secuencia = Comprobantes.Secuencia + 1
         listado                = W_PathSpl + "CpteRevApor.Lst".
                      
  FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
 
  SESSION:SET-WAIT-STATE("GENERAL").
  ASSIGN Msaje:SCREEN-VALUE = "Espere un momento por favor, Procesando Aportes...".
        
  RUN Proceso NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     SESSION:SET-WAIT-STATE("").

     Msaje:SCREEN-VALUE = "   Proceso Contable Rev-Aportes no ejecutado... Revise por favor".

     RETURN.     
  END.
  ELSE DO:              
     RUN Contable NO-ERROR.
     IF ERROR-STATUS:ERROR THEN DO:
        SESSION:SET-WAIT-STATE("").

        RETURN.     
     END.
     ELSE DO:
        FOR EACH Agencias WHERE Agencias.Estado  NE 3                                     
                            AND Agencias.Agencia GE W_OfiIni 
                            AND Agencias.Agencia LE W_OfiFin NO-LOCK:                            
            FIND ProcDia WHERE ProcDia.Agencia     EQ Agencias.Agencia                    
                           AND ProcDia.Cod_Proceso EQ 4                                   
                           AND ProcDia.Fecha_Proc  EQ W_FecMes                            
                           AND ProcDia.Estado      EQ 1 NO-ERROR.                         
                                                                                       
            ASSIGN ProcDia.Estado = 2
                   W_SiContab     = TRUE.                                                    
        END.

        {Incluido\Imprimir.I "listado"}

        ASSIGN Msaje:SCREEN-VALUE = "   Proceso Contabilizaciòn Reval-Aportes Terminó Exitosamente..."
               W_SiContab         = FALSE.

        SESSION:SET-WAIT-STATE("").
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-Prc_RevAportes
ON CHOOSE OF Btn_Done IN FRAME F_Proc /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Proc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Proc W-Prc_RevAportes
ON CHOOSE OF Btn_Proc IN FRAME F_Proc /* Procesar */
DO:
  DEFI   VAR Listado    AS CHAR FORM "X(40)".
  DEFINE VAR W_Rpta     AS LOGICAL.

  FOR EACH TempLiq: DELETE TempLiq. END.

  ASSIGN Rs_Op.
    
  IF W_Porc LE 0 AND W_Val LE 0 THEN DO:
     MESSAGE "Falta Porcentaje o Valor para la Distribuciòn...Rectifique por favor."
             VIEW-AS ALERT-BOX ERROR.
        
     RETURN.       
  END.        

  SESSION:SET-WAIT-STATE("GENERAL").
  ASSIGN Msaje:SCREEN-VALUE = "Espere un momento por favor, Procesando Aportes...".
  
  RUN Proceso NO-ERROR.
  IF ERROR-STATUS:ERROR THEN 
     Msaje:SCREEN-VALUE = "   Proceso Revalorizaciòn-Aportes no ejecutado... Revise por favor".
  ELSE DO:
     ASSIGN Listado = W_PathSpl + "RevalAporte.Lst"
            Msaje:SCREEN-VALUE = "   Proceso Rev-Aportes terminò Exitosamente...".
                                                                                           
     {Incluido\Imprimir.I "listado"}
  END.

  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-Prc_RevAportes
ON CHOOSE OF BUTTON-5 IN FRAME F_Proc /* Button 5 */
DO:
  RUN W-InfDia.R.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rs_Op
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_Op W-Prc_RevAportes
ON MOUSE-SELECT-CLICK OF Rs_Op IN FRAME F_Proc
DO:
  ASSIGN Rs_Op.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_Op W-Prc_RevAportes
ON VALUE-CHANGED OF Rs_Op IN FRAME F_Proc
DO:
  ASSIGN Rs_Op.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfi W-Prc_RevAportes
ON VALUE-CHANGED OF W_CmbOfi IN FRAME F_Proc /* Agencia */
DO:
  IF INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)) EQ 0 THEN DO:
     FIND FIRST Agencias WHERE Agencias.Agencia GT 0
                           AND Agencias.Estado  NE 3 NO-LOCK NO-ERROR.
     IF AVAILABLE Agencias THEN 
        ASSIGN W_OfiIni = Agencias.Agencia.
        
     FIND LAST Agencias WHERE Agencias.Agencia GT 0
                          AND Agencias.Estado  NE 3 NO-LOCK NO-ERROR.
     IF AVAILABLE Agencias THEN 
        ASSIGN W_OfiFin = Agencias.Agencia.
  END.
  ELSE ASSIGN W_OfiIni = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)) 
              W_OfiFin = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecMes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecMes W-Prc_RevAportes
ON LEAVE OF W_FecMes IN FRAME F_Proc /* Fecha Proceso */
DO:
  ASSIGN W_FecMes.                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Porc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Porc W-Prc_RevAportes
ON LEAVE OF W_Porc IN FRAME F_Proc /* Porcentaje % */
DO:
  ASSIGN W_Porc.
    
  IF W_Porc GT 0 THEN
     ASSIGN W_Val              = 0
            W_Val:SCREEN-VALUE = "0".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Val
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Val W-Prc_RevAportes
ON LEAVE OF W_Val IN FRAME F_Proc /* Vlr.a Distribuir */
DO:
  ASSIGN W_Val.
    
  IF W_Val GT 0 THEN
     ASSIGN W_Porc              = 0
            W_Porc:SCREEN-VALUE = "0.00".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Prc_RevAportes 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  W_CmbOfi:ADD-LAST("000 CONSOLIDADO").
  
   ASSIGN W_OfiIni                              = W_Agencia
          W_OfiFin                              = W_Agencia
          W_FecMes:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Fecha)
          W_FecMes                              = W_Fecha.
                  
   FOR EACH Agencias WHERE Agencias.Estado  NE 3 
                       AND Agencias.Agencia GT 0 NO-LOCK:
       W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"999") + 
                        "-" + STRING(Agencias.Nombre,"X(25)")).
       IF W_OfiCierre NE 0 AND Agencias.Agencia EQ W_OfiCierre THEN
          ASSIGN W_CmbOfi:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + "-" + 
                                         STRING(Agencias.Nombre,"X(25)").
   END.              
     
   IF W_OfiCierre EQ 0 THEN
      ASSIGN W_CmbOfi:SCREEN-VALUE = "000 CONSOLIDADO".
     
   APPLY "VALUE-CHANGED" TO W_CmbOfi.


  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contable W-Prc_RevAportes 
PROCEDURE Contable :
DO TRANSACTION ON ERROR UNDO:
    FOR EACH TempLiq WHERE TempLiq.VrRev GT 0 BREAK BY TempLiq.Age
                                                    BY TempLiq.Nit:
        IF FIRST-OF(TempLiq.Age) THEN DO:
            FIND FIRST CortoLargo WHERE CortoLargo.Agencia EQ TempLiq.Age
                                    AND CortoLargo.Clase_Producto EQ 1
                                    AND CortoLargo.Cod_Producto EQ Pro_Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
            IF NOT AVAIL(CortoLargo) THEN DO:
                MESSAGE "Debe Existir CortoLargo con el Pdcto de aportes: " Pro_Ahorros.Cod_Ahorro
                        "En la Agencia : " TempLiq.Age SKIP
                        "                             Proceso Rechazado..."
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.
        END.

        CREATE Mov_Contable.
        ASSIGN Mov_Contable.Agencia = TempLiq.Age
               Mov_Contable.Cuenta = CortoLargo.Cta_AsoAd
               Mov_Contable.Nit = TempLiq.Nit
               Mov_Contable.Fec_Contable = W_Fecha
               Mov_Contable.Comentario = "Revaloriz.Aportes"
               Mov_Contable.Usuario = W_Usuario
               Mov_Contable.Cen_Costos = W_Cencosgral
               Mov_Contable.Destino = W_Agencia
               Mov_Contable.Comprobante = Comprobantes.Comprobante
               Mov_Contable.Num_Documento = Comprobantes.Secuencia
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Hora = TIME
               Mov_Contable.Estacion = W_Estacion
               Mov_Contable.Cr = TempLiq.VrRev.

        CREATE Mov_Contable.
        ASSIGN Mov_Contable.Agencia = TempLiq.Age
               Mov_Contable.Cuenta = Pro_Ahorros.Cta_Revaloriz
               Mov_Contable.Nit = TempLiq.Nit
               Mov_Contable.Fec_Contable = W_Fecha
               Mov_Contable.Comentario = "Revaloriz.Aportes"
               Mov_Contable.Usuario = W_Usuario
               Mov_Contable.Cen_Costos = W_Cencosgral
               Mov_Contable.Destino = W_Agencia
               Mov_Contable.Comprobante = Comprobantes.Comprobante
               Mov_Contable.Num_Documento = Comprobantes.Secuencia
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Hora = TIME
               Mov_Contable.Estacion = W_Estacion
               Mov_Contable.Db = TempLiq.VrRev.

        FIND FIRST Ahorros WHERE Ahorros.Nit EQ TempLiq.Nit
                             AND Ahorros.Cod_Ahorro EQ Pro_Ahorros.Cod_Ahorro NO-ERROR.
        IF NOT AVAIL(Ahorros) THEN DO:
            CREATE Ahorros.
            ASSIGN Ahorros.Agencia = TempLiq.Age
                   Ahorros.Nit = TempLiq.Nit
                   Ahorros.Cod_Ahorro = Pro_Ahorros.Cod_Ahorro
                   Ahorros.Cue_Ahorro = TempLiq.Nit
                   Ahorros.Fec_Apert = W_Fecha.
        END.

       CREATE Mov_Ahorros.
       ASSIGN Ahorros.Sdo_Dispon         = Ahorros.Sdo_Dispon + TempLiq.VrRev
              Ahorros.Estado             = 1
              Ahorros.Fec_UltTr          = W_Fecha
              Ahorros.Detalle_Estado     = 1
              Mov_Ahorros.Agencia        = TempLiq.Age
              Mov_Ahorros.Age_Destino    = TempLiq.Age
              Mov_Ahorros.Age_Fuente     = W_Agencia
              Mov_Ahorros.Cod_Ahorro     = Pro_Ahorros.Cod_Ahorro
              Mov_Ahorros.Cod_Operacion  = Pro_Ahorros.Cod_CobroLibreta
              Mov_Ahorros.Cue_Ahorros    = Ahorros.Cue_Ahorro
              Mov_Ahorros.Fecha          = W_Fecha
              Mov_Ahorros.Hora           = TIME 
              Mov_Ahorros.Nit            = TempLiq.Nit
              Mov_Ahorros.Num_Documento  = STRING(Comprobantes.Secuencia)
              Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Dispon
              Mov_Ahorros.Usuario        = W_Usuario
              Mov_Ahorros.Val_Efectivo   = TempLiq.VrRev.

   END.
END.  /*Fin Tx*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Prc_RevAportes  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Prc_RevAportes)
  THEN DELETE WIDGET W-Prc_RevAportes.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Prc_RevAportes  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY W_CmbOfi W_FecMes Msaje W_Cont Rs_Op W_Porc W_Val 
      WITH FRAME F_Proc IN WINDOW W-Prc_RevAportes.
  ENABLE RECT-314 RECT-315 BUTTON-5 W_CmbOfi Btn_Proc Btn_Contab Btn_Done Rs_Op 
         W_Porc W_Val 
      WITH FRAME F_Proc IN WINDOW W-Prc_RevAportes.
  {&OPEN-BROWSERS-IN-QUERY-F_Proc}
  VIEW W-Prc_RevAportes.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpCpte W-Prc_RevAportes 
PROCEDURE ImpCpte :
/*------------------------------------------------------------------------------
      Invocado desde ProcesoImprimir.
  Purpose:     Imprime el resumen contable de la liquidaciòn.
  ------------------------------------------------------------------------------*/
  DEFI VAR TotD   LIKE Mov_Contable.Db INIT 0.
  DEFI VAR TotC   LIKE Mov_Contable.Db INIT 0.
  DEFI VAR TTotD  LIKE Mov_Contable.Db INIT 0.
  DEFI VAR TTotC  LIKE Mov_Contable.Db INIT 0.
  
  DISPLAY SKIP(2)
            "  "
            STRING(W_Nom_Entidad,"X(45)") +
            "   "  +
            TRIM(W_Nom_Agencia,"X(25)")          FORMAT "X(120)" SKIP(1)
            "                   Comprobante Resumen Revalorizaciòn Aportes     Fecha del Cpte: " +
            STRING(W_FecMes,"99/99/9999")        FORMAT "X(120)"  SKIP
            "                   " + STRING(Comprobantes.Comprobante,"99") + "-" + 
                                   STRING(Comprobantes.Secuencia,"99999999") FORMAT "X(120)"  
            SKIP(1)
            "--------------------------------------------------------------------------------------------------------"
       WITH DOWN WIDTH 140 FRAME F1 USE-TEXT NO-LABELS STREAM-IO NO-BOX. 
  
  FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante   EQ Comprobantes.Comprobante
                          AND Mov_Contable.Num_Documento EQ Comprobantes.Secuencia
                          AND Mov_Contable.Fec_Contable  EQ W_Fecha NO-LOCK
                              BREAK BY Mov_Contable.Agencia BY Mov_Contable.Cuenta:
      ASSIGN TotD  = TotD  + Mov_Contable.Db
             TTotD = TTotD + Mov_Contable.Db
             TotC  = TotC  + Mov_Contable.Cr
             TTotC = TTotC + Mov_Contable.Cr.

      IF LAST-OF(Mov_Contable.Cuenta) THEN DO:
         FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Mov_Contable.Cuenta
                              AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
         DISPLAY Mov_Contable.Agencia   LABEL "Ag."
                 Mov_Contable.Cuenta    LABEL "Cta-Contable"
                 Cuentas.Nombre         LABEL "Descripciòn de la Cuenta" WHEN AVAIL(Cuentas)
                 TotD                   LABEL "TOTAL DEBITOS"  FORM "->>>>>>,>>>,>>9.99"
                 TotC                   LABEL "TOTAL CREDITOS" FORM "->>>>>>,>>>,>>9.99"
             WITH DOWN WIDTH 140 FRAME F21 USE-TEXT NO-LABELS STREAM-IO NO-BOX.

         ASSIGN TotD  = 0
                TotC  = 0.
      END.

  END.

  DISPLAY SKIP(1)
          "                     TOTAL FINAL------------>            ---------------- ----------------"
          SKIP
          "                                                 "
          TTotD      FORM "->>>>>>,>>>,>>9.99"
          TTotC      FORM "->>>>>>,>>>,>>9.99"
             WITH DOWN WIDTH 140 FRAME FT21T USE-TEXT NO-LABELS STREAM-IO NO-BOX.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proceso W-Prc_RevAportes 
PROCEDURE Proceso :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  DEFI VAR W_TotApo LIKE   Ahorros.SDo_Disponible INIT 0.
  DEFI VAR K        AS INTEG FORM "99".
    
  FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro EQ 4
                         AND Pro_Ahorros.Estado     EQ 1 NO-LOCK:
      FOR EACH Ahorros WHERE Ahorros.Agencia    GE W_OfiIni
                         AND Ahorros.Agencia    LE W_OfiFin 
                         AND Ahorros.Cod_Ahorro EQ Pro_Ahorros.Cod_Ahorro
                         AND Ahorros.Fec_Apert  LE DATE(12,31,YEAR(W_Fecha) - 1) NO-LOCK
                             BREAK BY Ahorros.Nit BY Ahorros.Estado:
          IF FIRST-OF(Ahorros.Nit) THEN DO:
             CREATE TempLiq.
             ASSIGN TempLiq.Age    = Ahorros.Agencia       
                    TempLiq.Nit    = Ahorros.Nit           
                    TempLiq.Cod    = Ahorros.Cod_Ahorro    
                    TempLiq.Cta    = Ahorros.Cue_Ahorros.   
          END.  
            
          ASSIGN TempLiq.SdoMes[12] = TempLiq.SdoMes[12]  + Ahorros.Sdo_Anuales[12]
                 W_Cont             = W_Cont + 1
                 W_Cont:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Cont).
            
          IF Rs_Op GT 1 THEN DO:
             DO K = MONTH(W_Fecha) TO 11:
                ASSIGN TempLiq.SdoMes[K] = TempLiq.SdoMes[K] + Ahorros.Sdo_Anuales[K].                        
             END.
                
             DO K = 1 TO MONTH(W_Fecha) - 1:
                ASSIGN TempLiq.SdoMes[K] = TempLiq.SdoMes[K]  + Ahorros.Sdo_AnualPerAnt[K].                        
             END.
          END.
      END.        
  END.
    
  FOR EACH TempLiq:
      IF Rs_Op EQ 1 AND W_Porc GT 0 THEN 
         ASSIGN TempLiq.SdoPro = TempLiq.SdoMes[12]
                TempLiq.PorcR  = W_Porc
                TempLiq.VrRev  = (TempLiq.SdoMes[12] * W_Porc) / 100.
      ELSE IF Rs_Op EQ 1 AND W_Val GT 0 THEN
         ASSIGN TempLiq.SdoPro = TempLiq.SdoMes[12]
                TempLiq.PorcR = (TempLiq.SdoMes[12] / W_Val) * 100
                TempLiq.VrRev = (TempLiq.SdoMes[12] * TempLiq.PorcR) / 100.   
      ELSE IF Rs_Op EQ 2 THEN DO:
         DO K = 1 TO 12:
            ASSIGN TempLiq.SdoPro = TempLiq.SdoPro + TempLiq.SdoMes[K].    
         END.

         IF W_Porc GT 0 THEN
            ASSIGN TempLiq.PorcR  = W_Porc
                   TempLiq.SdoPro = TempLiq.SdoPro / 12
                   TempLiq.VrRev  = (TempLiq.SdoPro * W_Porc) / 100. 
         ELSE 
            ASSIGN TempLiq.SdoPro = TempLiq.SdoPro / 12
                   TempLiq.PorcR  = (TempLiq.SdoPro / W_Val) * 100
                   TempLiq.VrRev  = (TempLiq.SdoPro * TempLiq.PorcR) / 100.
      END.
      ELSE DO:  
         IF W_Porc GT 0 THEN DO:
            ASSIGN TempLiq.PorcR  = W_Porc / 12.

            DO K = 1 TO 12:
               ASSIGN TempLiq.SdoPro = TempLiq.SdoPro + TempLiq.SdoMes[K]
                      TempLiq.VrRev  = TempLiq.VrRev  + (TempLiq.SdoMes[K] * (W_Porc / 12)) / 100.   
            END.
         END.
         ELSE DO K = 1 TO 12:
               ASSIGN TempLiq.SdoPro = TempLiq.SdoPro + TempLiq.SdoMes[K]
                      TempLiq.PorcR  = TempLiq.PorcR + ((TempLiq.SdoMes[K] / W_Val) * 100)
                      TempLiq.VrRev  = TempLiq.VrRev + (TempLiq.SdoMes[K] * 
                                                       (TempLiq.SdoMes[K] / W_Val)).   
         END.
      END.        
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Prc_RevAportes 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:  Imprime Informe con la Revalorizaciòn, si ok la Contabilizaciòn
            imprime el cpte resumen.
 ------------------------------------------------------------------------------*/
 {Incluido\RepEncabezado.I}

 IF W_SiContab THEN RUN ImpCpte.                                                                                                                                         
                                                                                                                                                   
 DEFI VAR TRev     LIKE TempLiq.SdoPro INIT 0.                                                                  
 DEFI VAR TTRev    LIKE TempLiq.SdoPro INIT 0.
                                                                                                                                                   
 DISPLAY SKIP(2)                                                                                                                                      
          "  "                                                                                                                                        
          STRING(W_Nom_Entidad,"X(45)") +                                                                                                             
          "   "  +                                                                                                                                    
          TRIM(W_Nom_Agencia,"X(25)")          FORMAT "X(120)" SKIP(1)                                                                                
          "                   Informe Proceso Revalorizaciòn Aportes     Fecha del Informe: " +                                                       
          STRING(W_FecMes,"99/99/9999")        FORMAT "X(120)"                                                                                        
          SKIP(1)                                                                                                                                     
          "---------------------------------------------------------------------------------------------------------"          
     WITH DOWN WIDTH 140 FRAME F1 USE-TEXT NO-LABELS STREAM-IO NO-BOX.                                                                                
                                                                                                                                                   
 FOR EACH TempLiq BREAK BY TempLiq.Age BY TempLiq.Nit:                                                                                                                                    
     ASSIGN TRev  = TRev  + TempLiq.VrRev
            TTRev = TTRev + TempLiq.VrRev.
                                                                                                                                                  
     DISPLAY                                                                                                                                          
          TempLiq.Age    LABEL "Ag."                                                                                                                  
          TempLiq.Nit    LABEL "Ced./Nit"                                                                                                                
          TempLiq.SdoPro LABEL "Sdo.Promedio/Base"             FORM "->>>>>,>>>,>>9.99" 
          TempLiq.PorcR  LABEL "% Porcentaje"                  FORM "->>>9.9999"
          TempLiq.VrRev  LABEL "Valor Revalorizaciòn"          FORM "->>>>,>>>,>>9.99"
        SKIP(0)                                                                                                                                       
        WITH DOWN WIDTH 140 FRAME F2 USE-TEXT NO-LABELS STREAM-IO NO-BOX.

     IF LAST-OF(TempLiq.Age) THEN DO:
        DISPLAY SKIP(1)                                                                                                                                      
           "                            AGENCIA TOTALES-----> "                                                                                           
           TRev          FORM "->>>>>,>>>,>>9.99"  SKIP(1)                                                                                                   
          WITH DOWN WIDTH 140 FRAME F4 USE-TEXT NO-LABELS STREAM-IO NO-BOX. 

        TRev = 0.
     END.
 END.                                                                                                                                                 
                                                                                                                                                   
 DISPLAY SKIP(2)                                                                                                                                      
         "                            GENERAL TOTALES-----> "                                                                                           
         TTRev          FORM "->>>>>,>>>,>>9.99"                                                                                                     
    WITH DOWN WIDTH 140 FRAME F3 USE-TEXT NO-LABELS STREAM-IO NO-BOX.                                                                                 
                                                                                                                                                   
                                                                                                                                                   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

