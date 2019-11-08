&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W_Cierre_Cartera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W_Cierre_Cartera 
CREATE WIDGET-POOL.

ON RETURN TAB.

{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I   "SHARED"}

DEFI VAR W_PrelProc AS LOG INIT FALSE.
DEFI VAR W_MesP AS INTEG FORM "99".
DEFI VAR W_AnoP AS INTEG FORM "9999".
DEFINE VAR WK_CtaProDeb AS CHARACTER FORMAT "X(12)".
DEFINE VAR WK_CtaProIntGto AS CHARACTER FORMAT "X(12)".
DEFINE VAR WK_CtaProCosGto AS CHARACTER FORMAT "X(12)".
DEFINE VAR W_NumCbt AS INTEGER.
DEFINE VAR W_Cbte AS INTEGER.

DEFI TEMP-TABLE TMov_Contable LIKE Mov_Contable.

DEFINE TEMP-TABLE TotCre
    FIELD Nit AS CHARACTER
    FIELD Tot AS DECIMAL
    INDEX nit IS PRIMARY nit.

DEFINE TEMP-TABLE TSdos
    FIELD Age AS INTEGER
    FIELD Cta AS CHARACTER
    FIELD Aju AS CHARACTER
    FIELD Id  AS CHARACTER FORM "X(2)" 
    FIELD Sdo AS DECIMAL
    FIELD SiC AS LOGICAL
    FIELD ctaGtoPrv AS CHARACTER
    INDEX age cta id.

DEFINE TEMP-TABLE Pro
    FIELD Agencia AS INTEGER
    FIELD Nit AS CHARACTER
    FIELD Cod_Credito AS INTEGER
    FIELD Pagare AS CHARACTER
    FIELD Num_Credito AS INTEGER
    FIELD Sdo_Capital AS DECIMAL

    /* oakley */

    FIELD Provision   LIKE Creditos.Provision
    FIELD ProvInt     LIKE Creditos.Provision
    FIELD ProvCos     LIKE Creditos.Provision
    FIELD Cal_Credito LIKE Creditos.Cod_Califica
    FIELD Cal_Cliente LIKE Clientes.Calificacion
    FIELD ApoDistribu LIKE Ahorros.Sdo_Disponible
    FIELD ValDefecto  LIKE Ahorros.Sdo_Disponible
    FIELD TotAportes  LIKE Ahorros.Sdo_Disponible
    FIELD Reestru     LIKE Clientes.Reestructurado
    FIELD ValGarant   LIKE Ahorros.Sdo_Disponible
    FIELD diamora     LIKE Creditos.Dias_atraso
    FIELD CuentaCon   LIKE Cuentas.Cuenta
    FIELD NEGOCIO     LIKE Pro_Creditos.Cod_otro
    FIELD categoria   LIKE creditos.categoria
    FIELD porc        LIKE CarteraVencida.Porc_Admisible.

  DEFINE TEMP-TABLE Tmp-Provis
   FIELD P_Age             LIKE Agencias.Agencia
   FIELD P_Tipo            LIKE Creditos.Tip_Credito
   FIELD P_CtaPro          LIKE Cuentas.Cuenta
   FIELD Cruce             LIKE Cuentas.Cuenta
   FIELD P_CodVar          LIKE Varios.Codigo
   FIELD P_Valor           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99".

  DEFINE TEMP-TABLE Tmp-ProvisDet
   FIELD P_Age             LIKE Agencias.Agencia
   FIELD P_CodCre          LIKE Creditos.Cod_Credito
   FIELD P_NumCre          LIKE Creditos.Num_Credito
   FIELD P_Nit             LIKE Clientes.Nit
   FIELD P_CodCal          LIKE Creditos.Cod_Califica
   FIELD P_CtaPro          LIKE Cuentas.Cuenta
   FIELD P_CodVar          LIKE Varios.Codigo
   FIELD P_Valor           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99".

  DEFINE TEMP-TABLE Tmp-ProvisInt
   FIELD P_Age             LIKE Agencias.Agencia
   FIELD P_Tipo            LIKE Creditos.Tip_Credito
   FIELD P_CtaPro          LIKE Cuentas.Cuenta
   FIELD P_CtaGto          LIKE Cuentas.Cuenta
   FIELD P_CtaIng          LIKE Cuentas.Cuenta
   FIELD P_CodVar          LIKE Varios.Codigo
   FIELD P_Valor           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99".

  DEFINE TEMP-TABLE Tmp-ProvisCos
   FIELD P_Age             LIKE Agencias.Agencia
   FIELD P_Tipo            LIKE Creditos.Tip_Credito
   FIELD P_CtaPro          LIKE Cuentas.Cuenta
   FIELD P_CtaIng          LIKE Cuentas.Cuenta
   FIELD P_CtaGto          LIKE Cuentas.Cuenta
   FIELD P_CodVar          LIKE Varios.Codigo
   FIELD P_Valor           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99".

DEFINE TEMP-TABLE Tmp-Calif
 FIELD C_Age             LIKE Agencias.Agencia
 FIELD C_Tipo            LIKE Creditos.Tip_Credito
 FIELD C_CtaCal          LIKE Cuentas.Cuenta
 FIELD C_CodVar          LIKE Varios.Codigo
 FIELD C_Valor           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99".

DEFINE TEMP-TABLE Tmp-CalifDet
 FIELD C_Age             LIKE Agencias.Agencia
 FIELD C_CtaCal          LIKE Cuentas.Cuenta
 FIELD C_CodVar          LIKE Varios.Codigo
 FIELD C_Nit             LIKE Clientes.Nit
 FIELD C_NumCre          LIKE Creditos.Num_Credito
 FIELD C_Valor           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99"
 INDEX C_age C_CtaCal c_Nit C_NumCre.

 

DEFINE TEMP-TABLE Tmp-CalifInt
 FIELD C_Age             LIKE Agencias.Agencia
 FIELD C_Tipo            LIKE Creditos.Tip_Credito
 FIELD C_CtaCal          LIKE Cuentas.Cuenta
 FIELD C_CodVar          LIKE Varios.Codigo
 FIELD C_Valor           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99".

DEFINE TEMP-TABLE Tmp-CalifConting
 FIELD C_Age             LIKE Agencias.Agencia
 FIELD C_Tipo            LIKE Creditos.Tip_Credito
 FIELD C_CtaCal          LIKE Cuentas.Cuenta
 FIELD Cruce             LIKE Cuentas.Cuenta
 FIELD C_CodVar          LIKE Varios.Codigo
 FIELD C_Valor           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99".

DEFINE TEMP-TABLE Tmp-CalifCos
 FIELD C_Age             LIKE Agencias.Agencia
 FIELD C_Tipo            LIKE Creditos.Tip_Credito
 FIELD C_CtaCal          LIKE Cuentas.Cuenta
 FIELD C_CodVar          LIKE Varios.Codigo
 FIELD C_Valor           AS   DECIMAL FORMAT "->>,>>>,>>>,>>>.99".

DEFI TEMP-TABLE TArrastre
     FIELD Ced LIKE Creditos.Nit
     FIELD tip LIKE creditos.tip_credito
     FIELD arrastre LIKE creditos.categoria
     FIELD califica     LIKE CREDitos.cod_califica
     INDEX x1 ced tip .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_CieCar

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-314 BUTTON-5 W_FecMes Btn_Proc Btn_Done ~
Btn_Prelim 
&Scoped-Define DISPLAYED-OBJECTS W_CmbOfi W_FecMes W_Cont Msaje 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 Msaje 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W_Cierre_Cartera AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Salir" 
     SIZE 11.43 BY 1.42
     BGCOLOR 8 .

DEFINE BUTTON Btn_Prelim 
     LABEL "&Procesar-Preliminar" 
     SIZE 25.43 BY 1.46 TOOLTIP "Procesar Preliminar sin Contabilizar".

DEFINE BUTTON Btn_Proc 
     LABEL "&Procesar" 
     SIZE 11.43 BY 1.46 TOOLTIP "Procesa y Contabiliza Cierre-Cartera".

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 5" 
     SIZE 11.43 BY 1.62.

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(30)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 43.14 BY 1 TOOLTIP "Agencias Activas"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Msaje AS CHARACTER FORMAT "X(100)":U 
     VIEW-AS FILL-IN 
     SIZE 58.57 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Cont AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.57 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FecMes AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Proceso" 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .81 TOOLTIP "Fecha Corte para el Proceso"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-314
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16.14 BY 6.12.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_CieCar
     W_CmbOfi AT ROW 2.35 COL 15 COLON-ALIGNED
     BUTTON-5 AT ROW 2.77 COL 68.29
     W_FecMes AT ROW 3.96 COL 44 COLON-ALIGNED
     Btn_Proc AT ROW 4.73 COL 68.29
     Btn_Done AT ROW 6.54 COL 68.29 HELP
          "Sale del proceso de Depreciación y Ajustes"
     Btn_Prelim AT ROW 8.69 COL 8.29
     W_Cont AT ROW 9.88 COL 49.43 COLON-ALIGNED NO-LABEL
     Msaje AT ROW 11.12 COL 7.72 NO-LABEL
     "Regist.Proceso" VIEW-AS TEXT
          SIZE 14.57 BY .88 AT ROW 8.96 COL 51.29
     "Ver- Fodun mayo 31-2011" VIEW-AS TEXT
          SIZE 30 BY .62 AT ROW 13.38 COL 52 WIDGET-ID 2
     RECT-314 AT ROW 2.27 COL 65.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 83.14 BY 13.27
         BGCOLOR 17 .


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
  CREATE WINDOW W_Cierre_Cartera ASSIGN
         HIDDEN             = YES
         TITLE              = "Cierre de Cartera, Programa W_Cierre_Cartera.W"
         HEIGHT             = 13.27
         WIDTH              = 83.14
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 83.57
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 83.57
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
/* SETTINGS FOR WINDOW W_Cierre_Cartera
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_CieCar
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN Msaje IN FRAME F_CieCar
   NO-ENABLE ALIGN-L 2                                                  */
/* SETTINGS FOR COMBO-BOX W_CmbOfi IN FRAME F_CieCar
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Cont IN FRAME F_CieCar
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W_Cierre_Cartera)
THEN W_Cierre_Cartera:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_CieCar
/* Query rebuild information for FRAME F_CieCar
     _Query            is NOT OPENED
*/  /* FRAME F_CieCar */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W_Cierre_Cartera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Cierre_Cartera W_Cierre_Cartera
ON END-ERROR OF W_Cierre_Cartera /* Cierre de Cartera, Programa W_Cierre_Cartera.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Cierre_Cartera W_Cierre_Cartera
ON WINDOW-CLOSE OF W_Cierre_Cartera /* Cierre de Cartera, Programa W_Cierre_Cartera.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W_Cierre_Cartera
ON CHOOSE OF Btn_Done IN FRAME F_CieCar /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Prelim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Prelim W_Cierre_Cartera
ON CHOOSE OF Btn_Prelim IN FRAME F_CieCar /* Procesar-Preliminar */
DO:
  MESSAGE "Proceso Preliminar del Cierre-Cartera."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  ASSIGN W_PrelProc = FALSE.

  SESSION:SET-WAIT-STATE("GENERAL").
      FOR EACH TMov_Contable. DELETE TMov_Contable. END.
      FOR EACH TSdos:         DELETE TSdos.         END.
      FOR EACH Pro:           DELETE Pro.           END.
      FOR EACH TotCre:        DELETE TotCre.        END.
      FOR EACH Tmp-Provis:    DELETE Tmp-Provis.    END.
      FOR EACH Tmp-ProvisInt: DELETE Tmp-ProvisInt. END.
      FOR EACH Tmp-ProvisCos: DELETE Tmp-ProvisCos. END.
      FOR EACH Tmp-ProvisDet: DELETE Tmp-ProvisDet. END.
      FOR EACH Tmp-Calif:     DELETE Tmp-Calif.     END.
      FOR EACH Tmp-CalifDet:  DELETE Tmp-CalifDet.  END.
      FOR EACH Tmp-CalifInt:  DELETE Tmp-CalifInt.  END.
      FOR EACH Tmp-CalifConting: DELETE Tmp-CalifConting. END.
      FOR EACH Tmp-CalifCos:     DELETE Tmp-CalifCos.     END.

      RUN Proceso NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         MESSAGE "Proceso Presentó Errores....Proceso cancelado."
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN.
      END.
      ELSE
         MESSAGE "Finalizó Proceso Preliminar Cierre-Cartera."
             VIEW-AS ALERT-BOX INFO BUTTONS OK.

  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Proc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Proc W_Cierre_Cartera
ON CHOOSE OF Btn_Proc IN FRAME F_CieCar /* Procesar */
DO:
    MESSAGE "Segura(o) de Procesar Cierre-Cartera...?" SKIP
            "Para el Mes : " W_MesP SKIP
            "Para el Año : " W_AnoP
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "CONFIRMAR PROCESO" UPDATE W_SiNoP AS LOGICAL.

    IF NOT W_SiNoP THEN
        RETURN.

    W_PrelProc = TRUE.

    SESSION:SET-WAIT-STATE("GENERAL").
    
    EMPTY TEMP-TABLE TMov_Contable.
    EMPTY TEMP-TABLE TSdos.
    EMPTY TEMP-TABLE Pro.
    EMPTY TEMP-TABLE TotCre.
    EMPTY TEMP-TABLE Tmp-Provis.
    EMPTY TEMP-TABLE Tmp-ProvisInt.
    EMPTY TEMP-TABLE Tmp-ProvisCos.
    EMPTY TEMP-TABLE Tmp-ProvisDet.
    EMPTY TEMP-TABLE Tmp-Calif.
    EMPTY TEMP-TABLE Tmp-CalifDet.

    FOR EACH Tmp-CalifInt:
        DELETE Tmp-CalifInt.
    END.

    FOR EACH Tmp-CalifConting:
        DELETE Tmp-CalifConting.
    END.

    FOR EACH Tmp-CalifCos:
        DELETE Tmp-CalifCos.
    END.

    RUN Proceso NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Proceso Presentó Errores....Proceso cancelado."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        W_PrelProc = FALSE.

        RETURN.
    END.
    ELSE DO:
        RUN ProvisionGeneral.

        MESSAGE "Finalizó Proceso Cierre-Cartera."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        FOR EACH agencias WHERE agencias.agencia >= w_ofiIni
                            AND agencias.agencia <= w_ofiFin NO-LOCK:
            FIND FIRST procDia WHERE procDia.agencia = agencias.agencia
                                 AND procDia.cod_proceso = 15
                                 AND fecha_proc = w_fecha NO-ERROR.
            IF NOT AVAILABLE procDia THEN DO:
                CREATE procDia.
                ASSIGN procDia.agencia = agencias.agencia
                       procDia.cod_proceso = 15
                       procDia.fecha_Proc = w_fecha
                       procDia.nombre = "Cierre de Cartera"
                       procDia.usuario = w_usuario.
            END.

            procDia.estado = 2.
        END.
    END.

    W_PrelProc = FALSE.

    SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W_Cierre_Cartera
ON CHOOSE OF BUTTON-5 IN FRAME F_CieCar /* Button 5 */
DO:
  RUN W-InfDia.R.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfi W_Cierre_Cartera
ON VALUE-CHANGED OF W_CmbOfi IN FRAME F_CieCar /* Agencia */
DO:
    IF INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)) EQ 0 THEN DO:
        FIND FIRST Agencias WHERE Agencias.Agencia GT 0
                              AND Agencias.Estado  NE 3 NO-LOCK NO-ERROR.
        IF AVAILABLE Agencias THEN 
            W_OfiIni = Agencias.Agencia.

        FIND LAST Agencias WHERE Agencias.Agencia GT 0
                             AND Agencias.Estado  NE 3 NO-LOCK NO-ERROR.
        IF AVAILABLE Agencias THEN
            W_OfiFin = Agencias.Agencia.
    END.
    ELSE
        ASSIGN W_OfiIni = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3))
               W_OfiFin = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecMes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecMes W_Cierre_Cartera
ON LEAVE OF W_FecMes IN FRAME F_CieCar /* Fecha Proceso */
DO:
   ASSIGN W_FecMes
          W_MesP   = MONTH(W_FecMes) 
          W_AnoP   = YEAR(W_FecMes). 

   IF W_FecMes GT W_Fecha THEN
      MESSAGE "Revise por favor...La fecha es Superior a la de Hoy...?"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W_Cierre_Cartera 


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
         W_FecMes:SCREEN-VALUE IN FRAME F_CieCar = STRING(W_Fecha)
         W_FecMes
         W_MesP   = MONTH(W_FecMes)
         W_AnoP   = YEAR(W_FecMes).
                  
  FOR EACH Agencias WHERE Agencias.Estado  NE 3 
                      AND Agencias.Agencia GT 0 NO-LOCK:
      W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"999") + 
                        "-" + STRING(Agencias.Nombre,"X(25)")).      
  END.              
     
  ASSIGN W_CmbOfi:SCREEN-VALUE = "000 CONSOLIDADO".
     
  APPLY "VALUE-CHANGED" TO W_CmbOfi.


  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asigna_linea W_Cierre_Cartera 
PROCEDURE asigna_linea :
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuscarSdoCta W_Cierre_Cartera 
PROCEDURE BuscarSdoCta :
DEFINE INPUT PARAMETER CAge AS INTEGER.
DEFINE INPUT PARAMETER CCta AS CHARACTER.
DEFINE OUTPUT PARAMETER SFin AS DECIMAL.

DEFINE VAR I AS INTEGER.

FIND FIRST cuentas WHERE cuentas.cuenta = CCta NO-LOCK NO-ERROR.
IF NOT AVAILABLE cuentas THEN DO:
    MESSAGE "La cuenta" Ccta "para la línea de crédito" pro_creditos.cod_credito SKIP
            "no se encuentra matriculada dentro del PUC. El proceso no se suspenderá" SKIP
            "pero debe reportar este error al Administrador del Sistema para su" SKIP
            "corrección."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    RETURN.
END.

FOR EACH Sal_Cuenta WHERE Sal_Cuenta.Agencia = CAge
                      AND Sal_Cuenta.Cuenta = CCta
                      AND Sal_Cuenta.Ano = W_AnoP NO-LOCK:
    SFin = Sfin + Sal_Cuenta.Sal_Inicial.

    DO I = 1 TO W_MesP BY 1:
        IF Cuentas.Naturaleza EQ "DB" THEN
            SFin = SFin + Sal_Cuenta.DB[I] - Sal_Cuenta.Cr[I].
        ELSE
            SFin = SFin - Sal_Cuenta.DB[I] + Sal_Cuenta.Cr[I].
    END.
END.

IF SUBSTRING(cuentas.cuenta,1,4) = "1445" OR
   SUBSTRING(cuentas.cuenta,1,4) = "1446" OR
   SUBSTRING(cuentas.cuenta,1,4) = "1471" OR
   SUBSTRING(cuentas.cuenta,1,4) = "1472" OR
   SUBSTRING(cuentas.cuenta,1,6) = "166099" THEN
    Sfin = Sfin * -1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contab_Calificar_Capital W_Cierre_Cartera 
PROCEDURE Contab_Calificar_Capital :
DEFINE VAR CtaCre AS CHARACTER.
DEFINE VAR TCalif AS DECIMAL.
DEFINE VAR SdoCta AS DECIMAL.

FOR EACH Tmp-Calif BREAK BY Tmp-Calif.C_Age
                         BY Tmp-Calif.C_CtaCal:
    IF FIRST-OF(Tmp-Calif.C_Age) THEN DO:
        FIND FIRST Varios WHERE Varios.Tipo = 10
                            AND Varios.Codigo = Tmp-Calif.C_CodVar NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Varios THEN DO:
            MESSAGE "No se ha encontrado la configuración en varios para" SKIP
                    "extractar el tipo de comprobante para la contabilización" SKIP
                    "de la Calificación de cartera para la agencia:" Tmp-Calif.C_Age SKIP
                    "No se creará comprobante contable para esta agencia" SKIP(2)
                    "Revise la configuración de varios para el código:" Tmp-Calif.C_CodVar
                VIEW-AS ALERT-BOX ERROR.

            NEXT.
        END.
        ELSE
            W_Cbte = Varios.Comprobante.

        FIND FIRST Comprobantes WHERE Comprobantes.Agencia = Tmp-Calif.C_Age
                                  AND Comprobantes.Comprobante = W_Cbte NO-ERROR.
        IF NOT AVAILABLE Comprobantes THEN DO:
            IF LOCKED Comprobantes THEN
                RETRY.

            IF ERROR-STATUS:ERROR THEN DO:
                MESSAGE "Error al contabilizar la Calificacion" SKIP
                        "Agencia:" Tmp-Calif.C_Age SKIP
                        "Comprobante:" W_Cbte
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.
        END.

        ASSIGN W_NumCbt = Comprobantes.Secuencia + 1
               Comprobantes.Secuencia = Comprobantes.Secuencia + 1
               TCalif = 0.

        FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
    END.

    TCalif = TCalif + Tmp-Calif.C_Valor.

    IF LAST-OF(Tmp-Calif.C_CtaCal) THEN DO:
        IF TCalif > 0 THEN DO:
            SdoCta = 0.
            
            FIND FIRST TSdos WHERE TSdos.Age = Tmp-Calif.C_Age
                               AND TSdos.Cta = Tmp-Calif.C_CtaCal
                               AND TSdos.Id = "CC"
                               AND NOT TSdos.SiC NO-ERROR.
            IF AVAILABLE(TSdos) AND TSdos.Sdo > TCalif THEN
                ASSIGN CtaCre = Tmp-Calif.C_CtaCal
                       SdoCta = TCalif - TSdos.Sdo
                       TSdos.SiC = TRUE.
            ELSE
                IF AVAILABLE(TSdos) AND TSdos.Sdo < TCalif THEN
                    ASSIGN CtaCre = Tmp-Calif.C_CtaCal
                           SdoCta = TCalif - TSdos.Sdo
                           TSdos.SiC = TRUE.
                ELSE
                    IF AVAILABLE(TSdos) THEN
                        TSdos.SiC = TRUE.
                    ELSE
                        IF NOT AVAILABLE(TSdos) THEN
                            ASSIGN CtaCre = Tmp-Calif.C_CtaCal
                                   SdoCta = TCalif.

            IF SdoCta <> 0 THEN
                RUN Contab_PartNuevas (INPUT Tmp-Calif.C_Age,
                                       INPUT CtaCre,
                                       INPUT "ReclasificaciónCapital",
                                       INPUT SdoCta) NO-ERROR.
        END.

        TCalif = 0.
    END.

    IF LAST-OF(Tmp-Calif.C_Age) THEN DO:
        FOR EACH TSdos WHERE TSdos.Age = Tmp-Calif.C_Age
                         AND NOT TSdos.SiC
                         AND TSdos.Id = "CC"
                         AND TSdos.Sdo <> 0:
            RUN Contab_PartNuevas (INPUT Tmp-Calif.C_Age,
                                   INPUT TSdos.Cta,
                                   INPUT "Ajuste Reclasif.K",
                                   INPUT TSdos.Sdo * -1) NO-ERROR.

            TSdos.SiC = TRUE.
        END.
    END.
END.

FOR EACH TSdos WHERE NOT TSdos.SiC
                 AND TSdos.Id = "CC"
                 AND TSdos.Sdo <> 0:
    RUN Contab_PartNuevas (INPUT TSdos.Age,
                           INPUT TSdos.Cta,
                           INPUT "Ajuste Sin K",
                           INPUT TSdos.Sdo * -1) NO-ERROR.

    TSdos.SiC = TRUE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contab_Calificar_Conting W_Cierre_Cartera 
PROCEDURE Contab_Calificar_Conting :
/*------------------------------------------------------------------------------
  Purpose:   Contabiliza La ReClasificacion de Intereses-Contingentes.  
  Notes:     El cpte es resumen por Oficina y cuenta.  
  Marzo 7/06 GAER
------------------------------------------------------------------------------*/
  DEFINE VAR W_Des    AS CHARACTER FORMAT "X(30)".
  DEFINE VAR CtaCre   LIKE Cuentas.Cuenta.
  DEFI   VAR TCalif   LIKE Sal_Cuenta.Sal_Inicial INIT 0.
  DEFI   VAR SdoCta   LIKE Sal_Cuenta.Sal_Inicial.
        
  FOR EACH Tmp-CalifConting BREAK BY Tmp-CalifConting.C_Age BY Tmp-CalifConting.C_CtaCal:
                                                          /*Contab.de Clasific.Interes-Contingente*/
      IF FIRST-OF(Tmp-CalifConting.C_Age) THEN DO:
         FIND Varios WHERE Varios.Tipo EQ 10 AND Varios.Codigo EQ Tmp-CalifConting.C_CodVar NO-LOCK NO-ERROR.    
         IF NOT AVAILABLE Varios THEN DO:                                                                   
            MESSAGE "No se ha encontrado la configuración en varios para" SKIP                             
                  "extractar el tipo de comprobante para la contabilización" SKIP                        
                  "de la Calificación de Contingentes para la agencia: " Tmp-CalifConting.C_Age SKIP                   
                  "no se creará comprobante contable para esta agencia" SKIP(2)                                                                      
                  "revise la configuración de varios para el código: " Tmp-CalifConting.C_CodVar 
               VIEW-AS ALERT-BOX ERROR.                             
            NEXT.                                                                                          
         END.                                                                                               
         ELSE ASSIGN W_Des  = "ReClasif.Int-Contingentes"                                                 
                     W_Cbte = Varios.Comprobante.   
                                                                   
         FIND Comprobantes WHERE Comprobantes.Agencia     EQ Tmp-CalifConting.C_Age AND                                           
                                 Comprobantes.Comprobante EQ W_Cbte NO-ERROR.                                               
         IF NOT AVAILABLE Comprobantes THEN DO:                                                          
            IF LOCKED Comprobantes THEN RETRY.                                                           
            IF ERROR-STATUS:ERROR THEN DO:                                                               
               MESSAGE "Error al contabilizar la Calificacion" SKIP                                         
                       "Agencia: " Tmp-CalifConting.C_Age SKIP                                                 
                       "Comprobante: " W_Cbte VIEW-AS ALERT-BOX ERROR.                                                         
              RETURN ERROR.                                                                              
            END.                                                                                         
         END.  

         ASSIGN W_NumCbt               = Comprobantes.Secuencia + 1                                      
                Comprobantes.Secuencia = Comprobantes.Secuencia + 1
                TCalif                 = 0.

         FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
      END.

      TCalif = TCalif + Tmp-CalifConting.C_Valor.

      IF LAST-OF(Tmp-CalifConting.C_CtaCal) THEN DO:
         IF TCalif GT 0 THEN DO:
            SdoCta = 0.
            FIND FIRST TSdos WHERE TSdos.Age     EQ Tmp-CalifConting.C_Age
                               AND TSdos.Cta     EQ Tmp-CalifConting.C_CtaCal
                               AND TSdos.Id      EQ "IC"
                               AND NOT TSdos.SiC NO-ERROR.
            IF AVAIL(TSdos) AND TSdos.Sdo GT TCalif THEN        /*Debe restar de Sdos*/
               ASSIGN CtaCre    = Tmp-CalifConting.C_CtaCal
                      SdoCta    = TCalif - TSdos.Sdo   /*Para que el Ajuste sea negativo, o sea al Haber*/
                      TSdos.SiC = TRUE.    
            ELSE IF AVAIL(TSdos) AND TSdos.Sdo LT TCalif THEN   /*Debe sumar al Sdo*/
                ASSIGN CtaCre    = Tmp-CalifConting.C_CtaCal
                       SdoCta    = TCalif - TSdos.Sdo   /*Ajuste Positivo va para el DEBE*/
                       TSdos.SiC = TRUE.
            ELSE IF AVAIL(TSdos) THEN                /*Son iguales no hay asiento*/
                TSdos.SiC = TRUE. 
            ELSE IF NOT AVAIL(TSdos) THEN            /*Ajuste Se debe crear al DEBE */
               ASSIGN CtaCre = Tmp-CalifConting.C_CtaCal
                      SdoCta = TCalif.
           
            IF SdoCta NE 0 THEN DO:
               RUN Contab_PartNuevas (INPUT Tmp-CalifConting.C_Age,CtaCre,W_Des,SdoCta) NO-ERROR.
               RUN Contab_PartNuevas (INPUT Tmp-CalifConting.C_Age,Tmp-CalifConting.Cruce,W_Des,SdoCta * -1) NO-ERROR.
            END.
         END.

         TCalif = 0.
      END.

      IF LAST-OF(Tmp-CalifConting.C_Age) THEN DO:  /*Cancela Las Ctas que ya no tienen Sdos, No ajustadas*/
         FOR EACH TSdos WHERE TSdos.Age EQ Tmp-CalifConting.C_Age AND NOT TSdos.SiC 
                          AND TSdos.Id  EQ "IC"                   AND TSdos.Sdo NE 0:
             RUN Contab_PartNuevas (INPUT Tmp-CalifConting.C_Age,TSdos.Cta,"Ajuste Calif.Conting.",TSdos.Sdo * -1) NO-ERROR.
             RUN Contab_PartNuevas (INPUT Tmp-CalifConting.C_Age,Tmp-CalifConting.Cruce,"Ajuste Calif.Conting",TSdos.Sdo) NO-ERROR.
             ASSIGN TSdos.SiC = TRUE.
         END.
      END.
  END.

  FOR EACH TSdos WHERE NOT TSdos.SiC AND TSdos.Id EQ "IC" AND TSdos.Sdo NE 0:
      RUN Contab_PartNuevas (INPUT TSdos.Age,TSdos.Cta,"Ajuste Final Conting",TSdos.Sdo * -1) NO-ERROR.             
      RUN Contab_PartNuevas (INPUT TSdos.Age,TSdos.Aju,"Ajuste Final Conting",TSdos.Sdo) NO-ERROR.        
      ASSIGN TSdos.SiC = TRUE.                                                                            
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contab_Calificar_Costas W_Cierre_Cartera 
PROCEDURE Contab_Calificar_Costas :
/*------------------------------------------------------------------------------
  Purpose:   Contabiliza La ReClasificacion de las Costas.  
  Notes:     El cpte es resumen por Oficina y cuenta.  
  Feb.16/06 GAER
------------------------------------------------------------------------------*/
  DEFINE VAR W_Des    AS CHARACTER FORMAT "X(30)".
  DEFINE VAR CtaCre   LIKE Cuentas.Cuenta.
  DEFI   VAR TCalif   LIKE Sal_Cuenta.Sal_Inicial INIT 0.
  DEFI   VAR SdoCta   LIKE Sal_Cuenta.Sal_Inicial.
        
  FOR EACH Tmp-CalifCos BREAK BY Tmp-CalifCos.C_Age BY Tmp-CalifCos.C_CtaCal: /*Contab.de la Clasific.Costas*/
      IF FIRST-OF(Tmp-CalifCos.C_Age) THEN DO:
         FIND Varios WHERE Varios.Tipo EQ 10 AND Varios.Codigo EQ Tmp-CalifCos.C_CodVar NO-LOCK NO-ERROR.    
         IF NOT AVAILABLE Varios THEN DO:                                                                   
            MESSAGE "No se ha encontrado la configuración en varios para" SKIP                             
                  "extractar el tipo de comprobante para la contabilización" SKIP                        
                  "de la Calificación de cartera para la agencia: " Tmp-CalifCos.C_Age SKIP                   
                  "no se creará comprobante contable para esta agencia" SKIP(2)                                                                      
                  "revise la configuración de varios para el código: " Tmp-CalifCos.C_CodVar 
               VIEW-AS ALERT-BOX ERROR.                             
            NEXT.                                                                                          
         END.                                                                                               
         ELSE ASSIGN W_Des  = "ReClasif.Costas"                                                 
                     W_Cbte = Varios.Comprobante.   
                                                                   
         FIND Comprobantes WHERE Comprobantes.Agencia     EQ Tmp-CalifCos.C_Age AND                                           
                                 Comprobantes.Comprobante EQ W_Cbte NO-ERROR.                                               
         IF NOT AVAILABLE Comprobantes THEN DO:                                                          
            IF LOCKED Comprobantes THEN RETRY.                                                           
            IF ERROR-STATUS:ERROR THEN DO:                                                               
               MESSAGE "Error al contabilizar la Calificacion" SKIP                                         
                       "Agencia: " Tmp-CalifCos.C_Age SKIP                                                 
                       "Comprobante: " W_Cbte VIEW-AS ALERT-BOX ERROR.                                                         
              RETURN ERROR.                                                                              
            END.                                                                                         
         END.  

         ASSIGN W_NumCbt               = Comprobantes.Secuencia + 1                                      
                Comprobantes.Secuencia = Comprobantes.Secuencia + 1
                TCalif                 = 0.

         FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
      END.

      TCalif = TCalif + Tmp-CalifCos.C_Valor.

      IF LAST-OF(Tmp-CalifCos.C_CtaCal) THEN DO:
         IF TCalif GT 0 THEN DO:
            SdoCta = 0.
            FIND FIRST TSdos WHERE TSdos.Age     EQ Tmp-CalifCos.C_Age
                               AND TSdos.Cta     EQ Tmp-CalifCos.C_CtaCal
                               AND TSdos.Id      EQ "CO"
                               AND NOT TSdos.SiC NO-ERROR.
            IF AVAIL(TSdos) AND TSdos.Sdo GT TCalif THEN
               ASSIGN CtaCre    = Tmp-CalifCos.C_CtaCal
                      SdoCta    = TCalif - TSdos.Sdo   /*Para que el Ajuste sea negativo, o sea al Haber*/
                      TSdos.SiC = TRUE.    
            ELSE IF AVAIL(TSdos) AND TSdos.Sdo LT TCalif THEN
                ASSIGN CtaCre    = Tmp-CalifCos.C_CtaCal
                       SdoCta    = TCalif - TSdos.Sdo /*Ajuste Positivo va para el DEBE*/
                       TSdos.SiC = TRUE.
            ELSE IF AVAIL(TSdos) THEN                /*Son iguales no hay asiento*/
                TSdos.SiC = TRUE. 
            ELSE IF NOT AVAIL(TSdos) THEN           /*Ajuste Se debe crear al DEBE */
               ASSIGN CtaCre = Tmp-CalifCos.C_CtaCal
                      SdoCta = TCalif.
           
            IF SdoCta NE 0 THEN 
               RUN Contab_PartNuevas (INPUT Tmp-CalifCos.C_Age,CtaCre,W_Des,SdoCta) NO-ERROR.
         END.

         TCalif = 0.
      END.

      IF LAST-OF(Tmp-CalifCos.C_Age) THEN DO:  /*Cancela Las Ctas que ya no tienen Sdo-Costas, No ajustadas*/
         FOR EACH TSdos WHERE TSdos.Age EQ Tmp-CalifCos.C_Age AND NOT TSdos.SiC 
                          AND TSdos.Id  EQ "CO"               AND TSdos.Sdo NE 0:
             RUN Contab_PartNuevas (INPUT Tmp-CalifCos.C_Age,TSdos.Cta,"Ajuste Reclasif.Costas",TSdos.Sdo * -1) NO-ERROR.
             ASSIGN TSdos.SiC = TRUE.
         END.
      END.
  END.

  FOR EACH TSdos WHERE NOT TSdos.SiC AND TSdos.Id EQ "CO" AND TSdos.Sdo NE 0:
      RUN Contab_PartNuevas (INPUT TSdos.Age,TSdos.Cta,"Ajuste sin Costas",TSdos.Sdo * -1) NO-ERROR.
      ASSIGN TSdos.SiC = TRUE.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contab_Calificar_Interes W_Cierre_Cartera 
PROCEDURE Contab_Calificar_Interes :
DEFINE VAR W_Des AS CHARACTER FORMAT "X(30)".
DEFINE VAR CtaCre LIKE Cuentas.Cuenta.
DEFI VAR TCalif LIKE Sal_Cuenta.Sal_Inicial INIT 0.
DEFI VAR SdoCta LIKE Sal_Cuenta.Sal_Inicial.

FOR EACH Tmp-CalifInt BREAK BY Tmp-CalifInt.C_Age BY Tmp-CalifInt.C_CtaCal: /*Contab.de la Clasific.Interes*/
    IF FIRST-OF(Tmp-CalifInt.C_Age) THEN DO:
        FIND FIRST Varios WHERE Varios.Tipo EQ 10 AND Varios.Codigo EQ Tmp-CalifInt.C_CodVar NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Varios THEN DO:
            MESSAGE "No se ha encontrado la configuración en varios para extractar el tipo de comprobante para la contabilización de la Calificación de cartera para la agencia:" Tmp-CalifInt.C_Age
                    "No se creará comprobante contable para esta agencia. Revise la configuración de varios para el código:" Tmp-CalifInt.C_CodVar
                VIEW-AS ALERT-BOX ERROR.

            NEXT.
        END.
        ELSE
            ASSIGN W_Des  = "ReClasif.Intereses"
                   W_Cbte = Varios.Comprobante.

        FIND FIRST Comprobantes WHERE Comprobantes.Agencia EQ Tmp-CalifInt.C_Age
                                  AND Comprobantes.Comprobante EQ W_Cbte NO-ERROR.
        IF NOT AVAILABLE Comprobantes THEN DO:
            IF LOCKED Comprobantes THEN
                RETRY.

            IF ERROR-STATUS:ERROR THEN DO:
                MESSAGE "Error al contabilizar la Calificacion" SKIP
                        "Agencia:" Tmp-CalifInt.C_Age SKIP
                        "Comprobante: " W_Cbte
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.
        END.

        ASSIGN W_NumCbt = Comprobantes.Secuencia + 1
               Comprobantes.Secuencia = Comprobantes.Secuencia + 1
               TCalif = 0.

        FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
    END.

    TCalif = TCalif + Tmp-CalifInt.C_Valor.

    IF LAST-OF(Tmp-CalifInt.C_CtaCal) THEN DO:
        IF TCalif GT 0 THEN DO:
            SdoCta = 0.

            FIND FIRST TSdos WHERE TSdos.Age EQ Tmp-CalifInt.C_Age
                               AND TSdos.Cta EQ Tmp-CalifInt.C_CtaCal
                               AND TSdos.Id EQ "CI"
                               AND NOT TSdos.SiC NO-ERROR.
            IF AVAIL(TSdos) AND TSdos.Sdo GT TCalif THEN
                ASSIGN CtaCre = Tmp-CalifInt.C_CtaCal
                       SdoCta = TCalif - TSdos.Sdo   /*Para que el Ajuste sea negativo, o sea al Haber*/
                       TSdos.SiC = TRUE.
            ELSE
                IF AVAIL(TSdos) AND TSdos.Sdo LT TCalif THEN
                    ASSIGN CtaCre = Tmp-CalifInt.C_CtaCal
                           SdoCta = TCalif - TSdos.Sdo   /*Ajuste Positivo va para el DEBE*/
                           TSdos.SiC = TRUE.
                ELSE
                    IF AVAIL(TSdos) THEN                /*Son iguales no hay asiento*/
                        TSdos.SiC = TRUE.
                    ELSE
                        IF NOT AVAIL(TSdos) THEN           /*Ajuste Se debe crear al DEBE */
                            ASSIGN CtaCre = Tmp-CalifInt.C_CtaCal
                                   SdoCta = TCalif.

            IF SdoCta NE 0 THEN
                RUN Contab_PartNuevas (INPUT Tmp-CalifInt.C_Age,
                                       INPUT CtaCre,
                                       INPUT W_Des,SdoCta) NO-ERROR.
        END.

        TCalif = 0.
    END.

    IF LAST-OF(Tmp-CalifInt.C_Age) THEN DO:  /*Cancela Las Ctas que ya no tienen Sdo-Intereses, No ajustadas*/
        FOR EACH TSdos WHERE TSdos.Age EQ Tmp-CalifInt.C_Age
                         AND NOT TSdos.SiC
                         AND TSdos.Id  EQ "CI"
                         AND TSdos.Sdo NE 0:
            RUN Contab_PartNuevas (INPUT Tmp-CalifInt.C_Age,
                                   INPUT TSdos.Cta,
                                   INPUT "Ajuste Reclasif.Int",
                                   INPUT TSdos.Sdo * -1) NO-ERROR.

            TSdos.SiC = TRUE.
        END.
    END.
END.

FOR EACH TSdos WHERE NOT TSdos.SiC
                 AND TSdos.Id EQ "CI"
                 AND TSdos.Sdo NE 0:
    RUN Contab_PartNuevas (INPUT TSdos.Age,
                           INPUT TSdos.Cta,
                           INPUT "Ajuste sin Int.",
                           INPUT TSdos.Sdo * -1) NO-ERROR.

    TSdos.SiC = TRUE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contab_PartNuevas W_Cierre_Cartera 
PROCEDURE Contab_PartNuevas :
DEFINE INPUT PARAMETER W_AgeCon AS INTEGER.
DEFINE INPUT PARAMETER W_Cta AS CHARACTER.
DEFINE INPUT PARAMETER W_Descripcion AS CHARACTER FORMAT "X(30)".
DEFINE INPUT PARAMETER W_Valor AS DECIMAL.

IF W_PrelProc THEN DO:
    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia = W_AgeCon
           Mov_Contable.Comprobante = W_Cbte
           Mov_Contable.Cuenta = W_Cta
           Mov_Contable.Fec_Contable = W_FecMes
           Mov_Contable.Comentario = W_Descripcion
           Mov_Contable.Usuario = W_Usuario
           Mov_contable.Nit = ""
           Mov_Contable.Cen_Costos = 999
           Mov_Contable.Destino = W_Agencia
           Mov_Contable.Num_Documento = INTEGER(W_NumCbt)
           Mov_Contable.Doc_Referencia = ""
           Mov_contable.Enlace = ""
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Hora = TIME
           Mov_Contable.Estacion = W_Estacion
           Mov_Contable.DB = W_Valor NO-ERROR.

    IF W_Valor LT 0 THEN
        ASSIGN Mov_Contable.DB = 0
               Mov_Contable.Cr = W_Valor * -1.
END.
ELSE DO:
    CREATE TMov_Contable.
    ASSIGN TMov_Contable.Agencia = W_AgeCon
           TMov_Contable.Comprobante = W_Cbte
           TMov_Contable.Cuenta = W_Cta
           TMov_Contable.Fec_Contable = W_FecMes
           TMov_Contable.Comentario = W_Descripcion
           TMov_Contable.Usuario = W_Usuario
           TMov_Contable.Nit = ""
           TMov_Contable.Cen_Costos = 999
           TMov_Contable.Destino = W_Agencia
           TMov_Contable.Num_Documento = INTEGER(W_NumCbt)
           TMov_Contable.Doc_Referencia = ""
           TMov_Contable.Enlace = ""
           TMov_Contable.Fec_Grabacion = TODAY
           TMov_Contable.Hora = TIME
           TMov_Contable.Estacion = W_Estacion
           TMov_Contable.DB = W_Valor NO-ERROR.

     IF W_Valor < 0 THEN
         ASSIGN TMov_Contable.DB = 0
                TMov_Contable.Cr = W_Valor * -1.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contab_Provision_Capital W_Cierre_Cartera 
PROCEDURE Contab_Provision_Capital :
DEFINE VAR W_Des AS CHARACTER FORMAT "X(30)".
DEFINE VAR CtaCre LIKE Cuentas.Cuenta.
DEFI VAR TCalif LIKE Sal_Cuenta.Sal_Inicial INIT 0.
DEFI VAR SdoCta LIKE Sal_Cuenta.Sal_Inicial.

/* Genera tabla para revisión */
OUTPUT TO c:\info_Fodun\provis.d.
FOR EACH Tmp-Provis BREAK BY Tmp-Provis.P_Age
                          BY Tmp-Provis.P_CtaPro:   /*Contab.de la Provision K*/
    EXPORT tmp-provis.
END.
OUTPUT CLOSE.

OUTPUT TO c:\info_Fodun\config.txt.
FOR EACH TSdos NO-LOCK:
    EXPORT DELIMITER ";" tSdos.
END.
OUTPUT CLOSE.

FOR EACH Tmp-Provis BREAK BY Tmp-Provis.P_Age
                          BY Tmp-Provis.P_CtaPro:   /*Contab.de la Provision K*/
    IF FIRST-OF(Tmp-Provis.P_Age) THEN DO:
        FIND FIRST Varios WHERE Varios.Tipo EQ 10 AND Varios.Codigo EQ Tmp-Provis.P_CodVar NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Varios THEN DO:
            MESSAGE "No se ha encontrado la configuración en varios para" SKIP
                    "extractar el tipo de comprobante para la contabilización" SKIP
                    "de la Calificación de cartera para la agencia:" Tmp-Provis.P_Age SKIP
                    "no se creará comprobante contable para esta agencia" SKIP(2)
                    "revise la configuración de varios para el código: " Tmp-Provis.P_CodVar
                VIEW-AS ALERT-BOX ERROR.

            NEXT.
        END.
        ELSE
            ASSIGN W_Des = "Provision-Capital"
                   W_Cbte = Varios.Comprobante.

        FIND FIRST Comprobantes WHERE Comprobantes.Agencia EQ Tmp-Provis.P_Age
                                  AND Comprobantes.Comprobante EQ W_Cbte NO-ERROR.
        IF NOT AVAILABLE Comprobantes THEN DO:
            IF LOCKED Comprobantes THEN
                RETRY.

            IF ERROR-STATUS:ERROR THEN DO:
                MESSAGE "Error al contabilizar la Calificacion" SKIP
                        "Agencia:" Tmp-Provis.P_Age SKIP
                        "Comprobante:" W_Cbte
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.
        END.

        ASSIGN W_NumCbt = Comprobantes.Secuencia + 1
               Comprobantes.Secuencia = Comprobantes.Secuencia + 1
               TCalif = 0.

        FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
    END.

    TCalif = TCalif + Tmp-Provis.P_Valor.

    IF LAST-OF(Tmp-Provis.P_CtaPro) THEN DO:
        IF TCalif GT 0 THEN DO:
            SdoCta = 0.

            FIND FIRST TSdos WHERE TSdos.Age EQ Tmp-Provis.P_Age
                               AND TSdos.Cta EQ Tmp-Provis.P_CtaPro
                               AND TSdos.Id EQ "PC"
                               AND NOT TSdos.SiC NO-ERROR.
            IF AVAILABLE TSdos THEN DO:
                IF TCalif GT TSdos.Sdo THEN
                    ASSIGN CtaCre = Tmp-Provis.P_CtaPro        /*Debe aumentar la provision*/
                           SdoCta = TSdos.Sdo - TCalif   /*Para que sea negativo, o sea al Haber*/
                           TSdos.SiC = TRUE.
                
                IF TCalif LT TSdos.Sdo THEN
                    ASSIGN CtaCre = Tmp-Provis.P_CtaPro   /*Se debe disminuir la provision*/
                           SdoCta = TSdos.Sdo - TCalif     /*Positivo va para el DEBE*/
                           TSdos.SiC = TRUE.

                IF TCalif = TSdos.Sdo THEN
                    TSdos.SiC = TRUE.
            END.
            ELSE DO:
                ASSIGN CtaCre = Tmp-Provis.P_CtaPro
                       SdoCta = TCalif * - 1.
            END.
           
            IF SdoCta NE 0 THEN DO:
                RUN Contab_PartNuevas (INPUT Tmp-Provis.P_Age,
                                       INPUT CtaCre,
                                       INPUT W_Des,
                                       INPUT SdoCta) NO-ERROR.

                RUN Contab_PartNuevas (INPUT Tmp-Provis.P_Age,
                                       INPUT Tmp-Provis.Cruce,
                                       INPUT W_Des,
                                       INPUT SdoCta * -1) NO-ERROR.
            END.
        END.

        TCalif = 0.
    END.

    IF LAST-OF(Tmp-Provis.P_Age) THEN DO:  /*Cancela Las Ctas que ya no tienen Provision, No ajustadas*/
        FOR EACH TSdos WHERE TSdos.Age EQ Tmp-Provis.P_Age
                         AND NOT TSdos.SiC
                         AND TSdos.Id EQ "PC"
                         AND TSdos.Sdo NE 0:
            RUN Contab_PartNuevas (INPUT Tmp-Provis.P_Age,
                                   INPUT TSdos.Cta,
                                   INPUT "Ajuste Prov.K",
                                   INPUT TSdos.Sdo) NO-ERROR.

            RUN Contab_PartNuevas (INPUT Tmp-Provis.P_Age,
                                   /*INPUT Tmp-Provis.Cruce,*/ INPUT TSdos.ctaGtoPrv,
                                   INPUT "Ajuste Prov.K",
                                   INPUT TSdos.Sdo * -1) NO-ERROR.

            TSdos.SiC = TRUE.
        END.
    END.
END.

FOR EACH TSdos WHERE NOT TSdos.SiC AND TSdos.Id EQ "PC" AND TSdos.Sdo NE 0:
    RUN Contab_PartNuevas (INPUT TSdos.Age,
                           INPUT TSdos.Cta,
                           INPUT "Ajuste Final P.K",
                           INPUT TSdos.Sdo) NO-ERROR.

    RUN Contab_PartNuevas (INPUT TSdos.Age,
                           INPUT WK_CtaProDeb,
                           INPUT "Ajuste Final P.K",
                           INPUT TSdos.Sdo * -1) NO-ERROR.

    TSdos.SiC = TRUE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contab_Provision_Costas W_Cierre_Cartera 
PROCEDURE Contab_Provision_Costas :
/*------------------------------------------------------------------------------
  Purpose:   Contabiliza La Provision de Costas(Otras Ctas x Cobrar del Credito).
  Notes:     El cpte es resumen por Oficina y cuenta.  
  Feb.17/06 GAER
------------------------------------------------------------------------------*/
  DEFINE VAR W_Des    AS CHARACTER FORMAT "X(30)".
  DEFINE VAR CtaCre   LIKE Cuentas.Cuenta.
  DEFI   VAR TCalif   LIKE Sal_Cuenta.Sal_Inicial INIT 0.
  DEFI   VAR SdoCta   LIKE Sal_Cuenta.Sal_Inicial.

  FOR EACH Tmp-ProvisCos BREAK BY Tmp-ProvisCos.P_Age BY Tmp-ProvisCos.P_CtaPro:  /*Contab.de la Prov.Costas*/
      IF FIRST-OF(Tmp-ProvisCos.P_Age) THEN DO:
         FIND Varios WHERE Varios.Tipo EQ 10 AND Varios.Codigo EQ Tmp-ProvisCos.P_CodVar NO-LOCK NO-ERROR.    
         IF NOT AVAILABLE Varios THEN DO:                                                                   
            MESSAGE "No se ha encontrado la configuración en varios para" SKIP                             
                  "extractar el tipo de comprobante para la contabilización" SKIP                        
                  "de la Calificación de cartera para la agencia: " Tmp-ProvisCos.P_Age SKIP                   
                  "no se creará comprobante contable para esta agencia" SKIP(2)                                                                      
                  "revise la configuración de varios para el código: " Tmp-ProvisCos.P_CodVar 
               VIEW-AS ALERT-BOX ERROR.                             
            NEXT.                                                                                          
         END.                                                                                               
         ELSE ASSIGN W_Des  = "Provision-Costas"                                                 
                     W_Cbte = Varios.Comprobante.   
                                                                   
         FIND Comprobantes WHERE Comprobantes.Agencia     EQ Tmp-ProvisCos.P_Age AND                                           
                                 Comprobantes.Comprobante EQ W_Cbte NO-ERROR.                                               
         IF NOT AVAILABLE Comprobantes THEN DO:                                                          
            IF LOCKED Comprobantes THEN RETRY.                                                           
            IF ERROR-STATUS:ERROR THEN DO:                                                               
               MESSAGE "Error al contabilizar la Calificacion" SKIP                                         
                       "Agencia: " Tmp-ProvisCos.P_Age SKIP                                                 
                       "Comprobante: " W_Cbte VIEW-AS ALERT-BOX ERROR.                                                         
              RETURN ERROR.                                                                              
            END.                                                                                         
         END.  

         ASSIGN W_NumCbt               = Comprobantes.Secuencia + 1                                      
                Comprobantes.Secuencia = Comprobantes.Secuencia + 1
                TCalif                 = 0.

         FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
      END.

      TCalif = TCalif + Tmp-ProvisCos.P_Valor.

      IF LAST-OF(Tmp-ProvisCos.P_CtaPro) THEN DO:
         IF TCalif GT 0 THEN DO:
            SdoCta = 0.
            FIND FIRST TSdos WHERE TSdos.Age     EQ Tmp-ProvisCos.P_Age
                               AND TSdos.Cta     EQ Tmp-ProvisCos.P_CtaPro
                               AND TSdos.Id      EQ "PO"
                               AND NOT TSdos.SiC NO-ERROR.
            IF AVAIL(TSdos) AND TCalif GT TSdos.Sdo THEN
               ASSIGN CtaCre    = Tmp-ProvisCos.P_CtaPro        /*Debe aumentar la provision*/
                      SdoCta    = TSdos.Sdo - TCalif   /*Para que sea negativo, o sea al Haber*/
                      TSdos.SiC = TRUE.    
            ELSE IF AVAIL(TSdos) AND TCalif LT TSdos.Sdo THEN
                ASSIGN CtaCre    = Tmp-ProvisCos.P_CtaPro   /*Se debe disminuir la provision*/
                       SdoCta    = TSdos.Sdo - TCalif     /*Positivo va para el DEBE*/
                       TSdos.SiC = TRUE.
            ELSE IF AVAIL(TSdos) THEN           /*Igual no hay asiento*/
                TSdos.SiC = TRUE.
            ELSE IF NOT AVAIL(TSdos) THEN           /*Se debe crear al Haber */
               ASSIGN CtaCre = Tmp-ProvisCos.P_CtaPro
                      SdoCta = TCalif * - 1.
           
            IF SdoCta NE 0 THEN DO:
               RUN Contab_PartNuevas (INPUT Tmp-ProvisCos.P_Age,CtaCre,W_Des,SdoCta) NO-ERROR.
               RUN Contab_PartNuevas (INPUT Tmp-ProvisCos.P_Age,Tmp-ProvisCos.P_CtaGto,W_Des,SdoCta * -1) NO-ERROR.
            END.
         END.

         TCalif = 0.
      END.

      IF LAST-OF(Tmp-ProvisCos.P_Age) THEN DO:  /*Cancela Las Ctas que ya no tienen Provision, No ajustadas*/
         FOR EACH TSdos WHERE TSdos.Age EQ Tmp-ProvisCos.P_Age AND NOT TSdos.SiC 
                          AND TSdos.Id  EQ "PO"                AND TSdos.Sdo NE 0:
             RUN Contab_PartNuevas (INPUT Tmp-ProvisCos.P_Age,TSdos.Cta,"Ajuste Prov.Costas",TSdos.Sdo) NO-ERROR.
             RUN Contab_PartNuevas (INPUT Tmp-ProvisCos.P_Age,Tmp-ProvisCos.P_CtaGto,"Ajuste Prov.Costas",TSdos.Sdo * -1) NO-ERROR.
             ASSIGN TSdos.SiC = TRUE.
         END.
      END.
  END.

  FOR EACH TSdos WHERE NOT TSdos.SiC AND TSdos.Id EQ "PO" AND TSdos.Sdo NE 0:                                
      RUN Contab_PartNuevas (INPUT TSdos.Age,TSdos.Cta,"Ajuste Final P.O.",TSdos.Sdo) NO-ERROR.                  
      RUN Contab_PartNuevas (INPUT TSdos.Age,WK_CtaProCosGto,"Ajuste Final P.O.",TSdos.Sdo * -1) NO-ERROR.
      ASSIGN TSdos.SiC = TRUE.                                                                             
  END.                                                                                                          

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contab_Provision_Interes W_Cierre_Cartera 
PROCEDURE Contab_Provision_Interes :
DEFINE VAR W_Des AS CHARACTER FORMAT "X(30)".
DEFINE VAR CtaCre LIKE Cuentas.Cuenta.
DEFI VAR TCalif LIKE Sal_Cuenta.Sal_Inicial INIT 0.
DEFI VAR SdoCta LIKE Sal_Cuenta.Sal_Inicial.

FOR EACH Tmp-ProvisInt BREAK BY Tmp-ProvisInt.P_Age
                             BY Tmp-ProvisInt.P_CtaPro:  /*Contab.de la Prov.Intereses*/
    IF FIRST-OF(Tmp-ProvisInt.P_Age) THEN DO:
        FIND Varios WHERE Varios.Tipo EQ 10 AND Varios.Codigo EQ Tmp-ProvisInt.P_CodVar NO-LOCK NO-ERROR.    
        IF NOT AVAILABLE Varios THEN DO:
            MESSAGE "No se ha encontrado la configuración en varios para" SKIP
                    "extractar el tipo de comprobante para la contabilización" SKIP
                    "de la Calificación de cartera para la agencia: " Tmp-ProvisInt.P_Age SKIP
                    "no se creará comprobante contable para esta agencia" SKIP(2)
                    "revise la configuración de varios para el código: " Tmp-ProvisInt.P_CodVar
                VIEW-AS ALERT-BOX ERROR.

            NEXT.
        END.
        ELSE
            ASSIGN W_Des = "Provision-Interes"
                   W_Cbte = Varios.Comprobante.

        FIND Comprobantes WHERE Comprobantes.Agencia EQ Tmp-ProvisInt.P_Age
                            AND Comprobantes.Comprobante EQ W_Cbte NO-ERROR.
        IF NOT AVAILABLE Comprobantes THEN DO:
            IF LOCKED Comprobantes THEN
                RETRY.

            IF ERROR-STATUS:ERROR THEN DO:
                MESSAGE "Error al contabilizar la Calificacion" SKIP
                        "Agencia: " Tmp-ProvisInt.P_Age SKIP
                        "Comprobante: " W_Cbte
                    VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
            END.
        END.

        ASSIGN W_NumCbt = Comprobantes.Secuencia + 1
               Comprobantes.Secuencia = Comprobantes.Secuencia + 1
               TCalif = 0.

        FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
    END.

    TCalif = TCalif + Tmp-ProvisInt.P_Valor.

    IF LAST-OF(Tmp-ProvisInt.P_CtaPro) THEN DO:
        IF TCalif GT 0 THEN DO:
            SdoCta = 0.

            FIND FIRST TSdos WHERE TSdos.Age EQ Tmp-ProvisInt.P_Age
                               AND TSdos.Cta EQ Tmp-ProvisInt.P_CtaPro
                               AND TSdos.Id EQ "PI"
                               AND NOT TSdos.SiC NO-ERROR.
            IF AVAIL(TSdos) AND TCalif GT TSdos.Sdo THEN
                ASSIGN CtaCre = Tmp-ProvisInt.P_CtaPro        /*Debe aumentar la provision*/
                       SdoCta = TSdos.Sdo - TCalif   /*Para que sea negativo, o sea al Haber*/
                       TSdos.SiC = TRUE.
            ELSE
                IF AVAIL(TSdos) AND TCalif LT TSdos.Sdo THEN
                    ASSIGN CtaCre = Tmp-ProvisInt.P_CtaPro   /*Se debe disminuir la provision*/
                           SdoCta    = TSdos.Sdo - TCalif     /*Positivo va para el DEBE*/
                           TSdos.SiC = TRUE.
                ELSE
                    IF AVAIL(TSdos) THEN            /*Igual no hay asiento*/
                        TSdos.SiC = TRUE.
                    ELSE
                        IF NOT AVAIL(TSdos) THEN           /*Se debe crear al Haber */
                            ASSIGN CtaCre = Tmp-ProvisInt.P_CtaPro
                                   SdoCta = TCalif * - 1.

            IF SdoCta NE 0 THEN DO:
                RUN Contab_PartNuevas (INPUT Tmp-ProvisInt.P_Age,CtaCre,W_Des,SdoCta) NO-ERROR.
                RUN Contab_PartNuevas (INPUT Tmp-ProvisInt.P_Age,Tmp-ProvisInt.P_CtaGto,W_Des,SdoCta * -1) NO-ERROR.
            END.
        END.

        TCalif = 0.
    END.

    IF LAST-OF(Tmp-ProvisInt.P_Age) THEN DO:  /*Cancela Las Ctas que ya no tienen Provision, No ajustadas*/
        FOR EACH TSdos WHERE TSdos.Age EQ Tmp-ProvisInt.P_Age AND NOT TSdos.SiC
                         AND TSdos.Id EQ "PI" AND TSdos.Sdo NE 0:
            RUN Contab_PartNuevas (INPUT Tmp-ProvisInt.P_Age,
                                   INPUT TSdos.Cta,
                                   INPUT "Ajuste Prov.I.",
                                   INPUT TSdos.Sdo) NO-ERROR.

            RUN Contab_PartNuevas (INPUT Tmp-ProvisInt.P_Age,
                                   /*INPUT Tmp-ProvisInt.P_CtaGto,*/ INPUT TSdos.ctaGtoPrv,
                                   INPUT "Ajuste Prov.I.",
                                   INPUT TSdos.Sdo * -1) NO-ERROR.

            TSdos.SiC = TRUE.
        END.
    END.
END.

FOR EACH TSdos WHERE NOT TSdos.SiC AND TSdos.Id EQ "PI" AND TSdos.Sdo NE 0:
    RUN Contab_PartNuevas (INPUT TSdos.Age,TSdos.Cta,"Ajuste Final P.I.",TSdos.Sdo) NO-ERROR.
    RUN Contab_PartNuevas (INPUT TSdos.Age,Wk_CtaProIntGto,"Ajuste Final P.I.",TSdos.Sdo * -1) NO-ERROR.

    TSdos.SiC = TRUE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W_Cierre_Cartera  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W_Cierre_Cartera)
  THEN DELETE WIDGET W_Cierre_Cartera.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W_Cierre_Cartera  _DEFAULT-ENABLE
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
  DISPLAY W_CmbOfi W_FecMes W_Cont Msaje 
      WITH FRAME F_CieCar IN WINDOW W_Cierre_Cartera.
  ENABLE RECT-314 BUTTON-5 W_FecMes Btn_Proc Btn_Done Btn_Prelim 
      WITH FRAME F_CieCar IN WINDOW W_Cierre_Cartera.
  {&OPEN-BROWSERS-IN-QUERY-F_CieCar}
  VIEW W_Cierre_Cartera.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba_Informes W_Cierre_Cartera 
PROCEDURE Graba_Informes :
DEFINE VAR TotProvision AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".
DEFINE VAR TotKC AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".
DEFINE VAR TotKA AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".
DEFINE VAR TotKG AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".
DEFINE VAR TotProAge    AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".
DEFINE VAR ListadoPro AS CHARACTER INITIAL "".
DEFINE VAR ListadoCal AS CHARACTER INITIAL "".
DEFINE VAR calCre AS CHARACTER.
DEFINE VAR calCli AS CHARACTER.

ASSIGN ListadoPro = W_PathSpl + "\000-" + STRING(W_Fecha,"999999") + "Provis.Lst"
       ListadoCal = W_PathSpl + "\000-" + STRING(W_Fecha,"999999") + "CalifD.Lst"
       Msaje:SCREEN-VALUE IN FRAME F_CieCar = "InfProv:" + ListadoPro.

OUTPUT TO VALUE(ListadoCal).
FOR EACH Tmp-CalifDet BREAK BY Tmp-CalifDet.C_Age
                            BY Tmp-CalifDet.C_CtaCal
                            BY Tmp-CalifDet.C_CodVar
                            BY Tmp-CalifDet.C_Nit:
    DISPLAY Tmp-CalifDet.C_Age      LABEL "Ag."
            Tmp-CalifDet.C_CtaCal   LABEL "Cta-Calificac"
            Tmp-CalifDet.C_CodVar   LABEL "Cod-Var"
            Tmp-CalifDet.C_Nit      LABEL "Ced./Nit"
            Tmp-CalifDet.C_NumCre   LABEL "Num-Crédito"
            Tmp-CalifDet.C_Valor    LABEL "Sdo-Capital"
        WITH DOWN WIDTH 250 FRAME Tdet NO-LABELS USE-TEXT NO-BOX STREAM-IO.

    ASSIGN TotKA = TotKA + Tmp-CalifDet.C_Valor
           TotKC = TotKC + Tmp-CalifDet.C_Valor
           TotKG = TotKG + Tmp-CalifDet.C_Valor.

    IF LAST-OF(Tmp-CalifDet.C_CtaCal) THEN DO:
        DISPLAY "Tot.Cta   $ " TotKC WITH WIDTH 100 NO-LABELS NO-BOX.

        TotKC = 0.
    END.

    IF LAST-OF(Tmp-CalifDet.C_Age) THEN DO:
        DISPLAY "Tot.Agencia $ " TotKA WITH WIDTH 100 NO-LABELS NO-BOX.

        TotKA = 0.
    END.
END.

DISPLAY "Tot.General $ " TotKG WITH WIDTH 100 NO-LABELS NO-BOX.

totKG = 0.

OUTPUT CLOSE.

OUTPUT TO VALUE(ListadoPro).

DISPLAY "AGE NIT         CRE NUM_CREDITO           SDO_CAPITAL    PROV_KAP    PROV_INT PROV_COSTAS CAT Porc(%) CAL_CRE CAL_CLI         APORTES_DISTRIB         VALOR_DEFECTO         TOTAL_APORTES CUENTA           DIAS_MORA        VALOR_GARANTIA" SKIP
        "--- ----------- --- ----------- --------------------- ----------- ----------- ----------- --- ------- ------- ------- ----------------------- --------------------- --------------------- --------         ---------        --------------"
    WITH FRAME FTI WIDTH 320.

FOR EACH Pro BREAK BY Pro.Agencia
                   BY Pro.Nit:
    CASE pro.cal_Credito:
        WHEN 1 THEN calCre = "A".
        WHEN 2 THEN calCre = "A".
        WHEN 3 THEN calCre = "B".
        WHEN 4 THEN calCre = "C".
        WHEN 5 THEN calCre = "D".
        OTHERWISE calCre = "E".
    END CASE.

    CASE pro.cal_cliente:
        WHEN 1 THEN calCli = "A".
        WHEN 2 THEN calCli = "A".
        WHEN 3 THEN calCli = "B".
        WHEN 4 THEN calCli = "C".
        WHEN 5 THEN calCli = "D".
        OTHERWISE calCli = "E".
    END CASE.

    DISPLAY Pro.Agencia
            Pro.Nit FORMAT "x(11)"
            Pro.Cod_Credito
            Pro.Num_Credito FORMAT ">>>>>>>>>>>"
            Pro.Sdo_Capital
            Pro.Provision
            Pro.Provint
            Pro.ProvCos
            " " + Pro.categoria FORMAT "X(2)"
            pro.porc
            "   " + calCre
            "  " + calCli
            Pro.ApoDistribu
            Pro.ValDefecto
            Pro.TotAportes
            Pro.CuentaCon FORMAT "x(20)"
            Pro.diamora FORMAT ">>>9"
            Pro.ValGarant
        WITH FRAME Fp WIDTH 320  NO-LABELS USE-TEXT NO-BOX STREAM-IO NO-UNDERLINE DOWN.

    DOWN WITH FRAME fp.

    TotProAge = TotProAge + Pro.Provision.

    IF LAST-OF(Pro.Agencia) THEN DO:
        DISPLAY "Total Provision Agencia : " Pro.Agencia " - $ " TotProaGE WITH NO-LABELS.

        ASSIGN TotProvision = TotProvision  + TotProAge
               TotProAge = 0.
    END.
END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Halla_Saldos W_Cierre_Cartera 
PROCEDURE Halla_Saldos :
FOR EACH Agencias WHERE Agencias.Estado <> 3 NO-LOCK:
    FOR EACH CarteraVencida NO-LOCK:
        FIND FIRST pro_creditos WHERE cod_credito = CarteraVencida.Cod_Producto NO-ERROR.
        IF AVAILABLE(pro_creditos) THEN
            IF pro_creditos.tip_credito > 4 THEN
                NEXT.

            FIND FIRST TSdos WHERE Tsdos.Age = Agencias.Agencia
                               AND TSdos.Cta = CarteraVencida.Cta_AsoAdDB
                               AND Tsdos.Id = "CC" NO-ERROR.
            IF NOT AVAILABLE(TSdos) THEN
                CREATE TSdos.

            ASSIGN TSdos.Age = Agencias.Agencia
                   Tsdos.Cta = CarteraVencida.Cta_AsoAdDB
                   Tsdos.Id = "CC".

            FIND FIRST TSdos WHERE Tsdos.Age = Agencias.Agencia
                               AND TSdos.Cta = CarteraVencida.Cta_NoAAdDB 
                               AND Tsdos.Id = "CC" NO-ERROR.
            IF NOT AVAILABLE(TSdos) THEN
                CREATE TSdos.

            ASSIGN TSdos.Age = Agencias.Agencia
                   Tsdos.Cta = CarteraVencida.Cta_NoAAdDB
                   Tsdos.Id = "CC".

            FIND FIRST TSdos WHERE Tsdos.Age = Agencias.Agencia
                               AND TSdos.Cta = CarteraVencida.Cta_AsoNaDB
                               AND Tsdos.Id = "CC" NO-ERROR.
            IF NOT AVAILABLE(TSdos) THEN
                CREATE TSdos.

            ASSIGN TSdos.Age = Agencias.Agencia
                   TSdos.Cta = CarteraVencida.Cta_AsoNaDB
                   Tsdos.Id = "CC".

            FIND FIRST TSdos WHERE Tsdos.Age = Agencias.Agencia
                               AND TSdos.Cta = CarteraVencida.Cta_NoANaDB
                               AND Tsdos.Id = "CC" NO-ERROR.
            IF NOT AVAILABLE(TSdos) THEN
                CREATE TSdos.

            ASSIGN TSdos.Age = Agencias.Agencia
                   TSdos.Cta = CarteraVencida.Cta_NoANaDB
                   Tsdos.Id = "CC".

            FIND FIRST TSdos WHERE Tsdos.Age = Agencias.Agencia
                               AND TSdos.Cta = CarteraVencida.CtaCal_Interes
                               AND Tsdos.Id = "CI" NO-ERROR.
            IF NOT AVAILABLE(TSdos) THEN
                CREATE TSdos.

            ASSIGN TSdos.Age = Agencias.Agencia
                   TSdos.Cta = CarteraVencida.CtaCal_Interes
                   Tsdos.Id = "CI".

            FIND FIRST TSdos WHERE Tsdos.Age = Agencias.Agencia            /*Reclasif.Int-contingente*/
                               AND TSdos.Cta = CarteraVencida.Cta_IntContingDb
                               AND Tsdos.Id = "IC" NO-ERROR.
            IF NOT AVAILABLE(TSdos) THEN
                CREATE TSdos.

            ASSIGN TSdos.Age = Agencias.Agencia                 
                   TSdos.Cta = CarteraVencida.Cta_IntContingDb
                   TSdos.Aju = CarteraVencida.Cta_IntContingCr
                   Tsdos.Id = "IC".

            FIND FIRST TSdos WHERE Tsdos.Age = Agencias.Agencia
                               AND TSdos.Cta = CarteraVencida.CtaCal_Costas 
                               AND Tsdos.Id = "CO" NO-ERROR.
            IF NOT AVAILABLE(TSdos) THEN
                CREATE TSdos.

            ASSIGN TSdos.Age = Agencias.Agencia
                   TSdos.Cta = CarteraVencida.CtaCal_Costas
                   Tsdos.Id = "CO".

            FIND FIRST TSdos WHERE Tsdos.Age = Agencias.Agencia
                               AND TSdos.Cta = CarteraVencida.Cta_AsoPrvAdCr    /*Cta_AsoPrvAdCr*/
                               AND Tsdos.Id = "PC" NO-ERROR.
            IF NOT AVAILABLE(TSdos) THEN
                CREATE TSdos.

            ASSIGN TSdos.Age = Agencias.Agencia
                   TSdos.Cta = CarteraVencida.Cta_AsoPrvAdCr
                   TSdos.ctaGtoPrv = carteraVencida.cta_AsoPrvAdDb
                   Tsdos.Id = "PC".

            FIND FIRST TSdos WHERE Tsdos.Age = Agencias.Agencia
                               AND TSdos.Cta = CarteraVencida.Cta_AsoPrvNaCr   /*Cta_AsoPrvNaCr*/
                               AND Tsdos.Id = "PC" NO-ERROR.
            IF NOT AVAILABLE(TSdos) THEN
                CREATE TSdos.

            ASSIGN TSdos.Age = Agencias.Agencia
                   TSdos.Cta = CarteraVencida.Cta_AsoPrvNaCr
                   TSdos.ctaGtoPrv = carteraVencida.cta_asoPrvNaDb
                   Tsdos.Id = "PC".

            FIND FIRST TSdos WHERE Tsdos.Age = Agencias.Agencia
                               AND TSdos.Cta = CarteraVencida.Cta_NoaPrvAdCr
                               AND Tsdos.Id = "PC" NO-ERROR.
            IF NOT AVAILABLE(TSdos) THEN
                CREATE TSdos.

            ASSIGN TSdos.Age = Agencias.Agencia
                   TSdos.Cta = CarteraVencida.Cta_NoaPrvAdCr
                   TSdos.CtaGtoPrv = CarteraVencida.Cta_NoaPrvAdDb
                   Tsdos.Id = "PC".

            FIND FIRST TSdos WHERE Tsdos.Age = Agencias.Agencia
                               AND TSdos.Cta = CarteraVencida.Cta_NoaPrvNaCr
                               AND Tsdos.Id = "PC" NO-ERROR.
            IF NOT AVAILABLE(TSdos) THEN
                CREATE TSdos.

            ASSIGN TSdos.Age = Agencias.Agencia
                   TSdos.Cta = CarteraVencida.Cta_NoaPrvNaCr
                   TSdos.CtaGtoPrv = CarteraVencida.Cta_NoaPrvNaDb
                   Tsdos.Id = "PC".

            FIND FIRST TSdos WHERE Tsdos.Age = Agencias.Agencia
                               AND TSdos.Cta = CarteraVencida.Cta_AsoIntAdCr
                               AND Tsdos.Id = "PI" NO-ERROR.
            IF NOT AVAILABLE(TSdos) THEN
                CREATE TSdos.

            ASSIGN TSdos.Age = Agencias.Agencia
                   TSdos.Cta = CarteraVencida.Cta_AsoIntAdCr
                   TSdos.CtaGtoPrv = CarteraVencida.Cta_AsoIntAdDb
                   Tsdos.Id = "PI".

            FIND FIRST TSdos WHERE Tsdos.Age = Agencias.Agencia
                               AND TSdos.Cta = CarteraVencida.Cta_AsoIntNaCr
                               AND Tsdos.Id = "PI" NO-ERROR.
            IF NOT AVAILABLE(TSdos) THEN
                CREATE TSdos.

            ASSIGN TSdos.Age = Agencias.Agencia
                   TSdos.Cta = CarteraVencida.Cta_AsoIntNaCr
                   TSdos.CtaGtoPrv = CarteraVencida.Cta_AsoIntNaDb
                   Tsdos.Id = "PI".

            FIND FIRST TSdos WHERE Tsdos.Age = Agencias.Agencia
                               AND TSdos.Cta = CarteraVencida.Cta_NoaIntAdCr
                               AND Tsdos.Id = "PI" NO-ERROR.
            IF NOT AVAILABLE(TSdos) THEN
                CREATE TSdos.

            ASSIGN TSdos.Age = Agencias.Agencia
                   TSdos.Cta = CarteraVencida.Cta_NoaIntAdCr
                   TSdos.CtaGtoPrv = CarteraVencida.Cta_NoaIntAdDb
                   Tsdos.Id = "PI".

            FIND FIRST TSdos WHERE Tsdos.Age = Agencias.Agencia
                               AND TSdos.Cta = CarteraVencida.Cta_NoaIntNaCr
                               AND Tsdos.Id = "PI" NO-ERROR.
            IF NOT AVAILABLE(TSdos) THEN
                CREATE TSdos.

            ASSIGN TSdos.Age = Agencias.Agencia
                   TSdos.Cta = CarteraVencida.Cta_NoaIntNaCr
                   TSdos.CtaGtoPrv = CarteraVencida.Cta_NoaIntNaDb
                   Tsdos.Id = "PI".

            FIND FIRST TSdos WHERE Tsdos.Age = Agencias.Agencia
                               AND TSdos.Cta = CarteraVencida.Cta_CostasCR
                               AND Tsdos.Id = "PO" NO-ERROR.   /*Costas*/
            IF NOT AVAILABLE(TSdos) THEN
                CREATE TSdos.

            ASSIGN TSdos.Age = Agencias.Agencia
                   TSdos.Cta = CarteraVencida.Cta_CostasCR
                   Tsdos.Id = "PO".
        END.
    END.

    FOR EACH TSdos BY TSdos.Age
                   BY TSdos.Cta:
        IF TSdos.cta = "" THEN DO:
            MESSAGE pro_creditos.cod_credito tSdos.age tSdos.id
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            NEXT.
        END.

        RUN BuscarSdoCta (INPUT TSdos.Age,
                          INPUT TSdos.Cta,
                          OUTPUT TSdos.Sdo).
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProCal_Cierre W_Cierre_Cartera 
PROCEDURE ProCal_Cierre :
DEFINE VAR W_CtaCal AS CHARACTER.
DEFINE VAR W_CalMayor AS INTEGER.
DEFINE VAR W_Aportes AS DECIMAL.
DEFINE VAR W_CalCliente AS INTEGER.
DEFINE VAR W_PorAporte AS DECIMAL.
DEFINE VAR W_TotCredito AS DECIMAL.
DEFINE VAR W_ApoDesc AS DECIMAL.
DEFINE VAR W_ValGarAdm AS DECIMAL.
DEFINE VAR W_ValHipoteca AS DECIMAL.
DEFINE VAR W_ValNoHipoteca AS DECIMAL.
DEFINE VAR W_Valdefecto AS DECIMAL.
DEFINE VAR W_ValProvision AS DECIMAL.
DEFINE VAR WK_CtaProCre AS CHARACTER.
DEFINE VAR WK_CtaProIntPro AS CHARACTER.
DEFINE VAR WK_CtaProCosPro AS CHARACTER.

ASSIGN Msaje:SCREEN-VALUE IN FRAME F_CieCar = "Totalizando Créditos x Nit".

/* Recorre la tabla de creditos para hallar el total de credito para la busqueda del porcentaje que le corresponde de descuento de los Aportes */
FOR EACH rep_creditos WHERE rep_credito.fecCorte = w_fecMes
                        AND rep_creditos.tip_credito <= 4
                        AND rep_creditos.estado = 2 BREAK BY rep_creditos.Nit:
    FIND FIRST creditos WHERE creditos.nit = rep_creditos.nit
                          AND creditos.cod_credito = rep_creditos.cod_credito
                          AND creditos.num_credito = rep_creditos.num_credito NO-ERROR.

    FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = rep_creditos.cod_credito NO-LOCK NO-ERROR.
    
    FIND FIRST CarteraVencida WHERE CarteraVencida.Cod_Producto = rep_creditos.cod_credito
                                AND CarteraVencida.Per_Inicial <= (rep_creditos.Dias_Atraso)
                                AND CarteraVencida.Per_Final >= (rep_creditos.Dias_Atraso) NO-LOCK NO-ERROR.
    IF AVAILABLE CarteraVencida AND rep_creditos.sdo_capital > 0 THEN DO:
        ASSIGN rep_creditos.Cod_Califica = CarteraVencida.Cod_Califica
               creditos.cod_califica = carteraVencida.cod_califica
               rep_creditos.Categoria = CarteraVencida.Categoria
               creditos.Categoria = CarteraVencida.Categoria
               rep_creditos.categoriames = CarteraVencida.Categoria
               creditos.categoriames = CarteraVencida.Categoria.
    END.
    ELSE
        ASSIGN rep_creditos.Cod_Califica = 1
               creditos.cod_califica = 1
               rep_creditos.Categoria = "A"
               creditos.Categoria = "A"
               rep_creditos.categoriames = "A"
               creditos.categoriames = "A".

    ASSIGN rep_creditos.Provision = 0
           creditos.Provision = 0
           rep_creditos.Provision_Interes = 0
           creditos.Provision_Interes = 0
           rep_creditos.Provision_Otros = 0
           creditos.Provision_Otros = 0.

    FIND FIRST totcre WHERE totcre.nit = rep_creditos.nit NO-ERROR.
    IF NOT AVAILABLE(totcre) THEN DO:
       ASSIGN W_Cont = W_Cont + 1
              W_Cont:SCREEN-VALUE = STRING(W_Cont).

       CREATE TotCre.
       ASSIGN TotCre.Nit = rep_creditos.Nit.
    END.
    
    TotCre.Tot = TotCre.Tot + rep_creditos.Sdo_Capital.

    /* Calcula el mayor vencimiento */
    FIND FIRST tarrastre WHERE tarrastre.ced = rep_creditos.nit
                           AND tarrastre.tip = rep_creditos.tip_credito NO-ERROR.
    IF AVAILABLE(tarrastre) THEN DO:
        IF tarrastre.califica < rep_creditos.cod_califica THEN
            tarrastre.califica = rep_creditos.cod_califica.
    END.
    ELSE DO:
        CREATE tarrastre.
        ASSIGN tarrastre.ced = rep_creditos.nit
               tarrastre.tip = rep_creditos.tip_credito
               tarrastre.arrastre = rep_creditos.categoria
               tarrastre.califica = rep_creditos.cod_califica.
    END.
END.

Msaje:SCREEN-VALUE = "Calculando Provisión".

W_Cont = 0.

FOR EACH rep_creditos WHERE rep_creditos.fecCorte = w_fecMes
                        AND rep_creditos.tip_credito <= 4
                        AND rep_creditos.estado = 2 BREAK BY rep_creditos.Nit
                                                          BY rep_creditos.Tip_Credito
                                                          BY rep_creditos.Cod_Califica DESCENDING:
    FIND FIRST creditos WHERE creditos.nit = rep_creditos.nit
                          AND creditos.cod_credito = rep_creditos.cod_credito
                          AND creditos.num_credito = rep_creditos.num_credito NO-ERROR.

    IF FIRST-OF(rep_creditos.Nit) THEN DO:
        FIND FIRST TotCre WHERE TotCre.Nit = rep_creditos.Nit NO-ERROR.
        IF AVAILABLE TotCre THEN
            W_TotCredito = TotCre.Tot.

        FIND FIRST Clientes WHERE Clientes.Nit = rep_creditos.Nit NO-LOCK NO-ERROR.
        IF AVAILABLE Clientes THEN
            W_CalCliente = Clientes.Calificacion.

        W_Aportes = 0.
        W_CalMayor = 1.

        FOR EACH rep_Ahorros WHERE rep_ahorros.fecCorte = w_fecMes
                               AND rep_Ahorros.Nit = rep_creditos.Nit
                               AND rep_ahorros.tip_ahorro = 4
                               AND rep_ahorros.estado = 1 NO-LOCK:
            W_Aportes = W_Aportes + (rep_Ahorros.Sdo_Disponible + rep_Ahorros.Sdo_Canje).
        END.
    END.

    FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = rep_creditos.cod_credito  NO-LOCK NO-ERROR.

    ASSIGN W_Cont = W_Cont + 1
           W_Cont:SCREEN-VALUE = STRING(W_Cont).
           W_ValProvision = 0.

    W_CalMayor = rep_creditos.Cod_Califica.

    FIND FIRST tarrastre WHERE tarrastre.ced = rep_creditos.nit
                           AND tarrastre.tip = rep_creditos.tip_credito NO-ERROR.
    IF AVAILABLE(tarrastre) THEN
        W_CalMayor = tarrastre.califica.
    
    IF W_CalCliente > W_Calmayor THEN
        W_CalMayor = W_CalCliente.

    FIND FIRST CarteraVencida WHERE CarteraVencida.Cod_Producto = rep_creditos.cod_credito
                                AND CarteraVencida.Cod_Califica = W_CalMayor NO-LOCK NO-ERROR.
    IF NOT AVAILABLE CarteraVencida  THEN DO:
        MESSAGE "La Configuración para la Clasificación de Cartera" SKIP
                "no existe en la Tabla CARTERAVENCIDA para el" SKIP(1)
                "Producto:" rep_creditos.Cod_Credito SKIP
                "Cliente:" rep_creditos.Nit SKIP
                "Con un Rango valido que contenga el Número:" rep_creditos.Dias_Atraso SKIP(1)
                "Se cancela el proceso de Calificación" SKIP
                "Conmuniquese con el Administrador del Sistema"
            VIEW-AS ALERT-BOX ERROR.

        RETURN ERROR.
    END.

    CREATE Pro.
    ASSIGN Pro.Agencia = rep_creditos.Agencia
           Pro.Nit = rep_creditos.Nit
           Pro.Cod_Credito = rep_creditos.Cod_Credito
           Pro.Num_Credito = rep_creditos.Num_Credito
           Pro.Sdo_Capital = rep_creditos.Sdo_Capital
           Pro.Cal_Credito = rep_creditos.Cod_Califica
           Pro.TotAportes = W_Aportes
           Pro.Cal_Cliente = W_CalCliente
           Pro.Pagare = rep_creditos.Pagare
           Pro.diamora = rep_creditos.Dias_atraso
           Pro.ValGarant = 0
           Pro.Negocio = Pro_Creditos.Cod_otro
           pro.categoria = carteravencida.categoria.

    ASSIGN W_ValGarAdm = 0
           W_ValHipoteca = 0
           W_ValNoHipoteca = 0
           W_ValDefecto = 0.

    FOR EACH Garantias WHERE Garantias.Cod_Credito = rep_creditos.Cod_Credito
                         AND Garantias.Tip_Credito = rep_creditos.Tip_Credito
                         AND Garantias.Tipo_Garantia < 4
                         AND Garantias.Num_Solicitud = rep_creditos.Num_Solicitud
                         AND Garantias.Num_Credito = rep_creditos.Num_Credito
                         AND Garantias.Estado = 1 NO-LOCK:
        W_ValGarAdm = W_ValGarAdm + Garantias.Val_Bien.

        IF Garantias.Tipo_Garantia = 1 THEN
            W_ValHipoteca = W_ValHipoteca + Garantias.Val_Bien.
        ELSE
            W_ValNoHipoteca = W_ValNoHipoteca + Garantias.Val_Bien.

        Pro.ValGarant = Pro.ValGarant + Garantias.Val_Bien.
    END.

    ASSIGN W_PorAporte = (Pro.Sdo_Capital * 100) / W_TotCredito
           W_ApoDesc = W_Aportes * (W_PorAporte / 100)
           Pro.ApoDistribu = W_ApoDesc.

    ASSIGN Wk_CtaProCosGto = CarteraVencida.Cta_CostasDB
           WK_CtaProCosPro = CarteraVencida.Cta_CostasCr 
           Wk_CtaProIntGto = CarteraVencida.Cta_AsoIntAdDb
           WK_CtaProIntPro = CarteraVencida.Cta_AsoIntAdCr.

    IF W_aportes > Pro.Sdo_Capital THEN
        W_valGarAdm = W_aportes.

    IF W_ValGarAdm > 0 THEN DO:
        ASSIGN WK_CtaProCre = CarteraVencida.Cta_AsoPrvAdCr
               WK_CtaProDeb = CarteraVencida.Cta_AsoPrvAdDb.

        IF rep_creditos.FOR_Pago = 2 THEN
            W_CtaCal = CarteraVencida.Cta_AsoAdDB.
        ELSE
            ASSIGN W_CtaCal = CarteraVencida.Cta_NoAAdDB
                   WK_CtaProCre = CarteraVencida.Cta_NoaPrvAdCr.

        IF W_ValHipoteca > 0 THEN
            W_ValDefecto = (W_ValHipoteca * CarteraVencida.Porc_DefGarantia).

        IF W_ValNoHipoteca > 0 THEN
            W_ValDefecto = W_ValDefecto + (W_ValNoHipoteca * CarteraVencida.Porc_DefNoHipoteca).

        Pro.ValDEfecto = W_ValDefecto.

        IF (rep_creditos.Sdo_Capital - W_ApoDesc - W_ValDefecto) > 0 THEN
            ASSIGN W_ValProvision = (rep_creditos.Sdo_Capital - W_ApoDesc - W_ValDefecto) * CarteraVencida.Porc_Admisible
                   pro.porc = CarteraVencida.Porc_Admisible * 100.
    END.
    ELSE DO: 
        ASSIGN WK_CtaProCre = CarteraVencida.Cta_AsoPrvNaCr
               WK_CtaProDeb = CarteraVencida.Cta_NoaPrvAdDb
               Wk_CtaProIntGto = CarteraVencida.Cta_AsoIntNaDb
               WK_CtaProIntPro = CarteraVencida.Cta_AsoIntNaCr.

        IF rep_creditos.FOR_Pago = 2 THEN 
            W_CtaCal = CarteraVencida.Cta_AsoNaDB.
        ELSE
            ASSIGN W_CtaCal = CarteraVencida.Cta_NoANaDB
                   WK_CtaProCre = CarteraVencida.Cta_NoaPrvNaCr.

        IF (rep_creditos.Sdo_Capital - W_ApoDesc) > 0 THEN
            ASSIGN W_ValProvision = (rep_creditos.Sdo_Capital - W_ApoDesc) * CarteraVencida.Por_ProvNoAdm
                   pro.porc = CarteraVencida.Por_ProvNoAdm * 100.
    END.

    IF W_ValProvision > 0 THEN DO:
        FIND FIRST Tmp-Provis WHERE Tmp-Provis.P_Age = rep_creditos.Agencia
                                AND Tmp-Provis.P_Tipo = rep_creditos.Tip_Credito
                                AND Tmp-Provis.P_CtaPro = Wk_CtaProCre
                                AND Tmp-Provis.P_CodVar = 2 NO-ERROR.
        IF NOT AVAILABLE Tmp-Provis THEN DO:
            CREATE Tmp-Provis.
            ASSIGN Tmp-Provis.P_Age = rep_creditos.Agencia
                   Tmp-Provis.P_Tipo = rep_creditos.Tip_Credito
                   Tmp-Provis.P_CtaPro = Wk_CtaProCre
                   Tmp-Provis.Cruce = Wk_CtaProDeb
                   Tmp-Provis.P_CodVar = 2.
        END.

        Tmp-Provis.P_Valor = Tmp-Provis.P_Valor + ROUND(W_ValProvision,0).
    END.

    ASSIGN rep_creditos.Provision = ROUND(W_ValProvision,0)
           creditos.Provision = ROUND(W_ValProvision,0)
           rep_creditos.Cta_Contable = W_CtaCal
           creditos.Cta_Contable = W_CtaCal
           Pro.CuentaCon = W_CtaCal
           Pro.Provision = ROUND(W_ValProvision,0)
           rep_creditos.Cod_CalificaMes = W_CalMayor
           creditos.Cod_CalificaMes = W_CalMayor.

    CASE W_CalMayor:
        WHEN 1 THEN
            ASSIGN rep_creditos.CategoriaMes = "A"
                   creditos.CategoriaMes = "A".

        WHEN 2 THEN
            ASSIGN rep_creditos.CategoriaMes = "A"
                   creditos.CategoriaMes = "A".

        WHEN 3 THEN
            ASSIGN rep_creditos.CategoriaMes = "B"
                   creditos.CategoriaMes = "B".

        WHEN 4 THEN
            ASSIGN rep_creditos.CategoriaMes = "C"
                   creditos.CategoriaMes = "C".

        WHEN 5 THEN
            ASSIGN rep_creditos.CategoriaMes = "D"
                   creditos.CategoriaMes = "D".

        OTHERWISE
            ASSIGN rep_creditos.CategoriaMes = "E"
                   creditos.CategoriaMes = "E".
    END CASE.

    IF rep_creditos.CategoriaMes <> "A" AND rep_creditos.CategoriaMes <> "B" AND rep_creditos.INT_Corriente > 0 THEN DO:
        FIND FIRST Tmp-ProvisInt WHERE Tmp-ProvisInt.P_Age = rep_creditos.Agencia
                                   AND Tmp-ProvisInt.P_Tipo = rep_creditos.Tip_Credito
                                   AND Tmp-ProvisInt.P_CtaPro = Wk_CtaProIntPro
                                   AND Tmp-ProvisInt.P_CodVar = 2 NO-ERROR.
        IF NOT AVAILABLE Tmp-ProvisInt THEN DO:
            CREATE Tmp-ProvisInt.
            ASSIGN Tmp-ProvisInt.P_Age = rep_creditos.Agencia
                   Tmp-ProvisInt.P_Tipo = rep_creditos.Tip_Credito
                   Tmp-ProvisInt.P_CtaPro = Wk_CtaProIntPro
                   Tmp-ProvisInt.P_CtaGto = Wk_CtaProIntGto
                   Tmp-ProvisInt.P_CodVar = 2.
        END.

        ASSIGN Tmp-ProvisInt.P_Valor = Tmp-ProvisInt.P_Valor + rep_creditos.INT_Corrientes
               rep_creditos.Provision_Interes = rep_creditos.INT_Corrientes
               creditos.Provision_Interes = rep_creditos.INT_Corrientes
               Pro.Provint = rep_creditos.Provision_Interes.
    END.

    IF rep_creditos.CategoriaMes <> "A" AND rep_creditos.CategoriaMes <> "B" AND (rep_creditos.Polizas > 0 OR rep_creditos.Costas > 0 OR rep_creditos.Honorarios > 0) THEN DO:
        FIND FIRST Tmp-ProvisCos WHERE Tmp-ProvisCos.P_Age = rep_creditos.Agencia
                                   AND Tmp-ProvisCos.P_Tipo = rep_creditos.Tip_Credito
                                   AND Tmp-ProvisCos.P_CtaPro = Wk_CtaProCosPro
                                   AND Tmp-ProvisCos.P_CodVar = 2 NO-ERROR.
        IF NOT AVAILABLE Tmp-ProvisCos THEN DO:
            CREATE Tmp-ProvisCos.
            ASSIGN Tmp-ProvisCos.P_Age = rep_creditos.Agencia
                   Tmp-ProvisCos.P_Tipo = rep_creditos.Tip_Credito
                   Tmp-ProvisCos.P_CtaPro = Wk_CtaProCosPro
                   Tmp-ProvisCos.P_CtaGto = Wk_CtaProCosGto
                   Tmp-ProvisCos.P_CodVar = 2.
        END.

        ASSIGN Tmp-ProvisCos.P_Valor = Tmp-ProvisCos.P_Valor + (rep_creditos.Polizas + rep_creditos.Costas + rep_creditos.Honorarios)
               rep_creditos.Provision_Otros = rep_creditos.Polizas + rep_creditos.Costas + rep_creditos.Honorarios
               creditos.Provision_Otros = rep_creditos.Polizas + rep_creditos.Costas + rep_creditos.Honorarios
               Pro.ProvCos = rep_creditos.Provision_Otros.
    END.

    FIND FIRST Tmp-Calif WHERE Tmp-Calif.C_Age = rep_creditos.Agencia
                           AND Tmp-Calif.C_Tipo = rep_creditos.Tip_Credito
                           AND Tmp-Calif.C_CtaCal = W_CtaCal
                           AND Tmp-Calif.C_CodVar = 2 NO-ERROR.
    IF NOT AVAILABLE Tmp-Calif THEN DO:
        CREATE Tmp-Calif.
        ASSIGN Tmp-Calif.C_Age = rep_creditos.Agencia
               Tmp-Calif.C_CtaCal = W_CtaCal
               Tmp-Calif.C_Tipo = rep_creditos.Tip_Credito
               Tmp-Calif.C_CodVar = 2.
    END.

    Tmp-Calif.C_Valor = Tmp-Calif.C_Valor + rep_creditos.Sdo_Capital.

    FIND FIRST Tmp-CalifDet WHERE Tmp-CalifDet.C_Age = rep_creditos.Agencia
                              AND Tmp-CalifDet.C_CtaCal = W_CtaCal
                              AND Tmp-CalifDet.C_Nit = rep_creditos.Nit
                              AND Tmp-CalifDet.C_NumCre = rep_creditos.Num_Credito
                              AND Tmp-CalifDet.C_CodVar = CarteraVencida.Cod_Califica NO-ERROR.
    IF NOT AVAILABLE Tmp-CalifDet THEN DO:
        CREATE Tmp-CalifDet.
        ASSIGN Tmp-CalifDet.C_Age = rep_creditos.Agencia
               Tmp-CalifDet.C_CtaCal = W_CtaCal
               Tmp-CalifDet.C_CodVar = CarteraVencida.Cod_Califica
               Tmp-CalifDet.C_Nit = rep_creditos.Nit
               Tmp-CalifDet.C_NumCre = rep_creditos.Num_Credito
               Tmp-CalifDet.C_Valor = rep_creditos.Sdo_Capital.
    END.

    IF rep_creditos.INT_Corrientes > 0 THEN DO:
        FIND FIRST Tmp-CalifInt WHERE Tmp-CalifInt.C_Age = rep_creditos.Agencia
                                  AND Tmp-CalifInt.C_Tipo = rep_creditos.Tip_Credito
                                  AND Tmp-CalifInt.C_CtaCal = CarteraVencida.CtaCal_Interes
                                  AND Tmp-CalifInt.C_CodVar = 2 NO-ERROR.
        IF NOT AVAILABLE Tmp-CalifInt THEN DO:
            CREATE Tmp-CalifInt.
            ASSIGN Tmp-CalifInt.C_Age = rep_creditos.Agencia
                   Tmp-CalifInt.C_Tipo = rep_creditos.Tip_Credito
                   Tmp-CalifInt.C_CtaCal = CarteraVencida.CtaCal_Interes
                   Tmp-CalifInt.C_CodVar = 2.
        END.

        Tmp-CalifInt.C_Valor = Tmp-CalifInt.C_Valor + rep_creditos.INT_Corrientes.
    END.

    IF (rep_creditos.Int_MoraDifCob + rep_creditos.Int_DifCobro) > 0 THEN DO:
        FIND FIRST Tmp-CalifConting WHERE Tmp-CalifConting.C_Age = rep_creditos.Agencia
                                      AND Tmp-CalifConting.C_Tipo = rep_creditos.Tip_Credito
                                      AND Tmp-CalifConting.C_CtaCal = CarteraVencida.Cta_IntContingDb
                                      AND Tmp-CalifConting.C_CodVar = 2 NO-ERROR.
        IF NOT AVAILABLE Tmp-CalifConting THEN DO:
            CREATE Tmp-CalifConting.
            ASSIGN Tmp-CalifConting.C_Age = rep_creditos.Agencia
                   Tmp-CalifConting.C_Tipo = rep_creditos.Tip_Credito
                   Tmp-CalifConting.C_CtaCal = CarteraVencida.Cta_IntContingDb
                   Tmp-CalifConting.Cruce = CarteraVencida.Cta_IntContingCr
                   Tmp-CalifConting.C_CodVar = 2.
        END.
        
        Tmp-CalifConting.C_Valor = Tmp-CalifConting.C_Valor + (rep_creditos.Int_MoraDifCob + rep_creditos.Int_DifCobro).
    END.

    IF (rep_creditos.Polizas GT 0 OR rep_creditos.Costas > 0 OR rep_creditos.Honorarios > 0) THEN DO:
        FIND FIRST Tmp-CalifCos WHERE Tmp-CalifCos.C_Age = rep_creditos.Agencia
                                  AND Tmp-CalifCos.C_Tipo = rep_creditos.Tip_Credito
                                  AND Tmp-CalifCos.C_CtaCal = CarteraVencida.CtaCal_Costas
                                  AND Tmp-CalifCos.C_CodVar = 2 NO-ERROR.
        IF NOT AVAILABLE Tmp-CalifCos THEN DO:
            CREATE Tmp-CalifCos.
            ASSIGN Tmp-CalifCos.C_Age = rep_creditos.Agencia
                  Tmp-CalifCos.C_Tipo = rep_creditos.Tip_Credito
                  Tmp-CalifCos.C_CtaCal = CarteraVencida.CtaCal_Costas
                  Tmp-CalifCos.C_CodVar = 2.
        END.

        Tmp-CalifCos.C_Valor = Tmp-CalifCos.C_Valor + (rep_creditos.Polizas + rep_creditos.Costas + rep_creditos.Honorarios).
    END.

    IF LAST-OF(rep_creditos.Tip_Credito) THEN
        W_CalMayor = 1.
END.

Msaje:SCREEN-VALUE  = "Sale procal".

RUN Graba_Informes.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proceso W_Cierre_Cartera 
PROCEDURE Proceso :
Procesando:
DO TRANSACTION ON ERROR UNDO Procesando:
    RUN ProCal_Cierre NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.

    Msaje:SCREEN-VALUE IN FRAME F_CieCar = "Hallando Saldos en Sal_Cuenta.".
    RUN Halla_Saldos.

    Msaje:SCREEN-VALUE IN FRAME F_CieCar = "Generando Comprobante de Capital.".
    RUN Contab_Calificar_Capital NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.

    Msaje:SCREEN-VALUE IN FRAME F_CieCar = "Generando Comprobante de Intereses.".
    RUN Contab_Calificar_Interes NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.
    
    /*Msaje:SCREEN-VALUE IN FRAME F_CieCar = "Generando Comprobante de Cobro-Jurídico.".
    RUN Contab_Calificar_Costas NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.*/

    /*Msaje:SCREEN-VALUE IN FRAME F_CieCar = "Generando Comprobante de Intreses-Contingentes.".
    RUN Contab_Calificar_Conting NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.*/

    
    

    
    Msaje:SCREEN-VALUE IN FRAME F_CieCar = "Generando Comprobante de Provisión-Capital.".
    RUN Contab_Provision_Capital NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.
    
    Msaje:SCREEN-VALUE IN FRAME F_CieCar = "Generando Comprobante de Provisión-Intereses.".
    RUN Contab_Provision_Interes NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.
    
    /*Msaje:SCREEN-VALUE IN FRAME F_CieCar = "Generando Comprobante de Provisión-CobJurid.".
    RUN Contab_Provision_Costas NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.*/

    IF NOT W_PrelProc THEN DO:
        DEFI VAR Listado AS CHAR FORM "X(40)".

        Listado = W_PathSpl + "CpteCieCar-Preli-" + STRING(W_FecMes,"999999")  + ".Lst".

        {Incluido\Imprimir.I "listado"}

        RETURN ERROR.
    END.
END.   /*Fin Tx.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W_Cierre_Cartera 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR NN AS INTEG FORM "999".
  DEFI VAR TTotD LIKE TMov_Contable.Db.
  DEFI VAR TTotC LIKE TMov_Contable.Cr.
  DEFI VAR TotD  LIKE TMov_Contable.Db.
  DEFI VAR TotC  LIKE TMov_Contable.Cr.
  DEFI VAR CTotD  LIKE TMov_Contable.Db.
  DEFI VAR CTotC  LIKE TMov_Contable.Cr.

{Incluido\RepEncabezado.I}

 ASSIGN W_Reporte = "Cpte Resumen : Contabilización Preliminar Cierre-Cartera      Fecha del Informe: " +
                     STRING(W_FecMes,"99/99/9999") + "      Hora : " + STRING(TIME,"HH:MM:SS").
        
 VIEW FRAME F-Encabezado.
 VIEW FRAME f-ftr.

 
    FOR EACH TMov_Contable BREAK BY TMov_Contable.Agencia 
                                 BY TMov_Contable.Num_Docum
                                 BY TMov_Contable.Cuenta:        
            ASSIGN CTotD  = CTotD  + TMov_Contable.Db
                   CTotC  = CTotC  + TMov_Contable.Cr
                   TotD  = TotD  + TMov_Contable.Db
                   TotC  = TotC  + TMov_Contable.Cr
                   TTotD = TTotD + TMov_Contable.Db
                   TTotC = TTotC + TMov_Contable.Cr.

            FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ TMov_Contable.Cuenta                      
                                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.                         
            DISPLAY TMov_Contable.Agencia      LABEL "Ag."                                             
                    TMov_Contable.Cuenta       LABEL "Cta-Contable"                                    
                    Cuentas.Nombre             LABEL "Descripciòn de la Cuenta" WHEN AVAIL(Cuentas) FORM "X(25)"
                    TMov_Contable.Coment       LABEL "Descrip.del Asiento"      FORM "X(20)"                                  
                    TMov_Contable.Db           LABEL "TOTAL DEBITOS"  FORM "->>>>>>,>>>,>>9.99"    
                    TMov_Contable.Cr           LABEL "TOTAL CREDITOS" FORM "->>>>>>,>>>,>>9.99"  SKIP(0)  
                WITH DOWN WIDTH 140 FRAME F21 USE-TEXT NO-LABELS STREAM-IO NO-BOX.   

            IF LAST-OF(TMov_Contable.Num_Docum) THEN DO:
                DISPLAY SKIP(1)
                        "                     TOTAL Documento-------->                      ------------------ ------------------"
                         SKIP
                        "                                                                  "
                        CTotD      FORM "->>>>>>,>>>,>>9.99"
                        CTotC      FORM "->>>>>>,>>>,>>9.99"
                   WITH DOWN WIDTH 180 FRAME FT21Tdoc USE-TEXT NO-LABELS STREAM-IO NO-BOX.

                ASSIGN CTotD  = 0 
                       CTotC  = 0.
            END.

            IF LAST-OF(TMov_Contable.Agencia) THEN DO:
                DISPLAY SKIP(1)
                        "                     TOTAL Agencia---------->                      ------------------ ------------------"
                         SKIP
                        "                                                                  "
                        TotD      FORM "->>>>>>,>>>,>>9.99"
                        TotC      FORM "->>>>>>,>>>,>>9.99"
                   WITH DOWN WIDTH 180 FRAME FT21Tdoc USE-TEXT NO-LABELS STREAM-IO NO-BOX.

                ASSIGN TotD  = 0 
                       TotC  = 0.
            END.
    END.
   
 DISPLAY SKIP(1)
         "                     TOTAL FINAL------------>                      ------------------ ------------------"
         SKIP
         "                                                                  "
         TTotD      FORM "->>>>>>,>>>,>>9.99"
         TTotC      FORM "->>>>>>,>>>,>>9.99"
      WITH DOWN WIDTH 180 FRAME FgFT21T USE-TEXT NO-LABELS STREAM-IO NO-BOX.
            
 ASSIGN TotD  = 0
        TotC  = 0
        TTotD = 0
        TTotC = 0
        CTotD = 0
        CTotC = 0.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proceso_bck W_Cierre_Cartera 
PROCEDURE Proceso_bck :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  Procesando:
  DO TRANSACTION ON ERROR UNDO Procesando:
     RUN ProCal_Cierre NO-ERROR.            
     IF ERROR-STATUS:ERROR THEN            
        RETURN ERROR.                      
                                           
     Msaje:SCREEN-VALUE IN FRAME F_CieCar = "Hallando Saldos en Sal_Cuenta.".
     RUN Halla_Saldos.                     
                                           
     Msaje:SCREEN-VALUE IN FRAME F_CieCar = "Generando Comprobante de Capital.".
     RUN Contab_Calificar_Capital NO-ERROR.
     IF ERROR-STATUS:ERROR THEN            
        RETURN ERROR. 
                                           
     /*
     Msaje:SCREEN-VALUE IN FRAME F_CieCar = "Generando Comprobante de Intereses.".
     RUN Contab_Calificar_Interes NO-ERROR.
     IF ERROR-STATUS:ERROR THEN            
        RETURN ERROR.      
     */  
                                           
     /*Msaje:SCREEN-VALUE IN FRAME F_CieCar = "Generando Comprobante de Cobro-Jurídico.".
     RUN Contab_Calificar_Costas NO-ERROR. 
     IF ERROR-STATUS:ERROR THEN            
        RETURN ERROR.*/
                                           
    /* Msaje:SCREEN-VALUE IN FRAME F_CieCar = "Generando Comprobante de Intreses-Contingentes.".
     RUN Contab_Calificar_Conting NO-ERROR.
     IF ERROR-STATUS:ERROR THEN            
        RETURN ERROR. */
                                           
     Msaje:SCREEN-VALUE IN FRAME F_CieCar = "Generando Comprobante de Provisión-Capital.".
     RUN Contab_Provision_Capital NO-ERROR.
     IF ERROR-STATUS:ERROR THEN            
        RETURN ERROR.                      
   
    /* Msaje:SCREEN-VALUE IN FRAME F_CieCar = "Generando Comprobante de Provisión-Intereses.".
     RUN Contab_Provision_Interes NO-ERROR.
     IF ERROR-STATUS:ERROR THEN            
        RETURN ERROR.    */                 
                                           
    /* Msaje:SCREEN-VALUE IN FRAME F_CieCar = "Generando Comprobante de Provisión-CobJurid.".
    RUN Contab_Provision_Costas NO-ERROR. 
     IF ERROR-STATUS:ERROR THEN            
        RETURN ERROR.  */

     IF NOT W_PrelProc THEN DO:
        DEFI VAR Listado AS CHAR FORM "X(40)".                                                                   
                                                                                                                
        ASSIGN Listado = W_PathSpl + "CpteCieCar-Preli-" + STRING(W_FecMes,"999999")  + ".Lst".
              
       {Incluido\Imprimir.I "listado"} 
       

        RETURN ERROR.
     END.

  END.   /*Fin Tx.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProvisionGeneral W_Cierre_Cartera 
PROCEDURE ProvisionGeneral :
DEFINE VAR saldoFinal AS DECIMAL.
DEFINE VAR cont AS INTEGER.
DEFINE VAR provision AS DECIMAL.
DEFINE VAR saldoActual AS DECIMAL.
DEFINE VAR valContabilizar AS DECIMAL.
DEFINE VAR vDebito AS DECIMAL.
DEFINE VAR vCredito AS DECIMAL.

W_Cbte = 20.

FOR EACH agencias NO-LOCK:
    FIND FIRST Comprobantes WHERE Comprobantes.Agencia EQ agencias.agencia
                              AND Comprobantes.Comprobante EQ W_Cbte NO-ERROR.
    IF NOT AVAILABLE Comprobantes THEN DO:
        IF LOCKED Comprobantes THEN
            RETRY.

        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "Error al contabilizar la Provisión General" SKIP
                    "Agencia:" agencias.agencia SKIP
                    "Comprobante:" W_Cbte
                VIEW-AS ALERT-BOX ERROR.

            RETURN ERROR.
        END.
    END.

    ASSIGN W_NumCbt = Comprobantes.Secuencia + 1
           Comprobantes.Secuencia = Comprobantes.Secuencia + 1.

    FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
    
    /* Consumo Nómina */
    saldoFinal = 0.

    FOR EACH sal_cuenta WHERE sal_cuenta.agencia = agencias.agencia
                          AND sal_cuenta.ano = YEAR(w_fecMes)
                          AND SUBSTRING(sal_cuenta.cuenta,1,4) = "1441" NO-LOCK:
        saldoFinal = saldoFinal + sal_cuenta.sal_inicial.

        DO cont = 1 TO MONTH(w_fecMes):
            saldoFinal = saldoFinal + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
        END.
    END.

    provision = saldoFinal * 0.01.

    RUN buscarSdoCta(INPUT agencias.agencia,
                     INPUT "14680501",
                     OUTPUT saldoActual).

    valContabilizar = provision /*-*/ + saldoActual.

    ASSIGN vDebito = valContabilizar
           vCredito = valContabilizar * -1.

    RUN contab_partNuevas(INPUT agencias.agencia,
                          INPUT "51152901",
                          INPUT "ProvGeneral-Nómina",
                          INPUT vDebito).

    RUN contab_partNuevas(INPUT agencias.agencia,
                          INPUT "14680501",
                          INPUT "ProvGeneral-Nómina",
                          INPUT vCredito).

    /* Consumo Caja */
    saldoFinal = 0.

    FOR EACH sal_cuenta WHERE sal_cuenta.agencia = agencias.agencia
                          AND sal_cuenta.ano = YEAR(w_fecMes)
                          AND SUBSTRING(sal_cuenta.cuenta,1,4) = "1442" NO-LOCK:
        saldoFinal = saldoFinal + sal_cuenta.sal_inicial.

        DO cont = 1 TO MONTH(w_fecMes):
            saldoFinal = saldoFinal + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
        END.
    END.

    provision = saldoFinal * 0.01.

    RUN buscarSdoCta(INPUT agencias.agencia,
                     INPUT "14680502",
                     OUTPUT saldoActual).

    valContabilizar = provision /*-*/ + saldoActual.

    ASSIGN vDebito = valContabilizar
           vCredito = valContabilizar * -1.

    RUN contab_partNuevas(INPUT agencias.agencia,
                          INPUT "51152902",
                          INPUT "ProvGeneral-Caja",
                          INPUT vDebito).

    RUN contab_partNuevas(INPUT agencias.agencia,
                          INPUT "14680502",
                          INPUT "ProvGeneral-Caja",
                          INPUT vCredito).

    /* Vivienda (Nómina) */
    saldoFinal = 0.

    FOR EACH sal_cuenta WHERE sal_cuenta.agencia = agencias.agencia
                          AND sal_cuenta.ano = YEAR(w_fecMes)
                          AND (SUBSTRING(sal_cuenta.cuenta,1,4) = "1404" OR
                               SUBSTRING(sal_cuenta.cuenta,1,4) = "1404") NO-LOCK:
        saldoFinal = saldoFinal + sal_cuenta.sal_inicial.

        DO cont = 1 TO MONTH(w_fecMes):
            saldoFinal = saldoFinal + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
        END.
    END.

    provision = saldoFinal * 0.01.

    RUN buscarSdoCta(INPUT agencias.agencia,
                     INPUT "14681001",
                     OUTPUT saldoActual).

    valContabilizar = provision /*-*/ + saldoActual.

    ASSIGN vDebito = valContabilizar
           vCredito = valContabilizar * -1.

    RUN contab_partNuevas(INPUT agencias.agencia,
                          INPUT "51152901",
                          INPUT "ProvGeneral-Nómina",
                          INPUT vDebito).

    RUN contab_partNuevas(INPUT agencias.agencia,
                          INPUT "14681001",
                          INPUT "ProvGeneral-Nómina",
                          INPUT vCredito).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

