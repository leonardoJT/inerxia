&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Prc_CierreMes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Prc_CierreMes 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

ON RETURN TAB.

{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I   "SHARED"}

DEFINE VAR W_CdC AS INTEGER.
DEFINE VAR W_Nit AS CHARACTER.
DEFINE VAR W_Sdo AS DECIMAL.
DEFINE VAR K AS INTEGER FORMAT "99".

DEFINE TEMP-TABLE ttAnexos
    FIELD agencia AS INTEGER
    FIELD nit AS CHARACTER
    FIELD cuenta AS CHARACTER
    FIELD cen_costos AS INTEGER
    FIELD saldo AS DECIMAL
    INDEX idx agencia nit cuenta cen_costos.

DEFINE TEMP-TABLE ttSaldos
    FIELD agencia AS INTEGER
    FIELD cuenta AS CHARACTER
    FIELD saldo AS DECIMAL
    INDEX idx agencia cuenta.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Proc

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-120 RECT-314 BUTTON-5 W_CmbOfi ~
Btn_AbrirMes W_MesC W_AnoC Btn_Cerrar Btn_Done 
&Scoped-Define DISPLAYED-OBJECTS W_CmbOfi W_MesC W_Nmes W_AnoC W_mensaje ~
W_Cont 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 W_MesC W_AnoC 
&Scoped-define List-2 W_mensaje 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Prc_CierreMes AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_AbrirMes 
     LABEL "&AbrirMes" 
     SIZE 10 BY 1.46 TOOLTIP "Abre el mes contable".

DEFINE BUTTON Btn_Cerrar 
     LABEL "&Cerrar Mes" 
     SIZE 10 BY 1.46 TOOLTIP "Realiza el Proceso de Cierre contable".

DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Salir" 
     SIZE 10 BY 1.27
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 5" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(30)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 31.29 BY 1 TOOLTIP "Agencias Activas"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_AnoC AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Año" 
     VIEW-AS FILL-IN 
     SIZE 4.72 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_Cont AS DECIMAL FORMAT "->>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.57 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_mensaje AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33.14 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_MesC AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Mes" 
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_Nmes AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-120
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50.43 BY 6.73.

DEFINE RECTANGLE RECT-314
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14.29 BY 6.73.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Proc
     BUTTON-5 AT ROW 3.12 COL 65.43
     W_CmbOfi AT ROW 3.88 COL 12.72 COLON-ALIGNED
     Btn_AbrirMes AT ROW 4.81 COL 65.43 HELP
          "Permite Realizar el Proceso de Cierre"
     W_MesC AT ROW 6.35 COL 12.72 COLON-ALIGNED
     W_Nmes AT ROW 6.35 COL 16 COLON-ALIGNED NO-LABEL
     W_AnoC AT ROW 6.35 COL 39 COLON-ALIGNED
     Btn_Cerrar AT ROW 6.38 COL 65.57 HELP
          "Permite Realizar el Proceso de Cierre"
     Btn_Done AT ROW 8 COL 65.43 HELP
          "Sale del proceso de Depreciación y Ajustes"
     W_mensaje AT ROW 8.73 COL 4.86 COLON-ALIGNED NO-LABEL
     W_Cont AT ROW 8.73 COL 39 COLON-ALIGNED NO-LABEL
     "Regist.Prox.Año" VIEW-AS TEXT
          SIZE 11.43 BY .5 AT ROW 8.19 COL 41
          BGCOLOR 1 
     RECT-120 AT ROW 2.85 COL 5.43
     RECT-314 AT ROW 2.85 COL 62.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82.57 BY 11.69
         BGCOLOR 17 FGCOLOR 0 FONT 4.


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
  CREATE WINDOW W-Prc_CierreMes ASSIGN
         HIDDEN             = YES
         TITLE              = "Cierre Mensual Contable, Programa W-Prc_CierreMes.W"
         HEIGHT             = 11.69
         WIDTH              = 82.57
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 82.57
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 82.57
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
/* SETTINGS FOR WINDOW W-Prc_CierreMes
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Proc
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN W_AnoC IN FRAME F_Proc
   1                                                                    */
/* SETTINGS FOR FILL-IN W_Cont IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_mensaje IN FRAME F_Proc
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN W_MesC IN FRAME F_Proc
   1                                                                    */
/* SETTINGS FOR FILL-IN W_Nmes IN FRAME F_Proc
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Prc_CierreMes)
THEN W-Prc_CierreMes:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Proc
/* Query rebuild information for FRAME F_Proc
     _Query            is NOT OPENED
*/  /* FRAME F_Proc */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Prc_CierreMes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Prc_CierreMes W-Prc_CierreMes
ON END-ERROR OF W-Prc_CierreMes /* Cierre Mensual Contable, Programa W-Prc_CierreMes.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Prc_CierreMes W-Prc_CierreMes
ON WINDOW-CLOSE OF W-Prc_CierreMes /* Cierre Mensual Contable, Programa W-Prc_CierreMes.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_AbrirMes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_AbrirMes W-Prc_CierreMes
ON CHOOSE OF Btn_AbrirMes IN FRAME F_Proc /* AbrirMes */
DO:
  RUN _SetCurs.r("WAIT").
  
  IF W_MesC LE 0 OR W_MesC GT 12
  OR W_AnoC LE 0 THEN DO:
     MESSAGE "El Mes o el Año no es vàlido, Teclee uno correcto por favor..." VIEW-AS ALERT-BOX.
                
     ASSIGN W_MesC              = MONTH(W_Fecha)         
            W_MesC:SCREEN-VALUE = STRING(MONTH(W_Fecha)).       
                                                                
     APPLY "LEAVE" TO W_MesC.                                     
                                                                
     RETURN NO-APPLY.                                                  
  END.
    
  MESSAGE "El mes contable para Abrir es : " W_Mesc " del Año : " W_AnoC SKIP
          "                              Està Segura(o)..." 
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Abrir Mes"
          UPDATE W_RptaCh1 AS LOGICAL.
  IF NOT W_RptaCh1 THEN RETURN.
  ELSE FOR EACH Agencias WHERE Agencias.Agencia GE W_OfiIni                                                                               
                           AND Agencias.Agencia LE W_OfiFin
                           AND Agencias.Estado  NE 3 NO-LOCK:
       FIND FIRST ProcDia WHERE ProcDia.Agencia          EQ Agencias.Agencia                                                                     
                           AND YEAR (ProcDia.Fecha_Proc) EQ W_AnoC                                                                              
                           AND MONTH(ProcDia.Fecha_Proc) EQ W_MesC 
                           AND ProcDia.Cod_Proceso       EQ 7  NO-ERROR.   
       IF NOT AVAIL(ProcDia) THEN DO:
          CREATE ProcDia.
          ASSIGN ProcDia.Fecha_Proc = DATE(W_MesC,DAY(TODAY),W_AnoC).
       END.
        
       ASSIGN ProcDia.Agencia     = Agencias.Agencia
              ProcDia.Cod_Proceso = 7 
              ProcDia.Estado      = 1
              ProcDia.Usuario     = W_Usuario
              W_Mensaje:SCREEN-VALUE = "El Mes ha sido Abierto!...!".    
  END.    
  
  RUN _SetCurs.r("ARROW").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cerrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cerrar W-Prc_CierreMes
ON CHOOSE OF Btn_Cerrar IN FRAME F_Proc /* Cerrar Mes */
DO:
    DEFI VAR W_AAnt LIKE W_AnoC.
    DEFI VAR W_MAnt LIKE W_MesC.

    RUN _SetCurs.r("WAIT").

    IF W_MesC <= 0 OR W_MesC > 12 OR W_AnoC <= 0 THEN DO:
        MESSAGE "El Mes o el Año no es válido, Teclee uno correcto por favor..."
            VIEW-AS ALERT-BOX.

        ASSIGN W_MesC = MONTH(W_Fecha)
               W_MesC:SCREEN-VALUE = STRING(MONTH(W_Fecha)).

        APPLY "LEAVE" TO W_MesC.
        RETURN NO-APPLY.
    END.

    ASSIGN W_AAnt = W_AnoC
           W_MAnt = W_MesC - 1.

    IF W_MAnt <= 0 THEN
        ASSIGN W_AAnt = W_AnoC - 1
               W_MAnt = 12.

    FOR EACH Agencias WHERE Agencias.Agencia >= W_OfiIni
                        AND Agencias.Agencia <= W_OfiFin
                        AND Agencias.Estado <> 3 NO-LOCK:
        FIND FIRST ProcDia WHERE ProcDia.Agencia = Agencias.Agencia
                             AND YEAR(ProcDia.Fecha_Proc) = W_AnoC
                             AND MONTH(ProcDia.Fecha_Proc) = W_MesC
                             AND DAY(ProcDia.Fecha_Proc + 1) = 1
                             AND ProcDia.Cod_Proceso = 6
                             AND ProcDia.Estado = 2 NO-LOCK NO-ERROR.
        IF NOT AVAIL(ProcDia) THEN DO:
            MESSAGE "El Ultimo-Dìa de este mes en La Agencia : " Agencias.Agencia " No ha sido cerrado..." SKIP
                    "                              No se permite cerrar este mes."
                VIEW-AS ALERT-BOX ERROR.

            RETURN.
        END.
    END.

    MESSAGE "El mes contable para Cerrar es : " W_Mesc W_NMes " del Año : " W_AnoC SKIP
            "                              Està Segura(o)..."
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Cerrar Mes" UPDATE W_RptaCh1 AS LOGICAL.

    IF NOT W_RptaCh1 THEN
        RETURN.

    IF W_Mesc = 12 THEN DO:
        MESSAGE "El mes contable para Cerrar es Diciembre, del Año" W_AnoC SKIP
                "                Se van a Generar los Sdos-Iniciales del Año" W_AnoC + 1 SKIP
                "                Està Segura(o)...de cerrar Diciembre...?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Cerrar Año" UPDATE W_RptaAno AS LOGICAL.

        IF NOT W_RptaAno THEN
            RETURN.
    END.

    FOR EACH Agencias WHERE Agencias.Agencia >= W_OfiIni
                        AND Agencias.Agencia <= W_OfiFin
                        AND Agencias.Estado <> 3 NO-LOCK:
        IF W_Mesc = 12 THEN DO:
            RUN CerrarAno NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                W_Mensaje:SCREEN-VALUE = "El Proceso Tiene errores...Verifique por favor.".
                RUN _SetCurs.r("ARROW").
                RETURN.
            END.
        END.

        FIND FIRST ProcDia WHERE ProcDia.Agencia = Agencias.Agencia
                             AND YEAR(ProcDia.Fecha_Proc) = W_AnoC
                             AND MONTH(ProcDia.Fecha_Proc) = W_MesC
                             AND ProcDia.Cod_Proceso = 7 NO-ERROR.
        IF NOT AVAIL(ProcDia) THEN DO:
            CREATE ProcDia.
            ProcDia.Fecha_Proc = DATE(W_MesC,DAY(TODAY),W_AnoC).
        END.

        ASSIGN ProcDia.Agencia = Agencias.Agencia
               ProcDia.Cod_Proceso = 7
               ProcDia.Estado = 2
               W_Mensaje:SCREEN-VALUE = "Generado el Proceso de Cierre Mensual!...!".
    END.

    RUN _SetCurs.r("ARROW").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-Prc_CierreMes
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


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-Prc_CierreMes
ON CHOOSE OF BUTTON-5 IN FRAME F_Proc /* Button 5 */
DO:
  RUN W-InfDia.R.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_AnoC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_AnoC W-Prc_CierreMes
ON LEAVE OF W_AnoC IN FRAME F_Proc /* Año */
DO:
  ASSIGN W_AnoC.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfi W-Prc_CierreMes
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


&Scoped-define SELF-NAME W_MesC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_MesC W-Prc_CierreMes
ON LEAVE OF W_MesC IN FRAME F_Proc /* Mes */
DO:
    ASSIGN W_MesC.

    CASE W_MesC:
          WHEN 1 THEN
            W_NMes = "Enero".
          WHEN 2 THEN
            W_NMes = "Febrero".
          WHEN 3 THEN
            W_NMes = "Marzo".
          WHEN 4 THEN
            W_NMes = "Abril".
          WHEN 5 THEN
            W_NMes = "Mayo".
          WHEN 6 THEN
            W_NMes = "Junio".
          WHEN 7 THEN
            W_NMes = "Julio".
          WHEN 8 THEN
            W_NMes = "Agosto".
          WHEN 9 THEN
            W_NMes = "Septiembre".
          WHEN 10 THEN
            W_NMes = "Octubre".
          WHEN 11 THEN
            W_NMes = "Noviembre".
          WHEN 12 THEN
            W_NMes = "Diciembre".
          OTHERWISE DO: 
            MESSAGE "El Mes no es vàlido, Teclee uno correcto por favor..." VIEW-AS ALERT-BOX.
                
            ASSIGN W_MesC              = MONTH(W_Fecha)
                   W_MesC:SCREEN-VALUE = STRING(MONTH(W_Fecha)).
                
            APPLY "LEAVE" TO SELF. 
                
            RETURN NO-APPLY.
                        
          END.
     END CASE. 

       
        
     W_NMes:SCREEN-VALUE = W_NMes.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Prc_CierreMes 


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
  
   ASSIGN W_OfiIni                            = W_Agencia
          W_OfiFin                            = W_Agencia
          W_MesC:SCREEN-VALUE IN FRAME F_Proc = STRING(MONTH(W_Fecha))
          W_AnoC:SCREEN-VALUE                 = STRING(YEAR(W_Fecha))
          W_Mesc
          W_AnoC.
                  
   FOR EACH Agencias WHERE Agencias.Estado  NE 3 
                       AND Agencias.Agencia GT 0 NO-LOCK:
       W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"999") + 
                        "-" + STRING(Agencias.Nombre,"X(25)")).      
   END.              
     
   ASSIGN W_CmbOfi:SCREEN-VALUE = "000 CONSOLIDADO".
     
   APPLY "VALUE-CHANGED" TO W_CmbOfi.
    
   CASE MONTH(W_Fecha):
        WHEN 1 THEN
          W_NMes = "Enero".
        WHEN 2 THEN
          W_NMes = "Febrero".
        WHEN 3 THEN
          W_NMes = "Marzo".
        WHEN 4 THEN
          W_NMes = "Abril".
        WHEN 5 THEN
          W_NMes = "Mayo".
        WHEN 6 THEN
          W_NMes = "Junio".
        WHEN 7 THEN
          W_NMes = "Julio".
        WHEN 8 THEN
          W_NMes = "Agosto".
        WHEN 9 THEN
          W_NMes = "Septiembre".
        WHEN 10 THEN
          W_NMes = "Octubre".
        WHEN 11 THEN
          W_NMes = "Noviembre".
        WHEN 12 THEN
          W_NMes = "Diciembre".
   END CASE. 

   W_NMes:SCREEN-VALUE = W_NMes.


  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CerrarAnexos W-Prc_CierreMes 
PROCEDURE CerrarAnexos :
DEFINE VAR valor AS DECIMAL.
DEFINE VAR cont AS INTEGER.
DEFINE VAR naturaleza AS CHARACTER.

EMPTY TEMP-TABLE ttAnexos.

/* 2430 --> 800197268 */
/* 2435 (243575) --> 800197268 */
/* 2440 --> 800197268 */
/* 243575 --> 899999061 */

Proceso:
DO TRANSACTION ON ERROR UNDO Proceso:
    FOR EACH anexos WHERE anexos.agencia = agencias.agencia
                      AND (SUBSTRING(anexos.cuenta,1,4) = "2430" OR
                           (SUBSTRING(anexos.cuenta,1,4) = "2435" AND SUBSTRING(anexos.cuenta,1,6) <> "243575") OR
                           SUBSTRING(anexos.cuenta,1,4) = "2440")
                      AND anexos.ano = w_anoC:
        valor = anexos.sdo_inicial.

        DO cont = 1 TO 12:
            valor = valor + anexos.cr[cont] - anexos.db[cont].
        END.

        FIND FIRST ttAnexos WHERE ttAnexos.agencia = anexos.agencia
                              AND ttAnexos.cuenta = anexos.cuenta
                              AND ttAnexos.cen_costos = anexos.cen_costos NO-ERROR.
        IF NOT AVAILABLE ttAnexos THEN DO:
            CREATE ttAnexos.
            ttAnexos.agencia = anexos.agencia.
            ttAnexos.nit = "800197268".
            ttAnexos.cuenta = anexos.cuenta.
            ttAnexos.cen_costos = anexos.cen_costos.
        END.

        ttAnexos.saldo = ttAnexos.saldo + valor.
    END.

    FOR EACH anexos WHERE anexos.agencia = agencias.agencia
                      AND SUBSTRING(anexos.cuenta,1,6) = "243575"
                      AND anexos.ano = w_anoC:
        valor = anexos.sdo_inicial.

        DO cont = 1 TO 12:
            valor = valor + anexos.cr[cont] - anexos.db[cont].
        END.

        FIND FIRST ttAnexos WHERE ttAnexos.agencia = anexos.agencia
                              AND ttAnexos.cuenta = anexos.cuenta
                              AND ttAnexos.cen_costos = anexos.cen_costos NO-ERROR.
        IF NOT AVAILABLE ttAnexos THEN DO:
            CREATE ttAnexos.
            ttAnexos.agencia = anexos.agencia.
            ttAnexos.nit = "899999061".
            ttAnexos.cuenta = anexos.cuenta.
            ttAnexos.cen_costos = anexos.cen_costos.
        END.

        ttAnexos.saldo = ttAnexos.saldo + valor.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CerrarAno W-Prc_CierreMes 
PROCEDURE CerrarAno :
DEFINE VAR saldoFinal AS DECIMAL.

DO TRANSACTION ON ERROR UNDO:
    RUN cerrarAnexos.
    RUN cerrarCuentas.

    FOR EACH Anexos WHERE Anexos.Agencia = Agencias.Agencia   /*Inicializa el sdo anexos en 0*/
                      AND Anexos.Ano = W_AnoC + 1:
        ASSIGN Anexos.Sdo_Inicial = 0
               W_Cont = W_Cont + 1.
    END.

    FOR EACH Sal_Cuenta WHERE Sal_Cuenta.Agencia = Agencias.Agencia   /*Inicializa el sdo cuentas en 0*/
                          AND Sal_Cuenta.Ano = W_AnoC + 1:
        ASSIGN Sal_Cuenta.Sal_Inicial = 0
               W_Cont = W_Cont + 1.
    END.

    W_Cont:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Cont).

    FOR EACH Anexos WHERE Anexos.Agencia = Agencias.Agencia
                      AND Anexos.Ano = W_AnoC NO-LOCK BREAK BY Anexos.Cuenta:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta = Anexos.Cuenta NO-LOCK NO-ERROR.

        /* 2430 --> 800197268 */
        /* 2435 (243575) --> 800197268 */
        /* 2440 --> 800197268 */
        /* 243575 --> 899999061 */

        IF SUBSTRING(anexos.cuenta,1,4) = "2430" OR
           SUBSTRING(anexos.cuenta,1,4) = "2435" OR
           SUBSTRING(anexos.cuenta,1,4) = "2440" THEN
            NEXT.

        ASSIGN W_CdC = Anexos.Cen_Costos
               W_Nit = Anexos.Nit
               W_Sdo = Anexos.Sdo_Inicial.

        DO K = 1 TO 12:
            IF Cuentas.Natur = "DB" THEN
                W_Sdo = W_Sdo + (Anexos.Db[K] - Anexos.Cr[K]).
            ELSE
                W_Sdo = W_Sdo + (Anexos.Cr[K] - Anexos.Db[K]).
        END.

        IF W_Sdo <> 0 THEN
            RUN CreaAnexo.
    END.

    /* Paso los anexos de cierre */
    FOR EACH ttAnexos WHERE ttAnexos.agencia = agencias.agencia
                        AND ttAnexos.saldo <> 0 NO-LOCK:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta = ttAnexos.cuenta NO-LOCK NO-ERROR.

        ASSIGN W_CdC = ttAnexos.Cen_Costos
               W_Nit = ttAnexos.Nit
               W_Sdo = ttAnexos.saldo.

        RUN creaAnexo.
    END.

    FOR EACH Sal_Cuenta WHERE Sal_Cuenta.Agencia = Agencias.Agencia
                          AND SUBSTRING(sal_cuenta.cuenta,1,1) <> "4"
                          AND SUBSTRING(sal_cuenta.cuenta,1,1) <> "5"
                          AND SUBSTRING(sal_cuenta.cuenta,1,1) <> "6"
                          AND Sal_Cuenta.Ano = W_AnoC NO-LOCK BREAK BY Sal_Cuenta.Cuenta:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta = Sal_Cuenta.Cuenta NO-LOCK NO-ERROR.

        ASSIGN W_CdC = Sal_Cuenta.Cen_Costos
               W_Sdo = Sal_Cuenta.Sal_Inicial.

        DO K = 1 TO 12:
            IF Cuentas.Natur = "DB" THEN
                W_Sdo = W_Sdo + (Sal_Cuenta.Db[K] - Sal_Cuenta.Cr[K]).
            ELSE
                W_Sdo = W_Sdo + (Sal_Cuenta.Cr[K] - Sal_Cuenta.Db[K]).
        END.

        IF W_Sdo <> 0 THEN
            RUN CreaSalCta.
    END.

    /* Contabiliza Excedentes o Pérdidas */
    FOR EACH ttSaldos WHERE ttSaldos.agencia = agencias.agencia AND ttSaldos.saldo <> 0 NO-LOCK:
        IF ttSaldos.cuenta = "4" THEN
            saldoFinal = saldoFinal + ttSaldos.saldo.
        ELSE
            saldoFinal = saldoFinal - ttSaldos.saldo.
    END.

    IF saldoFinal > 0 THEN
        FIND FIRST Cuentas WHERE Cuentas.Cuenta = "35050501" NO-LOCK NO-ERROR.

    IF saldoFinal < 0 THEN
        FIND FIRST Cuentas WHERE Cuentas.Cuenta = "35100501" NO-LOCK NO-ERROR.

    w_cdc = 999.
    w_sdo = saldoFinal.

    RUN CreaSalCta.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CerrarCuentas W-Prc_CierreMes 
PROCEDURE CerrarCuentas :
DEFINE VAR valor AS DECIMAL.
DEFINE VAR cont AS INTEGER.
DEFINE VAR naturaleza AS CHARACTER.

EMPTY TEMP-TABLE ttSaldos.

Proceso:
DO TRANSACTION ON ERROR UNDO Proceso:
    FOR EACH sal_cuenta WHERE (SUBSTRING(sal_cuenta.cuenta,1,1) = "4" OR
                               SUBSTRING(sal_cuenta.cuenta,1,1) = "5" OR
                               SUBSTRING(sal_cuenta.cuenta,1,1) = "6")
                           AND sal_cuenta.ano = w_anoC
                           AND sal_cuenta.agencia = agencias.agencia:
        valor = sal_cuenta.sal_inicial.
    
        DO cont = 1 TO 12:
            IF SUBSTRING(sal_cuenta.cuenta,1,1) = "5" OR SUBSTRING(sal_cuenta.cuenta,1,1) = "6" THEN
                valor = valor + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
            ELSE
                valor = valor + sal_cuenta.cr[cont] - sal_cuenta.db[cont].
        END.

        FIND FIRST ttSaldos WHERE ttSaldos.agencia = agencias.agencia
                              AND ttSaldos.cuenta = SUBSTRING(sal_cuenta.cuenta,1,1) NO-ERROR.
        IF NOT AVAILABLE ttSaldos THEN DO:
            CREATE ttSaldos.
            ttSaldos.agencia = sal_cuenta.agencia.
            ttSaldos.cuenta = SUBSTRING(sal_cuenta.cuenta,1,1).
        END.

        ttSaldos.saldo = ttSaldos.saldo + valor.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreaAnexo W-Prc_CierreMes 
PROCEDURE CreaAnexo :
FIND FIRST Anexos WHERE Anexos.Agencia = Agencias.Agencia
                    AND Anexos.Cen_Costos = W_CdC
                    AND Anexos.Cuenta = Cuentas.Cuenta
                    AND Anexos.Ano = W_AnoC + 1
                    AND Anexos.Nit = W_Nit NO-ERROR.
IF NOT AVAILABLE(Anexos) THEN DO:
    CREATE Anexos.
    ASSIGN anexos.agencia = Agencias.Agencia
           Anexos.Cen_Costos = W_CdC
           Anexos.Cuenta = Cuentas.Cuenta
           Anexos.Ano = W_AnoC + 1
           Anexos.Nit = W_Nit.
END.

ASSIGN Anexos.Sdo_Inicial = anexos.sdo_inicial + W_Sdo
       W_Cont = W_Cont + 1.

W_Cont:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Cont).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreaSalCta W-Prc_CierreMes 
PROCEDURE CreaSalCta :
FIND FIRST Sal_Cuenta WHERE Sal_Cuenta.Agencia = Agencias.Agencia
                        AND Sal_Cuenta.Cen_Costos = W_CdC
                        AND Sal_Cuenta.Cuenta = Cuentas.Cuenta
                        AND Sal_Cuenta.Ano = W_AnoC + 1 NO-ERROR.
IF NOT AVAILABLE(Sal_Cuenta) THEN DO:
    CREATE Sal_Cuenta.
    ASSIGN Sal_Cuenta.Agencia = Agencias.Agencia
           Sal_Cuenta.Cen_Costos = W_CdC
           Sal_Cuenta.Cuenta = Cuentas.Cuenta
           Sal_Cuenta.Ano = W_AnoC + 1.
END.

ASSIGN Sal_Cuenta.Sal_Inicial = sal_cuenta.sal_inicial + W_Sdo
       W_Cont = W_Cont + 1.

W_Cont:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Cont).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Prc_CierreMes  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Prc_CierreMes)
  THEN DELETE WIDGET W-Prc_CierreMes.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Prc_CierreMes  _DEFAULT-ENABLE
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
  DISPLAY W_CmbOfi W_MesC W_Nmes W_AnoC W_mensaje W_Cont 
      WITH FRAME F_Proc IN WINDOW W-Prc_CierreMes.
  ENABLE RECT-120 RECT-314 BUTTON-5 W_CmbOfi Btn_AbrirMes W_MesC W_AnoC 
         Btn_Cerrar Btn_Done 
      WITH FRAME F_Proc IN WINDOW W-Prc_CierreMes.
  {&OPEN-BROWSERS-IN-QUERY-F_Proc}
  VIEW W-Prc_CierreMes.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

