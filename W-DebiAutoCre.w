&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W_DebiAuto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W_DebiAuto 

/*------------------------------------------------------------------------
  File:        W-DebiAuto.W 
  Description: Débito Automático por Traslado entre Cuentas. Débita la
               cuenta-ahorros matriculada en el pdcto como Age_DebAutomatico 
               Cod_DebAutomatico y Cue_DebAutomatico, con generación 
               de Cuenta-SyA, si las agencias son diferentes.
  Author:      GAER
  Created:     Marzo 30/2004
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* oakley */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

   ON RETURN TAB.

   {Incluido/Variable.I "SHARED"}

   {Incluido/VARCON.I   "SHARED"}

   DEFI VAR W_Age       LIKE Agencias.Agencia.
   DEFI VAR W_Nat       LIKE Cuentas.Naturaleza.
   DEFI VAR W_Ctr       LIKE Cuentas.Ctr_Natur.
   DEFI VAR K           AS INTEG FORM "99".
   DEFI VAR W_Cpte      LIKE Mov_Contable.Comprobante.
   DEFI VAR VrCargos    LIKE Mov_Contable.Db INIT 0 EXTENT 8.
   DEFI VAR VrAbonos    LIKE Mov_Contable.Db INIT 0 EXTENT 8.
   DEFI VAR W_Valor     LIKE Creditos.Cuota INITIAL 0.
   DEFI VAR W_Cta       LIKE Cuentas.Cuenta.
   DEFI VAR W_Oper      LIKE Mov_Creditos.Cod_Operacion.
   DEFI VAR W_Desc      LIKE Mov_Creditos.Descrip.
   DEFI VAR VrDist      LIKE Creditos.Sdo_Capital INITIAL 0.
   DEFI VAR W_AgePto    LIKE Agencias.Agencia.
   DEFI VAR W_AgeDeb    LIKE Agencias.Agencia.
   DEFI VAR W_CodDeb    LIKE Ahorros.Cod_Ahorro.
   DEFI VAR W_CtaDeb    LIKE Ahorros.Cue_Ahorro.
   DEFI VAR W_VrADeb    LIKE Creditos.Cuota INITIAL 0.
   DEFI VAR W_SiInf     AS LOG INIT FALSE.
   DEFI VAR wctaDeb     AS INTEGER INITIAL 0. /* tendra valor 1:Creditos  2:Ahorros */

   /*Para Parámetros Output de AboCreditos.P, donde retorna lo de c/abono*/                                                               
   DEFINE VAR  P_Poliza         LIKE Creditos.Cuota INITIAL 0.                                   
   DEFINE VAR  P_Honora         LIKE Creditos.Cuota INITIAL 0.                                   
   DEFINE VAR  P_Costas         LIKE Creditos.Cuota INITIAL 0.                                   
   DEFINE VAR  P_IMora          LIKE Creditos.Cuota INITIAL 0.                                   
   DEFINE VAR  P_IMorDifC       LIKE Creditos.Cuota INITIAL 0.    
   DEFINE VAR  P_IDifCob        LIKE Creditos.Cuota INITIAL 0.                                   
   DEFINE VAR  P_ICte           LIKE Creditos.Cuota INITIAL 0.                                   
   DEFINE VAR  P_IAntic         LIKE Creditos.Cuota INITIAL 0. /*Si P_IAntic(-) Neg.son cargos*/ 
   DEFINE VAR  P_Capit          LIKE Creditos.Cuota INITIAL 0.                                   
   DEFINE VAR  P_VlrNoDist      LIKE Creditos.Cuota INITIAL 0. /*Valor NO Distribuido*/

   DEFI VAR W_OpAboAho  LIKE Operacion.Cod_Operacion.
   DEFI VAR W_OpCgoAho  LIKE Operacion.Cod_Operacion.

   ASSIGN W_OpAboAho = 010301001      /*Consignaciones para Ahorros*/ 
          W_OpCgoAho = 010302001.      /*Retiros para Ahorros*/ 

   DEFI TEMP-TABLE TempCtas
        FIELD Agen   LIKE Ahorros.Agencia
        FIELD TipP   AS CHAR FORM "X(1)"
        FIELD Pto    LIKE Ahorros.Cod_Ahorro
        FIELD CtaPro LIKE Cuentas.Cuenta
        FIELD CtaIng LIKE Cuentas.Cuenta        /*Para Ahorros Los Por Pagar*/
        FIELD CtaLiq LIKE Cuentas.Cuenta        /*Para Ahorros Los Causados*/
        FIELD IntAnt LIKE Cuentas.Cuenta        /*Para Ahorros Ret-Fuente*/
        FIELD IntMor LIKE Cuentas.Cuenta
        FIELD DifCoD LIKE Cuentas.Cuenta
        FIELD DifCoH LIKE Cuentas.Cuenta
        FIELD CtaPol LIKE Cuentas.Cuenta
        FIELD CtaHon LIKE Cuentas.Cuenta
        FIELD CtaCos LIKE Cuentas.Cuenta
        FIELD CtaGar LIKE CortoLargo.Cta_VigGarAd
        FIELD CtaCGa LIKE CortoLargo.Cta_ContrapartidaGar
        FIELD Oper   LIKE Liqui_Int.Cod_Operacion
        FIELD CtaSyA LIKE Cuentas.Cuenta.

   DEFI TEMP-TABLE CopMov_Contable LIKE Mov_Contable.

   DEFI TEMP-TABLE TInf
        FIELD AgeDeb LIKE Ahorros.Agencia
        FIELD PtoDeb LIKE Ahorros.Cod_Ahorro
        FIELD NitDeb LIKE Ahorros.Nit
        FIELD CtaDeb LIKE Ahorros.Cue_Ahorro
        FIELD TipAbo AS CHAR FORM "X(1)"
        FIELD AgeAbo LIKE Ahorros.Agencia
        FIELD PtoAbo LIKE Ahorros.Cod_Ahorro
        FIELD NitAbo LIKE Ahorros.Nit
        FIELD CtaAbo LIKE Ahorros.Cue_Ahorro
        FIELD VrADeb LIKE Creditos.Cuota INITIAL 0
        FIELD VrCuot LIKE Creditos.Cuota INITIAL 0
        FIELD VrDeb  LIKE Creditos.Cuota INITIAL 0
        FIELD FecPxD LIKE Ahorros.Fec_ProxDeb
              INDEX Ppal AgeAbo NitAbo TipAbo PtoAbo CtaAbo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Proc

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-314 RECT-319 W_CmbOfi Tg_Proc W_NitCte ~
BUTTON-5 Btn_Contabilizar Btn_Done 
&Scoped-Define DISPLAYED-OBJECTS W_CmbOfi Tg_Proc W_NitCte W_NomCte W_DiaC ~
W_MesC W_Nmes W_AnoC W_mensaje W_Cont 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 W_MesC W_AnoC 
&Scoped-define List-2 W_mensaje 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W_DebiAuto AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Contabilizar 
     LABEL "&Contabilizar" 
     SIZE 11.14 BY 1.58 TOOLTIP "Carga/Abona productos y Contabiliza".

DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Salir" 
     SIZE 11.14 BY 1.46
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 5" 
     SIZE 11.14 BY 1.73.

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(50)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     DROP-DOWN-LIST
     SIZE 36.14 BY 1 TOOLTIP "Agencias Disponibles"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_AnoC AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Año" 
     VIEW-AS FILL-IN 
     SIZE 5.57 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Cont AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.72 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_DiaC AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Dia" 
     VIEW-AS FILL-IN 
     SIZE 3.29 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_mensaje AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43.29 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_MesC AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Mes" 
     VIEW-AS FILL-IN 
     SIZE 3.72 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NitCte AS CHARACTER FORMAT "X(12)":U 
     LABEL "Ced/Nit" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .85 TOOLTIP "Ced/Nit del Cliente"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_Nmes AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 15.72 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCte AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 50.14 BY .85 TOOLTIP "Nombre del Cliente"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-314
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 14.43 BY 6.38.

DEFINE RECTANGLE RECT-319
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 57 BY 6.38.

DEFINE VARIABLE Tg_Proc AS LOGICAL INITIAL no 
     LABEL "Débito para toda la Agencia" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .88 TOOLTIP "Marque si el Débito se procesa para toda la Agencia"
     BGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Proc
     W_CmbOfi AT ROW 1.31 COL 8.86 COLON-ALIGNED
     Tg_Proc AT ROW 1.35 COL 47
     W_NitCte AT ROW 2.54 COL 8.86 COLON-ALIGNED
     W_NomCte AT ROW 2.54 COL 21.86 COLON-ALIGNED NO-LABEL
     BUTTON-5 AT ROW 4.27 COL 61.29
     W_DiaC AT ROW 5.88 COL 8.86 COLON-ALIGNED
     W_MesC AT ROW 5.88 COL 18 COLON-ALIGNED
     W_Nmes AT ROW 5.88 COL 22.14 COLON-ALIGNED NO-LABEL
     W_AnoC AT ROW 5.88 COL 44.86 COLON-ALIGNED
     Btn_Contabilizar AT ROW 6.35 COL 61.29 HELP
          "Realiza la Contabilización de los Traslados por Deb/Automático"
     Btn_Done AT ROW 8.23 COL 61.29 HELP
          "Sale del proceso de Depreciación y Ajustes"
     W_mensaje AT ROW 9.19 COL 1.86 COLON-ALIGNED NO-LABEL
     W_Cont AT ROW 9.19 COL 45.29 COLON-ALIGNED NO-LABEL
     "RegistrosTotales" VIEW-AS TEXT
          SIZE 15.29 BY .5 AT ROW 8.65 COL 42.57
          BGCOLOR 18 FGCOLOR 15 
     RECT-314 AT ROW 3.73 COL 59.57
     RECT-319 AT ROW 3.73 COL 2.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 76.43 BY 10.23
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
  CREATE WINDOW W_DebiAuto ASSIGN
         HIDDEN             = YES
         TITLE              = "Débito Automático, Programa W-DebiAuto.W"
         HEIGHT             = 10.23
         WIDTH              = 76.72
         MAX-HEIGHT         = 20.77
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 20.77
         VIRTUAL-WIDTH      = 114.29
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
/* SETTINGS FOR WINDOW W_DebiAuto
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Proc
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN W_AnoC IN FRAME F_Proc
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN W_Cont IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_DiaC IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_mensaje IN FRAME F_Proc
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN W_MesC IN FRAME F_Proc
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN W_Nmes IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomCte IN FRAME F_Proc
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W_DebiAuto)
THEN W_DebiAuto:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Proc
/* Query rebuild information for FRAME F_Proc
     _Query            is NOT OPENED
*/  /* FRAME F_Proc */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W_DebiAuto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_DebiAuto W_DebiAuto
ON END-ERROR OF W_DebiAuto /* Débito Automático, Programa W-DebiAuto.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_DebiAuto W_DebiAuto
ON WINDOW-CLOSE OF W_DebiAuto /* Débito Automático, Programa W-DebiAuto.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Contabilizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Contabilizar W_DebiAuto
ON CHOOSE OF Btn_Contabilizar IN FRAME F_Proc /* Contabilizar */
DO:
    FOR EACH TempCtas:        DELETE TempCtas.        END.
    FOR EACH CopMov_Contable: DELETE CopMov_Contable. END.
    FOR EACH TInf:            DELETE TInf.            END.

    RUN Valida NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
       MESSAGE "La Configuración Contable Presento Errores...Proceso cancelado."
              VIEW-AS ALERT-BOX ERROR.

       RETURN.
    END.

    FIND FIRST Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
    IF AVAIL(Entidad) THEN
       FIND FIRST Comprobantes WHERE Comprobantes.Agencia EQ W_Agencia
                                 AND Comprobantes.Comprob EQ 12
                                 AND Comprobantes.Estado  EQ 1 NO-ERROR.

    IF NOT AVAIL(Comprobantes) OR NOT AVAIL(Entidad) THEN DO:
       MESSAGE "El Comprobante-Fuente Contable para el proceso debe existir en Entidad y en Comprobantes" SKIP
             "                        No se acepta la Operación." VIEW-AS ALERT-BOX ERROR.
        
       ASSIGN W_Mensaje:SCREEN-VALUE = "Proceso Contable Cancelado...Revise por favor".
       RETURN.
    END.
        
    IF NOT Tg_Proc THEN DO:
       APPLY "LEAVE" TO W_NitCte. 
        
       IF NOT AVAIL(Clientes) THEN DO:
          FIND CURRENT Comprobantes NO-LOCK NO-ERROR. 
       
          RETURN.
       END.
    END.
        
    SESSION:SET-WAIT-STATE("GENERAL").

    RUN Contabilizar NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
       FIND CURRENT Comprobantes NO-LOCK NO-ERROR.

       MESSAGE "La Contabilización Presentó Errores...Proceso cancelado."
              VIEW-AS ALERT-BOX ERROR.

       SESSION:SET-WAIT-STATE("").

       ASSIGN W_Mensaje:SCREEN-VALUE = "Proceso Contable Cancelado...Revise por favor".

       RETURN.
    END.
    ELSE DO:
       DEFI VAR Listado AS CHAR FORM "X(40)".                                                                   
                                                                                                                
       ASSIGN Listado = W_PathSpl + "DebAutCont-" + STRING(Comprobantes.Secuencia)  + ".Lst"
              W_Mensaje:SCREEN-VALUE = "Proceso Contable Débito-Automático Exitoso..."
              W_SiInf = TRUE.
                                                                                                                
       {Incluido\Imprimir.I "listado"} 

       W_SiInf = FALSE.
                                                                                                                
       SESSION:SET-WAIT-STATE("").

       FOR EACH CopMov_Contable: DELETE CopMov_Contable. END.
       FOR EACH TempCtas:        DELETE TempCtas.        END.
       FOR EACH TInf:            DELETE TInf.            END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W_DebiAuto
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W_DebiAuto
ON CHOOSE OF BUTTON-5 IN FRAME F_Proc /* Button 5 */
DO:
  RUN W-InfDia.R.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tg_Proc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tg_Proc W_DebiAuto
ON VALUE-CHANGED OF Tg_Proc IN FRAME F_Proc /* Débito para toda la Agencia */
DO:
  ASSIGN Tg_Proc
         W_NitCte:SENSITIVE = TRUE.

  IF Tg_Proc THEN
     ASSIGN W_NitCte:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbOfi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfi W_DebiAuto
ON VALUE-CHANGED OF W_CmbOfi IN FRAME F_Proc /* Agencia */
DO:
    IF INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)) EQ 0 THEN DO:
       FIND FIRST Agencias WHERE Agencias.Agencia GT 0
                             AND Agencias.Estado  EQ 1 NO-LOCK NO-ERROR.
       IF AVAILABLE Agencias THEN 
          ASSIGN W_OfiIni = Agencias.Agencia.

       FIND LAST Agencias WHERE Agencias.Agencia GT 0
                            AND Agencias.Estado  EQ 1 NO-LOCK NO-ERROR.
       IF AVAILABLE Agencias THEN 
          ASSIGN W_OfiFin = Agencias.Agencia.
    END.
    ELSE ASSIGN W_OfiIni = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)) 
                W_OfiFin = INTEGER(SUBSTRING(W_CmbOfi:SCREEN-VALUE,1,3)).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NitCte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitCte W_DebiAuto
ON LEAVE OF W_NitCte IN FRAME F_Proc /* Ced/Nit */
DO:
   ASSIGN W_NitCte
          W_NomCte:SCREEN-VALUE = "".

   FIND FIRST Clientes WHERE Clientes.Nit         EQ W_NitCte 
                         AND Clientes.Estado      EQ 1 NO-LOCK NO-ERROR.
   IF NOT AVAIL(Clientes) THEN DO:                                                                    
      ASSIGN FRAME F_Proc:SENSITIVE = FALSE.                                                           
                                                                                                    
      RUN C-Clientes.R (INPUT  1,W_Agencia,                                                           
                        OUTPUT W_NitCte, OUTPUT W_NomCte, OUTPUT W_NomCte, OUTPUT W_Age).       
                                                                                                    
      ASSIGN FRAME F_Proc:SENSITIVE = TRUE.                                                            
                                                                                                    
      FIND FIRST Clientes WHERE Clientes.Nit         EQ W_NitCte
                            AND Clientes.Estado      EQ 1 NO-LOCK NO-ERROR.                            
      IF AVAIL(Clientes) THEN                                                                         
         ASSIGN W_NitCte:SCREEN-VALUE = W_NitCte                                                      
                W_NomCte:SCREEN-VALUE = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) +   
                                        " " + TRIM(Clientes.Nombre).                                  
      ELSE                                                                                            
         MESSAGE "El Nit debe existir Activo en Clientes." VIEW-AS ALERT-BOX.                                 
   END.                                                                                               
   ELSE ASSIGN W_NomCte:SCREEN-VALUE = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) +    
                                       " " + TRIM(Clientes.Nombre).                                                                                                            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W_DebiAuto 


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

  FIND FIRST Agencias WHERE Agencias.Agencia EQ W_Agencia
                        AND Agencias.Estado  EQ 1  NO-LOCK NO-ERROR.
  IF NOT AVAIL(Agencias) THEN DO:
     MESSAGE "La Agencia no está disponible para realizar Transacciones..." VIEW-AS ALERT-BOX ERROR.

     APPLY "CLOSE":U TO THIS-PROCEDURE.
     RETURN NO-APPLY.
  END.  
  
  ASSIGN W_OfiIni            = W_Agencia
         W_OfiFin            = W_Agencia
         W_MesC:SCREEN-VALUE = STRING(MONTH(W_Fecha))
         W_DiaC:SCREEN-VALUE = STRING(DAY(W_Fecha))
         W_AnoC:SCREEN-VALUE = STRING(YEAR(W_Fecha)).
                  
  W_CmbOfi:ADD-LAST("000 CONSOLIDADO").

  FOR EACH Agencias WHERE Agencias.Estado  EQ 1
                      AND Agencias.Agencia GT 0 NO-LOCK:
      W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"999") + "-" + STRING(Agencias.Nombre,"X(25)")).

      IF Agencias.Agencia EQ W_Agencia THEN
         W_CmbOfi:SCREEN-VALUE = (STRING(Agencias.Agencia,"999") + "-" + STRING(Agencias.Nombre,"X(40)")).
  END. 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CompletaSyA W_DebiAuto 
PROCEDURE CompletaSyA :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
 FIND FIRST CopMov_Contable WHERE CopMov_Contable.Agencia EQ W_AgeDeb
                              AND CopMov_Contable.Cuenta  EQ TempCtas.CtaSyA
                              AND CopMov_Contable.Nit     EQ STRING(W_AgePto,"999") 
                              AND CopMov_Contable.Cr      GT 0   NO-ERROR.
 IF NOT AVAIL(CopMov_Contable) THEN                                                  
    CREATE CopMov_Contable.                                                          
                                                                                     
 ASSIGN CopMov_Contable.Agencia        = W_AgeDeb                               
        CopMov_Contable.Cuenta         = TempCtas.CtaSyA                             
        CopMov_Contable.Nit            = STRING(W_AgePto,"999")                        
        CopMov_Contable.Fec_Contable   = W_Fecha                                     
        CopMov_Contable.Comentario     = "Débito-Automático"                           
        CopMov_Contable.Usuario        = W_Usuario                                   
        CopMov_Contable.Cen_Costos     = W_Cencosgral                                
        CopMov_Contable.Destino        = W_Agencia                                   
        CopMov_Contable.Comprobante    = Comprobantes.Comprobante                    
        CopMov_Contable.Num_Documento  = Comprobantes.Secuencia                      
        CopMov_Contable.Fec_Grabacion  = TODAY                                       
        CopMov_Contable.Hora           = TIME                                        
        CopMov_Contable.Estacion       = W_Estacion                                  
        CopMov_Contable.Cr             = CopMov_Contable.Cr + W_VrADeb.         
                                                                                 
 FIND FIRST CopMov_Contable WHERE CopMov_Contable.Agencia EQ W_AgePto               
                              AND CopMov_Contable.Cuenta  EQ TempCtas.CtaSyA         
                              AND CopMov_Contable.Db      GT 0 NO-ERROR.             
 IF NOT AVAIL(CopMov_Contable) THEN                                                  
    CREATE CopMov_Contable.                                                          
                                                                                     
 ASSIGN CopMov_Contable.Agencia        = W_AgePto                                   
        CopMov_Contable.Cuenta         = TempCtas.CtaSyA                             
        CopMov_Contable.Nit            = STRING(W_AgeDeb,"999")                        
        CopMov_Contable.Fec_Contable   = W_Fecha                                     
        CopMov_Contable.Comentario     = "Débito-Automático"                           
        CopMov_Contable.Usuario        = W_Usuario                                   
        CopMov_Contable.Cen_Costos     = W_Cencosgral                                
        CopMov_Contable.Destino        = W_AgeDeb                                    
        CopMov_Contable.Comprobante    = Comprobantes.Comprobante                    
        CopMov_Contable.Num_Documento  = Comprobantes.Secuencia                      
        CopMov_Contable.Fec_Grabacion  = TODAY                                       
        CopMov_Contable.Hora           = TIME                                        
        CopMov_Contable.Estacion       = W_Estacion                                  
        CopMov_Contable.Db             = CopMov_Contable.Db + W_VrADeb.        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar W_DebiAuto 
PROCEDURE Contabilizar :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  DEFI VAR TotT    LIKE Mov_Contable.Db INIT 0.
  DEFI VAR Listado AS CHAR FORM "X(40)".
  DEFI VAR W_SiErr AS LOG INIT FALSE.
  
  OUTPUT TO c:\INFO_cooprudea\DebitosCreditos_AUTO28-11-2008.csv.
  PUT "creditos.pagare;creditos.fec_pago;W_VrADeb" SKIP(0).

  ASSIGN W_Cont                              = 0
         W_Cont:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Cont).

  DO TRANS ON ERROR UNDO:
     ASSIGN Comprobantes.Secuencia = Comprobantes.Secuencia + 1.
     FIND CURRENT Comprobantes NO-LOCK NO-ERROR.   
        
     IF Tg_Proc THEN FOR EACH Agencias WHERE Agencias.Estado  NE 3 
                                         AND Agencias.Agencia GE W_OfiIni 
                                         AND Agencias.Agencia LE W_OfiFin NO-LOCK:  

         
        FOR EACH Creditos WHERE Creditos.Agencia         EQ Agencias.Agencia 
                          AND Creditos.FOR_pago          EQ 3:
           IF Creditos.Age_DebAutomatico GT 0   AND Creditos.Cod_DebAutomatico GT 0 AND
              Creditos.Cue_DebAutomatico GT " " AND Creditos.Estado            EQ 2 AND
              Creditos.Cuota             GT 0  /* AND Creditos.Val_Atraso        GT 0 */ THEN DO:
               ASSIGN wctaDeb = 1.
               RUN DebitaCreditos NO-ERROR.
               IF ERROR-STATUS:ERROR THEN     
                  RETURN ERROR.             
           END.
        END.

        /*
        FOR EACH Ahorros WHERE Ahorros.Agencia   EQ Ahorros.Agencia  AND tip_ahorro = 2 AND Ahorros.Fec_ProxDeb LE W_Fecha
                          AND Ahorros.FOR_pago   EQ 3:
           IF Ahorros.Age_DebAutomatico GT 0   AND Ahorros.Cod_DebAutomatico GT 0 AND
              Ahorros.Cue_DebAutomatico GT " " AND Ahorros.Estado            EQ 1 AND
              Ahorros.Cuota             GT 0  THEN DO:
               ASSIGN wctaDeb = 2.
               RUN DebitaAhorros NO-ERROR.
               IF ERROR-STATUS:ERROR THEN     
                  RETURN ERROR.             
           END.
        END.
        */
     END.
     
     ELSE DO:
         
        FOR EACH Creditos WHERE Creditos.Nit             EQ W_NitCte AND
                                Creditos.FOR_Pago        EQ 3:
           IF Creditos.Age_DebAutomatico GT 0   AND Creditos.Cod_DebAutomatico GT 0 AND
              Creditos.Cue_DebAutomatico GT " " AND Creditos.Estado            EQ 2 AND
              Creditos.Cuota             GT 0  /* AND Creditos.Val_Atraso        GT 0 */ THEN DO:
               ASSIGN wctaDeb = 1.
               RUN DebitaCreditos NO-ERROR.
               IF ERROR-STATUS:ERROR THEN     
                  RETURN ERROR. 
           END.
        END.
        /*
        FOR EACH Ahorros WHERE Ahorros.Nit       EQ W_NitCte AND tip_ahorro = 2
                          AND Ahorros.FOR_pago   EQ 3:
           IF Ahorros.Age_DebAutomatico GT 0   AND Ahorros.Cod_DebAutomatico GT 0 AND
              Ahorros.Cue_DebAutomatico GT " " AND Ahorros.Estado            EQ 1 AND
              Ahorros.Cuota             GT 0  THEN DO:
              ASSIGN wctaDeb = 2.
              RUN DebitaAhorros NO-ERROR.
              IF ERROR-STATUS:ERROR THEN     
                 RETURN ERROR.             
           END.
        END. */
     END.

     FOR EACH CopMov_Contable:
         CREATE Mov_Contable.
         BUFFER-COPY CopMov_Contable TO Mov_Contable.

         DELETE CopMov_Contable.
     END.

     FOR EACH Mov_Contable WHERE  Mov_Contable.Comprobante  EQ Comprobantes.Comprobante          
                             AND Mov_Contable.Num_Documento EQ Comprobantes.Secuencia            
                             AND Mov_Contable.Fec_Contable  EQ W_Fecha NO-LOCK                   
                             BREAK BY Mov_Contable.Agencia:                                      
          ASSIGN TotT = TotT + (Mov_Contable.Db - Mov_Contable.Cr).                               
                                                                                                  
          IF LAST-OF(Mov_Contable.Agencia) AND TotT NE 0 THEN DO:                                 
              MESSAGE "Los DEBE - HABER en la Agencia : " Mov_Contable.Agencia SKIP               
                      "                 Están Diferentes...Revise por favor."                     
                       VIEW-AS ALERT-BOX ERROR.                                                   
             ASSIGN W_SiErr = TRUE.                                                                        
          END.                                                                                    
     END.
     
     FIND FIRST Mov_Contable WHERE  Mov_Contable.Comprobante   EQ Comprobantes.Comprobante          
                                AND Mov_Contable.Num_Documento EQ Comprobantes.Secuencia            
                                AND Mov_Contable.Fec_Contable  EQ W_Fecha NO-LOCK NO-ERROR.
     IF NOT AVAIL(Mov_Contable) THEN DO:
        MESSAGE "No hay Registros Para Aplicar Débito-Autómatico, o no hay Saldos_Disponibles."
                VIEW-AS ALERT-BOX TITLE "INFORMATIVO".
        ASSIGN W_SiErr = TRUE.
     END.

     ASSIGN Listado = W_PathSpl + "DebAutCont-" + STRING(Comprobantes.Secuencia)  + ".Lst".                            
                                                                                                                
     {Incluido\Imprimir.I "listado"}

     IF W_SiErr THEN DO:
        RUN ImpDescuadre.

        RETURN ERROR.
     END.
  END.  /*Fin Tx*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DebitaAhorros W_DebiAuto 
PROCEDURE DebitaAhorros :
/*-------------------------------------------------------------------------------------
  Purpose:  Halla la cuenta a Debitar y la debita, y Abona el valor débitado en ahorros.   
--------------------------------------------------------------------------------------*/
  DEFI VAR W_RowIdA  AS ROWID.
  DEFI VAR DiasPer   AS INTEG FORM "99" INIT 30.
  DEFI VAR W_ProxDeb AS DATE.
  DEFI VAR W_VrXDeb  LIKE Ahorros.Cuota INIT 0.
  DEFI VAR Pdo       AS INTEG FORM "99".
  DEFI VAR Vigen     AS INTEG FORM "999999".
  DEFI VAR T_gracia  AS INTEG FORM "999999".

  IF      Ahorros.Per_Deduc EQ 1 THEN
     DiasPer = 7.
  ELSE IF Ahorros.Per_Deduc EQ 2 THEN
     DiasPer = 10.
  ELSE IF Ahorros.Per_Deduc EQ 3 THEN
     DiasPer = 15.

  ASSIGN W_AgeDeb  = Ahorros.Age_DebAutomatico  
         W_CodDeb  = Ahorros.Cod_DebAutomatico            
         W_CtaDeb  = Ahorros.Cue_DebAutomatico            
         W_AgePto  = Ahorros.Agencia                   
         W_VrADeb  = Ahorros.Cuota
         W_RowIdA  = ROWID(Ahorros)
         W_ProxDeb = Ahorros.Fec_ProxDeb + DiasPer.

  IF Ahorros.Val_DebAutPdo LT Ahorros.Cuota THEN
     W_VrADeb = Ahorros.Cuota - Ahorros.Val_DebAutPdo.

  IF Ahorros.Fec_ProxDeb + DiasPer LE W_Fecha THEN  /* HALLA CUOTAS ATRASADAS */
  DO K = 1 TO 52:
     ASSIGN W_ProxDeb = W_ProxDeb + DiasPer
            W_VrADeb  = W_VrADeb  + Ahorros.Cuota.

     IF W_ProxDeb GT W_Fecha THEN
        LEAVE.
  END.

  RUN HallaCtaDeb NO-ERROR.                              
  IF ERROR-STATUS:ERROR THEN                             
     RETURN ERROR. 

  FIND Ahorros WHERE ROWID(Ahorros) EQ W_RowidA NO-ERROR.

  IF W_VrADeb GT 0 THEN DO:
     FIND FIRST TempCtas WHERE TempCtas.Agen EQ Ahorros.Agencia                                     
                           AND TempCtas.TipP EQ "A"                                                 
                           AND TempCtas.Pto  EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.                
     IF NOT AVAIL(TempCtas) THEN DO:                                                                
        MESSAGE "Falta Configuración con Producto_Ahorro: " Ahorros.Cod_Ahorro SKIP                 
                "Para la Agencia : " Ahorros.Agencia                                                
               VIEW-AS ALERT-BOX ERROR.                                                            
        RETURN ERROR.                                                                               
     END.
     
     FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
     
     IF Pro_Ahorros.Id_PerGracia EQ TRUE AND                          
     Pro_Ahorros.Bas_Calculo     EQ 1    AND                             
     Pro_Ahorros.Dia_Gracia      GT 0    THEN DO:                        
          ASSIGN Pdo = 1.                                                  
          IF Ahorros.Per_Liquid      EQ 2 THEN                             
             Pdo = 30.                                                     
          ELSE IF Ahorros.Per_Liquid EQ 3 THEN                             
             Pdo = 90.                                                     
          ELSE IF Ahorros.Per_Liquid EQ 4 THEN                             
             Pdo = 180.                                                    
          ELSE IF Ahorros.Per_Liquid EQ 5 THEN                             
             Pdo = 360.                                                    
                                                                           
          ASSIGN Vigen    = Ahorros.Fec_ProLiquidacion - W_Fecha           
                 T_Gracia = Pdo - Vigen.                                   
                                                                           
          IF T_Gracia LE Pro_Ahorros.Dia_Gracia THEN                       
             ASSIGN Ahorros.Sdo_Minimo = Ahorros.Sdo_Minimo + W_VrADeb. 
     END.

     ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + W_VrADeb
            Ahorros.Fec_UltDeb     = W_Fecha
            Ahorros.Fec_UltTrans   = W_Fecha
            Ahorros.Num_DepMes     = Ahorros.Num_DepMes + 1                 
            Ahorros.Val_DepMes     = Ahorros.Val_DepMes + W_VrADeb
            Ahorros.Val_DepDia     = Ahorros.Val_DepDia + W_VrADeb
            Ahorros.Num_DepDia     = Ahorros.Num_DepDia + 1
            W_Cta                  = TempCtas.CtaPro                  
            W_Oper                 = W_OpAboAho
            W_VrXDeb               = W_VrADeb + Ahorros.Val_DebAutPdo.
            
     IF Ahorros.Detalle_Estado EQ 1 THEN DO: /*APERTURA DE LA CUENTA:Primera Consignacion*/                                   
        ASSIGN Ahorros.Detalle_Estado = 2                                                                                     
               Ahorros.Sdo_Inicial    = W_VrADeb.                                                                              
        IF Ahorros.Tip_Ahorro GT 1 AND Ahorros.Tip_Ahorro LT 4 THEN DO: /*asigna fecvencimiento para atermino y contractual*/ 
           ASSIGN Ahorros.Fec_Vencimiento = W_Fecha + Ahorros.Plazo.                                                          
           CASE Ahorros.Per_Liquidacion:                                                                                      
             WHEN 1 THEN Ahorros.Fec_ProLiquidacion = W_Fecha + 1.                                                            
             WHEN 2 THEN Ahorros.Fec_ProLiquidacion = W_Fecha + 29.                                                           
             WHEN 3 THEN Ahorros.Fec_ProLiquidacion = W_Fecha + 90.                                                           
             WHEN 4 THEN Ahorros.Fec_ProLiquidacion = W_Fecha + 181.                                                          
             WHEN 5 THEN Ahorros.Fec_ProLiquidacion = W_Fecha + 364.                                                          
             WHEN 6 THEN Ahorros.Fec_ProLiquidacion = W_Fecha + Ahorros.Plazo.                                                
           END CASE.                                                                                                          
        END.                                                                                                                          
     END.                                                                                                                                       

     IF (Ahorros.Val_DebAutPdo + W_VrADeb) GE Ahorros.Cuota THEN DO K = 1 TO 52:
        ASSIGN Ahorros.Fec_ProxDeb   = Ahorros.Fec_ProxDeb + DiasPer
               W_VrXDeb              = W_VrXDeb - Ahorros.Cuota.

        IF W_VrXDeb LE 0 OR W_VrXDeb LT Ahorros.Cuota THEN DO:
           Ahorros.Val_DebAutPdo = W_VrXDeb.

           LEAVE.
        END.
     END.
     ELSE 
        ASSIGN Ahorros.Val_DebAutPdo = Ahorros.Val_DebAutPdo + W_VrADeb.

     RUN MovAhorros.   /*Mov_Contable y Mov_Ahorros*/                 
     ASSIGN Mov_Contable.Cr = W_VrADeb
            Mov_Contable.Db = 0.

     IF W_AgeDeb NE Ahorros.Agencia THEN 
        RUN CompletaSyA.
  END.

  ASSIGN TInf.TipAbo = "A"
         TInf.AgeAbo = Ahorros.Agencia
         TInf.PtoAbo = Ahorros.Cod_Ahorro
         TInf.NitAbo = Ahorros.Nit
         TInf.CtaAbo = Ahorros.Cue_Ahorro
         TInf.VrCuot = Ahorros.Cuota
         TInf.FecPxD = Ahorros.Fec_ProxDeb.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DebitaCreditos W_DebiAuto 
PROCEDURE DebitaCreditos :
/*--------------------------------------------------------------------------------------
  Purpose: Halla la cuenta a Debitar y la debita, y Abona el valor débitado en Créditos.    
--------------------------------------------------------------------------------------*/
  DEFI VAR W_VrSdo  LIKE Creditos.Sdo_capital INIT 0.

  ASSIGN W_AgeDeb = Creditos.Age_DebAutomatico 
         W_AgePto = Creditos.Agencia
         W_CodDeb = Creditos.Cod_DebAutomatico                                                      
         W_CtaDeb = Creditos.Cue_DebAutomatico                                                               
         /*
         W_VrADeb = Creditos.cuota + Creditos.Val_Atraso     + Creditos.Int_Corrientes +                             
                    Creditos.Int_MorCobrar  + Creditos.Int_DifCobro   + Creditos.Int_MoraDifCob +
                    Creditos.Costas         + Creditos.Polizas        + Creditos.Honorarios     - Creditos.Int_Anticipado         
         */
         W_VrSdo  = Creditos.Sdo_capital    + Creditos.Int_Corrientes +                             
                    Creditos.Int_MorCobrar  + Creditos.Int_DifCobro   + Creditos.Int_MoraDifCob +
                    Creditos.Costas         + Creditos.Polizas        +                             
                    Creditos.Honorarios     - Creditos.Int_Anticipado. 

         ASSIGN W_VrADeb = Creditos.cuota.


  FIND FIRST PlanPagos WHERE PlanPagos.Nit            EQ Creditos.Nit
                            AND PlanPagos.Cod_Credito EQ Creditos.Cod_Credito
                            AND PlanPagos.Num_Credito EQ Creditos.Num_Credito
                            AND PlanPagos.Id_PdoMes   EQ 1 NO-LOCK NO-ERROR.
/*   IF AVAIL(Planpagos) THEN DO:                                                             */
/*      IF PlanPagos.Nro_Cuota GT Creditos.Plazo THEN                                         */
/*         W_VrADeb = W_VrSdo.                                                                */
/*      ELSE IF PlanPagos.INT_LiqPdo GT 0 THEN DO:                                            */
/*         IF (Creditos.Int_Corrientes + Creditos.INT_DifCobro) GT PlanPagos.INT_LiqPdo THEN  */
/*             W_VrADeb = W_VrADeb - PlanPagos.INT_LiqPdo.                                    */
/*         ELSE                                                                               */
/*             W_VrADeb = W_VrADeb - (Creditos.Int_Corrientes + Creditos.INT_DifCobro).       */
/*      END.                                                                                  */
/*   END.                                                                                     */



  IF W_VrSdo LE W_VrADeb THEN                                                                       
     ASSIGN W_VrADeb = W_VrSdo.                                                                     
  ELSE IF W_VrADeb LE 0 THEN     /*No debe darse...? Está vencido*/
     ASSIGN W_VrADeb = 0.



  /*IF W_VrADeb LT Creditos.Cuota AND W_VrADeb LT W_VrSdo THEN DO:
     RUN AboCredito.R         /*Distribuye abonos en Créditos,sin actualizar*/
            (INPUT NO,
             INPUT Creditos.Agencia,Creditos.Cod_Credito,Creditos.Nit,
             INPUT Creditos.Num_Credito,Creditos.Cuota,
             INPUT 0,0,0,1,
             OUTPUT P_Poliza,  OUTPUT P_Honora, OUTPUT P_Costas, OUTPUT P_IMorDifC, OUTPUT P_IMora,
             OUTPUT P_IDifCob, OUTPUT P_ICte,   OUTPUT P_IAntic, OUTPUT P_Capit,
             OUTPUT P_VlrNoDist). 

     IF (Creditos.Sdo_Capital - P_Capit) GT Creditos.Sdo_Proyect OR (Creditos.Int_Corrientes - P_ICte) GT PlanPagos.INT_LiqPdo THEN
        W_VrADeb = Creditos.Cuota.
  END. No necesario porque DebPagar de K. lo calcula con base en el Proyectado, Ya está en el Vdo*/
                                                                                                                                                                                       
  IF W_VrADeb GT 0 THEN DO:                                                                         
     RUN HallaCtaDeb NO-ERROR.                                                                      
     IF ERROR-STATUS:ERROR THEN                                                                     
        RETURN ERROR.                                                                               
                                                                                       
     IF W_VrADeb GT 0 THEN DO:                                                                      
        RUN AboCredito.R         /*Distribuye abonos en Créditos,graba Mov_creditos,Mov_Contab y PlanPagos*/
            (INPUT TRUE,
             INPUT Creditos.Agencia,Creditos.Cod_Credito,Creditos.Nit,
             INPUT Creditos.Num_Credito,W_VrADeb,
             INPUT Comprobantes.Comprobante,Comprobantes.Secuencia,0,1,
             OUTPUT P_Poliza,  OUTPUT P_Honora, OUTPUT P_Costas, OUTPUT P_IMorDifC, OUTPUT P_IMora,
             OUTPUT P_IDifCob, OUTPUT P_ICte,   OUTPUT P_IAntic, OUTPUT P_Capit,
             OUTPUT P_VlrNoDist) NO-ERROR.

/**************************************************************************************************************** PEDRO Y WILLIAM ******/
      PUT creditos.pagare ";" creditos.fec_pago ";" W_VrADeb SKIP(0).

        IF ERROR-STATUS:ERROR OR P_VlrNoDist GT 0 OR P_VlrNoDist LT 0 THEN DO:
           MESSAGE "El Programa AboCreditos.P...Retornó valor no distribuido :$" P_VlrNoDist SKIP
                "O Retornò ERROR..." SKIP
                "para el Nit. : " Creditos.Nit ", Cod_producto : " Creditos.Cod_Credito ", Nro-Crédito : " Creditos.Num_Credito SKIP
                "                        Revise por favor...Distribución cancelada."
                VIEW-AS ALERT-BOX ERROR.
           RETURN ERROR.
        END.          

        
        FIND FIRST TempCtas WHERE TempCtas.Agen EQ Creditos.Agencia                      
                        AND TempCtas.TipP EQ "C"                                       
                        AND TempCtas.Pto  EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.    
        IF NOT AVAIL(TempCtas) THEN DO:
           MESSAGE "Falta Configuración con Producto_Credito: " Creditos.Cod_Credito SKIP
                   "Para la Agencia : " Creditos.Agencia
                VIEW-AS ALERT-BOX ERROR.
           RETURN ERROR.
        END.                                                       

        IF W_AgeDeb NE Creditos.Agencia THEN 
           RUN CompletaSyA.
     END.

     ASSIGN TInf.TipAbo = "C"
            TInf.AgeAbo = Creditos.Agencia     
            TInf.PtoAbo = Creditos.Cod_Credito
            TInf.NitAbo = Creditos.Nit         
            TInf.CtaAbo = STRING(Creditos.Num_Credito)  
            TInf.VrCuot = Creditos.Cuota       
            TInf.FecPxD = Creditos.Fec_Pago.

  END.                                                                                              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W_DebiAuto  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W_DebiAuto)
  THEN DELETE WIDGET W_DebiAuto.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W_DebiAuto  _DEFAULT-ENABLE
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
  DISPLAY W_CmbOfi Tg_Proc W_NitCte W_NomCte W_DiaC W_MesC W_Nmes W_AnoC 
          W_mensaje W_Cont 
      WITH FRAME F_Proc IN WINDOW W_DebiAuto.
  ENABLE RECT-314 RECT-319 W_CmbOfi Tg_Proc W_NitCte BUTTON-5 Btn_Contabilizar 
         Btn_Done 
      WITH FRAME F_Proc IN WINDOW W_DebiAuto.
  {&OPEN-BROWSERS-IN-QUERY-F_Proc}
  VIEW W_DebiAuto.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallaCtaDeb W_DebiAuto 
PROCEDURE HallaCtaDeb :
/*-----------------------------------------------------------------------------------
  Purpose:     Halla la cuenta para el Débito y la Débita, crea temp.para el informe
 -----------------------------------------------------------------------------------*/
 
  DEFINE VAR P_ImpAplic  LIKE Creditos.Cuota INITIAL 0.
 
  
  CREATE TInf.
  IF wctaDeb = 1 THEN /* Para Creditos */
     ASSIGN TInf.AgeDeb = Creditos.Age_DebAutomatico
            TInf.PtoDeb = Creditos.Cod_DebAutomatico
            TInf.CtaDeb = Creditos.Cue_DebAutomatico
            TInf.VrADeb = W_VrADeb
            TInf.NitAbo = Creditos.Nit.
  ELSE
      ASSIGN TInf.AgeDeb = Ahorros.Age_DebAutomatico
             TInf.PtoDeb = AhorroS.Cod_DebAutomatico
             TInf.CtaDeb = Ahorros.Cue_DebAutomatico
             TInf.VrADeb = W_VrADeb
             TInf.NitAbo = Ahorros.Nit.

     
  FIND FIRST Ahorros WHERE Ahorros.Agencia    EQ TInf.AgeDeb
                       AND Ahorros.Cod_Ahorro EQ TInf.PtoDeb
                       AND Ahorros.Cue_Ahorros EQ TInf.CtaDeb
                       AND Ahorros.Estado     EQ 1
                       AND Ahorros.Sdo_Dispon GT 0 NO-ERROR.
  IF NOT AVAIL(Ahorros) THEN DO:
     ASSIGN W_VrADeb = 0.
     RETURN.
  END.
  
  TInf.NitDeb = Ahorros.Nit.
  
  FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ W_CodDeb NO-LOCK NO-ERROR.
  
  IF  (Ahorros.Sdo_Dispon - Pro_Ahorros.Val_SdoMinimo) LT W_VrADeb
  AND (Ahorros.Sdo_Dispon - Pro_Ahorros.Val_SdoMinimo) GT 0 THEN
      W_VrADeb = Ahorros.Sdo_Disponible - Pro_Ahorros.Val_SdoMinimo.
  ELSE IF Ahorros.Sdo_Dispon LE Pro_Ahorros.Val_SdoMinimo THEN DO:
      ASSIGN W_VrADeb = 0.
      RETURN.
  END.

  FIND FIRST TempCtas WHERE TempCtas.Agen EQ Ahorros.Agencia                                     
                        AND TempCtas.TipP EQ "A"                                                 
                        AND TempCtas.Pto  EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.                
  IF NOT AVAIL(TempCtas) THEN DO:                                                                
     MESSAGE "Falta Configuración con Producto_Ahorro: " Ahorros.Cod_Ahorro SKIP                 
             "Para la Agencia : " Ahorros.Agencia                                                
             VIEW-AS ALERT-BOX ERROR.                                                            
     RETURN ERROR.                                                                               
  END.
  

  /*IF TInf.NitDeb NE TInf.NitAbo THEN DO:   Siempre lo debe Cobrar*/
  RUN RutGMF.R (INPUT  TRUE,W_Agencia,Ahorros.Agencia,1,Ahorros.Cod_Ahorro,Ahorros.Nit,  
                INPUT  Ahorros.Cue_Ahorros,W_OpCgoAho,W_VrADeb,                           
                INPUT  Comprobantes.Comprobante,                                            
                INPUT  STRING(Comprobantes.Secuencia),"Débito-Automático",0,0,                           
                OUTPUT P_ImpAplic) NO-ERROR.                                                
  IF ERROR-STATUS:ERROR THEN DO:                                                            
     MESSAGE "El programa RutGMF.P...Retornò ERROR, no se permite la operaciòn."                  
         VIEW-AS ALERT-BOX ERROR.                                                                 
     RETURN ERROR.                                                                                
  END.                                                                                                                                                                           
  /*END.*/
  
  ASSIGN Ahorros.Sdo_Disponible              = Ahorros.Sdo_Disponible - W_VrADeb
         Ahorros.Fec_UltTrans                = W_Fecha
         Ahorros.Num_RetMes                  = Ahorros.Num_RetMes + 1
         Ahorros.Num_RetDia                  = Ahorros.Num_RetDia + 1
         Ahorros.Val_RetDia                  = Ahorros.Val_RetDia + W_VrADeb
         Ahorros.Val_RetMes                  = Ahorros.Val_RetMes + W_VrADeb
         W_Cta                               = TempCtas.CtaPro
         W_Oper                              = W_OpCgoAho
         W_Cont                              = W_Cont + 1
         W_Cont:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Cont)
         TInf.VrDeb                          = W_VrADeb
         TInf.NitDeb                         = Ahorros.Nit.
    
  RUN MovAhorros.   /*Mov_Contable y Mov_Ahorros*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpDescuadre W_DebiAuto 
PROCEDURE ImpDescuadre :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  DEFI VAR Listado AS CHAR FORM "X(40)".                                                                   
                                                                                                                
  ASSIGN Listado = W_PathSpl + "DebAutCont-" + STRING(Comprobantes.Secuencia)  + ".Lst"
         W_SiInf = TRUE.
                                                                                                                
  {Incluido\Imprimir.I "listado"}

  W_SiInf = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpInforme W_DebiAuto 
PROCEDURE ImpInforme :
/*------------------------------------------------------------------------------
  Purpose:     
 ------------------------------------------------------------------------------*/
 DEFI VAR TotXD   LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TotDb   LIKE Mov_Contable.Db INIT 0.
  
 {Incluido\RepEncabezado.I}

 ASSIGN W_Reporte = "Detalle : Aplicación Débito-Automático      Fecha del Informe: " +
                     STRING(W_Fecha,"99/99/9999") + "      Hora : " + STRING(TIME,"HH:MM:SS")
        W_EncColumna = "Comprobante: " + STRING(Comprobantes.Comprobante,"99") + "-" + 
                       STRING(Comprobantes.Secuencia,"99999999") + "-" + Comprobantes.Nombre.

 VIEW FRAME F-Encabezado.
 VIEW FRAME f-ftr.

 FOR EACH TInf BY AgeAbo BY NitAbo BY TipAbo BY PtoAbo BY CtaAbo:
     ASSIGN TotXD = TotXD + TInf.VrADeb
            TotDb = TotDb + TInf.VrDeb.

     DISPLAY TInf.AgeAbo       LABEL "Ag."
             TInf.TipAbo       LABEL "T"
             TInf.PtoAbo       LABEL "Pto"
             TInf.NitAbo       LABEL "Nit-a-Abonar"
             TInf.CtaAbo       LABEL "Cta-a-Abonar"
             TInf.VrCuot       LABEL "Cuota Pdcto"
             TInf.VrADeb       LABEL "Vr.Tot x Debitar"  FORM "->>>>>>,>>>,>>9.99"
             TInf.VrDeb        LABEL "Valor Debitado"    FORM "->>>>>>,>>>,>>9.99"
             TInf.FecPxD       LABEL "Fec-ProxDeb"
             TInf.AgeDeb       LABEL "Ag.Db"
             TInf.NitDeb       LABEL "Nit Debitado"
             TInf.PtoDeb       LABEL "Pto"
             TInf.CtaDeb       LABEL "Cta-Debitada" SKIP (0)
          WITH DOWN WIDTH 200 FRAME FDet USE-TEXT NO-LABELS STREAM-IO NO-BOX. 
             
 END.

 DISPLAY "Totales------------> Valores por Debitar $"
         TotXD     FORM "->>>>>>,>>>,>>9.99" SKIP
         "                     Valores Debitados   $"
         TotDb     FORM "->>>>>>,>>>,>>9.99" 
    WITH DOWN WIDTH 200 FRAME FDetot USE-TEXT NO-LABELS STREAM-IO NO-BOX. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MovAhorros W_DebiAuto 
PROCEDURE MovAhorros :
/*------------------------------------------------------------------------------
  Purpose:   Crea Mov_Contable y Mov_Ahorros.  
------------------------------------------------------------------------------*/
  CREATE Mov_Contable.
  ASSIGN Mov_Contable.Agencia        = Ahorros.Agencia             
         Mov_Contable.Cuenta         = W_Cta
         Mov_Contable.Nit            = Ahorros.Nit
         Mov_Contable.Fec_Contable   = W_Fecha                        
         Mov_Contable.Comentario     = "Débito-Automático"          
         Mov_Contable.Usuario        = W_Usuario                      
         Mov_Contable.Cen_Costos     = W_Cencosgral                   
         Mov_Contable.Destino        = W_Agencia                      
         Mov_Contable.Comprobante    = Comprobantes.Comprobante       
         Mov_Contable.Num_Documento  = Comprobantes.Secuencia   
         Mov_Contable.Doc_Refer      = STRING(Comprobantes.Secuencia)
         Mov_Contable.Fec_Grabacion  = TODAY                          
         Mov_Contable.Hora           = TIME                           
         Mov_Contable.Estacion       = W_Estacion               
         Mov_Contable.Db             = W_VrADeb.  

  CREATE Mov_Ahorros.
  ASSIGN Mov_Ahorros.Agencia        = Ahorros.Agencia
         Mov_Ahorros.Age_Destino    = Ahorros.Agencia                   
         Mov_Ahorros.Age_Fuente     = W_Agencia                          
         Mov_Ahorros.Cod_Ahorro     = Ahorros.Cod_Ahorro                        
         Mov_Ahorros.Cue_Ahorros    = Ahorros.Cue_Ahorro                 
         Mov_Ahorros.Fecha          = W_Fecha                            
         Mov_Ahorros.Hora           = TIME                               
         Mov_Ahorros.Nit            = Ahorros.Nit                        
         Mov_Ahorros.Num_Documento  = STRING(Comprobantes.Secuencia)     
         Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible  +  Ahorros.Sdo_Canje              
         Mov_Ahorros.Usuario        = W_Usuario                          
         Mov_Ahorros.Val_Efectivo   = W_VrADeb
         Mov_Ahorros.Cod_Operacion  = W_Oper
         Mov_Ahorros.Cpte           = Comprobantes.Comprobante
         Mov_Ahorros.Descrip        = "Débito-Automático".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W_DebiAuto 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
 ------------------------------------------------------------------------------*/
 IF W_SiInf THEN DO:
    RUN ImpInforme.

    RETURN.
 END.

 DEFI VAR TotD   LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TotC   LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TTotD  LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TTotC  LIKE Mov_Contable.Db INIT 0.

 {Incluido\RepEncabezado.I}

 ASSIGN W_Reporte = "Cpte Resumen : Contabilización Débito-Automático      Fecha del Informe: " +
                     STRING(W_Fecha,"99/99/9999") + "      Hora : " + STRING(TIME,"HH:MM:SS")
        W_EncColumna = "Comprobante: " + STRING(Comprobantes.Comprobante,"99") + "-" + 
                       STRING(Comprobantes.Secuencia,"99999999") + "-" + Comprobantes.Nombre.

 VIEW FRAME F-Encabezado.
 VIEW FRAME f-ftr.

 FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante   EQ Comprobantes.Comprobante
                         AND Mov_Contable.Num_Documento EQ Comprobantes.Secuencia
                         AND Mov_Contable.Fec_Contable  EQ W_Fecha NO-LOCK
                             BREAK BY Mov_Contable.Agencia BY Mov_Contable.Cuenta
                                   BY Mov_Contable.Nit:
     ASSIGN TotD  = TotD  + Mov_Contable.Db
            TTotD = TTotD + Mov_Contable.Db
            TotC  = TotC  + Mov_Contable.Cr
            TTotC = TTotC + Mov_Contable.Cr.

     IF LAST-OF(Mov_Contable.Nit) THEN DO:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Mov_Contable.Cuenta
                             AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
        DISPLAY Mov_Contable.Agencia   LABEL "Ag."
                Mov_Contable.Cuenta    LABEL "Cta-Contable"
                Cuentas.Nombre         LABEL "Descripciòn de la Cuenta" WHEN AVAIL(Cuentas)
                Mov_Contable.Nit       LABEL "Ced/Nit"
                Mov_Contable.Doc_Refer LABEL "Doc-Refer"
                TotD                   LABEL "TOTAL DEBITOS"  FORM "->>>>>>,>>>,>>9.99"
                TotC                   LABEL "TOTAL CREDITOS" FORM "->>>>>>,>>>,>>9.99"
            WITH DOWN WIDTH 140 FRAME F21 USE-TEXT NO-LABELS STREAM-IO NO-BOX.

        ASSIGN TotD  = 0
               TotC  = 0.
     END.

 END.

 DISPLAY SKIP(1)
         "                     TOTAL FINAL------------>                                       ------------------ ------------------"
         SKIP
         "                                                                                   "
         TTotD      FORM "->>>>>>,>>>,>>9.99"
         TTotC      FORM "->>>>>>,>>>,>>9.99"
            WITH DOWN WIDTH 180 FRAME FT21T USE-TEXT NO-LABELS STREAM-IO NO-BOX.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida W_DebiAuto 
PROCEDURE Valida :
/*------------------------------------------------------------------------------
  Purpose: Valida y halla las Ctas-Contables (Las Graba en tabla-temp TempCtas).
------------------------------------------------------------------------------*/
  FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado EQ 1 NO-LOCK 
                          BY Pro_Ahorros.Cod_Ahorro:
      FOR EACH CortoLargo WHERE CortoLargo.Clase_Producto EQ 1
                            AND CortoLargo.Cod_Producto   EQ Pro_Ahorros.Cod_Ahorro
                            AND CortoLargo.Plazo_Inicial  GE 0 NO-LOCK
               BREAK BY CortoLargo.Agencia BY CortoLargo.Cod_Producto BY CortoLargo.Plazo_Inicial:
          IF FIRST-OF(CortoLargo.Cod_Producto) THEN DO:
             FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_AsoAd
                                  AND Cuentas.Tipo   EQ 2
                                  AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
             IF AVAIL(Cuentas) THEN 
                FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_SyA
                                     AND Cuentas.Tipo   EQ 2
                                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.

             IF NOT AVAIL(Cuentas) THEN DO:
                MESSAGE "En CortoLargo.Cta_AsoAd y CortoLargo.Cta_SyA deben existir Activas en Cuentas..." SKIP
                        "Para el Pro_Ahorros.Cod_Ahorro : " Pro_Ahorros.Cod_Ahorro   SKIP
                        "De la Agencia : "                  CortoLargo.Agencia
                        VIEW-AS ALERT-BOX ERROR.
                RETURN ERROR.
             END.

             CREATE TempCtas.
             ASSIGN TempCtas.Age    = CortoLargo.Agencia
                    TempCtas.TipP   = "A"
                    TempCtas.Pto    = CortoLargo.Cod_Producto
                    TempCtas.CtaPro = CortoLargo.Cta_AsoAd
                    TempCtas.CtaSyA = CortoLargo.Cta_SyA.

             IF Pro_Ahorros.Tip_Ahorro EQ 4 THEN NEXT.

             FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 1
                                    AND Liqui_Int.Cod_Producto   EQ CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
             IF NOT AVAIL(Liqui_Int) THEN DO:
                MESSAGE "Falta Liqui_Int Para el Pro_Ahorros.Cod_Ahorro : " Pro_Ahorros.Cod_Ahorro   SKIP
                        VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
             END.

             ASSIGN TempCtas.CtaLiq = Liqui_Int.Cta_CauCr         /*Los Causados*/   
                    TempCtas.CtaIng = Liqui_Int.CtaCr_LiqAso      /*Los Por Pagar*/
                    TempCtas.IntAnt = Liqui_Int.CtaCr_Ret.        /*Ret-Fuente*/     
          END.
      END.
  END.
    
  FOR EACH Pro_Creditos WHERE Pro_Creditos.Estado EQ 1 NO-LOCK 
                           BY Pro_Creditos.Cod_Credito:
      FOR EACH CortoLargo WHERE CortoLargo.Clase_Producto EQ 2
                            AND CortoLargo.Cod_Producto   EQ Pro_Creditos.Cod_Credito
                            AND CortoLargo.Plazo_Inicial  GE 0 NO-LOCK
               BREAK BY CortoLargo.Agencia BY CortoLargo.Cod_Producto BY CortoLargo.Plazo_Inicial:
          IF FIRST-OF(CortoLargo.Cod_Producto) THEN DO:
             FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_AsoAd
                                  AND Cuentas.Tipo   EQ 2
                                  AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
             IF AVAIL(Cuentas) THEN DO:
                FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_SyA
                                     AND Cuentas.Tipo   EQ 2
                                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                IF AVAIL(Cuentas) THEN DO:
                   FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_CostasDB 
                                        AND Cuentas.Tipo   EQ 2
                                        AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                   IF AVAIL(Cuentas) THEN DO:
                      FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_HonorariosDB 
                                           AND Cuentas.Tipo   EQ 2
                                           AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                      IF AVAIL(Cuentas) THEN
                         FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_PolizasDB 
                                              AND Cuentas.Tipo   EQ 2
                                              AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR. 
                   END.
                END.
             END.
                
             IF NOT AVAIL(Cuentas) THEN DO:
                MESSAGE "En CortoLargo.Cta_AsoAd,Cta_SyA,Cta_CostasDB,Cta_HonorariosDB,Cta_PolizasDB..." SKIP
                        "deben existir Activas en Cuentas...Para el Pro_Creditos.Cod_Credito : " Pro_Creditos.Cod_Credito   SKIP
                        "De la Agencia : "                  CortoLargo.Agencia
                        VIEW-AS ALERT-BOX ERROR.
                RETURN ERROR.
             END.

             CREATE TempCtas.
             ASSIGN TempCtas.Age    = CortoLargo.Agencia
                    TempCtas.TipP   = "C"
                    TempCtas.Pto    = CortoLargo.Cod_Producto
                    TempCtas.CtaPro = CortoLargo.Cta_AsoAd
                    TempCtas.CtaSyA = CortoLargo.Cta_SyA
                    TempCtas.CtaHon = CortoLargo.Cta_HonorariosDB
                    TempCtas.CtaPol = CortoLargo.Cta_PolizasDB
                    TempCtas.CtaCos = CortoLargo.Cta_CostasDB
                    TempCtas.CtaGar = CortoLargo.Cta_VigGarAd
                    TempCtas.CtaCGa = CortoLargo.Cta_ContrapartidaGar.
                
             FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 2
                                    AND Liqui_Int.Cod_Producto   EQ CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
             IF NOT AVAIL(Liqui_Int) THEN DO:
                MESSAGE "Falta Liqui_Int Para el Pro_Creditos.Cod_Credito : " Pro_Creditos.Cod_Credito   SKIP
                        VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
             END.
             
             FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaCr_LiqAso
                                  AND Cuentas.Tipo   EQ 2
                                  AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
             IF AVAIL(Cuentas) THEN DO:
                FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaDb_LiqAso 
                                     AND Cuentas.Tipo   EQ 2
                                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                IF AVAIL(Cuentas) THEN DO:
                   FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaInt_AntAso 
                                        AND Cuentas.Tipo   EQ 2
                                        AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                   IF AVAIL(Cuentas) THEN DO:
                      FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaDb_MoraAso 
                                           AND Cuentas.Tipo   EQ 2
                                           AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                      IF AVAIL(Cuentas) THEN DO:
                         FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaDb_DifCobAso 
                                              AND Cuentas.Tipo   EQ 2
                                              AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                         IF AVAIL(Cuentas) THEN
                            FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaCr_DifCobAso 
                                                 AND Cuentas.Tipo   EQ 2
                                                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                      END.
                   END.
                END.
             END.

             IF NOT AVAIL(Cuentas) THEN DO:
                MESSAGE "En Liqui_Int las Cuentas : CtaCr_LiqAso,CtaDb_LiqAso,CtaCr_DifCobAso"
                        "                           CtaInt_AntAso,CtaDb_MoraAso,CtaDb_DifCobAso" SKIP
                        "Deben existir Activas en Plan de Cuentas..." SKIP
                        "Para el Pro_Creditos.Cod_Credito : " Pro_Creditos.Cod_Credito
                        VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
             END.

             ASSIGN TempCtas.CtaLiq = Liqui_Int.CtaDb_LiqAso
                    TempCtas.CtaIng = Liqui_Int.CtaCr_LiqAso
                    TempCtas.IntAnt = Liqui_Int.CtaInt_AntAso
                    TempCtas.IntMor = Liqui_Int.CtaDb_MoraAso 
                    TempCtas.DifCoD = Liqui_Int.CtaDb_DifCobAso
                    TempCtas.DifCoH = Liqui_Int.CtaCr_DifCobAso
                    TempCtas.Oper   = Liqui_Int.Cod_Operacion.
          END.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

