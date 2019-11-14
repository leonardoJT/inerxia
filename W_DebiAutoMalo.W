&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W_DebiAuto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W_DebiAuto 
/*------------------------------------------------------------------------
  File:        W-DebiAuto.W 
  Description: Débito Automático por Traslado entre Cuentas.
  Author:      GAER
  Created:     Marzo 30/2004
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

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
   DEFI VAR W_Oper      LIKE Mov_Creditos.Cod_Operacion.
   DEFI VAR W_Desc      LIKE Mov_Creditos.Descrip.
   DEFI VAR VrDist      LIKE Creditos.Sdo_Capital INITIAL 0.

   /*Para Parámetros Output de AboCreditos.P, donde retorna lo de c/abono*/                                                               
   DEFINE VAR  P_Poliza         LIKE Creditos.Cuota INITIAL 0.                                   
   DEFINE VAR  P_Honora         LIKE Creditos.Cuota INITIAL 0.                                   
   DEFINE VAR  P_Costas         LIKE Creditos.Cuota INITIAL 0.                                   
   DEFINE VAR  P_IMora          LIKE Creditos.Cuota INITIAL 0.                                   
   DEFINE VAR  P_IDifCob        LIKE Creditos.Cuota INITIAL 0.                                   
   DEFINE VAR  P_ICte           LIKE Creditos.Cuota INITIAL 0.                                   
   DEFINE VAR  P_IAntic         LIKE Creditos.Cuota INITIAL 0. /*Si P_IAntic(-) Neg.son cargos*/ 
   DEFINE VAR  P_Capit          LIKE Creditos.Cuota INITIAL 0.                                   
   DEFINE VAR  P_VlrNoDist      LIKE Creditos.Cuota INITIAL 0. /*Valor NO Distribuido*/

   DEFI VAR W_OpAboCred LIKE Operacion.Cod_Operacion EXTENT 8. /*Abonos a Créditos*/
   DEFI VAR W_OpCgoCred LIKE Operacion.Cod_Operacion.

   ASSIGN W_OpAboCred [1] = 020101001  /*Abonos a capital Créditos*/
          W_OpAboCred [2] = 020101002  /*Abonos a IntMora Créditos*/
          W_OpAboCred [3] = 020101003  /*Abonos a IntCtes Créditos*/
          W_OpAboCred [4] = 020101004  /*Abonos a IntDifCob Créditos*/
          W_OpAboCred [5] = 020101005  /*Abonos a IntAntic. Créditos*/
          W_OpAboCred [6] = 020101006  /*Abonos a Polizas Créditos*/
          W_OpAboCred [7] = 020101007  /*Abonos a Honorarios Créditos*/
          W_OpAboCred [8] = 020101008  /*Abonos a CostasJ.Créditos*/
          W_OpCgoCred = 020102006.     /*Cargos a Intereses(Todos) Créditos, Para trasl Dif-Cobro*/ 

   DEFI VAR W_OpAboAho  LIKE Operacion.Cod_Operacion.
   DEFI VAR W_OpCgoAho  LIKE Operacion.Cod_Operacion.

   ASSIGN W_OpAboAho = 010301001      /*Consignaciones para Ahorros*/ 
          W_OpCgoAho = 010302001.      /*Retiros para Ahorros*/ 

   DEFI TEMP-TABLE TPC
        FIELD TP      AS CHAR FORM "X(1)"
        FIELD Op      AS INTEG FORM "9"
        FIELD Cpto    AS CHAR FORM "X(10)"
        FIELD AgeP    LIKE Ahorros.Agencia
        FIELD NitP    LIKE Ahorros.Nit
        FIELD CodP    LIKE Pro_Ahorros.Cod_Ahorro
        FIELD CtaP    LIKE Ahorros.Cue_Ahorro
        FIELD NomP    LIKE Pro_Ahorros.Nom_Produc
        FIELD Cargo   LIKE Mov_Contable.Db INIT 0
        FIELD Abono   LIKE Mov_Contable.Cr INIT 0
        FIELD AgDest  LIKE Ahorros.Agencia
        FIELD Contra  LIKE Mov_Contable.Cuenta
        FIELD CedCont LIKE Mov_Contable.Nit
        FIELD DocRef  LIKE Mov_Contable.Doc_Refer
              INDEX Ppal IS UNIQUE TP AgeP NitP CodP CtaP Cpto.
    
   DEFI TEMP-TABLE TPdctos
        FIELD Agen    LIKE Ahorros.Agencia
        FIELD FApert  LIKE Ahorros.Fec_Apert
        FIELD TP      AS   CHAR FORM "X(1)"
        FIELD CodP    LIKE Pro_Ahorros.Cod_Ahorro
        FIELD CtaP    LIKE Ahorros.Cue_Ahorro
        FIELD NomP    LIKE Pro_Ahorros.Nom_Produc
        FIELD Cuota   LIKE Ahorros.Cuota
        FIELD VrSdo   LIKE Ahorros.Sdo_Dispon.

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Proc

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS W_CmbOfi Tg_Proc W_NitCte BUTTON-5 ~
Btn_Contabilizar Btn_Done RECT-314 
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
     SIZE 11.14 BY 1.69 TOOLTIP "Carga/Abona productos y Contabiliza".

DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Salir" 
     SIZE 11.14 BY 1.65
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 5" 
     SIZE 11.14 BY 1.81.

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(50)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     DROP-DOWN-LIST
     SIZE 36.14 BY .92 TOOLTIP "Agencias Disponibles"
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
     SIZE 14.43 BY 6.27.

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
     Btn_Contabilizar AT ROW 6.15 COL 61.29 HELP
          "Permite Realizar la Contabilización de los Traslados"
     Btn_Done AT ROW 7.92 COL 61.29 HELP
          "Sale del proceso de Depreciación y Ajustes"
     W_mensaje AT ROW 9.19 COL 1.86 COLON-ALIGNED NO-LABEL
     W_Cont AT ROW 9.19 COL 45.29 COLON-ALIGNED NO-LABEL
     RECT-314 AT ROW 3.73 COL 59.57
     "RegistrosTotales" VIEW-AS TEXT
          SIZE 15.29 BY .5 AT ROW 8.65 COL 42.57
          BGCOLOR 18 FGCOLOR 15 
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
         TITLE              = "Débito Automático, Programa W_DebiAuto.W"
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
                                                                        */
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
ON END-ERROR OF W_DebiAuto /* Débito Automático, Programa W_DebiAuto.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_DebiAuto W_DebiAuto
ON WINDOW-CLOSE OF W_DebiAuto /* Débito Automático, Programa W_DebiAuto.W */
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
    FOR EACH TempCtas: DELETE TempCtas. END.

    RUN Valida NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
       MESSAGE "La Configuración Contable Presento Errores...Proceso cancelado."
              VIEW-AS ALERT-BOX ERROR.

       RETURN.
    END.

    FIND FIRST Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
    IF AVAIL(Entidad) THEN
       FIND FIRST Comprobantes WHERE Comprobantes.Agencia EQ W_Agencia
                                 AND Comprobantes.Comprob EQ Entidad.Cpte_RecNom
                                 AND Comprobantes.Estado  EQ 1 NO-ERROR.

    IF NOT AVAIL(Comprobantes) OR NOT AVAIL(Entidad) THEN DO:
       MESSAGE "El Comprobante-Fuente Contable para el proceso debe existir en Entidad y en Comprobantes" SKIP
             "                        No se acepta la Operación." VIEW-AS ALERT-BOX ERROR.
        
       ASSIGN W_Mensaje:SCREEN-VALUE = "Proceso Contable Cancelado...Revise por favor".
       RETURN.
    END.
        
    IF NOT Tg_Proc THEN DO:
       APPLY "LEAVE" TO W_NitCte. 
        
       IF NOT AVAIL(Clientes) THEN
          RETURN.
    END.
        

    SESSION:SET-WAIT-STATE("GENERAL").

    RUN Contabilizar NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
       MESSAGE "La Contabilización Presento Errores...Proceso cancelado."
              VIEW-AS ALERT-BOX ERROR.

       SESSION:SET-WAIT-STATE("").

       ASSIGN W_Mensaje:SCREEN-VALUE = "Proceso Contable Cancelado...Revise por favor". 
        
       RETURN.
    END.
    ELSE DO:
       DEFI VAR Listado AS CHAR FORM "X(40)".                                                                   
                                                                                                                
       ASSIGN Listado = W_PathSpl + "DAContab-" + STRING(Comprobantes.Secuencia)  + ".Lst"
              W_Mensaje:SCREEN-VALUE = "Proceso Contable Débito-Automático Exitoso...".
                                                                                                                
       {Incluido\Imprimir.I "listado"}                                                                           
                                                                                                                
       SESSION:SET-WAIT-STATE("").

       FOR EACH TPC:             DELETE TPC.             END.
       FOR EACH TPdctos:         DELETE TPdctos.         END.
       FOR EACH CopMov_Contable: DELETE CopMov_Contable. END.
       FOR EACH TempCtas:        DELETE TempCtas.        END.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W_DebiAuto
ON CHOOSE OF Btn_Done IN FRAME F_Proc /* Salir */
DO:
  FIND FIRST TPC NO-ERROR.
  IF AVAIL(TPC) THEN DO:
     MESSAGE "Existen Registros pendientes por Contabilizar...?" SKIP
             "                  Desea Ignorarlos...?" 
             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Salir"
             UPDATE W_RptaRet AS LOGICAL.
     IF NOT W_RptaRet THEN RETURN. 
  END.
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar W_DebiAuto 
PROCEDURE Contabilizar :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  DO TRANS ON ERROR UNDO:
     ASSIGN Comprobantes.Secuencia = Comprobantes.Secuencia + 1.
     FIND CURRENT Comprobantes NO-LOCK NO-ERROR.   
        
     IF Tg_Proc THEN FOR EACH Agencias WHERE Agencias.Estado  EQ 1 
                                         AND Agencias.Agencia GE W_OfiIni 
                                         AND Agencias.Agencia LE W_OfiFin NO-LOCK:  
        FOR EACH Creditos WHERE Creditos.Agencia   EQ Agencias.Agencia
                          AND Creditos.Cuota       GT 0
                          AND Creditos.FOR_Pago    EQ 3
                          AND Creditos.Sdo_capital GT 0:
            ASSIGN W_NitCte = Creditos.Nit.
                
            
        END.
            
        FOR EACH Ahorros WHERE Ahorros.Agencia EQ Agencias.Agencia
                         AND Ahorros.Cuota     GT 0
                         AND Ahorros.FOR_Pago  EQ 3
                         AND Ahorros.Estado    EQ 1
                         AND Ahorros.Sdo_Dispo GT 0:
            ASSIGN W_NitCte = Ahorros.Nit.              
        END.                
     END.
     ELSE DO:
        FOR EACH Creditos WHERE Creditos.Nit       EQ W_NitCte
                          AND Creditos.Cuota       GT 0
                          AND Creditos.FOR_Pago    EQ 3
                          AND Creditos.Sdo_capital GT 0:
           
                
            
        END.
            
        FOR EACH Ahorros WHERE Ahorros.Nit     EQ W_NitCte
                         AND Ahorros.Cuota     GT 0
                         AND Ahorros.FOR_Pago  EQ 3
                         AND Ahorros.Estado    EQ 1
                         AND Ahorros.Sdo_Dispo GT 0:
                          
        END.               
     END.
  END.  /*Fin Tx*/
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
  ENABLE W_CmbOfi Tg_Proc W_NitCte BUTTON-5 Btn_Contabilizar Btn_Done RECT-314 
      WITH FRAME F_Proc IN WINDOW W_DebiAuto.
  {&OPEN-BROWSERS-IN-QUERY-F_Proc}
  VIEW W_DebiAuto.
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

