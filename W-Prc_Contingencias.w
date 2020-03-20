&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
  DEFINE VARIABLE AgeIni     AS INTEGER FORMAT "999" INITIAL 0.
  DEFINE VARIABLE AgeFin     AS INTEGER FORMAT "999" INITIAL 999.
  DEFINE VARIABLE W_Ok       AS LOGICAL.
  DEFINE VAR w-hora AS CHARACTER FORMAT "X(8)" .
  DEFINE VARIABLE W_Agencia  AS INTEGER FORMAT "999".
  DEFINE VARIABLE W_PCBack   AS CHARACTER FORMAT "X(40)" EXTENT 11.

/* Variables para el control concurrente */
  DEFINE VAR w-h_act AS INTEGER.
  DEFINE VAR w-h_prg AS INTEGER.
  DEFINE VAR w-h_pro AS INTEGER.
  DEFINE VAR w-prohora AS INTEGER.
  DEFINE VAR W-promer   AS CHARACTER FORMAT "X(2)".
  DEFINE VAR w-hora_act AS CHARACTER FORMAT "X(8)".
  DEFINE VAR w-hora_prg AS CHARACTER FORMAT "X(8)".
  DEFINE VAR w-hora_pro AS CHARACTER FORMAT "X(8)".
  DEFINE VAR w-copia    AS LOGICAL INITIAL FALSE.

  W_PCBack[1]  = "\\172.28.1.151\datos\Contingencias\".      /* Ag. Belen    ok */
  W_PCBack[2]  = "\\172.29.1.2\d\Contingencias\".            /* Ag. Centro   ok */
  W_PCBack[3]  = "\\172.29.2.1\d\Contingencias\".            /* Ag. Sabaneta ok */
  W_PCBack[4]  = "\\172.29.3.1\d\Contingencias\".            /* Ag. Caldas   ok */
  W_PCBack[5]  = "\\172.29.4.1\d\Contingencias\".            /* Ag. Envigado ok */
  W_PCBack[6]  = "\\172.29.5.1\d\Contingencias\".            /* Ag. Itagui   ok */
  W_PCBack[7]  = "\\172.29.6.1\d\Contingencias\".            /* Ag. Floresta ok*/
  W_PCBack[8]  = "\\172.29.7.1\contingencias\".              /* Ag. Rionegro ok*/
  W_PCBack[9]  = "\\172.29.8.1\d\Contingencias\".            /* Ag. Altavista ok*/
  W_PCBack[10] = "\\172.28.1.100\h\Contingencias\".          /* Ag. Administ */
  w_PCBack[11] = "\\172.28.1.158\c\Contingencias\".          /* SERVICAJA */
  
/* Consulta de Saldos por Cuentas de Ahorros */  
  DEFINE TEMP-TABLE Tnit
    FIELD T_Agen     AS INTEGER   FORMAT "z9"
    FIELD T_Nit      AS CHARACTER FORMAT "X(12)"
    FIELD T_Nombre   AS CHARACTER FORMAT "X(40)"
    FIELD T_CodAho   AS INTEGER   FORMAT "z9"
    FIELD T_Producto AS CHARACTER FORMAT "X(16)"
    FIELD T_CueAho   AS CHARACTER FORMAT "X(8)"
    FIELD T_Cuota    AS DECIMAL   FORMAT "99999999999999"
    FIELD T_Dispon   AS DECIMAL   FORMAT "99999999999999"
    FIELD T_Canje    AS DECIMAL   FORMAT "99999999999999"
    FIELD T_Interes  AS DECIMAL   FORMAT "99999999999999"
    FIELD T_FecApe   AS DATE FORMAT "99/99/9999"
    FIELD T_FecVen   AS DATE FORMAT "99/99/9999"
    INDEX Tidx_Nit T_Agen T_Nit T_CodAho DESC.

  DECLARE S1 CURSOR FOR
    SELECT a.agencia     FORMAT "z9",                                                                                      
           a.nit         FORMAT "X(12)",                                                                                   
           SUBSTRING(TRIM(c.apellido1) + " " + TRIM(c.apellido2) + " " + TRIM(c.nombre),1,40)  LABEL "Nombre",             
           a.cod_ahorro         FORMAT "z9",
           TRIM(d.nom_producto) FORMAT "X(16)"          LABEL "Producto",
           a.cue_ahorros        FORMAT "X(8)"           LABEL "CueAho",
           a.cuota              FORMAT "zzz,zzz,zz9",                                                                             
           a.sdo_disponible     FORMAT "99999999999999" LABEL "Saldo",                                         
           a.sdo_canje          FORMAT "99999999999999" LABEL "Canje",
           a.Int_Pagar          FORMAT "99999999999999" LABEL "Interes",
           a.fec_apertura       FORMAT "99/99/9999",                                                                          
           a.fec_vencimiento    FORMAT "99/99/9999"                                                                           
    FROM  ahorros a, Clientes C, pro_ahorros D                                                                                            
    WHERE a.nit = c.nit AND a.estado = 1       AND                                                            
          (a.sdo_disponible + a.sdo_canje) > 0 AND
          a.cod_ahorro  = d.cod_ahorro AND
          a.agencia     GE AgeIni AND 
          a.agencia     LE AgeFin
    ORDER BY a.Agencia, a.nit, a.cod_ahorro.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-285 RECT-286 RECT-287 Cmb_Agencias ~
T-Concurrente cmb-horacte t-actProg T-Inf_ahorros Cmb-hora Cmb-minutos ~
Cmb-Meridiano T-Inf_Creditos btn-Salida 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Agencias T-Concurrente cmb-horacte ~
t-actProg T-Inf_ahorros Cmb-hora Cmb-minutos Cmb-Meridiano T-Inf_Creditos 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Procesar 
     LABEL "Procesar" 
     SIZE 18 BY 1.35.

DEFINE BUTTON btn-Salida 
     LABEL "Salir" 
     SIZE 18 BY 1.35.

DEFINE VARIABLE Cmb-hora AS CHARACTER FORMAT "X(2)":U INITIAL "08" 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "01","02","03","04","05","06","07","08","09","10","11","12" 
     DROP-DOWN-LIST
     SIZE 6 BY 1
     BGCOLOR 15 FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE cmb-horacte AS INTEGER FORMAT "9":U INITIAL 1 
     LABEL "Efectuar contingencia cada" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1","2","3","4","5","6","7","8","9" 
     DROP-DOWN-LIST
     SIZE 4.86 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Cmb-Meridiano AS CHARACTER FORMAT "X(2)":U INITIAL "AM" 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "AM","PM" 
     DROP-DOWN-LIST
     SIZE 7 BY 1
     BGCOLOR 15 FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE Cmb-minutos AS CHARACTER FORMAT "X(2)":U INITIAL "45" 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "00","05","10","15","20","25","30","35","40","45","50","55" 
     DROP-DOWN-LIST
     SIZE 6 BY 1
     BGCOLOR 15 FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(40)":U INITIAL "000 - Todas las Agencias" 
     LABEL "Agencias" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "000 - Todas las Agencias","011 - Servicaja" 
     DROP-DOWN-LIST
     SIZE 29 BY 1 TOOLTIP "Seleccione la Agencia"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-285
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17.86 BY 2.27.

DEFINE RECTANGLE RECT-286
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY 2.23.

DEFINE RECTANGLE RECT-287
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 2.42.

DEFINE VARIABLE t-actProg AS LOGICAL INITIAL no 
     LABEL "Activar" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE T-Concurrente AS LOGICAL INITIAL no 
     LABEL "Activar" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE T-Inf_ahorros AS LOGICAL INITIAL no 
     LABEL "Ahorros" 
     VIEW-AS TOGGLE-BOX
     SIZE 10.72 BY .81 NO-UNDO.

DEFINE VARIABLE T-Inf_Creditos AS LOGICAL INITIAL no 
     LABEL "Créditos" 
     VIEW-AS TOGGLE-BOX
     SIZE 10.72 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     Cmb_Agencias AT ROW 2.04 COL 10 COLON-ALIGNED
     T-Concurrente AT ROW 3.69 COL 28.86
     cmb-horacte AT ROW 4.65 COL 26.72 COLON-ALIGNED
     t-actProg AT ROW 7 COL 24
     T-Inf_ahorros AT ROW 7.12 COL 4.57
     Cmb-hora AT ROW 7.88 COL 21.29 COLON-ALIGNED NO-LABEL
     Cmb-minutos AT ROW 7.88 COL 27 COLON-ALIGNED NO-LABEL
     Cmb-Meridiano AT ROW 7.88 COL 33 COLON-ALIGNED NO-LABEL
     T-Inf_Creditos AT ROW 7.92 COL 4.57
     Btn-Procesar AT ROW 9.35 COL 2
     btn-Salida AT ROW 9.46 COL 23.43
     "Hora(s)" VIEW-AS TEXT
          SIZE 6.72 BY .62 AT ROW 4.88 COL 34.29
     "Concurrente" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 3.19 COL 4
     "Programación" VIEW-AS TEXT
          SIZE 14 BY .62 TOOLTIP "Señale este item, si lo desea automáticamente" AT ROW 6.27 COL 24
     "Informes" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 6.35 COL 4
     "Plan de Contingencias - Reportes Generales" VIEW-AS TEXT
          SIZE 43 BY .81 AT ROW 1.04 COL 1
          BGCOLOR 15 FGCOLOR 7 
     RECT-285 AT ROW 6.65 COL 2.29
     RECT-286 AT ROW 6.69 COL 23
     RECT-287 AT ROW 3.42 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.04
         SIZE 43.14 BY 9.96
         BGCOLOR 7 FGCOLOR 15 .


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
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Plan Contingencias"
         HEIGHT             = 10
         WIDTH              = 42.14
         MAX-HEIGHT         = 21.19
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.19
         VIRTUAL-WIDTH      = 114.29
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
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON Btn-Procesar IN FRAME f-main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME f-main:HANDLE
       ROW             = 1.81
       COLUMN          = 2
       HEIGHT          = 1.35
       WIDTH           = 4
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Plan Contingencias */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Plan Contingencias */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Procesar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Procesar C-Win
ON CHOOSE OF Btn-Procesar IN FRAME f-main /* Procesar */
DO:
    ASSIGN FRAME F-MAIN
        cmb-hora
        Cmb-minutos
        cmb-meridiano
        Cmb_Agencias
        T-Concurrente
        cmb-horacte.

    IF SUBSTRING(Cmb_Agencias,1,3) EQ "000" OR SUBSTRING(Cmb_Agencias,1,3) EQ "011" THEN
        ASSIGN AgeIni = 0
               AgeFin = 999.
    ELSE
        ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencias,1,3))
               AgeFin = AgeIni.

    w-hora_prg = SUBSTR(cmb-hora,1,2) + ":" + SUBSTR(cmb-minutos,1,2) + " " + SUBSTR(cmb-meridiano,1,2).
    w-hora_pro = w-hora_prg.

    IF t-Inf_Ahorros THEN DO:
        W-Copia = TRUE.
        RUN inf_ahorros.
    END.

    IF t-Inf_Creditos THEN DO:
        W-Copia = TRUE.
        RUN inf_Creditos.
    END.

    w-hora = STRING(TIME,"HH:MM AM").

    IF t-ActProg OR t-concurrente THEN DO:
        ASSIGN FRAME F-MAIN
            cmb-hora
            Cmb-minutos
            cmb-meridiano.

        IF INTEGER(SUBSTR(cmb-hora,1,2)) = INTEGER(SUBSTR(w-hora,1,2)) THEN
            IF INTEGER(SUBSTR(cmb-minutos,1,2)) = INTEGER(SUBSTR(w-hora,4,2)) THEN
                IF SUBSTR(cmb-meridiano,1,2) = SUBSTR(w-hora,7,2) THEN DO:
                    ASSIGN t-inf_creditos = TRUE 
                           t-inf_ahorros = TRUE
                           w-copia = TRUE.

                    RUN inf_Ahorros.
                    RUN inf_Creditos.
                END.
                ELSE
                    IF t-ActProg THEN
                        RETURN.
                    ELSE
                        IF t-ActProg THEN
                            RETURN.
                        ELSE
                            IF t-ActProg THEN
                                RETURN.
    END.

    IF t-concurrente THEN DO:
        /* Minutos de la hora actual */
        w-h_act = (INTEGER(SUBSTRING(w-hora,1,2)) * 60 ) + INTEGER(SUBSTRING(w-hora,4,2)).

        IF (SUBSTRING(w-hora,7,2) = 'PM' AND integer(SUBSTRING(w-hora,1,2)) <> 12) OR 
           (SUBSTRING(w-hora,7,2) = 'AM' AND integer(SUBSTRING(w-hora,1,2)) = 12) THEN DO:
            w-h_act = W-h_act + 720.
        END.

        /* Minutos de la hora Programada */
        w-h_prg = (INTEGER(SUBSTRING(w-hora_prg,1,2)) * 60 ) + INTEGER(SUBSTRING(w-hora_prg,4,2)).

        IF (SUBSTRING(w-hora_prg,7,2) = 'PM' AND integer(SUBSTRING(w-hora_prg,1,2)) NE 12)  OR 
         (SUBSTRING(w-hora_prg,7,2) = 'AM' AND integer(SUBSTRING(w-hora_prg,1,2)) EQ 12) THEN DO:
        w-h_prg = W-h_prg + 720. 
      END.


      IF w-h_act GT w-h_prg THEN DO:
        /* Minutos de la proxima hora a Programar */
        w-h_pro = (INTEGER(SUBSTRING(w-hora_pro,1,2)) * 60) + INTEGER(SUBSTRING(w-hora_pro,4,2)) .
        IF (SUBSTRING(w-hora_pro,7,2) = 'PM' AND integer(SUBSTRING(w-hora_pro,1,2)) NE 12) OR   
           (SUBSTRING(w-hora_pro,7,2) = 'AM' AND integer(SUBSTRING(w-hora_pro,1,2)) EQ 12) THEN  
          w-h_pro = W-h_pro + 720.
        w-h_pro = w-h_pro + ( Cmb-Horacte * 60).

        IF w-h_pro GE 720 THEN
           W-promer = "PM".
        ELSE
           W-promer = "AM".


        /* Total minutos del dia 24 * 60 = 1440 minutos */
        IF w-h_pro GT 1440 THEN DO: /* Horas de la Madrugada del siguiente dia */
          W-promer = "AM".
          w-h_pro = w-h_pro - 1440.
        END.
        /*  ELSE */

        /* Tomar la parte enterea para la hora */
        w-prohora = TRUNCATE( w-h_pro / 60,0).
        IF w-prohora GT 12 THEN
           w-prohora = w-prohora - 12.

        ASSIGN cmb-hora:SCREEN-VALUE      = STRING(w-prohora,"99")
               cmb-meridiano:SCREEN-VALUE = w-promer.
      END.
  END.

 IF w-copia THEN DO:
  DEFINE VAR i       AS INTEGER INITIAL 0. 
  DEFINE VAR comando  AS CHARACTER FORMAT "X(150)".
  DEFINE VAR comando2 AS CHARACTER FORMAT "X(150)".
  DEFINE VAR lineabat AS CHARACTER FORMAT "X(60)".
  DEFINE VAR err-status AS INTEGER INITIAL 0.
  DEFINE VARIABLE arc_fuente  AS CHARACTER FORMAT "X(60)".
  IF SUBSTRING(Cmb_Agencias,1,3) NE "011" THEN DO:
    REPEAT i = ageini TO agefin:
      FIND agencias WHERE agencias.agencia EQ i NO-LOCK NO-ERROR.
      IF AVAILABLE(agencias) AND (t-Inf_Ahorros OR t-Inf_Creditos) THEN DO:
        comando = "c:\conting\cont_" + TRIM(agencias.nombre) + ".bat" .
        OUTPUT TO VALUE(trim(comando)).
        IF t-Inf_Ahorros THEN DO:                                                                           
          lineabat = TRIM("C:\conting\PKZIP C:\conting\" + TRIM(SUBSTRING(TRIM(agencias.nombre),1,5)) + " C:\conting\IA_" + TRIM(SUBSTRING(TRIM(agencias.nombre),1,5)) + ".TXT +u +y").  
          PUT lineabat SKIP(0).                                                                         
        END.         
        IF t-Inf_Creditos THEN DO:                                                                          
          lineabat = TRIM("C:\conting\PKZIP C:\conting\" + TRIM(SUBSTRING(TRIM(agencias.nombre),1,5)) + " C:\conting\IC_" + TRIM(SUBSTRING(TRIM(agencias.nombre),1,5)) + ".TXT +u +y").
          PUT lineabat SKIP(0).                                                                             
        END.                                                                                                
        comando2 = "COPY c:\conting\" + TRIM(SUBSTRING(TRIM(agencias.nombre),1,5)) + ".ZIP " + W_PCBack[i].
        PUT comando2 SKIP(0).
        comando2 = "DEL C:\conting\IA_" + TRIM(SUBSTRING(TRIM(agencias.nombre),1,5)) + ".TXT".
        PUT comando2 SKIP(0).
        comando2 = "DEL C:\conting\IC_" + TRIM(SUBSTRING(TRIM(agencias.nombre),1,5)) + ".TXT".
        PUT comando2 SKIP(0).
        PUT "EXIT".                                                                               
        OUTPUT CLOSE.
        OS-COMMAND VALUE(comando) SILENT.
      END.
    END.
    /* Copia a Servicaja */
    IF SUBSTRING(Cmb_Agencias,1,3) EQ "000" THEN DO:
      comando = "c:\conting\cont_Ser.bat".
      OUTPUT TO VALUE(trim(comando)).
      comando2 = "COPY c:\conting\*.ZIP " + W_PCBack[11].
      PUT comando2 SKIP(0).
      PUT "EXIT".                                                                               
      OUTPUT CLOSE.
      OS-COMMAND VALUE(comando) SILENT.
    END.
  END.
  ELSE DO:
      comando = "c:\conting\cont_Ser.bat".
      OUTPUT TO VALUE(trim(comando)).
      comando2 = "COPY c:\conting\*.ZIP " + W_PCBack[11].
      PUT comando2 SKIP(0).
      PUT "EXIT".                                                                               
      OUTPUT CLOSE.
      OS-COMMAND VALUE(comando) SILENT.
  END.
  ASSIGN t-inf_creditos = FALSE  t-inf_ahorros = FALSE w-copia = FALSE.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-Salida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-Salida C-Win
ON CHOOSE OF btn-Salida IN FRAME f-main /* Salir */
DO:
   /* OS-DELETE VALUE(Listado).*/
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Agencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencias C-Win
ON VALUE-CHANGED OF Cmb_Agencias IN FRAME f-main /* Agencias */
DO:
  ASSIGN FRAME F-main Cmb_Agencias.
  IF SUBSTRING(Cmb_Agencias,1,3) EQ "000" THEN
     ASSIGN AgeIni = 0 AgeFin = 999.
  ELSE
     ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencias,1,3)) AgeFin = INTEGER(SUBSTRING(Cmb_Agencias,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
IF T-actProg:SCREEN-VALUE IN FRAME F-main     EQ "YES" OR 
   T-Concurrente:SCREEN-VALUE IN FRAME F-main EQ "YES" THEN DO:
  IF FRAME F-main:VISIBLE THEN DO:
     APPLY "choose" TO btn-procesar IN FRAME F-main.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-actProg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-actProg C-Win
ON MOUSE-SELECT-CLICK OF t-actProg IN FRAME f-main /* Activar */
DO:
  APPLY "value-changed" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-actProg C-Win
ON VALUE-CHANGED OF t-actProg IN FRAME f-main /* Activar */
DO:
  ASSIGN t-actprog.
  IF t-actProg THEN DO:
     ASSIGN cmb-hora:SENSITIVE   = TRUE  cmb-minutos:SENSITIVE = TRUE  cmb-meridiano:SENSITIVE = TRUE
            btn-salida:SENSITIVE = FALSE.

  END.
  ELSE
     ASSIGN cmb-hora:SENSITIVE   = FALSe  cmb-minutos:SENSITIVE = FALSE  cmb-meridiano:SENSITIVE = FALSE
            btn-salida:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-Concurrente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Concurrente C-Win
ON VALUE-CHANGED OF T-Concurrente IN FRAME f-main /* Activar */
DO:
  ASSIGN t-concurrente.
  IF t-concurrente THEN DO:
     ASSIGN cmb-horacte:SENSITIVE = TRUE
            btn-salida:SENSITIVE  = FALSE
            cmb-hora:SENSITIVE    = FALSE  cmb-minutos:SENSITIVE = FALSE  cmb-meridiano:SENSITIVE = FALSE.
  END.
  ELSE
     ASSIGN cmb-horaCte:SENSITIVE = FALSe 
            btn-salida:SENSITIVE  = TRUE
            cmb-hora:SENSITIVE    = TRUE  cmb-minutos:SENSITIVE = TRUE   cmb-meridiano:SENSITIVE = TRUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-Inf_ahorros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Inf_ahorros C-Win
ON VALUE-CHANGED OF T-Inf_ahorros IN FRAME f-main /* Ahorros */
DO:
  ASSIGN T-Inf_ahorros T-Inf_Creditos.
  IF T-Inf_Ahorros OR T-Inf_Creditos THEN
     Btn-Procesar:SENSITIVE = true.
  ELSE
     Btn-Procesar:SENSITIVE = false.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-Inf_Creditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Inf_Creditos C-Win
ON VALUE-CHANGED OF T-Inf_Creditos IN FRAME f-main /* Créditos */
DO:
    ASSIGN T-Inf_Ahorros T-Inf_Creditos.
    IF T-Inf_Ahorros OR T-Inf_Creditos THEN 
       Btn-Procesar:SENSITIVE = true.
    ELSE
       Btn-Procesar:SENSITIVE = false.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


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
  RUN InitializeObject.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "W-Prc_Contingencias.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "W-Prc_Contingencias.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  RUN control_load.
  DISPLAY Cmb_Agencias T-Concurrente cmb-horacte t-actProg T-Inf_ahorros 
          Cmb-hora Cmb-minutos Cmb-Meridiano T-Inf_Creditos 
      WITH FRAME f-main IN WINDOW C-Win.
  ENABLE RECT-285 RECT-286 RECT-287 Cmb_Agencias T-Concurrente cmb-horacte 
         t-actProg T-Inf_ahorros Cmb-hora Cmb-minutos Cmb-Meridiano 
         T-Inf_Creditos btn-Salida 
      WITH FRAME f-main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_Ahorros C-Win 
PROCEDURE Inf_Ahorros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR W_agen     AS INTEGER   FORMAT "z9".                              
  DEFINE VAR W_Nit      AS CHARACTER FORMAT "X(12)".                           
  DEFINE VAR W_Nombre   AS CHARACTER FORMAT "X(40)".                           
  DEFINE VAR W_CodAho   AS INTEGER   FORMAT "z9".                                
  DEFINE VAR W_Producto AS CHARACTER FORMAT "X(16)".
  DEFINE VAR W_CueAho   AS CHARACTER FORMAT "X(8)".
  DEFINE VAR W_Cuota    AS DECIMAL   FORMAT "99999999999999".                   
  DEFINE VAR W_Dispon   AS DECIMAL   FORMAT "99999999999999".                   
  DEFINE VAR W_Canje    AS DECIMAL   FORMAT "99999999999999".                   
  DEFINE VAR W_Int_pag  AS DECIMAL   FORMAT "99999999999999".                   
  DEFINE VAR W_FecApe   AS DATE      FORMAT "99/99/9999".                           
  DEFINE VAR W_FecVen   AS DATE      FORMAT "99/99/9999".                           
  DEFINE VAR W_fecpro   AS DATE      FORMAT "99/99/9999" INITIAL TODAY.             
  
  DEFINE VAR W_totDis  AS DECIMAL FORMAT "99999999999999" INITIAL 0.                    
  DEFINE VAR W_totCan  AS DECIMAL FORMAT "99999999999999" INITIAL 0.                    
  DEFINE VAR W_totInt  AS DECIMAL FORMAT "99999999999999" INITIAL 0.                    
  DEFINE VAR W_ttotDis AS DECIMAL FORMAT "99999999999999" INITIAL 0.                    
  DEFINE VAR W_ttotCan AS DECIMAL FORMAT "99999999999999" INITIAL 0.                    
  DEFINE VAR W_ttotInt AS DECIMAL FORMAT "99999999999999" INITIAL 0.                    
  DEFINE VAR w_archivo AS CHARACTER FORMAT "X(45)".
  
  DEFINE VAR i AS INTEGER INITIAL 0.
  FOR EACH Tnit:
    DELETE Tnit.
  END.
  OPEN S1.
  REPEAT :
    FETCH S1 INTO W_Agen, W_Nit, W_nombre, W_CodAho, W_Producto, W_cueAho, W_cuota, W_dispon, w_canje, w_int_pag, W_FecApe, W_FecVen.
    CREATE Tnit.
    ASSIGN Tnit.T_Agen     = W_Agen
           Tnit.T_Nit      = W_Nit
           Tnit.T_Nombre   = W_Nombre
           Tnit.T_codaho   = W_codaho
           Tnit.T_producto = W_Producto
           Tnit.T_CueAho   = W_CueAho
           Tnit.T_Cuota    = w_Cuota
           Tnit.T_Dispon   = W_Dispon
           Tnit.T_Canje    = W_Canje
           Tnit.T_Interes  = W_Int_pag
           Tnit.T_FecApe   = W_Fecape
           Tnit.T_FecVen   = W_FecVen.
    END.
    CLOSE S1.
    /* OUTPUT TO c:\tempo.txt. */
    FOR EACH Tnit BREAK BY T_Agen:
      IF FIRST-OF(T_Agen) THEN DO:
        ASSIGN W_totDis = 0   W_totCan = 0   W_totInt = 0.
        FIND agencias WHERE agencias.agencia = Tnit.T_Agen NO-LOCK NO-ERROR.
        w_archivo =  "c:\conting\IA_" + TRIM(SUBSTRING(TRIM(agencias.nombre),1,5)) + ".TXT".
        OUTPUT TO VALUE(W_archivo).
        PUT "SALDO DE AHORROS AL " STRING(TODAY) " Hora: " STRING(TIME,"hh:mm am") SKIP(2).
        PUT "Agencia: " UPPER(TRIM(agencias.nombre)) SKIP(2).
        PUT "Cedula/Nit   Nombres Completos                        Tipo de Ahorro      Cta.Aho        Cuota  Disponible       Canje   IntxPagar  Apertura  Vencimiento" SKIP(2).
      END.
      ASSIGN W_ttotDis = W_ttotDis + T_Dispon 
             W_ttotCan = W_ttotCan + T_Canje  
             W_ttotInt = W_ttotInt + T_Interes
             W_totDis  = W_totDis  + T_Dispon 
             W_totCan  = W_totCan  + T_Canje  
             W_totInt  = W_totInt  + T_Interes.
      PUT T_Nit      " "
          T_Nombre   " "
          T_CodAho   FORMAT "99" "-"  
          T_producto FORMAT "X(16)" " "
          T_CueAho   FORMAT "X(8)"  " "
          T_Cuota    FORMAT "zzz,zzz,zz9" " "
          T_Dispon   FORMAT "zzz,zzz,zz9" " "
          T_Canje    FORMAT "zzz,zzz,zz9" " "
          T_Interes  FORMAT "zzz,zzz,zz9" " "
          T_FecApe   FORMAT "99/99/9999"  " "
          T_FecVen   FORMAT "99/99/9999"  SKIP(0).
      IF LAST-OF(T_Agen) THEN DO:
         PUT "================================================================================================================================= " SKIP(2).
         PUT "     Total Disponible: " W_totDis  FORMAT "zzz,zzz,zzz,zz9" SKIP(0).
         PUT "     Total Canje     : " W_totCan  FORMAT "zzz,zzz,zzz,zz9" SKIP(0).
         PUT "     Total IntxPagar : " W_totInt  FORMAT "zzz,zzz,zzz,zz9" SKIP(3).
      END.
    END.
    PUT "================================================================================================================================= " SKIP(2).
    PUT "Total Gral Disponible: " W_ttotDis  FORMAT "zzz,zzz,zzz,zz9" SKIP(0).
    PUT "Total Gral Canje     : " W_ttotCan  FORMAT "zzz,zzz,zzz,zz9" SKIP(0).
    PUT "Total Gral IntxPagar : " W_ttotInt  FORMAT "zzz,zzz,zzz,zz9" SKIP(3).
    OUTPUT CLOSE.
   /* OUTPUT TO c:\Prome_AhoC.txt. */  
   /* OUTPUT CLOSE. */                 
   ASSIGN w_fecpro = TODAY.            

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inf_Creditos C-Win 
PROCEDURE Inf_Creditos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR zFecVcto   AS DATE.
DEFINE VAR zFecCanc   AS DATE.
DEFINE VAR zper_pago  AS INTEGER INITIAL 0.
DEFINE VAR znombre    AS CHARACTER FORMAT "X(40)".

DEFINE VAR W_totsdoCap   AS DECIMAL INITIAL 0.
DEFINE VAR W_ttotsdoCap  AS DECIMAL INITIAL 0.
DEFINE VAR W_TSdoVdo     AS DECIMAL INITIAL 0. 
DEFINE VAR W_SdoDeuda    AS DECIMAL INITIAL 0. 
DEFINE VAR W_archivo     AS CHARACTER FORMAT "X(45)".

FOR EACH Creditos WHERE Creditos.estado = 2 AND
         Creditos.agencia GE ageini AND Creditos.agencia LE Agefin NO-LOCK BREAK By Creditos.agencia:
    IF FIRST-OF(Creditos.agencia) THEN DO:
       W_totsdoCap = 0.
       FIND agencias WHERE agencias.agencia = Creditos.agencia NO-LOCK.
       w_archivo = "c:\conting\IC_" + TRIM(SUBSTRING(TRIM(agencias.nombre),1,5)) + ".TXT".
       OUTPUT TO VALUE(W_archivo).
       PUT "Informe General de Creditos al " STRING(TODAY) " Hora: " STRING(TIME,"hh:mm am") SKIP(1).
       PUT "Agencia: " UPPER(TRIM(agencias.nombre)) SKIP(2).
       PUT "Cedula       Nombres Completos                            #Cre #Pagaré  Linea                     Per       Monto Pla       Cuota Tasa        Saldo SdoPonerDia Desembolso Vencimiento   Int.Ctes   IntDifCob   IntMorCob  IntMorDifC" SKIP.
    END.

    /* Saldo para ponerse al dia */
     ASSIGN W_TSdoVdo = Creditos.Honorarios     + Creditos.Costas         + Creditos.Polizas        +
                        Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob
           W_SdoDeuda = Creditos.Honorarios     + Creditos.Costas         + Creditos.Polizas        +
                        Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob + Creditos.Int_Corrientes +
                        Creditos.Sdo_Capital    + Creditos.INT_DifCobro   - Creditos.Int_Anticipado.

     IF Creditos.Capital_Acum GT Creditos.Sdo_CapPag THEN
        W_TSdoVdo = W_TSdoVdo + (Creditos.Capital_Acum - Creditos.Sdo_CapPag).

     IF Creditos.INT_LiqAcum GT Creditos.Sdo_IntPag THEN                               
        W_TSdoVdo = W_TSdoVdo + (Creditos.INT_LiqAcum - Creditos.Sdo_IntPag).          
                                                                            
     IF W_TSdoVdo LE 0 THEN                                                            
        W_TSdoVdo = 0. 
                                                                            
     IF W_TSdoVdo GT W_SdoDeuda THEN                                                   
        W_TSdoVdo = W_SdoDeuda.
    /* Termina saldo apara ponerse al dia */

     FIND LAST planpagos WHERE planpagos.nit       EQ creditos.nit         AND 
                               planpagos.num_credi EQ Creditos.num_credito AND
                               Id_Pdomes LE 2              AND 
                               nro_cuota EQ creditos.plazo NO-LOCK.
    IF NOT AVAIL(planpagos) THEN
       FOR EACH planpagos WHERE planpagos.nit       EQ creditos.nit         AND 
                                planpagos.num_credi EQ Creditos.num_credito AND
                                Id_Pdomes LE 2 NO-LOCK BY nro_Cuota:
           ASSIGN zfecVcto = Planpagos.Fec_Vcto.
       END.
    ELSE 
        zFecVcto = Planpagos.fec_vcto.

    CASE Creditos.Per_Pago:                                                                        
     WHEN 1 THEN ASSIGN zper_pago = 7.                                                        
     WHEN 2 THEN ASSIGN zper_pago = 10.                                                        
     WHEN 3 THEN ASSIGN zper_pago = 15.                                                        
     WHEN 4 THEN ASSIGN zper_pago = 30.                                                        
     WHEN 5 THEN ASSIGN zper_pago = 60.                                                        
     WHEN 6 THEN ASSIGN zper_pago = 90.                                                        
     WHEN 7 THEN ASSIGN zper_pago = 120.                                                        
     WHEN 8 THEN ASSIGN zper_pago = 180.                                                        
     WHEN 9 THEN ASSIGN zper_pago = 360.                                                        
    END CASE. 

  FIND clientes     WHERE clientes.nit = creditos.nit NO-LOCK NO-ERROR.
  IF AVAILABLE(Clientes) THEN
     znombre = TRIM(clientes.apellido1) + " " + TRIM(clientes.apellido2) + " " + TRIM(Clientes.nombre).

  W_totsdoCap  = W_totsdoCap  + Creditos.Sdo_capital.
  W_ttotsdoCap = W_ttotsdoCap + Creditos.Sdo_capital.
  FIND pro_creditos WHERE pro_creditos.cod_credito = creditos.cod_credito NO-LOCK.
  IF AVAILABLE(pro_creditos) THEN
     PUT creditos.nit              FORMAT "X(12)"       " "
         znombre                   FORMAT "X(40)"       " "
         Creditos.num_credito      FORMAT "zzzzzzz9"    " "
         Creditos.pagare           FORMAT "X(8)"        " "
         Pro_creditos.Nom_Producto FORMAT "X(25)"       " "
         Zper_pago                 FORMAT "zz9"         " "
         Creditos.monto            FORMAT "zzz,zzz,zz9" " "
         Creditos.plazo            FORMAT "zz9"         " "
         Creditos.Cuota            FORMAT "zzz,zzz,zz9" " "
         Creditos.tasa             FORMAT "z9.99"       " "
         Creditos.Sdo_capital      FORMAT "zzz,zzz,zz9" " "
         W_TSdoVdo                 FORMAT "zzz,zzz,zz9" " "
         Creditos.Fec_Desembolso   FORMAT "99/99/9999"  " "
         zfecVcto                  FORMAT "99/99/9999"  " " 
         Creditos.Int_Corrientes   FORMAT "zzz,zzz,zz9" " "
         Creditos.Int_DifCobro     FORMAT "zzz,zzz,zz9" " "
         Creditos.Int_MorCobrar    FORMAT "zzz,zzz,zz9" " "
         Creditos.Int_MoraDifCob   FORMAT "zzz,zzz,zz9" " "
      SKIP.
  IF LAST-OF(creditos.agencia) THEN DO:
     PUT "===========================================================================" SKIP(2).
     PUT "        Total Saldo a Capital : " W_totsdoCap FORMAT "zzz,zzz,zzz,zz9" SKIP(2).
  END.
END.
PUT "Total General Saldo a Capital :" W_ttotsdoCap FORMAT "zzz,zzz,zzz,zz9" SKIP(2).
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME F-main:
  FOR EACH Agencias WHERE Agencias.Estado EQ 1  NO-LOCK:
     W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
     IF Agencias.Agencia EQ W_Agencia THEN
        Cmb_Agencias:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

