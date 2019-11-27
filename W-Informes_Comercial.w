&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Wwin 
/*------------------------------------------------------------------------
  Description:  INFORMES HABITUALES EN el area comercial
  Author:   JOHN JAIRO MONCADA PUERTA
  Created: 7 DE mayo DE 2007
 */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{Incluido\VARIABLE.i "SHARED"}
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VAR Listado AS CHARACTER INITIAL "".
DEFINE VAR j       AS INTEGER.
DEFINE VAR k       AS INTEGER.    
DEFINE VAR w_ok     AS LOGICAL.

DEFINE VARIABLE WImagen  AS CHARACTER FORMAT "X(40)". 
DEFINE VARIABLE WAno     AS INTEGER FORMAT "9999".    
DEFINE VARIABLE WMes     AS INTEGER FORMAT "99".      
DEFINE VARIABLE WFec     AS DATE.   
DEFINE VARIABLE FecIni   AS DATE.
DEFINE VARIABLE FecFin   AS DATE.
DEFINE VARIABLE MesIni   AS INTEGER FORMAT "99".
DEFINE VARIABLE MesFin   AS INTEGER FORMAT "99".
DEFINE VARIABLE AgeIni   AS INTEGER FORMAT "999" INITIAL 0.
DEFINE VARIABLE AgeFin   AS INTEGER FORMAT "999" INITIAL 999.
DEFINE VARIABLE wcajero  AS CHARACTER                              NO-UNDO INITIAL "".
DEFINE VARIABLE wtotefecti AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO INITIAL 0.00.
DEFINE VARIABLE wtotcheque AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" NO-UNDO INITIAL 0.00.
/********** Aperturas y Cancelaciones - Transacciones ********************/
DEFINE VAR wagei      LIKE ahorros.agencia           NO-UNDO INITIAL 0.
DEFINE VAR wnita      LIKE ahorros.nit               NO-UNDO INITIAL 0.
DEFINE VAR wgrupo     LIKE grupos.grupo              NO-UNDO INITIAL 0.
DEFINE VAR wnomage    LIKE agencias.nombre           NO-UNDO INITIAL "".
DEFINE VAR wagencli    LIKE ahorros.agencia          NO-UNDO INITIAL 0.
DEFINE VAR wagencli1   AS CHARACTER FORMAT "X(40)"   NO-UNDO INITIAL "".
DEFINE VAR widempre    AS CHARACTER FORMAT "X(80)"   NO-UNDO INITIAL "".
DEFINE VAR wniti       LIKE clientes.nit             NO-UNDO INITIAL "".
DEFINE VAR wnomestado  AS CHARACTER FORMAT "X(12)"   NO-UNDO INITIAL "".
DEFINE VAR wsalcli     LIKE clientes.salario         NO-UNDO INITIAL 0.00.
DEFINE VAR wnomagecli  LIKE agencia.nombre           NO-UNDO INITIAL "".

DEFINE VAR wcodemp     LIKE clientes.cod_empresa     NO-UNDO INITIAL "".
DEFINE VAR wnomemp     LIKE empresas.alias_empresa   NO-UNDO INITIAL "".
DEFINE VAR wcontrato   AS CHARACTER FORMAT "X(30)"   NO-UNDO INITIAL "".
DEFINE VAR wnomcli     AS CHARACTER FORMAT "X(40)"   NO-UNDO INITIAL "".
DEFINE VAR wdircom     LIKE clientes.dir_comercial   NO-UNDO INITIAL "".
DEFINE VAR wtelcom     LIKE clientes.tel_comercial   NO-UNDO INITIAL "".
DEFINE VAR wtotahoapo  AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99"    INITIAL 0.
DEFINE VAR wtotcre     AS INTEGER                    NO-UNDO INITIAL 0.
DEFINE VAR Wtieahor    AS INTEGER                    NO-UNDO INITIAL 0.
DEFINE VAR Wtiecre     AS INTEGER                    NO-UNDO INITIAL 0.
DEFINE VAR wnomusu    LIKE usuarios.nombre           NO-UNDO INITIAL "".
DEFINE VAR wnomgru    LIKE grupos.nombre             NO-UNDO INITIAL "".
DEFINE VAR wcpte      LIKE mov_ahorros.cpte          NO-UNDO INITIAL 0.
DEFINE VAR wcpteo     LIKE operacion.comprobante     NO-UNDO INITIAL 0.
DEFINE VAR wcodpro    LIKE ahorros.cod_ahorro        NO-UNDO INITIAL 0.
DEFINE VAR wtippro    LIKE ahorros.tip_ahorro        NO-UNDO INITIAL 0.
DEFINE VAR wusuario   LIKE ahorros.usu_creacion      NO-UNDO INITIAL "".
DEFINE VAR wnompro    LIKE pro_ahorros.nom_producto  NO-UNDO INITIAL "".
DEFINE VAR wcodoper   LIKE mov_ahorros.cod_operacion NO-UNDO INITIAL 0.
DEFINE VAR wtipope    AS CHARACTER FORMAT "X(10)"    NO-UNDO INITIAL "".
DEFINE VAR wnomclase  AS CHARACTER FORMAT "X(12)"    NO-UNDO INITIAL "".
DEFINE VAR wefeche    AS CHARACTER FORMAT "X(10)"    NO-UNDO INITIAL "".
DEFINE VAR wopera1    AS CHARACTER FORMAT "X(40)"    NO-UNDO INITIAL "".
DEFINE VAR wcompr1    AS CHARACTER FORMAT "X(40)"    NO-UNDO INITIAL "".
DEFINE VAR wusuar1    AS CHARACTER FORMAT "X(40)"    NO-UNDO INITIAL "".
DEFINE VAR wagenc1    AS CHARACTER FORMAT "X(40)"    NO-UNDO INITIAL "".
DEFINE VAR wgrupo1    AS CHARACTER FORMAT "X(30)"    NO-UNDO INITIAL "".
DEFINE VAR wnomtipro  AS CHARACTER FORMAT "X(20)"    NO-UNDO INITIAL "".
DEFINE VAR wnomoper   LIKE operacion.nom_operacion   NO-UNDO INITIAL "".
DEFINE VAR wnomcomp   LIKE comprobantes.nombre       NO-UNDO INITIAL "".
DEFINE VAR wprodu1    AS CHARACTER FORMAT "X(50)"    NO-UNDO INITIAL "".
DEFINE VAR wcodcre    LIKE creditos.cod_credito      NO-UNDO INITIAL 0.
DEFINE VAR wtotcompa  AS INTEGER                     NO-UNDO INITIAL 0.
DEFINE VAR wtotcompu  AS INTEGER                     NO-UNDO INITIAL 0.
DEFINE VAR wtotcompo  AS INTEGER                     NO-UNDO INITIAL 0.
DEFINE VAR wtotcompc  AS INTEGER                     NO-UNDO INITIAL 0.
DEFINE VAR wtotcompg  AS INTEGER                     NO-UNDO INITIAL 0.
DEFINE VAR wtotproga  AS INTEGER                     NO-UNDO INITIAL 0.
DEFINE VAR wtotcompoa AS INTEGER                     NO-UNDO INITIAL 0.
DEFINE VAR wtotaso    AS INTEGER                     NO-UNDO INITIAL 0.
DEFINE VAR wtotaso1   AS INTEGER                     NO-UNDO INITIAL 0.
DEFINE VAR wtotasog   AS INTEGER                     NO-UNDO INITIAL 0.
DEFINE VAR wfechaini  AS DATE      FORMAT "99/99/9999" NO-UNDO INITIAL "".
DEFINE VAR wfechafin  AS DATE      FORMAT "99/99/9999" NO-UNDO INITIAL "".
DEFINE VAR wperiodo   AS CHARACTER FORMAT "X(24)"    NO-UNDO INITIAL "".
DEFINE VAR wmonto     AS DECIMAL   FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99"  NO-UNDO INITIAL 0.
DEFINE VAR wtotmonto  AS DECIMAL   FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99"  NO-UNDO INITIAL 0.
DEFINE VAR whoraini   AS INTEGER   FORMAT "99"    NO-UNDO INITIAL 0.
DEFINE VAR whorafin   AS INTEGER   FORMAT "99"    NO-UNDO INITIAL 0.
DEFINE VAR whorario   AS CHARACTER FORMAT "X(12)"    NO-UNDO INITIAL "".

DEFINE TEMP-TABLE TemCance
  FIELD TTabla     AS CHARACTER FORMAT "X(12)" INITIAL ""
  FIELD TPeriodo   AS CHARACTER FORMAT "X(20)" INITIAL ""
  FIELD Tagencia   AS CHARACTER FORMAT "X(40)" INITIAL ""
  FIELD TUsuario   AS CHARACTER FORMAT "X(40)" INITIAL ""
  FIELD TGrupo     AS CHARACTER FORMAT "X(20)" INITIAL ""
  FIELD TNomtipro  AS CHARACTER FORMAT "X(40)" INITIAL ""
  FIELD TProducto  AS CHARACTER FORMAT "X(40)" INITIAL ""
  FIELD TNumProdu  AS CHARACTER FORMAT "X(20)" INITIAL ""
  FIELD TNit       AS CHARACTER FORMAT "X(12)" INITIAL "" 
  FIELD TTotcompc  AS INTEGER INITIAL 0
  FIELD TMonto     AS DECIMAL FORMAT "->>>,>>>,>>9.99" 
  FIELD Tasocia    AS DECIMAL FORMAT "->>>,>>>,>>9.99".
/**********************************************************/

DEFINE TEMP-TABLE TPdtoAho
    FIELD Tcodage     LIKE ahorros.agencia
    FIELD TnomAge     LIKE agencias.nombre    
    FIELD TCodAho     LIKE ahorros.cod_ahorro
    FIELD TNomPdto    LIKE Pro_Ahorros.Nom_Producto
    FIELD Tsaldo      AS DECIMAL INITIAL 0.00
    INDEX IdxAge Tcodage TcodAho.

DECLARE InfPdtoAho CURSOR FOR
   SELECT a.agencia         FORMAT "z9",
          b.nombre          FORMAT "X(25)",                                                        
          a.cod_ahorro      FORMAT "zz9", 
          c.nom_producto    FORMAT "X(25)",
          SUM(a.sdo_disponible + a.sdo_canje) FORMAT "zzz,zzz,zzz,zz9"                                    
       FROM  ahorros a, agencias b, pro_ahorros c                                                     
       WHERE a.agencia          = b.agencia                                AND 
            (a.agencia         >= ageini AND a.agencia         <= agefin ) AND
            (a.fec_apertura    >= Fecini AND a.fec_apertura    <= Fecfin ) AND
            a.cod_ahorro = c.cod_ahorro AND
        a.estado = 1 /* GCamacho - May08/08 - */
       GROUP BY a.agencia, a.cod_ahorro ORDER BY a.agencia                                        
            WITH FRAME j1 WIDTH 210 NO-LABELS.

DEFINE TEMP-TABLE TDetPdtoAho
    FIELD Tcodage     LIKE ahorros.agencia
    FIELD TnomAge     LIKE agencias.nombre  
    FIELD Ttipodoc    LIKE Clientes.Tipo_Identificacion
    FIELD Tnit        LIKE ahorros.nit    
    FIELD Tcodaho     LIKE ahorros.cod_ahorro
    FIELD TNombre     AS CHARACTER FORMAT "X(40)"
    FIELD Tcant       AS INTEGER INITIAL 0
    FIELD Tsaldo      AS DECIMAL initial 0.00
    INDEX IdxAgeESt Tcodage Tnit.

DECLARE InfDetPdtoAho CURSOR FOR
  SELECT d.agencia         FORMAT "z9",
         f.nombre          FORMAT "X(15)",                                                        
         e.tipo_identificacion FORMAT "X(3)",
         d.nit             FORMAT "X(12)",   
         d.cod_ahorro      FORMAT "zz9",
         TRIM(e.apellido1) + " " + TRIM(e.apellido2) + " " + TRIM(e.nombre) FORMAT "X(40)",       
         COUNT(DISTINCT(d.cue_ahorros))     FORMAT "z,zz9",                                                        
         SUM(d.sdo_disponible + d.sdo_canje) FORMAT "zzz,zzz,zz9"                                    
      FROM  ahorros d, clientes e, agencias f                                                     
      WHERE d.agencia          = f.agencia                                AND 
            d.nit = e.nit AND 
           (d.agencia         >= ageini AND d.agencia         <= agefin ) AND
           (d.fec_apertura    >= Fecini AND d.fec_apertura    <= Fecfin ) AND
        d.estado = 1 /* GCamacho - May08/08 */
      GROUP BY d.agencia, d.nit  ORDER BY d.agencia, d.nit                                        
           WITH FRAME j2 WIDTH 210 NO-LABELS.

DEFINE TEMP-TABLE TpdtoCre
    FIELD tcage    LIKE creditos.agencia
    FIELD tcsaldo  AS DECIMAL initial 0.00
    INDEX idxage tcage.

DECLARE InfPdtoCre CURSOR FOR
   SELECT g.agencia          FORMAT "z9",
          SUM(g.sdo_capital) FORMAT "zzz,zzz,zzz,zz9"                                    
       FROM  creditos g
       WHERE (g.agencia         >= ageini AND g.agencia          <= agefin ) AND
             g.tip_credito < 7  AND g.estado = 2 AND g.sdo_capital > 0       AND 
             (g.fec_desembolso  >= Fecini AND g.fec_desembolso <= Fecfin  )
       GROUP BY g.agencia ORDER BY g.agencia                                        
            WITH FRAME j3 WIDTH 210 NO-LABELS.


DEFINE TEMP-TABLE tasoc
    FIELD tage   LIKE clientes.agencia
    FIELD tcant  AS DECIMAL INITIAL 0
    INDEX idxage tage.

DECLARE infAsoc CURSOR FOR
   SELECT h.agencia         FORMAT "z9",
          COUNT(DISTINCT(h.nit)) FORMAT "zzz,zzz,zz9"                                    
       FROM  ahorros h
       WHERE h.Tip_Ahorro = 4 AND cod_ahorro = 5 AND 
            (h.sdo_disponible + h.sdo_canje) > 0    AND 
            (h.agencia       >= ageini AND h.agencia          <= agefin ) AND
            (h.fec_apertura  >= Fecini AND h.fec_apertura <= Fecfin ) AND
        h.estado = 1 /* GCamacho - May08/08 */ 
       GROUP BY h.agencia ORDER BY h.agencia                                        
            WITH FRAME j4 WIDTH 210 NO-LABELS.

DEFINE TEMP-TABLE tclte
    FIELD tage   LIKE clientes.agencia
    FIELD tcant  AS DECIMAL INITIAL 0
    INDEX idxage tage.

DECLARE infclte CURSOR FOR
   SELECT i.agencia         FORMAT "z9",
      COUNT(DISTINCT(i.nit)) FORMAT "zzz,zzz,zz9"                                    
   FROM  clientes i
   WHERE i.Tipo_vinculo = 2 AND i.estado = 1    AND 
        (i.agencia      >= ageini AND i.agencia     <= agefin ) AND
        (i.fec_ingreso  >= Fecini AND i.fec_ingreso <= Fecfin )
   GROUP BY i.agencia ORDER BY i.agencia                                        
        WITH FRAME j4 WIDTH 210 NO-LABELS.


DEFINE TEMP-TABLE TDetpdtoCre
    FIELD tcage    LIKE creditos.agencia
    FIELD tcnit    LIKE clientes.nit
    FIELD Tccant   AS INTEGER INITIAL 0
    FIELD tcsaldo  AS DECIMAL initial 0.00
    INDEX idxage tcage tcnit.

DECLARE InfDetPdtoCre CURSOR FOR
   SELECT k.agencia         FORMAT "z9",
          k.nit             FORMAT "X(12)",
          COUNT(DISTINCT(num_credito))  FORMAT "z,zz9",
          SUM(k.sdo_capital)  FORMAT "zzz,zzz,zzz,zz9"                                    
       FROM  creditos k
       WHERE k.tip_credito < 7 AND k.estado = 2 AND k.sdo_capital > 0     AND 
            (k.agencia         >= ageini AND k.agencia        <= agefin ) AND
            (k.fec_desembolso  >= Fecini AND k.fec_desembolso <= Fecfin )
       GROUP BY k.agencia, k.nit ORDER BY k.agencia, k.nit                                        
            WITH FRAME j3 WIDTH 210 NO-LABELS.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FrmGerencia

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Img_MesF Img_MesI RECT-282 RECT-283 RECT-291 ~
BUTTON-120 BUTTON-121 Cmb_Agencias BUTTON-3 Cmb_Informes BUTTON-2 BtnDone 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Agencias W_DiaIni AnoIni W_DiaFin ~
AnoFin Cmb_Informes Msaje 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Cmb_Agencias Cmb_Informes 
&Scoped-define List-2 Msaje 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR Wwin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 13 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-120 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 120" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-121 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 121" 
     SIZE 4 BY .54.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 13 BY 1.62.

DEFINE BUTTON BUTTON-3 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 3" 
     SIZE 13 BY 1.62.

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(40)":U INITIAL "000 - Todas" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "000 - Todas" 
     DROP-DOWN-LIST
     SIZE 27 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Informes AS CHARACTER FORMAT "X(256)":U INITIAL "1" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "1 - Productos Consolidados x Agencia","1",
                     "2 - Detalle Producto Cons.x Age x Cliente","2",
                     "3 - Informe Detallado de Colocaciones","3",
                     "4 - Informe Detallado de Captaciones","4",
                     "5 - Información Demográfica","5",
                     "6 - Informe Aperturas x Usuario","6",
                     "7 - Informe Cancelaciones x Usuario","7",
                     "8 - Informe Transacciones x Usuario","8",
                     "9 - Informe Transacciones Caja x Usuario","9"
     DROP-DOWN-LIST
     SIZE 51 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AnoFin AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AnoIni AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Msaje AS CHARACTER FORMAT "X(100)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_DiaFin AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE W_DiaIni AS DECIMAL FORMAT "99":U INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81 NO-UNDO.

DEFINE IMAGE Img_MesF
     FILENAME "adeicon/blank":U
     SIZE 11 BY .81.

DEFINE IMAGE Img_MesI
     FILENAME "adeicon/blank":U
     SIZE 11 BY .81.

DEFINE RECTANGLE RECT-282
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22 BY 1.88.

DEFINE RECTANGLE RECT-283
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 23 BY 1.88.

DEFINE RECTANGLE RECT-291
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14.72 BY 5.35.

DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 8 BY .81
     BGCOLOR 8 .

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 8 BY .81
     BGCOLOR 8 .

DEFINE VARIABLE F_HoraFin AS INTEGER FORMAT "Z9":U INITIAL 24 
     VIEW-AS FILL-IN 
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE F_HoraIni AS INTEGER FORMAT "Z9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.43 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FrmGerencia
     BUTTON-120 AT ROW 2.5 COL 49.57
     BUTTON-121 AT ROW 2.5 COL 71
     Cmb_Agencias AT ROW 3.04 COL 1 COLON-ALIGNED NO-LABEL
     W_DiaIni AT ROW 3.15 COL 31.72 NO-LABEL
     AnoIni AT ROW 3.15 COL 44.72 COLON-ALIGNED NO-LABEL
     W_DiaFin AT ROW 3.15 COL 54.29 NO-LABEL
     AnoFin AT ROW 3.15 COL 67.29 COLON-ALIGNED NO-LABEL
     BUTTON-3 AT ROW 5.46 COL 62.72
     Cmb_Informes AT ROW 5.85 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     BUTTON-2 AT ROW 7.04 COL 62.72
     BtnDone AT ROW 8.65 COL 62.86
     Msaje AT ROW 12.04 COL 20 NO-LABEL WIDGET-ID 36
     "Fecha Inicial" VIEW-AS TEXT
          SIZE 14.72 BY .69 AT ROW 2.35 COL 32
          BGCOLOR 17 FGCOLOR 7 
     "Si desea consolidadoTotal no ingrese Fechas Inicial y Final" VIEW-AS TEXT
          SIZE 56 BY .81 AT ROW 1.27 COL 21 WIDGET-ID 6
          FGCOLOR 7 
     "Lista de Informes" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 5.04 COL 3 WIDGET-ID 12
     "Fecha de Corte" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 2.38 COL 54.72
          BGCOLOR 17 FGCOLOR 7 
     "Agencias:" VIEW-AS TEXT
          SIZE 18 BY .81 AT ROW 2.23 COL 3
     Img_MesF AT ROW 3.15 COL 58.29
     Img_MesI AT ROW 3.15 COL 35.72
     RECT-282 AT ROW 2.23 COL 31
     RECT-283 AT ROW 2.23 COL 53.29
     RECT-291 AT ROW 5.15 COL 61.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82.14 BY 12.54
         BGCOLOR 17 .

DEFINE FRAME FrmHoras
     BtnOK AT ROW 1.54 COL 37 WIDGET-ID 36
     F_HoraIni AT ROW 2.35 COL 1.57 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     F_HoraFin AT ROW 2.35 COL 15 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     BtnCancel AT ROW 2.62 COL 37 WIDGET-ID 38
     "Hora Final" VIEW-AS TEXT
          SIZE 13.43 BY .54 AT ROW 1.54 COL 17 WIDGET-ID 26
     "Hora Inicial" VIEW-AS TEXT
          SIZE 12.57 BY .54 AT ROW 1.54 COL 4 WIDGET-ID 18
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 7.46
         SIZE 49 BY 4.04
         BGCOLOR 17 
         TITLE BGCOLOR 17 "Horario"
         DEFAULT-BUTTON BtnOK CANCEL-BUTTON BtnCancel WIDGET-ID 100.


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
  CREATE WINDOW Wwin ASSIGN
         HIDDEN             = YES
         TITLE              = "Informes Comercial - Winformes_Comercial.r"
         COLUMN             = 18.86
         ROW                = 5.12
         HEIGHT             = 12.73
         WIDTH              = 77.14
         MAX-HEIGHT         = 27.65
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 27.65
         VIRTUAL-WIDTH      = 146.29
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = 17
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
/* SETTINGS FOR WINDOW Wwin
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FrmHoras:FRAME = FRAME FrmGerencia:HANDLE.

/* SETTINGS FOR FRAME FrmGerencia
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN AnoFin IN FRAME FrmGerencia
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AnoIni IN FRAME FrmGerencia
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_Agencias IN FRAME FrmGerencia
   1                                                                    */
/* SETTINGS FOR COMBO-BOX Cmb_Informes IN FRAME FrmGerencia
   1                                                                    */
/* SETTINGS FOR FILL-IN Msaje IN FRAME FrmGerencia
   NO-ENABLE ALIGN-L 2                                                  */
/* SETTINGS FOR FILL-IN W_DiaFin IN FRAME FrmGerencia
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_DiaIni IN FRAME FrmGerencia
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FRAME FrmHoras
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Wwin)
THEN Wwin:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON END-ERROR OF Wwin /* Informes Comercial - Winformes_Comercial.r */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON WINDOW-CLOSE OF Wwin /* Informes Comercial - Winformes_Comercial.r */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FrmHoras
&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel Wwin
ON CHOOSE OF BtnCancel IN FRAME FrmHoras /* Cancel */
DO:
 ASSIGN F_HoraIni = 0  F_HoraFin = 24.
 ASSIGN FRAME FrmGerencia:SENSITIVE = YES. /*HIDDEN*/
 ASSIGN FRAME FrmHoras:HIDDEN = YES.
 APPLY "Button-2".
 /*APPLY "CLOSE":U TO THIS-PROCEDURE.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FrmGerencia
&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone Wwin
ON CHOOSE OF BtnDone IN FRAME FrmGerencia /* Salir */
DO:
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


&Scoped-define FRAME-NAME FrmHoras
&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK Wwin
ON CHOOSE OF BtnOK IN FRAME FrmHoras /* OK */
DO:
  ASSIGN F_HoraIni F_HoraFin.
  IF (F_HoraIni GE F_HoraFin) OR F_HoraFin EQ 0 THEN DO:
     MESSAGE "La Hora Inicial Debe ser Menor que la Hora Final"
             "ó la Hora Final Debe ser Mayor que Cero..."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN NO-APPLY. /* "ENTRY".*/
     APPLY "ENTRY" TO F_HoraIni.
    END.
  ELSE DO:
     ASSIGN FRAME FrmGerencia:SENSITIVE = YES. /*HIDDEN*/
     ASSIGN FRAME FrmHoras:HIDDEN = YES.
     APPLY "Button-2".
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FrmGerencia
&Scoped-define SELF-NAME BUTTON-120
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-120 Wwin
ON CHOOSE OF BUTTON-120 IN FRAME FrmGerencia /* Button 120 */
DO:
  RUN C-Fecha.r (OUTPUT WImagen, OUTPUT WAno, OUTPUT WMes, OUTPUT WFec).
  ASSIGN W_OK = Img_MesI:LOAD-IMAGE("imagenes\month" + STRING(WMes) + ".gif")
      AnoIni:SCREEN-VALUE = STRING(WAno)
      W_DiaIni:SCREEN-VALUE = STRING(DAY(WFec))
      AnoIni = WAno
      MesIni = WMes
      FecIni = WFec.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-121
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-121 Wwin
ON CHOOSE OF BUTTON-121 IN FRAME FrmGerencia /* Button 121 */
DO:
  RUN C-Fecha.r (OUTPUT WImagen, OUTPUT WAno, OUTPUT WMes, OUTPUT WFec).
  ASSIGN W_OK = Img_MesF:LOAD-IMAGE("imagenes\month" + STRING(WMes) + ".gif")
         W_DiaFin:SCREEN-VALUE = STRING(DAY(WFec))
         AnoFin:SCREEN-VALUE = STRING(WAno)
         AnoFin = WAno
         MesFin = WMes
         FecFin = WFec.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 Wwin
ON CHOOSE OF BUTTON-2 IN FRAME FrmGerencia /* Button 2 */
DO:
  Msaje:HIDDEN IN FRAME FrmGerencia = NO.  
  VIEW FRAME f_progreso.
  IF ((fecini = ? ) OR (fecfin = ?)) THEN 
      IF Cmb_Informes:SCREEN-VALUE = "9" THEN
         ASSIGN fecini = TODAY - 15 fecfin = TODAY.
      ELSE
         ASSIGN fecini = TODAY - 36000 fecfin = TODAY.
  ASSIGN Cmb_Informes.
  CASE Cmb_Informes:SCREEN-VALUE:
      WHEN "1"  THEN RUN InfPdtosAge.
      WHEN "2"  THEN RUN InfPdtosAgeAso.
      WHEN "3"  THEN RUN infColocacion.
      WHEN "4"  THEN RUN infCaptaciones.
      WHEN "5"  THEN RUN infDemografica.
      WHEN "6"  THEN RUN infAperturas.
      WHEN "7"  THEN RUN infCancelaciones.
      WHEN "8"  THEN RUN infTransacciones.
      WHEN "9"  THEN RUN infTransTaquilla.
/*       WHEN "10" THEN RUN InfAsoSinPdto.  */
  END CASE.
  HIDE FRAME f_progreso.
  IF Listado EQ "" THEN 
     MESSAGE "No se ha ejecutado ningún informe" VIEW-AS ALERT-BOX.
  ELSE
  DO:
    DEFINE VAR W_Dispositivo AS CHARACTER INITIAL " ".
    DEFINE VAR W_sw          AS LOGICAL. 
    RUN P-DisPos IN W_Manija (INPUT-OUTPUT Listado,INPUT-OUTPUT W_Dispositivo).
    IF W_Dispositivo = "" THEN
      RETURN.
    IF W_Dispositivo = "P" THEN  
      RUN Pantalla IN W_Manija (INPUT Listado).
    ELSE                                                  
      IF W_Dispositivo = "I" THEN
          RUN adecomm/_osprint.r ( INPUT  ?, INPUT Listado,INPUT  ?,INPUT  1,INPUT  1,
                                          INPUT  99999,OUTPUT W_sw).
    IF W_Dispositivo = "E" THEN
      RUN Imprimir_Excel.
  END.
  Msaje:SCREEN-VALUE IN FRAME FrmGerencia = "                                   ".
  Msaje:HIDDEN IN FRAME FrmGerencia = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 Wwin
ON CHOOSE OF BUTTON-3 IN FRAME FrmGerencia /* Button 3 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Agencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencias Wwin
ON VALUE-CHANGED OF Cmb_Agencias IN FRAME FrmGerencia
DO:
  ASSIGN FRAME FrmGerencia Cmb_Agencias.
  IF SUBSTRING(Cmb_Agencias,1,3) EQ "000" THEN
     ASSIGN AgeIni = 0 AgeFin = 999.
  ELSE
     ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencias,1,3)) AgeFin = INTEGER(SUBSTRING(Cmb_Agencias,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Informes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Informes Wwin
ON VALUE-CHANGED OF Cmb_Informes IN FRAME FrmGerencia
DO:
    ASSIGN Cmb_Informes.
    IF Cmb_Informes:SCREEN-VALUE EQ "9" THEN DO:
/*         Buscar:SCREEN-VALUE IN FRAME FrmHoras = E1:SELECTION-TEXT. */
/*         ASSIGN FRAME F_Buscar Buscar.                              */
/*         ASSIGN FRAME FrmGerencia:SENSITIVE = NO. /*HIDDEN*/        */
        VIEW FRAME FrmHoras.
        APPLY "ENTRY" TO F_HoraIni.
      END.
    ELSE DO:
        ASSIGN FRAME FrmGerencia:SENSITIVE = YES. /*HIDDEN*/
        ASSIGN FRAME FrmHoras:HIDDEN = YES.   /*VIEW FRAME Frm_Horas.*/
    END.
/*         ENABLE F_HoraIni WITH FRAME FrmGerencia.    */
/*         ENABLE F_HoraFin WITH FRAME FrmGerencia.    */
/*         F_HoraIni:HIDDEN IN FRAME FrmGerencia = NO. */
/*         F_HoraFin:HIDDEN IN FRAME FrmGerencia = NO. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FrmHoras
&Scoped-define SELF-NAME F_HoraFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_HoraFin Wwin
ON LEAVE OF F_HoraFin IN FRAME FrmHoras
DO:
 ASSIGN F_HoraIni F_HoraFin.
 IF F_HoraFin GT 24 THEN DO:
    MESSAGE "La Hora debe ser menor a 24"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY "ENTRY" TO F_HoraFin.
 END.
 ELSE 
  IF(F_HoraFin LE F_HoraIni) OR F_HoraFin EQ 0 THEN DO:
    MESSAGE "La Hora Final Debe ser Mayor que la Hora Inicial"
            "ó la Hora Final Debe ser Mayor que Cero..."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY "ENTRY" TO F_HoraFin.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_HoraIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_HoraIni Wwin
ON LEAVE OF F_HoraIni IN FRAME FrmHoras
DO:
 ASSIGN F_HoraIni F_HoraFin.
 IF F_HoraIni GT 23 THEN DO:
    MESSAGE "La Hora debe ser menor a 23"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY "ENTRY" TO F_HoraIni.
 END.
 IF F_HoraIni GE F_HoraFin THEN DO:
    MESSAGE "La Hora Inicial Debe ser Menor que la Hora Final"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY "ENTRY" TO F_HoraIni.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FrmGerencia
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Wwin 


/* ***************************  Main Block  *************************** */
/* ******************************************************************** 
   Modifica: AGordon                Fecha: 04-abr-2008
   Descripcion: Se Modifica el Procedimiento InfPdtosAge asignando a 
                la variable Listado los valores de nombre de la base 
                de datos, fecha y hora del Pcs.
   ******************************************************************** */

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

  /* Al inicio del  programa y una sola vez */
  HIDE FRAME FrmHoras.
  Msaje:HIDDEN IN FRAME FrmGerencia = YES.
/*   DISABLE F_HoraIni WITH FRAME FrmGerencia.    */
/*   DISABLE F_HoraFin WITH FRAME FrmGerencia.    */
/*   F_HoraIni:HIDDEN IN FRAME FrmGerencia = YES. */
/*   F_HoraFin:HIDDEN IN FRAME FrmGerencia = YES. */
  
  DO WITH FRAME FrmGerencia:     
     FOR EACH Agencias WHERE Agencias.Estado EQ 1 NO-LOCK:
        W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
        IF Agencias.Agencia EQ W_Agencia THEN
           Cmb_Agencias:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
     END.
  END.



  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Wwin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Wwin)
  THEN DELETE WIDGET Wwin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Wwin  _DEFAULT-ENABLE
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
  DISPLAY Cmb_Agencias W_DiaIni AnoIni W_DiaFin AnoFin Cmb_Informes Msaje 
      WITH FRAME FrmGerencia IN WINDOW Wwin.
  ENABLE Img_MesF Img_MesI RECT-282 RECT-283 RECT-291 BUTTON-120 BUTTON-121 
         Cmb_Agencias BUTTON-3 Cmb_Informes BUTTON-2 BtnDone 
      WITH FRAME FrmGerencia IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-FrmGerencia}
  DISPLAY F_HoraIni F_HoraFin 
      WITH FRAME FrmHoras IN WINDOW Wwin.
  ENABLE BtnOK F_HoraIni F_HoraFin BtnCancel 
      WITH FRAME FrmHoras IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-FrmHoras}
  VIEW Wwin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfAperturas Wwin 
PROCEDURE InfAperturas :
/*------------------------------------------------------------------------------   
   PROPOSITO  : Reporte de Aperturas en Ahorros y Créditos X Usuario
   AUTOR      : FÉLIX VARGAS CASTAÑO
   CREACIÓN   : 23 Agosto de 2007
   ACTAULIZADO: 23 Agosto de 2007     
------------------------------------------------------------------------------*/  
RUN _SetCurs.r ("WAIT").
Listado = W_PathSpl + "InfAperturas-" + W_Usuario + ".csv".
OS-DELETE VALUE(Listado).
OUTPUT TO value(Listado). /* NO-ECHO PAGED PAGE-SIZE 65 */
{Incluido\RepEncabezado.i}

PUT "Producto;Periodo;Agencia;Usuario;Grupo;Producto;Subproducto;Tot_Operacion;Tot_Monto;Tot_Asociados" SKIP(0).
ASSIGN wperiodo  = STRING(fecini) + " Al " + STRING(fecfin).

ASSIGN wtotcompc = 0  wmonto = 0.00 wtotaso = 0.
FOR EACH ahorros WHERE 
    ahorros.agencia      GE ageini AND ahorros.agencia      LE agefin AND    
    ahorros.fec_apertura GE fecini AND ahorros.fec_apertura LE fecfin AND
    ahorros.estado EQ 1  AND ahorros.sdo_canje + ahorros.sdo_disponible GT 0 /*ahorros.monto_apertura NE 0*/
    NO-LOCK BREAK BY ahorros.agencia 
    BY ahorros.usu_creacion BY ahorros.tip_ahorro BY ahorros.cod_ahorro BY ahorros.nit:

    Msaje:SCREEN-VALUE IN FRAME FrmGerencia = ".....Procesando Agencia : " + string(ahorros.agencia).
    ASSIGN wagei    = ahorros.agencia
           wusuario = ahorros.usu_creacion
           wcodpro  = ahorros.cod_ahorro
           wtippro  = ahorros.tip_ahorro.

    IF FIRST-OF (ahorros.agencia) THEN
     DO:
       ASSIGN wtotcompa = 0
              wnomage   = "NO EXISTE".
       FIND FIRST agencias WHERE agencias.agencia = wagei NO-LOCK NO-ERROR.
       IF AVAILABLE(agencias)THEN ASSIGN wnomage = agencias.nombre. 
     END.

    IF FIRST-OF (ahorros.agencia) OR FIRST-OF (ahorros.usu_creacion) THEN 
     DO:
       ASSIGN wtotcompu = 0
              wnomusu   = "NO EXISTE"
              wnomgru   = "NO EXISTE".
       FIND FIRST usuarios WHERE usuarios.usuario = wusuario NO-LOCK NO-ERROR.
/*            FIND FIRST usuarios WHERE usuarios.agencia = wagei AND usuarios.usuario = wusuario NO-LOCK NO-ERROR.  */
       IF AVAILABLE(usuarios)THEN 
          ASSIGN wnomusu = usuarios.nombre
                 wgrupo  = usuarios.grupo.
       FIND FIRST grupos WHERE grupos.grupo = wgrupo NO-LOCK NO-ERROR.
       IF AVAILABLE(grupos)THEN ASSIGN wnomgru = grupos.nombre.
     END.

    IF FIRST-OF (ahorros.agencia) OR FIRST-OF (ahorros.usu_creacion) OR FIRST-OF (ahorros.tip_ahorro)THEN 
       ASSIGN  wtotcompoa = 0.
    IF FIRST-OF (ahorros.agencia) OR FIRST-OF (ahorros.usu_creacion) OR FIRST-OF (ahorros.tip_ahorro) OR FIRST-OF (ahorros.cod_ahorro)THEN 
     DO:
       ASSIGN  wtotcompo = 0 wtotcompc = 0
               wmonto = 0
               wnompro = "NO EXISTE".
       FIND FIRST pro_ahorros WHERE pro_ahorros.tip_ahorro = wtippro AND pro_ahorros.cod_ahorro = wcodpro NO-LOCK NO-ERROR.
       IF AVAILABLE(pro_ahorros)THEN 
        DO:
          ASSIGN wnompro = pro_ahorros.nom_producto.
          CASE pro_ahorros.tip_ahorro:
               WHEN 1 THEN ASSIGN wnomtipro = "1- A La Vista".
               WHEN 2 THEN ASSIGN wnomtipro = "2- Contractual".
               WHEN 3 THEN ASSIGN wnomtipro = "3- A Termino".
               WHEN 4 THEN ASSIGN wnomtipro = "4- Aportes".
               OTHERWISE 
                   ASSIGN wnomtipro = "0-NO EXISTE".
          END CASE.
        END.
     END.
    IF (ahorros.sdo_canje + ahorros.sdo_disponible) GT 0 THEN
       ASSIGN wtotcompc = wtotcompc + 1
              wmonto  = wmonto + Monto_Apertura.
    /* Asociados que tiene Ahorros */
    IF FIRST-OF (ahorros.agencia) OR FIRST-OF (ahorros.usu_creacion) OR FIRST-OF (ahorros.tip_ahorro) OR FIRST-OF (ahorros.cod_ahorro) OR FIRST-OF (ahorros.nit) THEN
        ASSIGN wtotaso = wtotaso + 1.
     /*************************************/
    IF LAST-OF (ahorros.agencia) OR LAST-OF (ahorros.usu_creacion) OR LAST-OF (ahorros.tip_ahorro) OR LAST-OF (ahorros.cod_ahorro) THEN
       DO:
         ASSIGN wtotcompo  = wtotcompo + wtotcompc
                wtotmonto  = wtotmonto + wmonto
                wtotasog   = wtotasog  + wtotaso.

         IF wagei = 0 THEN
            ASSIGN wagenc1 = wnomage.
         ELSE
            ASSIGN wagenc1 = STRING(wagei)    + "-" + wnomage.
         IF wusuario = "" THEN
            ASSIGN wusuar1 = wnomusu.
         ELSE
            ASSIGN wusuar1 = STRING(wusuario) + "-" + wnomusu.
         IF wgrupo = 0 THEN
            ASSIGN wgrupo1 = wnomgru.
         ELSE
            ASSIGN wgrupo1 = STRING(wgrupo) + "-" + wnomgru.
         IF wcodpro = 0 THEN
            ASSIGN wprodu1 = wnompro.
         ELSE
            ASSIGN wprodu1 = STRING(wcodpro) + "-" + wnompro.
/*          IF (wgrupo NE 1 AND wgrupo NE 2 AND wgrupo NE 16) AND wnomusu NE "NO EXISTE" THEN  */
             PUT "Ahorros" ";"
                 wperiodo  ";"
                 wagenc1   ";"
                 wusuar1   ";"
                 wgrupo1   ";"
                 wnomtipro ";"
                 wprodu1   ";"
                 wtotcompc ";"
                 wmonto    FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" ";"
                 wtotaso   SKIP(0).
         ASSIGN wtotaso = 0.
       END.    
END.

ASSIGN wtotcompc = 0  wmonto = 0.00 wtotaso = 0.
FOR EACH creditos WHERE
    creditos.agencia        GE ageini AND creditos.agencia        LE agefin AND    
    creditos.fec_desembolso GE fecini AND creditos.fec_desembolso LE fecfin AND
    creditos.estado EQ 2    AND creditos.sdo_capital GT 0 /*creditos.monto NE 0*/
    NO-LOCK BREAK BY creditos.agencia BY creditos.usuario BY creditos.tip_credito BY creditos.cod_credito BY creditos.nit:

    ASSIGN wagei    = creditos.agencia
           wusuario = creditos.usuario
           wcodpro  = creditos.cod_credito
           wtippro  = creditos.tip_credito.
    
    IF FIRST-OF (creditos.agencia) THEN
     DO:
       ASSIGN wtotcompa = 0
              wnomage   = "NO EXISTE".
       FIND FIRST agencias WHERE agencias.agencia = wagei NO-LOCK NO-ERROR.
       IF AVAILABLE(agencias)THEN ASSIGN wnomage = agencias.nombre. 
     END.

    IF FIRST-OF (creditos.agencia) OR FIRST-OF (creditos.usuario) THEN 
     DO:
       ASSIGN wtotcompu = 0
              wnomusu   = "NO EXISTE"
              wnomgru   = "NO EXISTE".
       FIND FIRST usuarios WHERE usuarios.usuario = wusuario NO-LOCK NO-ERROR.
/*            FIND FIRST usuarios WHERE usuarios.agencia = wagei AND usuarios.usuario = wusuario NO-LOCK NO-ERROR.  */       
       IF AVAILABLE(usuarios)THEN 
          ASSIGN wnomusu = usuarios.nombre
                 wgrupo  = usuarios.grupo.
       FIND FIRST grupos WHERE grupos.grupo = wgrupo NO-LOCK NO-ERROR.
       IF AVAILABLE(grupos)THEN ASSIGN wnomgru = grupos.nombre.
     END.

    IF FIRST-OF (creditos.agencia) OR FIRST-OF (creditos.usuario) OR FIRST-OF (creditos.tip_credito)THEN 
       ASSIGN  wtotcompoa = 0.
    IF FIRST-OF (creditos.agencia) OR FIRST-OF (creditos.usuario) OR FIRST-OF (creditos.tip_credito) OR FIRST-OF (creditos.cod_credito)THEN 
     DO:
       ASSIGN  wtotcompo = 0 wtotcompc = 0
               wmonto = 0
               wnompro = "NO EXISTE".
       FIND FIRST pro_creditos WHERE pro_creditos.tip_credito = wtippro AND pro_creditos.cod_credito = wcodpro NO-LOCK NO-ERROR.
       IF AVAILABLE(pro_creditos)THEN 
        DO:
           ASSIGN wnompro = pro_creditos.nom_producto.
           CASE pro_creditos.tip_credito:
                WHEN 1 THEN wnomtipro = "1- Consumo".
                WHEN 2 THEN wnomtipro = "2- Comercial".
                WHEN 3 THEN wnomtipro = "3- Hipotecario".
                WHEN 4 THEN wnomtipro = "4- Microcredito".
                WHEN 5 THEN wnomtipro = "5- Bienes y Serv.".
                WHEN 6 THEN wnomtipro = "6- Empleados".
                WHEN 7 THEN wnomtipro = "7- Convenios".
                OTHERWISE 
                    ASSIGN wnomtipro = "0-NO EXISTE".
           END CASE.
        END.
    END.

    IF creditos.sdo_capital GT 0 THEN
       ASSIGN wtotcompc = wtotcompc + 1.
              wmonto  = wmonto + creditos.Monto.
    /* Asociados que tiene Creditos */
    IF FIRST-OF (creditos.agencia) OR FIRST-OF (creditos.usuario) OR FIRST-OF (creditos.tip_credito) OR FIRST-OF (creditos.cod_credito) OR FIRST-OF (creditos.nit) THEN
        ASSIGN wtotaso = wtotaso + 1.
     /*************************************/
    IF LAST-OF (creditos.agencia) OR LAST-OF (creditos.usuario) OR LAST-OF (creditos.tip_credito) OR LAST-OF (creditos.cod_credito) THEN
       DO:
         ASSIGN wtotcompo  = wtotcompo + wtotcompc
                wtotmonto  = wtotmonto + wmonto
                wtotasog   = wtotasog  + wtotaso.

         IF wagei = 0 THEN
            ASSIGN wagenc1 = wnomage.
         ELSE
            ASSIGN wagenc1 = STRING(wagei)    + "-" + wnomage.
         IF wusuario = "" THEN
            ASSIGN wusuar1 = wnomusu.
         ELSE
            ASSIGN wusuar1 = STRING(wusuario) + "-" + wnomusu.
         IF wgrupo = 0 THEN
            ASSIGN wgrupo1 = wnomgru.
         ELSE
            ASSIGN wgrupo1 = STRING(wgrupo) + "-" + wnomgru.
         IF wcodpro = 0 THEN
            ASSIGN wprodu1 = wnompro.
         ELSE
            ASSIGN wprodu1 = STRING(wcodpro) + "-" + wnompro.
/*          IF (wgrupo NE 1 AND wgrupo NE 2 AND wgrupo NE 16) AND wnomusu NE "NO EXISTE" THEN  */
             PUT "Creditos" ";"
                 wperiodo  ";"
                 wagenc1   ";"
                 wusuar1   ";"
                 wgrupo1   ";"
                 wnomtipro ";"
                 wprodu1   ";"
                 wtotcompc ";" 
                 wmonto    FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" ";"
                 wtotaso   SKIP(0).
         ASSIGN wtotaso = 0.
       END.    
END.
OUTPUT CLOSE.
MESSAGE "El archivo plano esta en " listado VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfAsoSinPdto Wwin 
PROCEDURE InfAsoSinPdto :
/*------------------------------------------------------------------------------   
   PROPOSITO  : Reporte de Asociados Sin Productos
   AUTOR      : FÉLIX VARGAS CASTAÑO
   CREACIÓN   : 27 Agosto de 2007
   ACTAULIZADO: 27 Agosto de 2007     
------------------------------------------------------------------------------*/  
RUN _SetCurs.r ("WAIT").
Listado = W_PathSpl + "InfAsoSinPdto-" + W_Usuario + ".csv".
OS-DELETE VALUE(Listado).
OUTPUT TO value(Listado). /* NO-ECHO PAGED PAGE-SIZE 65 */
{Incluido\RepEncabezado.i}

PUT "agencia;nit;nombres;empresa;tipo_contrato;estado;fec_ingreso;fec_retiro;salario;aportes;dir_comercial;tel_comercial" SKIP(0).
FOR EACH clientes WHERE
    clientes.agencia GE ageini AND clientes.agencia LE agefin AND
    tipo_vinculo = 1 
    NO-LOCK BREAK BY Clientes.agencia BY Clientes.nit:

    Msaje:SCREEN-VALUE IN FRAME FrmGerencia = ".....Procesando Agencia : " + string(clientes.agencia).
    ASSIGN wnomagecli="NO EXISTE"
           wagencli=Clientes.agencia
           wniti=Clientes.nit
           wcodemp=clientes.cod_empresa
           wnomemp="NO EXISTE"
           wnomcli=trim(clientes.nombre) + " " + trim(clientes.apellido1) + " " + trim(clientes.apellido2)
           wsalcli=clientes.Salario
           wtotahoapo= 0.00.
    CASE clientes.tip_contrato:
         WHEN 0 THEN ASSIGN wcontrato = "0- Ninguno".
         WHEN 1 THEN ASSIGN wcontrato = "1- Indefinido".
         WHEN 2 THEN ASSIGN wcontrato = "2- Fijo".
         WHEN 3 THEN ASSIGN wcontrato = "3- Labor Contratada".
         WHEN 4 THEN ASSIGN wcontrato = "4- Prestación de Servicios".
         OTHERWISE 
             ASSIGN wcontrato = "No Asignado".
    END CASE.
         
    CASE clientes.estado:
         WHEN 1 THEN ASSIGN wnomestado = "1- Activo".
         WHEN 2 THEN ASSIGN wnomestado = "2- Inactivo".
         OTHERWISE 
             ASSIGN wnomestado = "No Asignado".
    END CASE.

   FIND FIRST agencia WHERE Agencias.Agencia=wagencli NO-LOCK NO-ERROR.
   IF AVAILABLE(agencia)THEN ASSIGN wnomagecli=agencia.nombre.
   FIND FIRST empresas WHERE empresas.cod_empresa=wcodemp NO-LOCK NO-ERROR.
   IF AVAILABLE(empresas)THEN ASSIGN wnomemp=empresas.alias_empresa.
   ASSIGN widempre = STRING(wcodemp) + "-" + wnomemp.
   
   /* Tabla de Ahorros **
    Cod. 005 - Aportes Obligatorios , 010 - Aportes Voluntarios , 015 - Aportes Extraordinarios , 216 - Remanentes de Pagaduría */ 
   ASSIGN Wtieahor = 0.
   FOR EACH ahorros WHERE ahorros.nit=wniti AND (tip_ahorro lt 4 )
       AND sdo_disponible NE 0 NO-LOCK:
       /*IF ((cod_ahorro NE 005) AND (cod_ahorro NE 010) AND (cod_ahorro NE 015) AND (cod_ahorro NE 216)) THEN*/
          ASSIGN Wtieahor = 1.
   END.

   /* Solo Aportes */
   FOR EACH ahorros WHERE ahorros.nit=wniti AND (tip_ahorro = 4) and
                          sdo_disponible NE 0 NO-LOCK:
       ASSIGN wtotahoapo = wtotahoapo + sdo_disponible.
   END.
   
   /* Tabla de Créditos **
    Cod. 569 - Cuota de Afiliacion , 545 - Fdo. Mutual Funarario , 546 - Fdo. Mutual Educativo , 570 - Credito Rotativo*/ 
   ASSIGN Wtiecre = 0.
   FOR EACH creditos WHERE creditos.nit=wniti AND sdo_capital NE 0 NO-LOCK:
       ASSIGN Wtiecre = 1.
   END.
   IF wagencli = 0 THEN
      ASSIGN wagencli1 = wnomagecli.
   ELSE
      ASSIGN wagencli1 = STRING(wagencli)    + "-" + wnomagecli.
  
   IF Wtieahor = 0 AND Wtiecre = 0 THEN
      PUT wagencli1 ";"
          wniti   ";"
          wnomcli ";" 
          widempre ";"
          wcontrato ";"          
          wnomestado ";"
          clientes.fec_ingreso ";"
          clientes.fec_retiro ";"
          wsalcli ";"
          wtotahoapo ";"
          Clientes.Dir_comercial ";"
          Clientes.Tel_comercial SKIP(0).
END.
OUTPUT CLOSE.
MESSAGE "El archivo plano esta en " listado VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfCancelaciones Wwin 
PROCEDURE InfCancelaciones :
/*------------------------------------------------------------------------------   
   PROPOSITO  : Reporte de Aperturas en Ahorros y Créditos X Usuario
   AUTOR      : FÉLIX VARGAS CASTAÑO
   CREACIÓN   : 23 Agosto de 2007
   ACTAULIZADO: 23 Agosto de 2007     
------------------------------------------------------------------------------*/  
RUN _SetCurs.r ("WAIT").
Listado = W_PathSpl + "InfCancelaciones-" + W_Usuario + ".csv".
OS-DELETE VALUE(Listado).
OUTPUT TO value(Listado). /* NO-ECHO PAGED PAGE-SIZE 65 */
{Incluido\RepEncabezado.i}

PUT "Producto;Periodo;Agencia;Usuario;Grupo;Producto;Subproducto;Tot_Operacion;Tot_Monto;Tot_Asociados" SKIP(0).
ASSIGN wperiodo  = STRING(fecini) + " Al " + STRING(fecfin).
FOR EACH ahorros WHERE 
    ahorros.agencia         GE ageini AND ahorros.agencia         LE agefin AND    
    ahorros.Fec_Cancelacion GE fecini AND ahorros.Fec_Cancelacion LE fecfin AND
    ahorros.estado EQ 2 AND ahorros.sdo_disponible + ahorros.sdo_canje EQ 0 NO-LOCK : 
    
    Msaje:SCREEN-VALUE IN FRAME FrmGerencia = ".....Procesando Agencia : " + string(ahorros.agencia).
    ASSIGN wagei    = ahorros.agencia
           wcodpro  = ahorros.cod_ahorro
           wtippro  = ahorros.tip_ahorro.
    FIND LAST mov_ahorros WHERE /*Cod. 010101001 - Consignacion Ahorros Efectivo . 010101002 - Consignacion Ahorros Cheque*/
         (mov_ahorros.cod_operacion EQ 010101001             OR
          mov_ahorros.cod_operacion EQ 010101002)            AND
         mov_ahorros.fecha        LE ahorros.fec_cancelacion AND
         mov_ahorros.cod_ahorro   EQ ahorros.cod_ahorro      AND
         (mov_ahorros.cue_ahorros EQ ahorros.cue_ahorros     OR 
          mov_ahorros.cue_ahorros EQ ahorros.num_formato)    AND
         mov_ahorros.agencia      EQ ahorros.agencia         AND
         mov_ahorros.nit = ahorros.nit 
         NO-LOCK NO-ERROR.
    IF NOT AVAILABLE mov_ahorros THEN NEXT.
    ELSE 
     DO:
       ASSIGN wusuario  = mov_ahorros.usuario
              wnomusu   = "NO EXISTE"
              wnomgru   = "NO EXISTE"
              wnomage   = "NO EXISTE"
              wnompro   = "NO EXISTE".
       FIND FIRST agencias WHERE agencias.agencia = wagei NO-LOCK NO-ERROR.
       IF AVAILABLE(agencias)THEN ASSIGN wnomage = agencias.nombre. 
       FIND FIRST usuarios WHERE usuarios.usuario = wusuario NO-LOCK NO-ERROR.
/*            FIND FIRST usuarios WHERE usuarios.agencia = wagei AND usuarios.usuario = wusuario NO-LOCK NO-ERROR.  */
       IF AVAILABLE(usuarios)THEN 
          ASSIGN wnomusu = usuarios.nombre
                 wgrupo  = usuarios.grupo.
       FIND FIRST grupos WHERE grupos.grupo = wgrupo NO-LOCK NO-ERROR.
       IF AVAILABLE(grupos)THEN ASSIGN wnomgru = grupos.nombre.
       FIND FIRST pro_ahorros WHERE pro_ahorros.tip_ahorro = wtippro AND pro_ahorros.cod_ahorro = wcodpro NO-LOCK NO-ERROR.
       IF AVAILABLE(pro_ahorros)THEN 
       DO:
          ASSIGN wnompro = pro_ahorros.nom_producto.
          CASE pro_ahorros.tip_ahorro:
               WHEN 1 THEN ASSIGN wnomtipro = "1- A La Vista".
               WHEN 2 THEN ASSIGN wnomtipro = "2- Contractual".
               WHEN 3 THEN ASSIGN wnomtipro = "3- A Termino".
               WHEN 4 THEN ASSIGN wnomtipro = "4- Aportes".
               OTHERWISE 
                   ASSIGN wnomtipro = "0-NO EXISTE".
          END CASE.
        END.
       IF wagei = 0 THEN
          ASSIGN wagenc1 = wnomage.
       ELSE
          ASSIGN wagenc1 = STRING(wagei)    + "-" + wnomage.
       IF wusuario = "" THEN
          ASSIGN wusuar1 = "XXX-" + wnomusu.
       ELSE
          ASSIGN wusuar1 = STRING(wusuario) + "-" + wnomusu.
       IF wgrupo = 0 THEN
          ASSIGN wgrupo1 = wnomgru.
       ELSE
          ASSIGN wgrupo1 = STRING(wgrupo) + "-" + wnomgru.
       IF wcodpro = 0 THEN
          ASSIGN wprodu1 = wnompro.
       ELSE
          ASSIGN wprodu1 = STRING(wcodpro) + "-" + wnompro.
/*        IF (wgrupo NE 1 AND wgrupo NE 2 AND wgrupo NE 16) AND wnomusu NE "NO EXISTE" THEN  */
/*          DO:                                                                              */
           CREATE TemCance.
           ASSIGN TTabla    = "Ahorros"
                  TPeriodo  = wperiodo 
                  Tagencia  = wagenc1  
                  TUsuario  = wusuar1  
                  TGrupo    = wgrupo1  
                  TNomtipro = wnomtipro
                  TProducto = wprodu1
                  TNumProdu = mov_ahorros.cue_ahorros
                  TNit      = mov_ahorros.nit
                  TTotcompc = 1
                  TMonto    = mov_ahorros.val_cheque + mov_ahorros.val_efectivo
                  Tasocia   = 1.
/*          END.  */
     END.
END.
/**** Créditos ***/

FOR EACH creditos WHERE
    creditos.agencia        GE ageini AND creditos.agencia        LE agefin AND    
    creditos.Fec_CanceTotal GE fecini AND creditos.Fec_CanceTotal LE fecfin AND
    creditos.estado EQ 3    AND creditos.sdo_capital EQ 0 NO-LOCK :
    ASSIGN wagei    = creditos.agencia
           wcodpro  = creditos.cod_credito
           wtippro  = creditos.tip_credito.
    FIND LAST mov_creditos WHERE  /* Cod. 020101001 - Pago de Capital */
         mov_creditos.cod_operacion = 020101001              AND
         mov_creditos.fecha       LE creditos.fec_CanceTotal AND
         mov_creditos.cod_credito EQ creditos.cod_credito    AND
         mov_creditos.num_credito EQ creditos.num_credito    AND
         mov_creditos.agencia     EQ creditos.agencia        AND
         mov_creditos.nit = creditos.nit 
         NO-LOCK NO-ERROR.
    IF NOT AVAILABLE mov_creditos THEN NEXT.
    ELSE 
     DO:
       ASSIGN wusuario  = mov_creditos.usuario
              wnomusu   = "NO EXISTE"
              wnomgru   = "NO EXISTE"
              wnomage   = "NO EXISTE"
              wnompro   = "NO EXISTE".
       FIND FIRST agencias WHERE agencias.agencia = wagei NO-LOCK NO-ERROR.
       IF AVAILABLE(agencias)THEN ASSIGN wnomage = agencias.nombre. 
       FIND FIRST usuarios WHERE usuarios.usuario = wusuario NO-LOCK NO-ERROR.
/*            FIND FIRST usuarios WHERE usuarios.agencia = wagei AND usuarios.usuario = wusuario NO-LOCK NO-ERROR.  */
       IF AVAILABLE(usuarios)THEN 
          ASSIGN wnomusu = usuarios.nombre
                 wgrupo  = usuarios.grupo.
       FIND FIRST grupos WHERE grupos.grupo = wgrupo NO-LOCK NO-ERROR.
       IF AVAILABLE(grupos)THEN ASSIGN wnomgru = grupos.nombre.
       FIND FIRST pro_creditos WHERE pro_creditos.tip_credito = wtippro AND pro_creditos.cod_credito = wcodpro NO-LOCK NO-ERROR.
       IF AVAILABLE(pro_creditos)THEN 
       DO:
          ASSIGN wnompro = pro_creditos.nom_producto.
          CASE pro_creditos.tip_credito:
               WHEN 1 THEN wnomtipro = "1- Consumo".
               WHEN 2 THEN wnomtipro = "2- Comercial".
               WHEN 3 THEN wnomtipro = "3- Hipotecario".
               WHEN 4 THEN wnomtipro = "4- Microcredito".
               WHEN 5 THEN wnomtipro = "5- Bienes y Serv.".
               WHEN 6 THEN wnomtipro = "6- Empleados".
               WHEN 7 THEN wnomtipro = "7- Convenios".
               OTHERWISE 
                   ASSIGN wnomtipro = "0-NO EXISTE".
          END CASE.
        END.
       IF wagei = 0 THEN
          ASSIGN wagenc1 = wnomage.
       ELSE
          ASSIGN wagenc1 = STRING(wagei)    + "-" + wnomage.
       IF wusuario = "" THEN
          ASSIGN wusuar1 = "XXX-" + wnomusu.
       ELSE
          ASSIGN wusuar1 = STRING(wusuario) + "-" + wnomusu.
       IF wgrupo = 0 THEN
          ASSIGN wgrupo1 = wnomgru.
       ELSE
          ASSIGN wgrupo1 = STRING(wgrupo) + "-" + wnomgru.
       IF wcodpro = 0 THEN
          ASSIGN wprodu1 = wnompro.
       ELSE
          ASSIGN wprodu1 = STRING(wcodpro) + "-" + wnompro.
/*        IF (wgrupo NE 1 AND wgrupo NE 2 AND wgrupo NE 16) AND wnomusu NE "NO EXISTE" THEN  */
/*          DO:                                                                              */
           CREATE TemCance.
           ASSIGN TTabla    = "Creditos"
                  TPeriodo  = wperiodo 
                  Tagencia  = wagenc1  
                  TUsuario  = wusuar1  
                  TGrupo    = wgrupo1  
                  TNomtipro = wnomtipro
                  TProducto = wprodu1
                  TNumProdu = string(mov_creditos.num_credito)
                  TNit      = mov_creditos.nit
                  TTotcompc = 1
                  TMonto    = mov_creditos.val_cheque + mov_creditos.val_efectivo
                  Tasocia   = 1.
/*          END.  */
     END.
END.

FOR EACH TemCance
    BREAK BY Ttabla BY Tagencia BY TUsuario BY TGrupo BY TNomtipro BY TProducto BY TNit:
    IF FIRST-OF (TemCance.Ttabla) OR FIRST-OF (TemCance.Tagencia)  OR FIRST-OF (TemCance.TUsuario) OR
       FIRST-OF (TemCance.TGrupo) OR FIRST-OF (TemCance.TNomtipro) OR FIRST-OF (TemCance.TProducto) THEN 
       ASSIGN wtotcompc = 0
              wmonto    = 0.00
              wtotaso   = 0 
              wtotaso1  = 0.
    ASSIGN wtotcompc = wtotcompc + TemCance.TTotcompc
           wmonto    = wmonto  + TemCance.Tmonto
           wtotaso   = wtotaso + TemCance.Tasocia.

    IF FIRST-OF (TemCance.Ttabla) OR FIRST-OF (TemCance.Tagencia)  OR FIRST-OF (TemCance.TUsuario)  OR
       FIRST-OF (TemCance.TGrupo) OR FIRST-OF (TemCance.TNomtipro) OR FIRST-OF (TemCance.TProducto) OR 
       FIRST-OF (TemCance.TNit) THEN 
       ASSIGN wtotaso1 = wtotaso1 + TemCance.Tasocia.

    IF LAST-OF (TemCance.Ttabla) OR LAST-OF (TemCance.Tagencia)  OR LAST-OF (TemCance.TUsuario)  OR
       LAST-OF (TemCance.TGrupo) OR LAST-OF (TemCance.TNomtipro) OR LAST-OF (TemCance.TProducto) THEN 
       PUT TTabla    ";"
           TPeriodo  ";"
           Tagencia  ";"
           TUsuario  ";"
           TGrupo    ";"
           TNomtipro ";"
           TProducto ";"
           wtotcompc ";"
           wmonto    FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99" ";"
           wtotaso1   SKIP(0).
END.
OUTPUT CLOSE.
FOR EACH TemCance: DELETE TemCance.
END.
MESSAGE "El archivo plano esta en " listado VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfCaptaciones Wwin 
PROCEDURE InfCaptaciones :
/*------------------------------------------------------------------------------   
   PROPOSITO  : Reporte de Captaciones
   AUTOR      :  JOHN  MONCADA PUERTA
   ACTAULIZADO: 9 septiembre de 2005     
------------------------------------------------------------------------------*/  
 RUN _SetCurs.r ("WAIT").
 Listado = W_PathSpl + "InfCaptaciones-" + W_Usuario + ".csv".
 OS-DELETE VALUE(Listado).
 OUTPUT TO value(Listado). /* NO-ECHO PAGED PAGE-SIZE 65 */
 {Incluido\RepEncabezado.i}
/* W_Reporte    = "REPORTE   : Retiro de Asociados -  Agencia: " + TRIM(SUBSTRING(Cmb_Agencias,6,15)) + " Del " + STRING(FecIni,"99/99/9999") + " Al " + STRING(FecFin,"99/99/9999").
 W_EncColumna = "#Numero Nro.Identif  Nombres Completos                     Fec_Ret-Aportes  Saldo Apo".
 VIEW FRAME F-Encabezado.                                                                               */

 DEFINE VAR wconteo  AS INTEGER INITIAL 0.
 DEFINE VAR wperliq  AS CHARACTER FORMAT "X(20)".
 DEFINE VAR wtipoAho AS CHARACTER FORMAT "X(20)".
 PUT "CodAge;NombreAge;TipoDoc;Identif;Nombres;TipAho;NomTipo;CodAhorro;NomProducto;#Cuenta;FecApert;Plazo;Cuota;Tasa;PerLiq;FecVcto;SdoTotal;cod_usuario;nom_usuario" SKIP(0).
 FOR EACH Ahorros WHERE ahorros.estado = 1 AND ( ahorros.sdo_disponible + ahorros.sdo_canje )> 0 AND
                        Ahorros.fec_apertura  GE fecini AND ahorros.fec_apertura LE fecfin   AND 
                        ahorros.agencia       GE ageini AND Ahorros.agencia      LE agefin  
                        NO-LOCK BREAK BY ahorros.agencia:
  
  Msaje:SCREEN-VALUE IN FRAME FrmGerencia = ".....Procesando Agencia : " + string(ahorros.agencia).
  IF FIRST-OF(ahorros.agencia) THEN DO:
     FIND FIRST agencias WHERE agencias.agencia = ahorros.agencia NO-LOCK NO-ERROR.
  END.
  PUT ahorros.agencia FORMAT "999" ";" CAPS(TRIM(agencias.nombre)) FORMAT "X(15)".
  FIND clientes WHERE clientes.nit EQ ahorros.nit  NO-LOCK NO-ERROR.
  IF AVAILABLE(clientes) THEN DO:
      PUT ";" Clientes.Tipo_Identificacion
          ";" clientes.nit  FORMAT "X(12)"  
          ";" trim(clientes.nombre) + " " + trim(clientes.apellido1) + " " + trim(clientes.apellido2)  FORMAT "X(40)".
  END.
  ELSE
      PUT ";;;".

  CASE ahorros.per_liquidacion:
      WHEN 1 THEN wperliq = "Diario".
      WHEN 2 THEN wperliq = "Mensual".
      WHEN 3 THEN wperliq = "Trimestral".
      WHEN 4 THEN wperliq = "Semestral".
      WHEN 5 THEN wperliq = "Anual".
      WHEN 6 THEN wperliq = "Al Vencimiento".
    OTHERWISE wperliq = "Sin PerLiq".
  END CASE.
  CASE ahorros.tip_ahorro: 
      WHEN 1 THEN wtipoaho = 'A la vista'.
      WHEN 2 THEN wtipoaho = 'Contractual'.
      WHEN 3 THEN wtipoaho = 'A termino'.
      WHEN 4 THEN wtipoaho = 'Aportes'.
      OTHERWISE wtipoaho = 'Sin definir'.
  END CASE.
  
  FIND FIRST pro_ahorros WHERE pro_ahorros.cod_ahorro = ahorros.cod_ahorro NO-LOCK NO-ERROR.
  PUT ";" ahorros.Tip_Ahorro FORMAT "z9" ";" wtipoaho
      ";" Ahorros.cod_Ahorro ";" Pro_Ahorros.Nom_Producto ";" Ahorros.cue_ahorros
      ";" ahorros.fec_apertura ";" TRIM(STRING(ahorros.plazo)) 
      ";" ahorros.cuota FORMAT "zzzzzzz9" ";" ahorros.tasa
      ";" TRIM(wperliq) ";" (ahorros.Fec_Vencimiento)
       ";" (ahorros.sdo_disponible + ahorros.sdo_canje) FORMAT "zzzzzzzz9" .
   
  FIND FIRST usuarios WHERE usuarios.usuario = creditos.usuario NO-LOCK NO-ERROR.
  IF AVAILABLE(usuarios) THEN
     PUT ";" Usuarios.Nit ";" Usuarios.Nombre SKIP(0).
  ELSE
     PUT ";;" SKIP(0).
  END.
 OUTPUT CLOSE.
 MESSAGE "El archivo plano esta en " listado
     VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfColocacion Wwin 
PROCEDURE InfColocacion :
/*------------------------------------------------------------------------------   
   PROPOSITO  : Reporte de Captaciones
   AUTOR      :  JOHN  MONCADA PUERTA
   ACTAULIZADO: 9 septiembre de 2005     
------------------------------------------------------------------------------*/  
 RUN _SetCurs.r ("WAIT").
 Listado = W_PathSpl + "InfColocaciones-" + W_Usuario + ".csv".
 OS-DELETE VALUE(Listado).
 OUTPUT TO value(Listado). /* NO-ECHO PAGED PAGE-SIZE 65 */
 {Incluido\RepEncabezado.i}
/* W_Reporte    = "REPORTE   : Retiro de Asociados -  Agencia: " + TRIM(SUBSTRING(Cmb_Agencias,6,15)) + " Del " + STRING(FecIni,"99/99/9999") + " Al " + STRING(FecFin,"99/99/9999").
 W_EncColumna = "#Numero Nro.Identif  Nombres Completos                     Fec_Ret-Aportes  Saldo Apo".
 VIEW FRAME F-Encabezado.                                                                               */

 DEFINE VAR wconteo  AS INTEGER INITIAL 0.
 DEFINE VAR wfactor  AS DECIMAL INITIAL 1.0000.
 DEFINE VAR wtipocre AS CHARACTER FORMAT "X(20)".
 PUT "CodAge;NombreAge;TipoDoc;Identif;Nombres;TipCred;Tipo;CodLinea;NombreLinea;#Credito;FecAprob;VlrCred;VlrDesemb;Fec_Desem;Plazo;Cuota;Tasa;FecVcto;SdoCapital;cod_usuario;nom_usuario" SKIP(0).
 FOR EACH creditos  WHERE creditos.estado = 2 AND creditos.sdo_capital > 0 AND
                        creditos.fec_desembolso  GE fecini AND creditos.fec_desembolso LE fecfin AND 
                        creditos.agencia         GE ageini AND creditos.agencia         LE agefin  
                        NO-LOCK BREAK BY creditos.agencia:
  
  Msaje:SCREEN-VALUE IN FRAME FrmGerencia = ".....Procesando Agencia : " + string(creditos.agencia).
  IF FIRST-OF(creditos.agencia) THEN DO:
     FIND FIRST agencias WHERE agencias.agencia = creditos.agencia NO-LOCK NO-ERROR.
  END.
  PUT creditos.agencia FORMAT "999" ";" CAPS(TRIM(agencias.nombre)) FORMAT "X(15)".
  FIND clientes WHERE clientes.nit EQ creditos.nit  NO-LOCK NO-ERROR.
  IF AVAILABLE(clientes) THEN DO:
      PUT ";" Clientes.Tipo_Identificacion
          ";" clientes.nit  FORMAT "X(12)"  
          ";" trim(clientes.nombre) + " " + trim(clientes.apellido1) + " " + trim(clientes.apellido2)  FORMAT "X(40)".
  END.
  ELSE
      PUT ";;;".

  CASE creditos.per_pago:
      WHEN 1 THEN wfactor = 4.    /* Semanal */
      WHEN 2 THEN wfactor = 3.    /* Decadal */
      WHEN 3 THEN wfactor = 2.    /* Quincenal */
      WHEN 4 THEN wfactor = 1.    /* Mensual */
      WHEN 5 THEN wfactor = round( 1 / 2, 4).  /* Bimestral */
      WHEN 6 THEN wfactor = round( 1 / 4, 4).  /* Cuatrimesntral */
      WHEN 7 THEN wfactor = round( 1 / 6, 4).  /* semestral */
      WHEN 8 THEN wfactor = round( 1 / 12,4). /* Anual */
    OTHERWISE wfactor = 1.
  END CASE.
  CASE creditos.tip_credito:
      WHEN 1 THEN wtipocre = 'Consumo'.
      WHEN 2 THEN wtipocre = 'Comercial'.
      WHEN 3 THEN wtipocre = 'Hipotecario'.
      WHEN 4 THEN wtipocre = 'Microcredito'.
      WHEN 5 THEN wtipocre = 'Bnes y Scios'.
      WHEN 6 THEN wtipocre = 'Empleados'.
      WHEN 7 THEN wtipocre = 'Fondos'.
      OTHERWISE wtipocre = 'Sin definir'.
  END CASE.

  FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.
  PUT ";" Creditos.Tip_Credito FORMAT "z9" ";" wtipocre
      ";" Creditos.cod_credito ";" Pro_Creditos.Nom_Producto ";" creditos.num_credito ";" creditos.fec_aprobacion
      ";" Creditos.Monto FORMAT "zzzzzzzz9" ";" creditos.val_desembolso FORMAT "zzzzzzzz9" ";" creditos.fec_desembolso
      ";" TRIM(STRING((round(creditos.plazo * wfactor,0)))) ";" round(creditos.cuota * wfactor,0) FORMAT "zzzzzzzz9" ";" creditos.tasa
      ";" (Fec_desembolso + ( round(creditos.plazo * wfactor,0) * 30 ) ) ";" creditos.sdo_capital FORMAT "zzzzzzzzzz9".
   
  FIND FIRST usuarios WHERE usuarios.usuario = creditos.usuario NO-LOCK NO-ERROR.
  IF AVAILABLE(usuarios) THEN
     PUT ";" Usuarios.Nit ";" Usuarios.Nombre SKIP(0).
  ELSE
     PUT ";;" SKIP(0).
  END.
 OUTPUT CLOSE.
 MESSAGE "El archivo plano esta en " listado
     VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfDemografica Wwin 
PROCEDURE InfDemografica :
/*------------------------------------------------------------------------------   
   PROPOSITO  : Reporte de Captaciones
   AUTOR      :  JOHN  MONCADA PUERTA
   ACTAULIZADO: 9 septiembre de 2005     
------------------------------------------------------------------------------*/  
 RUN _SetCurs.r ("WAIT").
 Listado = W_PathSpl + "InfDemografica-" + W_Usuario + ".csv".
 OS-DELETE VALUE(Listado).
 OUTPUT TO value(Listado). /* NO-ECHO PAGED PAGE-SIZE 65 */
 {Incluido\RepEncabezado.i}
 
 DEFINE VAR wconteo  AS INTEGER INITIAL 0.
 DEFINE VAR wperliq  AS CHARACTER FORMAT "X(20)".
 DEFINE VAR wtipoAho AS CHARACTER FORMAT "X(20)".
 DEFINE VAR wsexo    AS CHARACTER FORMAT "X(10)".
 DEFINE VAR wcasa    AS character FORMAT "X(10)".
 DEFINE VAR wtipvinc AS CHARACTER FORMAT "X(10)".
 DEFINE VAR wtipcont AS CHARACTER FORMAT "X(12)".
 DEFINE VAR W_NUbicacion           AS CHARACTER FORMAT "X(50)".
 DEFINE VAR W_UbicacionResidencia  AS CHARACTER FORMAT "X(50)".
 DEFINE VAR W_UbicacionComercial   AS CHARACTER FORMAT "X(50)".
 DEFINE VAR W_CiuNacimiento        AS CHARACTER FORMAT "X(50)".
 DEFINE VAR W_NomProfesion         AS CHARACTER FORMAT "X(20)".
 DEFINE VAR W_Nomcargo             AS CHARACTER FORMAT "X(20)".
 DEFINE VAR W_NomEmpresa           AS CHARACTER FORMAT "X(20)".
 DEFINE VAR w_toting               AS DECIMAL INITIAL 0.00 FORMAT "-zzzzzzzzzzz9".
 DEFINE VAR w_totegr               AS DECIMAL INITIAL 0.00 FORMAT "-zzzzzzzzzzz9".

 PUT "CodAge;NombreAge;Vinculo;TipoDoc;Identif;Nombres;FecAsoc;FecUltAct;Genero;FecNac;CiuNac;CiuRes"
     ";TipoViv;TelRes;EstCivil;#Hijos;#PerCargo;NivEduc;Profes;Cod_Ciiu;FecIngEmp;Empresa"
     ";ClaseCont;DirLabor;TelTrab;Email;Cargo;Sueldo;TotIng;TotEgre" skip(0).
FOR EACH clientes WHERE Clientes.estado = 1 AND 
                         Clientes.Fec_Ingreso   GE fecini AND Clientes.fec_Ingreso LE fecfin AND 
                         clientes.agencia       GE ageini AND Clientes.agencia     LE agefin  
                        NO-LOCK BREAK BY Clientes.agencia:

  Msaje:SCREEN-VALUE IN FRAME FrmGerencia = ".....Procesando Agencia : " + string(clientes.agencia).
  IF FIRST-OF(Clientes.agencia) THEN DO:
     FIND FIRST agencias WHERE agencias.agencia = Clientes.agencia NO-LOCK NO-ERROR.
  END.
  
  IF clientes.sexo = 1 THEN wsexo = "Masculino". ELSE wsexo = "Femenino".
  CASE clientes.tipo_vinculo:
      WHEN 1 THEN wtipvinc = 'Asociado'.
      WHEN 2 THEN wtipvinc = 'Cliente '.
      WHEN 3 THEN wtipvinc = 'Tercero '.
      WHEN 4 THEN wtipvinc = 'Proveedor'.
      OTHERWISE wtipvinc = "Cliente".
  END CASE.
  CASE clientes.Tipo_vivienda:
      WHEN 1 THEN wcasa = 'Propia'.
      WHEN 2 THEN wcasa = 'Arrendada'.
      OTHERWISE wcasa = 'Familiar'.
  END CASE.
  CASE clientes.tip_contrato:
      WHEN 1 THEN wtipcont = 'Ninguno'.
      WHEN 2 THEN wtipcont = 'Indefinido'.
      WHEN 3 THEN wtipcont = 'Fijo'.
      WHEN 4 THEN wtipcont = 'Labor Cont'.
      WHEN 5 THEN wtipcont = 'Prest Serv'.
      OTHERWISE wtipcont = 'No defin'.
  END CASE.


/*residencia*/
     /*residencia*/
     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "B" AND Ubicacion.Ubicacion EQ Clientes.Lugar_Residencia NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = Ubicacion.Nombre.

     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Residencia,1,5) NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = W_NUbicacion + " - " + Ubicacion.Nombre.

     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "D" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Residencia,1,2) NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = W_NUbicacion + " - " + Ubicacion.Nombre.
     W_UbicacionResidencia = TRIM(LC(W_NUbicacion)).
     /*comercial*/
     W_NUbicacion = "".
     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "B" AND Ubicacion.Ubicacion EQ Clientes.Lugar_Comercial NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = Ubicacion.Nombre.
     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Comercial,1,5) NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = W_NUbicacion + " - " + Ubicacion.Nombre.
     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "D" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Comercial,1,2) NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN ASSIGN W_NUbicacion  = W_NUbicacion + " - " + Ubicacion.Nombre.
     W_UbicacionComercial = trim(LC(W_NUbicacion)).

     /*nacimiento*/
     ASSIGN W_NUbicacion = ""
            W_CiuNacimiento = "".
     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "B" AND Ubicacion.Ubicacion EQ Clientes.Lugar_Nacimiento NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN 
        ASSIGN W_NUbicacion  = Ubicacion.Nombre.     

     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "C" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Nacimiento,1,5) NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN 
        ASSIGN W_NUbicacion  = W_NUbicacion + " - " + Ubicacion.Nombre.

     FIND FIRST Ubicacion WHERE Ubicacion.Tipo EQ "D" AND Ubicacion.Ubicacion BEGINS SUBSTRING(Clientes.Lugar_Nacimiento,1,2) NO-LOCK NO-ERROR.
     IF AVAILABLE Ubicacion THEN 
        ASSIGN W_NUbicacion  = W_NUbicacion + " - " + Ubicacion.Nombre.
     W_CiuNacimiento= TRIM(LC(W_NUbicacion)).

     FIND Varios WHERE Varios.Tipo EQ 1 AND Varios.Codigo EQ Clientes.Cod_Profesion NO-LOCK NO-ERROR.
     IF AVAILABLE(Varios) THEN W_NomProfesion = Varios.Descripcion.
                          ELSE W_NomProfesion = "".
     FIND Varios WHERE Varios.Tipo EQ 2 AND Varios.Codigo EQ Clientes.Cod_Cargo NO-LOCK NO-ERROR.        
     IF AVAILABLE(Varios) THEN W_NomCargo = Varios.Descripcion.
                          ELSE W_NomCargo = "".

     FIND Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
     IF AVAILABLE(Empresas) THEN  
       W_NomEmpresa = Empresas.Alias_Empresa.
     ELSE W_NomEmpresa = "".
  w_toting = (clientes.Ing_Arriendos + Clientes.Ing_Financieros + Clientes.Ing_Honorarios + Clientes.Ing_Otros).
  w_totegr = (Clientes.GtoFinanc_Indir + clientes.Gto_Arriendo + Clientes.Gto_Familiar + clientes.Gto_Obligacion ).
  PUT Clientes.agencia FORMAT "999" ";" CAPS(TRIM(agencias.nombre)) FORMAT "X(15)" ";" wtipvinc 
      ";" Clientes.Tipo_Identificacion ";" clientes.nit  FORMAT "X(12)"  
      ";" (trim(clientes.nombre) + " " + trim(clientes.apellido1) + " " + trim(clientes.apellido2))  FORMAT "X(40)"
      ";" Clientes.Fec_Ingreso ";" clientes.fec_UltAct ";" wsexo ";" clientes.Fec_Nacim ";" W_CiuNacimiento
      ";" W_UbicacionResidencia ";" wcasa ";" trim(clientes.Tel_Residencia) ";" trim(clientes.Est_civil) 
      ";" clientes.Num_hijos ";" clientes.per_acargo ";" clientes.Niv_Educativo
      ";" w_nomprofesion ";" Clientes.Codigo_CIIU ";" trim(string(clientes.fec_ingEmpresa))
      ";" w_nomempresa ";" wtipcont ";" clientes.DIR_comercial ";" clientes.tel_comercial
      ";" clientes.email
      ";" w_nomcargo ";" clientes.salario FORMAT "-zzzzzzzzzzz9" ";"  w_toting FORMAT "-zzzzzzzzzzz9" ";" w_totegr FORMAT "-zzzzzzzzzzz9" SKIP(0).

END.
OUTPUT CLOSE.
MESSAGE "El archivo plano esta en " listado
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfPdtosAge Wwin 
PROCEDURE InfPdtosAge :
/*------------------------------------------------------------------------------
  Purpose:     Totales por Productos de Ahorros
  Autor :     JOHN MONCADA PUERTA             
 Actualizacion:  9 septiembre de 2005
------------------------------------------------------------------------------*/
DEFINE VAR Wcodage  LIKE ahorros.agencia.
DEFINE VAR WnomAge  LIKE agencias.nombre.    
DEFINE VAR WCodaho  LIKE ahorros.cod_ahorro.
DEFINE VAR wNompdto LIKE pro_ahorros.nom_producto.
DEFINE VAR Wsaldo   AS DECIMAL INITIAL 0.00 FORMAT "zzzzzzzzzzzz9".

/*Obtiene el nombre del archivo - Agordon*/
DEFINE VARIABLE vcFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcDBName AS CHARACTER NO-UNDO.
ASSIGN vcDBName = DBNAME.
IF vcDBName EQ "BDCENTRAL" THEN
    ASSIGN vcDBNAME = "banca".
ELSE IF vcDBName = "BDCENTRAL1" THEN
         ASSIGN vcDBName = "pruebas".

ASSIGN vcFile = W_PathSpl + W_Usuario + "_InformeComercial" +
                vcDBname + "_" + STRING(W_Fecha,"99999999") + "_" +
                SUBSTRING(STRING(TIME,"HH:MM:SS"),1,2) + "-" + 
                SUBSTRING(STRING(TIME,"HH:MM:SS"),4,2) + "-" +
                SUBSTRING(STRING(TIME,"HH:MM:SS"),7,2) + ".csv".
/*Fin de Obtiene el nombre del archivo*/

RUN _SetCurs.r ("WAIT").
/*Listado = W_PathSpl + "InfPdtosxAge-" + W_Usuario + "-" + STRING(W_Fecha,"99999999")
          + "-" + STRING(INTEGER(TIME)) + ".Csv".*/
ASSIGN Listado = vcFile. /*AGordon*/
OS-DELETE VALUE(Listado).
FOR EACH TPdtoAho:
    DELETE TPdtoAho.
END.

OPEN infPdtoAho.
REPEAT :
    FETCH infPdtoAho INTO  Wcodage, WnomAge, WCodaho, wNompdto, Wsaldo.
    CREATE TPdtoAho.
    ASSIGN Tcodage   = Wcodage 
           TnomAge   = WnomAge 
           TcodAho   = WcodAho
           TNomPdto  = WNompdto 
           Tsaldo    = Wsaldo. 
END.
CLOSE infPdtoAho.

FOR EACH TPdtoCre:
    DELETE TPdtoCre.
END.
OPEN InfPdtoCre.
REPEAT:
  FETCH infPdtoCre INTO wcodage, wsaldo.
  CREATE TpdtoCre.
  ASSIGN TpdtoCre.Tcage   = wcodage
         TpdtoCre.Tcsaldo = wsaldo.

END.
CLOSE InfPdtoCre.

FOR EACH TAsoc:
    DELETE TAsoc.
END.
OPEN InfAsoc.
REPEAT:
  FETCH infAsoc INTO wcodage, wsaldo.
  CREATE TAsoc.
  ASSIGN Tasoc.Tage   = wcodage
         Tasoc.Tcant = wsaldo.
END.
CLOSE InfAsoc.

FOR EACH TClte:
    DELETE TClte.
END.
OPEN InfClte.
REPEAT:
  FETCH infClte INTO wcodage, wsaldo.
  CREATE TClte.
  ASSIGN TClte.Tage   = wcodage
         TClte.Tcant = wsaldo.
END.
CLOSE InfClte.

OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
/* W_Reporte    = "REPORTE   : Totales x Producto - Agencia: " + TRIM(SUBSTRING(Cmb_Agencias,6,15)) + " Del " + STRING(FecIni,"99/99/9999") + " Al " + STRING(FecFin,"99/99/9999").
W_EncColumna = "Agencia              Saldo".
VIEW FRAME F-Encabezado.                    */

PUT "CodAge;NombreAge;SdoCapitalCartera".
FOR EACH pro_ahorros WHERE pro_ahorros.estado = 1 NO-LOCK BREAK BY cod_ahorro:
   PUT ";" pro_ahorros.nom_producto FORMAT "X(15)".
END.
PUT ";#Asoc;#Clientes" SKIP(0).
    
FOR EACH agencias WHERE agencias.estado = 1 NO-LOCK BREAK BY agencias.agencia:
    FIND FIRST TpdtoAho WHERE TpdtoAho.Tcodage = agencias.agencia NO-LOCK NO-ERROR.
    IF AVAILABLE(TpdtoAho) THEN DO:
         Msaje:SCREEN-VALUE IN FRAME FrmGerencia = ".....Procesando Agencia : " + string(agencias.agencia).
         PUT TRIM(STRING(Tcodage)) FORMAT "999"  ";"  TRIM(TnomAge) FORMAT "X(30)". 
         FIND FIRST TpdtoCre WHERE TpdtoCre.Tcage   = agencias.agencia NO-LOCK NO-ERROR.
         IF AVAILABLE(TpdtoCre) THEN 
            PUT ";" TpdtoCre.Tcsaldo FORMAT "zzzzzzzzzzzzzz9".
         ELSE
           PUT ";0".
         FOR EACH pro_ahorros WHERE pro_ahorros.estado = 1 NO-LOCK BREAK BY cod_ahorro:
            FIND FIRST TpdtoAho WHERE TpdtoAho.Tcodage = agencias.agencia AND TpdtoAho.TcodAho = pro_ahorros.cod_ahorro NO-ERROR. 
            IF AVAILABLE(TpdtoAho) THEN 
               PUT ";" TpdtoAho.Tsaldo FORMAT "zzzzzzzzzzzzzz9".
            ELSE
               PUT ";0".
         END.
         FIND FIRST Tasoc WHERE Tasoc.tage = agencias.agencia NO-ERROR.
         IF AVAILABLE(Tasoc) THEN
            PUT ";"  Tasoc.Tcant FORMAT "zzzzzzz9".
         ELSE
            PUT ";0".
         
         FIND FIRST TClte WHERE TClte.tage = agencias.agencia NO-ERROR.
         IF AVAILABLE(TClte) THEN
             PUT ";" TClte.Tcant FORMAT "zzzzzzzz9".
          ELSE
             PUT ";0".

         PUT SKIP(0).
    END.
END.
OUTPUT CLOSE.
MESSAGE "El archivo plano esta en " listado
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfPdtosAgeAso Wwin 
PROCEDURE InfPdtosAgeAso :
/*------------------------------------------------------------------------------
  Purpose:     Totales por Productos de Ahorros x Agencia x Cliente
  Autor :     JOHN MONCADA PUERTA             
 Actualizacion:  9 septiembre de 2005
------------------------------------------------------------------------------*/
DEFINE VAR Wcodage  LIKE ahorros.agencia.
DEFINE VAR WnomAge  LIKE agencias.nombre. 
DEFINE VAR wtipdoc  LIKE clientes.tipo_identificacion.
DEFINE VAR wnit     LIKE clientes.nit.
DEFINE VAR WCodaho  LIKE ahorros.cod_ahorro.
DEFINE VAR wnombre  AS CHARACTER FORMAT "X(60)".
DEFINE VAR wcant    AS INTEGER INITIAL 0.
DEFINE VAR Wsaldo   AS DECIMAL INITIAL 0.00 FORMAT "zzzzzzzzzzzz9".

RUN _SetCurs.r ("WAIT").
Listado = W_PathSpl + "InfPdtosxAgexAso-" + W_Usuario + ".Csv".
OS-DELETE VALUE(Listado).
FOR EACH TDetPdtoAho:
    DELETE TDetPdtoAho.
END.

OPEN infDetPdtoAho.
REPEAT :
    FETCH infDetPdtoAho INTO  Wcodage, WnomAge, Wtipdoc, wnit, wcodaho, wNombre, wcant, Wsaldo.
    CREATE TDetPdtoAho.
    ASSIGN TDetPdtoAho.Tcodage     = Wcodage 
           TDetPdtoAho.TnomAge     = WnomAge 
           TDetPdtoAho.Ttipodoc    = wtipdoc
           TDetPdtoAho.Tnit        = wnit
           TDetPdtoAho.Tcodaho     = WcodAho
           TDetPdtoAho.TNombre     = WNombre
           TDetPdtoAho.Tcant       = wcant 
           TDetPdtoAho.Tsaldo      = Wsaldo.
END.
CLOSE infDetPdtoAho.

FOR EACH TDetPdtoCre:
    DELETE TDetPdtoCre.
END.
OPEN InfDetPdtoCre.
REPEAT:
  FETCH infDetPdtoCre INTO wcodage, wnit, wcant, wsaldo.
  CREATE TDetpdtoCre.
  ASSIGN TDetpdtoCre.Tcage   = wcodage
         TDetpdtoCre.Tcnit   = wnit
         TDetpdtoCre.Tccant  = wcant
         TDetpdtoCre.Tcsaldo = wsaldo.
END.
CLOSE InfDetPdtoCre.


OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
/* W_Reporte    = "REPORTE   : Totales x Producto - Agencia: " + TRIM(SUBSTRING(Cmb_Agencias,6,15)) + " Del " + STRING(FecIni,"99/99/9999") + " Al " + STRING(FecFin,"99/99/9999").
W_EncColumna = "Agencia              Saldo".
VIEW FRAME F-Encabezado.                    */

PUT "CodAge;NombreAge;TipDoc;Ident;Nombre;#Cre;SdoCapitCre".
FOR EACH pro_ahorros WHERE pro_ahorros.estado = 1 NO-LOCK BREAK BY cod_ahorro:
   PUT ";" + TRIM(pro_ahorros.nom_producto) FORMAT "X(15)".
END.
PUT SKIP(0).
    
FOR EACH agencias WHERE agencias.estado = 1 NO-LOCK BREAK BY agencias.agencia:
    FIND FIRST TDetpdtoAho WHERE TDetpdtoAho.Tcodage = agencias.agencia NO-LOCK NO-ERROR.
    IF AVAILABLE(TDetpdtoAho) THEN DO:
      FOR EACH TDetpdtoAho WHERE TDetpdtoAho.Tcodage = agencias.agencia NO-LOCK BREAK BY TDetpdtoAho.Tcodage BY TDetpdtoAho.Tnit BY TDetpdtoAho.TcodAho :
        Msaje:SCREEN-VALUE IN FRAME FrmGerencia = ".....Procesando Agencia : " + string(agencias.agencia).
        PUT TRIM(STRING(Tcodage)) FORMAT "999"  ";"  TRIM(TnomAge) FORMAT "X(30)"  ";"  trim(TDetpdtoAho.Ttipodoc)  ";" TDetpdtoAho.tNit ";"  TDetpdtoAho.tnombre. 
        FIND FIRST TDetpdtoCre WHERE TDetpdtoCre.Tcage = agencias.agencia AND TDetpdtoCre.tcnit = TDetpdtoAho.tnit NO-LOCK NO-ERROR.
        IF AVAILABLE(TDetpdtoCre) THEN 
          PUT ";" TDetpdtoCre.Tccant FORMAT "zzzzz9" ";"   TDetpdtoCre.Tcsaldo FORMAT "zzzzzzzzz9".
        ELSE
          PUT ";0;0".
        FOR EACH pro_ahorros WHERE pro_ahorros.estado = 1 NO-LOCK BREAK BY cod_ahorro:
           IF pro_ahorros.cod_ahorro = TDetpdtoAho.TcodAho THEN
              PUT ";" TDetpdtoAho.TCant FORMAT "zzzzz9" ";" TDetpdtoAho.Tsaldo FORMAT "zzzzzzzzz9".
           ELSE
              PUT ";0;0".
        END.
        PUT SKIP(0).
      END.
    END.
END.
OUTPUT CLOSE.
MESSAGE "El archivo plano esta en " listado
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfTransacciones Wwin 
PROCEDURE InfTransacciones :
/*------------------------------------------------------------------------------   
   PROPOSITO  : Reporte de Transacciones x Usuario
   AUTOR      : FÉLIX VARGAS CASTAÑO
   CREACIÓN   : 27 Agosto de 2007
   ACTAULIZADO: 27 Agosto de 2007     
------------------------------------------------------------------------------*/  
RUN _SetCurs.r ("WAIT").
Listado = W_PathSpl + "InfTransacciones-" + W_Usuario + ".csv".
OS-DELETE VALUE(Listado).
OUTPUT TO value(Listado). /* NO-ECHO PAGED PAGE-SIZE 65 */
{Incluido\RepEncabezado.i}

PUT "Movimientos;Periodo;Agencia_Fuente;Usuario;Grupo;Operacion;Tipo;Clase;Recibido_En;Comprobante;Total" SKIP(0).
ASSIGN wperiodo  = STRING(fecini) + " Al " + STRING(fecfin).
FOR EACH mov_ahorros WHERE
    mov_ahorros.age_fuente GE ageini AND mov_ahorros.age_fuente LE agefin AND    
    mov_ahorros.fecha      GE fecini AND mov_ahorros.fecha      LE fecfin 
    NO-LOCK BREAK BY mov_ahorros.age_fuente
    BY mov_ahorros.usuario BY mov_ahorros.cod_operacion BY mov_ahorros.cpte:

    ASSIGN wagei    = mov_ahorros.age_fuente
           wusuario = mov_ahorros.usuario
           wcodoper = mov_ahorros.cod_operacion.

    Msaje:SCREEN-VALUE IN FRAME FrmGerencia = ".....Procesando Agencia : " + string(wagei).
    IF FIRST-OF (mov_ahorros.age_fuente) THEN
     DO:
        ASSIGN wtotcompa = 0  wnomage   = "NO EXISTE".
        FIND FIRST agencias WHERE agencias.agencia = wagei NO-LOCK NO-ERROR.
        IF AVAILABLE(agencias)THEN ASSIGN wnomage = agencias.nombre.
     END.

        IF FIRST-OF (mov_ahorros.age_fuente) OR FIRST-OF (mov_ahorros.usuario) THEN 
         DO:
           ASSIGN wtotcompu = 0
                  wgrupo    = 0
                  wnomusu   = "NO EXISTE"
                  wnomgru   = "NO EXISTE".
           FIND FIRST usuarios WHERE usuarios.usuario = wusuario NO-LOCK NO-ERROR.
/*            FIND FIRST usuarios WHERE usuarios.agencia = wagei AND usuarios.usuario = wusuario NO-LOCK NO-ERROR.  */
           IF AVAILABLE(usuarios)THEN 
              ASSIGN wnomusu = usuarios.nombre
                     wgrupo  = usuarios.grupo.
              FIND FIRST grupos WHERE grupos.grupo = wgrupo NO-LOCK NO-ERROR.
              IF AVAILABLE(grupos)THEN ASSIGN wnomgru = grupos.nombre.
         END.
        
        IF FIRST-OF (mov_ahorros.age_fuente) OR FIRST-OF (mov_ahorros.usuario) OR FIRST-OF (mov_ahorros.cod_operacion)THEN 
         DO:
           ASSIGN  wtotcompoa = 0
                   wnomoper   = "NO EXISTE"
                   wcpteo     = 0.
           FIND FIRST operacion WHERE operacion.cod_operacion = wcodoper NO-LOCK NO-ERROR.
           IF AVAILABLE(operacion)THEN
              ASSIGN wnomoper = operacion.nom_operacion
                     wcpteo   = operacion.comprobante.
              CASE operacion.tipo_operacion: 
                   WHEN 1 THEN ASSIGN wtipope = "Ingreso".
                   WHEN 2 THEN ASSIGN wtipope = "Egreso".
                   WHEN 3 THEN ASSIGN wtipope = "Traslado".
                   OTHERWISE 
                       ASSIGN wtipope = "No Definida".
              END CASE.
              CASE operacion.clase_operacion: 
                   WHEN 1 THEN ASSIGN wnomclase = "Taquilla".
                   WHEN 2 THEN ASSIGN wnomclase = "Nomina".
                   WHEN 3 THEN ASSIGN wnomclase = "Procesos".
                   OTHERWISE 
                       ASSIGN wnomclase = "No Definida".
              END CASE.
              CASE operacion.ctrl_efeche: 
                   WHEN 1 THEN ASSIGN wefeche = "Efectivo".
                   WHEN 2 THEN ASSIGN wefeche = "Cheque".
                   WHEN 3 THEN ASSIGN wefeche = "Ninguno".
                   OTHERWISE 
                       ASSIGN wefeche = "No Definida".
              END CASE.
         END.
        
        IF FIRST-OF (mov_ahorros.age_fuente) OR FIRST-OF (mov_ahorros.usuario) OR FIRST-OF (mov_ahorros.cod_operacion) OR FIRST-OF (mov_ahorros.cpte)THEN 
         DO:
           ASSIGN  wtotcompo = 0 wtotcompc = 0 wcpte     = mov_ahorros.cpte. 
           FIND FIRST comprobantes WHERE comprobantes.agencia = wagei AND comprobantes.comprobante = wcpte NO-LOCK NO-ERROR.
           IF AVAILABLE(comprobantes)THEN ASSIGN wnomcomp = comprobantes.nombre.
         END.
        IF mov_ahorros.val_efectivo + mov_ahorros.val_cheque GT 0 THEN
           ASSIGN wtotcompc = wtotcompc + 1.
        IF LAST-OF (mov_ahorros.age_fuente) OR LAST-OF (mov_ahorros.usuario) OR LAST-OF (mov_ahorros.cod_operacion) OR LAST-OF (mov_ahorros.cpte)THEN
/*            IF wnomusu NE "NO EXISTE" AND wnomgru NE "NO EXISTE" AND wnomoper NE "NO EXISTE" THEN                                                       */
        /** 1- Administrador  2 - Sistemas  16 - Proveedor ********/                                                                                   
        IF (wgrupo NE 1 AND wgrupo NE 2 AND wgrupo NE 16) AND wnomusu NE "NO EXISTE" THEN 
         DO:
            ASSIGN wtotcompo  = wtotcompo + wtotcompc.
            ASSIGN wagenc1 = STRING(wagei)    + "-" + wnomage
                   wusuar1 = STRING(wusuario) + "-" + wnomusu
                   wgrupo1 = STRING(wgrupo)   + "-" + wnomgru
                   wopera1 = STRING(wcodoper) + "-" + wnomoper
                   wcompr1 = STRING(wcpte)    + "-" + wnomcomp.
            IF wtotcompc GT 0 THEN
               PUT "Mov_Ahorros" ";"
                    wperiodo     ";"
                    wagenc1   ";"
                    wusuar1   ";"
                    wgrupo1   ";"
                    wopera1   ";"
                    wtipope   ";"
                    wnomclase ";"
                    wefeche   ";"
                    wcompr1   ";"
                    wtotcompc SKIP(0).
          END.    
END. /* Mov_Ahorros */

FOR EACH mov_creditos WHERE
    mov_creditos.ofi_fuente GE ageini AND mov_creditos.ofi_fuente LE agefin AND    
    mov_creditos.fecha      GE fecini AND mov_creditos.fecha      LE fecfin 
    NO-LOCK BREAK BY mov_creditos.ofi_fuente
    BY mov_creditos.usuario BY mov_creditos.cod_operacion BY mov_creditos.cpte:

    ASSIGN wagei    = mov_creditos.ofi_fuente
           wusuario = mov_creditos.usuario
           wcodoper = mov_creditos.cod_operacion.
/**************************/
    IF FIRST-OF (mov_creditos.ofi_fuente) THEN
     DO:
        ASSIGN wtotcompa = 0  wnomage   = "NO EXISTE".
        FIND FIRST agencias WHERE agencias.agencia = wagei NO-LOCK NO-ERROR.
        IF AVAILABLE(agencias)THEN ASSIGN wnomage = agencias.nombre.
     END.

     IF FIRST-OF (mov_creditos.ofi_fuente) OR FIRST-OF (mov_creditos.usuario) THEN 
      DO:
        ASSIGN wtotcompu = 0
               wgrupo    = 0
               wnomusu   = "NO EXISTE"
               wnomgru   = "NO EXISTE".

        FIND FIRST usuarios WHERE usuarios.usuario = wusuario NO-LOCK NO-ERROR.
/*         FIND FIRST usuarios WHERE usuarios.agencia = wagei AND usuarios.usuario = wusuario NO-LOCK NO-ERROR.  */
        IF AVAILABLE(usuarios)THEN 
           ASSIGN wnomusu = usuarios.nombre
                  wgrupo  = usuarios.grupo.
           FIND FIRST grupos WHERE grupos.grupo = wgrupo NO-LOCK NO-ERROR.
           IF AVAILABLE(grupos)THEN ASSIGN wnomgru = grupos.nombre.
      END.
     
     IF FIRST-OF (mov_creditos.ofi_fuente) OR FIRST-OF (mov_creditos.usuario) OR FIRST-OF (mov_creditos.cod_operacion)THEN 
      DO:
        ASSIGN  wtotcompoa = 0
                wnomoper   = "NO EXISTE"
                wcpteo     = 0.
        FIND FIRST operacion WHERE operacion.cod_operacion = wcodoper NO-LOCK NO-ERROR.
        IF AVAILABLE(operacion)THEN
           ASSIGN wnomoper = operacion.nom_operacion
                  wcpteo   = operacion.comprobante.
           CASE operacion.tipo_operacion: 
                WHEN 1 THEN ASSIGN wtipope = "Ingreso".
                WHEN 2 THEN ASSIGN wtipope = "Egreso".
                WHEN 3 THEN ASSIGN wtipope = "Traslado".
                OTHERWISE 
                    ASSIGN wtipope = "No Definida".
           END CASE.
           CASE operacion.clase_operacion: 
                WHEN 1 THEN ASSIGN wnomclase = "Taquilla".
                WHEN 2 THEN ASSIGN wnomclase = "Nomina".
                WHEN 3 THEN ASSIGN wnomclase = "Procesos".
                OTHERWISE 
                    ASSIGN wnomclase = "No Definida".
           END CASE.
           CASE operacion.ctrl_efeche: 
                WHEN 1 THEN ASSIGN wefeche = "Efectivo".
                WHEN 2 THEN ASSIGN wefeche = "Cheque".
                WHEN 3 THEN ASSIGN wefeche = "Ninguno".
                OTHERWISE 
                    ASSIGN wefeche = "No Definida".
           END CASE.
      END.
     
     IF FIRST-OF (mov_creditos.ofi_fuente) OR FIRST-OF (mov_creditos.usuario) OR FIRST-OF (mov_creditos.cod_operacion) OR FIRST-OF (mov_creditos.cpte)THEN 
       DO:
         ASSIGN  wtotcompo = 0 wtotcompc = 0 wcpte     = mov_ahorros.cpte. 
         FIND FIRST comprobantes WHERE comprobantes.agencia = wagei AND comprobantes.comprobante = wcpte NO-LOCK NO-ERROR.
         IF AVAILABLE(comprobantes)THEN ASSIGN wnomcomp = comprobantes.nombre.
       END.
         
     IF mov_creditos.val_efectivo + mov_creditos.val_cheque GT 0 THEN
        ASSIGN wtotcompc = wtotcompc + 1.
     IF LAST-OF (mov_creditos.ofi_fuente) OR LAST-OF (mov_creditos.usuario) OR LAST-OF (mov_creditos.cod_operacion) OR LAST-OF (mov_creditos.cpte)THEN
/*         IF wnomusu NE "NO EXISTE" AND wnomgru NE "NO EXISTE" AND wnomoper NE "NO EXISTE" THEN                                                       */
     /** 1- Administrador  2 - Sistemas  16 - Proveedor ********/                                                                                   
     IF (wgrupo NE 1 AND wgrupo NE 2 AND wgrupo NE 16) AND wnomusu NE "NO EXISTE" THEN 
        DO:
         ASSIGN wtotcompo  = wtotcompo + wtotcompc.
         ASSIGN wagenc1 = STRING(wagei)    + "-" + wnomage
                wusuar1 = STRING(wusuario) + "-" + wnomusu
                wgrupo1 = STRING(wgrupo)   + "-" + wnomgru
                wopera1 = STRING(wcodoper) + "-" + wnomoper
                wcompr1 = STRING(wcpte)    + "-" + wnomcomp.
         IF wtotcompc GT 0 THEN
             PUT "Mov_Creditos" ";"
                  wperiodo     ";"
                  wagenc1   ";"
                  wusuar1   ";"
                  wgrupo1   ";"
                  wopera1   ";"
                  wtipope   ";"
                  wnomclase ";"
                  wefeche   ";"
                  wcompr1   ";"
                  wtotcompc SKIP(0).
        END.    
END. /* Mov_Creditos */
OUTPUT CLOSE.
MESSAGE "El archivo plano esta en " listado VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfTransTaquilla Wwin 
PROCEDURE InfTransTaquilla :
/*------------------------------------------------------------------------------   
   PROPOSITO  : Reporte de Transacciones En Caja (Taquilla) x Usuario
   AUTOR      : FÉLIX VARGAS CASTAÑO
   CREACIÓN   : 27 Agosto de 2007
   ACTAULIZADO: 27 Agosto de 2007     
------------------------------------------------------------------------------*/  
RUN _SetCurs.r ("WAIT").
Listado = W_PathSpl + "InfTransCaja-" + W_Usuario + ".csv".
OS-DELETE VALUE(Listado).
OUTPUT TO value(Listado). /* NO-ECHO PAGED PAGE-SIZE 65 */
{Incluido\RepEncabezado.i}

PUT "Periodo;Horario;Agencia;Usuario;Cajero;Grupo;Operacion;Tipo;Clase;Recibido_En;Transacciones;Total_Efectivo;Total_Cheque" SKIP(0).
ASSIGN wperiodo  = STRING(fecini) + " Al " + STRING(fecfin).

ASSIGN whoraini = F_HoraIni
       whorafin = F_HoraFin
       whorario = STRING(whoraini) + " A " + STRING(whorafin) + " H.M.".

FOR EACH taquilla WHERE
    taquilla.age_fuente GE ageini AND taquilla.age_fuente LE agefin AND    
    taquilla.fec_transaccion GE fecini AND taquilla.fec_transaccion LE fecfin AND 
    ((((((hora_transaccion - (hora_transaccion MOD 60) ) / 60) - (((hora_transaccion - (hora_transaccion MOD 60)) / 60) MOD 60 ) ) / 60) GE whoraini)
    AND (((((hora_transaccion - (hora_transaccion MOD 60) ) / 60) - (((hora_transaccion - (hora_transaccion MOD 60)) / 60) MOD 60 ) ) / 60) LE whorafin))
    NO-LOCK BREAK BY taquilla.age_fuente BY taquilla.usuario BY taquilla.cod_operacion:


    ASSIGN wagei    = taquilla.age_fuente
           wusuario = taquilla.usuario
           wcodoper = taquilla.cod_operacion.

    Msaje:SCREEN-VALUE IN FRAME FrmGerencia = ".....Procesando Agencia : " + string(wagei).
    IF FIRST-OF (taquilla.age_fuente) THEN
     DO:
       ASSIGN wtotcompa = 0
              wnomage   = "NO EXISTE".
       FIND FIRST agencias WHERE agencias.agencia = wagei NO-LOCK NO-ERROR.
       IF AVAILABLE(agencias)THEN ASSIGN wnomage = agencias.nombre.
     END.

    IF FIRST-OF (taquilla.age_fuente) OR FIRST-OF (taquilla.usuario) THEN 
     DO:
       ASSIGN wtotcompu = 0
              wgrupo    = 0
              wnomusu   = "NO EXISTE"
              wnomgru   = "NO EXISTE".
       FIND FIRST usuarios WHERE usuarios.usuario = wusuario NO-LOCK NO-ERROR.
/*            FIND FIRST usuarios WHERE usuarios.agencia = wagei AND usuarios.usuario = wusuario NO-LOCK NO-ERROR.  */
       IF AVAILABLE(usuarios)THEN 
          ASSIGN wnomusu = usuarios.nombre
                 wgrupo  = usuarios.grupo.
          FIND FIRST grupos WHERE grupos.grupo = wgrupo NO-LOCK NO-ERROR.
          IF AVAILABLE(grupos)THEN ASSIGN wnomgru = grupos.nombre.
       /*** Cajero ***/
       FIND LAST cajero WHERE cajero.usuario = taquilla.usuario AND 
        cajero.fecha EQ taquilla.fec_transaccion NO-ERROR.
       IF AVAILABLE cajero THEN
          ASSIGN wcajero = "Si".
       ELSE 
          ASSIGN wcajero = "No".
       /*****************/
     END.

    IF FIRST-OF (taquilla.age_fuente) OR FIRST-OF (taquilla.usuario) OR FIRST-OF (taquilla.cod_operacion)THEN 
     DO:
       ASSIGN  wtotcompoa = 0
               wnomoper   = "NO EXISTE"
               wcpteo     = 0.
       FIND FIRST operacion WHERE operacion.cod_operacion = wcodoper NO-LOCK NO-ERROR.
       IF AVAILABLE(operacion)THEN
          ASSIGN wnomoper = operacion.nom_operacion
                 wcpteo   = operacion.comprobante.
          CASE operacion.tipo_operacion: 
               WHEN 1 THEN ASSIGN wtipope = "Ingreso".
               WHEN 2 THEN ASSIGN wtipope = "Egreso".
               WHEN 3 THEN ASSIGN wtipope = "Traslado".
               OTHERWISE 
                   ASSIGN wtipope = "No Definida".
          END CASE.
          CASE operacion.clase_operacion: 
               WHEN 1 THEN ASSIGN wnomclase = "Taquilla".
               WHEN 2 THEN ASSIGN wnomclase = "Nomina".
               WHEN 3 THEN ASSIGN wnomclase = "Procesos".
               OTHERWISE 
                   ASSIGN wnomclase = "No Definida".
          END CASE.
          CASE operacion.ctrl_efeche: 
               WHEN 1 THEN ASSIGN wefeche = "Efectivo".
               WHEN 2 THEN ASSIGN wefeche = "Cheque".
               WHEN 3 THEN ASSIGN wefeche = "Ninguno".
               OTHERWISE 
                   ASSIGN wefeche = "No Definida".
          END CASE.
     END.

    IF FIRST-OF (taquilla.age_fuente) OR FIRST-OF (taquilla.usuario) OR FIRST-OF (taquilla.cod_operacion) THEN 
       ASSIGN  wtotcompo = 0 wtotcompc = 0 wtotefecti = 0.00 wtotcheque = 0.00.
    
    IF taquilla.val_efectivo + taquilla.val_cheque GT 0 THEN
       ASSIGN wtotcompc  = wtotcompc + 1
              wtotefecti = wtotefecti + val_efectivo
              wtotcheque = wtotcheque + val_cheque.
    IF LAST-OF (taquilla.age_fuente) OR LAST-OF (taquilla.usuario) OR LAST-OF (taquilla.cod_operacion) THEN
      IF (wgrupo NE 1 AND wgrupo NE 2 AND wgrupo NE 16) AND wnomusu NE "NO EXISTE" THEN 
       DO:
         ASSIGN wtotcompo  = wtotcompo + wtotcompc.
         ASSIGN wagenc1 = STRING(wagei)    + "-" + wnomage
                wusuar1 = STRING(wusuario) + "-" + wnomusu
                wgrupo1 = STRING(wgrupo)   + "-" + wnomgru
                wopera1 = STRING(wcodoper) + "-" + wnomoper.
         IF wtotcompc GT 0 THEN
             PUT wperiodo  ";"
                 whorario  ";"
                 wagenc1   ";"
                 wusuar1   ";"
                 wcajero   ";"
                 wgrupo1   ";"
                 wopera1   ";"
                 wtipope   ";"
                 wnomclase ";"
                 wefeche   ";"
                 wtotcompc ";"
                 wtotefecti FORMAT "->>>,>>>,>>>,>>9.99" ";"
                 wtotcheque FORMAT "->>>,>>>,>>>,>>9.99" SKIP(0).
       END.    
END.

/*** Usuario 998 - Tarjeta Debito - Cupo Rotativo 570 ***/
FOR EACH mov_creditos WHERE
    mov_creditos.Cod_Credito EQ 570 AND /*mov_creditos.usuario = "998" AND*/
    mov_creditos.ofi_fuente  GE ageini AND mov_creditos.ofi_fuente LE agefin AND    
    mov_creditos.fecha       GE fecini AND mov_creditos.fecha      LE fecfin AND 
    ((((((mov_creditos.hora - (mov_creditos.hora MOD 60) ) / 60) - (((mov_creditos.hora - (mov_creditos.hora MOD 60)) / 60) MOD 60 ) ) / 60) GE whoraini)
    AND (((((mov_creditos.hora - (mov_creditos.hora MOD 60) ) / 60) - (((mov_creditos.hora - (mov_creditos.hora MOD 60)) / 60) MOD 60 ) ) / 60) LE whorafin))
    NO-LOCK BREAK BY mov_creditos.ofi_fuente BY mov_creditos.usuario BY mov_creditos.cod_operacion :

    ASSIGN wagei    = mov_creditos.ofi_fuente
           wusuario = mov_creditos.usuario
           wcodoper = mov_creditos.cod_operacion.
/**************************/
    IF FIRST-OF (mov_creditos.ofi_fuente) THEN
     DO:
        ASSIGN wtotcompa = 0  wnomage   = "NO EXISTE".
        FIND FIRST agencias WHERE agencias.agencia = wagei NO-LOCK NO-ERROR.
        IF AVAILABLE(agencias)THEN ASSIGN wnomage = agencias.nombre.
     END.

    IF FIRST-OF (mov_creditos.ofi_fuente) OR FIRST-OF (mov_creditos.usuario) THEN 
      DO:
        ASSIGN wtotcompu = 0
               wgrupo    = 0
               wnomusu   = "NO EXISTE"
               wnomgru   = "NO EXISTE".

       FIND FIRST usuarios WHERE usuarios.usuario = wusuario NO-LOCK NO-ERROR.
/*            FIND FIRST usuarios WHERE usuarios.agencia = wagei AND usuarios.usuario = wusuario NO-LOCK NO-ERROR.  */
       IF AVAILABLE(usuarios)THEN 
          ASSIGN wnomusu = usuarios.nombre
                 wgrupo  = usuarios.grupo.
          FIND FIRST grupos WHERE grupos.grupo = wgrupo NO-LOCK NO-ERROR.
          IF AVAILABLE(grupos)THEN ASSIGN wnomgru = grupos.nombre.
       /*** Cajero ***/
       FIND LAST cajero WHERE cajero.usuario = mov_creditos.usuario AND 
        cajero.fecha EQ mov_creditos.fecha NO-ERROR.
       IF AVAILABLE cajero THEN
          ASSIGN wcajero = "Si".
       ELSE 
          ASSIGN wcajero = "No".
       /*****************/
      END.
     IF FIRST-OF (mov_creditos.ofi_fuente) OR FIRST-OF (mov_creditos.usuario) OR FIRST-OF (mov_creditos.cod_operacion)THEN 
      DO:
        ASSIGN  wtotcompoa = 0
                wnomoper   = "NO EXISTE"
                wcpteo     = 0.
        FIND FIRST operacion WHERE operacion.cod_operacion = wcodoper NO-LOCK NO-ERROR.
        IF AVAILABLE(operacion)THEN
           ASSIGN wnomoper = operacion.nom_operacion
                  wcpteo   = operacion.comprobante.
           CASE operacion.tipo_operacion: 
                WHEN 1 THEN ASSIGN wtipope = "Ingreso".
                WHEN 2 THEN ASSIGN wtipope = "Egreso".
                WHEN 3 THEN ASSIGN wtipope = "Traslado".
                OTHERWISE 
                    ASSIGN wtipope = "No Definida".
           END CASE.
           CASE operacion.clase_operacion: 
                WHEN 1 THEN ASSIGN wnomclase = "Mov.Creditos".
                WHEN 2 THEN ASSIGN wnomclase = "Mov.Creditos".
                WHEN 3 THEN ASSIGN wnomclase = "Mov.Creditos".
                OTHERWISE 
                    ASSIGN wnomclase = "No Definida".
           END CASE.
           CASE operacion.ctrl_efeche: 
                WHEN 1 THEN ASSIGN wefeche = "Efectivo".
                WHEN 2 THEN ASSIGN wefeche = "Cheque".
                WHEN 3 THEN ASSIGN wefeche = "Ninguno".
                OTHERWISE 
                    ASSIGN wefeche = "No Definida".
           END CASE.
      END.
    IF FIRST-OF (mov_creditos.ofi_fuente) OR FIRST-OF (mov_creditos.usuario) OR FIRST-OF (mov_creditos.cod_operacion) THEN 
       ASSIGN  wtotcompo = 0 wtotcompc = 0 wtotefecti = 0.00 wtotcheque = 0.00.
    
    IF mov_creditos.val_efectivo + mov_creditos.val_cheque GT 0 THEN
       ASSIGN wtotcompc  = wtotcompc + 1
              wtotefecti = wtotefecti + mov_creditos.val_efectivo
              wtotcheque = wtotcheque + mov_creditos.val_cheque.
    IF LAST-OF (mov_creditos.ofi_fuente) OR LAST-OF (mov_creditos.usuario) OR LAST-OF (mov_creditos.cod_operacion) THEN
      IF (wgrupo NE 1 AND wgrupo NE 2 AND wgrupo NE 16) AND wnomusu NE "NO EXISTE" THEN 
         DO:
          ASSIGN wtotcompo  = wtotcompo + wtotcompc.
          ASSIGN wagenc1 = STRING(wagei)    + "-" + wnomage
                 wusuar1 = STRING(wusuario) + "-" + wnomusu
                 wgrupo1 = STRING(wgrupo)   + "-" + wnomgru
                 wopera1 = STRING(wcodoper) + "-" + wnomoper.
          IF wtotcompc GT 0 THEN
              PUT wperiodo  ";"
                  whorario  ";"
                  wagenc1   ";"
                  wusuar1   ";"
                  wcajero   ";"
                  wgrupo1   ";"
                  wopera1   ";"
                  wtipope   ";"
                  wnomclase ";"
                  wefeche   ";"
                  wtotcompc ";"
                  wtotefecti FORMAT "->>>,>>>,>>>,>>9.99" ";"
                  wtotcheque FORMAT "->>>,>>>,>>>,>>9.99" SKIP(0).
         END.    
END. /* Mov_Creditos */
OUTPUT CLOSE.
MESSAGE "El archivo plano esta en " listado VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

