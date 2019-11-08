/*  NOMBRE      : Prc_LlenarScoring.p
    DESCRIPCION : CALCULA EL SCORING
    LOG         : 9 ENE 2009, ADAPTACIÓN PARA JURISCOOP, ING. Edilberto Mariño Moya
*/    
DEFINE INPUT PARAMETER W_Nit    LIKE Clientes.Nit NO-UNDO.
DEFINE INPUT PARAMETER W_NumSol LIKE Solicitud.Num_Solicitud NO-UNDO.
DEFINE INPUT PARAMETER W_NitTit LIKE Clientes.Nit NO-UNDO.
DEFINE INPUT PARAMETER deVlorCuotaEsteCrdto AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER DECpcdadPgo AS DECIMAL NO-UNDO.
DEFINE /*NEW SHARED*/ VARIABLE Wvble like pro_scoring.VARIABLE.
DEFINE /*NEW SHARED*/ VARIABLE WTbla like pro_scoring.Tabla.

{Incluido/Variable.I "SHARED"}   

DEFINE VAR W_NitOut    AS CHARACTER FORMAT "X(16)" NO-UNDO.
DEFINE VAR W_tabOut    AS CHARACTER FORMAT "x(14)" NO-UNDO.
DEFINE VAR W_Puntaje   AS CHARACTER FORMAT "X(14)" NO-UNDO.
DEFINE VAR W_Cfg       AS INTEG FORM "9" INIT 1.  /*Inicia en Diferentes de MicroCredito*/
DEFINE VAR W_LiqDispon LIKE Clientes.Salario.
DEF BUFFER bProScoringCAPAGO FOR pro_scoring. /* recorre los parámetros CAPAGO según sucursal y pagaduría */
DEF BUFFER bProScoringCAPAGO1 FOR pro_scoring. /* recorre los parámetros de CAPACIDAD PAGO */
DEF VAR c AS CHAR NO-UNDO.


DEF VAR hClientes AS HANDLE NO-UNDO.
hClientes = BUFFER clientes:HANDLE.
DEF VAR hAnexos_clientes AS HANDLE NO-UNDO.
hAnexos_clientes = BUFFER anexos_clientes:HANDLE.
DEF VAR hpro_scoring AS HANDLE NO-UNDO.
hpro_scoring = BUFFER pro_scoring:HANDLE.
DEF VAR hTbla AS HANDLE NO-UNDO.
DEF VAR hCmpo AS HANDLE NO-UNDO.
DEF VAR hCmpoli AS HANDLE NO-UNDO.
hcmpoli = hpro_scoring:BUFFER-FIELD("Rango_Inicial"):HANDLE.
DEF VAR hCmpols AS HANDLE NO-UNDO.
hcmpols = hpro_scoring:BUFFER-FIELD("Rango_Final"):HANDLE.
DEF VAR hSlctud AS HANDLE NO-UNDO.
hSlctud = BUFFER solicitud:HANDLE.

DEF VAR cLstaExclsion AS CHAR NO-UNDO INITIAL "PERFIL,ESCENARIO ECONOMICO". /* LISTA DE PARAMETROS DE PRO_SCORING QUE NO SE DEBEN LEER EN ESTE PROCEDIMIENTO */
DEF VAR cHbtoPgoIntrno AS CHAR NO-UNDO.
DEF BUFFER bpro_scoring FOR pro_scoring.
FUNCTION fAnlsisCrtraIntrna RETURN CHAR(cnit AS CHAR):
    /*
        DESCRIPCION: Analisis del comportamiento de pagos interno del asociado.
        log: 07 feb 2008, Creado Ing. Edilberto Mariño Moya
    */
    DEF VAR cRtrno AS CHAR NO-UNDO.
    IF NOT CAN-FIND(FIRST creditos WHERE creditos.nit = cnit)
    THEN cRtrno = "SIN EXPERIENCIA".
    ELSE
    IF CAN-FIND(FIRST creditos WHERE creditos.nit = cnit AND creditos.estado = 5)
    THEN cRtrno = "CARTERA CASTIGADA".
    ELSE
    IF CAN-FIND(FIRST creditos WHERE creditos.nit = cnit AND creditos.dias_atraso > 60)
    THEN cRtrno = "CALIFICACION C,D,E".
    ELSE
    IF CAN-FIND(FIRST creditos WHERE creditos.nit = cnit AND creditos.dias_atraso > 30)
    THEN cRtrno = "MORA 60".
    ELSE
    IF CAN-FIND(FIRST creditos WHERE creditos.nit = cnit AND creditos.dias_atraso > 0)
    THEN cRtrno = "MORA 30".
    ELSE cRtrno = "AL DIA CON EXPERIENCIA".
    RETURN cRtrno.
END FUNCTION.

FUNCTION fchar2decimal RETURNS DECIMAL(c AS CHAR):
    /*  descripcion : Devuelve true si cv está entre cli y cls 
        log         : Creada: 4 ene 2008, Ing. Edilberto Mariño Moya
    */
        DEF VAR cs AS CHAR NO-UNDO.
        cs = IF SESSION:NUMERIC-DECIMAL-POINT = "." THEN "," ELSE ".".
        RETURN DECIMAL(REPLACE(c,cs,SESSION:NUMERIC-DECIMAL-POINT)).
END FUNCTION. /* FUNCTION fchar2decimal RETURNS DECIMAL(c AS CHAR): */

FUNCTION fVlorEnRngo RETURN LOGICAL(cTpo AS CHAR,cv AS CHAR,cli AS CHAR,cls AS CHAR):
    /*  descripcion : Devuelve true si cv está entre cli y cls 
        log         : Creada: 4 ene 2008, Ing. Edilberto Mariño Moya
    */
    CASE cTpo:
        WHEN "integer" THEN RETURN fchar2decimal(cv) >= fchar2decimal(cli) AND fchar2decimal(cv) <= fchar2decimal(cls).
        WHEN "date" THEN RETURN DATE(cv) >= DATE(cli) AND DATE(cv) <= DATE(cls).
        WHEN "decimal" THEN RETURN fchar2decimal(cv) >= fchar2decimal(cli) AND fchar2decimal(cv) <= fchar2decimal(cls).
        WHEN "logical" THEN RETURN cv >= cli AND cv <= cls.
        WHEN "character" THEN RETURN cv >= cli AND cv <= cls.
    END CASE.
END FUNCTION. /* FUNCTION fCmpra RETURN LOGICAL(cTpo AS CHAR,cv AS CHAR,cli AS CHAR,cls AS CHAR): */
FUNCTION fcapagocaja RETURNS CHAR():
    /*  descripcion : Devuelve la capacidad de pago calculado para pagos por caj
        log         : Creada: 9 feb 2008, Ing. Edilberto Mariño Moya
    */
    DEF VAR cRtrno AS CHAR NO-UNDO.
    DEF VAR dePntje AS DECIMAL NO-UNDO.
    cRtrno = fill(CHR(1),2). /* capacidad de pago/ nemonico */
    FOR FIRST bpro_scoring NO-LOCK
        WHERE
            bpro_scoring.observacion BEGINS "CAPAGOCAJA"
        AND decimal(bpro_scoring.rango_final) >= DECpcdadPgo:
        dePntje = bpro_scoring.puntaje.
    END.
    ENTRY(1,cRtrno,CHR(1)) = STRING(DECpcdadPgo).
    ENTRY(3,cRtrno,CHR(1)) = STRING(dePntje). /* capacidad de pago */
    ENTRY(2,cRtrno,CHR(1)) = "CAJA". /* nemonico del scoring */
    RETURN cRtrno.
END FUNCTION. /*FUNCTION fsmmlv RETURNS DECIMAL():*/

FUNCTION fsmmlv RETURNS CHAR():
    /*  descripcion : Devuelve la capacidad de pago calculado con base en SMMLV
        log         : Creada: 15 ene 2008, Ing. Edilberto Mariño Moya
    */
    DEF VAR cRtrno AS CHAR NO-UNDO.
    DEF VAR deCpa AS DECIMAL NO-UNDO.
    cRtrno = fill(CHR(1),2).
    deCpa = Clientes.Salario - Clientes.Gto_obligacion - deVlorCuotaEsteCrdto.
    FOR FIRST indicadores NO-LOCK
        WHERE
             Indicadores.indicador = 21:
        deCpa = deCpa - Indicadores.Valor.
    END.
    ENTRY(1,cRtrno,CHR(1)) = STRING(deCpa). /* capacidad de pago */
    ENTRY(2,cRtrno,CHR(1)) = "SMMLV". /* nemonico del scoring */
    IF NOT deCpa <= 0 
    THEN
    FOR FIRST bProScoringCAPAGO1 NO-LOCK
        WHERE
            bProScoringCAPAGO1.observacion BEGINS "CAPACIDAD PAGO"
        AND bProScoringCAPAGO1.rango_inicial = "SMMLV":
        ENTRY(3,cRtrno,CHR(1)) =  string(bProScoringCAPAGO1.puntaje).
    END.
    ELSE ENTRY(3,cRtrno,CHR(1)) =  "-35". 
    RETURN cRtrno.
END FUNCTION. /*FUNCTION fsmmlv RETURNS DECIMAL():*/

FUNCTION f50% RETURNS CHAR():
    /*  descripcion : Devuelve la capacidad de pago calculado con base en el 50%
        log         : Creada: 15 ene 2008, Ing. Edilberto Mariño Moya
    */
    DEF VAR cRtrno AS CHAR NO-UNDO.
    DEF VAR deCpa AS DECIMAL NO-UNDO.
    cRtrno = fill(CHR(1),2).
    deCpa = Clientes.Salario / 2 - Clientes.Gto_obligacion - deVlorCuotaEsteCrdto.
    ENTRY(1,cRtrno,CHR(1)) = STRING(deCpa). /* capacidad de pago */
    ENTRY(2,cRtrno,CHR(1)) = "50% SAL". /* nemonico del scoring */
    IF NOT deCpa <= 0 
    THEN
    FOR FIRST bProScoringCAPAGO1 NO-LOCK
        WHERE
            bProScoringCAPAGO1.observacion BEGINS "CAPACIDAD PAGO"
        AND bProScoringCAPAGO1.rango_inicial = "50% SAL":
        ENTRY(3,cRtrno,CHR(1)) =  string(bProScoringCAPAGO1.puntaje).
    END.
    ELSE ENTRY(3,cRtrno,CHR(1)) =  "-35".
    RETURN cRtrno.
END FUNCTION. /*FUNCTION f50% RETURNS DECIMAL():*/

W_NitOut = '"' + W_Nit + '"'.
IF W_Nit EQ "" 
THEN FOR EACH Scoring EXCLUSIVE-LOCK: DELETE Scoring. END.
ELSE DO:
   FOR EACH Scoring WHERE Scoring.Nit EQ W_Nit:
       DELETE Scoring.
   END.
  /*FOR EACH Scoring WHERE Scoring.Nit EQ W_Nit: ASSIGN Scoring.NIT = "XXXXXXXX". END.*/
END.

FIND FIRST Clientes EXCLUSIVE-LOCK 
    WHERE 
        Clientes.Nit = W_Nit NO-ERROR.
DO WHILE LOCKED clientes:
    MESSAGE "Cliente " w_nit " está siendo actualizado por otro usuario." SKIP
        "Espere Por Favor o [CTRL-C] Para Abortar."
        VIEW-AS ALERT-BOX WARNING TITLE "REGISTRO EN USO"  UPDATE lsino AS LOGICAL.
    FIND FIRST Clientes EXCLUSIVE-LOCK 
        WHERE 
            Clientes.Nit = W_Nit NO-ERROR.
END.
cHbtoPgoIntrno = fAnlsisCrtraIntrna(W_Nit).

IF AVAILABLE Clientes 
THEN DO:
    /* Variables de tiempo estaban red. a 2 dec, se paso a redondeo a cero decimal JOHN MONCADA - 1 de febrero de 2005 */
    ASSIGN  Clientes.Edad              = INTEGER(ROUND((W_Fecha - Clientes.Fec_Nacimiento) / 365,2))
            Clientes.Tiempo_Asociacion = INTEGER(ROUND((W_Fecha - Clientes.Fec_ingreso)    / 365,2))
            Clientes.Tiempo_Empresa    = INTEGER(ROUND((W_Fecha - Clientes.Fec_IngEmpresa) / 365,2)).          

    IF clientes.tiempo_empresa    LT 0 THEN clientes.tiempo_empresa    = 0.
    IF clientes.tiempo_asociacion LT 0 THEN clientes.tiempo_asociacion = 0.
    IF Clientes.Edad              LT 0 THEN Clientes.Edad              = 0.

    FIND FIRST Solicitud NO-LOCK
        WHERE 
                Solicitud.Num_Solicitud EQ W_NumSol
        AND     Solicitud.Nit           EQ W_NitTit NO-ERROR.
    IF AVAILABLE solicitud
    THEN    FIND FIRST Cfg_RegCredito NO-LOCK
                WHERE 
                    Cfg_RegCredito.Cod_Credito EQ Solicitud.Cod_Credito NO-ERROR.
    /*
    IF AVAILABLE Cfg_RegCredito AND AVAILABLE Solicitud
    THEN DO:
        IF  Cfg_RegCredito.Agencia_Exigida EQ 11     /*Enero 19/06 GAER, MicroCredito = 2 De tabla Varios-Tipos*/
        AND Solicitud.Tip_Credito          EQ 4
        /*AND W_Nit EQ W_NitTit Cuando el Scoring de codeudores de Microcredito sea Cod. = 2 */ THEN 
           W_Cfg = 2.
    END.
    */
    W_Cfg = IF AVAILABLE solicitud THEN solicitud.FOR_pago ELSE W_Cfg.
    /* verificar las siguientes líneas con nelson */
    IF NOT CAN-FIND(FIRST anexos_clientes WHERE anexos_clientes.nit = clientes.nit)
    THEN DO:
        CREATE anexos_clientes.
        ASSIGN  anexos_clientes.nit     = clientes.nit.
    END.
    /* FIND - verificar las siguientes líneas con nelson */



    FIND FIRST anexos_clientes NO-LOCK WHERE anexos_clientes.nit = clientes.nit NO-ERROR.
    IF NOT AVAILABLE anexos_clientes 
    THEN DO:
        MESSAGE "Información Anexa De Clientes No Existe." SKIP "Por Favor Verifique E Intente De Nuevo"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN.
    END.
END.
ELSE DO:
    MESSAGE     "         Falta el Cliente"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

FIND CURRENT Clientes NO-LOCK NO-ERROR.

FOR EACH Pro_Scoring 
    WHERE 
        NOT CAN-DO(cLstaExclsion,pro_scoring.observacion)
    AND NOT pro_scoring.observacion BEGINS "CAPAGO"
    AND NOT pro_scoring.observacion BEGINS "CAPACIDAD PAGO"
    AND Pro_Scoring.Codigo EQ W_Cfg NO-LOCK 
    BREAK 
        BY Pro_Scoring.observacion:
/*     FOR EACH Pro_Scoring                        */
/*         WHERE                                   */
/*             Pro_Scoring.Codigo EQ W_Cfg NO-LOCK */
/*         BREAK                                   */
/*             BY Pro_Scoring.VARIABLE:            */
    ASSIGN  WVble = Pro_Scoring.VARIABLE                                                                                                                                
            WTbla = Pro_Scoring.Tabla.                                                                                                                                  
    /*W_TabOut = Pro_Scoring.Tabla.*/ 
    IF Pro_Scoring.VARIABLE = "niv_educativo"
    THEN DO:
    END.
    RUN HallaScor.   
    
    /*
    IF   LAST-OF(Pro_Scoring.VARIABLE)                                                                                                                                 
    AND ((AVAIL(SCORING) AND SCORING.VARIABLE NE Wvble) OR NOT AVAIL(Scoring)) 
    THEN DO:                                                                                   
        RUN CrearScoring.                                                                                                                                             
        ASSIGN  SCORING.Puntaje        = 0
                SCORING.Valor_Variable = "0".
    END.
    */
    /*RUN Prc_Scoring.p Pro_Scoring.Tabla WVBle Pro_Scoring.Rango_inicial Pro_Scoring.Rango_Final (Pro_Scoring.Puntaje * 1000) W_NumSol W_NitOut Pro_Scoring.Codigo.*/ 
END.


/* PROCEDURE HallaScor:                                                                                          */
/*                                                                                                               */
/*     IF  Cfg_RegCredito.Agencia_Exigida EQ 11     /*Enero 19/06 GAER, MicroCredito = 2 De tabla Varios-Tipos*/ */
/*     AND Solicitud.Tip_Credito          EQ 4                                                                   */
/*     AND Wvble EQ "Capacidad_Pago"                                                                             */
/*     THEN DO:                                                                                                  */
/*         RUN CrearScoring.                                                                                     */
/*         ASSIGN  W_LiqDispon             =   Clientes.Ing_Arriendos  + Clientes.Ing_Financieros +              */
/*                                             Clientes.Ing_Honorarios + Clientes.Ing_Otros       +              */
/*                                             Clientes.Salario        - Clientes.Gto_obligacion  -              */
/*                                             Clientes.Gto_Familiar   - Clientes.Gto_Arriendo                   */
/*                 SCORING.Valor_Variable  =   STRING(W_LiqDispon)                                               */
/*                 SCORING.puntaje         =   0.                                                                */
/*                                                                                                               */
/*         IF W_LiqDispon GT Solicitud.Cuota * 2                                                                 */
/*         THEN SCORING.puntaje = 40.                                                                            */
/*         ELSE                                                                                                  */
/*         IF W_LiqDispon EQ Solicitud.Cuota * 2                                                                 */
/*         THEN SCORING.puntaje = 30.                                                                            */
/*         ELSE                                                                                                  */
/*         IF W_LiqDispon GT Solicitud.Cuota                                                                     */
/*         THEN SCORING.puntaje = 20.                                                                            */
/*         RETURN.                                                                                               */
/*     END.                                                                                                      */
/*     ELSE                                                                                                      */
/*     IF  Wvble EQ "Capacidad_Pago"                                                                             */
/*     AND Clientes.Capacidad_Pago GE DEC(Pro_Scoring.Rango_inicial)                                             */
/*     AND Clientes.Capacidad_Pago LE DEC(Pro_Scoring.Rango_Final)                                               */
/*     THEN DO:                                                                                                  */
/*         RUN CrearScoring.                                                                                     */
/*         ASSIGN SCORING.Valor_Variable = STRING(Clientes.Capacidad_Pago).                                      */
/*         RETURN.                                                                                               */
/*     END.                                                                                                      */
/*     ELSE                                                                                                      */
/*     IF  Wvble EQ "Conocimiento_Cliente"                                                                       */
/*     AND Clientes.Conocimiento_Cliente GE REPLACE(Pro_Scoring.Rango_inicial,'"',"")                            */
/*     AND Clientes.Conocimiento_Cliente LE REPLACE(Pro_Scoring.Rango_Final,'"',"")                              */
/*     THEN DO:                                                                                                  */
/*         RUN CrearScoring.                                                                                     */
/*         ASSIGN SCORING.Valor_Variable = Clientes.Conocimiento_Cliente.                                        */
/*         RETURN.                                                                                               */
/*     END.                                                                                                      */
/*     ELSE                                                                                                      */
/*     IF  Wvble EQ "Destino"                                                                                    */
/*     AND Clientes.Destino GE REPLACE(Pro_Scoring.Rango_inicial,'"',"")                                         */
/*     AND Clientes.Destino LE REPLACE(Pro_Scoring.Rango_Final,'"',"")                                           */
/*     THEN DO:                                                                                                  */
/*         RUN CrearScoring.                                                                                     */
/*         ASSIGN SCORING.Valor_Variable = Clientes.Destino.                                                     */
/*         RETURN.                                                                                               */
/*     END.                                                                                                      */
/*     ELSE                                                                                                      */
/*     IF  Wvble EQ "Edad"                                                                                       */
/*       /*  John - adiciono integer a la operacion (W_Fecha - Clientes.Fec_Nacimiento) / 365)*/                 */
/*     AND ( INTEGER(ROUND((W_Fecha - Clientes.Fec_Nacimiento) / 365,2))) GE INTEG(Pro_Scoring.Rango_inicial)    */
/*     AND ( INTEGER(ROUND((W_Fecha - Clientes.Fec_Nacimiento) / 365,2))) LE INTEG(Pro_Scoring.Rango_Final)      */
/*     THEN DO:                                                                                                  */
/*         RUN CrearScoring.                                                                                     */
/*         ASSIGN SCORING.Valor_Variable = STRING(INTEGER(ROUND((W_Fecha - Clientes.Fec_Nacimiento) / 365,2))).  */
/*         RETURN.                                                                                               */
/*     END.                                                                                                      */
/*     ELSE                                                                                                      */
/*     IF  Wvble EQ "Endeud_Indirecto"                                                                           */
/*     AND Clientes.Endeud_Indirecto GE DEC(Pro_Scoring.Rango_inicial)                                           */
/*     AND Clientes.Endeud_Indirecto LE DEC(Pro_Scoring.Rango_Final)                                             */
/*     THEN DO:                                                                                                  */
/*         RUN CrearScoring.                                                                                     */
/*         ASSIGN SCORING.Valor_Variable = STRING(Clientes.Endeud_Indirecto).                                    */
/*         RETURN.                                                                                               */
/*     END.                                                                                                      */
/*     ELSE IF  Wvble EQ "Est_Civil"                                                                             */
/*     AND Clientes.Est_Civil GE REPLACE(Pro_Scoring.Rango_inicial,'"',"")                                       */
/*     AND Clientes.Est_Civil LE REPLACE(Pro_Scoring.Rango_Final,'"',"")                                         */
/*     THEN DO:                                                                                                  */
/*         RUN CrearScoring.                                                                                     */
/*         ASSIGN SCORING.Valor_Variable = STRING(Clientes.Est_Civil).                                           */
/*         RETURN.                                                                                               */
/*     END.                                                                                                      */
/*     ELSE                                                                                                      */
/*     IF  Wvble EQ "Garantia"                                                                                   */
/*     AND Clientes.Garantia GE REPLACE(Pro_Scoring.Rango_inicial,'"',"")                                        */
/*     AND Clientes.Garantia LE REPLACE(Pro_Scoring.Rango_Final,'"',"")                                          */
/*     THEN DO:                                                                                                  */
/*         RUN CrearScoring.                                                                                     */
/*         ASSIGN SCORING.Valor_Variable = STRING(Clientes.Garantia).                                            */
/*         RETURN.                                                                                               */
/*     END.                                                                                                      */
/*     ELSE                                                                                                      */
/*     IF  Wvble EQ "Mora_Comercial"                                                                             */
/*     AND Clientes.Mora_Comercial GE REPLACE(Pro_Scoring.Rango_Inicial,'"',"")                                  */
/*     AND Clientes.Mora_Comercial LE REPLACE(Pro_Scoring.Rango_Final,'"',"")                                    */
/*     THEN DO:                                                                                                  */
/*         RUN CrearScoring.                                                                                     */
/*         ASSIGN SCORING.Valor_Variable = STRING(Clientes.Mora_Comercial).                                      */
/*         RETURN.                                                                                               */
/*     END.                                                                                                      */
/*     ELSE                                                                                                      */
/*     IF  Wvble EQ "Per_Acargo"                                                                                 */
/*     AND Clientes.Per_Acargo GE INTEG(Pro_Scoring.Rango_Inicial)                                               */
/*     AND Clientes.Per_Acargo LE INTEG(Pro_Scoring.Rango_Final)                                                 */
/*     THEN DO:                                                                                                  */
/*         RUN CrearScoring.                                                                                     */
/*         ASSIGN SCORING.Valor_Variable = STRING(Clientes.Per_Acargo).                                          */
/*         RETURN.                                                                                               */
/*     END.                                                                                                      */
/*     ELSE                                                                                                      */
/*     IF  Wvble EQ "Respaldo_Patrim"                                                                            */
/*     AND Clientes.Respaldo_Patrim GE REPLACE(Pro_Scoring.Rango_Inicial,'"',"")                                 */
/*     AND Clientes.Respaldo_Patrim LE REPLACE(Pro_Scoring.Rango_Final,'"',"")                                   */
/*     THEN DO:                                                                                                  */
/*         RUN CrearScoring.                                                                                     */
/*         ASSIGN SCORING.Valor_Variable = STRING(Clientes.Respaldo_Patrim).                                     */
/*         RETURN.                                                                                               */
/*     END.                                                                                                      */
/*     ELSE                                                                                                      */
/*     IF  Wvble EQ "Tiempo_Asociacion"                                                                          */
/*     AND INTEGER(ROUND((W_Fecha - Clientes.Fec_ingreso)    / 365,2)) GE DEC(Pro_Scoring.Rango_Inicial)         */
/*     AND INTEGER(ROUND((W_Fecha - Clientes.Fec_ingreso)    / 365,2)) LE DEC(Pro_Scoring.Rango_Final) THEN DO:  */
/*         RUN CrearScoring.                                                                                     */
/*         ASSIGN SCORING.Valor_Variable = STRING(INTEGER(ROUND((W_Fecha - Clientes.Fec_ingreso)    / 365,2))).  */
/*         RETURN.                                                                                               */
/*     END.                                                                                                      */
/*     ELSE                                                                                                      */
/*     IF  Wvble EQ "Tiempo_Empresa"                                                                             */
/*     AND INTEGER(ROUND((W_Fecha - Clientes.Fec_IngEmpresa) / 365,2)) GE DEC(Pro_Scoring.Rango_Inicial)         */
/*     AND INTEGER(ROUND((W_Fecha - Clientes.Fec_IngEmpresa) / 365,2)) LE DEC(Pro_Scoring.Rango_Final) THEN DO:  */
/*         RUN CrearScoring.                                                                                     */
/*         ASSIGN SCORING.Valor_Variable = STRING(INTEGER(ROUND((W_Fecha - Clientes.Fec_IngEmpresa) / 365,2))).  */
/*         RETURN.                                                                                               */
/*     END.                                                                                                      */
/*     ELSE                                                                                                      */
/*     IF  Wvble EQ "Tipo_Actividad"                                                                             */
/*     AND Clientes.Tipo_Actividad GE REPLACE(Pro_Scoring.Rango_Inicial,'"',"")                                  */
/*     AND Clientes.Tipo_Actividad LE REPLACE(Pro_Scoring.Rango_Final,'"',"")                                    */
/*     THEN DO:                                                                                                  */
/*         RUN CrearScoring.                                                                                     */
/*         ASSIGN SCORING.Valor_Variable = STRING(Clientes.Tipo_Actividad).                                      */
/*         RETURN.                                                                                               */
/*     END.                                                                                                      */
/*     ELSE                                                                                                      */
/*     IF  Wvble EQ "Tip_Contrato"                                                                               */
/*     AND Clientes.Tip_Contrato GE INTEG(Pro_Scoring.Rango_Inicial)                                             */
/*     AND Clientes.Tip_Contrato LE INTEG(Pro_Scoring.Rango_Final)                                               */
/*     THEN DO:                                                                                                  */
/*         RUN CrearScoring.                                                                                     */
/*         ASSIGN SCORING.Valor_Variable = STRING(Clientes.Tip_Contrato).                                        */
/*         RETURN.                                                                                               */
/*     END.                                                                                                      */
/* END PROCE.                                                                                                    */

PROCEDURE CrearScoring:
    DEF VAR i AS integer NO-UNDO.
    i = NUM-ENTRIES(pro_scoring.observacion,"|").
    CREATE SCORING.
    ASSIGN SCORING.Agencia       = Clientes.Agencia
         SCORING.Nit             = Clientes.nit         
         SCORING.Num_Solicitud   = W_NumSol                 
         SCORING.Fec_Scoring     = W_Fecha            
         SCORING.Tabla           = Pro_Scoring.TABLa               
         SCORING.VARIABLE        = /*Wvble*/ substring(entry(i,pro_scoring.observacion,"|"),4) + "(" +  Wvble + ")"
         SCORING.puntaje         = Pro_Scoring.Puntaje          
         SCORING.Codigo          = Pro_Scoring.Codigo        
         Scoring.Usuario         = W_Usuario
         Scoring.Valor_variable  = hcmpo:BUFFER-VALUE.
END PROCE.

PROCEDURE HallaScor:
    /*  log : 4 ene 2008 Substituido por Ing. Edilberto Mariño Moya */
    /* selecciona el buffer a usar */
    CASE pro_scoring.tabla:
        WHEN "clientes" THEN htbla = hclientes.
        WHEN "anexos_clientes" THEN htbla = hanexos_clientes.
        WHEN "solicitud" THEN htbla = hSlctud.
    END CASE.
    /* FIN - selecciona el buffer a usar */
    CASE Wvble:
        WHEN "Capacidad_Pago" 
        THEN do:
            RUN CrearScoring.
            c = DYNAMIC-FUNCTION(anexos_clientes.cam_cat1).
            ASSIGN  SCORING.Valor_Variable = ENTRY(2,c,CHR(1)) 
                    SCORING.puntaje = decimal(ENTRY(3,c,CHR(1))) NO-ERROR.
            PUBLISH "CpcdadPgoScoring"(SCORING.Valor_Variable + chr(1) + string(SCORING.puntaje)).
            RETURN.
        END. /* "Capacidad_Pago"  */
        OTHERWISE DO:
            hcmpo = htbla:BUFFER-FIELD(pro_scoring.VARIABLE):HANDLE.
            IF hcmpo:NAME = "for_pago" THEN DO:
            END.
            IF  fVlorEnRngo(IF hcmpo:NAME = "dias_atraso" THEN "character" ELSE hcmpo:DATA-TYPE,(IF hcmpo:NAME = "dias_atraso" THEN cHbtoPgoIntrno ELSE STRING(hcmpo:BUFFER-VALUE)),STRING(hcmpoli:BUFFER-VALUE),STRING(hcmpols:BUFFER-VALUE))  
            THEN DO:
                RUN CrearScoring.
                ASSIGN SCORING.Valor_Variable = hcmpo:BUFFER-VALUE.
                RETURN.
            END.
        END. /* OTHERWISE DO: */
    END CASE.
END PROCEDURE. /* PROCEDURE HallaScor: */
