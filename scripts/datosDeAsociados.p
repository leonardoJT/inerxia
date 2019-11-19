DEFINE TEMP-TABLE tt
    FIELD nit AS CHARACTER
    FIELD fec_nacimiento AS CHARACTER
    FIELD estado_civil AS CHARACTER
    FIELD conyugue AS CHARACTER
    FIELD hijos AS CHARACTER
    FIELD hermanos AS CHARACTER
    FIELD padres AS CHARACTER
    FIELD estado_asociado AS CHARACTER
    FIELD pensionado AS CHARACTER
    FIELD sancionado AS CHARACTER
    FIELD fecIngreso AS CHARACTER
    FIELD motivo_Retiro AS CHARACTER
    FIELD grado_escolaridad AS CHARACTER
    FIELD vivienda_propia AS CHARACTER INITIAL "NO"
    FIELD vehiculo_propio AS CHARACTER INITIAL "NO"
    FIELD aportes_sociales AS DECIMAL
    FIELD ahorro_permanente AS DECIMAL
    FIELD ahorro_AlaVista AS DECIMAL
    FIELD CDAT_3meses AS DECIMAL
    FIELD CDAT_6meses AS DECIMAL
    FIELD devoluciones AS DECIMAL
    FIELD credito_ordinario AS DECIMAL
    FIELD credito_vehiculo AS DECIMAL
    FIELD credito_inmueble AS DECIMAL
    FIELD credito_recreacion AS DECIMAL
    FIELD credito_laboral AS DECIMAL
    FIELD credito_emergencia AS DECIMAL
    FIELD credito_corto_plazo AS DECIMAL
    FIELD credito_corto_plazo_educativo AS DECIMAL
    FIELD credito_rotativo AS DECIMAL
    FIELD credito_solidaridad AS DECIMAL
    FIELD credito_viajes_europa AS DECIMAL.

DEFINE BUFFER ttclientes FOR clientes.

FOR EACH clientes WHERE clientes.nit <> "" NO-LOCK:
    FIND FIRST ahorros WHERE ahorros.nit = clientes.nit
                         AND ahorros.tip_ahorro = 4 NO-LOCK NO-ERROR.
    IF AVAILABLE ahorros THEN DO:
        CREATE tt.
        tt.nit = clientes.nit.
        tt.fec_nacimiento = STRING(clientes.fec_nacimiento,"99/99/9999").
        tt.estado_civil = clientes.est_civil.

        FIND FIRST relaciones WHERE relaciones.nit = clientes.nit
                                AND (relaciones.descripcion = "Conyuge" OR
                                     relaciones.descripcion = "Compañero(a)") 
                                AND relaciones.estado = 1 NO-LOCK NO-ERROR.
        IF AVAILABLE relaciones THEN DO:
            FIND FIRST ttclientes WHERE ttclientes.nit = relaciones.nit_relacion NO-LOCK NO-ERROR.
            IF AVAILABLE ttclientes THEN
                tt.conyugue = ttclientes.nombre + " " + ttclientes.apellido1 + " " + ttclientes.apellido2.
        END.

        FOR EACH relaciones WHERE relaciones.nit = clientes.nit
                              AND (relaciones.descripcion = "Hijo(a)" OR
                                   relaciones.descripcion = "Hijastro(a)" OR
                                   relaciones.descripcion = "Hijo(a) del Conyuge" OR
                                   relaciones.descripcion = "Hijo(a) del Compañero(a)")
                              AND relaciones.estado = 1 NO-LOCK:
            FIND FIRST ttclientes WHERE ttclientes.nit = relaciones.nit_relacion NO-LOCK NO-ERROR.
            IF AVAILABLE ttclientes THEN
                tt.hijo = ttclientes.nombre + " " + ttclientes.apellido1 + " " + ttclientes.apellido2 + " / ".
        END.

        FOR EACH relaciones WHERE relaciones.nit = clientes.nit
                              AND relaciones.descripcion = "Hermano(a)"
                              AND relaciones.estado = 1 NO-LOCK:
            FIND FIRST ttclientes WHERE ttclientes.nit = relaciones.nit_relacion NO-LOCK NO-ERROR.
            IF AVAILABLE ttclientes THEN
                tt.hermano = ttclientes.nombre + " " + ttclientes.apellido1 + " " + ttclientes.apellido2 + " / ".
        END.

        FOR EACH relaciones WHERE relaciones.nit = clientes.nit
                              AND (relaciones.descripcion = "Madre" OR
                                   relaciones.descripcion = "Madre Adoptante" OR
                                   relaciones.descripcion = "Madrastra" OR
                                   relaciones.descripcion = "Padre" OR
                                   relaciones.descripcion = "Padre Adoptante" OR
                                   relaciones.descripcion = "Padrastro")
                              AND relaciones.estado = 1 NO-LOCK:
            FIND FIRST ttclientes WHERE ttclientes.nit = relaciones.nit_relacion NO-LOCK NO-ERROR.
            IF AVAILABLE ttclientes THEN
                tt.padres = ttclientes.nombre + " " + ttclientes.apellido1 + " " + ttclientes.apellido2 + " / ".
        END.

        IF ahorros.estado = 1 THEN
            tt.estado_asociado = "ACTIVO".
        ELSE
            tt.estado_asociado = "RETIRADO".

        FIND FIRST facultades WHERE facultades.codigo = STRING(clientes.facultad,"99") + STRING(clientes.departamento,"999") NO-LOCK NO-ERROR.
        IF AVAILABLE facultades THEN DO:
            IF INDEX(facultades.nombre,"Pens") = 1 THEN
                tt.pensionado = "JUBILADO".
        END.

        FIND FIRST ListaNegra WHERE ListaNegra.Nit = Clientes.Nit
                                AND (listaNegra.estado = 1 OR listaNegra.estado = 3) NO-ERROR.
        IF AVAILABLE ListaNegra THEN
            tt.sancionado = "sancionado".

        IF tt.estado_asociado = "ACTIVO" THEN
            tt.fecIngreso = STRING(clientes.fec_ingreso,"99/99/9999").

        IF tt.estado_asociado = "RETIRADO" THEN DO:
            FIND FIRST varios WHERE varios.tipo = 5
                                AND varios.codigo = clientes.cod_retiro NO-LOCK NO-ERROR.
            IF AVAILABLE varios THEN
                tt.motivo_retiro = varios.descripcion.
        END.

        tt.grado_escolaridad = clientes.niv_educativo.

        IF clientes.tipo_vivienda = 1 THEN
            tt.vivienda_propia = "SI".

        IF clientes.act_vehiculo > 0 THEN
            tt.vehiculo_propio = "SI".

        FOR EACH ahorros WHERE ahorros.nit = clientes.nit
                           AND ahorros.estado = 1 NO-LOCK BY ahorros.cod_ahorro:
            IF ahorros.cod_ahorro = 2 THEN
                tt.aportes_sociales = tt.aportes_sociales + ahorros.sdo_disponible + ahorros.sdo_canje.

            IF ahorros.cod_ahorro = 3 THEN
                tt.ahorro_permanente = tt.ahorro_permanente + ahorros.sdo_disponible + ahorros.sdo_canje.

            IF ahorros.cod_ahorro = 4 THEN
                tt.ahorro_aLaVista = tt.ahorro_aLaVista + ahorros.sdo_disponible + ahorros.sdo_canje.

            IF ahorros.cod_ahorro = 5 THEN
                tt.CDAT_3meses = tt.CDAT_3meses + ahorros.sdo_disponible + ahorros.sdo_canje.

            IF ahorros.cod_ahorro = 6 THEN
                tt.CDAT_6meses = tt.CDAT_6meses + ahorros.sdo_disponible + ahorros.sdo_canje.

            IF ahorros.cod_ahorro = 8 THEN
                tt.devoluciones = tt.devoluciones + ahorros.sdo_disponible + ahorros.sdo_canje.
        END.

        FOR EACH creditos WHERE creditos.nit = clientes.nit
                            AND creditos.estado = 2 NO-LOCK BY creditos.cod_credito:
            IF creditos.cod_credito = 17 THEN
                tt.credito_ordinario = tt.credito_ordinario + creditos.sdo_capital.

            IF creditos.cod_credito = 22 THEN
                tt.credito_vehiculo = tt.credito_vehiculo + creditos.sdo_capital.

            IF creditos.cod_credito = 27 THEN
                tt.credito_inmueble = tt.credito_inmueble + creditos.sdo_capital.

            IF creditos.cod_credito = 32 THEN
                tt.credito_recreacion = tt.credito_recreacion + creditos.sdo_capital.

            IF creditos.cod_credito = 62 THEN
                tt.credito_laboral = tt.credito_laboral + creditos.sdo_capital.

            IF creditos.cod_credito = 108 THEN
                tt.credito_emergencia = tt.credito_emergencia + creditos.sdo_capital.

            IF creditos.cod_credito = 113 THEN
                tt.credito_corto_plazo = tt.credito_corto_plazo + creditos.sdo_capital.

            IF creditos.cod_credito = 114 THEN
                tt.credito_corto_plazo_educativo = tt.credito_corto_plazo_educativo + creditos.sdo_capital.

            IF creditos.cod_credito = 123 THEN
                tt.credito_rotativo = tt.credito_rotativo + creditos.sdo_capital.

            IF creditos.cod_credito = 158 THEN
                tt.credito_solidaridad = tt.credito_solidaridad + creditos.sdo_capital.

            IF creditos.cod_credito = 186 THEN
                tt.credito_viajes_europa = tt.credito_viajes_europa + creditos.sdo_capital.
        END.

    END.
END.

OUTPUT TO d:\Leonardo\datosVarios.csv.
EXPORT DELIMITER ";"
    "CEDULA"
    "FECHA_NACIMIENTO"
    "ESTADO_CIVIL"
    "CONYUGUE"
    "HIJOS"
    "HERMANOS"
    "PADRES"
    "ESTADO_ASOCIADO"
    "PENSIONADO"
    "SANCIONADO"
    "FECHA_INGRESO"
    "MOTIVO_RETIRO"
    "GRADO_ESCOLARIDAD"
    "VIVIENDA_PROPIA"
    "VEHICULO_PROPIO"
    "APORTES_SOCIALES"
    "AHORRO_PERMANENTE"
    "AHORRO_A_LA_VISTA"
    "CDAT_3MESES"
    "CDAT_6MESES"
    "DEVOLUCIONES"
    "CREDITO_ORDINARIO"
    "CREDITO_VEHICULO"
    "CREDITO_INMUEBLE"
    "CREDITO_RECREACION"
    "CREDITO_LABORAL"
    "CREDITO_EMERGENCIA"
    "CREDITO_CORTO_PLAZO"
    "CREDITO_CORTO_PLAZO_EDUCATIVO"
    "CREDITO_ROTATIVO"
    "CREDITO_SOLIDARIDAD"
    "CREDITO_VIAJES_EUROPA".

FOR EACH tt NO-LOCK:
    EXPORT DELIMITER ";" tt.
END.
OUTPUT CLOSE.

MESSAGE "Fin" 
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
