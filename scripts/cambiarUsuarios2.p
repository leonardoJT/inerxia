DISABLE TRIGGERS FOR LOAD OF ahorros.
DISABLE TRIGGERS FOR LOAD OF aplicarVisionamos.
DISABLE TRIGGERS FOR LOAD OF asesoria.
DISABLE TRIGGERS FOR LOAD OF cajero.
DISABLE TRIGGERS FOR LOAD OF cfg_instancias.
DISABLE TRIGGERS FOR LOAD OF cfg_tarjetaDB.
DISABLE TRIGGERS FOR LOAD OF che_blo.
DISABLE TRIGGERS FOR LOAD OF clientes.
DISABLE TRIGGERS FOR LOAD OF creditos.
DISABLE TRIGGERS FOR LOAD OF garantias.
DISABLE TRIGGERS FOR LOAD OF hoja_vida.
DISABLE TRIGGERS FOR LOAD OF listaNegra.
DISABLE TRIGGERS FOR LOAD OF logs.
DISABLE TRIGGERS FOR LOAD OF mov_ahorros.
DISABLE TRIGGERS FOR LOAD OF mov_contable.
DISABLE TRIGGERS FOR LOAD OF mov_contable_NIIF.
DISABLE TRIGGERS FOR LOAD OF mov_creditos.
DISABLE TRIGGERS FOR LOAD OF procDia.
DISABLE TRIGGERS FOR LOAD OF relaciones.
DISABLE TRIGGERS FOR LOAD OF reportarVisionamos.
DISABLE TRIGGERS FOR LOAD OF rep_ahorros.
DISABLE TRIGGERS FOR LOAD OF rep_creditos.
DISABLE TRIGGERS FOR LOAD OF solicitud.
DISABLE TRIGGERS FOR LOAD OF taquilla.
DISABLE TRIGGERS FOR LOAD OF tarjetaDebito.
DISABLE TRIGGERS FOR LOAD OF tarjetas.
DISABLE TRIGGERS FOR LOAD OF usuarios.

DEFINE VAR oldUser AS CHARACTER INITIAL "agarcia".
DEFINE VAR newUser AS CHARACTER INITIAL "lgarcia".

MESSAGE "Ahorros..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH ahorros WHERE ahorros.usu_creacion = oldUser AND ahorros.agencia = 2:
    ahorros.usu_creacion = newUser.
END.

FOR EACH ahorros WHERE ahorros.usu_gestor = oldUser AND ahorros.agencia = 2:
    ahorros.usu_gestor = newUser.
END.


MESSAGE "AplicarVisionamos..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH aplicarVisionamos WHERE aplicarVisionamos.usuario = oldUser:
    aplicarVisionamos.usuario = newUser.
END.


MESSAGE "Asesoría..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH asesoria WHERE asesoria.usuario = oldUser AND asesoria.agencia = 2:
    asesoria.usuario = newUser.
END.


MESSAGE "Cajero..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH cajero WHERE cajero.usuario = oldUser:
    cajero.usuario = newUser.
END.


MESSAGE "Cfg_instancias..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH cfg_instancias WHERE cfg_instancias.usuario = oldUser AND cfg_instancias.agencia = 2:
    cfg_instancias.usuario = newUser.
END.

FOR EACH cfg_tarjetaDB WHERE cfg_tarjetaDB.usuarioAdministrador = oldUser:
    cfg_tarjetaDB.usuarioAdministrador = newUser.
END.


MESSAGE "Che_blo..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH che_blo WHERE che_blo.usuario = oldUser AND che_blo.agencia = 2:
    che_blo.usuario = newUser.
END.


MESSAGE "Clientes..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH clientes WHERE clientes.usuario = oldUser AND clientes.agencia = 2:
    clientes.usuario = newUser.
END.


MESSAGE "Creditos..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH creditos WHERE creditos.usuario = oldUser AND creditos.agencia = 2:
    creditos.usuario = newUser.
END.

FOR EACH creditos WHERE creditos.usuario_gestor = oldUser AND creditos.agencia = 2:
    creditos.usuario_gestor = newUser.
END.


MESSAGE "Garantías..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH garantias WHERE garantias.usuario = oldUser AND garantias.agencia = 2:
    garantias.usuario = newUser.
END.


MESSAGE "Hoja_vida..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH hoja_vida WHERE hoja_vida.usuario = oldUser:
    hoja_vida.usuario = newUser.
END.


MESSAGE "Lista_negra..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH listaNegra WHERE listaNegra.usuario = oldUser:
    listaNegra.usuario = newUser.
END.


MESSAGE "Logs..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH logs WHERE logs.usuario = oldUser AND logs.agencia = 2:
    logs.usuario = newUser.
END.


MESSAGE "Mov_ahorros..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH mov_ahorros WHERE mov_ahorros.usuario = oldUser AND mov_ahorros.agencia = 2:
    mov_ahorros.usuario = newUser.
END.


MESSAGE "Mov_contable..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


FOR EACH agencias NO-LOCK:
    FOR EACH mov_contable WHERE mov_contable.agencia = agencias.agencia
                            AND mov_contable.fec_contable >= 01/01/2011
                            AND mov_contable.fec_contable <= 12/31/2016
                            AND mov_contable.usuario = oldUser AND mov_contable.agencia = 2:
        mov_contable.usuario = newUser.
    END.
END.


MESSAGE "Mov_contable_NIIF..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH mov_contable_NIIF WHERE mov_contable_NIIF.usuario = oldUser AND mov_contable_NIIF.agencia = 2:
    mov_contable_NIIF.usuario = newUser.
END.


MESSAGE "Mov_creditos..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH mov_creditos WHERE mov_creditos.usuario = oldUser AND mov_creditos.agencia = 2:
    mov_creditos.usuario = newUser.
END.

MESSAGE "ProcDia..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH procDia WHERE procDia.usuario = oldUser AND procDia.agencia = 2:
    procDia.usuario = newUser.
END.


MESSAGE "Relaciones..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH relaciones WHERE relaciones.usuario = oldUser :
    relaciones.usuario = newUser.
END.


MESSAGE "ReportarVisionamos..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH reportarVisionamos WHERE reportarVisionamos.usuario = oldUser AND reportarVisionamos.agencia = 2:
    reportarVisionamos.usuario = newUser.
END.


MESSAGE "Rep_ahorros..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH rep_ahorros WHERE rep_ahorros.usu_gestor = oldUser AND rep_ahorros.agencia = 2:
    rep_ahorros.usu_gestor = newUser.
END.

FOR EACH rep_ahorros WHERE rep_ahorros.usu_creacion = oldUser AND rep_ahorros.agencia = 2:
    rep_ahorros.usu_creacion = newUser.
END.


MESSAGE "Rep_creditos..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH rep_creditos WHERE rep_creditos.usuario = oldUser AND rep_creditos.agencia = 2:
    rep_creditos.usuario = newUser.
END.

FOR EACH rep_creditos WHERE rep_creditos.usuario_gestor = oldUser AND rep_creditos.agencia = 2:
    rep_creditos.usuario_gestor = newUser.
END.


MESSAGE "Solicitud..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH solicitud WHERE solicitud.usuario = oldUser AND solicitud.agencia = 2:
    solicitud.usuario = newUser.
END.

FOR EACH solicitud WHERE solicitud.usuario_gestor = oldUser AND solicitud.agencia = 2:
    solicitud.usuario_gestor = newUser.
END.


MESSAGE "Taquilla..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH taquilla WHERE taquilla.usuario = oldUser AND taquilla.agencia = 2:
    taquilla.usuario = newUser.
END.

FOR EACH taquilla WHERE taquilla.autorizo = oldUser AND taquilla.agencia = 2:
    taquilla.autorizo = newUser.
END.


MESSAGE "TarjetaDebito..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH tarjetaDebito WHERE tarjetaDebito.usuario = oldUser AND tarjetaDebito.agencia = 2:
    tarjetaDebito.usuario = newUser.
END.


MESSAGE "Tarjetas..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH tarjetas WHERE tarjetas.usuario_bloqueo = oldUser AND tarjetas.agencia = 2:
    tarjetas.usuario_bloqueo = newUser.
END.

FOR EACH tarjetas WHERE tarjetas.usuario = oldUser AND tarjetas.agencia = 2:
    tarjetas.usuario = newUser.
END.

FOR EACH tarjetas WHERE tarjetas.usuario_actMonto = oldUser AND tarjetas.agencia = 2:
    tarjetas.usuario_actMonto = newUser.
END.


MESSAGE "Usuarios..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH usuarios WHERE usuarios.usuario = oldUser AND usuarios.agencia = 2:
    usuarios.usuario = newUser.
END.


MESSAGE "Terminó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
