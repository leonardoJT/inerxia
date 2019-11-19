/* Importar movimientos contables */
DISABLE TRIGGERS FOR LOAD OF mov_contable.
DISABLE TRIGGERS FOR LOAD OF anexos.

DEFINE TEMP-TABLE movContable
    FIELD agencia AS INTEGER
    FIELD fecha AS DATE
    FIELD comprobante AS INTEGER
    FIELD num_documento AS INTEGER
    FIELD cuenta AS CHARACTER
    FIELD concepto AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD doc_referencia AS CHARACTER
    FIELD db AS DECIMAL FORMAT "->>>,>>>,>>>"
    FIELD cr AS DECIMAL FORMAT "->>>,>>>,>>>".

DEFINE TEMP-TABLE movCont
    FIELD agencia AS INTEGER
    FIELD cuenta AS CHARACTER
    FIELD cen_costos AS INTEGER
    FIELD fecha AS DATE
    FIELD comprobante AS INTEGER
    FIELD num_documento AS INTEGER
    FIELD db AS DECIMAL FORMAT "->>>,>>>,>>>"
    FIELD cr AS DECIMAL FORMAT "->>>,>>>,>>>"
    FIELD nit AS CHARACTER
    FIELD doc_referencia AS CHARACTER
    FIELD concepto AS CHARACTER.

MESSAGE "Inicia"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

INPUT FROM "c:\INFO_Fodun\Leonardo\Terceros_1009.csv".

REPEAT:
    CREATE movContable.
    
    IMPORT DELIMITER ";" movContable NO-ERROR.

    FIND FIRST cuentas WHERE cuentas.cuenta = movContable.cuenta NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentas THEN DO:
        CASE movContable.cuenta:
            WHEN "17201504" OR
            WHEN "17201503" THEN movContable.cuenta = "17201501".

            WHEN "11050504" THEN movContable.cuenta = "11050501".

            WHEN "16403004" OR
            WHEN "16403003" THEN movContable.cuenta = "16403001".

            WHEN "24959506" THEN movContable.cuenta = "24959501".
            WHEN "24453506" THEN movContable.cuenta = "24453509".
            
            WHEN "27101002" OR
            WHEN "27101003" THEN movContable.cuenta = "27101001".

            WHEN "61753502" THEN movContable.cuenta = "61753501".
            WHEN "51105004" THEN movContable.cuenta = "51105001".
            
            WHEN "17250502" OR
            WHEN "17250504" THEN movContable.cuenta = "17250501".

            WHEN "17950590" OR
            WHEN "17950591" OR
            WHEN "17950592" OR
            WHEN "17950593" OR
            WHEN "17950595" OR
            WHEN "17950596" THEN movContable.cuenta = "17950501".

            WHEN "17251004" THEN movContable.cuenta = "17251001".
            WHEN "17952004" THEN movContable.cuenta = "17952002".

            WHEN "17951091" OR
            WHEN "17951092" OR
            WHEN "17951093" OR
            WHEN "17951094" OR
            WHEN "17951095" OR
            WHEN "17951003" OR
            WHEN "17951090" THEN movContable.cuenta = "17951001".

            WHEN "17951099" OR
            WHEN "17951005" OR
            WHEN "17951096" THEN movContable.cuenta = "17951002".

            WHEN "17952090" OR
            WHEN "17952003" OR
            WHEN "17952095" THEN movContable.cuenta = "17952001".

            WHEN "24151502" THEN movContable.cuenta = "24151501".
            
            WHEN "24500502" OR
            WHEN "24500503" OR
            WHEN "24500504" THEN movContable.cuenta = "24500501".
            
            WHEN "24502002" OR
            WHEN "24502004" OR
            WHEN "24502003" THEN movContable.cuenta = "24502001".

            WHEN "24501003" THEN movContable.cuenta = "24501001".
            WHEN "25100209" THEN movContable.cuenta = "25100205".
            WHEN "27101503" THEN movContable.cuenta = "27101501".
            WHEN "24501503" THEN movContable.cuenta = "24501501".
            WHEN "25100206" THEN movContable.cuenta = "25100201".
            
            WHEN "17953090" THEN movContable.cuenta = "17953001".

            WHEN "19041503" OR
            WHEN "19041504" OR
            WHEN "19041502" THEN movContable.cuenta = "19041501".

            WHEN "25100208" THEN movContable.cuenta = "25100204".
            
            WHEN "25100252" OR
            WHEN "25100253" OR
            WHEN "25100254" THEN movContable.cuenta = "25100251".
            
            WHEN "27050502" OR
            WHEN "27053003" OR
            WHEN "27053502" OR
            WHEN "27053503" OR
            WHEN "27054002" OR
            WHEN "27054003" OR
            WHEN "27054004" OR
            WHEN "27054502" OR
            WHEN "27054503" OR
            WHEN "27054504" OR
            WHEN "27055002" OR
            WHEN "27055003" OR
            WHEN "27055004" OR
            WHEN "27050504" OR
            WHEN "27053002" OR
            WHEN "27059502" OR
            WHEN "27059503" OR
            WHEN "27059504" OR
            WHEN "27050501" OR
            WHEN "27053001" OR
            WHEN "27053501" OR
            WHEN "27055001" THEN movContable.cuenta = "27059501".

            WHEN "28250502" THEN movContable.cuenta = "28250501".
            WHEN "28251002" THEN movContable.cuenta = "28251001".
            WHEN "28251502" THEN movContable.cuenta = "28251501".
            WHEN "28252002" THEN movContable.cuenta = "28252001".
            
            WHEN "51050602" OR
            WHEN "51050604" OR
            WHEN "51050603" THEN movContable.cuenta = "51050601".

            WHEN "51052702" OR
            WHEN "51052704" OR
            WHEN "51052703" THEN movContable.cuenta = "51052701".

            WHEN "51053002" OR
            WHEN "51053004" OR
            WHEN "51053003" THEN movContable.cuenta = "51053001".

            WHEN "51053302" OR
            WHEN "51053304" OR
            WHEN "51053303" THEN movContable.cuenta = "51053301".

            WHEN "51053602" OR
            WHEN "51053604" OR
            WHEN "51053603" THEN movContable.cuenta = "51053601".

            WHEN "51053902" OR
            WHEN "51053904" OR
            WHEN "51053903" THEN movContable.cuenta = "51053901".

            WHEN "51056902" OR
            WHEN "51056904" OR
            WHEN "51056903" THEN movContable.cuenta = "51056901".

            WHEN "51057002" OR
            WHEN "51057004" OR
            WHEN "51057003" THEN movContable.cuenta = "51057001".

            WHEN "51057102" OR
            WHEN "51057104" OR
            WHEN "51057103" THEN movContable.cuenta = "51057101".

            WHEN "51057202" OR
            WHEN "51057204" OR
            WHEN "51057203" THEN movContable.cuenta = "51057201".

            WHEN "51057502" OR
            WHEN "51057504" OR
            WHEN "51057503" THEN movContable.cuenta = "51057501".

            WHEN "51057802" OR
            WHEN "51057804" OR
            WHEN "51057803" THEN movContable.cuenta = "51057801".

            WHEN "51101002" OR
            WHEN "51101003" OR
            WHEN "51101004" THEN movContable.cuenta = "51101001".

            WHEN "51100403" THEN movContable.cuenta = "51100401".
            WHEN "51103804" THEN movContable.cuenta = "51103801".
            
            WHEN "51102002" OR
            WHEN "51102004" OR
            WHEN "51102003" THEN movContable.cuenta = "51102001".

            WHEN "51102202" OR
            WHEN "51102204" OR
            WHEN "51102203" THEN movContable.cuenta = "51102201".

            WHEN "51102402" OR
            WHEN "51102403" THEN movContable.cuenta = "51102401".

            WHEN "51104004" THEN movContable.cuenta = "51104001".
            
            WHEN "51102602" OR
            WHEN "51102604" OR
            WHEN "51102603" THEN movContable.cuenta = "51102601".

            WHEN "51102802" OR
            WHEN "51102804" OR
            WHEN "51102803" THEN movContable.cuenta = "51102801".

            WHEN "51103602" THEN movContable.cuenta = "51103601".
            WHEN "51106002" THEN movContable.cuenta = "51106001".
            WHEN "51109502" THEN movContable.cuenta = "51109501".
            
            WHEN "4140100102" OR
            WHEN "4140100103" OR
            WHEN "4140100104" OR
            WHEN "4140100106" THEN movContable.cuenta = "41401001".

            WHEN "42950502" OR
            WHEN "42950505" THEN movContable.cuenta = "42950501".
            WHEN "51100205" THEN movContable.cuenta = "53152001".
            WHEN "6140102227" THEN movContable.cuenta = "6140101227".

            WHEN "19043002" OR
            WHEN "19043003" THEN movContable.cuenta = "19043001".

            WHEN "24457002" THEN movContable.cuenta = "24457001".
            WHEN "25100401" THEN movContable.cuenta = "25100305".
            
            WHEN "41750103" OR
            WHEN "41750102" OR
            WHEN "41750104" THEN movContable.cuenta = "41750101".
            
            WHEN "6140103110" THEN
                movContable.cuenta = "6140101110".

            WHEN "6140103102" THEN
                movContable.cuenta = "6140101102".

            WHEN "6140103114" THEN
                movContable.cuenta = "6140101114".
                
            WHEN "6140104195" THEN
                movContable.cuenta = "6140101195".
                
            WHEN "6140103122" THEN
                movContable.cuenta = "6140101122".

            WHEN "6140104103" OR
            WHEN "51100215" OR
            WHEN "6140101103" THEN movContable.cuenta = "53152002".

            WHEN "6140104110" THEN
                movContable.cuenta = "6140101110".

            WHEN "6140104215" THEN
                movContable.cuenta = "6140101215".

            WHEN "6140104278" THEN
                movContable.cuenta = "6140101278".
                
            WHEN "6140104118" THEN
                movContable.cuenta = "6140101118".

            WHEN "6140104122" THEN
                movContable.cuenta = "6140101122".
            
            WHEN "6140104154" THEN
                movContable.cuenta = "6140101154".
            
            WHEN "6140104206" THEN
                movContable.cuenta = "6140101206".
            
            WHEN "6140104227" THEN
                movContable.cuenta = "6140101227".
            
            WHEN "6140104230" THEN
                movContable.cuenta = "6140101230".
            
            WHEN "6140104233" THEN
                movContable.cuenta = "6140101233".

            WHEN "6140104236" THEN
                movContable.cuenta = "6140101236".
            
            WHEN "6140104239" THEN
                movContable.cuenta = "6140101239".

            WHEN "6140104251" THEN
                movContable.cuenta = "6140101251".

            WHEN "6140104269" THEN
                movContable.cuenta = "6140101269".

            WHEN "6140104270" THEN
                movContable.cuenta = "6140101270".

            WHEN "6140104271" THEN
                movContable.cuenta = "6140101271".

            WHEN "6140104272" THEN
                movContable.cuenta = "6140101272".

            WHEN "6140104275" THEN
                movContable.cuenta = "6140101275".

            WHEN "86052000" THEN movContable.cuenta = "860520".
            WHEN "51106203" THEN movContable.cuenta = "51106201".
            WHEN "16250502" THEN movContable.cuenta = "16250501".
        END CASE.
    END.
END.

OUTPUT TO c:\INFO_Fodun\Leonardo\ImportarMovimientos.txt.
FOR EACH movContable NO-LOCK BREAK BY movContable.cuenta:
    IF FIRST-OF(movContable.cuenta) THEN DO:
        FIND FIRST cuentas WHERE cuentas.cuenta = movContable.cuenta NO-LOCK NO-ERROR.
        IF NOT AVAILABLE cuentas THEN
            DISPLAY movContable.cuenta FORMAT "X(12)".
    END.
END.
OUTPUT CLOSE.

/*FOR EACH movContable NO-LOCK:
    DISPLAY movContable WITH 1 COL.
END.*/


FOR EACH movContable WHERE movContable.db + movContable.cr <> 0 AND movContable.cuenta <> "" BREAK BY movContable.cuenta:
    CREATE movCont.

    /* Cuenta */
    FIND FIRST cuentas WHERE cuentas.cuenta = movContable.cuenta NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentas THEN
        DISPLAY movContable.cuenta.

    movCont.agencia = movContable.agencia.

    movCont.cuenta = movContable.cuenta.

    /* Fecha */
    movCont.fecha = movContable.fecha.

    /* Comprobante */
    movCont.comprobante = movContable.comprobante.

    /* Num_documento */
    movCont.num_Documento = movContable.num_documento.

    /* db y cr */
    movCont.db = movContable.db.
    movCont.cr = movContable.cr.

    /* nit */
    movCont.nit = movContable.nit.

    IF movCont.nit <> "" THEN DO:
        FIND FIRST clientes WHERE clientes.nit = movCont.nit NO-LOCK NO-ERROR.
        IF NOT AVAILABLE clientes THEN DO:
            CREATE clientes.
            clientes.nit = movCont.nit.
            clientes.nombre = movContable.nombre.
            clientes.tipo_cliente = 23.
        END.

        /* Anexos */
        FIND FIRST anexos WHERE anexos.nit = movCont.nit
                            AND anexos.cuenta = movCont.cuenta
                            AND anexos.cen_costos = 999
                            AND anexos.agencia = movContable.agencia
                            AND anexos.ano = 2011 NO-ERROR.
        IF NOT AVAILABLE anexos THEN DO:
            CREATE anexos.
            anexos.nit = movCont.nit.
            anexos.cuenta = movCont.cuenta.
            anexos.cen_costos = 999.
            anexos.agencia = movContable.agencia.
            anexos.ano = 2011.
        END.

        anexos.db[MONTH(movContable.fecha)] = anexos.db[MONTH(movContable.fecha)] + movCont.db.
        anexos.cr[MONTH(movContable.fecha)] = anexos.cr[MONTH(movContable.fecha)] + movCont.cr.

        /* Anexos13 */
        FIND FIRST anexos13 WHERE anexos13.nit = movCont.nit
                              AND anexos13.cuenta = movCont.cuenta
                              AND anexos13.cen_costos = 999
                              AND anexos13.agencia = movContable.agencia
                              AND anexos13.ano = 2011 NO-ERROR.
        IF NOT AVAILABLE anexos13 THEN DO:
            CREATE anexos13.
            anexos13.nit = movCont.nit.
            anexos13.cuenta = movCont.cuenta.
            anexos13.cen_costos = 999.
            anexos13.agencia = movContable.agencia.
            anexos13.ano = 2011.
        END.

        anexos13.db[MONTH(movContable.fecha)] = anexos13.db[MONTH(movContable.fecha)] + movCont.db.
        anexos13.cr[MONTH(movContable.fecha)] = anexos13.cr[MONTH(movContable.fecha)] + movCont.cr.
    END.

    /* doc_referencia */
    movCont.doc_referencia = movContable.doc_referencia.

    /* concepto */
    movCont.concepto = movContable.concepto.

    /* cen_costos */
    movCont.cen_costos = 999.

    /*DISPLAY movCont.cuenta
            movCont.fecha
            movCont.comprobante
            movCont.num_Documento
            movCont.db
            movCont.cr
            movCont.nit
            movCont.doc_referencia
            movCont.Concepto
        WITH WIDTH 200.*/
END.


FOR EACH movCont NO-LOCK:
    CREATE mov_contable.
    mov_contable.agencia = movCont.agencia.
    mov_contable.comprobante = movCont.comprobante.
    mov_contable.num_documento = movCont.num_documento.
    mov_contable.cuenta = movCont.cuenta.
    mov_contable.cen_costos = movCont.cen_costos.
    mov_contable.nit = movCont.nit.
    mov_contable.doc_referencia = movCont.doc_referencia.
    mov_contable.fec_contable = movCont.fecha.
    mov_contable.comentario = movCont.concepto.
    mov_contable.usuario = "2305".
    mov_contable.estacion = "5".
    mov_contable.db = movCont.db.
    mov_contable.cr = movCont.cr.
    mov_contable.fec_grabacion = TODAY.
    mov_contable.hora = TIME.
END.


MESSAGE "Finaliza"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
