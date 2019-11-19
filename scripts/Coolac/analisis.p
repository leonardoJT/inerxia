DEFINE TEMP-TABLE mayo
    FIELD a AS CHARACTER
    FIELD b AS CHARACTER
    FIELD c AS CHARACTER
    FIELD d AS CHARACTER
    FIELD e AS CHARACTER
    FIELD f AS CHARACTER
    FIELD g AS CHARACTER
    FIELD h AS CHARACTER
    FIELD i AS CHARACTER
    FIELD j AS CHARACTER
    FIELD k AS CHARACTER
    FIELD l AS CHARACTER
    FIELD m AS CHARACTER
    FIELD n AS CHARACTER
    FIELD o AS CHARACTER
    FIELD p AS CHARACTER
    FIELD q AS CHARACTER
    FIELD r AS CHARACTER
    FIELD s AS CHARACTER
    FIELD t AS CHARACTER
    FIELD u AS CHARACTER
    FIELD v AS CHARACTER
    FIELD w AS CHARACTER
    FIELD X AS CHARACTER
    FIELD Y AS CHARACTER
    FIELD z AS CHARACTER
    FIELD aa AS CHARACTER
    FIELD ab AS CHARACTER
    FIELD ac AS CHARACTER
    FIELD ad AS CHARACTER
    FIELD ae AS CHARACTER
    FIELD af AS CHARACTER
    FIELD ag AS CHARACTER
    FIELD ah AS CHARACTER
    FIELD ai AS CHARACTER
    FIELD aj AS CHARACTER
    FIELD ak AS CHARACTER
    FIELD al AS CHARACTER
    FIELD am AS CHARACTER
    FIELD an AS CHARACTER
    FIELD ao AS CHARACTER
    FIELD ap AS CHARACTER
    FIELD aq AS CHARACTER
    FIELD ar AS CHARACTER.

DEFINE TEMP-TABLE junio
    FIELD a AS CHARACTER
    FIELD b AS CHARACTER
    FIELD c AS CHARACTER
    FIELD d AS CHARACTER
    FIELD e AS CHARACTER
    FIELD f AS CHARACTER
    FIELD g AS CHARACTER
    FIELD h AS CHARACTER
    FIELD i AS CHARACTER
    FIELD j AS CHARACTER
    FIELD k AS CHARACTER
    FIELD l AS CHARACTER
    FIELD m AS CHARACTER
    FIELD n AS CHARACTER
    FIELD o AS CHARACTER
    FIELD p AS CHARACTER
    FIELD q AS CHARACTER
    FIELD r AS CHARACTER
    FIELD s AS CHARACTER
    FIELD t AS CHARACTER
    FIELD u AS CHARACTER
    FIELD v AS CHARACTER
    FIELD w AS CHARACTER
    FIELD X AS CHARACTER
    FIELD Y AS CHARACTER
    FIELD z AS CHARACTER
    FIELD aa AS CHARACTER
    FIELD ab AS CHARACTER
    FIELD ac AS CHARACTER
    FIELD ad AS CHARACTER
    FIELD ae AS CHARACTER
    FIELD af AS CHARACTER
    FIELD ag AS CHARACTER
    FIELD ah AS CHARACTER
    FIELD ai AS CHARACTER
    FIELD aj AS CHARACTER
    FIELD ak AS CHARACTER
    FIELD al AS CHARACTER
    FIELD am AS CHARACTER
    FIELD an AS CHARACTER
    FIELD ao AS CHARACTER
    FIELD ap AS CHARACTER
    FIELD aq AS CHARACTER
    FIELD ar AS CHARACTER.

DEFINE TEMP-TABLE analisis
    field cedula AS CHARACTER
    field num_credito AS CHARACTER
    field mayo_provision_k AS DECIMAL
    field mayo_provision_i AS DECIMAL
    field junio_provision_k AS DECIMAL
    field junio_provision_i AS DECIMAL
    INDEX idx cedula num_credito.

INPUT FROM d:\leonardo\junio.csv.
REPEAT :
    CREATE mayo.
    IMPORT DELIMITER ";" mayo.
END.
INPUT CLOSE.

INPUT FROM d:\leonardo\julio.csv.
REPEAT :
    CREATE junio.
    IMPORT DELIMITER ";" junio.
END.
INPUT CLOSE.

FOR EACH mayo NO-LOCK:
    CREATE analisis.
    analisis.cedula = mayo.b.
    analisis.num_credito = mayo.e.
    analisis.mayo_provision_k = DECIMAL(mayo.v).
    analisis.mayo_provision_i = DECIMAL(mayo.w).
END.

FOR EACH junio NO-LOCK:
    FIND FIRST analisis WHERE analisis.cedula = junio.b AND analisis.num_credito = junio.e NO-LOCK NO-ERROR.
    IF AVAILABLE analisis THEN DO:
        analisis.junio_provision_k = DECIMAL(junio.v).
        analisis.junio_provision_i = DECIMAL(junio.w).
    END.
    ELSE DO:
        CREATE analisis.
        analisis.cedula = junio.b.
        analisis.num_credito = junio.e.
        analisis.junio_provision_k = DECIMAL(junio.v).
        analisis.junio_provision_i = DECIMAL(junio.w).
    END.
END.

OUTPUT TO d:\leonardo\analisis.csv.
FOR EACH analisis NO-LOCK:
    EXPORT DELIMITER ";" analisis.
END.
OUTPUT CLOSE.
