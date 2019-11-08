DEFINE BUFFER ftbuf FOR _file-trig.
DEFINE VARIABLE savevnt LIKE _file-trig._event.

{incluido\VARIABLE.i}

/* REPEAT PRESELECT EACH _file-trig WHERE _file-trig._trig-crc NE ?: */
REPEAT PRESELECT EACH _file-trig WHERE _file-trig._proc-name NE ""
                                   AND _file-trig._proc-name NE ?:
    FIND NEXT _file-trig.

    DISPLAY "Compiling:" _file-trig._proc-name.
    COMPILE VALUE(_file-trig._proc-name) SAVE NO-ERROR.

    IF NOT COMPILER:ERROR THEN DO:
        RCODE-INFO:FILE-NAME = _proc-name.
        CREATE ftbuf.
        ASSIGN ftbuf._proc-name  = _file-trig._proc-name
               ftbuf._file-recid = _file-trig._file-recid
               savevnt           = _file-trig._event
               ftbuf._override   = _file-trig._override
               ftbuf._trig-crc   = RCODE-INFO:CRC-VALUE.
        DELETE _file-trig.
        ASSIGN ftbuf._event      = savevnt.
    END.
    ELSE MESSAGE "Couldn't compile:" _file-trig._proc-name.
END.
