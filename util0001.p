/*  nombre      : util0001.p
    descripcion : reinicia repositorio
    log         : 17 oct 2007,creado,Ing. Edilberto Mariño Moya
     */
MESSAGE "ADVERTENCIA:" SKIP(2)
        "ESTE PROGRAMA REINICIA LOS LABELS DEL REPOSITORIO"
        VIEW-AS ALERT-BOX WARNING.
FOR EACH repositorio._file
    WHERE
        _file-name = "repositorio":
    FOR EACH repositorio._FIELD
        WHERE
            _file-recid = RECID(_file)
        AND (_field-name BEGINS "char"
             OR _field-name BEGINS "int"
             OR _field-name BEGINS "dec"
             OR _field-name BEGINS "dat"
             OR _field-name BEGINS "log") :
        _label = _field-name.

    END.
END.
