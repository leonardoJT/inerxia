DISABLE TRIGGERS FOR LOAD OF mov_contable.

FOR EACH mov_contable WHERE mov_contable.nit = "899999061-9":
    mov_contable.nit = "899999061".
END.

FOR EACH anexos WHERE anexos.nit = "899999061-9":
    anexos.nit = "899999061".
END.

FOR EACH anexos13 WHERE anexos13.nit = "899999061-9":
    anexos13.nit = "899999061".
END.
