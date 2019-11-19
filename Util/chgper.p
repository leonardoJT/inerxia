for each _file:
    assign  _can-read = "*"
            _can-create = "*"
            _can-write = "*"
            _can-delete = "*"
            _can-load = "*"
            _can-dump = "*".
     for each _field OF _file:
         assign _can-read = "*"
                _can-write = "*".
     end.
end. 
