module TAst = TypedAst
module Sym = Symbol

let make_ident name = TAst.Ident {sym = Sym.symbol name}

let library_functions =
  [
    (Symbol.symbol "read_integer", TAst.FunTyp {ret = TAst.Int; params = []});
    (Symbol.symbol "print_integer", TAst.FunTyp {ret = TAst.Void; params = [Param {paramname = make_ident "value"; typ = TAst.Int}]});
    (Symbol.symbol "output_string", TAst.FunTyp {ret = TAst.Void; params = [Param {paramname = make_ident "value"; typ = TAst.String}; 
    Param {paramname = make_ident "stream"; typ = TAst.Record {rname = RecordName {name = "stream"}}}]});
    (Symbol.symbol "get_stdout", TAst.FunTyp {ret = TAst.Record {rname = RecordName {name = "stream"}}; params = []});
  ]

let library_rcrds =
  [
    (Symbol.symbol "stream", TAst.Rcrd {name = TAst.Ident {sym = Sym.symbol "dummy"}; fields = []});
    (Symbol.symbol "socket", TAst.Rcrd {name = TAst.Ident {sym = Sym.symbol "dummy"}; fields = []});
    (Symbol.symbol "socket_address", TAst.Rcrd {name = TAst.Ident {sym = Sym.symbol "dummy"}; fields = []});
    (Symbol.symbol "ip_version", TAst.Rcrd {name = TAst.Ident {sym = Sym.symbol "dummy"}; fields = []});
    (Symbol.symbol "accepted_connection", TAst.Rcrd {name = TAst.Ident {sym = Sym.symbol "dummy"}; fields = []});
    (Symbol.symbol "udp_recvfrom_result", TAst.Rcrd {name = TAst.Ident {sym = Sym.symbol "dummy"}; fields = []});
    (Symbol.symbol "connection_type", TAst.Rcrd {name = TAst.Ident {sym = Sym.symbol "dummy"}; fields = []});
  ]

let rsv_rcrds : string list = 
  [
    "stream";
    "socket";
    "socket_address";
    "ip_version"; 
    "accepted_connection"; 
    "udp_recvfrom_result"; 
    "connection_type";
  ]