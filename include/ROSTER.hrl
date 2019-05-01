-ifndef(_ROSTER_HRL_).
-define(_ROSTER_HRL_, true).
-record('N2O', { tok       = <<>> :: binary()  }).
-record('MUC', { dst       = <<>> :: binary()  }).
-record('P2P', { dst       = <<>> :: binary()  }).
-record('Adr', { src       = <<>> :: integer(), dst = []   :: [] | {atom(),[] | #'P2P'{} | #'MUC'{}} }).
-record('Sub', { key       = <<>> :: integer(), adr = []   :: [] | #'Adr'{} }).
-record('Pub', { key       = <<>> :: integer(), adr = []   :: [] | #'Adr'{},
                 tag       = <<>> :: binary(),  bin = <<>> :: binary() }).
-record('Ack', { lex       = <<>> :: integer() }).
-record('Nak', { key       = <<>> :: integer() }).
-record('Cut', { id        = <<>> :: binary()  }).
-record('FTP', { id        = []   :: term(),
                 sid       = []   :: term(),
                 filename  = []   :: term(),
                 meta      = []   :: term(),
                 size      = []   :: term(),
                 offset    = []   :: term(),
                 block     = []   :: term(),
                 data      = []   :: term(),
                 status    = []   :: term() }).
-endif.
