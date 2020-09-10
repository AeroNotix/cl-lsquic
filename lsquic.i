%feature("intern_function", "lispify");

%typemap(cin) int* "(:pointer :int)";

%insert("lisphead") %{
(in-package :lsquic)
%}

%ignore LSQUIC_DF_UA;

%include "lsquic_types.h"
%include "lsquic.h"
%include "lsxpack_header.h"

