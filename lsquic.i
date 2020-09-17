%feature("intern_function", "chomp-lsquic");
%feature("export");
%typemap(cin) int* "(:pointer :int)";
%typemap(cin) lsxpack_strlen_t ":unsigned-int";
%typemap(cin) uint32_t ":uint32";
%typemap(cin) uint16_t ":uint16";
%typemap(cin) uint8_t ":uint8";

%insert("lisphead") %{
(in-package :lsquic)
%}

%ignore LSQUIC_DF_UA;

%include "lsquic_types.h"
%include "lsquic.h"
%include "lsxpack_header.h"

