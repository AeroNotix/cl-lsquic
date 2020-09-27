%feature("intern_function", "chomp-lsquic");
%feature("export");
%typemap(cin) int * "(:pointer :int)";
%typemap(cin) size_t ":unsigned-int";
%typemap(cin) lsxpack_strlen_t ":uint16";
%typemap(cin) uint32_t ":uint32";
%typemap(cin) uint16_t ":uint16";
%typemap(cin) uint8_t ":uint8";

%insert("lisphead") %{
(in-package :lsquic)
%}
%include "lsquic-helpers.h"
