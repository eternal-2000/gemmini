#include "gentools/parseTrans.h"

transOpt parseTrans(char* const c){
  
  switch (tolower(c[0])){
  case 'n':
    return NoTrans;
  case 't':
    return Trans;
  default:
    fprintf(stderr, "Invalid transpose argument: %s\n", c);
    exit(EXIT_FAILURE);
  }
}
