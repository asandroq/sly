
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
  int ret;
  unsigned value;
  FILE *in, *out;
  char iname[512], oname[512];

  if(argc != 2) {
    fprintf(stderr, "Usage: %s <basename>\n", argv[0]);
    exit(1);
  }

  snprintf(iname, 511, "%s.fasl", argv[1]);
  iname[511] = '\0';
  snprintf(oname, 511, "%s.h", argv[1]);
  oname[511] = '\0';

  in  = fopen(iname, "r");
  if(!in) {
    fprintf(stderr, "Could not open file %s for reading.\n", iname);
    exit(1);
  }
  
  out = fopen(oname, "w");
  if(!out) {
    fclose(in);
    fprintf(stderr, "Could not open file %s for writing.\n", oname);
    exit(1);
  }

  fprintf(out, "/*\n");
  fprintf(out, "\tThis code was automatically generated from a .fasl file.\n");
  fprintf(out, "\tDo not edit it.\n");
  fprintf(out, "*/\n\n");

  fprintf(out, "static const uint8_t %s_buf[] = {\n", argv[1]);

  /* bytecode beginning */
  ret = fscanf(in, " #(");
  if(ret == EOF) {
    fclose(in);
    fclose(out);
    fprintf(stderr, "invalid input file\n");
    exit(1);
  }

  while(1) {
    ret = fscanf(in, " %u", &value);
    if(ret == 0) {
      break;
    } else if(ret == EOF) {
      fclose(in);
      fclose(out);
      fprintf(stderr, "unexpected end of input\n");
      exit(1);
    }

    fprintf(out, "%u, ", value);
  }

  fprintf(out, "};\n\n");

  return 0;
}

