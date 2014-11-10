void GetParams(int strat, int argc, char *argv[]);
int  ReadString(char Word[],int maxlength,FILE *THEFILE);
int  ReadSpaceString(char Word[],int maxlength,FILE *THEFILE);
void PrintAllParams(FILE *file);
int Whitespace(char next);

/* ECA: some Flockture globals needed outside stucture.c */
char gFLOCKTURE_START_FILE[301];
int gUSE_FLOCKTURE_START_CONFIG;
