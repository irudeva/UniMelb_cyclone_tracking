/* Program: binswap */

/* Purpose: Performs byte-swapping operations on a binary data file
   i.e. a number comprising 4 bytes 0123 -> 3210.
   This program will allow a binary file created on a Sun to be
   converted to a form suitable for a PC i.e. the output file can
   be read as an unformatted file using g77 on a PC. 
   Option -c is used to process conmap files containing up
   to 50 maps - see define statements for maxmaps. */

/* Author: Kevin Keay  Date: Jan 25 2000 */

#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>       /* Needed for optind, optarg in g77 
                             on the PC - remove for the Sun */

#define maxlats 200       /* Max. no. of lats */
#define maxlons 400       /* Max. no. of lons */
#define maxmaps 50        /* Max. no. of maps in the file */

/* Max. file size in 4 byte words - this is expressed in conmap
   terms for convenience only */
#define maxsize maxmaps*(14 +80/4 +maxlats +maxlons +maxlats*maxlons)

/* Functions */

void prusage(char progname[80]);
void swpword(char c1[4]);
void swpword2(char c2[4],char c1[4]);
int swpint(char cword[4]);
float swpfloat(char cword[4]);
void scpy(char c2[4], char c1[4]);
void i2char(int ival, char c[4]);
void f2char(float fval, char c[4]);
void getconmap();
void putconmap();
void getcmapsize();

/* Global variables */

    int idbg = 0;           /* Debug switch - overriddeb by -d1 option*/
    int icmap= 0;           /* =1 conmap file 0= other binary file */


    char dcmp[maxsize][4];  /* Character array for conmap file data */
    int wpos;               /* Position of a 4 byte value (character
                               subarray) in dcmp */

    int nwords;             /* No. of 4 byte words in binary file */
    int wsize;              /* Size of individual map (in 4 byte words)*/
    int nmaps;              /* No. of maps in file - assumed to be of the
			       same size i.e. wsize words */
    int readsize;           /* Size of file actually read */
    int writesize;          /* Size of file to write */

    int opt;                /* For command-line arguments */

/* conmap format (with formatting counters) */

    int inlats1;
    int nlats;
    int inlats2;
    int ixlats1;
    float xlats[maxlats];
    int ixlats2;
    int inlons1;
    int nlons;
    int inlons2;
    int ixlons1;
    float xlons[maxlons];
    int ixlons2;
    int ihead1;
    char head[81]; /* 80 +1 for end-of-string marker */
    int ihead2;
    int idat1;
    float cmpdat[maxlats][maxlons]; 
    int idat2;

int main(int argc, char **argv) /* int main()  */
{

/* const char SUNCONMAPFile[] = "test.cmp"; */

    int i,j,k;
    char w[5];       /* 4 byte character variable (debug) */

    FILE *infile;    /* Input file */
    FILE *outfile;   /* Output file */
    char *in_file;
    char *out_file;

/* Usage */

    if (argc == 1) {
       prusage(argv[0]);
       exit(1); 
    }

/* Process command-line arguments */

    while ((opt = getopt(argc, argv, "d:c")) != EOF)
       switch (opt) {
 	  case 'd':
   	     idbg = atoi(optarg); /* e.g. -d 1 */
   	     break;
 	  case 'c':
             icmap= 1; /* Conmap data - multiple maps allowed */
	     break;
	  default:
             prusage(argv[0]);
             exit(1);
      }
	
       in_file= argv[optind]; 
       out_file= argv[++optind];  

   
    if (idbg > 0) {
       fprintf(stderr,"idbg= %d  icmap= %d\n",idbg,icmap);
    }

/* Open input binary file */

    fprintf(stderr,"Input file:  %s\n",in_file);
    fprintf(stderr,"Output file: %s\n",out_file);

    infile= fopen(in_file, "rb");  /* Open binary file */

    if(infile == NULL) {
      printf("Cannot open binary file: %s\n",in_file);
      exit(1);
    }

    readsize= fread((char *)&dcmp,1,sizeof(dcmp),infile);

    nwords= readsize/4;  /* No. of 4 byte words in input file */

    if (idbg > 0) {
       fprintf(stderr,"sizeof= %d\n",sizeof(dcmp));  
       fprintf(stderr,"readsize= %d\n",readsize);
       fprintf(stderr,"nwords= %d\n",nwords);
    } 

    fclose(infile); 

/* Get conmap file size - could be multiple maps (nmaps), assumed to
   be of the same size (wsize) */

    if (icmap == 1) {
       getcmapsize();

       if (idbg == 0) {
           fprintf(stderr,"Map size: %d words  (%d bytes)\n",wsize,wsize*4);
           fprintf(stderr,"No. of maps: %d\n",nmaps);
       }
       else if (idbg > 0) {
           fprintf(stderr,"wsize= %d  (%d bytes)\n",wsize,wsize*4);
           fprintf(stderr,"nmaps= %d\n",nmaps);
       }
    }

/* Process binary data - fix up conmap headers afterwards */

    wpos= 0;
    while (wpos < nwords) {
       swpword(dcmp[wpos]);
       ++wpos;   
    }

/* Fix up conmap headers */

    if (icmap == 1) {
       head[80]= '\0';  /* Set last character to end-of-string marker 
                           (null) - this is only for writing to screen
                            - the 81st character is not output */
       wpos= nlats +nlons +11; /* Point to first word of first conmap header */
       i= 1;
       while (i <= nmaps) {
          j=0;
          while (j < 20) {
             swpword(dcmp[wpos+j]); /* Swap word back to original order */
             if (idbg == 2) {
                scpy(w,dcmp[wpos+j]);
                w[4]= '\0';
                fprintf(stderr,"j= %d w=%s\n",j,w);
             }
                                    /* Debug: Write out header to screen */
             k=0;
             while (k < 4) {
                head[4*j+k]= dcmp[wpos+j][k];
                ++k;
             }
             ++j;
          }

          fprintf(stderr,"Map: %d\n%s\n",i,head);

          wpos += wsize;       /* Point to next conmap header */
          ++i;
       }
    }

/* Write out binary file */

    outfile= fopen(out_file, "wb");  /* Open binary file */
    writesize= fwrite((char *)&dcmp,1,readsize,outfile);
    if (idbg > 0) {
       fprintf(stderr,"writesize= %d\n",writesize);
    }
    fclose(outfile);

    exit(0);
}

/* )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
   ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((( */

void prusage(char progname[80])
/* Purpose: Print usage */
{
    fprintf(stderr, "Usage: %s [-d debuglevel] [-c] inbinfile outbinfile\n",progname);
    fprintf(stderr, "          d: Debug level 0-2 (default: 0)\n");
    fprintf(stderr, "          c: conmap file (headers will be fixed)\n");
    fprintf(stderr, " \n");
    fprintf(stderr, "Examples:\n");
    fprintf(stderr, "  (1) %s -c pmsl.sun.cmp pmsl.pc.cmp\n",progname);
    fprintf(stderr, "  (2) %s -d1 pmsl.sun.cmp pmsl.pc.cmp\n",progname);
    fprintf(stderr, " \n"); 
    fprintf(stderr, "Author: Kevin Keay  Date: Jan 25 2000");
    fprintf(stderr, " \n"); 

    return;
}
/* )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
   ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((( */

int swpint(char cword[4])
/* Purpose: Byte-swap the contents of character variable cword and
   return as an integer value */
{
    union { char cval[4]; int ival; float fval;} v ;

    int k;

    k=0;
    while (k < 4) {
      v.cval[k]= cword[3-k]; 
      ++k;
    }
    return v.ival;
}

/* )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
   ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((( */

float swpfloat(char cword[4])
/* Purpose: Byte-swap the contents of character variable cword and
   return as a floating value */
{
    union { char cval[4]; int ival; float fval;} v ;

    int k;

    k=0;
    while (k < 4) {
      v.cval[k]= cword[3-k]; 
      ++k;
    }
    return v.fval;
}

/* )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
   ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((( */

void scpy(char c2[4],char c1[4])
/* Purpose: Copy string c1 to string c2 */
{
    int k=0;

    while (k < 4) {
      c2[k]= c1[k]; 
      ++k;
    }
    return;
}

/* )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
   ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((( */

void i2char(int ival, char c[4])
/* Purpose: Convert integer variable ival to character c */
{
    union { char cval[4]; int ival; float fval;} v ;

    int k=0;

    v.ival= ival;
     
    while (k < 4) {
      c[k]= v.cval[k]; 
      ++k;
    }
    return;
}

/* )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
   ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((( */

void f2char(float fval, char c[4])
/* Purpose: Convert floating variable fval to character c */
{
    union { char cval[4]; int ival; float fval;} v ;
     
    int k=0;

    v.fval= fval;
     
    while (k < 4) {
      c[k]= v.cval[k]; 
      ++k;
    }
    return;
}

/* )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
   ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((( */

void getconmap()
/* Purpose: Extract conmap file data from character array dcmp and perform
   byte-swapping operations i.e. order 0123 -> 3210 */
{
    int i,j,k;

/* Read nlats */

/* Next line - initialise wpos in main program so that multiple maps
   can be processed */
/*    wpos= 0; */  /* Initialise position in array dcmp */

    inlats1= swpint(dcmp[wpos]);
    nlats= swpint(dcmp[++wpos]);   /* Note: ++wpos => wpos= wpos +1 */
/*    fprintf(stderr,"getconmap: nlats= %d\n",nlats);  */
    inlats2= swpint(dcmp[++wpos]);

/* Convert lats array */

    ixlats1= swpint(dcmp[++wpos]);

    wpos= wpos +1;
    j=0;
    while (j < nlats) {
      xlats[j]= swpfloat(dcmp[j+wpos]);
/*      fprintf(stderr,"xlats (%d) = %8.4f\n",j,xlats[j]); */
      ++j;
    }

    wpos += nlats;  /* wpos= wpos +nlats */
    ixlats2= swpint(dcmp[wpos]);

/* Read nlons */

    inlons1= swpint(dcmp[++wpos]);
    nlons= swpint(dcmp[++wpos]);
/*    fprintf(stderr,"getconmap: nlons= %d\n",nlons);  */
    inlons2= swpint(dcmp[++wpos]);

/* Convert lons array */

    ixlons1= swpint(dcmp[++wpos]);

    wpos= wpos +1;
    j=0;
    while (j < nlons) {
      xlons[j]= swpfloat(dcmp[j+wpos]);
/*      fprintf(stderr,"xlons (%d) = %8.4f\n",j,xlons[j]); */
      ++j;
    }

    wpos += nlons;
    ixlons2= swpint(dcmp[wpos]);

/* Read header - no conversion but have to extract individual characters */

    ihead1= swpint(dcmp[++wpos]);
/*    fprintf(stderr,"ihead1= %d\n",ihead1); */

    wpos= wpos +1;
    j=0;
    while (j < 20) {
      k=0;
      while (k < 4) {
        head[4*j+k]= dcmp[j+wpos][k];
        ++k;
      }
      ++j;
    }
    
    head[80]= '\0';  /* Set last character to end-of-string marker 
                        (null) - this is only for writing to screen
                        - the 81st character is not output */

    if (idbg > 0) {
       fprintf(stderr,"getconmap: head= %s\n",head);
    }

    wpos += 20;
    ihead2= swpint(dcmp[wpos]);

/* Convert conmap (lon,lat) data
   Note:  This is stored differently to Fortran
   Reverse the order of the subscripts i.e. (lon,lat) -> [lat][lon]
*/

    idat1= swpint(dcmp[++wpos]);

    wpos= wpos +1;
    k=0;
    i=0;
    while (i < nlats) {
    j=0;
      while (j < nlons) {
        cmpdat[i][j]= swpfloat(dcmp[k+wpos]);
        ++k;
/*        fprintf(stderr,"dat (%d %d) = %10.6f\n",i,j,cmpdat[i][j]); */
        ++j;
      }
    ++i;
    }

    wpos += nlons*nlats;
    idat2= swpint(dcmp[wpos]);

/* This map has now been read - set wpos for next map */
    ++wpos;

    return;
}

/* )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
   ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((( */

void putconmap()
/* Purpose: Put byte-swapped data back into array dcmp for output - convert
   all values to character type first */
{
    int i,j,k;

    wpos -= wsize;  /* Point back to start of the current map */

    i2char(inlats1,dcmp[wpos]);
    i2char(nlats,dcmp[++wpos]);
    i2char(inlats2,dcmp[++wpos]);
    i2char(ixlats1,dcmp[++wpos]);
    wpos= wpos +1;
    j=0;
    while (j < nlats) {
      f2char(xlats[j],dcmp[j+wpos]);
      ++j;
    }
    wpos += nlats;
    i2char(ixlats2,dcmp[wpos]);
    i2char(inlons1,dcmp[++wpos]);
    i2char(nlons,dcmp[++wpos]);
    i2char(inlons2,dcmp[++wpos]);
    i2char(ixlons1,dcmp[++wpos]);
    wpos= wpos +1;
    j=0;
    while (j < nlons) {
      f2char(xlons[j],dcmp[j+wpos]);
      ++j;
    }
    wpos += nlons;
    i2char(ixlons2,dcmp[wpos]);

    i2char(ihead1,dcmp[++wpos]);

    wpos= wpos +1;
    j=0;
    while (j < 20) {
      k=0;
      while (k < 4) {
        dcmp[j+wpos][k]= head[4*j+k];
        ++k;
      }
      ++j;
    }

    wpos += 20;
    i2char(ihead2,dcmp[wpos]);

    i2char(idat1,dcmp[++wpos]);

    wpos= wpos +1;
    k=0;
    i=0;
    while (i < nlats) {
    j=0;
      while (j < nlons) {
        f2char(cmpdat[i][j],dcmp[k+wpos]);
        ++k;
        ++j;
      }
    ++i;
    }

    wpos += nlons*nlats;
    i2char(idat2,dcmp[wpos]);

    wpos++; /* Position is now restored to that on entry 
               i.e. at start of next map */

    return;
}
/* )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
   ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((( */

void getcmapsize()
/* Purpose: Extract nlats and nlons and thus compute conmap file size
   - this is used to handle multiple maps (assumed to be the same size) */
{
    int i,j,k;

/* Read nlats */

    wpos= 0;  /* Initialise position in array dcmp */

    nlats= swpint(dcmp[++wpos]);   /* Note: ++wpos => wpos= wpos +1 */
    fprintf(stderr,"getcmapsize: nlats= %d\n",nlats); 

    wpos += 1 +1 +nlats +1 +1;

    nlons= swpint(dcmp[++wpos]);
    fprintf(stderr,"getcmapsize: nlons= %d\n",nlons); 

    wsize= 14 +80/4 +nlats +nlons +nlats*nlons; /* Map size in words */
    nmaps= readsize/(wsize*4); /* No. of maps */
    
    return;
}
/* )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
   ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((( */

void swpword(char c1[4])
/* Purpose: Byte-swap character variable c1 and overwrite c1 */
{
    int k=0;
    char c2[4];

    while (k < 4) {
      c2[k]= c1[3-k]; 
      ++k;
    }
    scpy(c1,c2); /* Overwrite  c1 */

    return;
}

/* )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
   ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((( */

void swpword2(char c2[4],char c1[4])
/* Purpose: Byte-swap character variable c1 and save in c2 */
{
    int k=0;

    while (k < 4) {
      c2[k]= c1[3-k]; 
      ++k;
    }

    return;
}

/* )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
   ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((( */
