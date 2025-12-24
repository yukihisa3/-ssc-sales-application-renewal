# ACOSJOBS

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/ACOSJOBS.CL`

## ソースコード

```jcl
/.***************************************************************./
/.         ＧＰ６０００／５２０                                  ./
/.         サカタのタネ　システム始動プログラム                  ./
/.                                      1999.05.22 NAV-ASSIST    ./
/.***************************************************************./

/.##ＡＣＯＳ計上データ作成##./
/. CALL ACOSPG1.TOKELIB     ./
/.##ＡＣＯＳ計上データ消込ＪＯＢ起動##./
   SBMJOB JOB-ACOSCHK,JOBD-SKTJOBD.XUCL,JOBK-@B,
          PGM-PKY33010.TOKELIB,LOG-@YES!1024,PGMEL-@I/@L/@S/@T
RTN:

    RETURN PGMEC-@PGMEC

```
