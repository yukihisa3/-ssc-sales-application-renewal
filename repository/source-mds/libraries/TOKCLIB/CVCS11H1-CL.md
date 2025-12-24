# CVCS11H1

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/CVCS11H1.CL`

## ソースコード

```jcl

/.ホーマック関東　請求データ　配信処理./
    PGM

    DEFLIBL CVCSOBJ/CVCSLBT

    SBMJOB JOB-SEIKYU,JOBD-CVCS.XUCL,JOBK-@B,
           PGM-CVCS11H2.TOKCLIB,VSIZE-6,RSIZE-16,LOG-@YES!
           1024,LIBL-CVCSLBT/CVCSOBJ/TOKELIB

    RETURN    PGMEC-@PGMEC

```
