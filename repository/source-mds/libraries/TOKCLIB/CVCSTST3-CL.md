# CVCSTST3

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/CVCSTST3.CL`

## ソースコード

```jcl
    PGM
    VAR ?CHK      ,STRING*8,VALUE-'0'
    ACTMSGQ MAXRMSG-50
    DEFLIBL TOKELIB/TOKFLIB
CVCSTST1:
    OVRF      FILE-JHMJIKF,TOFILE-JHMJIKL1.TOKFLIB
    CALL      PGM-SCV0030B.TOKELIB,PARA-(?CHK)
    IF        @PGMEC    ^=   0    THEN
              SNDMSG '## ABEND ##',TO-XCTL.@ORGPROF
    END
    SNDMSG ?CHK,TO-XCTL.@ORGPROF
    RETURN    PGMEC-@PGMEC

```
