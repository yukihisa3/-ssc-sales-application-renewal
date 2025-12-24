# ACOSPG99

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/ACOSPG99.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    日次業務                             *  ./
/. *   JOB-ID      :    ACOSPG99                              *  ./
/. *   JOB-NAME    :    本社／各営業所　計上ＤＴ作成         *  ./
/. ***********************************************************  ./
    PGM
/.##ﾜｰｸｱﾘｱ##./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'ACOSPG99'
    VAR       ?STEP     ,STRING*8
    VAR       ?WKSTN    ,STRING*8

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB

/.################################################################./
/.# 部門間データ抽出処理                                         #./
/.################################################################./
STEP11:

    CALL SCVMSG1.TOKELIB,PARA-('STEP11=> IDO-BUMON H')

    OVRF      FILE-ZAIKOKEI,TOFILE-TOKUHOS.TOKBLIB
    OVRF      FILE-ZAIFURIK,TOFILE-ZAIFURIK.TOKFLIB
    CALL      PGM-SKY5001B.TOKELIB,PARA-('3266')
    IF        @PGMEC    ^=   0
              THEN
              CALL SCVMSG1.TOKELIB,PARA-('IDO-BUMON HONSYA    ')
              GOTO ABEND
    END

    CALL SCVMSG1.TOKELIB,PARA-('STEP11=> IDO-BUMON S')

    OVRF      FILE-ZAIKOKEI,TOFILE-TOKUSES.TOKBLIB
    OVRF      FILE-ZAIFURIK,TOFILE-ZAIFURIK.TOKFLIB
    CALL      PGM-SKY5001B.TOKELIB,PARA-('3236')
    IF        @PGMEC    ^=   0
              THEN
              CALL SCVMSG1.TOKELIB,PARA-('IDO-BUMON SENDAI    ')
              GOTO ABEND
    END

RTN:

    DEFLIBL TOKELIB/TOKFLIB
    CALL SCVMSG1.TOKELIB,PARA-('ACOSPG99 ｾｲｼﾞｮｳ END  ')
    RETURN    PGMEC-@PGMEC

ABEND:

    DEFLIBL TOKELIB/TOKFLIB
    CALL SCVMSG1.TOKELIB,PARA-('ACOSPG99 ｲｼﾞｮｳ  END  ')
    RETURN    PGMEC-@PGMEC

```
