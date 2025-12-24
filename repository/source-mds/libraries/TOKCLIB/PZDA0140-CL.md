# PZDA0140

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PZDA0140.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　在庫システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    在庫管理システム    　　　           *  ./
/. *   JOB-ID      :    PZDA0140                             *  ./
/. *   JOB-NAME    :    日次振替　　　　　                   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PZDA0140'
    VAR       ?STEP     ,STRING*8
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?OPR1  :=  '　＃＃＃＃＃＃＃　振替更新処理　＃＃＃＃＃＃＃＃'
    ?OPR2  :=  '　　振替データの再更新処理を開始します。'
    ?OPR3  :=  '　　確認して下さい。'
    ?OPR4  :=  ''
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##振替ｴﾗｰﾃﾞｰﾀｺﾋﾟｰ##./
    CNVFILE FILE-TOKUE.TOKFLIB,TOFILE-TOKFHO.TOKFLIB,BF-1

/.##振替ｴﾗｰﾃﾞｰﾀ再更新処理##./
ZDA0140B:

    ?STEP :=   'ZDA0140B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TOKU,TOFILE-TOKFHO.TOKFLIB
    OVRF      FILE-TOKUE,TOFILE-TOKUE.TOKFLIB
    OVRF      FILE-ZAMZAIL1,TOFILE-ZAMZAIL1.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    CALL      PGM-ZDA0140B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
    /.##振替ｴﾗｰﾃﾞｰﾀｸﾘｱ##./
    CLRFILE   TOKFHO.TOKFLIB

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC

```
