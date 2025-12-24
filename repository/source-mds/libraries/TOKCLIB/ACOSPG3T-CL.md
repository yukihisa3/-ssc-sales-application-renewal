# ACOSPG3T

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/ACOSPG3T.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    ACOSPG3                             *  ./
/. *   JOB-NAME    :    売上作成ＦＬＧクリアー               *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'ACOSPG3'
    VAR       ?STEP   ,STRING*8
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##エラーデータ振分##./
SKY2201B:
    ?STEP :=   'SKY2201B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TOKUERR,TOFILE-TOKUERR.TOKFLIB
    OVRF      FILE-TOK00,TOFILE-TOKUHOE.TOKFLIB
    OVRF      FILE-TOK01,TOFILE-TOKUFKE.TOKFLIB
    OVRF      FILE-TOK02,TOFILE-TOKUSEE.TOKFLIB
    OVRF      FILE-TOK03,TOFILE-TOK03.TOKFLIB
    OVRF      FILE-TOK04,TOFILE-TOKUOKE.TOKFLIB
    OVRF      FILE-TOK05,TOFILE-TOKUHKE.TOKFLIB
    OVRF      FILE-TOK06,TOFILE-TOK06.TOKFLIB
    OVRF      FILE-TOK07,TOFILE-TOK07.TOKFLIB
    OVRF      FILE-TOK08,TOFILE-TOK08.TOKFLIB
    OVRF      FILE-TOK09,TOFILE-TOKUOSE.TOKFLIB
    CALL      PGM-SKY2201B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

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
