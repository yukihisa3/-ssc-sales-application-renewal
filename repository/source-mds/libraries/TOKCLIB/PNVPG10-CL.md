# PNVPG10

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PNVPG10.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    コンバート                           *  ./
/. *   JOB-ID      :    PNVPG10                              *  ./
/. *   JOB-NAME    :    発注ファイルコンバート               *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PNVPG10'
    VAR       ?STEP     ,STRING*8
    VAR       ?WKSTN    ,STRING*8

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##発注ファイルコンバート##./
CNVPG1:

    ?STEP :=   'CNVPG1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    ?MSGX :=  '***  発注Ｆコンバート  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL TOKELIB

    OVRF FILE-ZHACHDT1,TOFILE-ZHACHDT1.TOKFLIB
    OVRF FILE-HACHEDL1,TOFILE-HACHEDL1.TOKFLIB
    OVRF FILE-HACMEIL1,TOFILE-HACMEIL1.TOKFLIB
    CALL PGM-CNVPG1.TOKELIB
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
