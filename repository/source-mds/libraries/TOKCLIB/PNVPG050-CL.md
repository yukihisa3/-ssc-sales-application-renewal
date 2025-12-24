# PNVPG050

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PNVPG050.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ殿　            　　　　　　　          *  ./
/. *   SYSTEM-NAME :    コンバート                           *  ./
/. *   JOB-ID      :    PNVPG050                             *  ./
/. *   JOB-NAME    :    売上データ　自社得意先コード変換     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PNVPG050'
    VAR       ?STEP     ,STRING*8
    VAR       ?WKSTN    ,STRING*8
    VAR       ?NWKSTN   ,NAME
    ?NWKSTN   :=        @ORGWS
    ?WKSTN    :=        %STRING(?NWKSTN)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.  売上データ　自社得意先変換                    ./
CNVPG050:

    ?STEP :=   'CNVPG050'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    ?MSGX :=  '***  売上データ　自社得意先変換  ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENL1,TOFILE-SHTDENL1.TOKFLIB
    CALL      PGM-CNVPG050.TOKELIB
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
