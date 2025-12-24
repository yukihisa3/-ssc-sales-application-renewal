/. ***********************************************************  ./
/. *     サカタのタネ殿　            　　　　　　　          *  ./
/. *   SYSTEM-NAME :    コンバート                           *  ./
/. *   JOB-ID      :    PNVPG020                             *  ./
/. *   JOB-NAME    :    商品変換テーブル倉庫変換 　          *  ./
/. ***********************************************************  ./
    PGM       (P1-?SOKCD1,P2-?SOKCD2)
    PARA      ?SOKCD1   ,STRING*2,IN,VALUE-'  '
    PARA      ?SOKCD2   ,STRING*2,IN,VALUE-'  '
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PNVPG020'
    VAR       ?STEP     ,STRING*8
    VAR       ?WKSTN    ,STRING*8
    VAR       ?NWKSTN   ,NAME
    ?NWKSTN   :=        @ORGWS
    ?WKSTN    :=        %STRING(?NWKSTN)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL   TOKFLIB/TOKELIB

/.                                                              ./
    DLTFILE   FILE-TBLWK.TOKFLIB
    CRTFILE   FILE-TBLWK.TOKFLIB,SIZE-10000,ESIZE-500,
              ORG-@SF

/.  商品変換テーブル倉庫コンバート                              ./
CNVPG020:

    ?STEP :=   'CNVPG020'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    ?MSGX :=  '***  商品変換テーブル倉庫変換  ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-TBLWK,TOFILE-TBLWK.TOKFLIB
    CALL      PGM-CNVPG020.TOKELIB,PARA-(?SOKCD1,?SOKCD2)
    IF        @PGMEC     =   4050 THEN
              GOTO RTN   END
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
