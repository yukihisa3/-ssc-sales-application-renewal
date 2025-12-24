/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    計上エラーデータ戻し処理             *  ./
/. *   JOB-ID      :    PSY80100                             *  ./
/. *   JOB-NAME    :    計上エラーデータ戻し処理             *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PSY80100'
    VAR       ?STEP   ,STRING*8

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL    TOKFLIB/TOKELIB

/.##売上作成ＦＬＧクリアー##./
TSY8010B:
    ?STEP :=   'TSY8010B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-DENJNL3,TOFILE-SHTDENL3.TOKFLIB
    OVRF      FILE-NYKFILL1,TOFILE-NYKFILL1.TOKFLIB
    OVRF      FILE-NYSFILL1,TOFILE-NYSFILL1.TOKFLIB
    OVRF      FILE-SGYFILL1,TOFILE-SGYFILL1.TOKFLIB
    OVRF      FILE-TOKU,TOFILE-TOKUHOE.TOKFLIB
    OVRF      FILE-HKYOTU,TOFILE-WORKF.TOKFLIB
    CALL      PGM-TSY8010B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##売上作成ＦＬＧクリアー##./
TSY8011B:
    ?STEP :=   'TSY8011B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-DENJNL3,TOFILE-SHTDENL3.TOKFLIB
    OVRF      FILE-NYKFILL1,TOFILE-NYKFILL1.TOKFLIB
    OVRF      FILE-NYSFILL1,TOFILE-NYSFILL1.TOKFLIB
    OVRF      FILE-SGYFILL1,TOFILE-SGYFILL1.TOKFLIB
    OVRF      FILE-TOKU,TOFILE-TOKUHOE.TOKFLIB
    OVRF      FILE-HKYOTU,TOFILE-WORKF.TOKFLIB
    CALL      PGM-TSY8011B.TOKELIB
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