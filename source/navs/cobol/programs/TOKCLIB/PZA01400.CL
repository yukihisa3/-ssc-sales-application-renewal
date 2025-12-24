/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :                                         *  ./
/. *   JOB-ID      :    PNI00100                                *  ./
/. *   JOB-NAME    :    日次更新                             *  ./
/. *               :    売上日次更新                         *  ./
/. *               :    在庫日時更新を行う                   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PNI00100'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,STRING*8
    VAR       ?JBNM   ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID   ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGNM   ,STRING*24!MIXED            /.ＰＧ漢字名 ./
    VAR       ?PGID   ,STRING*10                  /.ＰＧＩＤ　 ./

/.##仕入計上##./
STEP1:

    ?STEP :=   'SZA0140B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-NYKFILF,TOFILE-NYKFILL1.TOKFLIB
    OVRF      FILE-HACHEDF,TOFILE-HACHEDL1.TOKFLIB
    OVRF      FILE-HACMEIF,TOFILE-HACMEIL1.TOKFLIB
    OVRF      FILE-SHOTBL4,TOFILE-SHOTBL4.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TOKU,TOFILE-TOKUZ.TOKFLIB
    CALL      PGM-SZA0140B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##入出庫計上##./
STEP2:

    ?STEP :=   'SZA0200B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-NYSFILF,TOFILE-NYSFILL1.TOKFLIB
    OVRF      FILE-SGYFILF,TOFILE-SGYFILL1.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-TOKU,TOFILE-TOKUZ.TOKFLIB
    CALL      PGM-SZA0200B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN:  /.資源の解放./

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

    DMPWSLOG WKSTN-WKSTN001,FROM-@NEWACT

    RETURN    PGMEC-@PGMEC
