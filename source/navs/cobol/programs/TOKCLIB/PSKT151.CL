/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    PSKT151                              *  ./
/. *   JOB-NAME    :    伝票_　付番入力                     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSKT151 '
    VAR       ?STEP     ,STRING*8
    DEFLIBL TOKELIB/TOKFLIB
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL


/.  伝票_　付番入力                                            ./
OSKT151:

/.  ASSIGN FILE-OSKT150.TOKFLIB!@XCL
    IF        @PGMEC    ^=   0    THEN  GOTO ERR END  ./
    ?STEP :=   'OSKT151 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-DENJNL2,TOFILE-SHTDENL2.TOKFLIB
    OVRF      FILE-DENJNL15,TOFILE-SHTDENL3.TOKFLIB
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    CALL      PGM-OSKT151.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
              ELSE
              GOTO RTN END

ERR:
    PAUSE MSG-'他の端末で付番処理中です　　　　　　'
    GOTO OSKT151

RTN:

    RELEASE FILE-JYOKEN1.TOKFLIB!@XCL
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    RELEASE FILE-JYOKEN1.TOKFLIB!@XCL
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