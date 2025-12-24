/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    PSE99000                             *  ./
/. *   JOB-NAME    :    請求データ繰越                       *  ./
/. ***********************************************************  ./
    PGM  (P1-?TORICD)
/.###ﾊﾟﾗﾒﾀ定義####./
    PARA      ?TORICD   ,STRING*8,IN,VALUE-'00000000'
/.###ﾜｰｸｴﾘｱ定義####./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSE99000'
    VAR       ?STEP     ,STRING*8

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL    TOKFLIB/TOKELIB/TOKKLIB

/.##請求データ保守##./
SSE9900I:

    ?STEP :=   'SSE9900I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    IF  ?TORICD = '99999999'  THEN
        OVRF FILE-SETGKFA2,TOFILE-SETGKFA2.TOKFLIB
    END
    IF  ?TORICD = '00000173'  THEN
        OVRF FILE-SETGKFA2,TOFILE-SETGKFB2.TOKFLIB
    END
    IF  ?TORICD = '00001041'  THEN
        OVRF FILE-SETGKFA2,TOFILE-SETGKFG2.TOKFLIB
    END
    IF  ?TORICD = '00087372'  THEN
        OVRF FILE-SETGKFA2,TOFILE-SETGKFP2.TOKFLIB
    END
    IF  ?TORICD = '00038709'  THEN
        OVRF FILE-SETGKFA2,TOFILE-SEIGK502.TOKFLIB
    END
    IF  ?TORICD = '00387091'  THEN
        OVRF FILE-SETGKFA2,TOFILE-SEIGK502.TOKFLIB
    END
    IF  ?TORICD = '00030402'  THEN
        OVRF FILE-SETGKFA2,TOFILE-SETGKFS3.TOKKLIB
    END
    IF  ?TORICD = '00304021'  THEN
        OVRF FILE-SETGKFA2,TOFILE-SETGKFS3.TOKKLIB
    END
    IF  ?TORICD = '00033189'  THEN
        OVRF FILE-SETGKFA2,TOFILE-SEIGK462.TOKFLIB
    END
    IF  ?TORICD = '00087372'  THEN
        OVRF FILE-SETGKFA2,TOFILE-SETGKFP2.TOKFLIB
    END
    IF  ?TORICD = '00087373'  THEN
        OVRF FILE-SETGKFA2,TOFILE-SETGKFP2.TOKFLIB
    END
    IF  ?TORICD = '00087374'  THEN
        OVRF FILE-SETGKFA2,TOFILE-SETGKFP2.TOKFLIB
    END
    IF  ?TORICD = '00087375'  THEN
        OVRF FILE-SETGKFA2,TOFILE-SETGKFP2.TOKFLIB
    END
    IF  ?TORICD = '00271934'  THEN
        OVRF FILE-SETGKFA2,TOFILE-SETGKFK2.TOKKLIB
    END
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL      PGM-SSE9900I.TOKELIBO,PARA-(?TORICD)
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