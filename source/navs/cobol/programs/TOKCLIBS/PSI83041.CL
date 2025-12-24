/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    支払照合　ホーマックＷＥＢ           *  ./
/. *   JOB-ID      :    PSI83041                             *  ./
/. *   JOB-NAME    :    不照合リスト作成                     *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSI83041'
    VAR       ?STEP     ,STRING*8

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.##ＳＯＲＴ（取引先＋伝票番号順）##./
SORT1:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT      INFILE-SIHARASS.TOKKLIB,INRL-100,INBF-40,
              OUTFILE-SIHARASS.TOKKLIB,OUTBF-40,
              KEY-1!8!CA,
              KEY1-22!9!CA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##不照合リスト01427##./
SSI83041:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASS.TOKKLIB
    OVRF      FILE-SETGK831,TOFILE-SETGK841.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8304L.TOKELIBO,PARA-('00001427')
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##不照合リスト14272##./
SSI83042:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASS.TOKKLIB
    OVRF      FILE-SETGK831,TOFILE-SETGK841.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8304L.TOKELIBO,PARA-('00014272')
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##不照合リスト14273##./
SSI83043:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SIHARASF,TOFILE-SIHARASS.TOKKLIB
    OVRF      FILE-SETGK831,TOFILE-SETGK841.TOKKLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSI8304L.TOKELIBO,PARA-('00014273')
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