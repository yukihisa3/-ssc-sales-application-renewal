/. ***********************************************************  ./
/. *     　　　　　　サカタのタネ_　殿　　　　        　　　*  ./
/. *   SYSTEM-NAME : 直送データ請求　　　                    *  ./
/. *   JOB-ID      : PSE00400                                *  ./
/. *   JOB-NAME    : 請求データ作成　　　　　　　　          *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSE00400'
    VAR       ?STEP     ,STRING*8

    ?MSGX :=  '***  '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##直送データ抽出##./
SSE0050B:

    ?STEP :=   'SSE0050B'
    ?MSGX :=  '***  '  && ?STEP   &&   '            ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ZNYUKDT,TOFILE-ZNYUKDT.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-OUTF,TOFILE-OUTF.SKTFLIB
    CALL      SSE0050B.TOKELIB
    IF     @PGMEC   ^=   0        THEN
           GOTO ABEND END

/.##ソート##./
 SORT01:

    ?STEP :=   'SORT'
    ?MSGX :=  '***  '  && ?STEP   &&   '            ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT   INFILE-OUTF.TOKFLIB,INRL-200,
           OUTFILE-OUTF.TOKFLIB,RCD-4,
           KEY-32!8!DA,KEY1-15!9!DA,KEY2-1!2!DA,
           RCDL-@DSP
    IF     @PGMEC   ^=   0        THEN
           GOTO ABEND END

/.##請求データ作成##./
 SSE0040B:

    ?STEP :=  'SSE0040B'
    ?MSGX :=  '***  '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ZNYUKDTF,TOFILE-OUTF.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-SEIGK1,TOFILE-SETGKFA1.TOKFLIB
    CALL      PGM-SSE0040B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN:

    ?MSGX :=  '***  '  && ?PGMID  &&   ' END    ***'
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