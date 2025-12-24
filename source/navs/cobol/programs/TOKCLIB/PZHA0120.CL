/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    在庫管理                             *  ./
/. *   JOB-ID      :   PZHA0120                              *  ./
/. *   JOB-NAME    :    発注残リスト　                       *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PZHA0120'
    VAR       ?STEP     ,STRING*8
    VAR       ?WKSTN    ,STRING*8
    VAR       ?NWKSTN   ,NAME
    ?NWKSTN    :=       @ORGWS
    ?WKSTN    :=        %STRING(?NWKSTN)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL


/.  発注残リスト抽出                                          ./
ZHA0120B:
    DEFLIBL    TOKFLIB/TOKELIB/TOKFLIB
    ?STEP :=   'ZHA0120B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-ZHACHDT1,TOFILE-ZHACHDT1.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-ZSHIMS1,TOFILE-ZSHIMS1.TOKFLIB
    OVRF      FILE-ZOUTF,TOFILE-ZOUTF.TOKFLIB
    CALL      PGM-ZHA0120B.TOKELIB,PARA-(?WKSTN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.  ＳＯＲＴ                                                    ./
SORT01:
    ?STEP :=   'SORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT       INFILE-ZOUTF.TOKFLIB,INRL-200,INBF-5,
               OUTFILE-ZOUTF.TOKFLIB,OUTBF-5,RCD-4,
               KEY-25!8!CA,KEY1-154!2!CA,KEY2-3!7!DA,KEY3-13!2!DA,
               RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
/.  発注残リスト出力                                          ./
ZHA0125B:
    ?STEP :=   'ZHA0125B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ZOUTF,TOFILE-ZOUTF.TOKFLIB
    OVRF      FILE-ZSHIMS1,TOFILE-ZSHIMS1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRPRTF   FILE-XU04LP,TOFILE-XU04LP.XUCL,MEDLIB-TOKELIB
    CALL      PGM-ZHA0125B.TOKELIB,PARA-(?WKSTN)
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