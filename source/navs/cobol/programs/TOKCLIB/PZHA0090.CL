/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    在庫管理                             *  ./
/. *   JOB-ID      :    ZHA0090B                             *  ./
/. *   JOB-NAME    :    入荷予定リスト　                     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PZHA0090'
    VAR       ?STEP     ,STRING*8
    VAR       ?WKSTN    ,STRING*8
    VAR       ?NWKSTN   ,NAME
    ?NWKSTN    :=       @ORGWS
    ?WKSTN    :=        %STRING(?NWKSTN)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    CRTFILE   FILE-ZOUTF.@TEMP,SIZE-500,ESIZE-50
    CHGFILE   FILE-ZOUTF.@TEMP

/.  入荷予定リスト抽出                                          ./
ZHA0090B:
    DEFLIBL    TOKFLIB/TOKELIB/TOKFLIB
    ?STEP :=   'ZHA0090B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-ZHACHDT1,TOFILE-ZHACHDT1.TOKFLIB
    OVRF      FILE-ZOUTF,TOFILE-ZOUTF.@TEMP
    CALL      PGM-ZHA0090B.TOKELIB,PARA-(?WKSTN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.  ＳＯＲＴ                                                    ./
SORT01:
    ?STEP :=   'SORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT       INFILE-ZOUTF.@TEMP,INRL-200,INBF-5,
               OUTFILE-ZOUTF.@TEMP,OUTBF-5,RCD-4,
               KEY-154!2!CA,KEY1-25!8!CA,KEY2-3!7!DA,KEY3-13!2!DA,
               RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
/.  入荷予定リスト出力                                          ./
ZHA0095B:
    ?STEP :=   'ZHA0095B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ZOUTF,TOFILE-ZOUTF.@TEMP
    OVRF      FILE-ZSHIMS1,TOFILE-ZSHIMS1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-ZHACHDT1,TOFILE-ZHACHDT1.TOKFLIB
    OVRPRTF   FILE-XU04LP,TOFILE-XU04LP.XUCL,MEDLIB-TOKELIB
    CALL      PGM-ZHA0095B.TOKELIB,PARA-(?WKSTN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    DLTFILE   ZOUTF.@TEMP
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

    DLTFILE   ZOUTF.@TEMP
    RETURN    PGMEC-@PGMEC
