/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    在庫管理システム                     *  ./
/. *   JOB-ID      :    PZMO0120                             *  ./
/. *   JOB-NAME    :    未抽出チェックリスト                 *  ./
/. *   PG-NAME     :    未抽出チェックリスト抽出             *  ./
/. *   PG-NAME     :    ＳＯＲＴ                             *  ./
/. *   PG-NAME     :    未抽出チェックリスト出力             *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PZMO0120'
    VAR       ?STEP     ,STRING*8
    VAR       ?WKSTN    ,STRING*8
    VAR       ?NWKSTN   ,NAME
    ?NWKSTN   :=         @ORGWS
    ?WKSTN    :=         %STRING(?NWKSTN)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    CRTFILE FILE-DENJNLWK.@TEMP,SIZE-1000,ESIZE-500
    CHGFILE FILE-DENJNLWK.@TEMP


/.##未抽出チェックリスト抽出##./
ZMO0120B:

    ?STEP :=   'ZMO0120B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-DENJNL6,TOFILE-SHTDENL6.TOKFLIB
    OVRF      FILE-OUTFILE,TOFILE-DENJNLWK.@TEMP
    CALL      PGM-ZMO0120B.TOKELIBO,PARA-(?WKSTN)
    IF        @PGMEC    =   4050  THEN
              GOTO RTN
              ELSE
             IF        @PGMEC   ^=   0     THEN
              GOTO ABEND END
    END

/.##ソート##./
 SORT01:
    ?STEP :=   'SORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT   INFILE-DENJNLWK.@TEMP,INRL-1020,INBF-1,
           OUTFILE-DENJNLWK.@TEMP,OUTBF-1,RCD-4,
           KEY-1!8!DA,KEY1-9!9!DA,KEY2-20!1!DA,KEY3-18!2!DA,
           RCDL-@DSP
    IF     @PGMEC   ^=   0        THEN
           GOTO ABEND END

/.##未抽出チェックリスト出力##./
ZMO0130B:

    ?STEP :=   'ZMO0130B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-INFILE,TOFILE-DENJNLWK.@TEMP
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-ZMO0130B.TOKELIBO,PARA-(?WKSTN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN:
    /.##抽出伝票ﾃﾞｰﾀ削除(ﾃﾝﾎﾟﾗﾘ)##./
    DLTFILE   DENJNLWK.@TEMP
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:
    /.##抽出伝票ﾃﾞｰﾀ削除(ﾃﾝﾎﾟﾗﾘ)##./
    DLTFILE   DENJNLWK.@TEMP
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