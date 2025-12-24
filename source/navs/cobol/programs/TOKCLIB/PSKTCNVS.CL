/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    オンライン                           *  ./
/. *   JOB-ID      :    PSKTCNVS                             *  ./
/. *   JOB-NAME    :    セリ売上データ変換                   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSKTCNVS'
    VAR       ?STEP     ,STRING*8
/.##ﾌｧｲﾙ名称編集用##./
    VAR       ?WKSTN    ,NAME
    VAR       ?WS       ,STRING*8,VALUE-'        '
    VAR       ?MEMO     ,STRING*5,VALUE-'     '

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL TOKELIB/TOKFLIB
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##処理確認##./
SSY0707I:

    ?STEP :=   'SSY0707I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    CALL      PGM-SSY4007I.TOKELIB
    IF        @PGMEC     =   155  THEN
              SNDMSG MSG-'***   取消終了 SSY4007I  ***',TO-XCTL
              GOTO RTN   END
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##ユポラベルデータ作成##./
SSY4009B:

    ?STEP :=   'SSY4009B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-HSERJNL,TOFILE-HSERJNL.TOKFLIB
    OVRF      FILE-SERYUPO,TOFILE-SERYUP84.TOKFLIB
    CALL      PGM-SSY4009B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##量販店備考ﾜｰｸ初期化##./
    CLRFILE FILE-SERBIKF.TOKFLIB
/.##量販データＦコンバート##./
SSY4003B:

    ?STEP :=   'SSY4003B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-HRUTJNL,TOFILE-HSERJNL1.TOKFLIB
    OVRF      FILE-SHTDENWK,TOFILE-SHTSERWK.TOKFLIB
    OVRF      FILE-SHTBIKL1,TOFILE-SERBIKL1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSY4003B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##SORT1##./
SORT1:

    ?STEP :=   'SORT1   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.取引先、納品日、出荷場所、伝票番号、店舗コード、行番号，セリ_./
     SORT     INFILE-SHTSERWK.TOKFLIB,INRL-1020,INBF-1,
              OUTFILE-SHTSERWK.TOKFLIB,OUTBF-1,
              KEY-1!8!CA,KEY1-54!5!PA,KEY2-38!2!CA,KEY3-9!9!CA,
              KEY4-33!5!CA,KEY5-18!2!CA,KEY6-391!7!CA,
              SEQ-@YES,RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END


/.##伝票_ＣＮＴ##./
SSY4004B:

    ?STEP :=   'SSY4004B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-KYOTUX,TOFILE-SHTSERWK.TOKFLIB
    OVRF      FILE-HDENJNL,TOFILE-SHTDENF.TOKFLIB
    OVRF      FILE-SHTBIKL1,TOFILE-SERBIKL1.TOKFLIB
    OVRF      FILE-DENMEISF,TOFILE-DENMEISF.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZZAIMS1,TOFILE-ZZAIMS1.TOKFLIB
    CALL      PGM-SSY4004B.TOKELIB,PARA-(?WS)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##端末別量販店ジャーナル初期化##./
    CLRFILE FILE-HSERJNL.TOKFLIB

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