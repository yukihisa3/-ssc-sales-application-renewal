/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（ケーヨー新システム）    *  ./
/. *   SYSTEM-NAME :    配送発注集計表                       *  ./
/. *   JOB-ID      :    PSY40100                             *  ./
/. *   JOB-NAME    :    変換～コード変換～ルート取得～       *  ./
/. *               :    ＳＯＲＴ１～合計計算～ＳＯＲＴ２～   *  ./
/. *                    配送発注集計表                       *./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSY40100'
    VAR       ?STEP     ,STRING*8
/.##ﾌｧｲﾙ名称編集用##./
    VAR       ?WKSTN    ,NAME
    VAR       ?WS       ,STRING*8,VALUE-'        '

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL TOKELIB/TOKFLIB
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##処理確認##./
SSY4011I:

    ?STEP :=   'SSY4011I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    CALL      PGM-SSY4011I.TOKELIB,PARA-(?WS)
    IF        @PGMEC     =   4010 THEN
              ?MSGX := '##セリ配送ルート表出力　取消終了##'
              SNDMSG MSG-?MSGX,TO-XCTL
              GOTO RTN   END
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##量販データＦコンバート##./
SSY4003B:

    ?STEP :=   'SSY4003B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-HRUTJNL,TOFILE-HSERJNL1.TOKFLIB
    OVRF      FILE-SHTDENWK,TOFILE-SHTHAIWK.TOKFLIB
    OVRF      FILE-SHTBIKL1,TOFILE-SERBIKL1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSY4003B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.出荷場所，ルート，ルート順番をセット                          ./
SSY4010B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'***   ﾙｰﾄｼﾞｮｳﾎｳ ｾｯﾄ   ***',TO-XCTL


    OVRF      FILE-SHTDENLA,TOFILE-SHTHAIWK.TOKFLIB
    OVRF      FILE-SHWHISF,TOFILE-SHWHISF.TOKFLIB
    OVRF      FILE-TENRMS1,TOFILE-TENRMS1.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    CALL      PGM-SSY4010B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.ＳＯＲＴ(納品日，出荷場所，ルート，セリ＋枝、商品，ルート順）./
SORT2:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT      INFILE-SHWHISF.TOKFLIB,INRL-256,INBF-1,
              OUTFILE-SHWHISF.TOKFLIB,OUTBF-1,
              KEY-38!2!CA,
              KEY1-54!5!CA,
              KEY2-253!2!DA,
              KEY3-157!5!CA,
              KEY4-162!5!CA,
              KEY5-199!13!CA,
              KEY6-255!2!DA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.配送発注集計表出力                                            ./
SSY0019L:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'*** ﾊｲｿｳﾊｯﾁｭｳｼｭｳｹｲﾋｮｳ(ｾﾘ) ***',TO-XCTL

    OVRF      FILE-INFILE,TOFILE-SHWHISF.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-TENRMS2,TOFILE-TENRMS2.TOKFLIB
    OVRF      FILE-HTOKMS,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRPRTF   FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIB
    CALL      PGM-SSY4012L.TOKELIB
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