/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受配信管理システム                   *  ./
/. *   JOB-ID      :    KENLST                               *  ./
/. *   JOB-NAME    :    受信件数リスト                       *  ./
/. ***********************************************************  ./
    PGM (P1-?HIDUKE,P2-?JIKAN,P3-?TOKCD,P4-?LINE,P5-?YUSEN)
/.##パラメタ定義##./
    PARA ?HIDUKE ,STRING*8,IN,VALUE-'        ' /.受信日付./
    PARA ?JIKAN  ,STRING*4,IN,VALUE-'    '     /.受信時間./
    PARA ?TOKCD  ,STRING*8,IN,VALUE-'        ' /.受信取引先./
    PARA ?LINE   ,STRING*1,IN,VALUE-' '        /.回線種別./
    PARA ?YUSEN  ,STRING*1,IN,VALUE-' '        /.優先順位./
/.##ワーク定義##./
    VAR  ?GNO1     ,STRING*8,VALUE-'00000007'  /.受信取引先./
    VAR  ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR  ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR  ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR  ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR  ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR  ?PGMID    ,STRING*8,VALUE-'KENLST  '  /.PROGRAM-ID./
    VAR  ?STEP     ,STRING*8                   /.STEP-ID./
    VAR  ?PRTBUMON ,STRING*4,VALUE-'    '      /.出力部門./

/.##プログラム開始メッセージ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKJLIB/TOKKLIB

/.##出力部門取得##./
SKY9999B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                           /.##各種ﾌｧｲﾙ置換え##./
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL      PGM-SKY9999B.TOKELIBO,PARA-(?TOKCD,?PRTBUMON)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##出力部門取得##./
BUMONPRT:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.## 出力先変更　ナフコのみ九州へ
    IF  ?PRTBUMON = '2940'  THEN
        ?MSGX :=  '## 出力部門 = ' && ?PRTBUMON && '(九州) ##'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
        CHGCMVAR '@OUTQN',XXPAMGQ
    ELSE
        ?MSGX :=  '## 出力部門 = ' && ?PRTBUMON && '(本社) ##'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
        CHGCMVAR '@OUTQN',XSYSLSTQ
    END
.//.IF  ?TOKCD = '00137607'  THEN
        ?MSGX :=  '## 出力部門 = ' && ?PRTBUMON && '(九州) ##'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
        CHGCMVAR '@OUTQN',XXPAMGQ
    ELSE
        ?MSGX :=  '## 出力部門 = ' && ?PRTBUMON && '(本社) ##'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
        CHGCMVAR '@OUTQN',XSYSLSTQ
    END
./
/.##振分け件数ﾘｽﾄ出力##./
KENLST:

    ?STEP :=   'KENLST  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF    FILE-JSMKENL1,TOFILE-JSMKENL1.TOKJLIB
    OVRF    FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF    FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF    FILE-JSMEDIL1,TOFILE-JSMEDIL1.TOKJLIB
    CALL    PGM-SNJ0570L.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD)
    ?PGMEC := @PGMEC
    IF   ?PGMEC ^= 0 THEN
         GOTO      ABEND
    END

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    RETURN    PGMEC-@PGMEC

ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND ' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
    SNDMSG ?PGMEM,TO-XCTL.@ORGPROF,JLOG-@YES
    RETURN    PGMEC-@PGMEC
