/. ***********************************************************  ./
/. *     サカタのタネ　ＨＧシステム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受注自動欠品業務　　　　　　　　　　 *  ./
/. *   JOB-ID      :    PJK00200                             *  ./
/. *   JOB-NAME    :    受注自動欠品処理                     *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR ?PGMEC    ,INTEGER
    VAR ?PGMECX   ,STRING*11
    VAR ?PGMEM    ,STRING*99
    VAR ?MSG      ,STRING*99(6)
    VAR ?MSGX     ,STRING*99
    VAR ?PGMID    ,STRING*8,VALUE-'PJK00200'
    VAR ?STEP     ,STRING*8
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./

    VAR ?JDATE    ,STRING*8,VALUE-'00000000'    /.受信日./
    VAR ?JTIME    ,STRING*4,VALUE-'0000'        /.受信時刻./
    VAR ?JTOKCD   ,STRING*8,VALUE-'00000000'    /.受信取引先./
    VAR ?JNOUHIN  ,STRING*8,VALUE-'0000'        /.納品日./

/.##実行PG名称ｾｯﾄ##./
    ?PGNM := '受注自動欠品処理'

/.## ﾗｲﾌﾞﾗﾘﾘｽﾄ登録 ##./
    DEFLIBL   TOKELIB/TOKELIBO/TOKFLIB/TOKKLIB/TOKDLIB/TOKWLIB

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.#受注自動欠品処理指示画面##./
SJK0030I:

    ?STEP :=   'SJK0030I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF  FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    CALL  PGM-SJK0030I.TOKELIBO,PARA-(?JDATE,?JTIME,?JTOKCD,
                                      ?JNOUHIN)
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF        ?PGMEC    ^=   0    THEN
         IF   ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              ?KEKA4 := '受注自動欠品処理指示画面'
              GOTO ABEND
         END
    END

    ?MSGX :=  '＃　受信日付   = '  && ?JDATE   && '　＃'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '＃　受信時刻   = '  && ?JTIME   && '　　　＃'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '＃　受信取引先 = '  && ?JTOKCD  && '　＃'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '＃　受信日付   = '  && ?JNOUHIN && '　＃'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##受注自動欠品処理##./
SJK0030B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL     PGM-SJK0030B.TOKELIBO,PARA-(?JTOKCD,?JDATE,?JTIME,
                                         ?JNOUHIN)
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '受注自動欠品処理'
              GOTO ABEND
    END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?PGMECX   :=    %STRING(?PGMEC)

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

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