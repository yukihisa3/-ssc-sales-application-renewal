/. ***********************************************************  ./
/. *     サカタのタネ　ＨＧ基幹システム　　　　　　          *  ./
/. *   SYSTEM-NAME :    送受信システム　　　　　　　　　　　 *  ./
/. *   JOB-ID      :    PSK00150                             *  ./
/. *   JOB-NAME    :    受領受信関係リスト再発行　　　　　　 *  ./
/. ***********************************************************  ./
    PGM

/.##ﾊﾟﾗﾒﾀ定義##./
    VAR       ?HIDUKE   ,STRING*8,VALUE-'        ' /.受信日付./
    VAR       ?JIKAN    ,STRING*4,VALUE-'    '     /.受信時間./
    VAR       ?TOKCD    ,STRING*8,VALUE-'        ' /.受信取引先./
    VAR       ?LINE     ,STRING*1,VALUE-'I'        /.回線./
    VAR       ?YUSEN    ,STRING*1,VALUE-'1'        /.回線優先./
    VAR       ?LIBNM    ,STRING*8,VALUE-'ONLBLIB ' /.集信LIB./
    VAR       ?FILNM    ,STRING*8,VALUE-'ONLKEIJR' /.集信FILE./
    VAR       ?JKEKA    ,STRING*4,VALUE-'    '     /.結果./
    VAR       ?SYUKBN   ,STRING*1,VALUE-' '        /.出力区分./
/.##ﾜｰｸﾃｲｷﾞ##./
    VAR       ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR       ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR       ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR       ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR       ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR       ?PGMID    ,STRING*8,VALUE-'PSK00150'  /.PROGRAM-ID./
    VAR       ?STEP     ,STRING*8                   /.STEP-ID./
/.##ﾃﾞｰﾀ変換PG用ﾊﾟﾗﾒﾀ##./
    VAR       ?PARA     ,STRING*14,VALUE-'              '
/.##結果FLG用##./
    VAR       ?KEKA     ,STRING*4,VALUE-'    '      /.結果FLGﾊﾟﾗﾒﾀ./
/.##ﾌｧｲﾙ変換ﾜｰｸ##./
    VAR       ?LIBN     ,NAME                       /.ﾗｲﾌﾞﾗﾘ名前型./
    VAR       ?FILN     ,NAME                       /.ﾌｧｲﾙ名前型./
    VAR       ?FILLIB   ,NAME!MOD                   /.ﾌｧｲﾙ拡張用./
    VAR       ?FILID    ,STRING*17                  /.ﾌｧｲﾙ名表示用./
/.##メッセージ画面用##./
    VAR       ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                    /.      2    ./
    VAR       ?KEKA2    ,STRING*40                    /.      3    ./
    VAR       ?KEKA3    ,STRING*40                    /.      4    ./
    VAR       ?KEKA4    ,STRING*40                    /.      5    ./

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '受領受信関係リスト再発行'

STEP000:  /.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./

    ?STEP :=   'STEP000 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL   TOKELIB/TOKFLIB/TOKELIBO/TOKJLIB/TOKKLIB/ONLBLIB

STEP010:  /.##ﾌﾟﾛｸﾞﾗﾑｶｲｼﾒｯｾｰｼﾞ##./

    ?STEP :=   'STEP010 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

STEP015:  /.## 再取込エラーリスト指定画面 ##./

    ?STEP :=   'STEP015 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIBO,MEDLIB-TOKELIBO
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    CALL      PGM-SSK0015I.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD,
                                          ?SYUKBN)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
        IF    ?PGMEC     =   4010 THEN
              ?MSGX :=  '## ｹｰﾖｰ受領関係ﾘｽﾄ発行指示 取消終了 ##'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN    PGMEC-?PGMEC
        ELSE
              ?KEKA4 :=  '【再発行指示画面】'
              GOTO ABEND
        END
    END

    ?MSGX :=  '## バッチ日付   = ' && ?HIDUKE
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX :=  '## バッチ時刻   = ' && ?JIKAN
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX :=  '## バッチ取引先 = ' && ?TOKCD
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX :=  '## 出力区分     = ' && ?SYUKBN
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

STEP020:  /.##ﾃﾞｰﾀ変換PGﾍのﾊﾟﾗﾒﾀ作成##./

    ?STEP :=   'STEP020 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?PARA :=   ?HIDUKE && ?JIKAN && ?LINE && ?YUSEN
    SNDMSG ?PARA,TO-XCTL.@ORGPROF,JLOG-@YES

STEP030:  /.##出力区分により発行振分##./

    ?STEP :=   'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CASE  ?SYUKBN  OF
          #  ' '  #
           ?MSGX :=  '## ２帳票発行(ｴﾗｰﾘｽﾄ/件数ﾘｽﾄ)      ##'
           SNDMSG ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
           GOTO   STEP040
          #  '1'  #
           ?MSGX :=  '## １帳票発行(ｴﾗｰﾘｽﾄ)              ##'
           SNDMSG ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
           GOTO   STEP040
          #  '2'  #
           ?MSGX :=  '## １帳票発行(件数ﾘｽﾄ)             ##'
           SNDMSG ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
           GOTO   STEP050
    ELSE
           ?MSGX :=  '## 発行帳票　判定異常！！　　　　　##'
           SNDMSG ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
           ?KEKA4 :=  '【発行帳票　判定異常！！】'
           GOTO   ABEND
    END

STEP040:  /.##受領データ取込エラーリスト発行##./

    ?STEP :=   'STEP040 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-KEIJYRL1,TOFILE-KEIJYRL1.TOKKLIB
    CALL      PGM-SSK0012L.TOKELIBO,PARA-(?PARA)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【受領データ取込エラーリスト発行】'
              GOTO ABEND
    END

    IF  ?SYUKBN = '1'    THEN
        GOTO  RTN
    END

STEP050:  /.##受領データ取込件数リスト発行##./

    ?STEP :=   'STEP050 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-KEIJYOL1,TOFILE-KEIJYOL1.TOKKLIB
    CALL      PGM-SSK0013L.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【受領データ取込件数リスト発行】'
              GOTO ABEND
    END

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    RETURN    PGMEC-?PGMEC

/.##異常終了時##./
ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '請求締処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    /.##異常終了メッセージを表示                ##./
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
    RETURN    PGMEC-?PGMEC
