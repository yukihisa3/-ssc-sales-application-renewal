/. ************************************************************* ./
/. *   サカタのタネ　基幹システム（本社システム）              * ./
/. *   SYSTEM-NAME :    ケーヨー伝票レス                       * ./
/. *   JOB-ID      :    PSK00900                               * ./
/. *   JOB-NAME    :    ケーヨー累積データ削除                 * ./
/. *               :    起動元：CNI10000自動日次・月次更新処理 * ./
/. ************************************************************* ./
/.### ﾜｰｸｴﾘｱ定義 ###./
/.  PGM (P1-?JKEKA)
./  PGM
/.  PARA ?JKEKA    ,STRING*2,OUT,VALUE-'  ' ./  /.結果./
    VAR  ?JKEKA    ,STRING*2,VALUE-'  '     /.結果./
     VAR ?PGMEC    ,INTEGER                     /.ｴﾗｰｽﾃｲﾀｽ./
     VAR ?PGMECX   ,STRING*11                   /.ｴﾗｰｽﾃｲﾀｽ変換./
     VAR ?PGMEM    ,STRING*99                   /.ｴﾗｰﾒｯｾｰｼﾞ./
     VAR ?MSG      ,STRING*99(6)                /.ﾒｯｾｰｼﾞ定義./
     VAR ?MSGX     ,STRING*99                   /.ﾒｯｾｰｼﾞ定義変換./
     VAR ?PGMID    ,STRING*8,VALUE-'PSK00900'   /.ﾌﾟﾛｸﾞﾗﾑID./
     VAR ?STEP     ,STRING*8                    /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
     VAR ?PGNM     ,STRING*40                   /.ﾒｯｾｰｼﾞ1./
     VAR ?KEKA1    ,STRING*40                   /.      2./
     VAR ?KEKA2    ,STRING*40                   /.      3./
     VAR ?KEKA3    ,STRING*40                   /.      4./
     VAR ?KEKA4    ,STRING*40                   /.      5./
     VAR ?BUMON    ,STRING*4,VALUE-'    '       /.部門名./
     VAR ?TANCD    ,STRING*2,VALUE-'  '         /.担当者CD./
     VAR ?TANCD6   ,STRING*06,VALUE-'      '    /.担当者CDWK   ./
     VAR ?WS       ,STRING*8,VALUE-'        '   /.ﾜｰｸｽﾃｰｼｮﾝ文字./
     VAR ?WKSTN    ,NAME!MOD                    /.ﾜｰｸｽﾃｰｼｮﾝ名前./
/. 共通パラメタ./
     VAR ?SKBN     ,STRING*1,VALUE-' '         /. 出力区分 ./
     VAR ?DKBN     ,STRING*1,VALUE-' '         /. 日付区分 ./
     VAR ?DFROM    ,STRING*8,VALUE-'        '  /. 日付Ｆ   ./
     VAR ?DTO      ,STRING*8,VALUE-'        '  /. 日付Ｔ   ./
     VAR ?KKBN     ,STRING*1,VALUE-' '         /. 計上区分 ./
     VAR ?TANF     ,STRING*2,VALUE-'  '        /. 担当者Ｆ ./
     VAR ?TANT     ,STRING*2,VALUE-'  '        /. 担当者Ｔ ./
     VAR ?DENK1    ,STRING*2,VALUE-'  '        /. 伝区１   ./
     VAR ?DENK2    ,STRING*2,VALUE-'  '        /. 伝区２   ./
     VAR ?DENK3    ,STRING*2,VALUE-'  '        /. 伝区３   ./
     VAR ?DENK4    ,STRING*2,VALUE-'  '        /. 伝区４   ./
     VAR ?DENK5    ,STRING*2,VALUE-'  '        /. 伝区５   ./
     VAR ?TENF     ,STRING*5,VALUE-'     '     /. 店舗Ｆ   ./
     VAR ?TENT     ,STRING*5,VALUE-'     '     /. 店舗Ｔ   ./
     VAR ?DENNF    ,STRING*9,VALUE-'         ' /. 伝番Ｆ　 ./
     VAR ?DENNT    ,STRING*9,VALUE-'         ' /. 伝番Ｔ　 ./
     VAR ?SKBF     ,STRING*2,VALUE-'  '        /. 出場Ｆ　 ./
     VAR ?SKBT     ,STRING*2,VALUE-'  '        /. 出場Ｔ   ./
     VAR ?DENKF    ,STRING*2,VALUE-'  '        /. 伝区Ｆ　 ./
     VAR ?DENKT    ,STRING*2,VALUE-'  '        /. 伝区Ｔ   ./

/.### ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## ﾗｲﾌﾞﾗﾘﾘｽﾄ登録 ##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB

/.## ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ ##./
    ?PGNM :=  'ケーヨー　累積データ削除処理'

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得 ##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## 前回バックアップデータ削除 ##./
STEP000:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   ' && ?STEP && '      (前回分CLR)     ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    DLTFILE   FILE-KEIJYRB1.BAKFLIB
    DLTFILE   FILE-KEIJHRB1.BAKFLIB
    DLTFILE   FILE-KEIURIB1.BAKFLIB

/.## 受領累積データバックアップ ##./
STEP010:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   ' && ?STEP && '      (受領　BACKUP)  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CPYFILE   FILE-KEIJYRF.TOKKLIB,
              TOFILE-KEIJYRB1.BAKFLIB,
              CRTFILE-@YES

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
        ?JKEKA := 'NG'
        ?KEKA4 := '受領累積データバックアップ'
        GOTO  ABEND
    END

/.## 返品累積データバックアップ ##./
STEP020:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   ' && ?STEP && '      (返品　BACKUP)  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CPYFILE   FILE-KEIJHRF.TOKKLIB,
              TOFILE-KEIJHRB1.BAKFLIB,
              CRTFILE-@YES

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
        ?JKEKA := 'NG'
        ?KEKA4 := '返品累積データバックアップ'
        GOTO  ABEND
    END

/.## 売上累積データバックアップ ##./
STEP030:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP  && '      (売上　BACKUP)  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CPYFILE   FILE-KEIURIF.TOKKLIB,
              TOFILE-KEIURIB1.BAKFLIB,
              CRTFILE-@YES

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
        ?JKEKA := 'NG'
        ?KEKA4 := '売上累積データバックアップ'
        GOTO  ABEND
    END

/.## 受領累積データ削除 ##./
STEP040:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   ' && ?STEP && '      (受領累積削除)  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL      SSK0090B.TOKELIBO

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
        ?JKEKA := 'NG'
        ?KEKA4 := '受領累積データ削除'
        GOTO  ABEND
    END

/.## 返品累積データ削除 ##./
STEP050:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP && '      (返品累積削除)  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL      SSK0091B.TOKELIBO

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
        ?JKEKA := 'NG'
        ?KEKA4 := '返品累積データ削除'
        GOTO  ABEND
    END

/.## 売上累積データ削除 ##./
STEP060:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP && '      (売上累積削除)  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL      SSK0092B.TOKELIBO

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
        ?JKEKA := 'NG'
        ?KEKA4 := '売上累積データ削除'
        GOTO  ABEND
    END


/.### ﾌﾟﾛｸﾞﾗﾑ終了 ###./
RTN:

    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKKLIB
    ?JKEKA := 'OK'
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '更新結果等を確認して下さい。'
    ?KEKA3 :=  '『ケーヨー累積データ削除処理』'
    ?KEKA4 :=  '（正常終了）'
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '*** ' && ?PGMID && ' END  (ｹｰﾖｰ累積削除)  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-?PGMEC

ABEND:  /. ﾌﾟﾛｸﾞﾗﾑ異常終了時処理 ./

    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKKLIB
    ?KEKA1 :=  'ケーヨー累積データ削除処理が異常終了し'
    ?KEKA2 :=  'ました。結果リストをＨＧ部システム担当'
    ?KEKA3 :=  '者に渡し、ＮＡＶへ連絡して下さい。　　'
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' LINE = '  && %LAST(LINE)
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO ?MSGX :=   ?MSG(?I)
           SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

    RETURN    PGMEC-?PGMEC
