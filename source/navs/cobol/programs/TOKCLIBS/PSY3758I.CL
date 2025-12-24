/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    新受配信サブシステム      　　　     *  ./
/. *   JOB-ID      :    PSY3758I                             *  ./
/. *   JOB-NAME    :    出荷情報作成指示   *  ./
/. *               :    出荷情報作成                         *  ./
/. ***********************************************************  ./
/.### ﾜｰｸｴﾘｱ定義 ###./
    PGM
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'SSY3758I'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1./
    VAR ?KEKA1    ,STRING*40                    /.      2./
    VAR ?KEKA2    ,STRING*40                    /.      3./
    VAR ?KEKA3    ,STRING*40                    /.      4./
    VAR ?KEKA4    ,STRING*40                    /.      5./
    VAR ?BUMON    ,STRING*4,VALUE-'    '        /.部門名./
    VAR ?TANCD    ,STRING*2,VALUE-'  '          /.担当者CD./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?KBN      ,STRING*1,VALUE-' '        /. 区分 ./
    VAR ?BTHZK    ,STRING*8,VALUE-'        ' /. バッチＮＯ・日付 ./
    VAR ?BTTIM    ,STRING*4,VALUE-'    '     /. バッチＮＯ・時間 ./
    VAR ?BTTOR    ,STRING*8,VALUE-'        ' /. バッチＮＯ・取引先 ./
    VAR ?KANRNO   ,STRING*8,VALUE-'        ' /. 管理番号 ./
    VAR ?SAKBCD   ,STRING*2,VALUE-'  '       /. 作場コード ./
    VAR ?SYYMD    ,STRING*8,VALUE-'        ' /. 出荷日 ./
    VAR ?TENYMD   ,STRING*8,VALUE-'        ' /. 店着日 ./

/.### ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## ﾗｲﾌﾞﾗﾘﾘｽﾄ登録 ##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB

/.## ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ ##./
    ?PGNM :=  '出荷情報作成指示'

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得 ##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## ログインユーザー情報取得 ##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ログイン情報取得'
              GOTO ABEND
    END

/.## 出荷情報エラーファイルクリア ##./
    CLRFILE FILE-NFSERRF.TOKKLIB

/.## 出荷情報作成指示 ##./
SSY3758I:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.  OVRDSPF FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO ./
    /. I、?TANCD   担当者コード       ./
    /. O、?KBN     区分               ./
    /. O、?BTHZK   バッチＮＯ・日付   ./
    /. O、?BTTIM   バッチＮＯ・時間   ./
    /. O、?BTTOR   バッチＮＯ・取引先 ./
    /. O、?KANRNO  管理番号           ./
    /. O、?SAKBCD  作場コード         ./
    /. O、?SYYMD   出荷日             ./
    /. O、?TENYMD  店着日             ./
    CALL PGM-SSY3758I.TOKELIBO
   ,PARA-(?TANCD,?KBN,?BTHZK,?BTTIM,?BTTOR,?KANRNO,?SAKBCD,
          ?SYYMD,?TENYMD)
    IF  @PGMEC ^= 0  THEN
        IF @PGMEC = 4010  THEN
           SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
           GOTO  RTN
        ELSE
           ?KEKA4 := '出荷情報作成指示'
           GOTO  ABEND
        END
    END

    ?MSGX := '** TANCD ='  && ?TANCD   &&   '**'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '** KBN   ='  && ?KBN   &&   '**'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '** BTHZK ='  && ?BTHZK   &&   '**'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '** BTTIM ='  && ?BTTIM   &&   '**'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '** BTTOR ='  && ?BTTOR   &&   '**'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '** KANRNO='  && ?KANRNO   &&   '**'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '** SAKBCD='  && ?SAKBCD   &&   '**'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '** SYYMD ='  && ?SYYMD   &&   '**'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '** TENYMD='  && ?TENYMD   &&   '**'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## 出荷情報作成 ##./
SSY3759B:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    /. I、?KBN     区分               ./
    /. I、?BTHZK   バッチＮＯ・日付   ./
    /. I、?BTTIM   バッチＮＯ・時間   ./
    /. I、?BTTOR   バッチＮＯ・取引先 ./
    /. I、?KANRNO  管理番号           ./
    /. I、?SAKBCD  作場コード         ./
    /. I、?SYYMD   出荷日             ./
    /. I、?TENYMD  店着日             ./
    OVRF  FILE-NFJOHOL1,TOFILE-NFJOHOL1.TOKKLIB
    OVRF  FILE-NFJOHOL2,TOFILE-NFJOHOL2.TOKKLIB
    CALL PGM-SSY3759B.TOKELIBO
      ,PARA-(?KBN,?BTHZK,?BTTIM,?BTTOR,?KANRNO
            ,?SAKBCD,?SYYMD,?TENYMD)
    IF  @PGMEC ^= 0  THEN
        ?KEKA4 := '出荷情報作成'
        GOTO  ABEND
    END

/.## 出荷情報作成エラーリスト発行 ##./
SSY3760L:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    /. I、?KBN     区分               ./
    /. I、?KANRNO  管理番号           ./
    /. I、?BTHZK   バッチＮＯ・日付   ./
    /. I、?BTTIM   バッチＮＯ・時間   ./
    /. I、?BTTOR   バッチＮＯ・取引先 ./
    /. I、?SAKBCD  作場コード         ./
    CALL PGM-SSY3760L.TOKELIBO
      ,PARA-(?KBN,?KANRNO,?BTHZK,?BTTIM,?BTTOR,?SAKBCD)
    IF  @PGMEC ^= 0  THEN
        ?KEKA4 := '出荷情報作成エラーリスト'
        GOTO  ABEND
    END

/.### ﾌﾟﾛｸﾞﾗﾑ終了 ###./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:  /. ﾌﾟﾛｸﾞﾗﾑ異常終了時処理 ./
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  '出荷情報作成指示異常終了。'
    ?KEKA2 :=  'ログリスト等を採取しＮＡＶへ連絡して下'
    ?KEKA3 :=  'さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO ?MSGX :=   ?MSG(?I)
           SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

    RETURN    PGMEC-@PGMEC
