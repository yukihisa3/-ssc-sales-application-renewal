/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    新受配信サブシステム      　　　     *  ./
/. *   JOB-ID      :    PSY3781I                             *  ./
/. *   JOB-NAME    :    作場別出荷依頼確認リスト（商品別）   *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
/.### ﾜｰｸｴﾘｱ定義 ###./
    PGM
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PSY3781I'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1./
    VAR ?KEKA1    ,STRING*40                    /.      2./
    VAR ?KEKA2    ,STRING*40                    /.      3./
    VAR ?KEKA3    ,STRING*40                    /.      4./
    VAR ?KEKA4    ,STRING*40                    /.      5./
    VAR ?TAB      ,STRING*2                     /.タブ   ./
    VAR ?BUMON    ,STRING*4,VALUE-'    '        /.部門名./
    VAR ?TANCD    ,STRING*2,VALUE-'  '          /.担当者CD./
    VAR ?KBN      ,STRING*1,VALUE-' '           /.オンライン・手書./
    VAR ?KANRNO   ,STRING*8,VALUE-'00000000'    /.管理番号./
    VAR ?BCHYMD   ,STRING*8,VALUE-'00000000'    /.バッチ日付./
    VAR ?BCHTIM   ,STRING*4,VALUE-'0000'        /.バッチ時間./
    VAR ?BCHTRI   ,STRING*8,VALUE-'00000000'    /.バッチ取引先ＣＤ./
    VAR ?SAKCD    ,STRING*2,VALUE-'00'          /.作場ＣＤ./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./

/.### ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB

/.## ログインユーザー情報取得 ##./
STEP0010:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ログイン情報取得'
              GOTO ABEND
    END

/.## 作場出荷依頼データ出力 指示 ##./
STEP0020:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.  OVRDSPF FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO ./
    /. I、?BUMON   部門コード         ./
    /. I、?TANCD   担当者コード       ./
    /. I、" "      処理区分           ./
    /. O、?KBN     処理区分           ./
    /. O、?KANRNO  管理ＮＯ           ./
    /. O、?BCHYMD  バッチ日付         ./
    /. O、?BCHTIM  バッチ時間　       ./
    /. O、?BCHTRI  バッチ取引先ＣＤ   ./
    /. O、?SAKCD   作場ＣＤ           ./
    CALL PGM-SSY3781I.TOKELIBO
      ,PARA-(?BUMON,?TANCD,' ',?KBN,?KANRNO,?BCHYMD,BCHTIM,
                                         ?BCHTRI,?SAKCD)
    IF  @PGMEC ^= 0  THEN
        IF @PGMEC = 4010  THEN
           SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
           GOTO  RTN
        ELSE
           ?KEKA4 := '作場出荷リスト出力'
           GOTO  ABEND
        END
    END

/.## 作場別出荷依頼リスト（商品別）出力 ##./
STEP0030:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.  OVRDSPF FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO ./
    /. I、?KBN     オンライン／手書   ./
    /. I、?KANRNO  管理番号　　       ./
    /. I、?BCHYMD  バッチ　日付       ./
    /. I、?BCHTIM  バッチ　時間       ./
    /. I、?BCHTRI  バッチ　取引先     ./
    /. I、?SAKCD   作場ＣＤ　　       ./
    CALL PGM-SSY3752L.TOKELIBO,
        PARA-(?KBN,?KANRNO,?BCHYMD,BCHTIM,?BCHTRI,?SAKCD)
    IF  @PGMEC ^= 0  THEN
        IF @PGMEC = 4010  THEN
           SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
        ELSE
           ?KEKA4 := '作場出荷依頼リスト商品別出力'
           GOTO  ABEND
        END
    END

/.## 作場別出荷依頼リスト（店舗別）出力 ##./
STEP0040:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.  OVRDSPF FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO ./
    /. I、?KBN     オンライン／手書   ./
    /. I、?KANRNO  管理番号　　       ./
    /. I、?BCHYMD  バッチ　日付       ./
    /. I、?BCHTIM  バッチ　時間       ./
    /. I、?BCHTRI  バッチ　取引先     ./
    /. I、?SAKCD   作場ＣＤ　　       ./
    CALL PGM-SSY3779L.TOKELIBO,
        PARA-(?KBN,?KANRNO,?BCHYMD,BCHTIM,?BCHTRI,?SAKCD)
    IF  @PGMEC ^= 0  THEN
        IF @PGMEC = 4010  THEN
           SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
        ELSE
           ?KEKA4 := '作場出荷依頼リスト店舗別出力'
           GOTO  ABEND
        END
    END

/.### ﾌﾟﾛｸﾞﾗﾑ終了 ###./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:  /. ﾌﾟﾛｸﾞﾗﾑ異常終了時処理 ./
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  '作場出荷データ出力異常終了。'
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