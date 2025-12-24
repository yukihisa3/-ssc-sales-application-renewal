# PSY3762I

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PSY3762I.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    新受配信サブシステム      　　　     *  ./
/. *   JOB-ID      :    PSY3762I                             *  ./
/. *   JOB-NAME    :    手書出荷指示データ取込               *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
/.### ﾜｰｸｴﾘｱ定義 ###./
    PGM
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'SSY3762I'    /.ﾌﾟﾛｸﾞﾗﾑID./
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
    VAR ?SKBCD    ,STRING*2,VALUE-'  '        /. 作場コード ./
/.  VAR ?TORICD   ,STRING*8,VALUE-'00137607'     取引先ＣＤ ./
    VAR ?TORICD   ,STRING*8,VALUE-'00999977'  /. 取引先ＣＤ ./
    VAR ?NONYYMD  ,STRING*8,VALUE-'        '  /. 納入日 ./
    VAR ?STDENNO  ,STRING*9,VALUE-'         ' /. 開始伝票番号 ./
    VAR ?EDDENNO  ,STRING*9,VALUE-'         ' /. 終了伝票番号 ./
    VAR ?KANRINO  ,STRING*8,VALUE-'00000000'  /. 管理番号 ./
    VAR ?KEKKA    ,STRING*2,VALUE-'  '        /. 結果 ./

/.### ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## ﾗｲﾌﾞﾗﾘﾘｽﾄ登録 ##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB

/.## ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ ##./
    ?PGNM :=  '手書出荷指示データ取込'

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

/.## 手書出荷指示データ取込 ##./
SSY3762I:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.  OVRDSPF FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO ./
    /. I、?BUMON   部門コード         ./
    /. I、?TANCD   担当者コード       ./
    /. O、?SKBCD   作場コード         ./
    /. O、?NONYYMD 納入日             ./
    /. O、?STDENNO 開始伝票番号       ./
    /. O、?EDDENNO 終了伝票番号       ./
    CALL PGM-SSY3762I.TOKELIBO
      ,PARA-(?BUMON,?TANCD,?SKBCD,?NONYYMD,?STDENNO,?EDDENNO)
    IF  @PGMEC ^= 0  THEN
        IF @PGMEC = 4010  THEN
           SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
           GOTO  RTN
        ELSE
           ?KEKA4 := '手書出荷指示データ取込'
           GOTO  ABEND
        END
    END

    ?MSGX := '** BUMON  ='  && ?BUMON   &&   '**'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '** TANCD  ='  && ?TANCD   &&   '**'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '** SKBCD  ='  && ?SKBCD   &&   '**'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '** NONYYMD='  && ?NONYYMD   &&   '**'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '** STDENNO='  && ?STDENNO   &&   '**'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '** EDDENNO='  && ?EDDENNO   &&   '**'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## 手書出荷指示データ削除 ##./
SSY3763B:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    /. I、?SKBCD   作場ＣＤ           ./
    /. I、?NONYYMD 納入日             ./
    /. I、?STDENNO 開始伝票番号      ./
    /. I、?EDDENNO 終了伝票番号      ./
    CALL PGM-SSY3763B.TOKELIBO
      ,PARA-(?SKBCD,?NONYYMD,?STDENNO,?EDDENNO)
    IF  @PGMEC ^= 0  THEN
        ?KEKA4 := '手書出荷指示データ削除'
        GOTO  ABEND
    END

/.## 手書出荷指示データ作成 ##./
SSY3764B:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    /. I、?BUMON   部門ＣＤ           ./
    /. I、?TANCD   担当者ＣＤ         ./
    /. I、?SKBCD   作場ＣＤ　　　　   ./
    /. I、?TORICD  取引先ＣＤ　　　   ./
    /. I、?NONYYMD 納入日　　　　　　 ./
    /. I、?STDENNO 開始伝票番号       ./
    /. I、?EDDENNO 終了伝票番号       ./
    CALL PGM-SSY3764B.TOKELIBO
      ,PARA-(?BUMON,?TANCD,?SKBCD,?TORICD,?NONYYMD,
             ?STDENNO,?EDDENNO,?KANRINO)
    IF  @PGMEC ^= 0  THEN
        ?KEKA4 := '手書出荷指示データ作成'
        GOTO  ABEND
    END

/.## 管理番号リスト発行 ##./
SSY3751T:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL PGM-SSY3751T.TOKELIBO
      ,PARA-('2',?KANRINO,'99999999','9999','00137607',?BUMON,
             ?KEKKA)
    IF  @PGMEC ^= 0  THEN
        ?KEKA4 := '管理番号リスト発行'
        GOTO  ABEND
    END

/.### ﾌﾟﾛｸﾞﾗﾑ終了 ###./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:  /. ﾌﾟﾛｸﾞﾗﾑ異常終了時処理 ./
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  '手書出荷指示データ取込異常終了。'
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

```
