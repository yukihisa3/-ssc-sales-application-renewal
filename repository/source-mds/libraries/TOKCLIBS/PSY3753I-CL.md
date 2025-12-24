# PSY3753I

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PSY3753I.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    新受配信サブシステム      　　　     *  ./
/. *   JOB-ID      :    PSY3753I                             *  ./
/. *   JOB-NAME    :    作場出荷データ出力               *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
    PGM (P1-?KBN)

/.##ﾊﾟﾗﾒﾀ定義##./
    PARA      ?KBN      ,STRING*1,IN,VALUE-' '  /.処理区分./
/.### ﾜｰｸｴﾘｱ定義 ###./
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'SSY3753I'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1./
    VAR ?KEKA1    ,STRING*40                    /.      2./
    VAR ?KEKA2    ,STRING*40                    /.      3./
    VAR ?KEKA3    ,STRING*40                    /.      4./
    VAR ?KEKA4    ,STRING*40                    /.      5./
    VAR ?TAB      ,STRING*2                     /.タブ   ./
    VAR ?BUMON    ,STRING*4,VALUE-'    '        /.部門名./
    VAR ?TANCD    ,STRING*2,VALUE-'  '          /.担当者CD./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./

/.### ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## ﾗｲﾌﾞﾗﾘﾘｽﾄ登録 ##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKCLIBO/TOKWLIB

/.## ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ ##./
    ?PGNM :=  '作場出荷データ出力'

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得 ##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

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

STEP0020:
/.##ＣＳＶファイルクリア##./
    CLRFILE  NFSYKDT.TOKKLIB
/.##出荷指示ファイルクリア##./
    CLRFILE  NFSYUKF.TOKKLIB

/.## 作場出荷依頼データ出力 ##./
STEP0030:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.  OVRDSPF FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO ./
    /. I、?BUMON   部門コード         ./
    /. I、?TANCD   担当者コード       ./
    CALL PGM-SSY3753I.TOKELIBO,PARA-(?BUMON,?TANCD,?KBN)

    IF  @PGMEC ^= 0  THEN
        IF @PGMEC = 4010  THEN
           SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
           GOTO  RTN
        ELSE
           ?KEKA4 := '作場出荷データ出力'
           GOTO  ABEND
        END
    END

    ?MSGX := '** BUMON  ='  && ?BUMON   &&   '**'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '** TANCD  ='  && ?TANCD   &&   '**'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## 出荷依頼データ：店舗情報作成 ##./
STEP0040:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.  OVRDSPF FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO ./
    /. I、?BUMON   部門コード         ./
    /. I、?TANCD   担当者コード       ./
    CALL PGM-SSY3754B.TOKELIBO
    IF  @PGMEC ^= 0  THEN
        IF @PGMEC = 4010  THEN
           SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
        ELSE
           ?KEKA4 := '作場出荷依頼データ店舗情報出力'
           GOTO  ABEND
        END
    END

    ?MSGX := '** BUMON  ='  && ?BUMON   &&   '**'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '** TANCD  ='  && ?TANCD   &&   '**'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## 出荷依頼データＣＳＶ出力 ﾍｯﾀﾞﾃﾞｰﾀ ##./
STEP0050:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    /. I、?TAB     レコード種別     ./
    /.             ＨＤ：ヘッダ     ./
    ?TAB      :=   'HD'
    OVRF FILE-NFSYKDT,TOFILE-NFSYKDT.TOKKLIB
    CALL PGM-SSY3756V.TOKELIBO,PARA-(?TAB)
    IF  @PGMEC ^= 0  THEN
        IF @PGMEC = 4010  THEN
           SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
        ELSE
           ?KEKA4 := '出荷依頼データＣＳＶ出力ＨＤ'
           GOTO  ABEND
        END
    END

/.## 出荷依頼データＣＳＶ出力 ﾒｲｻｲﾃﾞｰﾀ ##./
STEP0060:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    /. I、?TAB     レコード種別     ./
    /.             ＤＴ：明細　　　 ./
    ?TAB      :=   'DT'
    OVRF FILE-NFSYKDT,TOFILE-NFSYKDT.TOKKLIB
    CALL PGM-SSY3756C.TOKELIBO,PARA-(?TAB)
    IF  @PGMEC ^= 0  THEN
        IF @PGMEC = 4010  THEN
           SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
        ELSE
           ?KEKA4 := '出荷依頼データＣＳＶ出力ＤＴ'
           GOTO  ABEND
        END
    END

/.## データ転送 ##./
STEP0070:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    FIMPORT FILE-NFSYKDT.TOKKLIB,PARA-NAFUKO,UNIT-1
    IF  @PGMEC ^= 0  THEN
        ?KEKA4 := 'ＣＳＶデータ転送'
        GOTO  ABEND
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

```
