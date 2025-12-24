# PSY3767B

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PSY3767B.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    新受配信サブシステム      　　　     *  ./
/. *   JOB-ID      :    PSY3767B                             *  ./
/. *   JOB-NAME    :    受信データ取込＆変換　　　　　       *  ./
/. *               :    受領書発行                           *  ./
/. *               :    受領書ＣＳＶ出力                     *  ./
/. *               :    受領アンマッチリスト発行             *  ./
/. ***********************************************************  ./
/.### ﾜｰｸｴﾘｱ定義 ###./
    PGM
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'SSY3767B'    /.ﾌﾟﾛｸﾞﾗﾑID./
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
    VAR ?KBN      ,STRING*1,VALUE-'2'        /. 区分 ./
    VAR ?BTHZK    ,STRING*8,VALUE-'99999999' /. バッチＮＯ・日付 ./
    VAR ?BTTIM    ,STRING*4,VALUE-'9999'     /. バッチＮＯ・時間 ./
    VAR ?BTTOR    ,STRING*8,VALUE-'99999999' /. バッチＮＯ・取引先 ./
    VAR ?KANRNO   ,STRING*8,VALUE-'00000101' /. 管理番号 ./
    VAR ?SAKBCD   ,STRING*2,VALUE-'  '       /. 作場コード ./
    VAR ?SYYMD    ,STRING*8,VALUE-'        ' /. 出荷日 ./
    VAR ?TENYMD   ,STRING*8,VALUE-'        ' /. 店着日 ./
    VAR ?TORICD   ,STRING*8,VALUE-'00137607' /. 取引先ＣＤ./
    VAR ?JDATE    ,STRING*8,VALUE-'        ' /. 受領日 ./
    VAR ?STYMD    ,STRING*8,VALUE-'        ' /. 開始仕入計上日 ./
    VAR ?EDYMD    ,STRING*8,VALUE-'        ' /. 終了仕入計上日 ./
    VAR ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2     ,STRING*50                  /.      2    ./
    VAR ?OPR3     ,STRING*50                  /.      3    ./
    VAR ?OPR4     ,STRING*50                  /.      4    ./
    VAR ?OPR5     ,STRING*50                  /.      5    ./

/.### ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## ﾗｲﾌﾞﾗﾘﾘｽﾄ登録 ##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/ONLBLIB/TOKELIBO

/.## ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ ##./
    ?PGNM :=  '受信データ取込＆変換'

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
/.##確認画面##./

STEP010:

    ?OPR1  :=  '【ナフコ受領データ取込＆変換処理】　　　　'
    ?OPR2  :=  '　　　　　　　　　　　　　　　　　　　　　'
    ?OPR3  :=  'この処理で、受領書、受領書ＣＳＶデータ、　'
    ?OPR4  :=  '受領アンマッチリストの出力が行われますので'
    ?OPR5  :=  '帳票、ファイルの確認を行ってください。　　'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKWLIB

/.## 受信データ取込 ##./
PFEXPORT:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    FEXPORT FILE-NFJYUDT.TOKWLIB,MODE-@REP,PARA-JYURYO,
            UNIT-1
    IF  @PGMEC ^= 0  THEN
           ?KEKA4 := '受信データ取込'
           GOTO  ABEND
    END

/.## 受信データ取込＆変換 ##./
SSY3767B:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    /. O、?JDATE   データ受信日　     ./
    /. O、?STYMD   開始仕入計上日     ./
    /. O、?EDYMD   終了仕入計上日　   ./
    OVRF    FILE-NFJYUDT,TOFILE-NFJYUDT.TOKWLIB
    CALL PGM-SSY3767B.TOKELIBO,PARA-(?JDATE,?STYMD,?EDYMD)
    IF  @PGMEC ^= 0  THEN
        IF @PGMEC = 4010  THEN
           SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
        ELSE
           ?KEKA4 := '受信データ取込＆変換'
           GOTO  ABEND
        END
    END

    ?MSGX := '### ?JDATE = ' && ?JDATE && ' ##'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF
    ?MSGX := '### ?STYMD = ' && ?STYMD && ' ##'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF
    ?MSGX := '### ?EDYMD = ' && ?EDYMD && ' ##'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF

/.## 受領データチェック ##./
SSY3780B:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    /. I、?JDATE   受信日　　     ./
    CALL PGM-SSY3780B.TOKELIBO,PARA-(?JDATE)
    IF  @PGMEC ^= 0  THEN
           ?KEKA4 := '受領データチェック'
           GOTO  ABEND
    END

/.## 受領書発行処理 ##./
SSY3768L:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.  OVRDSPF FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO ./
    /. I、?STYMD   開始仕入計上日     ./
    /. O、?EDYMD   終了仕入計上日     ./
    /. O、?BMNCD   部門ＣＤ　　　　   ./
/.  OVRPRTF   FILE-XU04LP,TOFILE-KAHMAPRT.XUCL
./  CALL PGM-SSY3768L.TOKELIBO
      ,PARA-(?STYMD,?EDYMD,?BUMON)
    IF  @PGMEC ^= 0  THEN
           ?KEKA4 := '受領書発行処理'
           GOTO  ABEND
    END

/.## 受領書ＣＳＶデータ出力 ##./
SSY3769V:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.  OVRDSPF FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO ./
    /. I、?STYMD   データ受信日　     ./
    /. I、?EDYMD   開始仕入計上日     ./
    /. I、?EDYMD   終了仕入計上日     ./
    OVRF    FILE-NFJYURL1,TOFILE-NFJYURL1.TOKKLIB
    CALL PGM-SSY3769V.TOKELIBO,PARA-(?JDATE,?STYMD,?EDYMD)
    IF  @PGMEC ^= 0  THEN
           ?KEKA4 := '受領書ＣＳＶデータ出力'
           GOTO  ABEND
    END

/.## 受領データＣＳＶ出力 ##./
PFIMPORT:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    FIMPORT FILE-NFJYRDT.TOKWLIB,PARA-JYURYO,UNIT-2
    IF  @PGMEC ^= 0  THEN
           ?KEKA4 := '受領データＣＳＶ出力'
           GOTO  ABEND
    END

/.## 受領アンマッチリスト出力 ##./
SSY3770L:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.  OVRDSPF FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO ./
    /. I、?TORICD  取引先ＣＤ　　     ./
    /. I、?EDYMD   開始仕入計上日     ./
    /. I、?EDYMD   終了仕入計上日     ./
/.  OVRPRTF   FILE-XU04LP,TOFILE-KAHMAPRT.XUCL
./  CALL PGM-SSY3770L.TOKELIBO,PARA-(?TORICD,?STYMD,?EDYMD)
    IF  @PGMEC ^= 0  THEN
           ?KEKA4 := '受領アンマッチリスト出力'
           GOTO  ABEND
    END

/.### ﾌﾟﾛｸﾞﾗﾑ終了 ###./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:  /. ﾌﾟﾛｸﾞﾗﾑ異常終了時処理 ./
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  'ＴＲＡＮＴＲＡＮ連携データ作成異常終了。'
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
