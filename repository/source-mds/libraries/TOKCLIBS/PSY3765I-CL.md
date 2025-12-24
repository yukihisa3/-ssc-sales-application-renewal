# PSY3765I

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PSY3765I.CL`

## ソースコード

```jcl
/. ***V*******************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    新受配信サブシステム      　　　     *  ./
/. *   JOB-ID      :    PSY3765I                             *  ./
/. *   JOB-NAME    :    ＴＲＡＮＴＲＡＮ連携データ作成指示   *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
/.### ﾜｰｸｴﾘｱ定義 ###./
    PGM
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'SSY3765I'    /.ﾌﾟﾛｸﾞﾗﾑID./
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
    ?PGNM :=  'ＴＲＡＮＴＲＡＮ連携データ作成指示'

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

/.## ＴＲＡＮＴＲＡＮ連携データ作成指示 ##./
SSY3765I:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL PGM-SSY3765I.TOKELIBO
      ,PARA-(?TANCD,?KBN,?BTHZK,?BTTIM,?BTTOR,?KANRNO
            ,?SAKBCD,?SYYMD,?TENYMD)
    IF  @PGMEC ^= 0  THEN
        IF @PGMEC = 4010  THEN
           SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
           GOTO  RTN
        ELSE
           ?KEKA4 := 'ＴＲＡＮＴＲＡＮ連携データ作成指示'
           GOTO  ABEND
        END
    END

/.## ＴＲＡＮＴＲＡＮ連携データ作成 ##./
SSY376TI:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL PGM-SSY3766A.TOKELIBO
      ,PARA-(?TANCD,?KBN,?BTHZK,?BTTIM,?BTTOR,?KANRNO
            ,?SAKBCD,?SYYMD,?TENYMD)
    IF  @PGMEC ^= 0  THEN
        ?KEKA4 := 'ＴＲＡＮＴＲＡＮ連携データ作成'
        GOTO  ABEND
    END

/.## 箱数ファイル転送 ##./
FIMPORT1:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    FIMPORT FILE-NFHAKDT.TOKKLIB,PARA-NAFUKO,UNIT-2
    IF  @PGMEC ^= 0  THEN
        ?KEKA4 := '箱数ファイル出力'
        GOTO  ABEND
    END

/.## 数量訂正ファイル転送 ##./
FIMPORT2:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    FIMPORT FILE-NFSUTDT.TOKKLIB,PARA-NAFUKO,UNIT-3
    IF  @PGMEC ^= 0  THEN
        ?KEKA4 := '数量訂正ファイル出力'
        GOTO  ABEND
    END

/.## ＦＡＸ発注ファイル転送 ##./
FIMPORT3:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    FIMPORT FILE-NFFAXDT.TOKKLIB,PARA-NAFUKO,UNIT-4
    IF  @PGMEC ^= 0  THEN
        ?KEKA4 := 'ＦＡＸ発注ファイル出力'
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
