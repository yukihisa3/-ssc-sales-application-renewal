/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    新受配信サブシステム      　　　     *  ./
/. *   JOB-ID      :    PSNS767D                             *  ./
/. *   JOB-NAME    :    受信データ取込＆変換（資材用）       *  ./
/. *               :    受領書発行                           *  ./
/. *               :    受領書ＣＳＶ出力                     *  ./
/. *               :    受領アンマッチリスト発行             *  ./
/. *   UPDATE      :    2017.10.06 S11270540                 *  ./
/. ***********************************************************  ./
/.### ﾜｰｸｴﾘｱ定義 ###./
    PGM
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PNS3767D'    /.ﾌﾟﾛｸﾞﾗﾑID./
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
    VAR ?BTHZK    ,STRING*8,VALUE-'20221215' /. バッチＮＯ・日付 ./
    VAR ?BTTIM    ,STRING*4,VALUE-'1559'     /. バッチＮＯ・時間 ./
    VAR ?BTTOR    ,STRING*8,VALUE-'00284202' /. バッチＮＯ・取引先 ./
    VAR ?KANRNO   ,STRING*8,VALUE-'00000101' /. 管理番号 ./
    VAR ?SAKBCD   ,STRING*2,VALUE-'  '       /. 作場コード ./
    VAR ?SYYMD    ,STRING*8,VALUE-'        ' /. 出荷日 ./
    VAR ?TENYMD   ,STRING*8,VALUE-'        ' /. 店着日 ./
    VAR ?TORICD   ,STRING*8,VALUE-'00284202' /. 取引先ＣＤ./
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

    ?OPR1  :=  '【ナフコ受領データ取込＆変換処理】'
    ?OPR2  :=  '（資材用）※連絡事項追加バージョン'
    ?OPR3  :=  '確認＝＞受信処理は完了していますか？'
    ?OPR4  :=  '受領データ取込後、受領書又はＣＳＶを出力'
    ?OPR5  :=  '可能です。受信日又はナフコ仕入計上日'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKWLIB/TOKELIBO/TOKJLIB/BMSFLIB/
            TOKDLIB/TOKSOLIB/ONLBLIB/TOKDTLIB

/.## 前回データ削除 ##./
PCLRFILE:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.  CLRFILE FILE-NSJYUDT.TOKWLIB
./  IF  @PGMEC ^= 0  THEN
           ?KEKA4 := '前回データ削除'
           GOTO  ABEND
    END

/.## 受信データ取込 ##./
PFEXPORT:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.  FEXPORT FILE-NSJYUDT.TOKWLIB,MODE-@ADD,PARA-JYURYO,
            UNIT-3
./  IF  @PGMEC ^= 0  THEN
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
    OVRF FILE-NFJYUDT,TOFILE-NSJYUDT.TOKWLIB
    OVRF FILE-NFJYURL1,TOFILE-NSJYURL1.TOKDTLIB
/.  CALL PGM-SSY3767D.TOKELIBO,PARA-(?JDATE,?STYMD,?EDYMD)
./  IF  @PGMEC ^= 0  THEN
        IF @PGMEC = 4010  THEN
           SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
        ELSE
           ?KEKA4 := '受信データ取込＆変換'
           GOTO  ABEND
        END
    END

/.##バッチ_採番処理##./
SJH8399B:

    ?STEP :=   'SJH8399B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.  CALL      PGM-SJH8399B.TOKELIBO,PARA-(?BTHZK,?BTTIM)
./  IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##受領データ共通変換##./
SJR0110B:

    ?STEP :=   'SJR0110B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF FILE-NFJYURL3,TOFILE-NSJYURL3.TOKDTLIB
    CALL      PGM-SJR0110B.TOKSOLIB,PARA-(?BTHZK,?BTTIM,?TORICD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##受信エラーリスト（受領・返品）##./
SJR0220L:

    ?STEP :=   'SJR0220L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0220L.TOKSOLIB,PARA-(?BTHZK,?BTTIM)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##受信状況リスト（受領・返品）##./
SJR0210L:

    ?STEP :=   'SJR0210L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0210L.TOKSOLIB,PARA-(?BTHZK,?BTTIM)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##返品データ抽出##./
SJR0280B:

    ?STEP :=   'SJR0280B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0280B.TOKSOLIB,PARA-(?BTHZK,?BTTIM,?TORICD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END


/.### ﾌﾟﾛｸﾞﾗﾑ終了 ###./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:  /. ﾌﾟﾛｸﾞﾗﾑ異常終了時処理 ./
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  '受領データ取込＆変換処理が異常終了。'
    ?KEKA2 :=  'ログリスト等を採取しＮＡＶへ連絡して下'
    ?KEKA3 :=  'さい。（資材用）'
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