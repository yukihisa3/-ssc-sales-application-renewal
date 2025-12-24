/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    新受配信サブシステム      　　　     *  ./
/. *   JOB-ID      :    PSY3767D                             *  ./
/. *   JOB-NAME    :    受信データ取込＆変換　　　　　       *  ./
/. *               :    受領書発行                           *  ./
/. *               :    受領書ＣＳＶ出力                     *  ./
/. *               :    受領アンマッチリスト発行             *  ./
/. *   UPDATE      :    2017.10.06 S11270540                 *  ./
/. *   UPDATE      :    2021.11.08 S22470210 返品自動計上    *  ./
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

/.##返品自動計上パラメタ##./
    VAR       ?JKBN     ,STRING*1,VALUE-' '         /.実行区分./
    VAR       ?BUMCD    ,STRING*4,VALUE-'2920'      /.部門ＣＤ./
  /.VAR       ?TANCD    ,STRING*2,VALUE-'98'./      /.担当者ＣＤ./
    VAR       ?HTOKCD   ,STRING*8,VALUE-'00000000'  /.取引先ＣＤ./
    VAR       ?NDATE    ,STRING*8,VALUE-'        '  /.入力日./
    VAR       ?UPTIME   ,STRING*6,VALUE-'      '    /.更新日./
    VAR       ?DENCNT   ,STRING*7,VALUE-'0000000'   /.売伝追加件数./
    /. リスト出力用パラメタ ./
    VAR       ?KEIKBN   ,STRING*1,VALUE-'3'         /. 計上区分 ./
    VAR       ?TANST    ,STRING*2,VALUE-'98'        /. 担当者開始 ./
    VAR       ?TANED    ,STRING*2,VALUE-'98'        /. 担当者終了 ./
    VAR       ?DKBN     ,STRING*1,VALUE-'2'         /. 日付区分　 ./
    VAR       ?AFROM    ,STRING*8,VALUE-'00000000'  /. 検収日開始 ./
    VAR       ?ATO      ,STRING*8,VALUE-'00000000'  /. 検収日終了 ./
    VAR       ?NFROM    ,STRING*8,VALUE-'00000000'  /. 入力日開始 ./
    VAR       ?NTO      ,STRING*8,VALUE-'00000000'  /. 入力日終了 ./
    VAR       ?KFROM    ,STRING*8,VALUE-'00000000'  /. 計上日開始 ./
    VAR       ?KTO      ,STRING*8,VALUE-'00000000'  /. 計上日終了 ./
    VAR       ?SFROM    ,STRING*8,VALUE-'00000000'  /. 作成日開始 ./
    VAR       ?STO      ,STRING*8,VALUE-'00000000'  /. 作成日終了 ./

/.### ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## ﾗｲﾌﾞﾗﾘﾘｽﾄ登録 ##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKWLIB/TOKELIBO/BSFLIB
           /TOKDLIB/TOKSOLIB/ONLBLIB
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
    ?OPR2  :=  '※連絡事項追加バージョン'
    ?OPR3  :=  '確認＝＞受信処理は完了していますか？'
    ?OPR4  :=  '受領データ取込後、受領書又はＣＳＶを出力'
    ?OPR5  :=  '可能です。受信日又はナフコ仕入計上日'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKWLIB/TOKELIBO/TOKJLIB/BMSFLIB/
            TOKDTLIB/TOKDLIB/TOKSOLIB/ONLBLIB

/.## 前回データ削除 ##./
PCLRFILE:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CLRFILE FILE-NFJYUDT.TOKWLIB
    IF  @PGMEC ^= 0  THEN
           ?KEKA4 := '前回データ削除'
           GOTO  ABEND
    END

/.## 受信データ取込 ##./
PFEXPORT:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    FEXPORT FILE-NFJYUDT.TOKWLIB,MODE-@ADD,PARA-JYURYO,
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
    CALL PGM-SSY3767D.TOKELIBO,PARA-(?JDATE,?STYMD,?EDYMD)
    IF  @PGMEC ^= 0  THEN
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

    CALL      PGM-SJH8399B.TOKELIBO,PARA-(?BTHZK,?BTTIM)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##受領データ共通変換##./
SJR0110B:

    ?STEP :=   'SJR0110B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

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

/.↓ 以下追加 2021.11.08 ./
/.##返品自動計上判定処理（通常）##./
SJR0430B:

    ?STEP :=   'SJR0430B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?TANCD    :=   '98'

    ?HTOKCD := ?TORICD /.1取引先CDであるため一意で処理./

    IF        ?JKBN  =  '1'  THEN
              OVRF  FILE-COMRHEL2,TOFILE-COMRMHL7.TOKDTLIB
    ELSE
              OVRF  FILE-COMRHEL2,TOFILE-COMRHEL2.TOKDTLIB
    END

/.##CALL      PGM-SJR0430B.TOKSOLIB,
              PARA-(?JKBN,?BUMCD,?TANCD,?BTHZK,?BTTIM,?HTOKCD,
                    ?NDATE)##./  /.計上対象の入力日をOUT./
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##返品自動計上処理（通常）##./
SJR0440B:

    ?STEP :=   'SJR0440B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    IF        ?JKBN = ' '  THEN
              OVRF  FILE-COMRHELA,TOFILE-COMRHELA.TOKDTLIB
    ELSE
              OVRF  FILE-COMRHELA,TOFILE-COMRMHLA.TOKDTLIB
    END
/.##CALL      PGM-SJR0440B.TOKSOLIB,
              PARA-(?BUMCD,?TANCD,?BTHZK,?BTTIM,?HTOKCD,?NDATE,
                    ?UPTIME,?DENCNT)   計上時刻・件数をOUT##./
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END
    IF        ?DENCNT    =   '0000000'    THEN
              GOTO RTN
    END

/.## 返品自動計上確認データ抽出 ##./
SJR0360A:

    ?STEP :=   'SJR0360A'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    IF  ?JKBN = ' '  THEN
        OVRF FILE-COMRHEL4,TOFILE-COMRHEL4.TOKDTLIB
        OVRF FILE-COMRHEL5,TOFILE-COMRHEL5.TOKDTLIB
        OVRF FILE-COMRHEL8,TOFILE-COMRHEL8.TOKDTLIB
        OVRF FILE-COMRHEL9,TOFILE-COMRHEL9.TOKDTLIB
    ELSE
        OVRF FILE-COMRHEL4,TOFILE-COMRMHL4.TOKDTLIB
        OVRF FILE-COMRHEL5,TOFILE-COMRMHL5.TOKDTLIB
        OVRF FILE-COMRHEL8,TOFILE-COMRMHL8.TOKDTLIB
        OVRF FILE-COMRHEL9,TOFILE-COMRMHL9.TOKDTLIB
    END
    OVRF FILE-COMWHEL1,TOFILE-AUTWHEL1.TOKDLIB

    ?NFROM  :=  ?NDATE   /.判定処理からの計上対象入力日./
    ?NTO    :=  ?NDATE   /.判定処理からの計上対象入力日./

/.##CALL PGM-SJR0360A.TOKSOLIB,PARA-(?KEIKBN,?HTOKCD,?TANST,?TANED,
                                     ?DKBN,?AFROM,?ATO,?NFROM,?NTO,
                                     ?KFROM,?KTO,?SFROM,?STO,
                                     ?UPTIME)   計上更新日も条件##./
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
        IF ?PGMEC = 4001  THEN
           GOTO  RTN
        ELSE
           GOTO  ABEND
        END
    END

/.## 返品自動計上確認リスト発行 ##./
SJR0460L:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL

/.##CALL PGM-SJR0460L.TOKSOLIB,PARA-(?JKBN,?KEIKBN,?HTOKCD,?TANST,
             ?TANED,?DKBN,?AFROM,?ATO,?NFROM,?NTO,?KFROM,?KTO,
             ?SFROM,?STO)
##./?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
        GOTO  ABEND
    ELSE
        GOTO  RTN
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