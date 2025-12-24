/. ***********************************************************  ./
/. *   サカタのタネ　　本社システム　                        *  ./
/. *   SYSTEM-NAME :    ＥＤＩサブシステム      　　　       *  ./
/. *   JOB-ID      :    PSY38750                             *  ./
/. *   MENU-ID     :    NFSYUKA1-65                          *  ./
/. *   JOB-NAME    :    ナフコ出荷（ＡＳＮ）データ作成　　   *  ./
/. *　　　　　　　　　　（送信チェックＶｅｒ）　　　　　　　 *  ./
/. *   UPDATE      :    2019/11/28 S2245053                  *  ./
/. ***********************************************************  ./
    PGM (P1-?SYORIKBN)

/.##ﾊﾟﾗﾒﾀ定義##./
    PARA ?SYORIKBN,STRING*1,IN,VALUE-' '        /.処理区分
                                                  1:新規送信
                                                  2:再送信　
                                                  3:再作成　 ./
/.##ﾜｰｸ定義##./
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./

    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./

    VAR ?PGMID    ,STRING*8,VALUE-'PSY38750'    /.ﾌﾟﾛｸﾞﾗﾑID./
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

    VAR ?KBN      ,STRING*1,VALUE-' '           /. ｵﾝﾗｲﾝ/手書区分 ./
    VAR ?KANRNO   ,STRING*8,VALUE-'        '    /. 管理番号 ./
                                                /. 作場ＣＤ * 20 ./
    VAR ?SAKBCD   ,STRING*40,
                   VALUE-'                                        '
    VAR ?SYYMD    ,STRING*8,VALUE-'00000000'    /. 出荷日 ./
    VAR ?TENYMD   ,STRING*8,VALUE-'00000000'    /. 店着日 ./
    VAR ?PCCHKNF  ,STRING*1,VALUE-' '           /. ＰＣ側送信CHK./

/.### ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## ﾗｲﾌﾞﾗﾘﾘｽﾄ登録 ##./
    DEFLIBL TOKELIB/TOKDLIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKDTLIB/ONLBLIB
            /TOKSOLIB

/.## ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ ##./
    ?PGNM :=  'ナフコ出荷（ＡＳＮ）データ作成'

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得 ##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES


/.##部門ｺｰﾄﾞ取得##./
SKY1602B:

    ?STEP :=   'SKY1602B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1602B.TOKELIBO,PARA-(?WS,?BUMON)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0
          THEN
              ?KEKA4 :=  '【部門取得】'
              GOTO ABEND
    END

/.##担当者CD指定##./
SKY6101I:

    ?STEP :=   'SKY6101I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    CALL      PGM-SKY6101I.TOKELIBO,PARA-(?BUMON,?TANCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0  THEN
              IF  ?PGMEC = 4010  THEN
                  SNDMSG MSG-'##取消終了##',TO-XCTL
                  RETURN
              ELSE
                  ?KEKA4 :=  '【担当者ＣＤ指定】'
                  GOTO  ABEND
              END
    END

/.##送信状態チェック１##./
SNDCHK01:

    ?STEP :=   'SNDCHK01'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-PCCHKNF.ONLBLIB,MODE-@REP,PARA-NAFUKO,
            UNIT-10
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0  THEN
              ?KEKA4 :=  '【送信状態チェック１：ＦＩＭＰＯＲＴ】'
              GOTO  ABEND
    END

/.##送信状態チェック２##./
SNDCHK02:

    ?STEP :=   'SNDCHK02'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL PGM-SSY3990B.TOKSOLIB,PARA-(?PCCHKNF)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0  THEN
              ?KEKA4 :=  '【送信状態チェック２：送信チェック】'
              GOTO  ABEND
    END

    IF  ?PCCHKNF = '1'  THEN
        ?MSGX :=  '*前回分ＡＳＮデータ送信済　ＯＫ*'
        SNDMSG    ?MSGX,TO-XCTL
    ELSE
        ?MSGX :=  '*前回分ＡＳＮデータ送信済　ＮＧ*'
        SNDMSG    ?MSGX,TO-XCTL
        ?KEKA4:=  '*前回分ＡＳＮデータ送信済　ＮＧ*'
        GOTO      SNDNG
    END

/.## ナフコ出荷（ＡＳＮ）データ作成指示 ##./
SSY3890I:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

  /.S2245053./
    OVRDSPF  FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
  /.S2245053
  --CALL PGM-SSY3870I.TOKELIBO,PARA-(?KANRNO,?SAKBCD,?KBN) ./
    CALL PGM-SSY3890I.TOKSOLIB,PARA-(?SYORIKBN,?KANRNO,?SAKBCD,?KBN)

    ?PGMEC     :=    @PGMEC
    ?PGMEM     :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
        IF ?PGMEC = 4010  THEN
           SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
           RETURN
        ELSE
           ?KEKA4 := 'ナフコ出荷（ＡＳＮ）データ作成指示'
           GOTO  ABEND
        END
    END

    ?MSGX :=  '管理番号＝'  && ?KANRNO
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '作場ＣＤ１　＝'  && %SBSTR(?SAKBCD,1,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ２　＝'  && %SBSTR(?SAKBCD,3,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ３　＝'  && %SBSTR(?SAKBCD,5,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ４　＝'  && %SBSTR(?SAKBCD,7,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ５　＝'  && %SBSTR(?SAKBCD,9,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ６　＝'  && %SBSTR(?SAKBCD,11,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ７　＝'  && %SBSTR(?SAKBCD,13,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ８　＝'  && %SBSTR(?SAKBCD,15,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ９　＝'  && %SBSTR(?SAKBCD,17,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１０＝'  && %SBSTR(?SAKBCD,19,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１１＝'  && %SBSTR(?SAKBCD,21,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１２＝'  && %SBSTR(?SAKBCD,23,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１３＝'  && %SBSTR(?SAKBCD,25,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１４＝'  && %SBSTR(?SAKBCD,27,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１５＝'  && %SBSTR(?SAKBCD,29,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１６＝'  && %SBSTR(?SAKBCD,31,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１７＝'  && %SBSTR(?SAKBCD,33,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１８＝'  && %SBSTR(?SAKBCD,35,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１９＝'  && %SBSTR(?SAKBCD,37,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ２０＝'  && %SBSTR(?SAKBCD,39,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'オ／手区分　＝'  && ?KBN
    SNDMSG    ?MSGX,TO-XCTL

/.## ナフコ出荷（ＡＳＮ）データ作成 ##./
SSY3891B:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

  /.S2245053
  --CALL PGM-SSY3871B.TOKELIBO,PARA-(?TANCD,?KBN,?KANRNO,
  --                                 ?SAKBCD,?SYYMD,?TENYMD) ./
    CALL PGM-SSY3891B.TOKSOLIB,PARA-(?TANCD,?KBN,?KANRNO,
                                     ?SAKBCD,?SYYMD,?TENYMD,
                                     ?SYORIKBN)
    ?PGMEC      :=    @PGMEC
    ?PGMEM      :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
        ?KEKA4 := 'ナフコ出荷（ＡＳＮ）データ作成'
        GOTO  ABEND
    END

/.S2245053./
/.## ナフコ出荷（ＡＳＮ）データ結合 ##./
CNVFILE1:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVFILE FILE-NFASNSF2.ONLBLIB,
            TOFILE-NFASNSF1.ONLBLIB,
            ADD-@YES
    ?PGMEC      :=    @PGMEC
    ?PGMEM      :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
        ?KEKA4 := 'ナフコ出荷（ＡＳＮ）データ結合'
        GOTO  ABEND
    END

/.S2245053./
/.## 出荷（ＡＳＮ）ファイル転送 ##./
FIMPORT1:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    FIMPORT FILE-NFASNSF1.ONLBLIB,
            PARA-NAFUKO,
            UNIT-9
    ?PGMEC     :=    @PGMEC
    ?PGMEM     :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
        ?KEKA4 := '出荷（ＡＳＮ）ファイル転送'
        GOTO  ABEND
    END

/.S2245053 にて廃止 ----------------------------------------
- ## 箱数ファイル転送 ##
-FIMPORT1:
-   ?STEP :=   %LAST(LABEL)
-   ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
-   SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
-   FIMPORT FILE-NFHAKDT.TOKKLIB,PARA-NAFUKO,UNIT-2
-   IF  @PGMEC ^= 0  THEN
-       ?KEKA4 := '箱数ファイル出力'
-       GOTO  ABEND
-   END
- ## 数量訂正ファイル転送 ##
-FIMPORT2:
-   ?STEP :=   %LAST(LABEL)
-   ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
-   SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
-   FIMPORT FILE-NFSUTDT.TOKKLIB,PARA-NAFUKO,UNIT-3
-   IF  @PGMEC ^= 0  THEN
-       ?KEKA4 := '数量訂正ファイル出力'
-       GOTO  ABEND
-   END
- ## ＦＡＸ発注ファイル転送 ##
-FIMPORT3:
-   ?STEP :=   %LAST(LABEL)
-   ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
-   SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
-   FIMPORT FILE-NFFAXDT.TOKKLIB,PARA-NAFUKO,UNIT-4
-   IF  @PGMEC ^= 0  THEN
-       ?KEKA4 := 'ＦＡＸ発注ファイル出力'
-       GOTO  ABEND
-   END
------------------------------------------------------------./

/.## 送信チェック用データ　ＰＣ上送信##./
 SNDCHK03:
    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    FIMPORT FILE-PCCHKNON.ONLBLIB,PARA-NAFUKO,UNIT-11
    ?PGMEC     :=    @PGMEC
    ?PGMEM     :=    @PGMEM
    IF  @PGMEC ^= 0  THEN
        ?KEKA4 := '送信チェック用データ　ＰＣ上送信'
        GOTO  ABEND
    END

/.### ﾌﾟﾛｸﾞﾗﾑ終了 ###./
RTN:

    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  'ナフコ出荷（ＡＳＮ）データ作成正常終了。'
    ?KEKA2 :=  'ナフコへの送信処理を実施して下さい。　　'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-?PGMEC

ABEND:  /. ﾌﾟﾛｸﾞﾗﾑ異常終了時処理 ./

    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  'ナフコ出荷（ＡＳＮ）データ作成異常終了。'
    ?KEKA2 :=  'ログリスト等を採取しＮＡＶへ連絡して下'
    ?KEKA3 :=  'さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
  /.?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM ./
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

    RETURN    PGMEC-?PGMEC

SNDNG:  /. 前回分送信チェック：ＮＧ./

    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  '前回分送信チェックエラー！！'
    ?KEKA2 :=  '前回分のナフコＡＳＮデータが未送信です'
    ?KEKA3 :=  '送信フォルダーを確認して下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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

    RETURN    PGMEC-?PGMEC
