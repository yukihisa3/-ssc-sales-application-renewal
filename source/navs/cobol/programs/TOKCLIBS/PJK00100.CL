/. ***********************************************************  ./
/. *     サカタのタネ　ＨＧシステム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受注自動欠品業務　　　　　　　　　　 *  ./
/. *   JOB-ID      :    PJK00100                             *  ./
/. *   JOB-NAME    :    出荷制限商品リストＣＳＶ取込　　　　 *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PJK00100'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?WS       ,STRING*08,VALUE-'        '   /.ﾜｰｸｽﾃｰｼｮﾝ文字  ./
    VAR ?BUMON    ,STRING*04,VALUE-'    '       /.部門名       ./
    VAR ?TANCD6   ,STRING*06,VALUE-'      '     /.担当者CDWK   ./
    VAR ?TANCD    ,STRING*02,VALUE-'  '         /.担当者CD     ./
    VAR ?WKSTN    ,NAME                         /.ﾜｰｸｽﾃｰｼｮﾝ名前  ./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
    VAR ?OPR1     ,STRING*50                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2     ,STRING*50                    /.      2    ./
    VAR ?OPR3     ,STRING*50                    /.      3    ./
    VAR ?OPR4     ,STRING*50                    /.      4    ./
    VAR ?OPR5     ,STRING*50                    /.      5    ./
    VAR ?JTOKCD   ,STRING*08,VALUE-'        '   /.取込取引先ＣＤ./
    VAR ?JDATE    ,STRING*08,VALUE-'        '   /.取込日付./
    VAR ?JTIME    ,STRING*08,VALUE-'        '   /.取込時刻./
    VAR ?JKENSU   ,STRING*06,VALUE-'000000'     /.取込件数./
    VAR ?JERR     ,STRING*01,VALUE-'0'          /.エラー区分./
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := '出荷制限商品リストＣＳＶ取込'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKDLIB/TOKKLIB/TOKWLIB/ONLBLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.==================================================================./

/.##実行確認##./
    ?OPR1  :=  '　＜出荷制限商品リストＣＳＶ取込＞'
    ?OPR2  :=  '　ＥＸＣＥＬで作成した出荷制限商品データ　'
    ?OPR3  :=  '　の取込を行います。'
    ?OPR4  :=  '　所定のフォルダーにデータをセットして'
    ?OPR5  :=  '　おりますか？確認後、実行願います！！'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.==================================================================./

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD6)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ログイン情報取得'
              GOTO ABEND
    END
    ?TANCD := %SBSTR(?TANCD6,1,2)

/.==================================================================./


/.==================================================================./

/.##出荷制限商品データ取込前初期化##./
PCLRFIL1:

    ?STEP :=  'PCLRFIL1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-SYUSEGWK.TOKDLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷制限商品データ取込前初期化'
              GOTO ABEND END

/.##出荷制限商品ワーク初期化##./
PCLRFIL2:

    ?STEP :=  'PCLRFIL2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-SYUSGWF.TOKDLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷制限商品ワーク初期化'
              GOTO ABEND END


/.##出荷制限商品データ取込（ＣＳＶ）##./
PFEXPORT:

    ?STEP :=   'PFEXPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-SYUSEGWK.TOKDLIB,MODE-@ADD,PARA-JIDOKEPP,
            UNIT-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷制限商品データ取込（ＣＳＶ）'
              GOTO ABEND END

/.##出荷制限商品ワーク作成##./
SJK0010W:

    ?STEP :=   'SJK0010W'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-SJK0010W.TOKELIBO,PARA-(?BUMON,?TANCD,?JTOKCD,
                                    ?JDATE,?JTIME,?JKENSU)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷制限商品ワーク作成'
              GOTO ABEND END

/.##出荷制限商品リストＣＳＶ取込画面##./
SJK0010I:

    ?STEP :=   'SJK0010I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-SJK0010I.TOKELIBO,PARA-(?JTOKCD,?JDATE,?JTIME,
                                          ?JKENSU)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
         IF   ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              ?KEKA4 :=  '出荷制限商品リストＣＳＶ取込画面'
              GOTO ABEND
         END
    END

/.##出荷制限商品ワークチェック##./
SJK0010C:

    ?STEP :=   'SJK0010C'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-SJK0010C.TOKELIBO,PARA-(?JKENSU,?JERR)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷制限商品ワークチェック'
              GOTO ABEND END
    IF  ?JERR  =  'E'  THEN
        ?MSGX :=  '＃データマスタ未登録有！！＃'
        SNDMSG    ?MSGX,TO-XCTL
        GOTO SJK0020L
    END

/.##出荷制限商品マスタ削除##./
SJK0010D:

    ?STEP :=   'SJK0010D'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-SJK0010D.TOKELIBO,PARA-(?JTOKCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷制限商品マスタ削除'
              GOTO ABEND END

/.##出荷制限商品マスタ作成##./
SJK0010B:

    ?STEP :=   'SJK0010B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-SJK0010B.TOKELIBO,PARA-(?JKENSU)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷制限商品マスタ削除'
              GOTO ABEND
    ELSE
              GOTO RTN
    END

/.##出荷制限商品マスタエラーＣＳＶ出力##./
SJK0020L:

    ?STEP :=   'SJK0020L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-SJK0020L.TOKELIBO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷制限商品マスタエラーＣＳＶ出力'
              GOTO ABEND END

/.##出荷制限商品マスタエラーＣＳＶ転送##./
PFIMPORT:

    ?STEP :=   'PFIMPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-SYUSEGER.TOKDLIB,PARA-JIDOKEPP,UNIT-2
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷制限商品マスタエラーＣＳＶ転送'
              GOTO ABEND END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC
