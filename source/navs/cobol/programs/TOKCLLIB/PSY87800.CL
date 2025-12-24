/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＤＣＭＪＡＰＡＮ　物品受領書         *  ./
/. *   JOB-ID      :    PSY87800                             *  ./
/. *   JOB-NAME    :    物品受領データＣＳＶ出力（自動）     *  ./
/. ***********************************************************  ./
    PGM (P1-?P1)
/.###ﾊﾟﾗﾒﾀ定義####./
    PARA ?P1      ,STRING*8,IN,VALUE-'00000000'    /.取引先CD./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PSY87800'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ＤＣＭＪＡＰＡＮ物品受領データ出力'

/.##ＤＣＭ受領データ初期化##./
PCLRFILE:

    ?STEP :=   'PCLRFILE '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CLRFILE DCMJYRF.TOKKLIB
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
        ?KEKA4 :=  'ＤＣＭ受領データ初期化'
        GOTO ABEND
    END

/.##受信ファイル⇒物品受領書Ｆ作成##./
SSY8732B:

    ?STEP :=   'SSY8732B '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?MSGX :=  '***  TORICD = '  && ?P1   &&   '  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CASE ?P1 OF
      #'00000880'# /.#ホーマック関東　資材#./
        OVRF    FILE-CVCSG001,TOFILE-JCAFILEC.ONLBLIB
      #'00000882'# /.#ホーマック東北　資材#./
        OVRF    FILE-CVCSG001,TOFILE-JCAFILEB.ONLBLIB
      #'00000883'# /.#ホーマック北海道資材#./
        OVRF    FILE-CVCSG001,TOFILE-JCAFILEA.ONLBLIB
      #'00001427'# /.#ホーマック関東　植物#./
        OVRF    FILE-CVCSG001,TOFILE-JCAFILEG.ONLBLIB
      #'00014272'# /.#ホーマック東北　植物#./
        OVRF    FILE-CVCSG001,TOFILE-JCAFILEF.ONLBLIB
      #'00014273'# /.#ホーマック北海道植物#./
        OVRF    FILE-CVCSG001,TOFILE-JCAFILEE.ONLBLIB
      #'00013938'# /.#カーマ西日本　　資材#./
        OVRF    FILE-CVCSG001,TOFILE-JCAKAMAA.ONLBLIB
      #'00139381'# /.#カーマ東日本　　資材#./
        OVRF    FILE-CVCSG001,TOFILE-JCAKAMAC.ONLBLIB
      #'00017137'# /.#カーマ西日本　　植物#./
        OVRF    FILE-CVCSG001,TOFILE-JCAKAMAB.ONLBLIB
      #'00171371'# /.#カーマ東日本　　植物#./
        OVRF    FILE-CVCSG001,TOFILE-JCAKAMAD.ONLBLIB
      #'00100403'# /.#ダイキ 100403      #./
        OVRF    FILE-CVCSG001,TOFILE-JCADAIK1.ONLBLIB
      #'00100404'# /.#ダイキ 100404       #./
        OVRF    FILE-CVCSG001,TOFILE-JCADAIK4.ONLBLIB
      #'00100427'# /.#ダイキ 100427       #./
        OVRF    FILE-CVCSG001,TOFILE-JCADAIK3.ONLBLIB
      #'00100428'# /.#ダイキ 100428       #./
        OVRF    FILE-CVCSG001,TOFILE-JCADAIK6.ONLBLIB
      #'00100441'# /.#ダイキ 100441       #./
        OVRF    FILE-CVCSG001,TOFILE-JCADAIK2.ONLBLIB
      #'00100442'# /.#ダイキ 100442       #./
        OVRF    FILE-CVCSG001,TOFILE-JCADAIK5.ONLBLIB
      #'00001731'# /.#ケーヨー東日本　資材#./
        OVRF    FILE-CVCSG001,TOFILE-DCMKEIY1.ONLBLIB
      #'00001732'# /.#ケーヨー西日本　資材#./
        OVRF    FILE-CVCSG001,TOFILE-DCMKEIY2.ONLBLIB
      #'00007601'# /.#ケーヨー東日本　植物#./
        OVRF    FILE-CVCSG001,TOFILE-DCMKEIY3.ONLBLIB
      #'00007602'# /.#ケーヨー西日本　植物#./
        OVRF    FILE-CVCSG001,TOFILE-DCMKEIY4.ONLBLIB
    ELSE
      ?KEKA4 :=  '各社受領データＦ判定　取引先無　　！'
      ?MSGX :=  ?KEKA4
      SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
      GOTO ABEND
    END
    OVRF      FILE-DCMJYUL1,TOFILE-DCMJYUL1.TOKKLIB
    CALL      PGM-SSY8732B.TOKELIBO,PARA-(?P1)
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              ?KEKA4 :=  '受信ファイル⇒物品受領書Ｆ作成'
              GOTO ABEND END

/.##ＤＣＭＪＡＰＡＮ物品受領ＣＳＶ出力##./
SSY0808V:

    ?STEP :=   'SSY0808V '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-DCMJYRL2,TOFILE-DCMJYRL2.TOKKLIB
    OVRF      FILE-DCMJYRDT,TOFILE-DCMJYRDT.TOKKLIB
    CALL      PGM-SSY0808V.TOKSOLIB,PARA-(?P1)
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              ?KEKA4 :=  'ＤＣＭＪＡＰＡＮ物品受領ＣＳＶ出力'
              GOTO ABEND END

/.##受領データＣＳＶ出力##./
PFIMPORT:

    ?STEP :=   'PFIMPORT '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CASE ?P1 OF
      #'00000880'# /.#ホーマック関東　資材#./
        FIMPORT FILE-DCMJYRDT.TOKKLIB,PARA-DCMRPA,UNIT-1
      #'00000882'# /.#ホーマック東北　資材#./
        FIMPORT FILE-DCMJYRDT.TOKKLIB,PARA-DCMRPA,UNIT-2
      #'00000883'# /.#ホーマック北海道資材#./
        FIMPORT FILE-DCMJYRDT.TOKKLIB,PARA-DCMRPA,UNIT-3
      #'00001427'# /.#ホーマック関東　植物#./
        FIMPORT FILE-DCMJYRDT.TOKKLIB,PARA-DCMRPA,UNIT-4
      #'00014272'# /.#ホーマック東北　植物#./
        FIMPORT FILE-DCMJYRDT.TOKKLIB,PARA-DCMRPA,UNIT-5
      #'00014273'# /.#ホーマック北海道植物#./
        FIMPORT FILE-DCMJYRDT.TOKKLIB,PARA-DCMRPA,UNIT-6
      #'00013938'# /.#カーマ西日本　　資材#./
        FIMPORT FILE-DCMJYRDT.TOKKLIB,PARA-DCMRPA,UNIT-7
      #'00139381'# /.#カーマ東日本　　資材#./
        FIMPORT FILE-DCMJYRDT.TOKKLIB,PARA-DCMRPA,UNIT-8
      #'00017137'# /.#カーマ西日本　　植物#./
        FIMPORT FILE-DCMJYRDT.TOKKLIB,PARA-DCMRPA,UNIT-9
      #'00171371'# /.#カーマ東日本　　植物#./
        FIMPORT FILE-DCMJYRDT.TOKKLIB,PARA-DCMRPA,UNIT-10
      #'00100403'# /.#ダイキ 100403      #./
        FIMPORT FILE-DCMJYRDT.TOKKLIB,PARA-DCMRPA,UNIT-11
      #'00100404'# /.#ダイキ 100404       #./
        FIMPORT FILE-DCMJYRDT.TOKKLIB,PARA-DCMRPA,UNIT-12
      #'00100427'# /.#ダイキ 100427       #./
        FIMPORT FILE-DCMJYRDT.TOKKLIB,PARA-DCMRPA,UNIT-13
      #'00100428'# /.#ダイキ 100428       #./
        FIMPORT FILE-DCMJYRDT.TOKKLIB,PARA-DCMRPA,UNIT-14
      #'00100441'# /.#ダイキ 100441       #./
        FIMPORT FILE-DCMJYRDT.TOKKLIB,PARA-DCMRPA,UNIT-15
      #'00100442'# /.#ダイキ 100442       #./
        FIMPORT FILE-DCMJYRDT.TOKKLIB,PARA-DCMRPA,UNIT-16
      #'00001731'# /.#ケーヨー東日本　資材#./
        FIMPORT FILE-DCMJYRDT.TOKKLIB,PARA-DCMRPA,UNIT-17
      #'00001732'# /.#ケーヨー西日本　資材#./
        FIMPORT FILE-DCMJYRDT.TOKKLIB,PARA-DCMRPA,UNIT-18
      #'00007601'# /.#ケーヨー東日本　植物#./
        FIMPORT FILE-DCMJYRDT.TOKKLIB,PARA-DCMRPA,UNIT-19
      #'00007602'# /.#ケーヨー西日本　植物#./
        FIMPORT FILE-DCMJYRDT.TOKKLIB,PARA-DCMRPA,UNIT-20
    ELSE
      ?KEKA4 :=  '受領データＣＳＶ転送取引先　判定無！'
      ?MSGX :=  ?KEKA4
      SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
      GOTO ABEND
    END
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              ?KEKA4 :=  '受領データＣＳＶ出力'
              GOTO ABEND END

RTN:

    ?MSGX :=  '#DCM受領連続出力 = ' && ?P1 && ' 正常終了 #'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログを採取し，ＮＡＶへ連絡して下さい'
    ?KEKA3 :=  ''
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END

    RETURN    PGMEC-@PGMEC
