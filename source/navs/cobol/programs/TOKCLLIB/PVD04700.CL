/. ***********************************************************  ./
/. *   サカタのタネ                                          *  ./
/. *   SYSTEM-NAME :    Ｄ３６５連携　　　　　　　　         *  ./
/. *   JOB-ID      :    PVD04700                             *  ./
/. *   JOB-NAME    :    倉庫間在庫移動入力チェックリスト　   *  ./
/. *   UPDATE      :                                         *  ./
/. *                                                         *  ./
/. ***********************************************************  ./
    PGM

    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMES    ,STRING*5,VALUE-'     '
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PVD04700'
    VAR       ?STEP     ,STRING*8
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門./
    VAR       ?TANCD8   ,STRING*8,VALUE-'      '   /.部門+担当者./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者./
    VAR       ?OUTTAN   ,STRING*2,VALUE-'  '       /.担当者./
    VAR       ?SOKO     ,STRING*2,VALUE-'  '       /.倉庫ＣＤ    ./
    VAR       ?DSOKO    ,STRING*2,VALUE-'  '       /.代表倉庫    ./

    VAR       ?OUTSOK   ,STRING*2,VALUE-'  '       /.倉庫ＣＤ./
    VAR       ?STDATE   ,STRING*8,VALUE-'00000000' /.開始日付./
    VAR       ?EDDATE   ,STRING*8,VALUE-'00000000' /.終了日付./
    VAR       ?STDEN    ,STRING*7,VALUE-'0000000'  /.開始伝票./
    VAR       ?EDDEN    ,STRING*7,VALUE-'0000000'  /.終了伝票./
    VAR       ?HAKKBN   ,STRING*1,VALUE-'0'        /.発行区分./
    VAR       ?PGNM     ,STRING*40                 /.ﾒｯｾｰｼﾞ1./
    VAR       ?KEKA1    ,STRING*40                 /.      2./
    VAR       ?KEKA2    ,STRING*40                 /.      3./
    VAR       ?KEKA3    ,STRING*40                 /.      4./
    VAR       ?KEKA4    ,STRING*40                 /.      5./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '倉庫間在庫移動入力チェックリスト'

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL  D365DLIB/TOKELIB/TOKELIBO/TOKSOLIB/TOKDLIB/TOKFLIB/
             TOKKLIB/HGWORKLB/TOKDTLIB/TOKMDLIB

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKO,?DSOKO)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  '倉庫コード取得'
              GOTO ABEND
    END

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ログインユーザー情報取得'
              GOTO ABEND
    END
    ?TANCD := %SBSTR(?TANCD8,1,2)

/.----------------------------------------------------------------./
/.##出力指示##./
NVD0470I:

    ?STEP :=   'NVD0470I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-NVD0470I.TOKSOLIB,PARA-(?BUMON,?TANCD,?SOKO,
                  ?OUTTAN,?OUTSOK,?STDATE,?EDDATE,?STDEN,?EDDEN)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
        IF  ?PGMEC  =  4010       THEN
            ?MSGX :=  '##取消終了##'
            SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
            RETURN
        ELSE
              ?KEKA4 :=  '出力指示'
              GOTO   ABEND
        END
    END

    ?MSGX :=  '## 担当CD　　　 = ' &&  ?OUTTAN
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX :=  '## 倉庫CD　　　 = ' &&  ?OUTSOK
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX :=  '## 開始日付     = ' &&  ?STDATE
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX :=  '## 終了日付     = ' &&  ?EDDATE
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX :=  '## 伝票番号Ｓ   = ' &&  ?STDEN
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX :=  '## 伝票番号Ｅ   = ' &&  ?EDDEN
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.----------------------------------------------------------------./
/.##倉庫間在庫移動入力チェックリスト##./
NVD0480L:

    ?STEP :=   'NVD0480L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    /.出力先スプールは実機必要に応じ記述./

    CALL      PGM-NVD0480L.TOKSOLIB,PARA-(?BUMON,?OUTTAN,?OUTSOK,
                                    ?STDATE,?EDDATE,?STDEN,?EDDEN)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '倉庫間在庫移動入力チェックリスト'
              GOTO   ABEND
    ELSE
              GOTO   RTN
    END


/.----------------------------------------------------------------./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  '出荷指示書発行処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取しＮＡＶへ連絡して下'
    ?KEKA3 :=  'さい。'
    OVRDSPF FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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

    RETURN    PGMEC-?PGMEC
