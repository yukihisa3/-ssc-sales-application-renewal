/. ***********************************************************  ./
/. *   サカタのタネ                                          *  ./
/. *   SYSTEM-NAME :    Ｄ３６５連携　　　　　　　　         *  ./
/. *   JOB-ID      :    PVD04300                             *  ./
/. *   JOB-NAME    :    出荷指示書発行　　　　　　　　　　   *  ./
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
    VAR       ?PGMID    ,STRING*8,VALUE-'PVD04300'
    VAR       ?STEP     ,STRING*8
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門./
    VAR       ?TANCD8   ,STRING*8,VALUE-'      '   /.部門+担当者./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者./
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
    ?PGNM :=  '出荷指示書発行発行'

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL D365DLIB/TOKELIB/TOKFLIB/TOKELIBO/TOKMDLIB/TOKKLIB/
            TOKDTLIB

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
/.##出荷指示書発行指示##./
NVD0430I:

    ?STEP :=   'NVD0430I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-NVD0430I.TOKSOLIB,PARA-(?BUMON,?SOKO,?DSOKO,
                  ?OUTSOK,?STDATE,?EDDATE,?STDEN,?EDDEN,?HAKKBN)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
        IF  ?PGMEC  =  4010       THEN
            ?MSGX :=  '##取消終了##'
            SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
            RETURN
        ELSE
              ?KEKA4 :=  '出荷指示書発行指示'
              GOTO   ABEND
        END
    END

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
    ?MSGX :=  '## 発行区分　   = ' &&  ?HAKKBN
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.----------------------------------------------------------------./
/.##出荷指示書発行##./
NVD0435L:

    ?STEP :=   'NVD0435L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    /. プリンタ出力先制御　空白：レーザー　１：連帳 ./
/.  IF    ?SYUKBN  =    '1'   THEN
./        OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL
/.  ELSE
          OVRPRTF FILE-XU04LP,TOFILE-KAHMAPRT.XUCL
    END
./
    CALL      PGM-NVD0435L.TOKSOLIB,PARA-(?BUMON,?SOKO,?DSOKO,
                  ?OUTSOK,?STDATE,?EDDATE,?STDEN,?EDDEN,?HAKKBN)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷指示書発行'
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