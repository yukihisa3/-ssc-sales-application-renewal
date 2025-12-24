/. ***********************************************************  ./
/. *   サカタのタネ                                          *  ./
/. *   SYSTEM-NAME :    部門間在庫移動機能構築　　　         *  ./
/. *   JOB-ID      :    PBZ00400                             *  ./
/. *   JOB-NAME    :    部門間在庫移動リスト発行　　　　　   *  ./
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
    VAR       ?PGMID    ,STRING*8,VALUE-'PBZ00400'
    VAR       ?STEP     ,STRING*8
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門./
    VAR       ?TANCD8   ,STRING*8,VALUE-'      '   /.部門+担当者./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者./
    VAR       ?SOKO     ,STRING*2,VALUE-'  '       /.倉庫ＣＤ    ./
    VAR       ?DSOKO    ,STRING*2,VALUE-'  '       /.代表倉庫    ./

    VAR       ?JKBN     ,STRING*1,VALUE-'0'        /.発行日付区分./
    VAR       ?STDATE   ,STRING*8,VALUE-'00000000' /.開始日付./
    VAR       ?EDDATE   ,STRING*8,VALUE-'00000000' /.終了日付./
    VAR       ?HAKKBN   ,STRING*1,VALUE-'0'        /.発行区分./
    VAR       ?SYUKBN   ,STRING*1,VALUE-'0'        /.出力区分./
    VAR       ?PGNM     ,STRING*40                 /.ﾒｯｾｰｼﾞ1./
    VAR       ?KEKA1    ,STRING*40                 /.      2./
    VAR       ?KEKA2    ,STRING*40                 /.      3./
    VAR       ?KEKA3    ,STRING*40                 /.      4./
    VAR       ?KEKA4    ,STRING*40                 /.      5./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '部門間在庫移動リスト発行'

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKMDLIB/TOKDTLIB/TOKKLIB

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
/.##部門間在庫移動リスト発行指示##./
SBZ0040I:

    ?STEP :=   'SBZ0040I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SBZ0040I.TOKSOLIB,PARA-(?JKBN,?STDATE,?EDDATE,
                  ?HAKKBN,?SYUKBN)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
        IF  ?PGMEC  =  4010       THEN
            ?MSGX :=  '##取消終了##'
            SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
            RETURN
        ELSE
              ?KEKA4 :=  '部門間在庫移動リスト発行指示'
              GOTO   ABEND
        END
    END

    ?MSGX :=  '## 発行日付区分 = ' &&  ?JKBN
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX :=  '## 開始日付     = ' &&  ?STDATE
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX :=  '## 終了日付     = ' &&  ?EDDATE
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX :=  '## 発行区分     = ' &&  ?HAKKBN
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX :=  '## 出力区分     = ' &&  ?SYUKBN
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##帳票／ＣＳＶ判定##./
    IF   ?HAKKBN = '1'  THEN
         GOTO  SBZ0050V
    END

/.----------------------------------------------------------------./
/.##部門間在庫移動リスト発行##./
SBZ0050L:

    ?STEP :=   'SBZ0050L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    /. プリンタ出力先制御　空白：レーザー　１：連帳 ./
    IF    ?SYUKBN  =    '1'   THEN
          OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL
    ELSE
          OVRPRTF FILE-XU04LP,TOFILE-KAHMAPRT.XUCL
    END

    CALL      PGM-SBZ0050L.TOKSOLIB,PARA-(?JKBN,?STDATE,?EDDATE,
                  ?HAKKBN,?SYUKBN)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '部門間在庫移動リスト発行'
              GOTO   ABEND
    ELSE
              GOTO   RTN
    END

/.----------------------------------------------------------------./
/.##部門間在庫移動ＣＳＶ出力##./
SBZ0050V:

    ?STEP :=   'SBZ0050V'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SBZ0050V.TOKSOLIB,PARA-(?JKBN,?STDATE,?EDDATE,
                  ?HAKKBN,?SYUKBN)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '部門間在庫移動ＣＳＶ出力'
              GOTO   ABEND
    END

/.----------------------------------------------------------------./
/.##ＣＳＶデータ転送##./
PFIMPORT:

    ?STEP :=   'PFIMPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    FIMPORT FILE-FURIDCSV.TOKDTLIB,PARA-FURIDCSV,UNIT-1
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ＣＳＶデータ転送'
              GOTO   ABEND
    END

/.----------------------------------------------------------------./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  '部門間在庫移動リスト異常終了です。'
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