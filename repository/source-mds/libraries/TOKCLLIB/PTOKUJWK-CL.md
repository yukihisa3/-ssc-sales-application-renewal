# PTOKUJWK

**種別**: JCL  
**ライブラリ**: TOKCLLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLLIB/PTOKUJWK.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    Ｄ３６５連携　　　　　　  　　　     *  ./
/. *   JOB-ID      :    PTOKUJWK                             *  ./
/. *   JOB-NAME    :    日次計上データバックアップ⇒初期化　 *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
/.###ﾊﾟﾗﾒﾀ 定義####./
    PGM

/.###ﾜｰｸｴﾘｱ定義###./
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PTOKUJWK'    /.ﾌﾟﾛｸﾞﾗﾑID./
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
/.###出力パラメタ用###./
    VAR ?NSKENSU  ,STRING*10,VALUE-'0000000000' /.入荷実績件数./
    VAR ?SGKENSU  ,STRING*10,VALUE-'0000000000' /.作業実績件数./
    VAR ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2     ,STRING*50                  /.      2    ./
    VAR ?OPR3     ,STRING*50                  /.      3    ./
    VAR ?OPR4     ,STRING*50                  /.      4    ./
    VAR ?OPR5     ,STRING*50                  /.      5    ./

/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL D365DLIB/TOKELIB/TOKFLIB/TOKKLIB/TOKELIBO

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '日次計上データバックアップ⇒初期化'

/.##ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##確認画面##./
STEP010:

    ?OPR1  :=  '【日次計上データバックアップ⇒初期化】'
    ?OPR2  :=  '日次計上データのバックアップ⇒初期化を行ない'
    ?OPR3  :=  'ます。　　　　　　　　　　　　　　　　　　　'
    ?OPR4  :=  '実行ＯＫか確認してください。　　　　　　　　'
    ?OPR5  :=  ''
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)


/.##バックアップ##./
PSAVFILE:

    ?STEP :=   'PSAVFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SAVFILE FILE-TOKUWK.TOKWLIB/TOKUJWK.TOKWLIB,TODEV-@NONE,
            MODE-@USED,TOPATH-'/HGNAS/URIKEJBK',REP-@YES,
   /.       MODE-@USED,TOPATH-'/NAVPC/URIKEJBK',REP-@YES,
   ./       COMPRESS-@YES
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'バックアップ'
              GOTO ABEND
    END

/.##初期化１##./
PCLRFIL1:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CLRFILE FILE-TOKUWK.TOKWLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '初期化１'
              GOTO ABEND
    END

/.##初期化２##./
PCLRFIL2:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CLRFILE FILE-TOKUJWK.TOKWLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '初期化２'
              GOTO ABEND
    END

RTN:

              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  '日次計上データバックアップ⇒初期化が'
    ?KEKA2 :=  '正常終了しました。送信して下さい。'
    ?KEKA3 :=  ''
    ?KEKA3 :=  ''
    OVRDSPF FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
/.###ﾌﾟﾛｸﾞﾗﾑ終了###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:  /.ﾌﾟﾛｸﾞﾗﾑ異常終了時処理./
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  '日次計上データバックアップ⇒初期化以上'
    ?KEKA2 :=  'ログリスト等を採取しＮＡＶへ連絡して下'
    ?KEKA3 :=  'さい。'
    OVRDSPF FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
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

    RETURN    PGMEC-@PGMEC

```
