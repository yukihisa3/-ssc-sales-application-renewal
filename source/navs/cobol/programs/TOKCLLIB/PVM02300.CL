/. ***********************************************************  ./
/. *  （株）サカタのタネ                                     *  ./
/. *   SYSTEM-NAME :    ＨＧ基幹　　　　　　　　　　　       *  ./
/. *   JOB-NAME    :   （ＥＸＣＥＬ取込）　　　　　　　　　　*  ./
/. *                    商品名称マスタ　取込チェックリスト　 *  ./
/. *                    ※Ｄ３６５連携対応　　　　　　　　   *  ./
/. *   JOB-ID      :    PVM02300  2020/04/15                 *  ./
/. *   UPDATE      :                                         *  ./
/. *                                                         *  ./
/. ***********************************************************  ./
    PGM  (P1-?OUTSYU)

/.  ##INパラメタ##                                              ./
    PARA  ?OUTSYU  ,STRING*1,IN,VALUE-' '
          /.実行区分                               ./
          /.  '1'    :   全件　　　　　　　　     ./
          /.  '2'    :   エラー分のみ　　　　　   ./

/.--------------------------------------------------------------./
    VAR  ?WS      ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝID./
    VAR  ?WKSTN   ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝID./
    VAR  ?PGMEC   ,INTEGER
    VAR  ?PGMES   ,STRING*5,VALUE-'     '
    VAR  ?PGMECX  ,STRING*11
    VAR  ?PGMEM   ,STRING*99
    VAR  ?MSG     ,STRING*99(6)
    VAR  ?MSGX    ,STRING*99
    VAR  ?PGMID   ,STRING*8,VALUE-'PVM02300'
    VAR  ?STEP    ,STRING*8
/.--------------------------------------------------------------./
    VAR  ?BUMON   ,STRING*4,VALUE-'    '     /.部門./
    VAR  ?TANCD8  ,STRING*8,VALUE-'        ' /.部門+担当者./
    VAR  ?TANCD   ,STRING*2,VALUE-'  '       /.担当者     ./
    VAR  ?SOKCD   ,STRING*2,VALUE-'00'       /.実行倉庫   ./
    VAR  ?DSOKCD  ,STRING*2,VALUE-'00'       /.代表倉庫   ./
/.--------------------------------------------------------------./
    VAR  ?CNT1    ,STRING*7,VALUE-'0000000'  /.件数(取込) ./
    VAR  ?CNT2    ,STRING*7,VALUE-'0000000'  /.件数(ＥＲ）./
/.--------------------------------------------------------------./
    VAR  ?OUTKBN  ,STRING*1,VALUE-' '        /.出力種別   ./
/.--------------------------------------------------------------./
    VAR  ?OPR1    ,STRING*50                 /.ﾒｯｾｰｼﾞ1    ./
    VAR  ?OPR2    ,STRING*50                 /.      2    ./
    VAR  ?OPR3    ,STRING*50                 /.      3    ./
    VAR  ?OPR4    ,STRING*50                 /.      4    ./
    VAR  ?OPR5    ,STRING*50                 /.      5    ./
/.--------------------------------------------------------------./
    VAR  ?MSG1    ,STRING*80                 /.開始終了MSG./
    VAR  ?PGNM    ,STRING*40                 /.ﾒｯｾｰｼﾞ1    ./
    VAR  ?KEKA1   ,STRING*40                 /.      2    ./
    VAR  ?KEKA2   ,STRING*40                 /.      3    ./
    VAR  ?KEKA3   ,STRING*40                 /.      4    ./
    VAR  ?KEKA4   ,STRING*40                 /.      5    ./
/.---------------------------------------------------------------./

  /.##端末別ファイルＩＤ編集用##./      /.当機能では非使用./

    /.共通_ライブラリ名./
    VAR  ?LIBNM    ,STRING*8,VALUE-'HGWORKLB'
    /.共通_ファイルＩＤワーク./
    VAR  ?FILID    ,NAME
    /.共通_ＬＩＢＩＤワーク./
    VAR  ?LIBID    ,NAME
    /.共通_端末番号./
    VAR  ?TANNO    ,STRING*03,VALUE-'   '

    /.データ_共通部_ファイルＩＤ先頭./
    VAR  ?STB      ,STRING*3,VALUE-'SMS'
    /.データ_個別部_ファイルＩＤ末尾./
    VAR  ?CS       ,STRING*2,VALUE-'CS'
    VAR  ?WK       ,STRING*2,VALUE-'WK'
    VAR  ?W1       ,STRING*2,VALUE-'W1'
    VAR  ?W2       ,STRING*2,VALUE-'W2'
    VAR  ?W3       ,STRING*2,VALUE-'W3'

    /.データ_OVRF_ファイルＩＤ./
    VAR  ?SMSXXXCS ,NAME!MOD
    VAR  ?SMSXXXWK ,NAME!MOD
    VAR  ?SMSXXXW1 ,NAME!MOD
    VAR  ?SMSXXXW2 ,NAME!MOD
    VAR  ?SMSXXXW3 ,NAME!MOD

    /.ファイルＩＤ表示用ワーク./
    VAR  ?DSPFILID,STRING*17


    VAR  ?FILNM7  ,STRING*7,VALUE-'       '
    VAR  ?FILNM8  ,STRING*8,VALUE-'        '

/.-----------------------------------------------------------./


/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKELIBO/TOKSOLIB/TOKDLIB/TOKFLIB/TOKKLIB
             /HGWORKLB/TOKDTLIB/D365DLIB

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
CLSTART:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM := '商品名称マスタ取込チェックリスト'
    CASE  ?OUTSYU     OF
          #'1'# ?PGNM := '商品名称マスタ　取込チェックリスト'
          #'2'# ?PGNM := '商品名称マスタ　取込エラーリスト　'
          ELSE  SNDMSG   '！！！！！！！！！！！！！！！！！！',
                          TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
                SNDMSG   '！実行区分を指定し、実行して下さい！',
                          TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
                SNDMSG   '！！！！！！！！！！！！！！！！！！',
                          TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
                RETURN
    END

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=  'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              GOTO   ABEND
    END
    ?TANCD := %SBSTR(?TANCD8,1,2)

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0   THEN
              ?KEKA4 := '倉庫コード取得'
              GOTO   ABEND
    END

/.##商品名称マスタ　取込チェックリスト##./
NVM0230L:

    ?STEP :=  'NVM0230L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    ?OUTKBN   :=  ?OUTSYU
/.実環境にあわせて変更ください./
/.  IF  ?BUMON  =  '2920'  THEN
        OVRPRTF FILE-XU04LP,TOFILE-LBPPRT01.TOKELIBO
    ELSE
        OVRPRTF FILE-XU04LP,TOFILE-LBPPRTE1.TOKELIBO
    END
./
    OVRF      FILE-SMSMNTW3,TOFILE-SMSMNTW3.HGWORKLB
    OVRF      FILE-SMSMNTW2,TOFILE-SMSMNTW2.HGWORKLB
    CALL      PGM-NVM0230L.TOKSOLIB,
              PARA-(?BUMON,?TANCD,?OUTKBN)
    ?PGMEC    := @PGMEC
    ?PGMES    := @PGMES
    IF        ?PGMEC ^= 0    THEN
              ?KEKA4 := 'リスト出力処理'
              GOTO   ABEND
    END

    GOTO  RTN

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  ''
    ?KEKA3 :=  'ログ採取し，ＮＡＶへ連絡して下さい。'
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB,
         PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO    ?MSGX  := ?MSG(?I)
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
    END

    RETURN    PGMEC-?PGMEC
