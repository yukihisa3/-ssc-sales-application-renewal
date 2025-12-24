/. *********************************************************** ./
/. *     サカタのタネ　特販システム（本社システム）          * ./
/. *   SYSTEM-NAME :    ＨＧ基幹システム　　　　　　　　　   * ./
/. *   JOB-ID      :    PNA02310                             * ./
/. *   JOB-NAME    :    連携_取込み結果受信エラー通知　　　 * ./
/. *                    (ｵﾝﾗｲﾝ/手書共通)　　　　　　　　　　 * ./
/. *          ※HALFTより起動されるｼﾞｮﾌﾞ(異常時)             * ./
/. *   UPDATE      :    HULFT/排他制御 2012.4.16             * ./
/. *********************************************************** ./
    PGM (HOSTID-?HOSTID,FILEID-?FILEID,DATE-?DATE,TIME-?TIME,
         COUNT-?COUNT,FILENM-?FILENM,LIBNM-?LIBNM)
/.###HULFT 定義####./
    PARA ?HOSTID   ,STRING*8  ,IN  ,REQ
    PARA ?FILEID   ,STRING*8  ,IN  ,REQ
    PARA ?DATE     ,STRING*8  ,IN  ,REQ
    PARA ?TIME     ,STRING*6  ,IN  ,REQ
    PARA ?COUNT    ,INTEGER   ,IN  ,REQ
    PARA ?FILENM   ,STRING*8  ,IN  ,REQ
    PARA ?LIBNM    ,STRING*8  ,IN  ,REQ
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS      ,STRING*08,VALUE-'        '      /.ﾜｰｸｽﾃｰｼｮﾝ文字  ./
    VAR ?WKSTN   ,NAME                            /.ﾜｰｸｽﾃｰｼｮﾝ名前  ./
    VAR ?PGMEC   ,INTEGER                         /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ ./
    VAR ?PGMECX  ,STRING*11                       /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ    ./
    VAR ?PGMEM   ,STRING*99                       /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ  ./
    VAR ?MSG     ,STRING*99(6)                    /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX    ,STRING*99                         /.SNDMSG表示用 ./
    VAR ?CLID    ,STRING*08,VALUE-'PNA02310'        /.CLID         ./
    VAR ?STEP    ,STRING*08                         /.STEP-ID      ./
    VAR ?CLNM    ,STRING*40                         /.CL名称       ./
    VAR ?KEKA1   ,STRING*40                         /.ﾒｯｾｰｼﾞ1      ./
    VAR ?KEKA2   ,STRING*40                         /.ﾒｯｾｰｼﾞ2      ./
    VAR ?KEKA3   ,STRING*40                         /.ﾒｯｾｰｼﾞ3      ./
    VAR ?KEKA4   ,STRING*40                         /.ﾒｯｾｰｼﾞ4      ./
   /. ↓2012.4.13 排他制御用 ./
    VAR ?UMU     ,STRING*1,VALUE-' '                /.有無区分用 ./
    VAR ?PBUMON  ,STRING*04,VALUE-'    '            /.部門ＣＤ　 ./
    VAR ?PTANCD  ,STRING*02,VALUE-'  '              /.担当者ＣＤ ./
    VAR ?PWS     ,STRING*08,VALUE-'        '        /.WKSTN名    ./
    VAR ?PWSN    ,NAME                              /.WKSTN名    ./
    VAR ?PREN    ,STRING*09,VALUE-'000000000'       /.連携ＮＯ   ./
    VAR ?PONL    ,STRING*01,VALUE-'0'               /.ｵﾝﾗｲﾝ/手書 ./
   /. ↑2012.4.13 ./

   /.##パラメタ（個別）##./

   /.##パラメタ（Ａ）##./

   /.##パラメタ（Ｂ）##./

/.------------------------------------------------------------------./

/.##実行PG名称ｾｯﾄ##./
    ?CLNM := '連携_取込み結果受信エラー通知　　　'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKKLIB/HULOLIB/HULFLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?CLID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
/.--?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES --./

/. ↓2012.4.16 ./
/.##排他端末情報取得##./
SNA0260B:

    ?STEP :=   'SNA0260B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-NARLOCF,TOFILE-NARLOCF.TOKKLIB
    CALL      PGM-SNA0260B.TOKELIBO,
                  PARA-(?UMU,?PBUMON,?PTANCD,?PWS,?PREN,?PONL)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '排他端末情報取得'
              GOTO ABEND
    END
    IF        ?PWS    =   '        '    THEN
              ?KEKA3 :=  '起動元端末名が空白！？'
              ?KEKA4 :=  '排他端末情報取得'
              GOTO ABEND
    END
    ?PWSN     :=  %NAME(?PWS)
/. ↑2012.4.16 ./

/.##異常警告リスト##./
SMG0020L:

    ?STEP :=   'SMG0020L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    /.##プリンタ出力位置判定##./
    IF  ?PBUMON = '2940'  THEN
        ?MSGX :=  '##九州ﾚｰｻﾞｰﾌﾟﾘﾝﾀへ出力##'
        SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES
        OVRPRTF FILE-PRTF,TOFILE-AMAGILBP.XUCL
    ELSE
        ?MSGX :=  '##本社ﾚｰｻﾞｰﾌﾟﾘﾝﾀへ出力##'
        SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES
        OVRPRTF FILE-PRTF,TOFILE-KAHMAPRT.XUCL
    END
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  '「苗業務システムからの'
    ?KEKA4 :=  '　取込結果通知データ受信」'
    CALL SMG0020L.TOKELIB
                    ,PARA-(?CLNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

  /. 起動元オペレータへ通知 2012.04.05 ./
    SNDMSG '！！！！！！！！！！！！！！！',TOWS-?PWSN,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    SNDMSG '！苗業務システムからの　　　！',TOWS-?PWSN,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    SNDMSG '！取込結果通知データ受信で　！',TOWS-?PWSN,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    SNDMSG '！異常が発生しました！　　　！',TOWS-?PWSN,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    SNDMSG '！確認してください。　　　　！',TOWS-?PWSN,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    SNDMSG '！！！！！！！！！！！！！！！',TOWS-?PWSN,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    RETURN    PGMEC-?PGMEC

/.------------------------------------------------------------------./

/.##異常終了処理##./
ABEND:

 /. OVRPRTF   FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIB  ./
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0020L.TOKELIB
                    ,PARA-(?CLNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?CLID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES
    END
  /. 起動元オペレータへ通知 2012.04.05 ./
    SNDMSG '！！！！！！！！！！！！！！！',TOWS-*,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    SNDMSG '！処理が異常終了しました！　！',TOWS-*,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    SNDMSG '！取込結果通知データ受信異常！',TOWS-*,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    SNDMSG '！→排他端末情報取得　　　　！',TOWS-*,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    SNDMSG '！！！！！！！！！！！！！！！',TOWS-*,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-?PGMEC
