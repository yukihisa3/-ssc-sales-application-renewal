/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＨＧ基幹システム　　　　　　　　　   *  ./
/. *   JOB-ID      :    PNA00120                             *  ./
/. *   JOB-NAME    :    取込依頼データ正常送信結果更新　　　 *  ./
/. *                                        　　　　　　　　 *  ./
/. *                  ※HALFTより起動されるジョブ　　　　　　*  ./
/. ***********************************************************  ./
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
    VAR ?CLID    ,STRING*08,VALUE-'PNA00120'        /.CLID         ./
    VAR ?STEP    ,STRING*08                         /.STEP-ID      ./
    VAR ?CLNM    ,STRING*40                         /.CL名称       ./
    VAR ?KEKA1   ,STRING*40                         /.ﾒｯｾｰｼﾞ1      ./
    VAR ?KEKA2   ,STRING*40                         /.ﾒｯｾｰｼﾞ2      ./
    VAR ?KEKA3   ,STRING*40                         /.ﾒｯｾｰｼﾞ3      ./
    VAR ?KEKA4   ,STRING*40                         /.ﾒｯｾｰｼﾞ4      ./
    VAR ?OPR1    ,STRING*50                         /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2    ,STRING*50                         /.      2    ./
    VAR ?OPR3    ,STRING*50                         /.      3    ./
    VAR ?OPR4    ,STRING*50                         /.      4    ./
    VAR ?OPR5    ,STRING*50                         /.      5    ./
    VAR ?UMU     ,STRING*1,VALUE-' '                /.有無区分用 ./
    VAR ?PBUMON  ,STRING*04,VALUE-'    '            /.部門ＣＤ　 ./
    VAR ?PTANCD  ,STRING*02,VALUE-'  '              /.担当者ＣＤ ./
    VAR ?PWS     ,STRING*08,VALUE-'        '        /.WKSTN名    ./
    VAR ?PWSN    ,NAME                              /.WKSTN名    ./
    VAR ?PREN    ,STRING*09,VALUE-'000000000'       /.連携ＮＯ   ./
    VAR ?PONL    ,STRING*01,VALUE-'0'               /.ｵﾝﾗｲﾝ/手書 ./

   /.##パラメタ（個別）##./

   /.##パラメタ（Ａ）##./

   /.##パラメタ（Ｂ）##./
    VAR ?PB01    ,STRING*06,VALUE-'000000'          /.部門・担当者 ./
    VAR ?PB02    ,STRING*01,VALUE-'0'               /.ｵﾝﾗｲﾝ手書種別./
    VAR ?PB03    ,STRING*09,VALUE-'000000000'       /.連携_(FROM) ./
    VAR ?PB04    ,STRING*09,VALUE-'000000000'       /.連携_(TO)   ./
    VAR ?PB05    ,STRING*15,VALUE-'               ' /.起動元ＩＰ   ./

/.------------------------------------------------------------------./

/.##実行PG名称ｾｯﾄ##./
    ?CLNM := '取込依頼データ正常送信結果更新　　　'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKKLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?CLID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

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
    ?PB01     :=  ?PBUMON &&  ?PTANCD

/.##取込依頼データ正常送信結果更新##./
SNA0190B:

    ?STEP :=   'SNA0190B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    OVRF      FILE-NARKANL1,TOFILE-NARKANL1.TOKKLIB
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
/.  OVRF      FILE-PRTFILE,TOFILE-PRTF.TOKELIB
./  /.##プリンタ出力位置判定##./
    IF  ?PBUMON = '2940'  THEN
        ?MSGX :=  '##九州ﾚｰｻﾞｰﾌﾟﾘﾝﾀへ出力##'
        SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES
        OVRPRTF FILE-XU04LP,TOFILE-AMAGILBP.XUCL
    ELSE
        ?MSGX :=  '##本社ﾚｰｻﾞｰﾌﾟﾘﾝﾀへ出力##'
        SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES
        OVRPRTF FILE-XU04LP,TOFILE-KAHMAPRT.XUCL
    END
    CALL      PGM-SNA0190B.TOKELIBO
                    ,PARA-(?PB01,?PONL,?PREN,?PB04,?PB05)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '小売連携データ送信完了処理'
              GOTO ABEND
    END


/.##正常終了処理##./
RTN:
  /. ---------------------------------------------------
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  ''
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?CLNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ------------------------------------------------------./
    ?MSGX :=  '***   '  && ?CLID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

  /. 起動端末へ通知            ./
    SNDMSG '＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊',TOWS-?PWSN,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    SNDMSG '＊苗業務システムへ　　　　　＊',TOWS-?PWSN,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    SNDMSG '＊取込依頼が送信されました。＊',TOWS-?PWSN,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    SNDMSG '＊結果通知をお待ち下さい。　＊',TOWS-?PWSN,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    SNDMSG '＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊',TOWS-?PWSN,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES


    RETURN    PGMEC-@PGMEC
/.------------------------------------------------------------------./

/.##異常終了処理##./
ABEND:

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
  /. 全オペレータへ通知 2012.04.05 ./
    SNDMSG '！！！！！！！！！！！！！！！',TOWS-*,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    SNDMSG '！処理が異常終了しました！　！',TOWS-*,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    SNDMSG '！「取込依頼送信結果更新」　！',TOWS-*,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    SNDMSG '！確認して下さい！　　　　　！',TOWS-*,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    SNDMSG '！！！！！！！！！！！！！！！',TOWS-*,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-?PGMEC
