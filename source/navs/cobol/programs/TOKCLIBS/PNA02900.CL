/. *********************************************************** ./
/. *     サカタのタネ　特販システム（本社システム）          * ./
/. *   SYSTEM-NAME :    ＨＧ基幹システム　　　　　　　　　   * ./
/. *   JOB-ID      :    PNA02900                             * ./
/. *   JOB-NAME    :    ダミーデータ送信結果通知　　　　　　 * ./
/. *                                                  　　　 * ./
/. *          ※HALFTより起動されるｼﾞｮﾌﾞ(正常時)             * ./
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
    VAR ?CLID    ,STRING*08,VALUE-'PNA02900'        /.CLID         ./
    VAR ?STEP    ,STRING*08                         /.STEP-ID      ./
    VAR ?CLNM    ,STRING*40                         /.CL名称       ./
    VAR ?KEKA1   ,STRING*40                         /.ﾒｯｾｰｼﾞ1      ./
    VAR ?KEKA2   ,STRING*40                         /.ﾒｯｾｰｼﾞ2      ./
    VAR ?KEKA3   ,STRING*40                         /.ﾒｯｾｰｼﾞ3      ./
    VAR ?KEKA4   ,STRING*40                         /.ﾒｯｾｰｼﾞ4      ./
    VAR ?UMU     ,STRING*1,VALUE-' '                /.有無区分用 ./
    VAR ?PBUMON  ,STRING*04,VALUE-'    '            /.部門ＣＤ　 ./
    VAR ?PTANCD  ,STRING*02,VALUE-'  '              /.担当者ＣＤ ./
    VAR ?PWS     ,STRING*08,VALUE-'        '        /.WKSTN名    ./
    VAR ?PWSN    ,NAME                              /.WKSTN名    ./
    VAR ?PREN    ,STRING*09,VALUE-'000000000'       /.連携ＮＯ   ./
    VAR ?PONL    ,STRING*01,VALUE-'0'               /.ｵﾝﾗｲﾝ/手書 ./


/.------------------------------------------------------------------./

/.##実行PG名称ｾｯﾄ##./
    ?CLNM := 'ダミーデータ送信結果通知　　　　　'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKKLIB/HULOLIB/HULFLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?CLID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

/.##排他制御ファイル更新　##./
SNA0290B:

    ?STEP :=   'SNA0290B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

 /. OVRF      FILE-NARLOCF2,TOFILE-NARLOCF2.TOKKLIB  ./
    CALL      PGM-SNA0290B.TOKELIBO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND
    END
    RETURN    PGMEC-@PGMEC

/.------------------------------------------------------------------./

/.##異常終了処理##./
ABEND:

  /. 全オペレータへ通知            ./
    SNDMSG '！！！！！！！！！！！！！！！',TOWS-*,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    SNDMSG '！処理が異常終了しました！　！',TOWS-*,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    SNDMSG '！排他制御‐疎通フラグＯＮ　！',TOWS-*,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    SNDMSG '！リストを確認してください。！',TOWS-*,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    SNDMSG '！！！！！！！！！！！！！！！',TOWS-*,LEVEL-@C
                                           ,SLOG-@YES,JLOG-@YES
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  '排他制御‐疎通フラグＯＮ'
    ?KEKA3 :=  'リストを確認してください。'
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

    RETURN    PGMEC-?PGMEC
