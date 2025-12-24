/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷検品サブシステム                 *  ./
/. *   JOB-ID      :    PKE00200                             *  ./
/. *   JOB-NAME    :    倉庫別検品取引先設定Ｍ送信データ作成 *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PKE00200'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##出荷検品用倉庫別検品取引先設定マスタ##./
SKE0020B:

    ?STEP :=   'SKE0020B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SOKKENL1,TOFILE-SOKKENL1.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-KNPKEN01,TOFILE-KNPKEN01.TOKWLIB
    OVRF      FILE-KNPKEN02,TOFILE-KNPKEN62.TOKFLIB
    OVRF      FILE-KNPKEN03,TOFILE-KNPKEN63.TOKFLIB
    OVRF      FILE-KNPKEN04,TOFILE-KNPKEN04.TOKWLIB
    OVRF      FILE-KNPKEN05,TOFILE-KNPKEN05.TOKWLIB
    OVRF      FILE-KNPKEN06,TOFILE-KNPKEN06.TOKWLIB
    OVRF      FILE-KNPKEN07,TOFILE-KNPKEN07.TOKWLIB
    OVRF      FILE-KNPKEN08,TOFILE-KNPKEN08.TOKWLIB
    OVRF      FILE-KNPKEN09,TOFILE-KNPKEN09.TOKWLIB
    OVRF      FILE-KNPKEN10,TOFILE-KNPKEN10.TOKWLIB
    OVRF      FILE-KNPKEN11,TOFILE-KNPKEN11.TOKWLIB
    OVRF      FILE-KNPKEN12,TOFILE-KNPKEN12.TOKWLIB
    OVRF      FILE-KNPKEN13,TOFILE-KNPKEN13.TOKWLIB
    OVRF      FILE-KNPKEN14,TOFILE-KNPKEN14.TOKWLIB
    OVRF      FILE-KNPKEN15,TOFILE-KNPKEN15.TOKWLIB
    OVRF      FILE-KNPKEN16,TOFILE-KNPKEN16.TOKWLIB
    OVRF      FILE-KNPKEN17,TOFILE-KNPKEN17.TOKWLIB
    OVRF      FILE-KNPKEN18,TOFILE-KNPKEN18.TOKWLIB
    OVRF      FILE-KNPKEN19,TOFILE-KNPKEN19.TOKWLIB
    OVRF      FILE-KNPKEN20,TOFILE-KNPKEN20.TOKWLIB
    OVRF      FILE-KNPKEK01,TOFILE-KNPKEK01.TOKWLIB
    OVRF      FILE-KNPKEK02,TOFILE-KNPKEK62.TOKFLIB
    OVRF      FILE-KNPKEK03,TOFILE-KNPKEK63.TOKFLIB
    OVRF      FILE-KNPKEK04,TOFILE-KNPKEK04.TOKWLIB
    OVRF      FILE-KNPKEK05,TOFILE-KNPKEK05.TOKWLIB
    OVRF      FILE-KNPKEK06,TOFILE-KNPKEK06.TOKWLIB
    OVRF      FILE-KNPKEK07,TOFILE-KNPKEK07.TOKWLIB
    OVRF      FILE-KNPKEK08,TOFILE-KNPKEK08.TOKWLIB
    OVRF      FILE-KNPKEK09,TOFILE-KNPKEK09.TOKWLIB
    OVRF      FILE-KNPKEK10,TOFILE-KNPKEK10.TOKWLIB
    OVRF      FILE-KNPKEK11,TOFILE-KNPKEK11.TOKWLIB
    OVRF      FILE-KNPKEK12,TOFILE-KNPKEK12.TOKWLIB
    OVRF      FILE-KNPKEK13,TOFILE-KNPKEK13.TOKWLIB
    OVRF      FILE-KNPKEK14,TOFILE-KNPKEK14.TOKWLIB
    OVRF      FILE-KNPKEK15,TOFILE-KNPKEK15.TOKWLIB
    OVRF      FILE-KNPKEK16,TOFILE-KNPKEK16.TOKWLIB
    OVRF      FILE-KNPKEK17,TOFILE-KNPKEK17.TOKWLIB
    OVRF      FILE-KNPKEK18,TOFILE-KNPKEK18.TOKWLIB
    OVRF      FILE-KNPKEK19,TOFILE-KNPKEK19.TOKWLIB
    OVRF      FILE-KNPKEK20,TOFILE-KNPKEK20.TOKWLIB
    CALL      PGM-SKE0020B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
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