/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷検品サブシステム                 *  ./
/. *   JOB-ID      :    PKE00600                             *  ./
/. *   JOB-NAME    :    検品グループマスタ送信データ作成     *  ./
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
    VAR ?PGMID    ,STRING*8,VALUE-'PKE00600'    /.ﾌﾟﾛｸﾞﾗﾑID./
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

/.##出荷検品用検品グループマスタ##./
SKE0060B:

    ?STEP :=   'SKE0060B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SOKKENL1,TOFILE-SOKKENL1.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-KNPKPG01,TOFILE-KNPKPG01.TOKWLIB
    OVRF      FILE-KNPKPG02,TOFILE-KNPKPG62.TOKFLIB
    OVRF      FILE-KNPKPG03,TOFILE-KNPKPG63.TOKFLIB
    OVRF      FILE-KNPKPG04,TOFILE-KNPKPG04.TOKWLIB
    OVRF      FILE-KNPKPG05,TOFILE-KNPKPG05.TOKWLIB
    OVRF      FILE-KNPKPG06,TOFILE-KNPKPG06.TOKWLIB
    OVRF      FILE-KNPKPG07,TOFILE-KNPKPG07.TOKWLIB
    OVRF      FILE-KNPKPG08,TOFILE-KNPKPG08.TOKWLIB
    OVRF      FILE-KNPKPG09,TOFILE-KNPKPG09.TOKWLIB
    OVRF      FILE-KNPKPG10,TOFILE-KNPKPG10.TOKWLIB
    OVRF      FILE-KNPKPG11,TOFILE-KNPKPG11.TOKWLIB
    OVRF      FILE-KNPKPG12,TOFILE-KNPKPG12.TOKWLIB
    OVRF      FILE-KNPKPG13,TOFILE-KNPKPG13.TOKWLIB
    OVRF      FILE-KNPKPG14,TOFILE-KNPKPG14.TOKWLIB
    OVRF      FILE-KNPKPG15,TOFILE-KNPKPG15.TOKWLIB
    OVRF      FILE-KNPKPG16,TOFILE-KNPKPG16.TOKWLIB
    OVRF      FILE-KNPKPG17,TOFILE-KNPKPG17.TOKWLIB
    OVRF      FILE-KNPKPG18,TOFILE-KNPKPG18.TOKWLIB
    OVRF      FILE-KNPKPG19,TOFILE-KNPKPG19.TOKWLIB
    OVRF      FILE-KNPKPG20,TOFILE-KNPKPG20.TOKWLIB
    OVRF      FILE-KNPKPK01,TOFILE-KNPKPK01.TOKWLIB
    OVRF      FILE-KNPKPK02,TOFILE-KNPKPK62.TOKFLIB
    OVRF      FILE-KNPKPK03,TOFILE-KNPKPK63.TOKFLIB
    OVRF      FILE-KNPKPK04,TOFILE-KNPKPK04.TOKWLIB
    OVRF      FILE-KNPKPK05,TOFILE-KNPKPK05.TOKWLIB
    OVRF      FILE-KNPKPK06,TOFILE-KNPKPK06.TOKWLIB
    OVRF      FILE-KNPKPK07,TOFILE-KNPKPK07.TOKWLIB
    OVRF      FILE-KNPKPK08,TOFILE-KNPKPK08.TOKWLIB
    OVRF      FILE-KNPKPK09,TOFILE-KNPKPK09.TOKWLIB
    OVRF      FILE-KNPKPK10,TOFILE-KNPKPK10.TOKWLIB
    OVRF      FILE-KNPKPK11,TOFILE-KNPKPK11.TOKWLIB
    OVRF      FILE-KNPKPK12,TOFILE-KNPKPK12.TOKWLIB
    OVRF      FILE-KNPKPK13,TOFILE-KNPKPK13.TOKWLIB
    OVRF      FILE-KNPKPK14,TOFILE-KNPKPK14.TOKWLIB
    OVRF      FILE-KNPKPK15,TOFILE-KNPKPK15.TOKWLIB
    OVRF      FILE-KNPKPK16,TOFILE-KNPKPK16.TOKWLIB
    OVRF      FILE-KNPKPK17,TOFILE-KNPKPK17.TOKWLIB
    OVRF      FILE-KNPKPK18,TOFILE-KNPKPK18.TOKWLIB
    OVRF      FILE-KNPKPK19,TOFILE-KNPKPK19.TOKWLIB
    OVRF      FILE-KNPKPK20,TOFILE-KNPKPK20.TOKWLIB
    CALL      PGM-SKE0060B.TOKELIB
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