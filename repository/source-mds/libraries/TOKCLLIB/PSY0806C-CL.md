# PSY0806C

**種別**: JCL  
**ライブラリ**: TOKCLLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLLIB/PSY0806C.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    欠品データ連続抽出　　　　　　       *  ./
/. *   JOB-ID      :    PSY0806C                             *  ./
/. *   JOB-NAME    :    欠品データ連続抽出　　　　　　       *  ./
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
    VAR ?PGMID    ,STRING*8,VALUE-'PSY0806C'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKDTLIB/TOKELIBO
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '欠品データ連続抽出'

/.##欠品データ連続抽出##./
SETP001:

    ?STEP :=   'STEP001  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SNDMSG '##20200302-0859##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200302','0859')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200303-0907##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200303','0907')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200304-0901##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200304','0901')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200305-0859##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200305','0859')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200305-1659##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200305','1659')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200306-0905##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200306','0905')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200309-0858##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200309','0858')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200309-0932##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200309','0932')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200311-0913##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200311','0913')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200311-1659##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200311','1659')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200312-0859##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200312','0859')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200312-1659##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200312','1659')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200313-0905##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200313','0905')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200316-0853##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200316','0853')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200317-0905##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200317','0905')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200318-0904##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200318','0904')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200319-0904##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200319','0904')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200319-1659##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200319','1659')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200320-0841##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200320','0841')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200323-0907##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200323','0907')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200324-0901##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200324','0901')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200325-0905##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200325','0905')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200326-0905##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200326','0905')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200326-1700##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200326','1700')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200326-1711##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200326','1711')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200330-0859##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200330','0859')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200331-0903##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200331','0903')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

    SNDMSG '##20200402-0858##',TO-XCTL.@ORGPROF,JLOG-@YES
    CALL SSY0806C.TOKSOLIB,PARA-('20200402','0858')
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF  ?PGMEC ^= 0  THEN
              GOTO ABEND END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    ?PGMEC    :=    @PGMEC
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

    RETURN    PGMEC-@PGMEC

```
