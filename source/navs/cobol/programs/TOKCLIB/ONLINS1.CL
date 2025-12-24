/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受配信管理システム                   *  ./
/. *   JOB-ID      :    ONLINS1                              *  ./
/. *   JOB-NAME    :    受信エラー処理（ＴＥＬ－１）         *  ./
/. ***********************************************************  ./
    PGM
/.##パラメタ定義##./
    VAR  ?LINE   ,STRING*1,VALUE-'I'        /.回線./
    VAR  ?YUSEN  ,STRING*1,VALUE-'1'        /.回線優先./
    VAR  ?LIBNM  ,STRING*8,VALUE-'TOKFLIB ' /.集信LIB./
    VAR  ?FILNM  ,STRING*8,VALUE-'PARAINS1' /.集信FILE./
/.##ワーク定義##./
    VAR  ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR  ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR  ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR  ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR  ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR  ?PGMID    ,STRING*8,VALUE-'ONLINS1 '  /.PROGRAM-ID./
    VAR  ?STEP     ,STRING*8                   /.STEP-ID./
/.##結果区分定義##./
    VAR  ?KEKA     ,STRING*2,VALUE-'00'        /.更新ｹｯｶFLG./
/.##受信結果区分##./
    VAR  ?EKEKA    ,STRING*2,VALUE-'00'        /.更新ｹｯｶFLG./
/.##受信取引先##./
    VAR  ?TORICD   ,STRING*8,VALUE-'00000000'  /.受信取引先./

/.##プログラム開始メッセージ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG MSG-'＊　受信エラーチェック＊',TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG MSG-'＊　　（ＩＮＳ－１）　＊',TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL.@ORGPROF,JLOG-@YES

    CALL SCVMSG.TOKELIB,PARA-('## ｴﾗｰ ﾁｪｯｸ WAIT1 ##')
    CALL TWAIT.XUCL
    CALL SCVMSG.TOKELIB,PARA-('## ｴﾗｰ ﾁｪｯｸ WAIT2 ##')
    CALL TWAIT.XUCL
    CALL SCVMSG.TOKELIB,PARA-('## ｴﾗｰ ﾁｪｯｸ WAIT3 ##')
    CALL TWAIT.XUCL

/.##エラー状況チェック##./
JYUCHK2:

    ?STEP :=   'JYUCHK2 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF FILE-ERRCHK1,TOFILE-ERRCHK1.TOKFLIB
    CALL PGM-JYUCHK2.TOKELIB,PARA-(?LINE,?YUSEN,?EKEKA)
    IF  @PGMEC ^= 0 THEN
        GOTO  ABEND
    END

/.##ｴﾗｰ判定##./
ERRCHK:

    ?STEP :=   'ERRCHK  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    IF    ?EKEKA  =  '00'  THEN
           GOTO   RTN
    END

/.##パラメタファイルＧＰ６０００へ転送##./
PARASND:

    ?STEP :=   'PARASND '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.##結果区分更新##./
  /.?KEKA := '99' ./
    IF   ?EKEKA  = '05'  THEN
         ?KEKA  := '05'
    ELSE
         IF   ?EKEKA = '84' THEN
              ?KEKA := '84'
         ELSE
              ?KEKA := '99'
         END
    END
    OVRF FILE-PARAFILE,TOFILE-PARAINS1.TOKFLIB
    CALL PGM-SCV0130B.TOKELIB,PARA-(?KEKA)
    ?PGMEC := @PGMEC
    IF  ?PGMEC ^= 0 THEN
        GOTO   ABEND
    END

    SNDFILE COM-PCPATH02.PCNODE02,SFILE-PARAINS1.TOKFLIB,
            DFILE-'PARAINS1',RL-150,BF-1
    ?PGMEC := @PGMEC
    IF  ?PGMEC ^= 0 THEN
        GOTO   ABEND
    END

/.##ＧＰ６０００側　ＪＯＢ起動##./
SNDRSJJ:

    ?STEP :=   'SNDRSJJ '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SNDRSJJ COM-PCPATH02.PCNODE02,DFILE-'ERRINS1'
    ?PGMEC := @PGMEC
    IF  ?PGMEC ^= 0 THEN
        GOTO   ABEND
    END

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    CALL SCVMSG.TOKELIB,PARA-('## ﾎｰﾏｰG  ｹﾝｼｭｳCHK##')
    CALL TWAIT.XUCL
/.## ﾄﾘﾋｷｻｷCD判定 ##./
    OVRF FILE-PARAFILE,TOFILE-PARAINS1.TOKFLIB
    CALL SCV0200B.TOKELIB,PARA-(?TORICD)
    IF  ?PGMEC ^= 0 THEN
        ?MSGX :=  '##  <<検収ﾃﾞｰﾀ異常>>  ##'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
        ?MSGX :=  '## 検収ﾃﾞｰﾀ受信 ﾃﾞ ｴﾗｰ##'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
        ?MSGX :=  '## 手集信ﾃﾞ受信ｼﾃｸﾀﾞｻｲ##'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
        GOTO   ABEND
    ELSE
        IF  ?TORICD = '00000881'  THEN
            /.##ﾎｰﾏｰGｶﾝﾄｳ検収ﾃﾞｰﾀ受信処理開始##./
            ?MSGX :=  '## ﾎｰﾏｰG検収ﾃﾞｰﾀ受信 ##'
            SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
            CALL PHOGJYU.TOKELIB
        END
    END

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    RETURN    PGMEC-@PGMEC

ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND ' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
    SNDMSG ?PGMEM,TO-XCTL.@ORGPROF,JLOG-@YES
    DSPLOG OUTPUT-@LIST

    RETURN    PGMEC-@PGMEC
