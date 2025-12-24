/. ***********************************************************  ./
/. *     サカタのタネ　Ｄ３６５連携機能構築                  *  ./
/. *   SYSTEM-NAME :    D365送受信制御　　　　               *  ./
/. *   JOB-ID      :    D365SEG1                             *  ./
/. *   JOB-NAME    :    Ｄ３６５送受信制御処理               *  ./
/. ***********************************************************  ./
    PGM (P1-?JDATE,P2-?JIKAN,P3-?SDRVKBN,P4-?JIKONO,P5-?JTANCD,
         P6-?ATSD,P7-?JIKOKEKA)
/.       P6-?ATSD,P7-?JIKOKEKA)
./
/.##パラメタ定義##./
    PARA ?JDATE    ,STRING*8,IN,VALUE-'        ' /.受信日./
    PARA ?JIKAN    ,STRING*4,IN,VALUE-'    '     /.受信時間./
    PARA ?SDRVKBN  ,STRING*2,IN,VALUE-'  '       /.送受信グループNO./
    PARA ?JIKONO   ,STRING*7,IN,VALUE-'       '  /.実行ＮＯ./
    PARA ?JTANCD   ,STRING*2,IN,VALUE-'  '       /.担当者./
    PARA ?ATSD     ,STRING*1,IN,VALUE-' '        /.実行区分./
    PARA ?JIKOKEKA ,STRING*1,IN,VALUE-' '       /.実行結果./
/.##ワーク定義##./
    VAR  ?PGMEC      ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR  ?PGMECX     ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR  ?PGMEM      ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR  ?MSGX       ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR  ?MSG        ,STRING*99                  /.ﾒｯｾｰｼﾞ標示用./
    VAR  ?PGMID      ,STRING*8,VALUE-'D365SEG1'  /.PROGRAM-ID./
    VAR  ?STEP       ,STRING*8,VALUE-'        '  /.STEP-ID./
    VAR  ?PGNM       ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR  ?KEKA1      ,STRING*40                  /.      2    ./
    VAR  ?KEKA2      ,STRING*40                  /.      3    ./
    VAR  ?KEKA3      ,STRING*40                  /.      4    ./
    VAR  ?KEKA4      ,STRING*40                  /.      5    ./
    VAR  ?DTKENSU    ,STRING*3,VALUE-'   '       /.回線ﾁｪｯｸ結果./
    VAR  ?JIKOKBN    ,STRING*1,VALUE-' '         /.実行区分./
    VAR  ?SEIGYOKB   ,STRING*1,VALUE-' '         /.制御区分./
    VAR  ?KEKAKBN    ,STRING*4,VALUE-'    '      /.結果区分./
    VAR  ?SYORIKEK   ,STRING*1,VALUE-' '         /.処理結果./
    VAR  ?KEKAKBN1   ,STRING*4,VALUE-'    '      /.結果区分./
    VAR  ?DJIKAN     ,STRING*6,VALUE-'      '    /.結果区分./

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'Ｄ３６５送受信制御処理'

/.##プログラム開始メッセージ##./

    ?MSG :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSG,TO-XCTL.@ORGPROF

    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
    SNDMSG MSG-'＊D365送受信制御起動]]＊',TO-XCTL
    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL D365DLIB/TOKELIB/TOKFLIB/TOKSOLIB/TOKCOLIB

    ?DJIKAN :=  ?JIKAN  &&  '00'

/.##送受信制御ファイル作成##./
NVS0100B:

    ?STEP :=   'NVS0100B'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSG,TO-XCTL

    CALL PGM-NVS0100B.TOKSOLIB,PARA-(?JDATE,?DJIKAN,?SDRVKBN,?JIKONO,
                                     ?ATSD,?JTANCD,?DTKENSU,?SEIGYOKB)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0 THEN
         ?KEKA4 := '【送受信制御ファイル作成】'
        GOTO  ABEND
    END
    IF  ?SEIGYOKB  =  '1'  THEN
        ?KEKAKBN :=  '1001'
        GOTO   ABEND
    END

/.##当日送受信制御ファイル情報更新##./
NVS0110B:

    ?STEP :=   'NVS0110B'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG   ?MSG,TO-XCTL

    SNDMSG MSG-'＊当日送受信制御ファイル情報更新＊',TO-XCTL
    CALL PGM-NVS0110B.TOKSOLIB,PARA-(?JDATE,?DJIKAN,'1',?JIKONO,
                                     ?SDRVKBN,'0000')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0 THEN
        ?KEKA4 := '【当日送受信制御ファイル情報更新】'
        ?KEKAKBN :=  '1002'
        GOTO  ABEND
    END

/.##Ｄ３６５送受信制御##./
NVS0120B:

    ?STEP :=   'NVS0120B'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSG,TO-XCTL

    SNDMSG MSG-'＊Ｄ３６５送受信制御＊',TO-XCTL
    CALL PGM-NVS0120B.TOKSOLIB,PARA-(?SYORIKEK,?KEKAKBN1)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0 THEN
        ?KEKA4 := '【Ｄ３６５送受信制御】'
        ?KEKAKBN :=  '1003'
        ?PGMECX  :=    %STRING(?PGMEC)
        ?MSG     :=    '### ' && ' PGMEC = ' &&
                         %SBSTR(?PGMECX,8,4) && ' ###'
        SNDMSG    ?MSG,TO-XCTL
        GOTO  ABEND
    END
    IF  ?KEKAKBN1 =  '1'  THEN
        ?KEKAKBN :=  '1004'
        GOTO  ABEND
    END

/.##当日送受信制御ファイル情報更新２##./
NVS0111B:

    ?STEP :=   'NVS0111B'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSG,TO-XCTL

    CALL PGM-NVS0110B.TOKSOLIB,PARA-(?JDATE,?DJIKAN,'2',?JIKONO,
                                     ?SDRVKBN,'0000')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0 THEN
        ?KEKA4 := '【当日送受信制御ファイル情報更新２】'
        ?KEKAKBN :=  '1005'
        GOTO  ABEND
    END


/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

/.##回線初期化##./
    ?STEP :=   'NVS0150B'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSG,TO-XCTL

    CALL PGM-NVS0150B.TOKSOLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0 THEN
        ?KEKA4 := '【正常終了　回線管理Ｍ初期化】'
        ?KEKAKBN :=  '1006'
        GOTO  ABEND
    END
    ?MSG :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSG,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    RETURN    PGMEC-@PGMEC

ABEND:
/.##異常セット##./
    ?STEP :=   'NVS0112B'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSG,TO-XCTL

    CALL PGM-NVS0110B.TOKSOLIB,PARA-(?JDATE,?DJIKAN,'2',?JIKONO,
                                     ?SDRVKBN,?KEKAKBN)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0 THEN
        ?KEKA4 := '【当日送受信制御ファイル情報更新２】'
        ?KEKAKBN :=  '1007'
        GOTO  ABEND
    END
    ?KEKA1 :=  '送受信チェック、起動が異常終了しました'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
/.##回線初期化##./
    ?STEP :=   'NVS0150B'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSG,TO-XCTL

    CALL PGM-NVS0150B.TOKSOLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0 THEN
        ?KEKA4 := '【異常終了　回線管理Ｍ初期化】'
        ?KEKAKBN :=  '1006'
        GOTO  ABEND
    END
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSGX(1)  :=    '### ' && ?PGMID && ' ABEND ' && ' ###'
    ?MSGX(2)  :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSGX(3)  :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSG :=   ?MSGX(?I)
               SNDMSG    ?MSG,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END
    RETURN    PGMEC-@PGMEC
