# D365SECK

**種別**: JCL  
**ライブラリ**: TOKCLLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLLIB/D365SECK.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　Ｄ３６５連携機能構築                  *  ./
/. *   SYSTEM-NAME :    D365送受信制御　　　　               *  ./
/. *   JOB-ID      :    D365SECK                             *  ./
/. *   JOB-NAME    :    送受信チェック、起動                 *  ./
/. ***********************************************************  ./
    PGM (P1-?JDATE,P2-?JIKAN,P3-?SDRVKBN,P4-?JIKONO,P5-?JTANCD,
         P6-?ATSD)

/.##パラメタ定義##./
    PARA ?JDATE    ,STRING*8,IN,VALUE-'        ' /.受信日./
    PARA ?JIKAN    ,STRING*4,IN,VALUE-'    '     /.受信時間./
    PARA ?SDRVKBN  ,STRING*2,IN,VALUE-'  '       /.送受信グループNO./
    PARA ?JIKONO   ,STRING*7,IN,VALUE-'       '  /.実行ＮＯ./
    PARA ?JTANCD   ,STRING*2,IN,VALUE-'  '       /.担当者./
    PARA ?ATSD     ,STRING*1,IN,VALUE-' '        /.実行区分./
    VAR  ?JIKOKEKA ,STRING*1,VALUE-' '       /.実行結果./
/.##ワーク定義##./
    VAR  ?PGMEC      ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR  ?PGMECX     ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR  ?PGMEM      ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR  ?MSGX       ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR  ?MSG        ,STRING*99                  /.ﾒｯｾｰｼﾞ標示用./
    VAR  ?PGMID      ,STRING*8,VALUE-'D365SECK'  /.PROGRAM-ID./
    VAR  ?STEP       ,STRING*8,VALUE-'        '  /.STEP-ID./
    VAR  ?PGNM       ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR  ?KEKA1      ,STRING*40                  /.      2    ./
    VAR  ?KEKA2      ,STRING*40                  /.      3    ./
    VAR  ?KEKA3      ,STRING*40                  /.      4    ./
    VAR  ?KEKA4      ,STRING*40                  /.      5    ./
    VAR  ?KAIKEKA    ,STRING*1,VALUE-' '         /.回線ﾁｪｯｸ結果./

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'Ｄ３６５送受信チェック＆起動'

/.##プログラム開始メッセージ##./

    ?MSG :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSG,TO-XCTL.@ORGPROF

    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
    SNDMSG MSG-'＊D365送受信CHK・起動]＊',TO-XCTL
    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL D365DLIB/TOKELIB/TOKFLIB/TOKSOLIB
    ?MSG :=  '# PARA日付       = ' && ?JDATE
    SNDMSG    ?MSG,TO-XCTL.@ORGPROF
    ?MSG :=  '# PARA時刻       = ' && ?JIKAN
    SNDMSG    ?MSG,TO-XCTL.@ORGPROF
    ?MSG :=  '# PARA送受信Ｇ   = ' && ?SDRVKBN
    SNDMSG    ?MSG,TO-XCTL.@ORGPROF
    ?MSG :=  '# PARA実行ＮＯ   = ' && ?JIKONO
    SNDMSG    ?MSG,TO-XCTL.@ORGPROF
    ?MSG :=  '# PARA担当者     = ' && ?JTANCD
    SNDMSG    ?MSG,TO-XCTL.@ORGPROF
    ?MSG :=  '# PARA実行区分   = ' && ?ATSD
    SNDMSG    ?MSG,TO-XCTL.@ORGPROF


/.##回線管理マスタチェック##./
NVS0140B:

    ?STEP :=   'NVS0140B'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSG,TO-XCTL

    CALL PGM-NVS0140B.TOKSOLIB,PARA-(?KAIKEKA)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0 THEN
         ?KEKA4 := '【回線管理マスタチェック】'
        GOTO  ABEND
    END
    IF  ?KAIKEKA  =  '1'  THEN
        ?KEKA1 :=  '回線が他で使用中です。使用中が確認後、'
        ?KEKA2 :=  '再度、送受信処理を実行してください！！'
        ?KEKA3 :=  ''
        ?KEKA4 := '【回線管理マスタチェック】'
        CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
        ?JIKOKEKA :=  '1'
        RETURN    PGMEC-@PGMEC
    END

/.##Ｄ３６５送受信制御処理起動##./
PSBMJOB:

    ?STEP :=   'PSBMJOB '
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSG,TO-XCTL

    SNDMSG MSG-'＊SBMJOB 起動開始]    ＊',TO-XCTL
    SBMJOB JOB-D365SEG1,JOBD-CVCS.XUCL,JOBK-@B,
    PGM-D365SEG1.TOKCOLIB,LOG-@YES!1024,
    PTY-8,PGMEL-@I/@L/@S/@T,LIBL-D365DLIB/TOKELIB/TOKFLIB/TOKDTLIB,
    PARA-(?JDATE,?JIKAN,?SDRVKBN,?JIKONO,?JTANCD,?ATSD,?JIKOKEKA)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0 THEN
        ?KEKA4 := '【Ｄ３６５送受信制御処理起動】'
        ?JIKOKEKA := '1'
        GOTO  ABEND
    END

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    ?MSG :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSG,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    RETURN    PGMEC-@PGMEC

ABEND:
/.##異常セット##./
    ?KEKA1 :=  '送受信チェック、起動が異常終了しました'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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

```
