# KENLSTT

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/KENLSTT.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受配信管理システム                   *  ./
/. *   JOB-ID      :    KENLST                               *  ./
/. *   JOB-NAME    :    受信件数リスト                       *  ./
/. ***********************************************************  ./
    PGM
/.##パラメタ定義##./
    VAR  ?HIDUKE ,STRING*8,VALUE-'20010611' /.受信日付./
    VAR  ?JIKAN  ,STRING*4,VALUE-'0930'     /.受信時間./
    VAR  ?TOKCD  ,STRING*8,VALUE-'00000007' /.受信取引先./
    VAR  ?LINE   ,STRING*1,VALUE-'I'        /.回線種別./
    VAR  ?YUSEN  ,STRING*1,VALUE-'1'        /.優先順位./
/.##ワーク定義##./
    VAR  ?GNO1     ,STRING*8,VALUE-'00000007'  /.受信取引先./
    VAR  ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR  ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR  ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR  ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR  ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR  ?PGMID    ,STRING*8,VALUE-'KENLST  '  /.PROGRAM-ID./
    VAR  ?STEP     ,STRING*8                   /.STEP-ID./

/.##プログラム開始メッセージ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB

/.##振分け件数ﾘｽﾄ出力##./
KENLST:

    ?STEP :=   'KENLST  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF    FILE-JHMKENL1,TOFILE-JHMKENL1.TOKFLIB
    OVRF    FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF    FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF    FILE-JHMEOSL1,TOFILE-JHMEOSL1.TOKFLIB
    CALL    PGM-SJH0013L.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD)
    ?PGMEC := @PGMEC
    IF   ?PGMEC ^= 0 THEN
         GOTO      ABEND
    END
/.##回線ﾏｽﾀ初期化##./
LINECLR:

    ?STEP :=   'LINECRT '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF    FILE-JHMKAIL1,TOFILE-JHMKAIL1.TOKFLIB
    CALL    PGM-SCV0120B.TOKELIB,PARA-(?LINE,?YUSEN)
    ?PGMEC := @PGMEC
    IF   ?PGMEC ^= 0 THEN
         GOTO      ABEND
    END
/.##セキチュー特別処理##./
SEKICHU:

    ?STEP :=   'SEKICHU '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    IF   ?TOKCD = '00321717'   THEN
         OVRF    FILE-JHMKENL1,TOFILE-JHMKENL1.TOKFLIB
         OVRF    FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
         OVRF    FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
         OVRF    FILE-JHMEOSL1,TOFILE-JHMEOSL1.TOKFLIB
         CALL    PGM-SJH0013L.TOKELIB,PARA-(?HIDUKE,?JIKAN,?GNO1)
         ?PGMEC := @PGMEC
       IF     ?PGMEC ^= 0 THEN
              GOTO      ABEND
       END
    END

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

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
    RETURN    PGMEC-@PGMEC

```
