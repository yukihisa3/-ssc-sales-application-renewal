# ERRPC1

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/ERRPC1.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受配信管理システム                   *  ./
/. *   JOB-ID      :    ERRPC1                              *  ./
/. *   JOB-NAME    :    受信エラー処理（ＰＣ－１）           *  ./
/. ***********************************************************  ./
    PGM
/.##パラメタ定義##./
    VAR  ?HIDUKE ,STRING*8,VALUE-'        ' /.受信日付./
    VAR  ?JIKAN  ,STRING*4,VALUE-'    '     /.受信時間./
    VAR  ?TOKCD  ,STRING*8,VALUE-'        ' /.受信取引先./
    VAR  ?PASS   ,STRING*13,VALUE-'             ' /.ﾊﾟｽﾜｰﾄﾞ./
    VAR  ?HENID  ,STRING*8,VALUE-'        ' /.変換ID./
    VAR  ?LINE   ,STRING*1,VALUE-' '        /.回線./
    VAR  ?YUSEN  ,STRING*1,VALUE-' '        /.回線優先./
    VAR  ?LIBNM  ,STRING*8,VALUE-'        ' /.集信LIB./
    VAR  ?FILNM  ,STRING*8,VALUE-'        ' /.集信FILE./
    VAR  ?KJOB   ,STRING*8,VALUE-'        ' /.起動JOB./
    VAR  ?KEKA   ,STRING*2,VALUE-'  '       /.結果./
    VAR  ?TIMES  ,STRING*4,VALUE-'    '     /.受信開始./
    VAR  ?TIMEE  ,STRING*4,VALUE-'    '     /.受信終了./
    VAR  ?JRL    ,STRING*4,VALUE-'    '     /.ﾚｺｰﾄﾞ長./
    VAR  ?TOKNM  ,STRING*15,VALUE-'               ' /.ﾄﾘﾋｷｻｷﾒｲｼｮｳ./
    VAR  ?HENKEKA,STRING*2,VALUE-'  '       /.変換処理結果./
/.##ワーク定義##./
    VAR  ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR  ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR  ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR  ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR  ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR  ?PGMID    ,STRING*8,VALUE-'ERRPC1 '  /.PROGRAM-ID./
    VAR  ?STEP     ,STRING*8                   /.STEP-ID./

    ACTMSGQ MAXRMSG-50

/.##プログラム開始メッセージ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
    SNDMSG MSG-'＊　受信エラー処理　　＊',TO-XCTL
    SNDMSG MSG-'＊　　（ＰＣ－１）　　＊',TO-XCTL
    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB

/.##パラメタファイルより受信パラメタ取得##./
    CALL SCVMSG.TOKELIB,PARA-('ﾊﾟﾗﾒﾀ ｼｭﾄｸ START    ')
SCV0150B:

    ?STEP :=   'SCV0150B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-PARAFILE,TOFILE-PARAPC1.TOKFLIB
    CALL PGM-SCV0150B.TOKELIB,
         PARA-(?HIDUKE,?JIKAN,?TOKCD,?PASS,?HENID,?LINE,?YUSEN,
               ?LIBNM,?FILNM,?KJOB,?KEKA,?TIMES,?TIMEE,?JRL,?TOKNM)
    IF  @PGMEC ^= 0 THEN
        GOTO  ABEND
    END

/.##回線クリア##./
    CALL SCVMSG.TOKELIB,PARA-('ｶｲｾﾝ ｸﾘｱ   START    ')
SCV0120B:

    ?STEP :=   'SCV0120B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-JHMKAIL1,TOFILE-JHMKAIL1.TOKFLIB
    CALL PGM-SCV0120B.TOKELIB,PARA-(?LINE,?YUSEN)
    IF  @PGMEC ^= 0 THEN
        GOTO  ABEND
    END

/.##結果更新##./
    CALL SCVMSG.TOKELIB,PARA-('ｼﾞｭｼﾝｹｯｶ ｺｳｼﾝ START ')
SCV0090B:

    ?STEP :=   'SCV0090B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

   /.##ｴﾗｰ結果更新##./
    DEFLIBL TOKELIB/TOKFLIB
/.  CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
    IF  @PGMEC ^= 0 THEN
        GOTO  ABEND
    END
  ./
/.##確認Ｆ初期化##./
    CALL SCVMSG.TOKELIB,PARA-('PCCHK1 CLRFILE START')
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-PCCHK1.TOKFLIB
    IF  @PGMEC ^= 0 THEN
        GOTO  ABEND
    END

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:
    CALL SCVMSG.TOKELIB,PARA-('ｼﾞｭｼﾝ ｶﾝｷｮｳ ﾁｪｯｸ    ')

   /.##ｴﾗｰﾘｽﾄ発行##./
    SBMJOB JOB-KEKALIST,JOBD-CVCS.XUCL,JOBK-@B,
           PGM-SCV0160B.TOKELIB,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
           @S/@T,LIBL-TOKFLIB/TOKELIB,PARA-(?HIDUKE,?JIKAN,
           ?TOKCD,?KEKA)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:
    CALL SCVMSG.TOKELIB,PARA-('ｲｼﾞｮｳ ｼｭｳﾘｮｳ  HENINS')

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND ' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END
    SNDMSG ?PGMEM,TO-XCTL
    DSPLOG OUTPUT-@LIST
    SNDMSG MSG-'＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃',
           TO-XCTL
    SNDMSG MSG-'＃＜受信処理において異常終了が発生　＞＃',
           TO-XCTL
    SNDMSG MSG-'＃　エラー情報を回収し、至急、　　　　＃',
           TO-XCTL
    SNDMSG MSG-'＃　ナブ・アシストまで連絡して下さい。＃',
           TO-XCTL
    SNDMSG MSG-'＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃',
           TO-XCTL
    RETURN    PGMEC-@PGMEC

```
