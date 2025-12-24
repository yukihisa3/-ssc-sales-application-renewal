# HENTEL2

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/HENTEL2.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受配信管理システム                   *  ./
/. *   JOB-ID      :    HENINS2                              *  ./
/. *   JOB-NAME    :    共通データ変換処理                   *  ./
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
    VAR  ?PGMID    ,STRING*8,VALUE-'HENINS2 '  /.PROGRAM-ID./
    VAR  ?STEP     ,STRING*8                   /.STEP-ID./
    VAR  ?LIBNAME  ,STRING*8,VALUE-'TOKELIB '  /.ﾌﾟﾛｸﾞﾗﾑ実行LIB./
    VAR  ?LIBN     ,NAME                       /.ﾌﾟﾛｸﾞﾗﾑ名編集用./
    VAR  ?PGN      ,NAME                       /.ﾌﾟﾛｸﾞﾗﾑ名編集用./
    VAR  ?PGLIB    ,NAME!MOD                   /.ﾌﾟﾛｸﾞﾗﾑ名編集用./
    VAR  ?PGNAME   ,STRING*17                  /.ﾌﾟﾛｸﾞﾗﾑ名表示用./
    VAR  ?HTIME    ,STRING*8,VALUE-'        '  /.ｼｽﾃﾑ時間退避用./
    VAR  ?HTIMES   ,STRING*4,VALUE-'    '      /.変換開始時間./
    VAR  ?HTIMEE   ,STRING*4,VALUE-'    '      /.変換終了時間./
/.##ﾌｧｲﾙ変換ﾜｰｸ##./
    VAR       ?LIBN1    ,NAME                       /.ﾗｲﾌﾞﾗﾘ名前型./
    VAR       ?FILN     ,NAME                       /.ﾌｧｲﾙ名前型./
    VAR       ?FILLIB   ,NAME!MOD                   /.ﾌｧｲﾙ拡張用./
    VAR       ?FILID    ,STRING*17                  /.ﾌｧｲﾙ名表示用./

    ACTMSGQ MAXRMSG-50

/.##プログラム開始メッセージ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
    SNDMSG MSG-'＊　データ変換開始　　＊',TO-XCTL
    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIB

/.##パラメタファイルより受信パラメタ取得##./
    CALL SCVMSG.TOKELIB,PARA-('ﾊﾟﾗﾒﾀ ｼｭﾄｸ START    ')
SCV0150B:

    ?STEP :=   'SCV0150B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-PARAFILE,TOFILE-PARATEL2.TOKFLIB
    CALL PGM-SCV0150B.TOKELIB,
         PARA-(?HIDUKE,?JIKAN,?TOKCD,?PASS,?HENID,?LINE,?YUSEN,
               ?LIBNM,?FILNM,?KJOB,?KEKA,?TIMES,?TIMEE,?JRL,?TOKNM)
    IF  @PGMEC ^= 0 THEN
        ?KEKA  :=  '70'
        GOTO  ABEND
    END

/.##受信ファイルバックアップ##./
    CALL SCVMSG.TOKELIB,PARA-('ｼﾞｭｼﾝﾌｧｲﾙﾊﾞｯｸｱｯﾌﾟ   ')
DATABACK:

    ?STEP :=   'DATABACK'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##受信ﾌｧｲﾙノ編集##./
    ?FILN   :=  %NAME(?FILNM)
    ?LIBN1  :=  %NAME(?LIBNM)
    ?FILLIB :=  %NCAT(?FILN,?LIBN1)
    ?FILID  :=  %STRING(?FILLIB)
    SNDMSG ?FILID,TO-XCTL.@ORGPROF,JLOG-@YES

    IF   ?JRL = '0128'  THEN
         OVRF FILE-CVCSG001,TOFILE-?FILLIB
         OVRF FILE-BKCVCS01,TOFILE-BACK0128.ONLBLIB
         CALL SJU2010B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD)
         IF  @PGMEC ^= 0 THEN
             ?KEKA  :=  '70'
             GOTO  ABEND
         END
    END
    IF   ?JRL = '0256'  THEN
         OVRF FILE-CVCSG002,TOFILE-?FILLIB
         OVRF FILE-BKCVCS02,TOFILE-BACK0256.ONLBLIB
         CALL SJU2020B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD)
         IF  @PGMEC ^= 0 THEN
             ?KEKA  :=  '70'
             GOTO  ABEND
         END
    END
    IF   ?JRL = '2048'  THEN
         OVRF FILE-CVCSG003,TOFILE-?FILLIB
         OVRF FILE-BKCVCS03,TOFILE-BACK2048.ONLBLIB
         CALL SJU2030B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD)
         IF  @PGMEC ^= 0 THEN
             ?KEKA  :=  '70'
             GOTO  ABEND
         END
    END

/.##ｴﾗｰ判断##./
    CALL SCVMSG.TOKELIB,PARA-('ﾊﾟﾗﾒﾀF ｹｯｶ CHK START')
ERRCHK:

    ?STEP :=   'ERRCHK  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '## 結果 = ' && ?KEKA
    SNDMSG    ?MSGX,TO-XCTL

    IF  ?KEKA  ^=  '00'  THEN
        IF  ?KEKA = '05' THEN
            GOTO RTN1
        ELSE
            IF  ?KEKA = '84' THEN
                 GOTO RTN1
            ELSE
                 GOTO ABEND
            END
        END
    END

    ?MSGX :=  '### 変換開始'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '### 取引先 = ' && ?TOKCD && ':' && ?TOKNM
    SNDMSG    ?MSGX,TO-XCTL

    CALL SCVMSG.TOKELIB,PARA-('ｼﾞｭｼﾝDT ﾍﾝｶﾝ   START')

HENKAN:

    ?STEP :=   'HENKAN  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
   /.##起動ﾌﾟﾛｸﾞﾗﾑ名称編集##./
    ?PGN   := %NAME(?HENID)
    ?LIBN  := %NAME(?LIBNAME)
    ?PGLIB := %NCAT(?PGN,?LIBN)
    ?PGNAME := %STRING(?PGLIB)
    ?MSGX  := '## 実行PG名称 = ' && ?PGNAME
    SNDMSG MSG-?MSGX,TO-XCTL

   /.##変換開始時間取得##./
    ?HTIME  := @STIME
    ?HTIMES := %SBSTR(?HTIME,1,4)
    ?HENKEKA := '00'

    CALL PGM-?PGLIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?LINE,?YUSEN,?LIBNM,
                          ?FILNM,?HENKEKA)
    CALL SCVMSG.TOKELIB,PARA-('PG END              ')
    ?PGMEC := @PGMEC
    IF  ?PGMEC ^= 0 THEN
        IF  ?HENKEKA ^=  '00'  THEN
            CALL SCVMSG.TOKELIB,PARA-('PG ABEND ST-CHK     ')
            ?MSGX := '## 変換異常終了 結果 = ' && ?HENKEKA
            SNDMSG MSG-?MSGX,TO-XCTL
            ?KEKA := ?HENKEKA
            GOTO      ABEND
        ELSE
            ?KEKA := '71'
            GOTO      ABEND
        END
    END

    CALL SCVMSG.TOKELIB,PARA-('ｼﾞｭｼﾝDT ﾍﾝｶﾝ   END  ')
   /.##変換開始時間取得##./
    ?HTIME  := @STIME
    ?HTIMEE := %SBSTR(?HTIME,1,4)

/.##受信時間開始／終了、変換時間開始／終了更新##./
    CALL SCVMSG.TOKELIB,PARA-('ﾍﾝｶﾝ TIME SET       ')
DATESET:

    ?STEP :=   'DATESET '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF    FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
    CALL    PGM-SCV0170B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,
                                       ?TIMES,?TIMEE,?HTIMES,?HTIMEE)
    ?PGMEC := @PGMEC
    IF   ?PGMEC ^= 0 THEN
         ?KEKA := '73'
         GOTO      ABEND
    END


/.##振分け件数ﾘｽﾄ出力##./
    CALL SCVMSG.TOKELIB,PARA-('ｹﾝｽｳ LST       START')
KENLST:

    ?STEP :=   'KENLST  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SBMJOB JOB-KENLSTT2,JOBD-CVCS.XUCL,JOBK-@B,PGM-KENLST.TOKELIB,
           VSIZE-5,RSIZE-16,LOG-@YES!1024,PARA-(?HIDUKE,?JIKAN,
           ?TOKCD,?LINE,?YUSEN)


/.##ﾌﾟﾛｸﾞﾗﾑ正常終了受信異常##./
RTN:
    CALL SCVMSG.TOKELIB,PARA-('ｾｲｼﾞｮｳ ｼｭｳﾘｮｳ HENTEL')

   /.##ｴﾗｰﾘｽﾄ発行##./
    SBMJOB JOB-KEKLSTT2,JOBD-CVCS.XUCL,JOBK-@B,
           PGM-SCV0160B.TOKELIB,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
           @S/@T,LIBL-TOKFLIB/TOKELIB,PARA-(?HIDUKE,?JIKAN,
           ?TOKCD,?KEKA)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了ゼロ件／リダイアルエラー##./
RTN1:
    CALL SCVMSG.TOKELIB,PARA-('0ｹﾝ/ﾘﾀﾞｲﾔﾙEND HENTEL')

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:
    CALL SCVMSG.TOKELIB,PARA-('ｲｼﾞｮｳ ｼｭｳﾘｮｳ  HENTEL')

   /.##ｴﾗｰ結果更新##./
    DEFLIBL TOKELIB/TOKFLIB
    CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
   /.##ｴﾗｰﾘｽﾄ発行##./
    SBMJOB JOB-KEKLSTT2,JOBD-CVCS.XUCL,JOBK-@B,
           PGM-SCV0160B.TOKELIB,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
           @S/@T,LIBL-TOKFLIB/TOKELIB,PARA-(?HIDUKE,?JIKAN,
           ?TOKCD,?KEKA)

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
