# CEDISBMJ

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/CEDISBMJ.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　ＥＤＩ受信サーバ入替                  *  ./
/. *   SYSTEM-NAME :    受信振分ジョブ                       *  ./
/. *   JOB-ID      :    CEDISBMJ                             *  ./
/. *   JOB-NAME    :    受信振分処理                         *  ./
/. ***********************************************************  ./
    PGM (P1-?JDATE,P2-?JIKAN,P3-?DATAKBN,P4-?JYUHKBN,P5-?TORICD,
         P6-?KAIKBN,P7-?KAINO,P8-?TANMATU,P9-?PARAF,P10-?TUSINM)

/.##パラメタ定義##./
    PARA ?JDATE    ,STRING*8,IN,VALUE-'        ' /.受信日./
    PARA ?JIKAN    ,STRING*4,IN,VALUE-'    '     /.受信時間./
    PARA ?DATAKBN  ,STRING*2,IN,VALUE-'  '       /.ﾃﾞｰﾀ区分./
    PARA ?JYUHKBN  ,STRING*1,IN,VALUE-' '        /.受配信区分./
    PARA ?TORICD   ,STRING*8,IN,VALUE-'        ' /.取引先CD./
    PARA ?KAIKBN   ,STRING*1,IN,VALUE-' '        /.回線種別./
    PARA ?KAINO    ,STRING*1,IN,VALUE-' '        /.回先制御番号./
    PARA ?TANMATU  ,STRING*8,IN,VALUE-'        ' /.端末名./
    PARA ?PARAF    ,STRING*8,IN,VALUE-'        ' /.ﾊﾟﾗﾒﾀﾌｧｲﾙ名./
    PARA ?TUSINM   ,STRING*1,IN,VALUE-' '        /.通信ﾓｰﾄﾞ./
/.##ワーク定義##./
    VAR  ?PGMEC      ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR  ?PGMECX     ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR  ?PGMEM      ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR  ?MSGX       ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR  ?MSG        ,STRING*99                  /.ﾒｯｾｰｼﾞ標示用./
    VAR  ?PGMID      ,STRING*8,VALUE-'CEDISBMJ'  /.PROGRAM-ID./
    VAR  ?STEP       ,STRING*8,VALUE-'        '  /.STEP-ID./
    VAR  ?WAITC      ,INTEGER                    /.ﾊﾞｯｸｱｯﾌﾟﾏﾁ./

    VAR  ?LIB        ,STRING*8,VALUE-'TOKJLIB '  /.ﾗｲﾌﾞﾗﾘ名./
    VAR  ?TRGERF     ,STRING*8,VALUE-'        '  /.ﾄﾘｶﾞｰﾌｧｲﾙ名./
    VAR  ?LIBN       ,NAME                       /.ﾗｲﾌﾞﾗﾘ名./
    VAR  ?PARAFILE   ,NAME                       /.ﾊﾟﾗﾒﾀﾌｧｲﾙ名./
    VAR  ?TRGERFIL   ,NAME                       /.ﾄﾘｶﾞｰﾌｧｲﾙ名./
    VAR  ?PARALIB    ,NAME!MOD                   /.ﾊﾟﾗﾒﾀﾗｲﾌﾞﾗﾘ名./
    VAR  ?PARALIBN   ,STRING*17                  /.ﾊﾟﾗﾒﾀﾗｲﾌﾞﾗﾘ名./
    VAR  ?TRGERLIB   ,NAME!MOD                   /.ﾄﾘｶﾞｰﾗｲﾌﾞﾗﾘ名./
    VAR  ?TRGERNM    ,STRING*17                  /.ﾄﾘｶﾞｰﾌｧｲﾙ名./
    VAR  ?LIBONL     ,STRING*8,VALUE-'ONLBLIB'   /.ONLﾗｲﾌﾞﾗﾘ名./
    VAR  ?KEKA       ,STRING*1,VALUE-' '         /.結果./
    VAR  ?ANSERF     ,STRING*8,VALUE-'        '  /.ｱﾝｻｰﾌｧｲﾙ名./
    VAR  ?ANSERFIL   ,NAME                       /.ｱﾝｻｰﾌｧｲﾙ名./
    VAR  ?ANSERLIB   ,NAME!MOD                   /.ｱﾝｻｰﾗｲﾌﾞﾗﾘ名./
    VAR  ?ANSERNM    ,STRING*17                  /.ｱﾝｻｰ+ﾗｲﾌﾞﾗﾘ名./
    VAR  ?JOBNM      ,STRING*8,VALUE-'        '  /.JOB名./
    VAR  ?JOBNMN     ,NAME                       /.JOB名./
    VAR  ?SEIGYKBN   ,STRING*1,VALUE-' '         /.制御区分./
    VAR  ?USERID     ,STRING*8,VALUE-'FTPUSER'   /.ﾕｰｻﾞｰID./
    VAR  ?PASS       ,STRING*8,VALUE-'FTPUSER'   /.ﾕｰｻﾞｰPASS./
    VAR  ?USERIDN    ,NAME                       /.ﾕｰｻﾞｰID./
    VAR  ?PASSN      ,NAME                       /.ﾕｰｻﾞｰPASS./
    VAR  ?HAISINF    ,STRING*8,VALUE-'        '  /.ﾊｲｼﾝﾌｧｲﾙ名./
    VAR  ?HAISINFL   ,NAME                       /.ﾊｲｼﾝﾌｧｲﾙ名./
    VAR  ?HAISINLB   ,NAME!MOD                   /.ﾊｲｼﾝﾗｲﾌﾞﾗﾘ名./
    VAR  ?HAISINNM   ,STRING*16,VALUE-'                '
                                     /.ﾊｲｼﾝﾌｧｲﾙ名./
    VAR  ?ERRCD      ,STRING*4,VALUE-'    '      /.ｴﾗｰｺｰﾄﾞ./
    VAR  ?STRTIME    ,STRING*4,VALUE-'    '      /.ｴﾗｰｺｰﾄﾞ./
    VAR  ?SYSTIMES   ,STRING*6,VALUE-'      '    /.SYSTEMｼﾞｶﾝ./
    VAR  ?TANMATUH   ,NAME                         /.端末名./
/.*2012/11/20 ADD ST *./
    VAR  ?JYUKBN     ,STRING*1,VALUE-'1'         /.受信手順./
/.*2012/11/20 ADD ED *./

/.  ACTMSGQ MAXRMSG-50  ./

/.##プログラム開始メッセージ##./

    ?MSG :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSG,TO-XCTL.@ORGPROF

    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
    SNDMSG MSG-'＊　受信振分処理開始　＊',TO-XCTL
    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL

    ?MSG := '?TANMATU = '&& ?TANMATU
    SNDMSG    ?MSG,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKJLIB/ONLBLIB


/.パラメタファイル名を生成./
    ?PARAFILE := %NAME(?PARAF)         /.名前型に変換./
    ?LIBN := %NAME(?LIB)               /.名前型に変換./
    ?PARALIB := %NCAT(?PARAFILE,?LIBN) /.名前型の結合./
    ?PARALIBN := %STRING(?PARALIB)     /.文字列型に変換./
    ?MSG := '## PARAFILE = ' && ?PARALIBN  &&'##'
    SNDMSG ?MSG,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES /.画面に出力./

/.パラメタファイル生成./
SNJ0530B:

    ?STEP :=   'SNJ0530B'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSG,TO-XCTL

    OVRF FILE-JSMEDIL1,TOFILE-JSMEDIL1.TOKJLIB
    OVRF FILE-JSMKAIL1,TOFILE-JSMKAIL1.TOKJLIB
    OVRF FILE-PARAFIL,TOFILE-?PARALIB
    CALL PGM-SNJ0530B.TOKELIBO,
         PARA-(?JDATE,?JIKAN,?DATAKBN,?JYUHKBN,?TORICD,?KAIKBN,
               ?KAINO,?KEKA,?TRGERF,?ANSERF,?JOBNM,?HAISINF)
    IF  @PGMEC ^= 0 THEN
        ?ERRCD :=  'K001'
        GOTO  ABEND
    END
    IF  ?KEKA  = '9' THEN
        ?ERRCD :=  'K002'
        GOTO  ABEND
    END

/.##トリガーファイル名生成##./
    ?TRGERFIL := %NAME(?TRGERF)                      /.名前型に変換./
    ?LIBN := %NAME(?LIB)                             /.名前型に変換./
    ?TRGERLIB := %NCAT(?TRGERFIL,?LIBN)              /.名前型の結合./
    ?TRGERNM := %STRING(?TRGERLIB)                 /.文字列型に変換./
    ?MSG := '## TRGERFIL = ' && ?TRGERNM  &&'##'
    SNDMSG ?MSG,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES /.画面に出力./
/.##アンサーファイル名生成##./
    ?ANSERFIL := %NAME(?ANSERF)                      /.名前型に変換./
    ?LIBN := %NAME(?LIB)                             /.名前型に変換./
    ?ANSERLIB := %NCAT(?ANSERFIL,?LIBN)              /.名前型の結合./
    ?ANSERNM := %STRING(?ANSERLIB)                 /.文字列型に変換./
    ?MSG := '## ANSERFIL = ' && ?ANSERNM  &&'##'
    SNDMSG ?MSG,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES /.画面に出力./


/.トリガーファイル生成 ./
SNJ0550B:

    ?STEP :=   'SNJ0550B'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSG,TO-XCTL

    OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF FILE-PARAFIL,TOFILE-?PARALIB
    OVRF FILE-TRIGGER,TOFILE-?TRGERLIB
/.## 旧バージョン  2012/11/20
    CALL PGM-SNJ0550B.TOKELIBO,PARA-(?TUSINM,?KEKA)
##./
    CALL PGM-SNJ0550B.TOKELIBO,PARA-(?TUSINM,?KEKA,?JYUKBN)

    ?MSG :=  '#受信手順=' && ?JYUKBN && '#'
    SNDMSG    ?MSG,TO-XCTL
    IF  @PGMEC ^= 0 THEN
        ?ERRCD :=  'K003'
        GOTO  ABEND
    END
    IF  ?KEKA  = '9' THEN
        ?ERRCD :=  'K004'
        GOTO  ABEND
    END

/.待受けＪＯＢ振分ＰＧ ./
SNJ0620B:

    ?STEP :=   'SNJ0620B'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSG,TO-XCTL

/.## 旧バージョン 2012/11/20
    CALL PGM-SNJ0620B.TOKELIBO,
         PARA-(?DATAKBN,?SEIGYKBN,?KEKA)
##./
    CALL PGM-SNJ0625B.TOKELIBO,
         PARA-(?DATAKBN,?JYUKBN,?SEIGYKBN,?KEKA)

    IF  @PGMEC ^= 0 THEN
        ?ERRCD :=  'K006'
        GOTO  ABEND
    END
    IF  ?KEKA  = '9' THEN
        ?ERRCD :=  'K007'
        GOTO  ABEND
    END

/.待受けＪＯＢ振分ＰＧ ./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSG,TO-XCTL

    CLRFILE FILE-?ANSERLIB
    IF  @PGMEC ^= 0 THEN
        ?ERRCD :=  'K544'
        GOTO  ABEND
    END

/.処理起動制御./
SHORKIDO:
    IF   ?WAITC < 2  THEN
         ?WAITC := ?WAITC + 1
         ?STEP  :=  'FTP WAITE '
         ?MSG   :=  '***   '  && ?STEP   &&   '        ***'
         SNDMSG    ?MSG,TO-XCTL
         /.### FTP ﾏﾁｱﾜｾ 5ﾋﾞｮｳ ###./
         CALL TWAIT05.XUCL
         GOTO   SHORKIDO
    END

    ?STEP :=   'SHORKIDO'
    ?MSG :=  '***   '  && ?STEP   &&   '        ***'
    ?MSG :=  '***   処理起動判定        ***'
    SNDMSG    ?MSG,TO-XCTL

    ?JOBNMN := %NAME(?JOBNM)     /.名前型に変換./
    ?TANMATUH := %NAME(?TANMATU)     /.名前型に変換./
    ?USERIDN  := %NAME(?USERID)  /.名前型に変換./
    ?PASSN    := %NAME(?PASS)    /.名前型に変換./

   /.##ｼﾞｭｼﾝｶｲｼ TIME ｼｭﾄｸ##./
    ?SYSTIMES := @STIME
    ?STRTIME  := %SBSTR(?SYSTIMES,1,4)
   /.##ｼﾞｭｼﾝｶﾝｹｲﾌｧｲﾙ ｵｷｶｴ##./
    /.発注系処理を制御./
    ?MSG := '?TANMATU1= '&& ?TANMATU
    SNDMSG    ?MSG,TO-XCTL
         SNDMSG MSG-'## A0 ##',TO-XCTL
    IF   ?SEIGYKBN = '1'  THEN
         SNDMSG MSG-'## A1 ##',TO-XCTL
             CNTFTPC RMTNAME-?TANMATUH,USER-?USERID,PASSWORD-?PASS
/.###    IF  ?TANMATU = 'EDIPC1  '  THEN
             CNTFTPC RMTADT-EDIPC1,USER-?USERID,PASSWORD-?PASS
         END
         IF  ?TANMATU = 'EDIPC2  '  THEN
             CNTFTPC RMTADT-EDIPC2,USER-?USERID,PASSWORD-?PASS
         END
         IF  ?TANMATU = 'EDIPC3  '  THEN
             CNTFTPC RMTADT-EDIPC3,USER-?USERID,PASSWORD-?PASS
         END
###./    /.##社内テスト用（社外の場合は、ホスト名）##
         CNTFTPC RMTADR-'192.168.103.93',
                         USER-?USERID,PASSWORD-?PASS./
         IF  @PGMEC ^= 0 THEN
             ?ERRCD :=  'K008'
             GOTO  ABEND
         END
         SNDMSG MSG-'## A2 ##',TO-XCTL

         SNDMSG MSG-'## A3 ##',TO-XCTL
         SNDFTPC ENT-?TRGERLIB,TOFILE-?TRGERF,DTYPE-@BINARY
         IF  @PGMEC ^= 0 THEN
             DCNTFTPC
             ?ERRCD :=  'K009'
             GOTO  ABEND
         END

         SNDMSG MSG-'## A4 ##',TO-XCTL
         DCNTFTPC
         IF  @PGMEC ^= 0 THEN
             DCNTFTPC
             ?ERRCD :=  'K010'
             GOTO  ABEND
         END

         SNDMSG MSG-'## A5 ##',TO-XCTL
/.       SBMJOB JOB-?JOBNMN,JOBD-NEWONL.XUCL,JOBK-@B,
./       SBMJOB JOB-?JOBNMN,JOBD-NEWONL1.XUCL,JOBK-@B,
         PGM-CHATYU.TOKCLIBO,LOG-@YES!1024,
         PTY-5,PGMEL-@I/@L/@S/@T,LIBL-TOKELIB/TOKFLIB/TOKJLIB/ONLBLIB,
         PARA-(?PARAF)
         IF  @PGMEC ^= 0 THEN
             ?ERRCD :=  'K011'
             GOTO  ABEND
         END

         GOTO  RTN

    END

         SNDMSG MSG-'## B0 ##',TO-XCTL
    /.その他系ジョブを制御./
    IF   ?SEIGYKBN = '2'  THEN
         SNDMSG MSG-'## B1 ##',TO-XCTL
         CNTFTPC RMTNAME-?TANMATUH,USER-?USERID,PASSWORD-?PASS
         IF  @PGMEC ^= 0 THEN
             ?ERRCD :=  'K012'
             GOTO  ABEND
         END

         SNDMSG MSG-'## B2 ##',TO-XCTL
         SNDFTPC ENT-?TRGERLIB,TOFILE-?TRGERF,DTYPE-@BINARY
         IF  @PGMEC ^= 0 THEN
             DCNTFTPC
             ?ERRCD :=  'K013'
             GOTO  ABEND
         END

         SNDMSG MSG-'## B3 ##',TO-XCTL
         DCNTFTPC
         IF  @PGMEC ^= 0 THEN
             DCNTFTPC
             ?ERRCD :=  'K014'
             GOTO  ABEND
         END

         SNDMSG MSG-'## B4 ##',TO-XCTL
/.       SBMJOB JOB-?JOBNMN,JOBD-NEWONL.XUCL,JOBK-@B,
         PGM-CSONOTA.TOKCLIBO,LOG-@YES!1024,
         PTY-5,PGMEL-@I/@L/@S/@T,LIBL-TOKELIB/TOKFLIB/TOKJLIB/ONLBLIB,
         PARA-(?PARAF)
./       SBMJOB JOB-?JOBNMN,JOBD-NEWONL1.XUCL,JOBK-@B,
         PGM-CSONOTA.TOKCLIBO,LOG-@YES!1024,
         PTY-5,PGMEL-@I/@L/@S/@T,LIBL-TOKELIB/TOKFLIB/TOKJLIB/ONLBLIB,
         PARA-(?PARAF)
         IF  @PGMEC ^= 0 THEN
             ?ERRCD :=  'K015'
             GOTO  ABEND
         END

         GOTO  RTN

    END

         SNDMSG MSG-'## C0 ##',TO-XCTL
    /.配信系ジョブを制御./
    IF   ?SEIGYKBN = '3'  THEN
         SNDMSG MSG-'## C1 ##',TO-XCTL
         CNTFTPC RMTNAME-?TANMATUH,USER-?USERID,PASSWORD-?PASS
         /.##社内テスト用（社外の場合は、ホスト名）##
         CNTFTPC RMTADR-'192.168.103.93',USER-?USERID,
                 PASSWORD-?PASS
./       IF  @PGMEC ^= 0 THEN
             ?ERRCD :=  'K012'
             GOTO  ABEND
         END

         /.配信ファイル名を生成 ./
         ?HAISINFL := %NAME(?HAISINF)        /.名前型に変換./
         ?LIBN := %NAME(?LIBONL)             /.名前型に変換./
         ?HAISINLB := %NCAT(?HAISINFL,?LIBN) /.名前型の結合./
         ?HAISINNM := %STRING(?HAISINLB)     /.文字列型に変換./
         ?MSG := '## HAISINFILE = ' && ?HAISINNM  &&'##'
         SNDMSG ?MSG,TO-XCTL.@ORGPROF        /.画面に出力./

         SNDFTPC ENT-?HAISINLB,TOFILE-?HAISINF,DTYPE-@BINARY
         IF  @PGMEC ^= 0 THEN
             DCNTFTPC
             ?ERRCD :=  'K013'
             GOTO  ABEND
         END

         SNDFTPC ENT-?TRGERLIB,TOFILE-?TRGERF,DTYPE-@BINARY
         IF  @PGMEC ^= 0 THEN
             DCNTFTPC
             ?ERRCD :=  'K013'
             GOTO  ABEND
         END

         DCNTFTPC
         IF  @PGMEC ^= 0 THEN
             DCNTFTPC
             ?ERRCD :=  'K014'
             GOTO  ABEND
         END

/.       SBMJOB JOB-?JOBNMN,JOBD-NEWONL.XUCL,JOBK-@B,
         PGM-CHAISIN.TOKCLIBO,LOG-@YES!1024,
         PTY-5,PGMEL-@I/@L/@S/@T,LIBL-TOKELIB/TOKFLIB/TOKJLIB/ONLBLIB,
         PARA-(?PARAF)
./       SBMJOB JOB-?JOBNMN,JOBD-NEWONL1.XUCL,JOBK-@B,
         PGM-CHAISIN.TOKCLIBO,LOG-@YES!1024,
         PTY-5,PGMEL-@I/@L/@S/@T,LIBL-TOKELIB/TOKFLIB/TOKJLIB/ONLBLIB,
         PARA-(?PARAF)
         IF  @PGMEC ^= 0 THEN
             ?ERRCD :=  'K015'
             GOTO  ABEND
         END

         GOTO  RTN

    END
/.##流通ＢＭＳ　受信系起動##./
         SNDMSG MSG-'## D0 ##',TO-XCTL
    IF   ?SEIGYKBN = '4'  THEN
         SNDMSG MSG-'## D1 ##',TO-XCTL
             CNTFTPC RMTNAME-?TANMATUH,USER-?USERID,PASSWORD-?PASS
/.####   IF  ?TANMATU = 'EDIPC1  '  THEN
             CNTFTPC RMTADT-EDIPC1,USER-?USERID,PASSWORD-?PASS
         END
         IF  ?TANMATU = 'EDIPC2  '  THEN
             CNTFTPC RMTADT-EDIPC2,USER-?USERID,PASSWORD-?PASS
         END
         IF  ?TANMATU = 'EDIPC3  '  THEN
             CNTFTPC RMTADT-EDIPC3,USER-?USERID,PASSWORD-?PASS
         END
###./    /.##社内テスト用（社外の場合は、ホスト名）##
         CNTFTPC RMTADR-'192.168.103.93',
                         USER-?USERID,PASSWORD-?PASS  ./
         IF  @PGMEC ^= 0 THEN
             ?ERRCD :=  'K008'
             GOTO  ABEND
         END
         SNDMSG MSG-'## D2 ##',TO-XCTL

         SNDMSG MSG-'## D3 ##',TO-XCTL
         SNDFTPC ENT-?TRGERLIB,TOFILE-?TRGERF,DTYPE-@BINARY
         IF  @PGMEC ^= 0 THEN
             DCNTFTPC
             ?ERRCD :=  'K009'
             GOTO  ABEND
         END

         SNDMSG MSG-'## D4 ##',TO-XCTL
         DCNTFTPC
         IF  @PGMEC ^= 0 THEN
             DCNTFTPC
             ?ERRCD :=  'K010'
             GOTO  ABEND
         END

         SNDMSG MSG-'## D5 ##',TO-XCTL
/.       SBMJOB JOB-?JOBNMN,JOBD-NEWONL.XUCL,JOBK-@B,
./       SBMJOB JOB-?JOBNMN,JOBD-NEWONL1.XUCL,JOBK-@B,
         PGM-CBMSJYU.TOKCLIBO,LOG-@YES!1024,
         PTY-5,PGMEL-@I/@L/@S/@T,LIBL-TOKELIB/TOKFLIB/TOKJLIB/ONLBLIB,
         PARA-(?PARAF)
         IF  @PGMEC ^= 0 THEN
             ?ERRCD :=  'K011'
             GOTO  ABEND
         END

         GOTO  RTN

    END
         SNDMSG MSG-'## E0 ##',TO-XCTL
    /.配信系ジョブを制御./
    IF   ?SEIGYKBN = '5'  THEN
         SNDMSG MSG-'## E1 ##',TO-XCTL
         CNTFTPC RMTNAME-?TANMATUH,USER-?USERID,PASSWORD-?PASS
         /.##社内テスト用（社外の場合は、ホスト名）##
         CNTFTPC RMTADR-'192.168.103.93',USER-?USERID,
                 PASSWORD-?PASS./
         IF  @PGMEC ^= 0 THEN
             ?ERRCD :=  'K012'
             GOTO  ABEND
         END

         /.配信ファイル名を生成 ./
         ?HAISINFL := %NAME(?HAISINF)        /.名前型に変換./
         ?LIBN := %NAME(?LIBONL)             /.名前型に変換./
         ?HAISINLB := %NCAT(?HAISINFL,?LIBN) /.名前型の結合./
         ?HAISINNM := %STRING(?HAISINLB)     /.文字列型に変換./
         ?MSG := '## HAISINFILE = ' && ?HAISINNM  &&'##'
         SNDMSG ?MSG,TO-XCTL.@ORGPROF        /.画面に出力./

         SNDFTPC ENT-?HAISINLB,TOFILE-?HAISINF,DTYPE-@BINARY
         IF  @PGMEC ^= 0 THEN
             DCNTFTPC
             ?ERRCD :=  'K013'
             GOTO  ABEND
         END

         SNDFTPC ENT-?TRGERLIB,TOFILE-?TRGERF,DTYPE-@BINARY
         IF  @PGMEC ^= 0 THEN
             DCNTFTPC
             ?ERRCD :=  'K013'
             GOTO  ABEND
         END

         DCNTFTPC
         IF  @PGMEC ^= 0 THEN
             DCNTFTPC
             ?ERRCD :=  'K014'
             GOTO  ABEND
         END

/.       SBMJOB JOB-?JOBNMN,JOBD-NEWONL.XUCL,JOBK-@B,
         PGM-CHAISIN.TOKCLIBO,LOG-@YES!1024,
         PTY-5,PGMEL-@I/@L/@S/@T,LIBL-TOKELIB/TOKFLIB/TOKJLIB/ONLBLIB,
         PARA-(?PARAF)
./       SBMJOB JOB-?JOBNMN,JOBD-NEWONL1.XUCL,JOBK-@B,
         PGM-CBMSHAI.TOKCLIBO,LOG-@YES!1024,
         PTY-5,PGMEL-@I/@L/@S/@T,LIBL-TOKELIB/TOKFLIB/TOKJLIB/ONLBLIB,
         PARA-(?PARAF)
         IF  @PGMEC ^= 0 THEN
             ?ERRCD :=  'K015'
             GOTO  ABEND
         END

         GOTO  RTN

    END

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:
    ?MSG := '## 振分処理 正常終了 ##'
    SNDMSG ?MSG,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSG := '## 取引先 = ' && ?TORICD && ' ##'
    SNDMSG ?MSG,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
/.##当日ﾌｧｲﾙ更新##./
    ?ERRCD := 'K521'
    OVRF FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    CALL SNJ0700B.TOKELIBO,PARA-(?JDATE,?JIKAN,?TORICD,
                                 ?STRTIME,?ERRCD)

    ?MSG :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSG,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    RETURN    PGMEC-@PGMEC

/.**********アべンド処理は今後対応 2010/08/25 *********./
ABEND:
    ?MSG := '## 振分処理 異常終了 ##'
    SNDMSG ?MSG,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSG := '## 取引先 = ' && ?TORICD && ' ##'
    SNDMSG ?MSG,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

   /.##ｴﾗｰ結果更新##./
    DEFLIBL TOKELIB/TOKFLIB
    CALL PGM-SCV0090B.TOKELIB,PARA-(?JDATE,?JIKAN,?TORICD,?ERRCD)
   /.##ｴﾗｰﾘｽﾄ発行##./
    SBMJOB JOB-TKEKALSE,JOBD-CVCS.XUCL,JOBK-@B,
           PGM-SCV0160B.TOKELIB,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
           @S/@T,LIBL-TOKFLIB/TOKELIB,PARA-(?JDATE,?JIKAN,
           ?TORICD,?ERRCD)

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
    SNDMSG ?PGMEM,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    DSPLOG OUTPUT-@LIST
    SNDMSG MSG-'＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃',
           TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    SNDMSG MSG-'＃＜受配信処理において異常終了が発生＞＃',
           TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    SNDMSG MSG-'＃　エラー情報を回収し、至急、　　　　＃',
           TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    SNDMSG MSG-'＃　ナブ・アシストまで連絡して下さい。＃',
           TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    SNDMSG MSG-'＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃',
           TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    RETURN    PGMEC-@PGMEC


```
