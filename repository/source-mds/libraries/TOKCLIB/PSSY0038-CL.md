# PSSY0038

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PSSY0038.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷管理                             *  ./
/. *   JOB-ID      :    PSSY0038                             *  ./
/. *   JOB-NAME    :    発注集計表                           *  ./
/. ***********************************************************  ./
    PGM   (PI-?PAR)
/.  PARA ?PAR1  ,STRING*8,IN,VALUE-'00000000'
    PARA ?PAR2  ,STRING*4,IN,VALUE-'0000'
    PARA ?PAR3  ,STRING*8,IN,VALUE-'00000000'    ./
    PARA ?PAR   ,STRING*22,IN,VALUE-'0000000000000000000000'

    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSSY0038'
    VAR       ?STEP     ,STRING*8
    VAR       ?P1       ,STRING*8,VALUE-'00000000' /.受信日付    ./
    VAR       ?P2       ,STRING*4,VALUE-'0000'     /.受信時間    ./
    VAR       ?P3       ,STRING*8,VALUE-'00000000' /.受信取引先  ./
    VAR       ?P4       ,STRING*2,VALUE-'00'       /.出力対象倉庫./
    VAR       ?P5       ,STRING*1,VALUE-'0'        /.発注／訂正  ./
    VAR       ?P6       ,STRING*1,VALUE-'0'        /.出力順      ./
    VAR       ?P7       ,STRING*2,VALUE-'00'       /.代表倉庫    ./
    VAR       ?PA1      ,STRING*8
    VAR       ?PA2      ,STRING*4
    VAR       ?PA3      ,STRING*8
    VAR       ?PA4      ,STRING*2
    VAR       ?PI       ,STRING*22,VALUE-'0000000000000000000000'
    VAR       ?OPR1     ,STRING*50                 /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                 /.      2    ./
    VAR       ?OPR3     ,STRING*50                 /.      3    ./
    VAR       ?OPR4     ,STRING*50                 /.      4    ./
    VAR       ?OPR5     ,STRING*50                 /.      5    ./
    VAR ?SOKCD    ,STRING*2,VALUE-'  '          /.自倉庫ｺｰﾄﾞ./
    VAR ?DSOKCD   ,STRING*2,VALUE-'  '          /.代表倉庫ｺｰﾄﾞ./
    VAR ?KEYA     ,STRING*1,VALUE-'1'           /.KEY1./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?FILNM    ,STRING*8,VALUE-'        '
    VAR ?FILNMH   ,STRING*6,VALUE-'      '
    VAR ?FILNMO   ,STRING*6,VALUE-'SHWHAC'
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKFLIB '
    VAR ?FILID    ,NAME
    VAR ?LIBID    ,NAME
    VAR ?SHWHAC   ,NAME!MOD
    VAR ?SHWHACN  ,STRING*17
    VAR ?KBN      ,STRING*2,VALUE-'  '

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?PA1  :=  %SBSTR(?PAR,1,8)
    ?PA2  :=  %SBSTR(?PAR,9,4)
    ?PA3  :=  %SBSTR(?PAR,13,8)
    ?PA4  :=  %SBSTR(?PAR,21,2)

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    IF        @PGMEC    ^=   0   THEN
              GOTO ABEND
    END

    ?MSGX    :=  '## 実行倉庫CD = ' && ?SOKCD
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX    :=  '## 本社区分   = ' && ?DSOKCD
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    IF   ?PA4    =   '00'  THEN
        ?FILNM    :=   'SHWHACF'
    ELSE
        ?FILNM    :=   ?FILNMO && ?SOKCD
    END
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?SHWHAC   :=    %NCAT(?FILID,?LIBID)
    ?SHWHACN  :=    %STRING(?SHWHAC)
    ?MSGX     :=    '## 発注集計F  = ' && ?SHWHACN  && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

FILECHK:
    ASSIGN FILE-?SHWHAC!@XCL
    IF     @PGMEC  ^=    0    THEN    GOTO   ERR  END

SKY1602B:

    ?STEP :=   'SKY1602B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P4,?P7)
    IF        @PGMEC    ^=   0   THEN
              GOTO ABEND
    END
/.##発注集計表発行指示入力##./
PSSY0038:

    ?STEP :=   'PSSY0038'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL   TOKELIB/TOKELIBO/TOKJLIB/TOKFLIB/TOKKLIB

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-SSY0005I.TOKELIB,PARA-(?P1,?P2,?P3,
                                         ?P4,?P5,?P6,?P7,
                                         ?PA1,?PA2,?PA3)
    IF        @PGMEC    ^=   0    THEN
        IF    @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO   RTN4
        ELSE
              GOTO   ABEND
        END
    END

/.##出荷明細データ抽出##./
SSY0006B:

    ?STEP :=   'SSY0006B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.  DEFLIBL   TOKELIB
./
    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-SHWHACF,TOFILE-?SHWHAC
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SSY0028B.TOKELIB,PARA-(?P1,?P2,?P3,?P4,?P5,?P6)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
                                  /.##相手商品ｺｰﾄﾞ順##./
    IF        ?P6        =  '2'   THEN
              GOTO AITESCD
    END
                                  /.##_番順##./
    IF        ?P6        =  '3'   THEN
              GOTO TANABAN
    END
                                  /.##分類集計##./
    IF        ?P6        =  '4'   THEN
              GOTO BUNRUI
    END

/.##ＳＯＲＴ　サカタ商品コード順##./
SORT1:
/.##ＳＯＲＴ順：部門、倉庫、納品日、商品ｺｰﾄﾞ、店舗順##./
    ?STEP :=  'SORT1   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SORT       INFILE-?SHWHAC,INRL-200,INBF-5,
              OUTFILE-?SHWHAC,OUTBF-5,
              KEY-28!4!CA,KEY1-183!10!CA,KEY2-40!8!CA,KEY3-61!16!CA,
              KEY4-21!5!CA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    ELSE
              GOTO SSY0029L
    END

RTN1:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    RELEASE FILE-?SHWHAC!@XCL
    RETURN    PGMEC-@PGMEC


/.##相手商品ｺｰﾄﾞ順#./
AITESCD:
/.##ＳＯＲＴ　相手商品コード##./
SORT2:
/.##ＳＯＲＴ順：部門、倉庫、納品日、相手商品ｺｰﾄﾞ、店舗順##./
    ?STEP :=  'SORT2   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SORT      INFILE-?SHWHAC,INRL-200,INBF-5,
              OUTFILE-?SHWHAC,OUTBF-5,
              KEY-28!4!CA,KEY1-183!10!CA,KEY2-40!8!CA,KEY3-48!13!CA,
              KEY4-21!5!CA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END


/.##発注集計表作表##./
SSY0029L:

    ?STEP :=   'SSY0029L'
      ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

    OVRF      FILE-SHWHACF,TOFILE-?SHWHAC

/.  OVRPRTF   FILE-PRTF,TOFILE-XU04LP.XUCL,MEDLIB-TOKELIB  ./
    CALL      PGM-SSY0038L.TOKELIB,PARA-(?P5,?P6)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
/.##相手商品順の時、分類系出力（但し、本社の場合）##./
    IF        ?P7        =   '01'  THEN
              IF   ?P6   =   '2'   THEN
                   GOTO  BUNRUI
              END
    END

RTN2:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    RELEASE FILE-?SHWHAC!@XCL
    RETURN    PGMEC-@PGMEC

/.##_番＋商品コード順##./
TANABAN:
/.##ＳＯＲＴ：部門、倉庫、納品日、_番、相手商品、店舗##./
SORT3:

    ?STEP :=  'SORT3   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SORT      INFILE-?SHWHAC,INRL-200,INBF-5,
              OUTFILE-?SHWHAC,OUTBF-5,
              KEY-28!4!CA,KEY1-183!10!CA,KEY2-40!8!CA,KEY3-77!6!CA,
              KEY4-61!16!CA,KEY5-21!5!CA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END


/.##発注集計表作表（_番＋商品コード順）##./
SSY0030L:

    ?STEP :=   'SSY0030L'
      ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

    OVRF      FILE-SHWHACF,TOFILE-?SHWHAC

/.  OVRPRTF   FILE-PRTF,TOFILE-XU04LP.XUCL,MEDLIB-TOKELIB  ./
    CALL      PGM-SSY0039L.TOKELIB,PARA-(?P5)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN3:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    RELEASE FILE-?SHWHAC!@XCL
    RETURN    PGMEC-@PGMEC

/.##ｻｶﾀ分類集計##./
BUNRUI:
/.##ＳＯＲＴ　サカタ商品コード##./
SORT4:
/.##ＳＯＲＴ順：サカタ商品コード順##./
    ?STEP :=  'SORT4   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SORT      INFILE-?SHWHAC,INRL-200,INBF-5,
              OUTFILE-?SHWHAC,OUTBF-5,
              KEY-61!8!CA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END


/.##発注集計表データ集計（サカタ分類集計）##./
SSY0007B:

    ?STEP :=   'SSY0007B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL   TOKELIB/TOKFLIB

    OVRF      FILE-SHWHACF,TOFILE-?SHWHAC
    OVRF      FILE-SHWHACBF,TOFILE-SHWHACBF.TOKFLIB
    CALL      PGM-SSY0007B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##発注集計表作表（サカタ分類集計）##./
SSY0009L:

    ?STEP :=   'SSY0009L'
      ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

    OVRF      FILE-SHWHACBF,TOFILE-SHWHACBF.TOKFLIB

    CALL      PGM-SSY0009L.TOKELIB,PARA-(?P5)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN4:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    RELEASE FILE-?SHWHAC!@XCL
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
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END

    RETURN    PGMEC-@PGMEC

ERR:  /.資源の解放./
    IF   ?PA4    =   '00'  THEN
    ?OPR1  :=  '　　＃＃＃＃＃＃＃　資源使用中　＃＃＃＃＃＃＃　　'
    ?OPR2  :=  '　　現在、発注集計表ワークＦを他倉庫にて使用中　　'
    ?OPR3  :=  '　　です。少し時間をおいて再度、実行して下さい。　'
    ?OPR4  :=  '　　　　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR5  :=  '　　ＥＮＴＥＲ＝再実行，ＰＦ９＝プログラム終了　　'
      CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
      GOTO   FILECHK
    ELSE
        OVRF  FILE-HACHLOC1,TOFILE-HACHLOC1.TOKKLIB
        CALL  SNJ0910B.TOKELIBO,PARA-('ON',?WS,?KBN)
    END
    RETURN    PGMEC-@PGMEC

```
