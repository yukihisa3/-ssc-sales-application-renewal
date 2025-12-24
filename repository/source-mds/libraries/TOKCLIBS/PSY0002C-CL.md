# PSY0002C

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PSY0002C.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷管理                             *  ./
/. *   JOB-ID      :    PSSY0001                             *  ./
/. *   JOB-NAME    :    出荷明細表                           *  ./
/. ***********************************************************  ./
    PGM

    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSSY0001'
    VAR       ?STEP     ,STRING*8
    VAR       ?P1       ,STRING*1,VALUE-'0'        /.出力順    ./
    VAR       ?P2       ,STRING*8,VALUE-'00000000' /.受信日付  ./
    VAR       ?P3       ,STRING*4,VALUE-'0000'     /.受信時間  ./
    VAR       ?P4       ,STRING*8,VALUE-'00000000' /.受信取引先./
    VAR       ?P5       ,STRING*2,VALUE-'00'       /.倉庫      ./
    VAR       ?P6       ,STRING*8,VALUE-'00000000' /.納品日    ./
    VAR       ?P7       ,STRING*5,VALUE-'00000'    /.開始店舗  ./
    VAR       ?P8       ,STRING*5,VALUE-'00000'    /.終了店舗  ./
    VAR       ?P9       ,STRING*4,VALUE-'0000'     /.開始部門  ./
    VAR       ?P10      ,STRING*4,VALUE-'0000'     /.終了部門  ./
    VAR       ?P11      ,STRING*6,VALUE-'000000'   /.開始_番  ./
    VAR       ?P12      ,STRING*6,VALUE-'000000'   /.終了_番  ./
    VAR       ?P13      ,STRING*1,VALUE-'0'        /.出力ﾊﾟﾀｰﾝ ./
    VAR       ?P14      ,STRING*2,VALUE-'00'       /.倉庫(代表)./

    DEFLIBL TOKELIB/TOKFLIB
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

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
    CALL      PGM-TKY1601B.TOKELIB,PARA-(?WS,?P5,?P14)
    IF        @PGMEC    ^=   0   THEN
              GOTO ABEND
    END

/.##出荷明細データ抽出##./
SSY0002B:

    ?STEP :=   'SSY0002B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JHTDENF,TOFILE-JHTDEN6A.TOKFLIB
    OVRF      FILE-SHWSYUKF,TOFILE-SHWSYUKF.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    CALL      PGM-SSY0002C.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##ＳＯＲＴ##./
SORT1:
/.ＳＯＲＴ順：ﾊﾞｯﾁ番号、倉庫ｺｰﾄﾞ、納品日、分類、店舗、_番./
    ?STEP :=  'SORT1   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT      INFILE-SHWSYUKF.TOKFLIB,INRL-200,INBF-5,
              OUTFILE-SHWSYUKF.TOKFLIB,OUTBF-5,
              KEY-1!20!CA,KEY1-21!2!CA,KEY2-126!8!CA,KEY3-28!4!CA,
              KEY4-23!5!CA,KEY5-32!6!CA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

SSY0025L:

    ?STEP :=   'SSY0025L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

    OVRF      FILE-SHWSYUKF,TOFILE-SHWSYUKF.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    CALL      PGM-SSY0040C.TOKELIBO,PARA-('1')
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN: /.##ﾌﾟﾛｸﾞﾗﾑ正常時、終了ﾒｯｾｰｼﾞ##./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:/.##ﾌﾟﾛｸﾞﾗﾑ異常終了時、終了ﾒｯｾｰｼﾞ##./

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

```
