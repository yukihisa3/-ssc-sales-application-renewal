# PTETEST

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PTETEST.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷管理（手書伝票データ用）         *  ./
/. *   JOB-ID      :    PTE00100                             *  ./
/. *   JOB-NAME    :    手書伝票用発注集計表                 *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?FILNM    ,STRING*8,VALUE-'        '
    VAR ?FILNMH   ,STRING*5,VALUE-'JHTTE'
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKFLIB '
    VAR ?KEYNO    ,STRING*1,VALUE-'1'
    VAR ?FILID    ,NAME
    VAR ?LIBID    ,NAME
    VAR ?JHTDENF  ,NAME!MOD
    VAR ?JHTDENFN ,STRING*17
    VAR ?PGMEC    ,INTEGER
    VAR ?PGMECX   ,STRING*11
    VAR ?PGMEM    ,STRING*99
    VAR ?MSG      ,STRING*99(6)
    VAR ?MSGX     ,STRING*99
    VAR ?PGMID    ,STRING*8,VALUE-'PTE00100'
    VAR ?STEP     ,STRING*8
    VAR ?WKSTN    ,NAME
    VAR ?WS       ,STRING*8,VALUE-'        '
    VAR ?SOKCD    ,STRING*2,VALUE-'  '
    VAR ?TORICD   ,STRING*8,VALUE-'00000000'  /.取引先./
    VAR ?SDEN     ,STRING*9,VALUE-'000000000' /.伝票開始./
    VAR ?EDEN     ,STRING*9,VALUE-'000000000' /.伝票終了./
    VAR ?KBN      ,STRING*1,VALUE-'0'         /.出力区分./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

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
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD)
    IF        @PGMEC ^= 0 THEN
              GOTO ABEND
    END

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:

    ?MSGX := '## 実行倉庫ｺｰﾄﾞ = ' && ?SOKCD && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?FILNM    :=    ?FILNMH && ?SOKCD && ?KEYNO
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?JHTDENF  :=    %NCAT(?FILID,?LIBID)
    ?JHTDENFN :=    %STRING(?JHTDENF)
    ?MSGX     :=    '## 倉庫ﾌｧｲﾙ名 = ' && ?JHTDENFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##発注集計表発行指示入力##./
STE0010I:

    ?STEP :=   'STE0010I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL   TOKELIB/TOKFLIB

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-SHTDENL1,TOFILE-SHTDENL1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-STE0010I.TOKELIB,PARA-(?TORICD,?SDEN,?EDEN,?KBN)
    IF        @PGMEC    ^=   0    THEN
        IF    @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO   RTN
        ELSE
              GOTO   ABEND
        END
    END
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

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

```
