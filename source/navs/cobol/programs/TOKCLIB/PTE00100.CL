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
    VAR ?FILNMW   ,STRING*6,VALUE-'TEGHAC'
    VAR ?LIBNM    ,STRING*8,VALUE-'TOKFLIB '
    VAR ?KEYNO    ,STRING*1,VALUE-'1'
    VAR ?FILID    ,NAME
    VAR ?LIBID    ,NAME
    VAR ?JHTDENF  ,NAME!MOD
    VAR ?JHTDENFN ,STRING*17
    VAR ?TEGHACF  ,NAME!MOD
    VAR ?TEGHACFN ,STRING*17
    VAR ?PGMEC    ,INTEGER
    VAR ?PGMECX   ,STRING*11
    VAR ?PGMEM    ,STRING*99
    VAR ?MSG      ,STRING*99(6)
    VAR ?MSGX     ,STRING*99
    VAR ?PGMID    ,STRING*8,VALUE-'PTE00100'
    VAR ?STEP     ,STRING*8
    VAR ?WKSTN    ,NAME
    VAR ?WS       ,STRING*8,VALUE-'        '
    VAR ?SOKCD    ,STRING*2,VALUE-'00'
    VAR ?DSOKCD   ,STRING*2,VALUE-'00'
    VAR ?TORICD   ,STRING*8,VALUE-'00000000'  /.取引先./
    VAR ?SDEN     ,STRING*9,VALUE-'000000000' /.伝票開始./
    VAR ?EDEN     ,STRING*9,VALUE-'000000000' /.伝票終了./
    VAR ?KBN      ,STRING*1,VALUE-'0'         /.出力区分./
    VAR ?TKBN     ,STRING*1,VALUE-'1'         /.出力区分./

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
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
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
    ?FILNM    :=    ?FILNMW && ?SOKCD
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?TEGHACF  :=    %NCAT(?FILID,?LIBID)
    ?TEGHACFN :=    %STRING(?TEGHACF)
    ?MSGX     :=    '## 発注集計ﾌｧｲﾙ名 = ' && ?TEGHACFN && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##発注集計表発行指示入力##./
STE0010I:

    ?STEP :=   'STE0010I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL   TOKELIB/TOKFLIB

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-SHTDENL1,TOFILE-?JHTDENF
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

/.##発注集計表データ抽出##./
STE0020B:

    ?STEP :=   'STE0020B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL   TOKELIB

    OVRF      FILE-SHTDENL1,TOFILE-?JHTDENF
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TEGHACF,TOFILE-?TEGHACF
    CALL      PGM-STE0020B.TOKELIB,PARA-(?TORICD,?SDEN,?EDEN,?KBN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
                                  /.##_番順##./
    IF        ?KBN       =  '2'   THEN
              GOTO AITESCD
    END

/.##ＳＯＲＴ　サカタ商品コード順##./
SORT1:
/.##ＳＯＲＴ順：部門、倉庫、納品日、商品ｺｰﾄﾞ、店舗順##./
    ?STEP :=  'SORT1   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SORT       INFILE-?TEGHACF,INRL-200,INBF-5,
              OUTFILE-?TEGHACF,OUTBF-5,
              KEY-28!4!CA,KEY1-26!2!CA,KEY2-40!8!CA,KEY3-61!16!CA,
              KEY4-21!5!CA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    ELSE
              GOTO STE0030L
    END

/.##相手商品ｺｰﾄﾞ順#./
AITESCD:
/.##ＳＯＲＴ　相手商品コード##./
SORT2:
/.##ＳＯＲＴ順：部門、倉庫、納品日、相手商品ｺｰﾄﾞ、店舗順##./
    ?STEP :=  'SORT2   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SORT       INFILE-?TEGHACF,INRL-200,INBF-5,
              OUTFILE-?TEGHACF,OUTBF-5,
              KEY-28!4!CA,KEY1-26!2!CA,KEY2-40!8!CA,KEY3-48!13!CA,
              KEY4-21!5!CA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END


/.##発注集計表作表##./
STE0030L:

    ?STEP :=   'STE0030L'
      ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

    OVRF      FILE-TEGHACF,TOFILE-?TEGHACF

/.  OVRPRTF   FILE-PRTF,TOFILE-XU04LP.XUCL,MEDLIB-TOKELIB  ./
    CALL      PGM-STE0030L.TOKELIB,PARA-(?TKBN,?KBN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

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
