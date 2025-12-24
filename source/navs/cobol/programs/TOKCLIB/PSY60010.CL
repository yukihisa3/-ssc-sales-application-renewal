/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷管理                             *  ./
/. *   JOB-ID      :    PSY60010                             *  ./
/. *   JOB-NAME    :    発注集計表（デリバリ集計）           *  ./
/. ***********************************************************  ./
    PGM

    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSY60010'
    VAR       ?STEP     ,STRING*8
    VAR       ?P1       ,STRING*8,VALUE-'00000000' /.受信日付    ./
    VAR       ?P2       ,STRING*4,VALUE-'0000'     /.受信時間    ./
    VAR       ?P3       ,STRING*8,VALUE-'00000000' /.受信取引先  ./
    VAR       ?P4       ,STRING*2,VALUE-'00'       /.出力対象倉庫./
    VAR       ?P5       ,STRING*1,VALUE-'0'        /.発注／訂正  ./
    VAR       ?P6       ,STRING*1,VALUE-'0'        /.出力順      ./
    VAR       ?P7       ,STRING*2,VALUE-'00'       /.代表倉庫    ./
    VAR       ?OPR1     ,STRING*50                 /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                 /.      2    ./
    VAR       ?OPR3     ,STRING*50                 /.      3    ./
    VAR       ?OPR4     ,STRING*50                 /.      4    ./
    VAR       ?OPR5     ,STRING*50                 /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

FILECHK:
    ASSIGN FILE-SHWHACF.TOKFLIB!@XCL
    IF     @PGMEC  ^=    0    THEN    GOTO   ERR  END
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
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P4,?P7)
    IF        @PGMEC    ^=   0   THEN
              GOTO ABEND
    END

/.##発注集計表発行指示入力##./
PSY60010:

    ?STEP :=   'PSY60010'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL   TOKELIB/TOKFLIB

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-SSY6001I.TOKELIB,PARA-(?P1,?P2,?P3,
                                         ?P4,?P5,?P6,?P7)
    IF        @PGMEC    ^=   0    THEN
        IF    @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO   RTN1
        ELSE
              GOTO   ABEND
        END
    END

/.##出荷明細データ抽出##./
SSY0006B:

    ?STEP :=   'SSY0006B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL   TOKELIB

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-SHWHACF,TOFILE-SHWHACF.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    CALL      PGM-SSY0006B.TOKELIB,PARA-(?P1,?P2,?P3,?P4,?P5,?P6)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##ＳＯＲＴ　相手商品コード##./
SORT1:
/.##ＳＯＲＴ順：部門、倉庫、納品日、相手商品ｺｰﾄﾞ、店舗順##./
    ?STEP :=  'SORT1   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SORT      INFILE-SHWHACF.TOKFLIB,INRL-200,INBF-5,
              OUTFILE-SHWHACF.TOKFLIB,OUTBF-5,
              KEY-28!4!CA,KEY1-183!10!CA,KEY2-40!8!CA,KEY3-48!13!CA,
              KEY4-21!5!CA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END


/.##発注集計表作表##./
SSY6002L:

    ?STEP :=   'SSY6002L'
      ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

    OVRF      FILE-SHWHACF,TOFILE-SHWHACF.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SSY6002L.TOKELIB,PARA-(?P5,?P6)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN1:

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

ERR:  /.資源の解放./

    ?OPR1  :=  '　　＃＃＃＃＃＃＃　資源使用中　＃＃＃＃＃＃＃　　'
    ?OPR2  :=  '　　現在、発注集計表ワークＦを他倉庫にて使用中　　'
    ?OPR3  :=  '　　です。少し時間をおいて再度、実行して下さい。　'
    ?OPR4  :=  '　　　　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR5  :=  '　　ＥＮＴＥＲ＝再実行，ＰＦ９＝プログラム終了　　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    GOTO   FILECHK
