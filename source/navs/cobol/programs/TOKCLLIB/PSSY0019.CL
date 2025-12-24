/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷管理（流通ＢＭＳ）               *  ./
/. *   JOB-ID      :    PSSY0019                             *  ./
/. *   JOB-NAME    :    手書伝票データ抽出（ムサシ）　　　   *  ./
/. *   UPDATE      :                                         *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSSY0019'
    VAR       ?STEP     ,STRING*8
    VAR       ?P1       ,STRING*02,VALUE-'00'
    VAR       ?P2       ,STRING*08,VALUE-'00000000'
    VAR       ?P3       ,STRING*09,VALUE-'000000000'
    VAR       ?P4       ,STRING*09,VALUE-'000000000'
    VAR       ?P5       ,STRING*08,VALUE-'00000000'
    VAR       ?P6       ,STRING*09,VALUE-'000000000'
    VAR       ?P7       ,STRING*09,VALUE-'000000000'
    VAR       ?P8       ,STRING*08,VALUE-'00000000'
    VAR       ?P9       ,STRING*09,VALUE-'000000000'
    VAR       ?P10      ,STRING*09,VALUE-'000000000'
    VAR       ?P11      ,STRING*08,VALUE-'00000000'
    VAR       ?P12      ,STRING*09,VALUE-'000000000'
    VAR       ?P13      ,STRING*09,VALUE-'000000000'
    VAR       ?P14      ,STRING*08,VALUE-'00000000'
    VAR       ?P15      ,STRING*09,VALUE-'000000000'
    VAR       ?P16      ,STRING*09,VALUE-'000000000'
    VAR       ?P17      ,STRING*08,VALUE-'00000000'
    VAR       ?P18      ,STRING*09,VALUE-'000000000'
    VAR       ?P19      ,STRING*09,VALUE-'000000000'
    VAR       ?P20      ,STRING*08,VALUE-'00000000'
    VAR       ?P21      ,STRING*09,VALUE-'000000000'
    VAR       ?P22      ,STRING*09,VALUE-'000000000'
    VAR       ?P23      ,STRING*08,VALUE-'00000000'
    VAR       ?P24      ,STRING*09,VALUE-'000000000'
    VAR       ?P25      ,STRING*09,VALUE-'000000000'
    VAR       ?P26      ,STRING*08,VALUE-'00000000'
    VAR       ?P27      ,STRING*09,VALUE-'000000000'
    VAR       ?P28      ,STRING*09,VALUE-'000000000'
    VAR       ?P29      ,STRING*08,VALUE-'00000000'
    VAR       ?P30      ,STRING*09,VALUE-'000000000'
    VAR       ?P31      ,STRING*09,VALUE-'000000000'
    VAR       ?P32      ,STRING*08,VALUE-'00000000'
    VAR       ?P33      ,STRING*09,VALUE-'000000000'
    VAR       ?P34      ,STRING*09,VALUE-'000000000'
    VAR       ?P35      ,STRING*08,VALUE-'00000000'
    VAR       ?P36      ,STRING*09,VALUE-'000000000'
    VAR       ?P37      ,STRING*09,VALUE-'000000000'
    VAR       ?P38      ,STRING*02,VALUE-'00'
    VAR       ?OUTKENSU ,STRING*07,VALUE-'0000000'
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./
    VAR       ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                    /.      2    ./
    VAR       ?KEKA2    ,STRING*40                    /.      3    ./
    VAR       ?KEKA3    ,STRING*40                    /.      4    ./
    VAR       ?KEKA4    ,STRING*40                    /.      5    ./
    VAR       ?SYORINM1 ,STRING*40
    VAR       ?SYORINM2 ,STRING*50
    VAR       ?SYORINM3 ,STRING*50
    VAR       ?SYORIKN1 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN2 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN3 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN4 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN5 ,STRING*7,VALUE-'0000000'
/.##DEFLIBL##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKDLIB/TOKDTLIB/TOKELIBO
           /BMSFLIB/TOKSOLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '手書伝票データ抽出（ムサシ）'
    ?SYORINM1 :=  '手書伝票データ抽出（ムサシ）'

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
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P1,?P38)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【倉庫ＣＤ取得】'
              GOTO ABEND
    END

/.##手書伝票データ抽出指示入力##./
PSSY0019:

    ?STEP :=   'PSSY0019'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES


    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-SSY0015I.TOKELIB,PARA-(?P1,
              ?P2,?P3,?P4,?P5,?P6,?P7,?P8,?P9,?P10,
              ?P11,?P12,?P13,?P14,?P15,?P16,?P17,?P18,?P19,
              ?P20,?P21,?P22,?P23,?P24,?P25,?P26,?P27,?P28,
              ?P29,?P30,?P31,?P32,?P33,?P34,?P35,?P36,?P37,?P38)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
         IF   ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              ?KEKA4 :=  '【手書伝票データ抽出指示入力】'
              GOTO ABEND
         END
    END

/.##出力ファイル確認##./
PASSIGN:

    ?STEP :=   'PASSIGN '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ASSIGN FILE-JHTTEGSF.BMSFLIB!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF     ?PGMEC  ^=    0    THEN
           GOTO  CHK
    END

/.##手書伝票データ抽出##./
SSY0016B:

    ?STEP :=   'SSY0016B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-SHTDENLC,TOFILE-SHTDENLC.TOKFLIB
    OVRF      FILE-JHTDENF,TOFILE-JHTTEGSF.BMSFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-SHOTBL2,TOFILE-SHOTBL2.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    CALL      PGM-SSY0016B.TOKELIBO,PARA-(?P1,
              ?P2,?P3,?P4,?P5,?P6,?P7,?P8,?P9,?P10,
              ?P11,?P12,?P13,?P14,?P15,?P16,?P17,?P18,?P19,
              ?P20,?P21,?P22,?P23,?P24,?P25,?P26,?P27,?P28,
              ?P29,?P30,?P31,?P32,?P33,?P34,?P35,?P36,?P37)

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【手書伝票データ抽出】'
              GOTO ABEND END
    /. データＳＯＲＴ./
    SORTF  INFILE-JHTTEGSF.BMSFLIB,
           OUTFILE-JHTTEGWF.BMSFLIB,
           KEY-'F01!A',
           SELECT-'F01!EQ!#00116501',
           RCDL-@DSP
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ＳＯＲＴ処理】'
              GOTO ABEND
    END
    ?MSGX :=  'SORTF-OUT  '
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    /.##抽出対象の各出荷場所ﾌｧｲﾙへﾃﾞｰﾀをｺﾋﾟｰする##./
    SETPF FILE-JHTTEGWF.BMSFLIB,TOFILE-BMSTE5F.BMSFLIB,
          ADD-@NO,ACTCHK-@NO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【抽出Ｆコピー】'
              GOTO ABEND
    END

/.##出荷ＭＳＧワーク初期化##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CLRFILE FILE-WKSYKAR.BMSFLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF     ?PGMEC  ^=    0    THEN
           ?KEKA4 :=  '【出荷ＭＳＧワーク初期化】'
           GOTO  ABEND
    END

/.##出荷ＭＳＧワーク作成##./
SSY4489B:

    ?STEP :=   'SSY4489B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF FILE-BMSTE5F,TOFILE-BMSTE5F.BMSFLIB
    CALL      PGM-SSY4489B.TOKSOLIB,PARA-(?OUTKENSU)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【出荷ＭＳＧワーク作成】'
              GOTO ABEND
    END
    ?SYORIKN1 :=  ?OUTKENSU

/.##伝票発行ワーク作成（ムサシ）手書分##./
SBM0057B:

    ?STEP :=   'SBM0057B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SBM0057B.TOKSOLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  '【伝票発行ワーク作成（ムサシ）手書分】'
              GOTO ABEND
    END

RTN:

              /.１２３４５６７８９０１２３４５６７８./
    ?KEKA1 :=  '＃（ムサシ）手書伝票データ抽出＃＃'
    ?KEKA2 :=  '手書伝票データ抽出処理が正常終了し'
    ?KEKA3 :=  'ました。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
                /.１２３４５６７８９０１２３４５６７８９０１２３４'./
    ?SYORINM2 := '（ムサシ）手書伝票データ抽出が正常終了しました。'
    ?SYORINM3 := '件数を確認して、各処理を実施して下さい。'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC


ABEND:

              /.１２３４５６７８９０１２３４５６７８./
    ?KEKA1 :=  '＃（ムサシ）手書伝票データ抽出＃＃'
    ?KEKA2 :=  '手書伝票データ抽出処理が異常終了し'
    ?KEKA3 :=  'ました。ログ採取しＮＡＶへ連絡！！'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
                /.１２３４５６７８９０１２３４５６７８９０１２３４'./
    ?SYORINM2 := '手書伝票データ抽出処理が異常終了しました。　　　'
    ?SYORINM3 := 'ログを採取して、ＮＡＶへ連絡して下さい。　　　　　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
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
/.#################################################################./
/.##資源使用時、他端末開放確認メッセージ出力）                   ##./
/.#################################################################./
CHK:  /.資源の解放./

    ?OPR1  :=  '　　＃＃＃＃＃＃＃　資源使用中　＃＃＃＃＃＃＃　　'
    ?OPR2  :=  '　　　　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR3  :=  '　　他端末にて資源を使用中です。　　　　　　　　　'
    ?OPR4  :=  '　　全端末の使用状況を確認して下さい。　　　　　　'
    ?OPR5  :=  '　ＥＮＴＥＲでリスタート，ＰＦ９でプログラム終了　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    GOTO      PASSIGN
