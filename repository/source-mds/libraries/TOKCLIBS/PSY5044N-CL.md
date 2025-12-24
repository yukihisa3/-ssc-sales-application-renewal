# PSY5044N

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PSY5044N.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    トステムビバ　オンラインシステム     *  ./
/. *   JOB-ID      :    PSY5044N                             *  ./
/. *   JOB-NAME    :    納品明細書出力                       *  ./
/. ***********************************************************  ./
    PGM (P1-?CHK)
/.###ﾊﾟﾗﾒﾀｴﾘｱ定義####./
    PARA ?CHK     ,STRING*1,IN,VALUE-' '        /.起動FLG./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PSY5044N'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*2,VALUE-'88'          /.倉庫(代表)./
    VAR ?P2       ,STRING*8,VALUE-'00038709'    /.取引先CD1./
    VAR ?P3       ,STRING*8,VALUE-'00387091'    /.取引先CD2./
    VAR ?P4       ,STRING*8,VALUE-'00000000'    /.取引先CD3./
    VAR ?P5       ,STRING*8,VALUE-'00000000'    /.取引先CD4./
    VAR ?P6       ,STRING*8,VALUE-'00000000'    /.取引先CD5./
    VAR ?P7       ,STRING*8,VALUE-'00000000'    /.受信日付  ./
    VAR ?P8       ,STRING*4,VALUE-'0000'        /.受信時間  ./
    VAR ?P9       ,STRING*8,VALUE-'00000000'    /.受信取引先./
    VAR ?P10      ,STRING*2,VALUE-'88'          /.倉庫CD ./
    VAR ?P11      ,STRING*5,VALUE-'00000'       /.店舗CD 開始./
    VAR ?P12      ,STRING*5,VALUE-'00000'       /.店舗CD 終了./
    VAR ?P13      ,STRING*8,VALUE-'00000000'    /.納品日 開始./
    VAR ?P14      ,STRING*8,VALUE-'00000000'    /.納品日 終了./
    VAR ?P15      ,STRING*9,VALUE-'000000000'   /.伝票番号 開始./
    VAR ?P16      ,STRING*9,VALUE-'000000000'   /.伝票番号 終了./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := 'トステムビバ納品明細書出力'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKELIBO
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
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
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P10,?P1)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  'ＷＳ－倉庫取得'
              GOTO ABEND
    END

/.##トステムビバ納品明細書出力指示##./
SSY5035I:

    ?STEP :=   'SSY5035I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-KHSYUKL5,TOFILE-KHSYUKL5.TOKKLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-SSY5035I.TOKELIBO
             ,PARA-(?P1,?P2,?P3,?P4,?P5,?P6,?P7,?P8,?P9,?P10,?P11
                    ,?P12,?P13,?P14,?P15,?P16)

    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4 :=  '抽出条件入力'
              GOTO ABEND
         END
    END


/.##トステムビバ納品明細書データ作成##./
SSY5037B:

    ?STEP :=   'SSY5037B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.  CLRFILE   LSTNMEF.TOKWLIB
./  OVRF      FILE-KHSYUKL4,TOFILE-KHSYUKL4.TOKKLIB
    OVRF      FILE-KHJOHOL5,TOFILE-KHJOHOL5.TOKKLIB
/.##帳票ﾌｧｲﾙ##./
  /.##本社##./
    IF  ?CHK = '1'  THEN
         OVRF      FILE-LSTNMEL1,TOFILE-LSTNMEL1.TOKWLIB
         CLRFILE   LSTNMEF.TOKWLIB
    END
  /.##片岡ﾚｰｻﾞｰに出力##./
    IF  ?CHK = '2'  THEN
         OVRF      FILE-LSTNMEL1,TOFILE-LSTNMKL1.TOKWLIB
         CLRFILE   LSTNMKF.TOKWLIB
    END
  /.##蔦井ﾚｰｻﾞｰに出力##./
    IF  ?CHK = '3'  THEN
         OVRF      FILE-LSTNMEL1,TOFILE-LSTNMTL1.TOKWLIB
         CLRFILE   LSTNMTF.TOKWLIB
    END
  /.##鴻巣ﾚｰｻﾞｰに出力##./
    IF  ?CHK = '4'  THEN
         OVRF      FILE-LSTNMEL1,TOFILE-LSTNMOL1.TOKWLIB
         CLRFILE   LSTNMOF.TOKWLIB
    END
  /.##西尾ﾚｰｻﾞｰに出力##./
    IF  ?CHK = '5'  THEN
         OVRF      FILE-LSTNMEL1,TOFILE-LSTNMNL1.TOKWLIB
         CLRFILE   LSTNMNF.TOKWLIB
    END
    CALL      PGM-SSY5038B.TOKELIBO,PARA-(?P7,?P8,?P9,?P10,?P11
                                         ,?P12,?P13,?P14,?P15,?P16)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '納品明細書データ出力'
              GOTO ABEND END


/.##トステムビバ納品明細書出力##./
SSY5040L:

    ?STEP :=   'SSY5040L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
/.##ﾌﾟﾘﾝﾀ切り替え##./
  /.##本社ﾚｰｻﾞｰに出力##./
    IF  ?CHK = '1'  THEN
         OVRPRTF FILE-XU04LP,TOFILE-TOSTMPRT.XUCL
         ?MSGX    :=  '## 出力ﾌﾟﾘﾝﾀ ==> 本社 ##'
         SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
  /.##片岡ﾚｰｻﾞｰに出力##./
    IF  ?CHK = '2'  THEN
         OVRPRTF FILE-XU04LP,TOFILE-TOSTMPR1.XUCL
         ?MSGX    :=  '## 出力ﾌﾟﾘﾝﾀ ==> 片岡 ##'
         SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
  /.##蔦井ﾚｰｻﾞｰに出力##./
    IF  ?CHK = '3'  THEN
         OVRPRTF FILE-XU04LP,TOFILE-TOSTMPR2.XUCL
         ?MSGX    :=  '## 出力ﾌﾟﾘﾝﾀ ==> 蔦井 ##'
         SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
  /.##鴻巣ﾚｰｻﾞｰに出力##./
    IF  ?CHK = '4'  THEN
         OVRPRTF FILE-XU04LP,TOFILE-TOSTMPR3.XUCL
         ?MSGX    :=  '## 出力ﾌﾟﾘﾝﾀ ==> 鴻巣 ##'
         SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
  /.##西尾ﾚｰｻﾞｰに出力##./
    IF  ?CHK = '5'  THEN
         OVRPRTF FILE-XU04LP,TOFILE-TOSTMPR4.XUCL
         ?MSGX    :=  '## 出力ﾌﾟﾘﾝﾀ ==> 西尾 ##'
         SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END

    CALL      PGM-SSY5044L.TOKELIBO

    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '納品明細書出力'
              GOTO ABEND END
RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '納品明細書出力が正常終了しました。'
    ?KEKA2 :=  ''
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '納品明細書出力が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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
