# PNSY020P

**種別**: JCL  
**ライブラリ**: TOKCLLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLLIB/PNSY020P.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＤＣＭＪＡＰＡＮ　オンラインシステム *  ./
/. *   JOB-ID      :    PNSY020P                             *  ./
/. *   JOB-NAME    :    新出荷リスト発行（電子帳票出力）　　 *  ./
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
    VAR ?PGMID    ,STRING*8,VALUE-'PNSY020P'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*8,VALUE-'00000000'    /.受信日付  ./
    VAR ?P2       ,STRING*4,VALUE-'0000'        /.受信時間  ./
    VAR ?P3       ,STRING*8,VALUE-'00000000'    /.受信取引先./
    VAR ?P4       ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?P5       ,STRING*2,VALUE-'00'          /.倉庫(代表)./
    VAR ?P6       ,STRING*5,VALUE-'00000'       /.店舗(開始)./
    VAR ?P7       ,STRING*5,VALUE-'00000'       /.店舗(終了)./
    VAR ?P8       ,STRING*8,VALUE-'00000000'    /.納品日(開始)./
    VAR ?P9       ,STRING*8,VALUE-'00000000'    /.納品日(終了)./
    VAR ?P10      ,STRING*9,VALUE-'000000000'   /.伝票番号(終了)./
    VAR ?P11      ,STRING*9,VALUE-'000000000'   /.伝票番号(開始)./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := 'ＤＣＭ出荷リスト発行'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKMDLIB/TOKKLIB/TOKDTLIB
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
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P4,?P5)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  'ＷＳ－倉庫取得'
              GOTO ABEND
    END

/.##環境変数変更##./
HENSUHEN:

    ?STEP :=   'HENSUHEN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CHGCMVAR '@OUTQN',XXLWQ        /.##出力スプール移動##./
    CHGCMVAR '@OUTDSN',DCMLIST     /.##スプールファイル名変更##./


/.##出荷リスト発行指示##./
NSY020PI:

    ?STEP :=   'NSY020PI'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '##　電子帳票へ出力　##'
    SNDMSG    ?MSGX,TO-XCTL
/.  OVRPRTF FILE-XU04LP,TOFILE-LBPPRTLW.TOKELIBO,MEDLIB-TOKELIBO
./
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    OVRF      FILE-DNSYUKL3,TOFILE-DNSYUKL3.TOKDTLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-NSY020PI.TOKSOLIB,PARA-(?P1,?P2,?P3,?P4,?P5,?P6,?P7
                                         ,?P8,?P9,?P10,?P11)
    ?PGMEC    :=   @PGMEC
    ?PGMEM    :=   @PGMEM
    IF        ?PGMEC    ^=   0    THEN
         IF   ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4 :=  '発行条件入力'
              GOTO ABEND
         END
    END
    ?MSGX    :=  '## ﾊﾞｯﾁ日付 = '&& ?P1  && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX    :=  '## ﾊﾞｯﾁ時刻 = '&& ?P2  && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX    :=  '## ﾊﾞｯﾁ取引 = '&& ?P3  && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX    :=  '## 倉庫ＣＤ = '&& ?P4  && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX    :=  '## 代表倉庫 = '&& ?P5  && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX    :=  '## 店舗開始 = '&& ?P6  && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX    :=  '## 店舗終了 = '&& ?P7  && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX    :=  '## 納品日開 = '&& ?P8  && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX    :=  '## 納品日終 = '&& ?P9  && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX    :=  '## 伝票開始 = '&& ?P10 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX    :=  '## 伝票終了 = '&& ?P11 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##出荷リスト発行##./
NSY0210L:

    ?STEP :=   'NSY0210L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL


    OVRF      FILE-DJSYUKL3,TOFILE-DJSYUKL3.TOKKLIB
    CALL      PGM-NSY0210L.TOKSOLIB,PARA-(?P1,?P2,?P3,?P4,?P6,?P7
                                         ,?P8,?P9,?P10,?P11)
    ?PGMEC    :=   @PGMEC
    ?PGMEM    :=   @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷リスト発行'
              GOTO ABEND END
    GOTO  RTN

RTN:

    CHGCMVAR '@OUTQN',XSYSLSTQ     /.##出力スプール移動##./
    CHGCMVAR '@OUTDSN',XSYSLIST    /.##スプールファイル名変更##./

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '出荷リスト出力が正常終了しました。'
    ?KEKA2 :=  ''
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-?PGMEC


ABEND:

    CHGCMVAR '@OUTQN',XSYSLSTQ     /.##出力スプール移動##./
    CHGCMVAR '@OUTDSN',XSYSLIST    /.##スプールファイル名変更##./

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '出荷リスト出力が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
  /.?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM  ./
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-?PGMEC

```
