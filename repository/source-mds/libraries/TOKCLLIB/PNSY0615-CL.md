# PNSY0615

**種別**: JCL  
**ライブラリ**: TOKCLLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLLIB/PNSY0615.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＤＣＭ仕入先統合　　　　　　　　　   *  ./
/. *   JOB-ID      :    PNSY0615                             *  ./
/. *   JOB-NAME    :    納品ラベル発行（店直センター経由）   *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PNSY0615'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*8,VALUE-'00000000'    /.受信日付  ./
    VAR ?P2       ,STRING*4,VALUE-'0000'        /.受信時間  ./
    VAR ?P3       ,STRING*8,VALUE-'00000000'    /.受信取引先./
    VAR ?P4       ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?P5       ,STRING*2,VALUE-'00'          /.倉庫(代表)./
    VAR ?P6       ,STRING*3,VALUE-'000'         /.部門./
    VAR ?P7       ,STRING*8,VALUE-'00000000'    /.納品日./
    VAR ?P8       ,STRING*8,VALUE-'00000000'    /.納品日./
    VAR ?P9       ,STRING*5,VALUE-'00000'       /.部門./
    VAR ?P10      ,STRING*5,VALUE-'00000'       /.部門./
    VAR ?P11      ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?P12      ,STRING*1,VALUE-'0'           /.結果./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
    VAR ?TANCD6   ,STRING*06,VALUE-'      '     /.部門+担当者./
    VAR ?TANCD    ,STRING*02,VALUE-'  '         /.担当者./
    VAR ?BUMON    ,STRING*04,VALUE-'    '       /.部門./
    VAR ?TANNO    ,STRING*03,VALUE-'   '        /.端末番号./
    VAR ?LBL      ,STRING*3,VALUE-'LBL'
    VAR ?LBL1     ,STRING*2,VALUE-'L1'
    VAR ?LBLF     ,STRING*1,VALUE-'F'
    VAR ?FILNM    ,STRING*8,VALUE-'        '
    VAR ?LIBNM    ,STRING*8,VALUE-'ZAIEXCEL'
    VAR ?FILID    ,NAME
    VAR ?LIBID    ,NAME
    VAR ?LBLXXXL1 ,NAME!MOD
    VAR ?LBLXXXF  ,NAME!MOD
    VAR ?NLBLXXX1 ,STRING*17,VALUE-'                 '
    VAR ?NLBLXXXF ,STRING*17,VALUE-'                 '
    VAR ?OPR1     ,STRING*50                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2     ,STRING*50                    /.      2    ./
    VAR ?OPR3     ,STRING*50                    /.      3    ./
    VAR ?OPR4     ,STRING*50                    /.      4    ./
    VAR ?OPR5     ,STRING*50                    /.      5    ./
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := 'センター納品ラベル発行（店直センター経由対応）'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKMDLIB/TOKELIBO/TOKKLIB/ZAIEXCEL
           /TOKDTLIB
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
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P11,?P5)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  'ＷＳ－倉庫取得'
              GOTO ABEND
    END

/.##端末番号を取得##./
SBT0620B:

    ?STEP :=   'SBT0620B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-SBT0620B.TOKELIBO,PARA-(?WS,?TANNO)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND END
    ?MSGX := '## 端末NO = ' && ?TANNO
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾌｧｲﾙ名称取得##./
    ?FILNM    :=    ?LBL && ?TANNO && ?LBLF
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?LBLXXXF  :=    %NCAT(?FILID,?LIBID)
    ?NLBLXXXF :=    %STRING(?LBLXXXF)
    ?MSGX     :=    '## ラベルデータ(PF)  = ' && ?NLBLXXXF
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?LBL && ?TANNO && ?LBL1
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?LBLXXXL1 :=    %NCAT(?FILID,?LIBID)
    ?NLBLXXX1:=    %STRING(?LBLXXXL1)
    ?MSGX     :=    '## ラベルデータ(LF)  = ' && ?NLBLXXX1
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##資源の獲得##./
ASS:
     ASSIGN FILE-?LBLXXXF!@XCL
     IF     @PGMEC ^=  0  THEN
       ?OPR1 := '　＃＃＃＃＃＃　センター納品対応　＃＃＃＃＃＃＃　'
       ?OPR2 := '　＃＜　センター納品対応（ＤＣＭ仕入統合）＞　＃　'
       ?OPR3 := '　＃他の端末にて資源が使用されております。　　＃　'
       ?OPR4 := '　＃確認して下さい。（全端末未使用ですか？）　＃　'
       ?OPR5 := '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
       CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
       GOTO   ASS     END

/.##ラベルデータ初期化##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE ?LBLXXXF
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '連携状況再計算Ｆ初期化'
              GOTO ABEND
    END

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD6)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'ログインユーザー情報取得'
              GOTO ABEND
    END
    ?TANCD := %SBSTR(?TANCD6,1,2)



/.##納品ラベル発行指示##./
NSY0600I:

    ?STEP :=   'NSY0600I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-NSY0600I.TOKSOLIB,PARA-(?P11,?P5,?P1,?P2,?P3
                                         ,?P4,?P6,?P7,?P8,?P9,?P10)
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4 :=  '抽出条件入力'
              GOTO ABEND
         END
    END

/.##納品ラベル発行##./
NSY0615L:

    ?STEP :=   'NSY0615L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-DDCENTL1,TOFILE-DDCENTL1.TOKDTLIB
    OVRF      FILE-LBLXXXL1,TOFILE-?LBLXXXL1
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-NSY0615L.TOKSOLIB,PARA-(?P1,?P2,?P3,?P4,?P6
                                        ,?P7,?P8,?P9,?P10,?P12)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '納品ラベル'
              GOTO ABEND END
    /.##再度範囲指定画面へ##./
    GOTO  PCLRFILE

RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  ''
    IF  ?P12 = '1'  THEN
        ?KEKA3 :=  '出力対象のラベルデータが存在しません！'
    ELSE
        ?KEKA3 :=  '納品ラベルがプリンタに出力されました！'
    END
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
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
