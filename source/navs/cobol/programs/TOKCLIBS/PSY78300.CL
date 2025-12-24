/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ロイヤルＨＣ　　　納品予定システム   *  ./
/. *   JOB-ID      :    PSY78300                             *  ./
/. *   JOB-NAME    :    ロイヤルＨＣ　　　納品予定データ作成 *  ./
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
    VAR ?PGMID    ,STRING*8,VALUE-'PSY78300'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*8,VALUE-'00000000'    /.受信日付  ./
    VAR ?P2       ,STRING*4,VALUE-'0000'        /.受信時間  ./
    VAR ?P3       ,STRING*8,VALUE-'00000000'    /.受信取引先./
    VAR ?P4       ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?P5       ,STRING*2,VALUE-'00'          /.倉庫(代表)./
    VAR ?P6       ,STRING*8,VALUE-'00000000'    /.納品日./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
    VAR ?CHK      ,STRING*1,VALUE-'0'           /.初期化FLG  ./
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := 'ロイヤルＨＣ納品予定データ抽出'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB
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

/.##ロイヤルＨＣ納品予定データ抽出指示##./
SSY7830I:

    ?STEP :=   'SSY7830I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-RHJOHOL1,TOFILE-RHJOHOL1.TOKKLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-SSY7830I.TOKELIBO
             ,PARA-(?P1,?P2,?P3,?P4,?P6,?P5)
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4 :=  '抽出条件入力'
              GOTO ABEND
         END
    END

/.##ロイヤルＨＣ納品予定データ削除##./
SSY7840B:

    ?STEP :=   'SSY7840B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-RHSYUKL1,TOFILE-RHSYUKL1.TOKKLIB
    CALL      PGM-SSY7840B.TOKELIBO,PARA-(?P1,?P2,?P3,?P4,?P6)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'データ削除'
              GOTO ABEND END

/.##ロイヤルＨＣ納品予定データ抽出##./
SSY7850B:

    ?STEP :=   'SSY7850B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENL1,TOFILE-SHTDENL1.TOKFLIB
    OVRF      FILE-RHJOHOL1,TOFILE-RHJOHOL1.TOKKLIB
    OVRF      FILE-RHSYUKL1,TOFILE-RHSYUKL1.TOKKLIB
    CALL      PGM-SSY7850B.TOKELIBO,PARA-(?P1,?P2,?P3,?P4,?P6)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'データ抽出'
              GOTO ABEND END

/.##送信編集ＤＴ初期化##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-RHSENDF.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '送信編集ＤＴ初期化'
              GOTO ABEND END

/.##送信ＤＴ編集##./
SSY7870B:

    ?STEP :=   'SSY7870B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-RHSYUKL1,TOFILE-RHSYUKL1.TOKKLIB
    OVRF      FILE-RHJOHOL1,TOFILE-RHJOHOL1.TOKKLIB
    OVRF      FILE-RHSENDL1,TOFILE-RHSENDL1.TOKKLIB
    CALL      PGM-SSY7870B.TOKELIBO,PARA-(?P1,?P2,?P3,?P4,?P6)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '送信データ編集'
              GOTO ABEND END

/.##送信ＤＴ初期化判定##./
SSY7893B:

    ?STEP :=   'SSY7893B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-PCCHK4,TOFILE-PCCHK4.ONLBLIB
    CALL      PGM-SSY7893B.TOKELIBO,PARA-(?CHK)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '送信データ初期化判定'
              GOTO ABEND END
    /.##判定##./
    ?MSGX :=  '## 初期化FLG判定 = ' && ?CHK && ' ##'
    SNDMSG    ?MSGX,TO-XCTL
    IF        ?CHK = '1'  THEN
               SNDMSG MSG-'##送信ﾌｧｲﾙ前回分初期化##',TO-XCTL
               CLRFILE SYKSNDRY.ONLBLIB
               CLRFILE PCCHK4.ONLBLIB
    ELSE
               SNDMSG MSG-'##送信ﾌｧｲﾙ前回分初期化なし##',TO-XCTL
    END


/.##送信形式に変換##./
SSY7860B:

    ?STEP :=   'SSY7860B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SNDRHS,TOFILE-SYKSNDRY.ONLBLIB
    OVRF      FILE-RHSENDL1,TOFILE-RHSENDL1.TOKKLIB
    CALL      PGM-SSY7860B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '送信データ作成'
              GOTO ABEND END

RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '確認後、送信処理を開始して下さい。'
    ?KEKA3 :=  ''
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
