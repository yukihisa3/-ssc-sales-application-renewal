/. ************************************************************ ./
/. *     サカタのタネ　特販システム                           * ./
/. *     オンライン手動受信システム　                         * ./
/. *     取引先：ホーマーグリーン                             * ./
/. *     作成者：NAV T.TAKAHASHI                              * ./
/. *     作成日：00/07/28                     ID:PHOGJYU      * ./
/. ************************************************************ ./
    PGM
    VAR       ?RETEL    ,INTEGER                    /.ﾘﾀﾞｲﾔﾙ./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMECY   ,STRING*4
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*13
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PHOGJYU '
    VAR       ?STEP     ,STRING*8
    VAR       ?CHK      ,STRING*1
/.-----------------------------------------------------------./
    VAR       ?PGNM     ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                  /.      2    ./
    VAR       ?KEKA2    ,STRING*40                  /.      3    ./
    VAR       ?KEKA3    ,STRING*40                  /.      4    ./
    VAR       ?KEKA4    ,STRING*40                  /.      5    ./

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ホーマーグリーン関東　受領データ受信処理'

/.ライブラリリストの登録                                        ./
    DEFLIBL CVCSLBI/CVCSOBJ
    SNDMSG MSG-'## 受信開始 ##',TO-XCTL.@ORGPROF

/.##受信状態ﾁｪｯｸ##./
    ?CHK   := ' '

/.##ﾘﾀﾞｲﾔﾙFLGｾｯﾄ##./
    ?RETEL := 1

/.ＶＬＤの活性化～データ受信                                    ./
SIJYUSN:
    OVRVLDF FILE-LD901,TOFILE-LD901.CVCSLBI
    CALL CVCS1001.CVCSOBJ,
         PARA-('01AA39840018C')
    ?PGMEC            :=        @PGMEC
    ?PGMECX           :=        %STRING(?PGMEC)
    ?PGMECY           :=        %SBSTR(?PGMECX,8,4)
    /.## 4095の時、ﾘﾀﾞｲﾔﾙ待機中         ##./
    /.## 4020の時、送信ﾌｧｲﾙ無し         ##./
    /.## 4095,4020以外は、受信ｴﾗｰとする ##./
    IF  ?PGMECY  =  '4095'  THEN
          IF ?RETEL < 5  THEN
             ?MSGX := '### ﾘﾀﾞｲﾔﾙ ﾀｲｷ ﾁｭｳ ###'
             SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
             ?RETEL := ?RETEL + 1
             /.### ｼﾞｭｼﾝﾏﾁｱﾜｾ ###./
             CALL TWAIT.XUCL
             GOTO   SIJYUSN
          ELSE
             ?CHK  :=   '1'
              GOTO       ABENDK
          END
    ELSE
          IF ?PGMECY  ^= '4020'  THEN
             ?CHK  :=    '2'
              GOTO       ABENDK
          ELSE
              IF ?PGMECY  ^=  '0000'  THEN
                 ?CHK  :=    '3'
                  GOTO   ABENDK
              END
          END
    END

/.##（ホーマーＧ）受信受領データ送信##./
PSNDFILE:

    ?STEP :=   'PSNDFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDFILE COM-PCPATH02.PCNODE02,SFILE-CVCSG001.CVCSLBI,
            DFILE-'HOGJYU',RL-128,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?CHK  :=  '4'
              GOTO ABENDK END

/.##（ホーマーＧ）受領変換ジョブ起動##./
PSI03000:

    ?STEP :=   'PSI03000'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDRSJJ COM-PCPATH02.PCNODE02,DFILE-'HOGSHI'
    IF        @PGMEC    ^=   0    THEN
              ?CHK  :=  '5'
              GOTO ABENDK END

/.##時間待ち合わせ##./
KWAIT:
    CALL   TWAIT3
/.##累積Ｆから集信データを抽出##./
KENTYUSY:

    ?STEP :=   'KENTYUSY'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL   CVCSOBJ/CVCSLBI
    OVRF      FILE-CVCSS005,TOFILE-CVCSS005.CVCSLBI
    OVRF      FILE-CVCSL004,TOFILE-CVCSL004.CVCSLBI
    OVRF      FILE-CVCSCARD,TOFILE-HOGJYU.CVCSLBI
    CALL      PGM-CVCS1003.CVCSOBJ
    IF        @PGMEC    ^=   0    THEN GOTO ABENDK END
/.  運用状況リスト出力                                         ./
LSTOUT:
    OVRF      FILE-CVCSCARD,TOFILE-CARD0010.CVCSLBI
    CALL      CLT1010.CVCSLBI
OWARI:

    DEFLIBL TOKELIB/TOKFLIB
    ?KEKA1 :=  'ホーマーグリーン受領データ受信処理が正常'
    ?KEKA2 :=  '終了しました。'
    ?KEKA3 :=  'ＧＰ側に受領データを転送後変換処理を実施'
    ?KEKA4 :=  'しました。受領書出力ＯＫです。！！！'
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    RETURN    PGMEC-@PGMEC

ABENDK:
    /.## CHK:1 リダイヤルエラー##./
    IF  ?CHK = '1'  THEN
        ?KEKA1 :=  '５回リダイヤルしましたが受信できません。'
        ?KEKA2 :=  '暫く時間をおいてから再度手動受信をお願い'
        ?KEKA3 :=  'します。'
        ?KEKA4 :=  '【リダイヤルエラー】'
    END
    /.## CHK:2 送信ファイル無し##./
    IF  ?CHK = '2'  THEN
        ?KEKA1 :=  '受領データの受信処理を行ないましたが、　'
        ?KEKA2 :=  '先方に用意されていないようです。'
        ?KEKA3 :=  '確認して下さい。'
        ?KEKA4 :=  '【送信ファイル無し】'
    END
    /.## CHK:3 受信エラー##./
    IF  ?CHK = '3'  THEN
        ?KEKA1 :=  '受領データ受信時に受信エラーが発生しまし'
        ?KEKA2 :=  'た。ログリストを採取しナブ・アシストまで'
        ?KEKA3 :=  '連絡！'
        ?KEKA4 :=  '【受信エラー】'
    END
    /.## CHK:4 支払データ送信エラー##./
    IF  ?CHK = '4'  THEN
        ?KEKA1 :=  '受信した受領データのＧＰ転送に失敗しまし'
        ?KEKA2 :=  'た。ナブ・アシストまで連絡をお願いします'
        ?KEKA3 :=  ''
        ?KEKA4 :=  '【ＧＰ送信エラー】'
    END
    /.## CHK:5 ＧＰジョブ起動エラー##./
    IF  ?CHK = '5'  THEN
        ?KEKA1 :=  '受領データ送信後のＧＰジョブ起動に失敗し'
        ?KEKA2 :=  'ました。ナブ・アシストまで連絡をお願いし'
        ?KEKA3 :=  'ます。'
        ?KEKA4 :=  '【ＧＰジョブ起動エラー】'
    END
    /.## エラーリスト出力 ##./
    DEFLIBL TOKELIB/TOKFLIB
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    DSPLOG    OUTPUT-@LIST,EDT-@JEF
    RETURN    PGMEC-@PGMEC
