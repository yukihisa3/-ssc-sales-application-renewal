/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    PSE00100                              *  ./
/. *   JOB-NAME    :    請求締日入力                         *  ./
/. *   UPDATE      :    MIURA 基幹サーバ統合                *  ./
/. *   UPDATE      :    2013/07/03 コメリ締日対応 TAKAHASHI *  ./
/. *   UPDATE      :    2023/11/02 イオンリテール　締日変更 *  ./
/. *   UPDATE      :    2024/01/19 サンデー　締日変更　　　 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSE00100'
    VAR       ?STEP     ,STRING*8
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
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
/.##2013/07/03 追加 ST ##./
    VAR       ?SEIKBN   ,STRING*1,VALUE-' '
    VAR       ?SIMEBI   ,STRING*8,VALUE-'00000000'
/.##2013/07/03 追加 ED ##./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '請求締処理　月末'
    ?SYORINM1 :=  '請求締処理　月末'

/.##請求締区分セット　２：月末締 ##./
    ?SEIKBN := '2'

     DEFLIBL   TOKFLIB/TOKELIB/TOKSOLIB
/.##資源の獲得##./
ASS:
     ASSIGN FILE-SHTDENF.TOKFLIB!@XCL
     IF     @PGMEC ^=  0  THEN
       ?OPR1 := '　＃＃＃＃＃＃　請求締日入力確認　＃＃＃＃＃＃＃　'
       ?OPR2 := '　＃＜　請求締日入力　売上伝票Ｆ使用チェック＞＃　'
       ?OPR3 := '　＃他の端末にて資源が使用されております。　　＃　'
       ?OPR4 := '　＃確認して下さい。（全端末未使用ですか？）　＃　'
       ?OPR5 := '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
       CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
       GOTO   ASS     END

/.##請求締日入力##./
SSE0010I:

    ?STEP :=   'SSE0010I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SSE0010I.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?PGMEC := @PGMEC
              IF   ?PGMEC = 4010  THEN
                   ?MSGX := '##取消終了##'
                   SNDMSG MSG-?MSGX,TO-XCTL
                   RETURN PGMEC-@PGMEC
              END
              ?KEKA4 :=  '【請求締日入力】'
              GOTO ABEND
    END

/.##請求合計ファイル初期化##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'＃＃請求合計ファイル初期化＃＃',TO-XCTL
    CLRFILE SETGKFA.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【請求合計ファイル初期化】'
              GOTO ABEND END

/.##請求合計ファイル作成##./
SSE0020B:

    ?STEP :=   'SSE0020B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-DENJNL5,TOFILE-SHTDENL5.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-SEIGKF2,TOFILE-SETGKFA2.TOKFLIB
    CALL      PGM-SSE0020B.TOKELIB,PARA-(?SYORIKN1)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【請求ＤＴ抽出】'
              GOTO ABEND END


/.##入荷データ直送データ抽出／作成##./
PSE00400:

    ?STEP :=   'PSE00400'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##CALL      PSE00400.TOKELIB##./
    CALL      CSE00400.TOKELIB

/.##2013/07/03 追加 ST ##./
SSIMEBI: /.##請求締日取得→保管##./

    ?STEP :=   'SSIMEBI '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF     FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL     PGM-SSIMEBI.TOKELIBO,PARA-(?SEIKBN,?SIMEBI)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【請求締日取得→保管】'
              GOTO ABEND END
/.##締日を画面上にメッセージする##./
    ?MSGX :=  '# 請求締日 = ' && ?SIMEBI && ' #'
    SNDMSG    ?MSGX,TO-XCTL

/.##各取引先前月データマージ##./
SSE0030B: /.##ケーヨーホームセンター##./

    ?STEP :=   'SSE0030B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF     FILE-SETGKFB,TOFILE-SETGKFB.TOKFLIB
    OVRF     FILE-SETGKFA2,TOFILE-SETGKFA2.TOKFLIB
    /.##2013/07/03 変更 ST ##./
/.##CALL     PGM-KURIKOSA.TOKELIBO##./
    CALL     PGM-KURIKOSA.TOKELIBO,PARA-(?SIMEBI)
    /.##2013/07/03 変更 ED ##./
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ケーヨー　前月繰越】'
              GOTO ABEND END

SSE0160B: /.##ジョイフル山新##./

    ?STEP :=   'SSE0160B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF     FILE-SETGKFB,TOFILE-SETGKFG.TOKFLIB
    OVRF     FILE-SETGKFA2,TOFILE-SETGKFA2.TOKFLIB
    /.##2013/07/03 変更 ST ##./
/.##CALL     PGM-KURIKOSA.TOKELIBO##./
    CALL     PGM-KURIKOSA.TOKELIBO,PARA-(?SIMEBI)
    /.##2013/07/03 変更 ED ##./
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ジョイフル山新　前月繰越】'
              GOTO ABEND END

SSE0240B: /.##ビーバー登山##./

    ?STEP :=   'SSE0240B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF     FILE-SETGKFB,TOFILE-SETGKFK.TOKFLIB
    OVRF     FILE-SETGKFA2,TOFILE-SETGKFA2.TOKFLIB
    /.##2013/07/03 変更 ST ##./
/.##CALL     PGM-KURIKOSA.TOKELIBO##./
    CALL     PGM-KURIKOSA.TOKELIBO,PARA-(?SIMEBI)
    /.##2013/07/03 変更 ED ##./
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ビーバー登山　　前月繰越】'
              GOTO ABEND END

WEBSIZAI: /.##ホーマック資材##./

/.  ?STEP :=   'WEBSIZAI'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'＃＃ホーマックＷＥＢ資材＃＃',TO-XCTL

    OVRF     FILE-SETGKFH,TOFILE-SETGK83.TOKKLIB
    OVRF     FILE-SEIGKF1,TOFILE-SETGKFA1.TOKFLIB
    CALL     PGM-SSE0190B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ホーマック資材前月繰越】'
              GOTO ABEND END
./
WEBSYOKU: /.##ホーマック植物##./

    ?STEP :=   'WEBSYOKU'
/.  ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'＃＃ホーマックＷＥＢ植物＃＃',TO-XCTL

    OVRF     FILE-SETGKFH,TOFILE-SETGK84.TOKKLIB
    OVRF     FILE-SEIGKF1,TOFILE-SETGKFA1.TOKFLIB
    CALL     PGM-SSE0190B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ホーマック植物前月繰越】'
              GOTO ABEND END
./
DCMSIZAI: /.##ＤＣＭ資材##./
/.## 2011/10/24 繰越場所を変更 ST
    ?STEP :=   'DCMSIZAI'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'＃＃ＤＣＭ　ＷＥＢ資材＃＃',TO-XCTL

    OVRF     FILE-SETGKFH,TOFILE-SETGK87.TOKKLIB
    OVRF     FILE-SEIGKF1,TOFILE-SETGKFA1.TOKFLIB
    CALL     PGM-SSE0190B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ホーマック資材前月繰越】'
              GOTO ABEND END
##################################./
DCMSYOKU: /.##ＤＣＭ植物##./
/.## 2011/10/24 繰越場所を変更 ST
    ?STEP :=   'DCMSYOKU'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'＃＃ＤＣＭ　ＷＥＢ植物＃＃',TO-XCTL

    OVRF     FILE-SETGKFH,TOFILE-SETGK88.TOKKLIB
    OVRF     FILE-SEIGKF1,TOFILE-SETGKFA1.TOKFLIB
    CALL     PGM-SSE0190B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ホーマック植物前月繰越】'
              GOTO ABEND END
##################################./

SEIKOMER: /.##コメリ##./

    ?STEP :=   'SEIKOMER'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF     FILE-SETGKFB,TOFILE-SETGKFK.TOKKLIB
    OVRF     FILE-SETGKFA2,TOFILE-SETGKFA2.TOKFLIB
    CALL     PGM-KURIKOSA.TOKELIBO,PARA-(?SIMEBI)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ビーバー登山　　前月繰越】'
              GOTO ABEND END

/.##2023/11/02 START-----------------------------------##./
SEIIONRT: /.##イオン東北　２０日締より末締めに変更##./

    ?STEP :=   'SEIIONRT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'＃＃イオンリテール繰越中＃＃',TO-XCTL

    OVRF     FILE-SETGKFB,TOFILE-SETGKFP.TOKFLIB
    OVRF     FILE-SEIGKFA2,TOFILE-SETGKFA2.TOKFLIB
    CALL     PGM-KURIKOSA.TOKELIBO,PARA-(?SIMEBI)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【イオンリテール繰越】'
              GOTO ABEND END

 /.##イオンリテール　２０日締め→末締めに変更##./
IONSORTR:

    ?STEP :=   'IONSORTR'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORTF     INFILE-SETGKFA.TOKFLIB,
              OUTFILE-SETGKFP.TOKFLIB,
              KEY-'F01!A',
              SELECT-'F01!EQ!#00087372',
              SELECT1-'F01!EQ!#00087373',
              SELECT2-'F01!EQ!#00087374',
              RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【イオン東北　請求ＤＴ抽出】'
              GOTO ABEND END
/.##2023/11/02 END  -----------------------------------##./
/.##2024/01/19 START--サンデー締め日変更---------------##./
SEISANDY: /.##サンデー##./

    ?STEP :=   'SEISANDY'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'＃＃サンデー　　　繰越中＃＃',TO-XCTL

    OVRF     FILE-SETGKFB,TOFILE-SETGKFS.TOKKLIB
    OVRF     FILE-SETGKFA2,TOFILE-SETGKFA2.TOKFLIB
    CALL     PGM-KURIKOSA.TOKELIBO,PARA-(?SIMEBI)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【サンデー繰越】'
              GOTO ABEND END

 /.サンデー./
SMDSORTR:

    ?STEP :=   'PSORT9  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORTF     INFILE-SETGKFA.TOKFLIB,
              OUTFILE-SETGKFS.TOKKLIB,
              KEY-'F01!A',
              SELECT-'F01!EQ!#00030402',
              RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【サンデー　　請求ＤＴ抽出】'
              GOTO ABEND END

/.##2024/01/19 END----サンデー締め日変更---------------##./

RTN:
              /.１２３４５６７８９０１２３４５６７８./
    ?KEKA1 :=  '＃＃　請求締日（末締）　＃＃'
    ?KEKA2 :=  '請求締処理が正常終了しました。'
    ?KEKA3 :=  '請求明細書の発行が可能です。確認後、'
    ?KEKA4 :=  '各取引先への送信処理を行なって下さい'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
                /.１２３４５６７８９０１２３４５６７８９０１２３４'./
    ?SYORINM2 := '請求締処理（月末）が正常終了しました。　　　　　'
    ?SYORINM3 := '件数を確認して下さい。　　　　　　　　　　　　　　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
     /.資源の解放./
     RELEASE FILE-SHTDENF.TOKFLIB!@XCL

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:
              /.１２３４５６７８９０１２３４５６７８./
    ?KEKA1 :=  '＃＃　請求締日（末締）　＃＃'
    ?KEKA2 :=  '請求締日処理が異常終了しました。'
    ?KEKA3 :=  'ログを出力し、ＮＡＶへ連絡して下さい'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
                /.１２３４５６７８９０１２３４５６７８９０１２３４'./
    ?SYORINM2 := '請求締処理（月末）が異常終了しました。　　　　　'
    ?SYORINM3 := 'ログを採取して、ＮＡＶへ連絡して下さい。　　　　　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
     /.資源の解放./
     RELEASE FILE-SHTDENF.TOKFLIB!@XCL

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'

    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC
