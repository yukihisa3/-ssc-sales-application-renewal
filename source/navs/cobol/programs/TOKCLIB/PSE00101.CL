/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    PSE00101                              *  ./
/. *   JOB-NAME    :    請求締日入力（月末以外）             *  ./
/. *   UPDATE      :    MIURA 基幹サーバ統合                *  ./
/. *   UPDATE      :    2013/07/03 コメリ締日対応 TAKAHASHI *  ./
/. *   UPDATE      :    2023/11/02 イオンリテール末締へ変更 *  ./
/. *   UPDATE      :    2024/01/19 サンデー末締めへ変更　　 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSE00101'
    VAR       ?STEP     ,STRING*8
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

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
     DEFLIBL   TOKFLIB/TOKELIB/TOKSOLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '請求締処理　月末以外'
    ?SYORINM1 :=  '請求締処理　月末以外'

/.##請求締区分セット　２：月末締 ##./
    ?SEIKBN := '1'

/.##請求締日入力##./
SSE0010I:

    ?STEP :=   'SSE0010I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'＃＃請求締日入力＃＃',TO-XCTL

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
    SNDMSG MSG-'＃＃請求ＤＴ抽出中＃＃',TO-XCTL

    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-DENJNL5,TOFILE-SHTDENL5.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-SEIGKF2,TOFILE-SETGKFA2.TOKFLIB
    CALL      PGM-SSE0020B.TOKELIB,PARA-(?SYORIKN1)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【請求ＤＴ抽出】'
              GOTO ABEND END

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

/.##片倉繰越##./
ZENGETU1:

    ?STEP :=   'ZENGETU1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'＃＃片倉繰越中＃＃',TO-XCTL

/.##片倉工業前月請求データコピー##./
 /. OVRF     FILE-SETGKFB,TOFILE-SETGKFD.TOKFLIB
    OVRF     FILE-SEIGKFA,TOFILE-SETGKFA1.TOKFLIB
    CALL     PGM-KURIKOSI.TOKELIBO   ./
 /. OVRF     FILE-SETGKFB,TOFILE-SETGKFD.TOKFLIB
    OVRF     FILE-SEIGKFA2,TOFILE-SETGKFA2.TOKFLIB
    CALL     PGM-KURIKOSA.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【片倉前月繰越】'
              GOTO ABEND END
 ./
/.##セキチュー繰越##./
ZENGETU2:

    ?STEP :=   'ZENGETU21'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'＃＃セキチュー繰越中＃＃',TO-XCTL

/.##セキチュー前月請求データコピー##./
/.  OVRF     FILE-SETGKFB,TOFILE-SETGKFE.TOKFLIB
    OVRF     FILE-SEIGKFA,TOFILE-SETGKFA1.TOKFLIB
    CALL     PGM-KURIKOSI.TOKELIBO      ./
    OVRF     FILE-SETGKFB,TOFILE-SETGKFE.TOKFLIB
    OVRF     FILE-SEIGKFA2,TOFILE-SETGKFA2.TOKFLIB
    /.##2013/07/03 変更 ST ##./
/.##CALL     PGM-KURIKOSA.TOKELIBO##./
    CALL     PGM-KURIKOSA.TOKELIBO,PARA-(?SIMEBI)
    /.##2013/07/03 変更 ED ##./
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【セキチュー前月繰越】'
              GOTO ABEND END

SSE0280B: /.##イオン東北##./
/.##　締日変更の為、処理停止する　２０２３／１１／０２ ./
/.  ?STEP :=   'SSE0280B'                              ./
/.  ?MSGX :=  '***   '  && ?STEP   &&   '        ***'  ./
/.  SNDMSG    ?MSGX,TO-XCTL                            ./
/.  SNDMSG MSG-'＃＃イオン仙台　　繰越中＃＃',TO-XCTL  ./
/.                                                     ./
/.  OVRF     FILE-SETGKFB,TOFILE-SETGKFP.TOKFLIB       ./
/.  OVRF     FILE-SEIGKFA2,TOFILE-SETGKFA2.TOKFLIB     ./
/.  CALL     PGM-KURIKOSA.TOKELIBO,PARA-(?SIMEBI)      ./
/.  IF        @PGMEC    ^=   0    THEN                 ./
/.            ?KEKA4 :=  '【イオン仙台繰越】'          ./
/.            GOTO ABEND END                           ./

SSE5002B: /.##トステムビバ##./

    ?STEP :=   'SSE5002B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'＃＃トステムビバ　繰越中＃＃',TO-XCTL

 /. OVRF     FILE-SEIGK50,TOFILE-SEIGK50.TOKFLIB
    OVRF     FILE-SEIGKF1,TOFILE-SETGKFA1.TOKFLIB
    CALL     PGM-SSE5002B.TOKELIB  ./
    OVRF     FILE-SETGKFB,TOFILE-SEIGK50.TOKFLIB
    OVRF     FILE-SEIGKFA2,TOFILE-SETGKFA2.TOKFLIB
    /.##2013/07/03 変更 ST ##./
/.##CALL     PGM-KURIKOSA.TOKELIBO##./
    CALL     PGM-KURIKOSA.TOKELIBO,PARA-(?SIMEBI)
    /.##2013/07/03 変更 ED ##./
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【トステムビバ繰越】'
              GOTO ABEND END

PSORT1: /.##片倉抽出##./

    ?STEP :=   'PSORT1  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'＃＃片倉抽出中＃＃',TO-XCTL

 /.片倉工業抽出./
 /. SORTF     INFILE-SETGKFA.TOKFLIB,
              OUTFILE-SETGKFD.TOKFLIB,
              KEY-'F01!A',
              SELECT-'F01!EQ!#00120020',
              RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【片倉工業　請求ＤＴ抽出】'
              GOTO ABEND END
 ./
PSORT2: /.##セキチュー抽出##./

    ?STEP :=   'PSORT2  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'＃＃セキチュー抽出中＃＃',TO-XCTL

 /.セキチュー抽出./
    SORTF     INFILE-SETGKFA.TOKFLIB,
              OUTFILE-SETGKFE.TOKFLIB,
              KEY-'F01!A',
              SELECT-'F01!EQ!#00321717',
              RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【セキチュー　請求ＤＴ抽出】'
              GOTO ABEND END

 /.イオン東北./
PSORT7:
/.##　締日変更の為、処理停止する　２０２３／１１／０２ ./
/.  ?STEP :=   'PSORT7  '                              ./
/.  ?MSGX :=  '***   '  && ?STEP   &&   '        ***'  ./
/.  SNDMSG    ?MSGX,TO-XCTL                            ./
/.                                                     ./
/.  SORTF     INFILE-SETGKFA.TOKFLIB,                  ./
/.            OUTFILE-SETGKFP.TOKFLIB,                 ./
/.            KEY-'F01!A',                             ./
/.            SELECT-'F01!EQ!#00087372',               ./
/.            SELECT1-'F01!EQ!#00087373',              ./
/.            SELECT2-'F01!EQ!#00087374',              ./
/.            RCDL-@DSP                                ./
/.  IF        @PGMEC    ^=   0    THEN                 ./
/.            ?KEKA4 :=  '【イオン東北　請求ＤＴ抽出】'./
/.            GOTO ABEND END                           ./

 /.トステムビバ　東日本・北海道分を計上する。./
PSORT8:

    ?STEP :=   'PSORT8  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORTF     INFILE-SETGKFA.TOKFLIB,
              OUTFILE-SEIGK50.TOKFLIB,
              KEY-'F01!A',
              SELECT-'F01!EQ!#00038709',
              SELECT1-'F01!EQ!#00387091',
              RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【トステムビバ　請求ＤＴ抽出】'
              GOTO ABEND END

SSE7702B: /.##サンデー##./

    ?STEP :=   'SSE7702B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'＃＃サンデー　　　繰越中＃＃',TO-XCTL

 /. OVRF     FILE-SETGKFS,TOFILE-SETGKFS.TOKKLIB
    OVRF     FILE-SETGKFA1,TOFILE-SETGKFA1.TOKFLIB
    CALL     PGM-SSE7702B.TOKELIBO  ./
    OVRF     FILE-SETGKFB,TOFILE-SETGKFS.TOKKLIB
    OVRF     FILE-SETGKFA2,TOFILE-SETGKFA2.TOKFLIB
    /.##2013/07/03 変更 ST ##./
/.##CALL     PGM-KURIKOSA.TOKELIBO##./
/.##2024/01/19 末締めへ変更 ST                    ##./
/.##CALL     PGM-KURIKOSA.TOKELIBO,PARA-(?SIMEBI) ##./
/.##2024/01/19 末締めへ変更 ED                    ##./
    /.##2013/07/03 変更 ED ##./
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【サンデー繰越】'
              GOTO ABEND END

SSE7702J: /.##ジョイ##./

    ?STEP :=   'SSE7702J'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'＃＃ジョイ　　　　繰越中＃＃',TO-XCTL

/.  OVRF     FILE-SETGKFS,TOFILE-SETGKFJ.TOKKLIB
    OVRF     FILE-SETGKFA1,TOFILE-SETGKFA1.TOKFLIB
    CALL     PGM-SSE7702B.TOKELIBO     ./
    OVRF     FILE-SETGKFB,TOFILE-SETGKFJ.TOKKLIB
    OVRF     FILE-SETGKFA2,TOFILE-SETGKFA2.TOKFLIB
    /.##2013/07/03 変更 ST ##./
/.##CALL     PGM-KURIKOSA.TOKELIBO##./
/.##CALL     PGM-KURIKOSA.TOKELIBO,PARA-(?SIMEBI) 2013/07/23 停止##./
    /.##2013/07/03 変更 ED ##./
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ジョイ繰越】'
              GOTO ABEND END

 /.サンデー./
PSORT9:

    ?STEP :=   'PSORT9  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
/.##2013/07/03 変更 ST ##./
/.  SORTF     INFILE-SETGKFA.TOKFLIB,                     ./
/.            OUTFILE-SETGKFS.TOKKLIB,                    ./
/.            KEY-'F01!A',                                ./
/.            SELECT-'F01!EQ!#00030402',                  ./
/.            RCDL-@DSP                                   ./
/.  IF        @PGMEC    ^=   0    THEN                    ./
/.            ?KEKA4 :=  '【サンデー　　請求ＤＴ抽出】'   ./
/.            GOTO ABEND END                              ./
/.##2013/07/03 変更 ED ##./

 /.ジョイ./
PSORT10:
/.## 2013/07/23 停止
    ?STEP :=   'PSORT10 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORTF     INFILE-SETGKFA.TOKFLIB,
              OUTFILE-SETGKFJ.TOKKLIB,
              KEY-'F01!A',
              SELECT-'F01!EQ!#00304021',
              RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ジョイ　　　請求ＤＴ抽出】'
              GOTO ABEND END
##./
IONKYUSY: /.##イオン九州##./

    ?STEP :=   'IONKYUSY'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'＃＃イオン九州　　繰越中＃＃',TO-XCTL

    OVRF     FILE-SETGKFB,TOFILE-SEIGK46.TOKFLIB
    OVRF     FILE-SETGKFA2,TOFILE-SETGKFA2.TOKFLIB
    /.##2013/07/03 変更 ST ##./
/.##CALL     PGM-KURIKOSA.TOKELIBO##./
    CALL     PGM-KURIKOSA.TOKELIBO,PARA-(?SIMEBI)
    /.##2013/07/03 変更 ED ##./
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【イオン九州繰越】'
              GOTO ABEND END

 /.イオン九州./
IONKSORT:

    ?STEP :=   'IONKSORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORTF     INFILE-SETGKFA.TOKFLIB,
              OUTFILE-SEIGK46.TOKFLIB,
              KEY-'F01!A',
              SELECT-'F01!EQ!#00033189',
              RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【イオン九州　請求ＤＴ抽出】'
              GOTO ABEND END

RTN:

    ?KEKA1 :=  '請求締処理が正常終了しました。'
    ?KEKA2 :=  '請求明細書の発行が可能です。'
    ?KEKA3 :=  '明細確認後は、請求データ送信を行って'
    ?KEKA4 :=  '下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
                /.１２３４５６７８９０１２３４５６７８９０１２３４'./
    ?SYORINM2 := '請求締処理（月末以外）が正常終了しました。　　　'
    ?SYORINM3 := '件数を確認して下さい。　　　　　　　　　　　　　　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  '請求締処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
                /.１２３４５６７８９０１２３４５６７８９０１２３４'./
    ?SYORINM2 := '請求締処理（月末以外）が異常終了しました。　　　'
    ?SYORINM3 := 'ログを採取して、ＮＡＶへ連絡して下さい。　　　　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
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